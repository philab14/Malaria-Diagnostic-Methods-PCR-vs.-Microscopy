# Install necessary libraries
install.packages(c("dplyr", "ggplot2", "stats", "writexl", "lmtest", "MASS", "openxlsx"))

# Load libraries
library(dplyr)
library(ggplot2)
library(stats)
library(writexl)
library(lmtest)
library(MASS)
library(openxlsx)


# Loading Dataset
link_to_dataset <- read.table(file = "https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/lancet_malaria.txt", header = TRUE, sep = "\t")
malaria_data <- link_to_dataset


# Step 1: Read the data from the URL
url <- "https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/lancet_malaria.txt"
data <- read.table(url, header = TRUE, sep = "\t")  # Adjust separator as needed (e.g., tab-separated)

# Step 2: Check the structure of the data to confirm it's a data frame
str(data)

# Step 3: Write the data to a CSV file
write.csv(data, "C:/Users/ASAPH/Documents/SOPS FOR GRADUATE SCHOOL.csv", row.names = FALSE)


# Define the URL and destination file path
url <- "https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/lancet_malaria.txt"
destfile <- "C:/Users/ASAPH/Documents/lancet_malaria.txt"  # Save the file to your local directory

# Download the file
download.file(url, destfile)


head(link_to_dataset)

# Renaming of the column names
names(link_to_dataset)
colnames(link_to_dataset) <- c("Review Found", "Author", "Title", "Year", "Region","Country","Location", "PCR_N_Tested", "PCR_N_Positive", "PCR_Percent",
                            "Microscopy_N_Tested", "Microscopy_N_Positive", "Microscopy_Percent", "Historical_Transmission", "Current_Transmission", "Setting_20", "Setting_15", "Setting_10", "Setting_5", "PCR_Method", "Microscopy_Fields", "Sampling_Season", "Notes")

head(link_to_dataset)


# Check if the dataset exists
exists("link_to_dataset")

# Check the structure of your dataset
str(link_to_dataset)

# Check column names
names(link_to_dataset)

# Check first few rows
head(link_to_dataset)

# Check if columns exist
"PCR_Percent" %in% names(link_to_dataset)
"Microscopy_Percent" %in% names(link_to_dataset)

# Check the actual data in these columns
link_to_dataset$PCR_Percent
link_to_dataset$Microscopy_Percent

# Check for missing values and data type
summary(link_to_dataset$PCR_Percent)
summary(link_to_dataset$Microscopy_Percent)
class(link_to_dataset$PCR_Percent)

# Visualization of PCR % against microscopy %
plot(link_to_dataset$PCR_Percent, link_to_dataset$Microscopy_Percent,
     xlab = "Microscopy_Percent", ylab = "PCR %",
     main = "PCR vs Microscopy Prevalence",
     col = "blue", pch = 19)
abline(0, 1, lty = 2, col = "red")

# Remove rows with missing values (NA) from the dataset
link_to_dataset_clean <- link_to_dataset[!is.na(link_to_dataset$`PCR_Percent`) & !is.na(link_to_dataset$`Microscopy_Percent`), ]

# Remove rows where either PCR_Percent or Microscopy_Percent are NA or Inf/-Inf
link_to_dataset_clean <- link_to_dataset[!is.na(link_to_dataset$`PCR_Percent`) & 
                                           !is.na(link_to_dataset$`Microscopy_Percent`) & 
                                           is.finite(link_to_dataset$`PCR_Percent`) & 
                                           is.finite(link_to_dataset$`Microscopy_Percent`), ]


# Now, plot the cleaned dataset
plot(link_to_dataset_clean$`PCR_Percent`, link_to_dataset_clean$`Microscopy_Percent`,
     xlab = "PCR %", ylab = "Microscopy %",
     main = "PCR vs Microscopy Prevalence",
     col = "blue", pch = 19)



#  Prevalence Ratio
link_to_dataset$Prevalence_Ratio <- link_to_dataset$Microscopy_N_Positive / link_to_dataset$PCR_N_Positive
head(link_to_dataset)

# PCR% vs Microscopy% by Region
ggplot(link_to_dataset, aes(x = Microscopy_Percent, y = PCR_Percent, color = Region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~Region) +
  labs(title = "PCR% vs Microscopy% by Region",
       x = "Microscopy %", y = "PCR %")

# Prevalence Ratio by Region
boxplot(Prevalence_Ratio ~ Region, data = link_to_dataset,
        main = "Prevalence Ratio by Region",
        xlab = "Global Region", ylab = "Prevalence Ratio",
        col = c("lightblue","lightgreen","lightpink","lightyellow"),
        las = 2, notch = TRUE)
abline(h = 1, col = "red", lty = 2) 


# Prevalence Ratio by Region Using ggplot
ggplot(link_to_dataset, aes(x = Region, y = Prevalence_Ratio, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Prevalence Ratio by Region",
       x = "Region", y = "Prevalence Ratio") 


# Calculate mean and median for PCR and Microscopy performance
mean(link_to_dataset$PCR_Percent, na.rm = TRUE)
median(link_to_dataset$PCR_Percent, na.rm = TRUE)

mean(link_to_dataset$Microscopy_Percent, na.rm = TRUE)
median(link_to_dataset$Microscopy_Percent, na.rm = TRUE)

# Perform a t-test to compare PCR and Microscopy performance
t.test(link_to_dataset$PCR_Percent, link_to_dataset$Microscopy_Percent, paired = TRUE, na.rm = TRUE)


#Geographical Analysis of Diagnostic Performance # Calculate mean PCR_Percent and Microscopy_Percent by Region
aggregate(cbind(PCR_Percent, Microscopy_Percent) ~ Region, data = link_to_dataset, FUN = mean, na.rm = TRUE)

# Calculate mean PCR_Percent and Microscopy_Percent by Country
aggregate(cbind(PCR_Percent, Microscopy_Percent) ~ Country, data = link_to_dataset, FUN = mean, na.rm = TRUE)

# Box plot comparing PCR and Microscopy performance by Region
boxplot(PCR_Percent ~ Region, data = link_to_dataset, main = "PCR Performance by Region", ylab = "PCR Percent")
boxplot(Microscopy_Percent ~ Region, data = link_to_dataset, main = "Microscopy Performance by Region", ylab = "Microscopy Percent")

#Temporal Trends in Diagnostic Performance
# Line plot showing trends in PCR_Percent over years
plot(link_to_dataset$Year, link_to_dataset$PCR_Percent, type = "b", col = "blue", 
     xlab = "Year", ylab = "PCR Percent", main = "PCR Performance Over Time")


# Finding the minimu and maximum values single variable
min_value <- min(link_to_dataset_clean$Year, na.rm = TRUE)
max_value <- max(link_to_dataset_clean$Year, na.rm = TRUE)

print(paste("Minimum:", min_value))
print(paste("Maximum:", max_value))


ggplot(link_to_dataset_clean, aes(x = Year, y = PCR_Percent)) +
  geom_point(size = 2, alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "solid") +
  labs(
    title = "Trends in PCR-Based Pathogen Detection (1995-2020)",
    x = "Year", 
    y = "PCR Detection Rate (%)",
    subtitle = "Data from [Your Study/Region]"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_x_continuous(breaks = seq(1995, 2020, 5))


# Line plot showing trends in Microscopy_Percent over years
plot(link_to_dataset$Year, link_to_dataset$Microscopy_Percent, type = "b", col = "red", 
     xlab = "Year", ylab = "Microscopy Percent", main = "Microscopy Performance Over Time")


# Check correlation between Year and PCR_Percent
cor(link_to_dataset$Year, link_to_dataset$PCR_Percent, use = "complete.obs")

# Check correlation between Year and Microscopy_Percent
cor(link_to_dataset$Year, link_to_dataset$Microscopy_Percent, use = "complete.obs")


#Impact of Transmission Intensity on Diagnostic Performance
# Group data by Transmission Intensity (e.g., low vs. high) and compare PCR_Percent
str(link_to_dataset$Current_Transmission)
link_to_dataset <- link_to_dataset[!is.na(link_to_dataset$`Current_Transmission`), ]  

low_transmission <- link_to_dataset[link_to_dataset$Current_Transmission == "Low", ]
high_transmission <- link_to_dataset[link_to_dataset$Current_Transmission == "High", ]

# Compare means of PCR_Percent between low and high transmission
mean(low_transmission$PCR_Percent, na.rm = TRUE)
mean(high_transmission$PCR_Percent, na.rm = TRUE)

# Create vectors with NA values removed
low_pcr_clean <- na.omit(low_transmission$PCR_Percent)
high_pcr_clean <- na.omit(high_transmission$PCR_Percent)

cat("Valid observations after NA removal:\n")
cat("Low transmission:", length(low_pcr_clean), "\n")
cat("High transmission:", length(high_pcr_clean), "\n")



# T-test to compare PCR performance between low and high transmission since obsrevations are 0
t.test(low_transmission$PCR_Percent, high_transmission$PCR_Percent, na.rm = TRUE)

# Multiple regression to predict PCR_Percent based on Transmission and other factors
model <- lm(PCR_Percent ~ Current_Transmission + Historical_Transmission + Setting_20 + Setting_15, data = link_to_dataset)
summary(model)

#Prevalence Ratio and Diagnostic Discrepancies
# Calculate the mean of Prevalence_Ratio
mean(link_to_dataset$Prevalence_Ratio, na.rm = TRUE)

# Histogram of Prevalence_Ratio to visualize the distribution
hist(link_to_dataset$Prevalence_Ratio, main = "Distribution of Prevalence Ratio", xlab = "Prevalence Ratio", col = "lightblue")
# Check summary statistics
summary(link_to_dataset$Prevalence_Ratio)

# Test for normality
shapiro.test(link_to_dataset$Prevalence_Ratio)

# Consider log transformation
link_to_dataset$log_prevalence_ratio <- log(link_to_dataset$Prevalence_Ratio + 0.01)  # add small constant to avoid log(0)

#Since your prevalence ratio is a continuous variable, start with:
# Basic linear model
linear_model <- lm(Prevalence_Ratio ~ Region + PCR_Method + Sampling_Season, 
                   data = link_to_dataset)

# Or Gaussian GLM with identity link (same as linear model)
gaussian_model <- glm(Prevalence_Ratio ~ Region + PCR_Method + Sampling_Season, 
                      data = link_to_dataset, 
                      family = "gaussian")
print(linear_model)

summary(linear_model)

#Exploring the Influence of Sampling Season and Setting #Comparison by Sampling Season
# Box plot comparing PCR_Percent and Microscopy_Percent by Sampling_Season
boxplot(PCR_Percent ~ Sampling_Season, data = link_to_dataset, main = "PCR Percent by Sampling Season", ylab = "PCR Percent")

boxplot(PCR_Percent ~ Sampling_Season, data = link_to_dataset,
        main = "PCR Detection Rate by Sampling Season",
        ylab = "PCR Detection Rate (%)",
        xlab = "Sampling Season",
        col = "lightblue",
        border = "darkblue",
        las = 1,  # Make axis labels horizontal
        cex.axis = 0.8,  # Adjust axis label size
        outline = TRUE)  # Show outliers

bp <- boxplot(PCR_Percent ~ Sampling_Season, data = link_to_dataset,
              main = "PCR Detection Rate by Sampling Season",
              ylab = "PCR Detection Rate (%)", 
              xlab = "Sampling Season",
              col = terrain.colors(length(unique(link_to_dataset$Sampling_Season))),
              ylim = c(0, 100))  # Set consistent y-axis for percentages


boxplot(Microscopy_Percent ~ Sampling_Season, data = link_to_dataset, main = "Microscopy Percent by Sampling Season", ylab = "Microscopy Percent")

boxplot(Microscopy_Percent ~ Sampling_Season, data = link_to_dataset,
        main = "Microscopy Detection Rate by Sampling Season",
        ylab = "Microscopy Detection Rate (%)",
        xlab = "Sampling Season",
        col = "lightgreen",
        border = "darkgreen",
        las = 1,
        cex.axis = 0.8,
        outline = TRUE)


#Regression Analysis (Predicting PCR_Percent)
# Multiple regression to predict PCR_Percent based on Setting_20, Sampling_Season, etc.
model2 <- lm(PCR_Percent ~ Setting_20 + Sampling_Season + Historical_Transmission, data = link_to_dataset)
summary(model2)


#Correlation Between PCR and Microscopy Performance
# Pearson correlation between PCR_Percent and Microscopy_Percent
cor(link_to_dataset$PCR_Percent, link_to_dataset$Microscopy_Percent, method = "pearson", use = "complete.obs")

# Spearman correlation (non-parametric)
cor(link_to_dataset$PCR_Percent, link_to_dataset$Microscopy_Percent, method = "spearman", use = "complete.obs")

#Scatter Plot with Regression Line
# Scatter plot with regression line to show correlation between PCR and Microscopy
plot(link_to_dataset$PCR_Percent, link_to_dataset$Microscopy_Percent, 
     xlab = "PCR Percent", ylab = "Microscopy Percent", main = "PCR vs Microscopy Performance")
abline(lm(Microscopy_Percent ~ PCR_Percent, data = link_to_dataset), col = "red")

#Comparing PCR Methods
# Calculate mean PCR_Percent by PCR_Method
aggregate(PCR_Percent ~ PCR_Method, data = link_to_dataset, FUN = mean, na.rm = TRUE)

# ANOVA to test if PCR_Percent differs significantly by PCR_Method
anova_model <- aov(PCR_Percent ~ PCR_Method, data = link_to_dataset)
summary(anova_model)
 
#Evaluating the Effect of Notes on Study Quality
# Compare PCR_Percent between studies with and without notes
notes_present <- link_to_dataset[!is.na(link_to_dataset$Notes), ]
notes_absent <- link_to_dataset[is.na(link_to_dataset$Notes), ]

print(notes_present)

# Mean comparison
mean(notes_present$PCR_Percent, na.rm = TRUE)
mean(notes_absent$PCR_Percent, na.rm = TRUE)




