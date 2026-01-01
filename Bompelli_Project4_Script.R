#File   : Project 4
#Course: Introduction to Analytics
#Professor : Kayal Chandrasekaran

#
#Clean canvas ----
#clears console
cat("\014")
#clears the variables
rm(list=ls())
#clears the plots
graphics.off() 

(.packages())

library(tidyverse)
library(dbplyr)
library(tibble)
library(janitor)

# set working directory
setwd("C:/Users/Omsri/OneDrive/Documents/R-Files")

hr_data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header=TRUE)

# Exploring ----

class(hr_data) # check data type

# Check dimensions
cat("Number of rows:", nrow(hr_data), "\n")
cat("Number of columns:", ncol(hr_data), "\n")

head(hr_data) # head() displays the first 6 rows of data

names(hr_data) # gives all column names of data

## Renaming columns ----
hr_data <- clean_names(hr_data) # standardize column names

## Managing NAs ----
hr_data <- na.omit(hr_data)

## Removing columns ----
# These columns have identical values throughout
hr_data <- select(hr_data, -employee_count, -standard_hours, -over18)

## Reorganizing the data ----
# Removing redundant and inconsistent rate columns 
# Rate columns don't correlate with monthly_income - will derive realistic rates later
hr_data <- select(hr_data, -daily_rate, -hourly_rate, -monthly_rate)

## Correcting data types ----
# Convert categorical variables to factors
hr_data$attrition <- as.factor(hr_data$attrition)
hr_data$business_travel <- as.factor(hr_data$business_travel)
hr_data$department <- as.factor(hr_data$department)
hr_data$education_field <- as.factor(hr_data$education_field)
hr_data$gender <- as.factor(hr_data$gender)
hr_data$job_role <- as.factor(hr_data$job_role)
hr_data$marital_status <- as.factor(hr_data$marital_status)
hr_data$over_time <- as.factor(hr_data$over_time)

# Convert ordinal variables to ordered factors
hr_data$education <- factor(hr_data$education, levels = 1:5, ordered = TRUE)
hr_data$environment_satisfaction <- factor(hr_data$environment_satisfaction, levels = 1:4, ordered = TRUE)
hr_data$job_satisfaction <- factor(hr_data$job_satisfaction, levels = 1:4, ordered = TRUE)
hr_data$work_life_balance <- factor(hr_data$work_life_balance, levels = 1:4, ordered = TRUE)
hr_data$relationship_satisfaction <- factor(hr_data$relationship_satisfaction, levels = 1:4, ordered = TRUE)
hr_data$job_involvement <- factor(hr_data$job_involvement, levels = 1:4, ordered = TRUE)
hr_data$performance_rating <- factor(hr_data$performance_rating, levels = 1:4, ordered = TRUE)
hr_data$job_level <- factor(hr_data$job_level, levels = 1:5, ordered = TRUE)
hr_data$stock_option_level <- factor(hr_data$stock_option_level, levels = 0:3, ordered = TRUE)

# No string manipulation needed in data

# Verify data structure
glimpse(hr_data)
summary(hr_data)

## Descriptive statistics ----

# Attrition analysis 
table(hr_data$attrition)
prop.table(table(hr_data$attrition)) * 100  # calculate percentage

# Gender analysis 
table(hr_data$gender)
prop.table(table(hr_data$gender)) * 100  # calculate percentage


### Create functions for population statistics ----

# Population Variance
pop_var <- function(x) {
  sum((x - mean(x))^2) / length(x)
}

# formula for population SD
pop_sd <- function(x) {
  sqrt(var(x) * (length(x) - 1) / length(x))
}

# Mode (most frequent value)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Coefficient of Variation (CV) - measures relative variability
coef_var <- function(x) {
  (pop_sd(x) / mean(x)) * 100  # expressed as percentage
}

complete_stats <- function(x, var_name = "Variable") {
  cat("Count:", length(x), "\n")
  cat("Mean:", mean(x), "\n")
  cat("Median:", median(x), "\n")
  cat("Mode:", get_mode(x), "\n")
  cat("Min:", min(x), "\n")
  cat("Max:", max(x), "\n")
  cat("Range:", max(x) - min(x), "\n")
  cat("Population Variance:", round(pop_var(x), 2), "\n")
  cat("Population SD:", pop_sd(x), "\n")
  cat("Coefficient of Variation:", round(coef_var(x), 2), "%\n")
  cat("25th Percentile (Q1):", quantile(x, 0.25), "\n")
  cat("50th Percentile (Q2/Median):", quantile(x, 0.50), "\n")
  cat("75th Percentile (Q3):", quantile(x, 0.75), "\n")
}

# Age
complete_stats(hr_data$age, "Age")

# Monthly Income
complete_stats(hr_data$monthly_income, "Monthly Income")

# Years at Company
complete_stats(hr_data$years_at_company, "Years at Company")

# Distance from Home
complete_stats(hr_data$distance_from_home, "Distance from Home")


# Visualizations ----

## 1. Histogram: Age Distribution ----
ggplot(hr_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#2A9D8F", color = "white") +
  scale_x_continuous(breaks = seq(18, 60, by = 5)) +
  labs(title = "Distribution of Employee Age",
       x = "Age (years)",
       y = "Frequency") +
  theme_minimal()

## 2. Histogram: Monthly Income Distribution ----
ggplot(hr_data, aes(x = monthly_income)) +
  geom_histogram(binwidth = 2000, fill = "#2A9D8F", color = "white", boundary = 0) +
  scale_x_continuous(breaks = seq(0, 20000, by = 2000)) +
  labs(title = "Distribution of Monthly Income",
       x = "Monthly Income ($)",
       y = "Frequency") +
  theme_minimal()

## 3. Stacked Bar Chart: Department Distribution by Gender ----
ggplot(hr_data, aes(x = department, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("#264653", "#E76F51")) +
  labs(title = "Employees by Department and Gender",
       x = "Department",
       y = "Number of Employees",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 10, hjust = 1))

## 4. Stacked Bar Chart: Education Field Distribution by Gender ----
ggplot(hr_data, aes(x = education_field, fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c("#264653", "#E76F51")) +
  labs(title = "Education Field Distribution by Gender",
       x = "Education Field",
       y = "Number of Employees",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 5. Boxplot: Monthly Income by Department ----
ggplot(hr_data, aes(x = department, y = monthly_income, fill = department)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E76F51")) +
  labs(title = "Monthly Income by Department",
       x = "Department",
       y = "Monthly Income ($)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

## 6. Boxplot: Total Working Years by Department ----
ggplot(hr_data, aes(x = department, y = total_working_years, fill = department)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E76F51")) +
  labs(title = "Total Working Experience by Department",
       x = "Department",
       y = "Total Working Years") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

## 7. Boxplot: Percent Salary Hike by Department ----
ggplot(hr_data, aes(x = department, y = percent_salary_hike, fill = department)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E76F51")) +
  labs(title = "Salary Hike Percentage by Department",
       x = "Department",
       y = "Percent Salary Hike (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

## 8. Stacked Bar Chart: Job Level Distribution by Department ----
ggplot(hr_data, aes(x = department, fill = job_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#264653", "#287271", "#2A9D8F", "#E9C46A", "#E76F51")) +
  labs(title = "Job Level Distribution by Department",
       x = "Department",
       y = "Percentage",
       fill = "Job Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 9. Histogram: Years at Company Distribution ----
ggplot(hr_data, aes(x = years_at_company)) +
  geom_histogram(binwidth = 5, fill = "#2A9D8F", color = "white", boundary = 0) +
  scale_x_continuous(breaks = seq(0, 40, by = 5)) +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +
  labs(title = "Employee Tenure",
       x = "Years at Company",
       y = "Number of Employees") +
  theme_minimal()

## 10. Pareto Chart - Years Since Last Promotion ----
# Calculate frequencies and cumulative percentages
promotion_data <- hr_data %>%
  count(years_since_last_promotion) %>%
  arrange(years_since_last_promotion) %>%
  mutate(cumulative = cumsum(n),
         cumulative_pct = (cumulative / sum(n)) * 100)

ggplot(promotion_data, aes(x = years_since_last_promotion)) +
  geom_bar(aes(y = n), stat = "identity", fill = "#2A9D8F", color = "white") +
  geom_line(aes(y = cumulative_pct * max(n) / 100), color = "#E76F51", size = 1.5) +
  geom_point(aes(y = cumulative_pct * max(n) / 100), color = "#E76F51", size = 3) +
  scale_y_continuous(
    name = "Number of Employees",
    sec.axis = sec_axis(~ . * 100 / max(promotion_data$n), name = "Cumulative Percentage (%)")
  ) +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  labs(title = "Promotion Frequency",
       x = "Years Since Last Promotion",
       subtitle = "Orange line shows cumulative percentage of employees") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "#E76F51"),
        axis.text.y.right = element_text(color = "#E76F51"))

## 11. Scatter Plot: Years at Company vs Monthly Income (colored by Job Level) ----
ggplot(hr_data, aes(x = years_at_company, y = monthly_income, color = job_level)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#264653", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("#264653", "#287271", "#2A9D8F", "#E9C46A", "#E76F51"),
                     labels = c("Level 1 (Entry)", "Level 2", "Level 3", "Level 4", "Level 5 (Senior)")) +
  labs(title = "Career Growth: Tenure Correlates with Income and Level",
       x = "Years at Company",
       y = "Monthly Income ($)",
       color = "Job Level") +
  theme_minimal() +
  theme(legend.position = "right")

# Derived variables ----

# 1. Salary bands for attrition analysis
hr_data$salary_band <- case_when(
  hr_data$monthly_income < 3000 ~ "Low (<$3K)",
  hr_data$monthly_income < 6000 ~ "Medium ($3K-$6K)",
  hr_data$monthly_income < 10000 ~ "High ($6K-$10K)",
  TRUE ~ "Very High ($10K+)"
)
hr_data$salary_band <- factor(hr_data$salary_band,
                              levels = c("Low (<$3K)", "Medium ($3K-$6K)", 
                                         "High ($6K-$10K)", "Very High ($10K+)"))

# 2. Total satisfaction score
hr_data$total_satisfaction <- (
  as.numeric(hr_data$environment_satisfaction) +
    as.numeric(hr_data$job_satisfaction) +
    as.numeric(hr_data$relationship_satisfaction) +
    as.numeric(hr_data$work_life_balance)
) / 4

hr_data$satisfaction_level <- case_when(
  hr_data$total_satisfaction < 2 ~ "Low (< 2.0)",
  hr_data$total_satisfaction < 3 ~ "Medium (2.0-3.0)",
  TRUE ~ "High (3.0+)"
)
hr_data$satisfaction_level <- factor(hr_data$satisfaction_level,
                                     levels = c("Low (< 2.0)", "Medium (2.0-3.0)", "High (3.0+)"))

# 3. Promotion delay categories
hr_data$promotion_delay <- case_when(
  hr_data$years_since_last_promotion <= 1 ~ "Recent (0-1 yr)",
  hr_data$years_since_last_promotion <= 3 ~ "Normal (2-3 yrs)",
  hr_data$years_since_last_promotion <= 7 ~ "Delayed (4-7 yrs)",
  TRUE ~ "Severely Delayed (8+ yrs)"
)
hr_data$promotion_delay <- factor(hr_data$promotion_delay,
                                  levels = c("Recent (0-1 yr)", "Normal (2-3 yrs)", 
                                             "Delayed (4-7 yrs)", "Severely Delayed (8+ yrs)"))


## 1. Bar Chart: Attrition Rate by Salary Band ----
attrition_by_salary <- hr_data %>%
  group_by(salary_band) %>%
  summarise(
    total = n(),
    attrition_count = sum(attrition == "Yes"),
    attrition_rate = (attrition_count / total) * 100
  ) %>%
  ungroup()

ggplot(attrition_by_salary, aes(x = salary_band, y = attrition_rate, fill = salary_band)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(attrition_rate, 1), "%")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#E76F51", "#E9C46A", "#2A9D8F", "#264653")) +
  labs(title = "Salary Band vs Attrition rate",
       x = "Salary Band",
       y = "Attrition Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

## 2. Bar Chart - Attrition Rate by Satisfaction Level ----
satisfaction_attrition <- hr_data %>%
  group_by(satisfaction_level) %>%
  summarise(
    total = n(),
    attrition_count = sum(attrition == "Yes"),
    attrition_rate = (attrition_count / total) * 100
  ) %>%
  ungroup()

ggplot(satisfaction_attrition, aes(x = satisfaction_level, y = attrition_rate, fill = satisfaction_level)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(attrition_rate, 1), "%")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#E76F51", "#E9C46A", "#264653")) +
  labs(title = "Job Satisfaction vs Attrition rate",
       x = "Overall Satisfaction Level",
       y = "Attrition Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

## 3. Bar Chart: Attrition Rate by Promotion Delay ----
promotion_attrition <- hr_data %>%
  group_by(promotion_delay) %>%
  summarise(
    total = n(),
    attrition_count = sum(attrition == "Yes"),
    attrition_rate = (attrition_count / total) * 100
  ) %>%
  ungroup()

ggplot(promotion_attrition, aes(x = promotion_delay, y = attrition_rate, fill = promotion_delay)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(round(attrition_rate, 1), "%")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#264653", "#2A9D8F", "#E9C46A", "#E76F51")) +
  labs(title = "Last Promotion vs Attrition rate",
       x = "Time Since Last Promotion",
       y = "Attrition Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))



# Hourly rate (assuming 8-hour workday, 22 working days per month)
hr_data$hourly_rate <- round(hr_data$monthly_income / (22 * 8), 2)


# HEATMAP: Hourly Rate × Department × Job Level

## Calculate average hourly rate by department and job level ----
hourly_dept_level <- hr_data %>%
  group_by(department, job_level) %>%
  summarise(
    avg_hourly_rate = mean(hourly_rate),
    count = n(),
    .groups = "drop"
  )

## 4. Create heatmap ----
ggplot(hourly_dept_level, aes(x = job_level, y = department, fill = avg_hourly_rate)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = paste0(round(avg_hourly_rate, 1), "\n(n=", count, ")")), 
            color = "white", fontface = "bold", size = 5) +
  scale_fill_gradient2(low = "#264653", mid = "#E9C46A", high = "#E76F51", 
                       midpoint = 40) +
  labs(title = "Hourly Rate: Department × Job Level",
       x = "Job Level",
       y = "Department",
       fill = "Avg Hourly\nRate") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Clean up ----
#clears console
cat("\014")
#clears the variables
rm(list=ls())
#clears the plots
dev.off() 


