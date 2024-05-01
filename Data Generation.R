library(MASS)  # For generating multivariate normal distributions
library(truncnorm)
library(dplyr)
library(reshape2)




# Parameters
n <- 100000
prob_male <- 0.48
mean_age <- 37.5
std_dev_age <- sqrt(500)
cor_age_income <- 0.30
cor_gender_income <- 0.07

# Seed for reproducibility
set.seed(1234)

# Generate age and gender non-deterministically
age <- rnorm(n, mean = mean_age, sd = std_dev_age)
age <- pmax(pmin(age, 100), 0)  # Constrain age within 0 to 100
gender <- rbinom(n, 1, prob_male)  # 1 for Male, 0 for Female

# Define the correlation matrix
cor_matrix <- matrix(c(
  1, 0, cor_age_income,  # Age with itself, age with gender, age with income
  0, 1, cor_gender_income,  # Gender with age, gender with itself, gender with income
  cor_age_income, cor_gender_income, 1  # Income with age, income with gender, income with itself
), nrow = 3, byrow = TRUE)

# Generate data
data <- mvrnorm(n, mu = c(mean_age, 0.5, 0), Sigma = cor_matrix)  # Mean income set to 0 for transformation

# Log-normal distribution parameters
log_mean_income <- log(45000) - 0.5 * (0.75^2)  # Shifting the log mean
sd_log_income <- 0.75  # Standard deviation for log-normal

# Generate log-normal income
income_log_normal <- rlnorm(n, meanlog = log_mean_income, sdlog = sd_log_income)

# Apply gender-specific adjustments
income <- ifelse(gender == 1, income_log_normal * 1.14, income_log_normal * 0.86)

# Smoothing and applying minimum income
base_min_income <- min(income[age >= 18 & age <= 65])
scale_factor <- 37500 - base_min_income
income[age >= 18 & age <= 65] <- income[age >= 18 & age <= 65] + scale_factor

# Apply age-specific income rules
income[age < 15] <- 0
income[age > 65] <- rnorm(sum(age > 65), mean = 25000, sd = 5000)

# Compile the final data frame
final_data <- data.frame(
  Age = round(age),
  Gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
  Income = round(income)
)
######################################################################################################

# Create a new column 'Employment Status' based on the given conditions
final_data$Employment_Status <- ifelse(final_data$Age < 18, "Outside of the Labor Force",
                                       ifelse(final_data$Age > 65, "Outside of the Labor Force",
                                              ifelse(final_data$Income > 0, "Employed", "Unemployed")))

######################################################################################################

# Adding the 'Marital Status' column based on age and probabilistic assignment
final_data$Marital_Status <- ifelse(final_data$Age < 18, "Single",
                                    ifelse(runif(nrow(final_data)) < 0.66, "Married", "Single"))
######################################################################################################
# Generate normally distributed education years with increased spread
education_years_raw <- rnorm(nrow(final_data), mean = 14, sd = 3)

# Apply initial non-negative filter and ensure no education for those under 6 years
final_data$Education_Years <- pmax(education_years_raw, 0)
final_data$Education_Years <- ifelse(final_data$Age < 6, 0, final_data$Education_Years)

# Capping education years at 'Age - 6' or 30, whichever is less
final_data$Education_Years <- pmin(final_data$Education_Years, ifelse(final_data$Age >= 6, final_data$Age - 6, 0), 30)

# Enhance the upper tail of the distribution
# Identify potential candidates for an increased education years who are already likely to have higher education
high_education_indices <- which(final_data$Education_Years > 18 & runif(nrow(final_data)) < 0.3)
final_data$Education_Years[high_education_indices] <- final_data$Education_Years[high_education_indices] + rexp(length(high_education_indices), rate = 0.5)

# Ensure the adjusted values do not exceed 30 or the age constraint
final_data$Education_Years <- pmin(final_data$Education_Years, ifelse(final_data$Age >= 6, final_data$Age - 6, 0), 30)

# Ensure all values are rounded and no negative values exist
final_data$Education_Years <- round(pmax(final_data$Education_Years, 0))

# Quick overview to check results
summary(final_data$Education_Years)
hist(final_data$Education_Years, breaks = 30, main = "Distribution of Education Years", xlab = "Education Years")


######################################################################################################

# Define race categories and their respective fixed probabilities
race_categories <- c("White", "Asian", "Indigenous", "Black",
                     "Latin", "Oceanian", "Other")

# Base probabilities defined as per your input
base_probs <- c(White = 0.695, Asian = 0.173, North_American_Indigenous = 0.05, 
                Black = 0.043, Latin_Central_and_South_American = 0.02, Oceanian = 0.003, Other = 0.016)

# Assign race based on fixed probabilities
final_data$Race <- sample(race_categories, size = nrow(final_data), replace = TRUE, prob = base_probs)

# Verify the assignment by checking the distribution of races
table(final_data$Race)  # To check the distribution matches the desired percentages

# Define baseline income adjustments based on race
base_adjustments <- c(White = 1.15, Asian = 1.175, North_American_Indigenous = 0.8, 
                      Black = 0.85, Latin_Central_and_South_American = 0.89, 
                      Oceanian = 0.99, Other = 0.90)

# Applying income adjustments with random variability (noise)
# We will use a normal distribution for noise with a small standard deviation, e.g., 5% of the current income
final_data$Adjusted_Income <- final_data$Income * (base_adjustments[final_data$Race] + rnorm(nrow(final_data), 0, 0.05))

# Define a function to adjust income based on education years, including noise
adjust_income_by_education <- function(education, income) {
  year_diff <- education - 14  # Difference from the benchmark of 14 years
  if (year_diff < 0) {
    # Negative correlation: Decrease income by 7% for each year below 14
    adjustment_factor <- (1 - 0.07) ^ abs(year_diff)
  } else {
    # Positive correlation: Increase income by 7% for each year above 14
    adjustment_factor <- (1 + 0.07) ^ year_diff
  }
  # Introduce random noise in the adjustment
  noise <- rnorm(1, mean = 0, sd = 0.13)  # 13% standard deviation as an example
  adjusted_income <- income * adjustment_factor * (1 + noise)
  return(adjusted_income)
}
######################################################################################################

# Apply the adjustment to each individual's income based on their education
final_data$Adjusted_Income <- mapply(adjust_income_by_education, final_data$Education_Years, final_data$Income)

final_data[] <- lapply(final_data, function(x) if(is.numeric(x)) as.integer(x) else x)

# Delete the original 'Income' column
final_data$Income <- NULL

# Rename 'Adjusted_Income' to 'Income'
names(final_data)[names(final_data) == "Adjusted_Income"] <- "Income"

######################################################################################################
# Assigning base credit score based on race
get_base_score_by_race <- function(race) {
  race_scores <- c(Asian = 745, White = 734, Latin = 701, Black = 677, Indigenous = 651, Other = 730)
  if (race %in% names(race_scores)) {
    return(race_scores[race])
  } else {
    return(730)  # Default for 'Other' or unspecified categories
  }
}

# Function to assign base credit score based on age
get_base_score_by_age <- function(age) {
  if (age >= 60) 749
  else if (age >= 50) 706
  else if (age >= 40) 684
  else if (age >= 30) 673
  else if (age >= 20) 662
  else 662  # Default for under 20
}

# Function to assign base credit score based on income
get_base_score_by_income <- function(income) {
  if (income >= 150000) 775
  else if (income >= 70000) 753
  else if (income >= 40000) 716
  else 664  # Income under 40000
}




# Adjust the blending function to constrain within realistic boundaries
blend_scores <- function(age_score, income_score, race_score) {
  # Calculate mean score from provided statistics
  mean_score <- (age_score * 0.2 + income_score * 0.4 + race_score * 0.4)
  
  # Generate noise with truncnorm to keep within acceptable boundaries
  # The sd = 30 is chosen based on typical credit score fluctuations; adjust as necessary
  noise <- rtruncnorm(1, a = -400, b = 150, mean = 0, sd = 100)
  
  # Calculate final score with noise and ensure it stays within the 300-900 range
  final_score <- mean_score + noise
  return(min(max(final_score, 300), 900))
}

# Apply the function across the dataset
final_data$Credit_Score <- mapply(function(age, income, race) {
  age_score <- get_base_score_by_age(age)
  income_score <- get_base_score_by_income(income)
  race_score <- get_base_score_by_race(race)
  blend_scores(age_score, income_score, race_score)
}, final_data$Age, final_data$Income, final_data$Race)

# Optional: Calculate averages to confirm they match expected values
mean(final_data$Credit_Score[final_data$Race == "Indigenous"])  # Should be around 745

# Optionally, you can plot the distribution to see how it looks
hist(final_data$Credit_Score, breaks = 50, main = "Distribution of Credit Scores", xlab = "Credit Score")
#####################################################################################################################



# Real Estate probability by age
final_data$Real_Estate_Held <- ifelse(final_data$Age < 18, 0,
                                      sapply(final_data$Age, function(age) {
                                        prob <- 1.1 * 2 ^ (age - 18)
                                        rbinom(1, 1, min(prob/100, 1))
                                      }))

# Bonds probability by age
final_data$Bonds_Held <- ifelse(final_data$Age < 18, 0,
                                sapply(final_data$Age, function(age) {
                                  prob <- 1.2 * 2 ^ (age - 18)
                                  rbinom(1, 1, min(prob/100, 1))
                                }))

# Stocks held, normal distribution probability
final_data$Stocks_Held <- rbinom(nrow(final_data), 1, pnorm(0.4, mean = 0.4, sd = 0.225))

# Crypto, inversely related to age starting from 75% at age 18
final_data$Crypto_Held <- ifelse(final_data$Age < 18, 0,
                                 sapply(final_data$Age, function(age) {
                                   prob <- 75 - 1.2 * (age - 18)
                                   rbinom(1, 1, max(prob/100, 0))
                                 }))

# Calculate total portfolio size (dummy values assuming unit value assets)
final_data$Portfolio_Size <- rowSums(final_data[, c("Real_Estate_Held", "Bonds_Held", "Stocks_Held", "Crypto_Held")])

# Check the structure and head of the final_data to see the new columns
str(final_data)
head(final_data)

# Optionally, review the distribution of each asset type
summary(final_data[c("Real_Estate_Held", "Bonds_Held", "Stocks_Held", "Crypto_Held", "Portfolio_Size")])
############################################################################################################################

final_data$ID <- seq_len(nrow(final_data))
# Create a data frame to hold historical asset data
years <- 2014:2024
historical_assets <- expand.grid(ID = final_data$ID, Year = years)
historical_assets$Asset_Value <- NA  # Initialize the asset values column

# Define base asset values for 2014 based on individual's age and income
historical_assets$Asset_Value[historical_assets$Year == 2014] <- mapply(function(age, income) {
  base_value <- ifelse(age >= 18, (age - 18) * income * runif(1, min = 0.05, max = 0.5), 0)
  base_value
}, age = final_data$Age, income = final_data$Income)

# Apply yearly trends and noise
for (year in min(years):max(years)) {
  if (year > min(years)) {
    prior_year_values <- historical_assets$Asset_Value[historical_assets$Year == (year - 1)]
    adjustment_factor <- case_when(
      year <= 2017 ~ 1.07,  # Upward trend from 2014 to 2017
      year == 2018 ~ 0.95,  # Downward trend in 2018
      year <= 2020 ~ 1.07,  # Upward trend from 2018 to 2020
      year == 2021 ~ 0.75,  # Huge downward spike in 2021
      year == 2022 ~ 1.28,  # Recovery in 2022
      TRUE ~ 1.07           # Slight upward trend from 2022 to 2024
    )
    noise <- rnorm(length(prior_year_values), mean = 0, sd = 0.03)
    historical_assets$Asset_Value[historical_assets$Year == year] <- prior_year_values * adjustment_factor * (1 + noise)
  }
}


# Convert historical_assets from long to wide format
historical_assets_wide <- dcast(historical_assets, ID ~ Year, value.var = "Asset_Value")

# Check the structure of the new wide format data
str(historical_assets_wide)

# Merge the wide-format historical data with the main dataset
final_data <- merge(final_data, historical_assets_wide, by = "ID", all.x = TRUE)
################################################################################################################################


# Create a basic function that simulates transactions based on a few parameters
simulate_transactions <- function(age, income, portfolio_size, credit_score) {
  # Calculate factors based on conditions (vectorized)
  age_factor <- ifelse(age <= 30, 1.2, ifelse(age <= 50, 1, 0.8))
  income_factor <- ifelse(income >= median(final_data$Income, na.rm = TRUE), 1.2, 0.8)
  portfolio_factor <- ifelse(portfolio_size >= median(final_data$Portfolio_Size, na.rm = TRUE), 1.2, 0.8)
  credit_factor <- ifelse(credit_score >= 700, 1.1, 0.9)
  
  # Base transactions influenced by factors
  base_transactions <- 5 * age_factor * income_factor * portfolio_factor * credit_factor
  noisy_transactions <- rnorm(length(base_transactions), base_transactions, base_transactions * 0.3)  # Reduced noise factor for simplicity
  
  # Ensure non-negative and at least one transaction
  total_transactions <- pmax(1, round(noisy_transactions))
  
  # Calculate transaction amounts
  avg_transaction_amounts <- runif(length(base_transactions), 0.05 * portfolio_size, 0.15 * portfolio_size) / total_transactions
  
  list(Total_Transactions = total_transactions, Avg_Transaction_Amount = avg_transaction_amounts)
}

# Apply the function to the dataset
transaction_data <- simulate_transactions(final_data$Age, final_data$Income, final_data$Portfolio_Size, final_data$Credit_Score)
final_data$Total_Transactions <- transaction_data$Total_Transactions
final_data$Avg_Transaction_Amount <- pmin(transaction_data$Avg_Transaction_Amount, final_data$Portfolio_Size * 0.95)

final_data$Total_Avg_Transaction_Value <- final_data$'2024' * final_data$Avg_Transaction_Amount
##############################################################################################################################
# Create the 'Has_Retirement_Account' column based on the specified condition
final_data$Has_Retirement_Account <- as.integer(final_data$`2024` > 2 * final_data$Income)
##############################################################################################################################

# Calculate percentiles of credit scores
credit_score_cuts <- quantile(final_data$Credit_Score, probs = c(0.1, 0.4, 0.6, 0.9))

# Define savings - let's use 'Portfolio_Size_2024' as a proxy for savings
final_data$Savings <- final_data$`2024`  # Assume this is defined, or replace with an actual 'Savings' column if available

# Generate debt based on savings and credit score percentiles
final_data$Total_Debt <- mapply(function(credit_score, savings) {
  if (credit_score <= credit_score_cuts[1]) {  # Bottom 10%
    debt = savings * 3
  } else if (credit_score <= credit_score_cuts[2]) {  # 10th to 40th percentile
    debt = savings * 1.5
  } else if (credit_score <= credit_score_cuts[3]) {  # 40th to 60th percentile
    debt = savings * 0.9
  } else if (credit_score <= credit_score_cuts[4]) {  # 60th to 90th percentile
    debt = savings * 0.25
  } else {  # Above 90th percentile
    debt = 0
  }
  # Add noise to simulate variability
  return(debt + rnorm(1, mean = 0, sd = debt * 0.20))  # Adding 10% of the debt as noise
}, credit_score = final_data$Credit_Score, savings = final_data$Savings)

# Ensure debts are not negative due to noise
final_data$Total_Debt <- pmax(final_data$Total_Debt, 0)
########################################################################################################################################################
# Save final_data to a CSV file

write.csv(final_data, "C:/Users/Guill/OneDrive/Documents/simulated_bank_data.csv", row.names = FALSE)

