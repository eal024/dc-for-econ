
# Set seed for reproducibility
set.seed(123)

# Number of rows and columns
num_rows <- 60
num_columns <- 100

# Generate a matrix of random values between 0 and 1
dummy_data <- matrix(runif(num_rows * num_columns), nrow = num_rows)

# Create a correlation matrix with positive correlations for economic indicators
cor_matrix <- matrix(0.5, ncol = num_columns, nrow = num_columns)
diag(cor_matrix) <- 1  # Diagonal elements set to 1
cor_matrix[1, 2:num_columns] <- 0.7  # Positive correlation with the outcome variable

# Multiply the data matrix by the Cholesky decomposition of the correlation matrix
dummy_data <- dummy_data %*% chol(cor_matrix)

# Convert to data frame
dummy_data_df <- as.data.frame(dummy_data)

# Add column names
word_list <- c("Outcome", "Unemployment", "Interest", "Upturn", "Downturn", "HighConsumption", 
               "GDP", "Inflation", "Investment", "LaborMarket", "Productivity", 
               "FinancialStability", "Consumption", "StockMarket", "HousingMarket", 
               "GovernmentSpending", "Exports", "Imports", "TradeBalance", 
               "BusinessConfidence", "ConsumerConfidence", "Wages", "TaxPolicy", 
               "ExchangeRate", "Infrastructure", "MonetaryPolicy", "FiscalPolicy", 
               "EconomicGrowth", "Debt", "Savings", "Retirement", 
               "GlobalEconomy", "Technology", "Innovation", "Education", 
               "Healthcare", "ClimateChange", "Sustainability", "Globalization", 
               "Demographics", "Entrepreneurship", "Poverty", "Equality", 
               "LaborForceParticipation", "ForeignInvestment", "Regulation", 
               "MarketCompetitiveness", "IncomeDistribution", "GovernmentDebt", 
               "InterestRates", "Commodities", "MarketVolatility", 
               "PublicInvestment", "PrivateInvestment", "EconomicIndicators", 
               "EconomicOutlook", "EconomicCycle", "FinancialCrises", 
               "CreditConditions", "BusinessCycle", "EconomicRecovery", 
               "EconomicIndicators", "EconomicPolicy", "EconomicForecast", 
               "EconomicRisk", "EconomicOpportunity", "EconomicResilience", 
               "EconomicStimulus", "EconomicSustainability", "EconomicTrends", 
               "EconomicUncertainty", "EconomicProspects", "EconomicDevelopment", 
               "EconomicIntegration", "EconomicPerformance", "EconomicInstability", 
               "EconomicDiversification", "EconomicEmpowerment", 
               "EconomicTransformation", "EconomicInclusion", "EconomicChallenges", 
               "EconomicCompetitiveness", "EconomicProsperity", "EconomicWellbeing", 
               "EconomicRevitalization", "EconomicInitiatives", 
               "EconomicTransformation", "EconomicInnovation", 
               "EconomicOpportunities", "EconomicRevival", "EconomicProspects", "EconomicStability", "EconomicSecurity", 
                   "EconomicOpportunity", "EconomicPotential", "EconomicReform", 
                   "EconomicSustainability", "EconomicTransformation", 
                   "EconomicWellbeing", "EconomicEmpowerment"
                   )


dummy_data_df <- as_tibble(dummy_data_df) |> set_names(word_list)


# Machine learning 
# dummy_data_df  |> data.table::fwrite("data/ml_data.csv")