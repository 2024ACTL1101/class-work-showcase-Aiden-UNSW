ACTL1101 Assignment Part B
Aiden van Domselaar
2024 T2
CAPM Analysis
Introduction
In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

Background
The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

Objectives
Load and Prepare Data: Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
CAPM Implementation: Focus will be placed on applying the CAPM to examine the relationship between AMD’s stock performance and the overall market as represented by the S&P 500.
Beta Estimation and Analysis: Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
Results Interpretation: Analyze the outcomes of the CAPM application, discussing the implications of AMD’s beta in terms of investment risk and potential returns.
Instructions
Step 1: Data Loading
We are using the quantmod package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
quantmod stands for “Quantitative Financial Modelling Framework”. It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
Make sure to install the quantmod package by running install.packages("quantmod") in the R console before proceeding.
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
Data Processing
colSums(is.na(df))
## Date  AMD GSPC   RF 
##    0    0    0    9
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
Step 2: CAPM Analysis
The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

The CAPM Formula
The formula for CAPM is given by:

E(Ri)=Rf+βi(E(Rm)−Rf)

Where:

E(Ri)
 is the expected return on the capital asset,
Rf
 is the risk-free rate,
βi
 is the beta of the security, which represents the systematic risk of the security,
E(Rm)
 is the expected return of the market.
CAPM Model Daily Estimation
Calculate Returns: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
Daily Return=Today's Price−Previous Trading Day's PricePrevious Trading Day's Price
df <- df %>%
  mutate(
    AMD_return = (AMD - lag(AMD)) / lag(AMD),
    GSPC_return = (GSPC - lag(GSPC)) / lag(GSPC)
  )
df <- na.omit(df)
Calculate Risk-Free Rate: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
Daily Risk-Free Rate=(1+Annual Rate100)1360−1
df <- df %>%
  mutate(Daily_RF = (1 + RF / 100)^(1 / 360) - 1)
Calculate Excess Returns: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.
df <- df %>%
  mutate(
    Excess_AMD_return = AMD_return - Daily_RF,
    Excess_GSPC_return = GSPC_return - Daily_RF
  )
Perform Regression Analysis: Using linear regression, we estimate the beta (β
) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock’s returns to fluctuations in the market.
capm_model <- lm(Excess_AMD_return ~ Excess_GSPC_return, data = df)
summary(capm_model)
## 
## Call:
## lm(formula = Excess_AMD_return ~ Excess_GSPC_return, data = df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.095781 -0.014735 -0.001152  0.012276  0.173632 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        0.0011041  0.0007243   1.524    0.128    
## Excess_GSPC_return 1.5699987  0.0540654  29.039   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02567 on 1256 degrees of freedom
## Multiple R-squared:  0.4017, Adjusted R-squared:  0.4012 
## F-statistic: 843.3 on 1 and 1256 DF,  p-value: < 2.2e-16
beta_estimate <- summary(capm_model)$coefficients[2,1]
Interpretation
What is your β
? Is AMD more volatile or less volatile than the market?

Answer: Beta = 1.56999869689412 This means that AMD is about 57% more volatile than the market on average.

Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

ggplot(df, aes(x = Excess_GSPC_return, y = Excess_AMD_return)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
  labs(
    title = "CAPM Analysis: AMD vs. S&P 500 Excess Returns",
    x = "S&P 500 Excess Returns",
    y = "AMD Excess Returns"
  ) +
  theme_minimal()
## `geom_smooth()` using formula = 'y ~ x'


Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD’s annual expected return.

Hint: Calculate the daily standard error of the forecast (sf
), and assume that the annual standard error for prediction is sf×252−−−√
. Use the simple return average method to convert daily stock returns to annual returns if needed.

Answer: $Expected_Annual_Return [1] 0.1803099

$Lower_Bound [1] -0.4904574

$Upper_Bound [1] 0.8510772

The expected return is 18.03%, with a 90% chance of the return being between -49.05% and 85.11%

s_f <- summary(capm_model)$sigma
s_annual <- s_f * sqrt(252)
Rf <- 0.05
E_Rm <- 0.133
E_Ramd <- Rf + beta_estimate * (E_Rm - Rf)

# Number of observations
n <- nrow(df)

# Calculate the t-value for a 90% prediction interval
t_value <- qt(0.95, df = n - 2)  # Two-tailed, so alpha/2 = 0.05

# Calculate the prediction interval
lower_bound <- E_Ramd - t_value * s_annual
upper_bound <- E_Ramd + t_value * s_annual

list(
  Expected_Annual_Return = E_Ramd,
  Lower_Bound = lower_bound,
  Upper_Bound = upper_bound
)
## $Expected_Annual_Return
## [1] 0.1803099
## 
## $Lower_Bound
## [1] -0.4904574
## 
## $Upper_Bound
## [1] 0.8510772
