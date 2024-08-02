Trading Algorithm Final
Aiden van Domselaar
2024-06-15
Step 1: Data Loading
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
##Plotting the Data

plot(amd_df$date, amd_df$close,'l')


Step 2: Trading Algorithm
# Initialize columns for trade type, costs/proceeds, and accumulated shares
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  if (previous_price == 0) {
    # First buy
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (current_price < previous_price) {
    # Buy if current price is less than previous price
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
  } else if (i == nrow(amd_df)) {
    # Sell all shares on the last day
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares
    accumulated_shares <- 0
  } else {
    # No trade
    amd_df$trade_type[i] <- "hold"
    amd_df$costs_proceeds[i] <- 0
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}
Step 3: Customize Trading Period
Define a trading period you wanted in the past five years
# Filter data for the last five years (if the data goes up to 2024)
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2024-12-31")
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
Step 4: Results
# Calculate total profit/loss
total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)

# Calculate invested capital (sum of costs for 'buy' trades)
total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)

# Calculate ROI
ROI <- (total_profit_loss / total_invested_capital) * 100

# Print results
total_profit_loss
## [1] 4658489
total_invested_capital
## [1] 5505757
ROI
## [1] 84.61124
Step 5: Profit-Taking Strategy
# This algorithm implements a profit-taking strategy that will sell half of shares if shares rise to 20% over their average buy price.

# Initialize columns for trade type, costs/proceeds, and accumulated shares
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_cost <- 0
num_buys <- 0

# Profit threshold for profit-taking strategy
profit_threshold <- 0.20  # 20%


for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  
  # Calculate average purchase price
  if (accumulated_shares > 0) {
    
    average_purchase_price <- total_cost / accumulated_shares
  } else {
    average_purchase_price <- 0
  }

  # Calculate the sell price threshold for profit-taking
  sell_price_threshold <- average_purchase_price * (1 + profit_threshold)
  
  if (previous_price == 0) {
    # First buy
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    total_cost <- total_cost + current_price * share_size
    num_buys <- num_buys + 1
  } else if (current_price >= sell_price_threshold && accumulated_shares > 0) {
    # Sell half of the holdings if price exceeds the profit threshold
    shares_to_sell <- accumulated_shares / 2
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- current_price * shares_to_sell
    accumulated_shares <- accumulated_shares - shares_to_sell
  } else if (current_price < previous_price) {
    # Buy if current price is less than previous price
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    total_cost <- total_cost + current_price * share_size
    num_buys <- num_buys + 1
  } else if (i == nrow(amd_df)) {
    # Sell all shares on the last day
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares
    accumulated_shares <- 0
  } else {
    # No trade
    amd_df$trade_type[i] <- "hold"
    amd_df$costs_proceeds[i] <- 0
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}
# Calculate updated total profit/loss and ROI after implementing the strategy
updated_total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)

# Calculate invested capital (sum of costs for 'buy' trades)
updated_total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)

# Calculate updated ROI
updated_ROI <- (updated_total_profit_loss / updated_total_invested_capital) * 100

# Print updated and original results
total_profit_loss
## [1] 4658489
total_invested_capital
## [1] 5505757
ROI
## [1] 84.61124
updated_total_profit_loss
## [1] 2865422
updated_total_invested_capital
## [1] 5505757
updated_ROI
## [1] 52.04411
Step 6: Summarize Your Findings
# The profit-taking strategy ended up with reduced total profit of $2 865 422 compared to the original profit of $4 658 489. Consequently, the profit-taking algorithm had a lower return on investment of 52.04 compared to the original strategy having an ROI of 84.61.

# AMD shares have risen significantly since Q4 of 2023, largely due to the general AI and semiconductor boom in the stock marker, fueled by AMD CEO Lisa Su's comments on Wednesday, December 6, 2023, regarding a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. This large rise in share price meant that the profit-taking strategy sold shares while prices continued to rise, resulting it having a lower profit and ROI.
