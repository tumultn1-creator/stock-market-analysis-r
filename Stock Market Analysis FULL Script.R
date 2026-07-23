# Load Libraries
View(Stock_Market_Analysis)
 library(tidyverse)
library(scales)

# Explore the Data
 glimpse(Stock_Market_Analysis)

# Explore the Data
 Stock_Market_Analysis %>%
     filter(ETF == "VOO") %>%
     select(Date, `Adjusted Close`, Daily_Return) %>%
     head(10)

# Number of Observations
 nrow(Stock_Market_Analysis)
 
# ETFs included in the Data-set
 unique(Stock_Market_Analysis$ETF)

 # Data Cleaning
 Stock_Market_Analysis <- Stock_Market_Analysis %>%
   group_by(ETF)%>%
   arrange(Date)%>%
   mutate(Daily_Return = ifelse(is.na(Daily_Return),0,Daily_Return)) %>%
   ungroup()
 
# Summary Statistics
   summary_table <- Stock_Market_Analysis %>%
     group_by(ETF) %>%
     summarise(
         avg_daily_return = mean(Daily_Return, na.rm = TRUE),
         volatility = sd(Daily_Return, na.rm = TRUE),
         annual_return = (1 + avg_daily_return)^252 - 1,
         sharpe = annual_return / (volatility * sqrt(252))
     )

# Sharpe Ratio Chart
 ggplot(summary_table, aes(x = ETF, y = sharpe)) +
     geom_bar(stat = "identity") +
     labs(title = "ETF Sharpe Ratios", x = "ETF", y = "Sharpe Ratio")

# Growth of $10,000 
 growth_data <- Stock_Market_Analysis %>%
  group_by(ETF) %>%
  arrange(Date) %>%
  mutate(cum_return = cumprod(1 + Daily_Return),
         portfolio_value = 10000 * cum_return)
ggplot(growth_data, aes(x = Date, y = portfolio_value, color = ETF)) +
  geom_line() +
  labs(title = "Growth of $10,000 by ETF",
       x = "Date",
       y = "Portfolio Value ($)")

# Benchmark
benchmark_data <- Stock_Market_Analysis %>%
  filter(ETF == "VOO") %>%
  arrange(Date) %>%
  mutate(
    benchmark_cum = cumprod(1 + Daily_Return),
    benchmark_value = 10000 * benchmark_cum
  )

# ETF Growth vs Benchmark  
ggplot() +
  geom_line(data = growth_data,
            aes(x = Date, y = portfolio_value, color = ETF)) +
  geom_line(data = benchmark_data,
            aes(x = Date, y = benchmark_value),
            color = "black",
            linewidth = 1.2) +
  labs(title = "ETF Growth vs S&P 500 Benchmark",
       x = "Date",
       y = "Portfolio Value ($)")

# Add Dollar Signs to Visual
ggplot() +
  geom_line(data = growth_data,
            aes(x = Date, y = portfolio_value, color = ETF, group = ETF),
            alpha = 0.7) +
  geom_line(data = benchmark_data,
            aes(x = Date, y = benchmark_value, group = 1),
            color = "black",
            linewidth = 1.2) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "ETF Growth vs S&P 500 Benchmark",
    x = "Date",
    y = "Portfolio Value ($)"
  )
 
# Rank ETFs by Sharpe Ratio
 summary_table %>%
  arrange(desc(sharpe))
