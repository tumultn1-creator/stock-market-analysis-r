#################################################
# ETF Portfolio Performance Analysis
# Author: Nathan Tumulty
# Purpose: Compare ETF performance, risk, and growth
# ETFs: VOO, VTI, QQQM, VUG, VXUS, VT
#################################################

# Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

#################################################
# Import Data
#################################################

Stock_Market_Analysis <- read.csv("Stock_Market_Analysis.csv")

#################################################
# Data Overview
#################################################

View(Stock_Market_Analysis)

glimpse(Stock_Market_Analysis)

nrow(Stock_Market_Analysis)

unique(Stock_Market_Analysis$ETF)

#################################################
# ETF Performance Summary
#################################################

summary_table <- Stock_Market_Analysis %>%
  group_by(ETF) %>%
  summarise(
    avg_daily_return = mean(`Daily Return`, na.rm = TRUE),
    volatility = sd(`Daily Return`, na.rm = TRUE),
    annual_return = (1 + avg_daily_return)^252 - 1,
    sharpe = annual_return / (volatility * sqrt(252))
  )

View(summary_table)

#################################################
# Sharpe Ratio Comparison
#################################################

ggplot(summary_table, aes(x = ETF, y = sharpe)) +
  geom_bar(stat = "identity") +
  labs(
    title = "ETF Sharpe Ratios",
    x = "ETF",
    y = "Sharpe Ratio"
  )

#################################################
# Growth of $10,000 Investment
#################################################

Stock_Market_Analysis <- Stock_Market_Analysis %>%
  group_by(ETF) %>%
  arrange(Date) %>%
  mutate(
    Daily_Return = ifelse(is.na(`Daily Return`), 0, `Daily Return`)
  )

growth_data <- Stock_Market_Analysis %>%
  group_by(ETF) %>%
  arrange(Date) %>%
  mutate(
    cum_return = cumprod(1 + Daily_Return),
    portfolio_value = 10000 * cum_return
  )

ggplot(growth_data, aes(x = Date, y = portfolio_value, color = ETF)) +
  geom_line() +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Growth of $10,000 by ETF",
    x = "Date",
    y = "Portfolio Value ($)"
  )

#################################################
# VOO Benchmark Comparison
#################################################

benchmark_data <- Stock_Market_Analysis %>%
  filter(ETF == "VOO") %>%
  arrange(Date) %>%
  mutate(
    benchmark_cum = cumprod(1 + Daily_Return),
    benchmark_value = 10000 * benchmark_cum
  )

ggplot() +
  geom_line(
    data = growth_data,
    aes(x = Date, y = portfolio_value, color = ETF)
  ) +
  geom_line(
    data = benchmark_data,
    aes(x = Date, y = benchmark_value),
    linewidth = 1.2
  ) +
  labs(
    title = "ETF Growth vs S&P 500 Benchmark",
    x = "Date",
    y = "Portfolio Value ($)"
  )

#################################################
# Rank ETFs by Sharpe Ratio
#################################################

summary_table %>%
  arrange(desc(sharpe))
