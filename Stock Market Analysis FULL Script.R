ew(Stock_Market_Analysis)
> library(tidyverse)
── Attaching core tidyverse packages ───────────
✔ dplyr     1.2.1     ✔ readr     2.2.0
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.3     ✔ tibble    3.3.1
✔ lubridate 1.9.5     ✔ tidyr     1.3.2
✔ purrr     1.2.2     
── Conflicts ────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
Warning messages:
  1: package ‘tidyverse’ was built under R version 4.6.1 
2: package ‘readr’ was built under R version 4.6.1 
3: package ‘lubridate’ was built under R version 4.6.1 

> view(Stock_Market_Analysis)
> glimpse(Stock_Market_Analysis)
Rows: 7,530
Columns: 10
$ ETF                   <chr> "VOO", "VOO", "V…
$ Date                  <dttm> 2026-06-30, 202…
$ Open                  <dbl> 681.32, 677.01, …
$ High                  <dbl> 687.52, 681.57, …
$ Low                   <dbl> 680.99, 672.93, …
$ Close                 <dbl> 686.81, 681.01, …
$ `Adjusted Close`      <dbl> 686.81, 681.01, …
$ Volume                <dbl> 6004801, 5703800…
$ `Prev Adjusted Close` <dbl> 0.8734588, 0.857…
$ `Daily Return`        <lgl> NA, NA, NA, NA, …
> Stock_Market_Analysis %>%
+     filter(ETF == "VOO") %>%
+     select(Date, `Adjusted Close`, Daily_Return) %>%
+     head(10)
Adding missing grouping variables: `ETF`
# A tibble: 10 × 4
# Groups:   ETF [1]
   ETF   Date                `Adjusted Close`
   <chr> <dttm>                         <dbl>
 1 VOO   2021-06-30 00:00:00             367.
 2 VOO   2021-07-01 00:00:00             369.
 3 VOO   2021-07-02 00:00:00             371.
 4 VOO   2021-07-06 00:00:00             371.
 5 VOO   2021-07-07 00:00:00             372.
 6 VOO   2021-07-08 00:00:00             369.
 7 VOO   2021-07-09 00:00:00             373.
 8 VOO   2021-07-12 00:00:00             374.
 9 VOO   2021-07-13 00:00:00             373.
10 VOO   2021-07-14 00:00:00             374.
# ℹ 1 more variable: Daily_Return <dbl>
> nrow(Stock_Market_Analysis)
[1] 7530
> unique(Stock_Market_Analysis$ETF)
[1] "VOO"  "VTI"  "QQQM" "VUG"  "VXUS" "VT"  
> library(dplyr)
> 
> summary_table <- Stock_Market_Analysis %>%
+     group_by(ETF) %>%
+     summarise(
+         avg_daily_return = mean(Daily_Return, na.rm = TRUE),
+         volatility = sd(Daily_Return, na.rm = TRUE),
+         annual_return = (1 + avg_daily_return)^252 - 1,
+         sharpe = annual_return / (volatility * sqrt(252))
+     )
> View(summary_table)
> View(Stock_Market_Analysis)
> library(ggplot2)
> ggplot(summary_table, aes(x = ETF, y = sharpe)) +
+     geom_bar(stat = "identity") +
+     labs(title = "ETF Sharpe Ratios", x = "ETF", y = "Sharpe Ratio")
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
Stock_Market_Analysis <- Stock_Market_Analysis %>%
  group_by(ETF) %>%
  arrange(Date) %>%
  mutate(Daily_Return = ifelse(is.na(Daily_Return), 0, Daily_Return))
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
benchmark_data <- Stock_Market_Analysis %>%
  filter(ETF == "VOO") %>%
  arrange(Date) %>%
  mutate(
    benchmark_cum = cumprod(1 + Daily_Return),
    benchmark_value = 10000 * benchmark_cum
  )
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
library(scales)
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
  summary_table %>%
  arrange(desc(sharpe))
