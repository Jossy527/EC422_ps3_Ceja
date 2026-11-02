library(tidyverse)
setwd("C:/Users/Jossy/OneDrive/Desktop/EC422/EC422_ps3_Ceja/data")
retail_raw <- read_csv("retail_sales_raw.csv")
head(retail_raw)

retail_raw <- read_csv(
  "retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), 
               names_to = "series_id", values_to = "sales")
head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_ts)
has_gaps(retail_ts)

head(canadian_gas)
retail_ts

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)
has_gaps(can_gas)
head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))
head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))
head(can_gas_q)
can_gas_q |> autoplot(Volume) 

#We sum() the monthly values because they represent the total amount produced 
#each month;instead, a mean() would just give the "typical month" value. Yes, 
#quarter series still shows seasonality because production differs between
#quarters, meaning that seasonal patterns are still observable.

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

#The NSA series shows a repeating seasonal pattern every year, in which in 
#December sales spike and then drop every January. The SA series follows the 
#same pattern because the adjustment does not change the movement in sales.
#The SA series is in fact smoother than the NSA series, but it follows the same
#behavior in the long-run.

can_gas |> 
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = "Month",
    y = "Billions of cubic meters"
  ) +
  theme_minimal()

retail_ts |>
  autoplot(sales) +
  geom_smooth(se = FALSE) +
  labs(
    title = "U.S. Retail and Food Services Sales with Trend Line",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
#I decided to add the geom_smooth() to highlight the trend in sales to make the
#the information clear and easier to observe.

#Part 3: Data Transformations

#3.1a.The seasonal swings do in fact get larger as the series grows, with spikes
#and drops over the years. There is an upward increase over the sample series,
#but it is not constant. It increases rapidly during the 1960s and early 1970s,
#grows slower and almost flat from the mid-1970s to the late 1980s, and then
#increases rapidly again during the 1990s.

#3.1b.
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |> 
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Gas Production",
    x = "Month",
    y = "Log(Volume)"
  ) +
  theme_minimal()
#After taking the logs, the seasonal swings became more consistent in size over
#time and it appears more uniform. The transformation made it smoother but it
#didn't completely make it series linear and it still changes over time.
#I think logs are a good idea for can_gas. Even though they did not make trend
#perfectly linear, they did improve the series by making the seasonal swings
#more constant. Being able to see the nonlinear behavior in growth in an easier
#model does in fact help.

#3.1c.
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Month-over-Month Growth Rate in Canadian Gas Production",
    x = "Month",
    y = "Percent Growth"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
    )

can_gas |>
  autoplot(growth_gap) +
  labs(
  title = "Growth Rate Difference Between Direct and Log-Difference",
    x = "Month",
    y = "Absolute Difference"
  ) +
  theme_minimal()
#I think the log difference approximation is good since the absolute difference
#is usually small, although there are some large gaps.

#3.3
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal() 
#The NSA series is noisier with large positive and negative spikes throughout
#the month-over-month growth rate. The SA series is much more narrow and ranges
#around zero. The reason the NSA series is noisy is because it still contains
#the seasonal patterns, similarly to what we observed in 2.7b.

#Part 4 - Seeing, Decomposing, and Modeling Seasonality

#4.1a.
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

#4.1b.
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
#The SA series is smoother and has less variation than the NSA series, showing
#that is seasonally adjusted. The NSA series shows a repeating seasonal pattern,
#with more spikes and drops in sales.

#4.1c.
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Log of Canadian Gas Production",
    x = "Month",
    y = "Log(Volume)"
  ) +
  theme_minimal()
#The seasonal pattern shows that gas production is higher during the winter 
#months and lower during the summer months. This matches my intuition because
#usually there is more demand for natural gas during the winter months since
#people use it for heating due to the colder weather.

#4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

#4.3a.
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()
retail_nsa_x13 |> autoplot()
#The seasonal factor is highest in December and roughly at 1.2, meaning that 
#sales are about 20% above trend during that season. The size of the seasonal
#swings appear to be stable over time with similar peaks and drops each year.

#4.3b.
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
#My X-13 seasonal adjustment appears to be very similar to the Census Bureau's
#because the the difference is relatively small around 0%. They do differ the 
#most around 2020 to 2023, where a gap exceeds 1%

#4.4a.
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))
report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
#The month with the most significant seasonal effect June with the largest 
#t-statistic of 4.777.

#4.4b.
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()
#The fitted line fits poorly in the early years of the sample, but is does well
#in capturing the seasonal pattern and follows that upward trend well.

#4.4c.
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
#The residuals do not appear to be white noise because they show a long-run 
#pattern and a strong autocorrelation. The Ljung-Box test rejects the null
#hypothesis of no autocorrelation at the 5% level.