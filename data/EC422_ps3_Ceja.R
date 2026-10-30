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

