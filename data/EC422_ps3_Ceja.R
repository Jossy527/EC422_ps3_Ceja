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

