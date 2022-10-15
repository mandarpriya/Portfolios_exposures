# Real-Estate Portfolio ----

# libraries ----
library(tidymodels)
library(tidyquant)
library(tidyverse)
library(scales)
library(ggplot2)
library(plotly)
library(timetk)
library(modeltime)
library(slider)
library(frenchdata)

# tickers ----

symbols <-  c("WELL","WY","LPX","PEAK","PSA") |> sort()

write_rds(symbols,"Symbols/real_estate.rds")

# stock prices ----

prices <- symbols |> 
    tq_get(get = "stock.prices", from = "1990-01-01" )

write_rds(prices, "stock-prices/real_estate_prices.rds")

# stock returns ----

real_estate_returns_tbl <- prices |> 
    group_by(symbol) |> 
    select(symbol, date,adjusted) |> 
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))
write_rds(real_estate_returns_tbl,"stock-returns/real_estate_returns_tbl.rds")

# portfolio analysis ----

real_estate_port_tbl <-  real_estate_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col    = symbol,
        returns_col   = monthly.returns,
        rebalance_on  = "years"
    ) |> 
    ungroup() |> 
    add_column(symbol = "real_estate")

write_rds(real_estate_port_tbl,"portfolios/real_estate_port_tbl.rds")
