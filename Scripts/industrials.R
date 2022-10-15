# Industrials Portfolio ----

# libraries ----
library(tidymodels)
library(tidyquant)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)
library(timetk)
library(modeltime)

# tickers ----
symbols <- c("GE","HON","BA","CAT","CMI") |>  sort()

write_rds(symbols, "Symbols/industrials.rds")

# stock prices ----
prices <- symbols |> 
    tq_get(get = "stock.prices", from = "1990-01-01")

write_rds(prices,"stock-prices/industrials_prices.rds")

# stock returns ----
industrials_returns_tbl <- prices |> 
    group_by(symbol) |> 
    select(symbol, date, adjusted) |> 
    tq_transmute(
        select     = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly",
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(industrials_returns_tbl,"stock-returns/industrials_returns_tbl.rds")

# portfolio analysis ----
n <- length(symbols)

w <- rep(1,n)/n

industrial_port_tbl <- industrials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col  = symbol,
        returns_col = monthly.returns,
        weights     = w,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "Industrials")

write_rds(industrial_port_tbl,"portfolios/industrial_port_tbl.rds")
