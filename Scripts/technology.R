# Technology Portfolio ----

# libraries
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
symbols <- c("AAPL","AMD","MSFT","ORCL","ADBE") |> sort()

write_rds(symbols ,"Symbols/technology.rds")    

# stock prices ----

prices <- symbols |> 
    tq_get(get = "stock.prices", from = "1990-01-01")

write_rds(prices, "stock-prices/technology_prices.rds")

# stock returns ----

technology_returns_tbl <- prices |> 
    group_by(symbol) |> 
    select(symbol, date, adjusted) |> 
    tq_transmute(
        select     = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
    ) |> 
    ungroup() |> 
    # we need from the starting of the month
    mutate(date = rollback(date, roll_to_first = TRUE)) 

write_rds(technology_returns_tbl, "stock-returns/technology_returns_tbl.rds")

# portfolio analysis ----

# number of assets
n <- length(symbols)

# Weight of the assets
w <- rep(1,n)/n


technology_port_tbl <- technology_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol, 
        returns_col = monthly.returns,
        weights = w,
        rebalance_on = "years"
    ) |> 
    ungroup() |> 
    add_column(symbol = "technology")

write_rds(technology_port_tbl,"portfolios/technology_port_tbl.rds")

    