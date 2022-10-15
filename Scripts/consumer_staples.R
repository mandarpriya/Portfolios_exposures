# Cosnumer-Staples-Portfolio ----

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
library(frenchdata)
library(slider)

# tickers ----

symbols <- c("PG","KO","WMT","TSN","GIS") |> sort()

write_rds(symbols,"Symbols/consumer_staples.rds")



# Stock Prices ----
prices <- symbols |> 
    tq_get(get = "stock.prices", from = "1990-01-01")

write_rds(prices,"stock-prices/consumer_staples_prices.rds")

# Stock Returns ----

consumer_staples_returns_tbl <- prices |> 
    group_by(symbol) |> 
    tq_transmute(
        select     = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
        
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(consumer_staples_returns_tbl,"stock-returns/consumers_staples_returns_tbl.rds")

# Portfolio Analysis ----
n <- length(symbols)

w <- rep(1,n) /n

consumer_staples_port_tbl <- consumer_staples_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col   = symbol,
        returns_col  = monthly.returns,
        weights      = w,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "Consumer_Staples")

write_rds(consumer_staples_port_tbl,"portfolios/consumer_staples_port_tbl.rds")
