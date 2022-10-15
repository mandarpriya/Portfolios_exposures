# Consumer-Discretionary_portfolio ----

## libraries ----
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
symbols <- c("TGT","NKE","MCD","HD","CCL") |> sort()

write_rds(symbols, "Symbols/consumer_discretionary.rds")

# stock prices ----
prices <- prices <- symbols |> 
    tq_get(get = "stock.prices", from = "1990-01-01")

write_rds(prices,"stock-prices/consumer_discretionary_prices.rds")



# Monthly Returns ----
consumer_discretionary_returns_tbl <- prices |> 
    group_by(symbol) |> 
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(consumer_discretionary_returns_tbl, "stock-returns/consumer_discretionary_returns_tbl.rds")


# Portfolio Analysis ----

n <- length(symbols)
#weights
w <- rep(1,n)/n

consumer_discretionary_port_tbl <- consumer_discretionary_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col   = symbol,
        returns_col  = monthly.returns,
        weights      = w,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "Consumer_Discretionary")

write_rds(consumer_discretionary_port_tbl,"portfolios/consumer_discretionary_port_tbl.rds")


