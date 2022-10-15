# Health-Care Portfolio ----

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

symbols <- c("PFE","ABT","BMY","CVS","JNJ") |> sort()

write_rds(symbols, "Symbols/health_care.rds")

# stock prices ----

prices <- symbols |> tq_get(get = "stock.prices", from = "1990-01-01")

write_rds(prices, "stock-prices/health_care_prices.rds")

# stock returns ----

health_care_returns_tbl <- prices |> 
    group_by(symbol) |> 
    select(symbol, date, adjusted) |> 
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(health_care_returns_tbl, "stock-returns/health_care_returns_tbl.rds")

# portfolio analysis ----

n <- length(symbols)

w <- rep(1,n)/n

health_care_port_tbl <- health_care_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col     = symbol,
        returns_col    = monthly.returns,
        weights        = w,
        rebalance_on   = "years"
    ) |> 
    ungroup() |> 
    add_column(symbol = "health_care")

write_rds(health_care_port_tbl, "portfolios/health_care_port_tbl.rds")    
