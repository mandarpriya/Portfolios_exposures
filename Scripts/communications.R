# Communications Portfolio ----

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
symbols <- c("VZ","T","DIS","CMCSA","OMC") |> sort()

write_rds(symbols,"Symbols/communications.rds")

# stock prices ----
prices <- symbols |> 
    tq_get(get = "stock.prices", from = "1990-01-01")

write_rds(prices,"stock-prices/communications_prices.rds")

# stock returns ----

communications_returns_tbl <-  prices |> 
    group_by(symbol) |> 
    select(symbol, date,adjusted) |> 
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(communications_returns_tbl,"stock-returns/communications_returns_tbl.rds")

# portfolio analysis ----

communications_port_tbl <- communications_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col    = symbol,
        returns_col   = monthly.returns,
        rebalance_on  = "years"
    ) |> 
    ungroup() |> 
    add_column(symbol = "communications")

write_rds(communications_port_tbl,"portfolios/communications_port_tbl.rds")
