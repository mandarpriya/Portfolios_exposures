# Materials Portfolio ----

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
symbols <- c("NEM","ECL","APD","GWW","CLF") |> sort()

write_rds(symbols,"Symbols/materials.rds")

# stock prices ----

prices <- symbols |> 
    tq_get(get = "stock.prices",from = "1990-01-01")

write_rds(prices,"stock-prices/materials_prices.rds")    

# stock returns ----

materials_returns_tbl <- prices |> 
    group_by(symbol) |> 
    select(symbol, date, adjusted) |> 
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly"
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(materials_returns_tbl, "stock-returns/materials_returns_tbl.rds")    

# portfolio analysis ----

n <- length(symbols)
#weights
w <- rep(1,n)/n

materials_port_tbl <- materials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col    = symbol,
        returns_col   = monthly.returns,
        weights       = w,
        rebalance_on  = "years"
    ) |> 
    add_column(symbol = "materials") |> 
    ungroup()
write_rds(materials_port_tbl,"portfolios/materials_port_tbl.rds")
