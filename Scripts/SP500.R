# S&P 500 ----

# libraries ----

library(tidyquant) 
library(tidyverse)
library(lubridate)

# Data ----

## Index ----
SP500_tbl <- tq_get("^GSPC", get = "stock.prices", from = "1990-01-01")

write_rds(SP500_tbl,"Macro_Data/SP500_tbl.rds")

## Index Returns

SP500_ret_tbl <- SP500_tbl |> 
    group_by(symbol) |> 
    tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period     = "monthly",
        col_rename = "SP500"
    ) |> 
    ungroup() |> 
    mutate(date = rollback(date, roll_to_first = TRUE))

write_rds(SP500_ret_tbl,"Macro_Data/SP500_ret_tbl.rds")


