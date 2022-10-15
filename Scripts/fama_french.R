# Fama-French 3 Factors data ----

# libraries ----
library(tidyverse)
library(lubridate)
library(scales)
library(frenchdata)
# Fama-French 3 Factors Monthly Data

## data avalilability ----

get_french_data_list()

## Code for downloading the fama-french data ----

start_date <- "1990-01-01"

end_date   <- Sys.Date()

factors_ff_monthly_raw <- download_french_data("Fama/French 3 Factors")
factors_ff_monthly <- factors_ff_monthly_raw$subsets$data[[1]] |>
    transmute(
        date = floor_date(ymd(str_c(date, "01")), "month"),
        rf = as.numeric(RF) / 100,
        mkt_excess = as.numeric(`Mkt-RF`) / 100,
        smb = as.numeric(SMB) / 100,
        hml = as.numeric(HML) / 100
    ) |> 
    filter(date >="1990-01-01" & date <= Sys.Date())

write_rds(factors_ff_monthly, "Fama-French/factors_ff_monthly.rds")

