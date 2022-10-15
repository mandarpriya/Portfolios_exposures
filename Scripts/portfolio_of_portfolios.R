# Portfolio of Portfolios ----

# libraries ----

library(tidymodels)
library(tidyquant)
library(tidyverse)
library(scales)
library(kableExtra)
library(lubridate)
library(ggplot2)
library(plotly)
library(timetk)
library(slider)
library(frenchdata)


# data input for portfolio of portfolios ----

## Step1: Uploading the data ----
communications_port_tbl <- read_rds("portfolios/communications_port_tbl.rds")
consumer_discretionary_port_tbl <- read_rds("portfolios/consumer_discretionary_port_tbl.rds")
consumer_staples_port_tbl <- read_rds("portfolios/consumer_staples_port_tbl.rds")
energy_port_tbl <- read_rds("portfolios/energy_port_tbl.rds")
financials_port_tbl <- read_rds("portfolios/financials_port_tbl.rds")
health_care_port_tbl <- read_rds("portfolios/health_care_port_tbl.rds")
industrial_port_tbl <- read_rds("portfolios/industrial_port_tbl.rds")
materials_port_tbl <- read_rds("portfolios/materials_port_tbl.rds")
real_estate_port_tbl <- read_rds("portfolios/real_estate_port_tbl.rds")
technology_port_tbl <- read_rds("portfolios/technology_port_tbl.rds")
utilities_port_tbl <-  read_rds("portfolios/utilities_port_tbl.rds")

## Step2 : Combining the data ----
returns_port_tbl <- rbind(communications_port_tbl,
      consumer_discretionary_port_tbl,
      consumer_staples_port_tbl,
      energy_port_tbl,
      utilities_port_tbl,
      financials_port_tbl,
      technology_port_tbl,
      real_estate_port_tbl,
      industrial_port_tbl,
      materials_port_tbl,
      health_care_port_tbl)

write_rds(returns_port_tbl,"stock-returns/returns_port_tbl.rds")


## Step3 : Portfolio analysis ----

port_of_port_tbl <- returns_port_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol, 
        returns_col = portfolio.returns,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "mega_portfolio")

write_rds(port_of_port_tbl,"portfolios/port_of_port_tbl.rds")    
