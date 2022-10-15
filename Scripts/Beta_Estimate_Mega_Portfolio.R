# Beta Estimation ----

## libraries 
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
library(readr)

# loading the Mega Portfolio ----

port_of_port_tbl <- read_rds("portfolios/port_of_port_tbl.rds")

# loading the fama-french 3 factors data ----

factors_ff_monthly <- read_rds("Fama-French/factors_ff_monthly.rds")

# Combining the Mega Portfolio and fam-french data ----

port_of_port_ff_tbl <- port_of_port_tbl |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol,excess_returns:mkt_excess)

write_rds(port_of_port_ff_tbl,"Beta_analysis/port_of_port_ff_tbl.rds")


# Loading the CAPM function ----


estimate_capm <- read_rds("capm-function/estimate_capm.rds")

roll_capm_estimation <- read_rds("capm-function/roll_capm_estimation.rds")

# Beta Estimation ----

beta_port_of_port_tbl <- port_of_port_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(date,symbol, beta) |> 
    drop_na()
write_rds(beta_port_of_port_tbl,"Beta_analysis/beta_port_of_port_tbl.rds")

# Visualization of Beta of Mega Portfolio ----

b <- beta_port_of_port_tbl |> 
    ggplot(aes(date, beta)) +
    geom_line() + theme_tq() +
    scale_color_tq(theme = "dark") +
    labs(
        title = "Beta Estimate for Portfolio of Portfolios",
        subtitle = "Low Volatile compared to the total market ",
        x = "",
        y = "Beta"
    )



write_rds(b, "Visualizations/b.rds")

read_rds("Visualizations/b.rds")

