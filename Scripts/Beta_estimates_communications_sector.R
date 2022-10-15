# libraries ----

library(readr)
library(tidymodels)
library(tidyquant)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)
library(timetk)
library(modeltime)
library(scales)
library(slider)
# data ----
communications_max_ret_port_tbl <- read_rds("Alternative_Portfolios/communications_max_ret_port_tbl.rds")
communications_min_var_port_tbl <- read_rds("Alternative_Portfolios/communications_min_var_port_tbl.rds")
communications_port_tbl <- read_rds("portfolios/communications_port_tbl.rds")
# Beta Estimation ----

estimate_capm <- read_rds("capm-function/estimate_capm.rds")

roll_capm_estimation <- read_rds("capm-function/roll_capm_estimation.rds")

factors_ff_monthly <- read_rds("Fama-French/factors_ff_monthly.rds")

communications_min_var_port_ff_tbl <- communications_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()
    



communications_max_ret_port_ff_tbl <- communications_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

communications_naive_port_ff_tbl <- communications_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

# Beta ----

beta_communications_naive_port_tbl <- communications_naive_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_communications_min_var_port_tbl <- communications_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_communications_max_ret_port_tbl <- communications_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

# Plot Beta ----

beta_communications_min_var_port_tbl |> 
    plot_time_series(
        .date_var = date,
        .value    = beta,
        .title = "Beta Estimates for Min-Var Communications-Sector Portfolio",
        .smooth = FALSE
    )
    

beta_communications_max_ret_port_tbl |> 
    plot_time_series(
        .date_var = date,
        .value = beta,
        .smooth = FALSE,
        .title = "Beta Estimates for Max-Returns Communications-Sector Portfolio",
        .x_lab = "",
        .y_lab = "Beta",
        .y_intercept = 0
        
    )

beta_communications_naive_port_tbl |> 
    plot_time_series(
        .date_var = date,
        .value = beta,
        .smooth = FALSE,
        .title = "Beta Estimates for Naive-Returns Communications-Sector Portfolio",
        .x_lab = "",
        .y_lab = "Beta"
    )

beta_communications_naive_port_tbl |> 

rbind(beta_communications_max_ret_port_tbl ,
      beta_communications_min_var_port_tbl) |> 
    plot_time_series(
        .date_var = date,
        .value    = beta,
        .color_var = symbol,
        .facet_scales = "free_y",
        .smooth = FALSE,
        .title = "Beta Estimates for Various Portfolios for Communications_Sector"
    )

write_rds(beta_communications_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_communications_max_ret_port_tbl.rds")

write_rds(beta_communications_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_communications_min_var_port_tbl.rds")

write_rds(beta_communications_naive_port_tbl,"Beta_estimates_alternative_portfolios/beta_communications_naive_port_tbl.rds")


    

                    
                                 
