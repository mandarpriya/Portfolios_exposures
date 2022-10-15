# Beta Estimates ----

## libraries ----



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
 
## loading the data ----

consumer_discretionary_max_ret_port_tbl <- read_rds("Alternative_Portfolios/consumer_discretionary_max_ret_port_tbl.rds")
consumer_discretionary_min_var_port_tbl <- read_rds("Alternative_Portfolios/consumer_discretionary_min_var_port_tbl.rds")
consumer_staples_max_ret_port_tbl <- read_rds("Alternative_Portfolios/consumer_staples_max_ret_port_tbl.rds")
consumer_staples_min_var_port_tbl <- read_rds("Alternative_Portfolios/consumer_staples_min_var_port_tbl.rds")
energy_max_ret_port_tbl <- read_rds("Alternative_Portfolios/energy_max_ret_port_tbl.rds")
energy_min_var_port_tbl <- read_rds("Alternative_Portfolios/energy_min_var_port_tbl.rds")
financials_min_var_port_tbl <- read_rds("Alternative_Portfolios/financials_min_var_port_tbl.rds")
financials_max_ret_port_tbl <- read_rds("Alternative_Portfolios/financials_max_ret_port_tbl.rds")
health_care_min_var_port_tbl <- read_rds("Alternative_Portfolios/health_care_min_var_port_tbl.rds")
health_care_max_ret_port_tbl <- read_rds("Alternative_Portfolios/health_care_max_ret_port_tbl.rds")
industrials_min_var_port_tbl <- read_rds("Alternative_Portfolios/industrials_min_var_port_tbl.rds")
industrials_max_ret_port_tbl <- read_rds("Alternative_Portfolios/industrials_max_ret_port_tbl.rds")
materials_max_ret_port_tbl <- read_rds("Alternative_Portfolios/materials_max_ret_port_tbl.rds")
materials_min_var_port_tbl <- read_rds("Alternative_Portfolios/materials_min_var_port_tbl.rds")
real_estate_max_ret_port_tbl <- read_rds("Alternative_Portfolios/real_estate_max_ret_port_tbl.rds")
real_estate_min_var_port_tbl <- read_rds("Alternative_Portfolios/real_estate_min_var_port_tbl.rds")
technology_max_ret_port_tbl <- read_rds("Alternative_Portfolios/technology_max_ret_port_tbl.rds")
technology_min_var_port_tbl <- read_rds("Alternative_Portfolios/technology_min_var_port_tbl.rds")
utilities_min_var_port_tbl <- read_rds("Alternative_Portfolios/utilities_min_var_port_tbl.rds")
utilities_max_ret_port_tbl <- read_rds("Alternative_Portfolios/utilities_max_ret_port_tbl.rds")

## CAPM function ----
estimate_capm <- read_rds("capm-function/estimate_capm.rds")
roll_capm_estimation <- read_rds("capm-function/roll_capm_estimation.rds")

## Fama-French ----
factors_ff_monthly <- read_rds("Fama-French/factors_ff_monthly.rds")

## Data Combined ----

consumer_discretionary_max_ret_port_ff_tbl <- consumer_discretionary_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

consumer_discretionary_min_var_port_ff_tbl <- consumer_discretionary_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

consumer_staples_max_ret_port_ff_tbl <- consumer_staples_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

consumer_staples_min_var_port_ff_tbl<- consumer_staples_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

energy_max_ret_port_ff_tbl<- energy_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

energy_min_var_port_ff_tbl<- energy_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()


financials_max_ret_port_ff_tbl <- financials_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

financials_min_var_port_ff_tbl<- financials_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

health_care_max_ret_port_ff_tbl<- health_care_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

health_care_min_var_port_ff_tbl <- health_care_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

industrials_max_ret_port_ff_tbl <- industrials_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

industrials_min_var_port_ff_tbl<- industrials_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

materials_max_ret_port_ff_tbl<- materials_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

materials_min_var_port_ff_tbl <- materials_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

real_estate_max_ret_port_ff_tbl <- real_estate_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

real_estate_min_var_port_ff_tbl <- real_estate_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

technology_max_ret_port_ff_tbl <- technology_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

technology_min_var_port_ff_tbl <- technology_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

utilities_max_ret_port_ff_tbl <- utilities_max_ret_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()

utilities_min_var_port_ff_tbl <- utilities_min_var_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns -rf) |> 
    select(symbol, date, mkt_excess:excess_returns) |> 
    ungroup()
# Beta Estimation ----

beta_consumer_discretionary_max_ret_port_tbl<- consumer_discretionary_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_consumer_discretionary_min_var_port_tbl<- consumer_discretionary_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_consumer_staples_max_ret_port_tbl<- consumer_staples_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()


beta_consumer_staples_min_var_port_tbl<- consumer_staples_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_energy_max_ret_port_tbl<- energy_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_energy_min_var_port_tbl <- energy_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_financials_max_ret_port_tbl<- financials_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_financials_min_var_port_tbl<- financials_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_health_care_max_ret_port_tbl<- health_care_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_health_care_min_var_port_tbl <- health_care_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()


beta_industrials_max_ret_port_tbl <- industrials_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_industrials_min_var_port_tbl<- industrials_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_materials_max_ret_port_tbl <- materials_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_materials_min_var_port_tbl <- materials_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_real_estate_max_ret_port_tbl<- real_estate_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_real_estate_min_var_port_tbl<- real_estate_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_technology_max_ret_port_tbl<- technology_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_technology_min_var_port_tbl<- technology_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_utilities_max_ret_port_tbl <- utilities_max_ret_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_utilities_min_var_port_tbl<- utilities_min_var_port_ff_tbl |> 
    group_by(symbol) |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    ungroup() |> 
    select(symbol, date, beta) |> 
    drop_na()


# Saving the Beta estimates

write_rds(beta_consumer_discretionary_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_consumer_discretionary_max_ret_port_tbl.rds")

write_rds(beta_consumer_discretionary_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_consumer_discretionary_min_var_port_tbl.rds")

write_rds(beta_consumer_staples_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_consumer_staples_max_ret_port_tbl.rds")
write_rds(beta_consumer_staples_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_consumer_staples_min_var_port_tbl.rds")

write_rds(beta_energy_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_energy_max_ret_port_tbl.rds")
write_rds(beta_energy_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_energy_min_var_port_tbl.rds")

write_rds(beta_financials_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_financials_max_ret_port_tbl.rds")
write_rds(beta_financials_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_financials_min_var_port_tbl.rds")

write_rds(beta_health_care_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_health_care_max_ret_port_tbl.rds")
write_rds(beta_health_care_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_health_care_min_var_port_tbl.rds")

write_rds(beta_industrials_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_industrials_max_ret_port_tbl.rds")
write_rds(beta_industrials_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_industrials_min_var_port_tbl.rds")

write_rds(beta_materials_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_materials_max_ret_port_tbl.rds")
write_rds(beta_materials_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_materials_min_var_port_tbl.rds")

write_rds(beta_real_estate_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_real_estate_max_ret_port_tbl.rds")
write_rds(beta_real_estate_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_real_estate_min_var_port_tbl.rds")

write_rds(beta_technology_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_technology_max_ret_port_tbl.rds")
write_rds(beta_technology_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_technology_min_var_port_tbl.rds")

write_rds(beta_utilities_max_ret_port_tbl,"Beta_estimates_alternative_portfolios/beta_utilities_max_ret_port_tbl.rds")
write_rds(beta_utilities_min_var_port_tbl,"Beta_estimates_alternative_portfolios/beta_utilities_min_var_port_tbl.rds")


# Visualization ----

beta_combined_max_ret_port_tbl <- rbind(
    beta_communications_max_ret_port_tbl,
    beta_consumer_discretionary_max_ret_port_tbl,
    beta_consumer_staples_max_ret_port_tbl,
    beta_energy_max_ret_port_tbl,
    beta_financials_max_ret_port_tbl,
    beta_health_care_max_ret_port_tbl,
    beta_industrials_max_ret_port_tbl,
    beta_materials_max_ret_port_tbl,
    beta_real_estate_max_ret_port_tbl,
    beta_technology_max_ret_port_tbl,
    beta_utilities_max_ret_port_tbl
) 


beta_combined_max_ret_port_tbl |> 
    plot_time_series(
        .date_var = date,
        .value = beta,
        .color_var = symbol,
        .smooth = FALSE,
        .facet_scales = "free_y",
        .y_intercept = 0,
        .y_intercept_color = "BLACK",
        .x_lab = "",
        .y_lab = "Beta",
        .title = "Beta Estimates for Max-Return Portfolio strategy for different Sectors ",
        .legend_show = FALSE
    )

beta_combined_min_var_port_tbl <- rbind(
    beta_communications_min_var_port_tbl,
    beta_consumer_discretionary_min_var_port_tbl,
    beta_consumer_staples_min_var_port_tbl,
    beta_energy_min_var_port_tbl,
    beta_financials_min_var_port_tbl,
    beta_health_care_min_var_port_tbl,
    beta_industrials_min_var_port_tbl,
    beta_materials_min_var_port_tbl,
    beta_real_estate_min_var_port_tbl,
    beta_technology_min_var_port_tbl,
    beta_utilities_min_var_port_tbl
)

beta_combined_min_var_port_tbl |> 
    plot_time_series(
        .date_var = date,
        .value = beta,
        .color_var = symbol,
        .smooth = FALSE,
        .facet_scales = "free_y",
        .y_intercept = 0,
        .y_intercept_color = "BLACK",
        .x_lab = "",
        .y_lab = "Beta",
        .title = "Beta Estimates for Min-VAR Portfolio strategy for different Sectors ",
        .legend_show = FALSE
        
    )
