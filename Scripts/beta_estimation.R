
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

library(readr)

# Portfolios  ----
industrial_port_tbl <- read_rds("portfolios/industrial_port_tbl.rds")
materials_port_tbl <- read_rds("portfolios/materials_port_tbl.rds")
real_estate_port_tbl <- read_rds("portfolios/real_estate_port_tbl.rds")
technology_port_tbl <- read_rds("portfolios/technology_port_tbl.rds")
utilities_port_tbl <-  read_rds("portfolios/utilities_port_tbl.rds")
health_care_port_tbl <- read_rds("portfolios/health_care_port_tbl.rds")
financials_port_tbl <- read_rds("portfolios/financials_port_tbl.rds")
energy_port_tbl <- read_rds("portfolios/energy_port_tbl.rds")
consumer_staples_port_tbl <- read_rds("portfolios/consumer_staples_port_tbl.rds")
communications_port_tbl <- read_rds("portfolios/communications_port_tbl.rds")
consumer_discretionary_port_tbl <- read_rds("portfolios/consumer_discretionary_port_tbl.rds")

# capm function ----
estimate_capm <- read_rds("capm-function/estimate_capm.rds")

roll_capm_estimation <- read_rds("capm-function/roll_capm_estimation.rds")

# fama-french factors ----

factors_ff_monthly <- read_rds("Fama-French/factors_ff_monthly.rds")

# Combining the portfolios and fama-french ----

communications_ff_tbl <- communications_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()
consumer_discretionary_ff_tbl <- consumer_discretionary_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

consumer_staples_ff_tbl <- consumer_staples_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

energy_ff_tbl <- energy_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

financials_ff_tbl <- financials_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

health_care_ff_tbl <- health_care_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

industrial_ff_tbl <- industrial_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

materials_ff_tbl <- materials_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

real_estate_ff_tbl <- real_estate_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

technology_ff_tbl <- technology_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

utilities_ff_tbl <- utilities_port_tbl |> 
    group_by(symbol) |> 
    left_join(factors_ff_monthly, by = "date") |> 
    mutate(excess_returns = portfolio.returns - rf) |> 
    select(date,symbol, excess_returns:mkt_excess) |> 
    ungroup()

write_rds(utilities_ff_tbl,"Beta_analysis/utilities_ff_tbl.rds")
write_rds(financials_ff_tbl,"Beta_analysis/financials_ff_tbl.rds")
write_rds(communications_ff_tbl,"Beta_analysis/communications_ff_tbl.rds")
write_rds(technology_ff_tbl,"Beta_analysis/technology_ff_tbl.rds")
write_rds(real_estate_ff_tbl,"Beta_analysis/real_estate_ff_tbl.rds")
write_rds(materials_ff_tbl,"Beta_analysis/materials_ff_tbl.rds")
write_rds(industrial_ff_tbl,"Beta_analysis/industrial_ff_tbl.rds")
write_rds(health_care_ff_tbl,"Beta_analysis/health_care_ff_tbl.rds")
write_rds(energy_ff_tbl,"Beta_analysis/energy_ff_tbl.rds")
write_rds(consumer_staples_ff_tbl,"Beta_analysis/consumer_staples_ff_tbl.rds")
write_rds(consumer_discretionary_ff_tbl,"Beta_analysis/consumer_discretionary_ff_tbl.rds")

# Beta Estimates

beta_communications_port_tbl <- communications_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date,beta) |> 
    drop_na()
beta_consumer_discretionary_port_tbl <- consumer_discretionary_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_consumer_staples_port_tbl <- consumer_staples_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_energy_port_tbl <- energy_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_financials_port_tbl <- financials_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()

beta_health_care_port_tbl <- health_care_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_industrial_port_tbl <- industrial_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_materials_port_tbl <- materials_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_real_estate_port_tbl <- real_estate_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_technology_port_tbl <- technology_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()
beta_utilities_port_tbl <- utilities_ff_tbl |> 
    mutate(roll_capm_estimation(cur_data(), months = 60, min_obs = 48)) |> 
    select(symbol, date, beta) |> 
    drop_na()

# Saving the beta 

write_rds(beta_communications_port_tbl,"Beta_analysis/beta_communications_port_tbl.rds")
write_rds(beta_utilities_port_tbl,"Beta_analysis/beta_utilities_port_tbl.rds")
write_rds(beta_technology_port_tbl,"Beta_analysis/beta_technology_port_tbl.rds")
write_rds(beta_real_estate_port_tbl,"Beta_analysis/beta_real_estate_port_tbl.rds")
write_rds(beta_materials_port_tbl,"Beta_analysis/beta_materials_port_tbl.rds")
write_rds(beta_industrial_port_tbl,"Beta_analysis/beta_industrial_port_tbl.rds")
write_rds(beta_health_care_port_tbl,"Beta_analysis/beta_health_care_port_tbl.rds")
write_rds(beta_financials_port_tbl,"Beta_analysis/beta_financials_port_tbl.rds")
write_rds(beta_energy_port_tbl,"Beta_analysis/beta_energy_port_tbl.rds")
write_rds(beta_consumer_staples_port_tbl,"Beta_analysis/beta_consumer_staples_port_tbl.rds")
write_rds(beta_consumer_discretionary_port_tbl,"Beta_analysis/beta_consumer_discretionary_port_tbl.rds")

beta_port_of_port_tbl <- read_rds("Beta_analysis/beta_port_of_port_tbl.rds")
# combining all the Beta 
combined_beta_port_tbl <- rbind(beta_communications_port_tbl,
      beta_utilities_port_tbl,
      beta_technology_port_tbl,
      beta_real_estate_port_tbl,
      beta_materials_port_tbl,
      beta_industrial_port_tbl,
      beta_health_care_port_tbl,
      beta_financials_port_tbl,
      beta_energy_port_tbl,
      beta_consumer_staples_port_tbl,
      beta_consumer_discretionary_port_tbl,
      beta_port_of_port_tbl
     ) 

# Visualzation of Beta 
g <- combined_beta_port_tbl |> 
    ggplot(aes(date, beta, color = symbol)) +
    geom_line() + theme_tq() +
    labs(
        title = "Beta Estimates of all the sector portfolios",
        subtitle = "Most of the portfolios have beta near unity",
        x = "",
        y = "beta"
        
    )
combined_beta_port_tbl |> 
    plot_time_series(
        .date_var = date,
        .value = beta,
        .color_var = symbol,
        .smooth = FALSE,
        .facet_scales = "free_y",
        .title = "Beta Estimates for all the sector portfolios",
        .legend_show = FALSE,
        # .y_intercept = 0,
        # .y_intercept_color = "BLACK"
    )
