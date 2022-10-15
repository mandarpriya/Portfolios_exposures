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

# loading the data ----



consumer_discretionary_returns_tbl <- read_rds("stock-returns/consumer_discretionary_returns_tbl.rds")

consumer_staples_returns_tbl <- read_rds("stock-returns/consumer_staples_returns_tbl.rds")

energy_returns_tbl <- read_rds("stock-returns/energy_returns_tbl.rds")

financials_returns_tbl<- read_rds("stock-returns/financials_returns_tbl.rds")

health_care_returns_tbl <- read_rds("stock-returns/health_care_returns_tbl.rds")

industrials_returns_tbl  <- read_rds("stock-returns/industrials_returns_tbl.rds")

materials_returns_tbl <- read_rds("stock-returns/materials_returns_tbl.rds")

real_estate_returns_tbl<- read_rds("stock-returns/real_estate_returns_tbl.rds")

technology_returns_tbl <- read_rds("stock-returns/technology_returns_tbl.rds")

utilities_returns_tbl <- read_rds("stock-returns/utilities_returns_tbl.rds")


#  Symbols ----

assets <- read_rds("Symbols/consumer_discretionary.rds")

assets1 <- read_rds("Symbols/consumer_staples.rds")

assets2 <- read_rds("Symbols/energy.rds")

assets3 <- read_rds("Symbols/financials.rds")

assets4 <- read_rds("Symbols/health_care.rds")

assets5 <- read_rds("Symbols/industrials.rds")

assets6 <- read_rds("Symbols/materials.rds")

assets7 <- read_rds("Symbols/real_estate.rds")

assets8 <- read_rds("Symbols/technology.rds")

assets9 <- read_rds("Symbols/utilities.rds")


library(timetk)

# long data conversion ----


consumer_discretionary_returns_xts <- consumer_discretionary_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol, monthly.returns) |> 
    tk_xts(date_var = date)

consumer_staples_returns_xts <- consumer_staples_returns_tbl |> 
    group_by(symbol)  |> 
    spread(symbol, monthly.returns) |> 
    tk_xts(date_var = date)
energy_returns_xts <- energy_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

financials_returns_xts <- financials_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

health_care_returns_xts <- health_care_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

industrials_returns_xts <- industrials_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

materials_returns_xts <- materials_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

real_estate_returns_xts <- real_estate_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

technology_returns_xts <- technology_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

utilities_returns_xts <- utilities_returns_tbl |> 
    group_by(symbol) |> 
    spread(symbol,monthly.returns) |> 
    tk_xts(date_var = date)

# portfolio specification ----

library(PortfolioAnalytics)

## Minimum Variance specification ----

min_var_consumer_discretionary_portfolio <- portfolio.spec(assets = assets)

min_var_consumer_staples_portfolio <- portfolio.spec(assets = assets1)

min_var_energy_portfolio <- portfolio.spec(assets = assets2)

min_var_financials_portfolio <- portfolio.spec(assets = assets3)

min_var_health_care_portfolio <- portfolio.spec(assets = assets4)

min_var_industrials_portfolio <- portfolio.spec(assets = assets5)

min_var_materials_portfolio <- portfolio.spec(assets = assets6)

min_var_real_estate_portfolio <- portfolio.spec(assets = assets7)

min_var_technology_portfolio <- portfolio.spec(assets = assets8)

min_var_utilities_portfolio  <- portfolio.spec(assets = assets9)



## Constraints ----
min_var_consumer_staples_portfolio <- add.constraint(portfolio = min_var_consumer_staples_portfolio, type = "full_investment")
min_var_consumer_staples_portfolio <- add.constraint(portfolio = min_var_consumer_staples_portfolio,
                                                     type = "box", min = 0.01, max = 0.99)

min_var_energy_portfolio <- add.constraint(portfolio = min_var_energy_portfolio, type = "full_investment")
min_var_energy_portfolio <- add.constraint(portfolio = min_var_energy_portfolio,
                                           type = "box", min = 0.01, max = 0.99)

min_var_financials_portfolio <- add.constraint(portfolio = min_var_financials_portfolio,type = "full_investment")
min_var_financials_portfolio <- add.constraint(portfolio = min_var_financials_portfolio,
                                               type = "box", min = 0.01, max = 0.99)
min_var_health_care_portfolio <- add.constraint(portfolio = min_var_health_care_portfolio, type = "full_investment")
min_var_health_care_portfolio <- add.constraint(portfolio = min_var_health_care_portfolio, 
                                                type = "box", min = 0.01, max = 0.99)
min_var_industrials_portfolio <- add.constraint(portfolio = min_var_industrials_portfolio, type = "full_investment")
min_var_industrials_portfolio <- add.constraint(portfolio = min_var_industrials_portfolio,
                                                type = "box", min = 0.01, max = 0.99)

min_var_materials_portfolio <- add.constraint(portfolio = min_var_materials_portfolio,type ="full_investment")
min_var_materials_portfolio <- add.constraint(portfolio = min_var_materials_portfolio,
                                              type = "box", min = 0.01, max = 0.99)

min_var_real_estate_portfolio <- add.constraint(portfolio = min_var_real_estate_portfolio,type = "full_investment")
min_var_real_estate_portfolio <- add.constraint(portfolio = min_var_real_estate_portfolio, 
                                                type = "box", min = 0.01, max = 0.99)
min_var_technology_portfolio <- add.constraint(portfolio = min_var_technology_portfolio, type = "full_investment")
min_var_technology_portfolio <- add.constraint(portfolio = min_var_technology_portfolio, 
                                               type = "box", min = 0.01, max = 0.99)

min_var_utilities_portfolio <- add.constraint(portfolio = min_var_utilities_portfolio, type = "full_investment")
min_var_utilities_portfolio <- add.constraint(portfolio = min_var_utilities_portfolio,
                                              type = "box", min = 0.01, max = 0.99)

min_var_consumer_discretionary_portfolio <- add.constraint(portfolio = min_var_consumer_discretionary_portfolio, type = "full_investment")
min_var_consumer_discretionary_portfolio <- add.constraint(portfolio = min_var_consumer_discretionary_portfolio,
                                                           type = "box", min = 0.01, max = 0.99)
##  Objective ----

min_var_consumer_discretionary_portfolio <- add.objective(portfolio = min_var_consumer_discretionary_portfolio,
                                                          type = "risk", name = "var")
min_var_consumer_staples_portfolio <- add.objective(portfolio = min_var_consumer_staples_portfolio,
                                                          type = "risk", name = "var")
min_var_energy_portfolio <- add.objective(portfolio = min_var_energy_portfolio,
                                                          type = "risk", name = "var")
min_var_financials_portfolio <- add.objective(portfolio = min_var_financials_portfolio,
                                                          type = "risk", name = "var")
min_var_health_care_portfolio <- add.objective(portfolio = min_var_health_care_portfolio,
                                                          type = "risk", name = "var")
min_var_industrials_portfolio <- add.objective(portfolio = min_var_industrials_portfolio,
                                                          type = "risk", name = "var")
min_var_materials_portfolio <- add.objective(portfolio = min_var_materials_portfolio,
                                                          type = "risk", name = "var")
min_var_real_estate_portfolio <- add.objective(portfolio = min_var_real_estate_portfolio,
                                                          type = "risk", name = "var")
min_var_technology_portfolio <- add.objective(portfolio = min_var_technology_portfolio,
                                                          type = "risk", name = "var")
min_var_utilities_portfolio<- add.objective(portfolio = min_var_utilities_portfolio,
                                                          type = "risk", name = "var")

## Optimization ----


min_var_consumer_discretionary_portfolio <- optimize.portfolio(
    R = consumer_discretionary_returns_xts,
    portfolio = min_var_consumer_discretionary_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
    
)

min_var_consumer_staples_portfolio <- optimize.portfolio(
    R = consumer_staples_returns_xts,
    portfolio = min_var_consumer_staples_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)


min_var_energy_portfolio <- optimize.portfolio(
    R = energy_returns_xts,
    portfolio = min_var_energy_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
    
)

min_var_financials_portfolio  <- optimize.portfolio(
    R = financials_returns_xts,
    portfolio = min_var_financials_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)

min_var_health_care_portfolio <- optimize.portfolio(
    R = health_care_returns_xts,
    portfolio = min_var_health_care_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)

min_var_industrials_portfolio <- optimize.portfolio(
    R = industrials_returns_xts,
    portfolio = min_var_industrials_portfolio,
    optimize_method = "quadprog",
    trace =  TRUE
)

min_var_materials_portfolio <- optimize.portfolio(
    R = materials_returns_xts,
    portfolio = min_var_materials_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)

min_var_real_estate_portfolio <- optimize.portfolio(
    R = real_estate_returns_xts,
    portfolio = min_var_real_estate_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)

min_var_technology_portfolio <- optimize.portfolio(
    R = technology_returns_xts,
    portfolio = min_var_technology_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)

min_var_utilities_portfolio <- optimize.portfolio(
    R = utilities_returns_xts,
    portfolio = min_var_utilities_portfolio,
    optimize_method = "quadprog",
    trace = TRUE
)




min_var_consumer_discretionary_portfolio

min_var_consumer_staples_portfolio

min_var_energy_portfolio

min_var_financials_portfolio

min_var_health_care_portfolio

min_var_industrials_portfolio

min_var_materials_portfolio

min_var_real_estate_portfolio

min_var_technology_portfolio

min_var_utilities_portfolio


## Max Expected Returns specification ----

max_ret_consumer_discretionary_portfolio <- portfolio.spec(assets = assets)

max_ret_consumer_staples_portfolio <- portfolio.spec(assets = assets1)

max_ret_energy_portfolio <- portfolio.spec(assets = assets2)

max_ret_financials_portfolio <- portfolio.spec(assets = assets3)

max_ret_health_care_portfolio <- portfolio.spec(assets = assets4)

max_ret_industrials_portfolio <- portfolio.spec(assets = assets5)

max_ret_materials_portfolio <- portfolio.spec(assets = assets6)

max_ret_real_estate_portfolio <- portfolio.spec(assets = assets7)

max_ret_technology_portfolio <- portfolio.spec(assets = assets8)

max_ret_utilities_portfolio  <- portfolio.spec(assets = assets9)

## Add constraints ----


max_ret_consumer_discretionary_portfolio <- add.constraint(
    portfolio = max_ret_consumer_discretionary_portfolio,type = "full_investment")
max_ret_consumer_discretionary_portfolio <- add.constraint(
    portfolio = max_ret_consumer_discretionary_portfolio,
    type = "box", min = 0.01, max = "0.99")

max_ret_consumer_staples_portfolio <- add.constraint(portfolio = max_ret_consumer_staples_portfolio, type = "full_investment")
max_ret_consumer_staples_portfolio <- add.constraint(portfolio = max_ret_consumer_staples_portfolio,
                                                     type = "box", min = 0.01, max = 0.99)

max_ret_energy_portfolio <- add.constraint(portfolio = max_ret_energy_portfolio, type = "full_investment")
max_ret_energy_portfolio <- add.constraint(portfolio = max_ret_energy_portfolio,
                                           type = "box", min = 0.01, max = 0.99)

max_ret_financials_portfolio <- add.constraint(portfolio = max_ret_financials_portfolio,type = "full_investment")
max_ret_financials_portfolio <- add.constraint(portfolio = max_ret_financials_portfolio,
                                               type = "box", min = 0.01, max = 0.99)
max_ret_health_care_portfolio <- add.constraint(portfolio = max_ret_health_care_portfolio, type = "full_investment")
max_ret_health_care_portfolio <- add.constraint(portfolio = max_ret_health_care_portfolio, 
                                                type = "box", min = 0.01, max = 0.99)
max_ret_industrials_portfolio <- add.constraint(portfolio = max_ret_industrials_portfolio, type = "full_investment")
max_ret_industrials_portfolio <- add.constraint(portfolio = max_ret_industrials_portfolio,
                                                type = "box", min = 0.01, max = 0.99)

max_ret_materials_portfolio <- add.constraint(portfolio = max_ret_materials_portfolio,type ="full_investment")
max_ret_materials_portfolio <- add.constraint(portfolio = max_ret_materials_portfolio,
                                              type = "box", min = 0.01, max = 0.99)

max_ret_real_estate_portfolio <- add.constraint(portfolio = max_ret_real_estate_portfolio,type = "full_investment")
max_ret_real_estate_portfolio <- add.constraint(portfolio = max_ret_real_estate_portfolio, 
                                                type = "box", min = 0.01, max = 0.99)
max_ret_technology_portfolio <- add.constraint(portfolio = max_ret_technology_portfolio, type = "full_investment")
max_ret_technology_portfolio <- add.constraint(portfolio = max_ret_technology_portfolio, 
                                               type = "box", min = 0.01, max = 0.99)

max_ret_utilities_portfolio <- add.constraint(portfolio = max_ret_utilities_portfolio, type = "full_investment")
max_ret_utilities_portfolio <- add.constraint(portfolio = max_ret_utilities_portfolio,
                                              type = "box", min = 0.01, max = 0.99)
# Objective ----

max_ret_consumer_discretionary_portfolio <- add.objective(
    portfolio = max_ret_consumer_discretionary_portfolio,
    type      = "return",
    name      = "mean"
)
max_ret_consumer_staples_portfolio<-  add.objective(
    portfolio = max_ret_consumer_staples_portfolio,
    type = "return",
    name = "mean"
)

max_ret_energy_portfolio <- add.objective(
    portfolio = max_ret_consumer_staples_portfolio,
    type = "return",
    name = "mean"
)

max_ret_financials_portfolio <- add.objective(
    portfolio = max_ret_financials_portfolio,
    type = "return",
    name = "mean"
)

max_ret_health_care_portfolio <- add.objective(
    portfolio = max_ret_health_care_portfolio,
    type = "return",
    name = "mean"
)

max_ret_industrials_portfolio <- add.objective(
    portfolio = max_ret_industrials_portfolio,
    type      = "return",
    name      = "mean"
)

max_ret_materials_portfolio <- add.objective(
    portfolio = max_ret_materials_portfolio,
    type      = "return",
    name      = "mean"
)

max_ret_real_estate_portfolio <- add.objective(
    portfolio = max_ret_real_estate_portfolio,
    type      = "return",
    name      = "mean"
)

max_ret_technology_portfolio <- add.objective(
    portfolio = max_ret_technology_portfolio,
    type      = "return",
    name      = "mean"
)

max_ret_utilities_portfolio <- add.objective(
    portfolio = max_ret_utilities_portfolio,
    type      = "return",
    name      = "mean"
)



## optimization ----

max_ret_consumer_discretionary_portfolio <- optimize.portfolio(
    portfolio = max_ret_consumer_discretionary_portfolio,
    R = consumer_discretionary_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)

max_ret_consumer_staples_portfolio <- optimize.portfolio(
    portfolio = max_ret_consumer_staples_portfolio,
    R = consumer_staples_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)

max_ret_energy_portfolio <- optimize.portfolio(
    portfolio = max_ret_energy_portfolio,
    R = energy_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)
max_ret_financials_portfolio <- optimize.portfolio(
    portfolio = max_ret_financials_portfolio,
    R = financials_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)

max_ret_health_care_portfolio <- optimize.portfolio(
    portfolio = max_ret_health_care_portfolio,
    R = health_care_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)



max_ret_industrials_portfolio <- optimize.portfolio(
    portfolio = max_ret_industrials_portfolio,
    R = industrials_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)

max_ret_materials_portfolio <- optimize.portfolio(
    portfolio = max_ret_materials_portfolio,
    R = materials_returns_xts,
    optimize_method = "glpk",
    trace = TRUE
)

max_ret_real_estate_portfolio <- optimize.portfolio(
    portfolio       = max_ret_real_estate_portfolio,
    R               = real_estate_returns_xts,
    optimize_method = "glpk",
    trace           = TRUE
)

max_ret_technology_portfolio <- optimize.portfolio(
    portfolio       = max_ret_technology_portfolio,
    R               = technology_returns_xts,
    optimize_method = "glpk",
    trace           = TRUE
)


max_ret_utilities_portfolio <- optimize.portfolio(
    portfolio       =  max_ret_utilities_portfolio,
    R               = utilities_returns_xts,
    optimize_method = "glpk",
    trace           = TRUE
)

# Portfolios Creation ----

## Min_var portfolio ----

### Weights & Weights tbl ----
weights1<- pluck(.x = min_var_consumer_discretionary_portfolio, "weights")
weights2<- pluck(.x = min_var_consumer_staples_portfolio, "weights")
weights3<- pluck(.x = min_var_energy_portfolio, "weights")
weights4<- pluck(.x = min_var_financials_portfolio, "weights")
weights5<- pluck(.x = min_var_health_care_portfolio, "weights")
weights6<- pluck(.x = min_var_industrials_portfolio, "weights")
weights7<- pluck(.x = min_var_materials_portfolio, "weights")
weights8<- pluck(.x = min_var_real_estate_portfolio, "weights")
weights9<- pluck(.x = min_var_technology_portfolio, "weights")
weights10<- pluck(.x = min_var_utilities_portfolio, "weights")

weights_tbl_1 <- tibble(assets,weights1)
weights_tbl_2 <- tibble(assets1,weights2)
weights_tbl_3 <- tibble(assets2, weights3)
weights_tbl_4 <- tibble(assets3, weights4)
weights_tbl_5 <- tibble(assets4, weights5)
weights_tbl_6 <- tibble(assets5, weights6)
weights_tbl_7 <- tibble(assets6, weights7)
weights_tbl_8 <- tibble(assets7, weights8)
weights_tbl_9 <- tibble(assets8, weights9)
weights_tbl_10 <- tibble(assets9, weights10)

### Minimum Variance Portfolio ----

consumer_discretionary_min_var_port_tbl <- consumer_discretionary_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_1,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "consumer_discretion_min_var_portfolio",.before = 1)

consumer_staples_min_var_port_tbl <- consumer_staples_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_2,
        rebalance_on = "years"
    ) |> 
    add_column(symbol= "consumer_staples_min_var_portfolio",.before = 1)

energy_min_var_port_tbl <- energy_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_3,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "energy_min_var_portfolio", .before = 1)

financials_min_var_port_tbl <- financials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_4,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "financials_min_var_portfolio", .before = 1)

health_care_min_var_port_tbl <- health_care_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_5,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "health_care_min_var_portfolio",.before = 1)

industrials_min_var_port_tbl <- industrials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_6,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "industrials_min_var_portfolio", .before = 1)

materials_min_var_port_tbl <- materials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_7,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "materials_min_var_portfolio", .before = 1)

real_estate_min_var_port_tbl <- real_estate_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl_8,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "real_estate_min_var_portfolio", .before = 1)


    
 technology_min_var_port_tbl <- technology_returns_tbl |> 
     group_by(symbol) |> 
     tq_portfolio(
         assets_col = symbol,
         returns_col = monthly.returns,
         weights = weights_tbl_9,
         rebalance_on = "years"
     ) |> 
     add_column(symbol = "technology_min_var_portfolio", .before = 1)
 
 utilities_min_var_port_tbl <- utilities_returns_tbl |> 
     group_by(symbol) |> 
     tq_portfolio(
         assets_col = symbol,
         returns_col = monthly.returns,
         weights = weights_tbl_10,
         rebalance_on = "years"
     ) |> 
     add_column(symbol = "utilities_min_var_portfolio",.before = 1)

 # Saving the data ----
write_rds(consumer_discretionary_min_var_port_tbl, "Alternative_Portfolios/consumer_discretionary_min_var_port_tbl.rds")
write_rds(consumer_staples_min_var_port_tbl,"Alternative_Portfolios/consumer_staples_min_var_port_tbl.rds")
write_rds(energy_min_var_port_tbl,"Alternative_Portfolios/energy_min_var_port_tbl.rds")
write_rds(financials_min_var_port_tbl,"Alternative_Portfolios/financials_min_var_port_tbl.rds")
write_rds(health_care_min_var_port_tbl,"Alternative_Portfolios/health_care_min_var_port_tbl.rds")
write_rds(industrials_min_var_port_tbl,"Alternative_Portfolios/industrials_min_var_port_tbl.rds")
write_rds(materials_min_var_port_tbl,"Alternative_Portfolios/materials_min_var_port_tbl.rds")
write_rds(real_estate_min_var_port_tbl,"Alternative_Portfolios/real_estate_min_var_port_tbl.rds")
write_rds(technology_min_var_port_tbl,"Alternative_Portfolios/technology_min_var_port_tbl.rds")
write_rds(utilities_min_var_port_tbl,"Alternative_Portfolios/utilities_min_var_port_tbl.rds")

## Maximum Returns  Portfolio ----

### Weights & Weights tbl ----
 
 wts1<- pluck(.x = max_ret_consumer_discretionary_portfolio, "weights")
 wts2<- pluck(.x = max_ret_consumer_staples_portfolio, "weights")
 wts3<- pluck(.x = max_ret_energy_portfolio, "weights")
 wts4<- pluck(.x = max_ret_financials_portfolio, "weights")
 wts5<- pluck(.x = max_ret_health_care_portfolio, "weights")
 wts6<- pluck(.x = max_ret_industrials_portfolio, "weights")
 wts7<- pluck(.x = max_ret_materials_portfolio, "weights")
 wts8<- pluck(.x = max_ret_real_estate_portfolio, "weights")
 wts9<- pluck(.x = max_ret_technology_portfolio, "weights")
 wts10<- pluck(.x = max_ret_utilities_portfolio, "weights")

wts_tbl1 <- tibble(assets,wts1) 

wts_tbl2 <- tibble(assets1,wts2)

wts_tbl3 <- tibble(assets2,wts3)

wts_tbl4 <- tibble(assets3,wts4)     

wts_tbl5 <- tibble(assets4,wts5)

wts_tbl6 <- tibble(assets5, wts6)

wts_tbl7 <- tibble(assets6, wts7)

wts_tbl8 <- tibble(assets7, wts8)

wts_tbl9 <- tibble(assets8, wts9)

wts_tbl10 <- tibble(assets9, wts10)

### Max_Returns Portfolio ----
consumer_discretionary_max_ret_port_tbl <- consumer_discretionary_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl1,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "consumer_discretionary_max_ret_portfolio", .before = 1)

consumer_staples_max_ret_port_tbl <- consumer_staples_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl2,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "consumer_staples_max_ret_portfolio", .before = 1)

energy_max_ret_port_tbl <- energy_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl3,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "energy_max_ret_portfolio",.before = 1)

financials_max_ret_port_tbl <- financials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl4,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "financials_max_ret_portfolio", .before = 1)

health_care_max_ret_port_tbl <- health_care_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl5,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "health_care_max_ret_portfolio", .before = 1)

industrials_max_ret_port_tbl <- industrials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl6,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "industrials_max_ret_portfolio", .before = 1)

materials_max_ret_port_tbl <- materials_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl7,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "materials_max_ret_portfolio", .before = 1)

real_estate_max_ret_port_tbl <- real_estate_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl8,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "real_estate_max_ret_portfolio", .before = 1)

technology_max_ret_port_tbl <- technology_returns_tbl    |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl9,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "technology_max_ret_portfolio", .before = 1)

utilities_max_ret_port_tbl <- utilities_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = wts_tbl10,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "utilities_max_ret_portfolio", .before = 1)

# Saving the data ----

write_rds(consumer_discretionary_max_ret_port_tbl,"Alternative_Portfolios/consumer_discretionary_max_ret_port_tbl.rds")
write_rds(consumer_staples_max_ret_port_tbl,"Alternative_Portfolios/consumer_staples_max_ret_port_tbl.rds")
write_rds(energy_max_ret_port_tbl,"Alternative_Portfolios/energy_max_ret_port_tbl.rds")
write_rds(financials_max_ret_port_tbl,"Alternative_Portfolios/financials_max_ret_port_tbl.rds")
write_rds(health_care_max_ret_port_tbl,"Alternative_Portfolios/health_care_max_ret_port_tbl.rds")
write_rds(industrials_max_ret_port_tbl,"Alternative_Portfolios/industrials_max_ret_port_tbl.rds")
write_rds(materials_max_ret_port_tbl,"Alternative_Portfolios/materials_max_ret_port_tbl.rds")
write_rds(real_estate_max_ret_port_tbl,"Alternative_Portfolios/real_estate_max_ret_port_tbl.rds")
write_rds(technology_max_ret_port_tbl,"Alternative_Portfolios/technology_max_ret_port_tbl.rds")
write_rds(utilities_max_ret_port_tbl,"Alternative_Portfolios/utilities_max_ret_port_tbl.rds")

