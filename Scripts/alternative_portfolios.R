# Global Minimum Variance Portfolio ----

#libraries----

library(tidymodels)
library(tidyquant)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(plotly)
library(timetk)
library(modeltime)
library(readr)
library(purrr)
library(furrr)
# tickers ----

symbols <- read_rds("Symbols/communications.rds")

# Returns ----

communications_returns_tbl <- read_rds("stock-returns/communications_returns_tbl.rds")

# Returns in wider format ----

communications_returns_xts <- communications_returns_tbl |> 
    spread(symbol, monthly.returns) |> 
    tk_xts(date_var = date)

# Portfolio Object ----

library(PortfolioAnalytics)


# minimum_variance_portfolio spec 

min_var_communications_portfolio <- portfolio.spec(assets = symbols)

# Adding constraints ----

min_var_communications_portfolio <- add.constraint(portfolio = min_var_communications_portfolio,
                                                   type = "full_investment")
min_var_communications_portfolio <- add.constraint(portfolio = min_var_communications_portfolio,
                                                   type = "box", min = 0.01, max = 0.99)
# Adding objective ----

min_var_communications_portfolio <- add.objective(portfolio = min_var_communications_portfolio,
                                                  type = "risk", name = "var")
# Optimization ----

min_var_communications_portfolio <- optimize.portfolio(
    R = communications_returns_xts,
    portfolio = min_var_communications_portfolio,
    optimize_method = "quadprog",
    trace = TRUE

)
min_var_communications_portfolio

# Max expected return portfolio spec ----


max_ret_communications_portfolio <- portfolio.spec(assets = symbols)

# adding constraint ----

max_ret_communications_portfolio <- PortfolioAnalytics::add.constraint(
    portfolio = max_ret_communications_portfolio,
    type = "full_investment"
)

max_ret_communications_portfolio <- add.constraint(
    portfolio = max_ret_communications_portfolio,
    type = "box", min = 0.01, max = 0.99
)

# adding objective ----

max_ret_communications_portfolio <-  add.objective(
    portfolio = max_ret_communications_portfolio,
    type = "return",
    name = "mean"
)

# Optimization ----

max_ret_communications_portfolio <- PortfolioAnalytics::optimize.portfolio(
    R = communications_returns_xts,
    portfolio = max_ret_communications_portfolio,
    # This defaults to the "glpk" solver
    optimize_method = "glpk",
    # Return additional information on the path or portfolios searched
    trace = TRUE
)
# Examine returned portfolio list object

max_ret_communications_portfolio

weights <- pluck(.x = max_ret_communications_portfolio, "weights") |>  as.numeric()

weights_tbl <- tibble(symbols,weights)

communications_max_ret_port_tbl <- communications_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "communication_max_ret_portfolio",.before = 1)

communications_max_ret_port_tbl  |> 
    ggplot(aes(date, portfolio.returns)) +
    geom_line() + scale_y_continuous(labels = scales::percent)

weights<- pluck(.x = min_var_communications_portfolio, "weights")    

weights_tbl <- tibble(symbols,weights)

communications_min_var_port_tbl <- communications_returns_tbl |> 
    group_by(symbol) |> 
    tq_portfolio(
        assets_col = symbol,
        returns_col = monthly.returns,
        weights = weights_tbl,
        rebalance_on = "years"
    ) |> 
    add_column(symbol = "communication_min_var_portfolio",.before = 1)

communications_min_var_port_tbl |> 
    ggplot(aes(date, portfolio.returns)) +
    geom_line() + scale_y_continuous(labels = scales::percent)

write_rds(communications_max_ret_port_tbl,"Alternative_Portfolios/communications_max_ret_port_tbl.rds")

write_rds(communications_min_var_port_tbl,"Alternative_Portfolios/communications_min_var_port_tbl.rds")


# Monte Carlo simulation ----

mean_ret <- colMeans(communications_returns_xts)

cov_mat <-  cov(communications_returns_xts) * 12

num_port <- 5000 #simulations

all_wts <- matrix(nrow = num_port, ncol = length(symbols))

# Create an empty vector to store  returns
port_returns <- vector('numeric', length = num_port)

port_risk <- vector('numeric', length = num_port)

sharpe_ratio <- vector('numeric', length = num_port)

for (i in seq_along(port_returns)) {
 
  wts <- runif(length(symbols))
wts <- wts/sum(wts)

         # Store weights in the matrix
        all_wts[i,] <- wts
             # Portfolio returns
         port_ret <- sum(wts * mean_ret)* 12
                 
             # Store Portfolio Returns 
             port_returns[i] <- port_ret
                     # Creating and storing portfolio risk
                          port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
                         port_risk[i] <- port_sd
                        
                          # Creating and storing Portfolio Sharpe Ratios
                         # Assuming 0 %Risk free rate
                             sr <- port_ret/port_sd
                             sharpe_ratio[i] <- sr
                             
}

portfolio_values <- tibble(Return = port_returns, Risk = port_risk, SharpeRatio = sharpe_ratio)  

all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(communications_returns_xts)

portfolio_values <- cbind(all_wts,portfolio_values) 

# Optimal Portfolio ----

(min_var <- portfolio_values[which.min(portfolio_values$Risk),])


(max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),])


# Plot ----
p <- min_var |> 
    pivot_longer(CMCSA:VZ, names_to = "Asset", values_to = "Weights") |> 
    mutate(Asset = as.factor(Asset)) |> 
    ggplot(aes(x = fct_reorder(Asset,Weights),y = Weights , fill = Asset)) +
    geom_bar(stat = "identity") + theme_tq() +
    scale_y_continuous(labels = scales::percent) +
    scale_color_tq(theme = "dark") +
    labs(
        x = "Asset", y = "Weights",
        title = "Minimum Variance Portfolio for Communications Sector",
        subtitle = "Verizon(VZ) has the highest weights and Comcast(CMCSA) the lowest",
        caption  = "Over the period 1990-2022"
        
    ) +
    theme(legend.position = "none")

library(plotly)

ggplotly(p)


s <- max_sr |> 
    pivot_longer(CMCSA:VZ, names_to = "Asset", values_to = "Weights") |> 
    mutate(Asset = as.factor(Asset)) |> 
    ggplot(aes(x = fct_reorder(Asset,Weights),y = Weights , fill = Asset)) +
    geom_bar(stat = "identity") + theme_tq() +
    scale_y_continuous(labels = scales::percent) +
    scale_color_tq(theme = "dark") +
    labs(
        x = "Asset", y = "Weights",
        title = "Tangency Portfolio for Communications Sector",
        subtitle = "Verizon(VZ) has the lowest weight and Omnicom(OMC) the highest",
        caption  = "Over the period 1990-2022"
        
    ) +
    theme(legend.position = "none")
ggplotly(s)    

# Portfolio Values and Efficient Frontier ----
V <-  portfolio_values |> 
    ggplot(aes(Risk, Return)) +
    geom_point(aes(color = SharpeRatio)) +
    scale_color_gradient(low = "yellow", high = "red") +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(
        title = "Efficient Frontier",
        x = "Annualized Risk",
        y = "Annualized Returns"
    ) +
geom_point(aes(Risk,Return), data = min_var) +
    geom_point(aes(Risk,Return), data = max_sr, color = "blue") +
    annotate("text",x = 0.185, y = 0.11,label = "Minimum Variance Portfolio") +
    annotate("text", x = 0.19, y = 0.124, label = "Tangency Portfolio")

ggplotly(V)

  

    

