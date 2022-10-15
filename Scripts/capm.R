# CAPM - Function ----

## estimate_capm ----

estimate_capm <- function(data, min_obs = 1) {
    if (nrow(data) < min_obs) {
        beta <- as.numeric(NA)
        
    } else {
        fit <- lm(excess_returns ~ mkt_excess + hml + smb , data = data)
        # note that we take into account the coefficient of mkt_excess
        beta <- as.numeric(coefficients(fit)[2])
        
    }
    return(beta)
}


## Rolling capm ----

# rolling capm estimation
roll_capm_estimation <- function(data, months, min_obs) {
    data <- data |>
        arrange(date)
    
    betas <- slide_period_vec(
        .x = data,
        .i = data$date,
        .period = "month",
        .f = ~ estimate_capm(., min_obs),
        .before = months - 1,
        .complete = FALSE
    )
    
    return(tibble(
        month = unique(data$date),
        beta = betas
        
    ))
}

write_rds(estimate_capm,"capm-function/estimate_capm.rds")

write_rds(roll_capm_estimation,"capm-function/roll_capm_estimation.rds")
