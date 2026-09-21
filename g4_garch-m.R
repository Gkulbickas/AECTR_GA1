# Load data
tic_data <- read_csv("input/ticker_data.csv", show_col_types = FALSE) 
appl_ret <- tic_data[tic_data$TICKER=="AAPL", ][1:2500, "RET"] * 100
pfe_ret  <- tic_data[tic_data$TICKER=="PFE", ][1:2500, "RET"] * 100
jnj_ret  <- tic_data[tic_data$TICKER=="JNJ", ][1:2500, "RET"] * 100
mrk_ret  <- tic_data[tic_data$TICKER=="MRK", ][1:2500, "RET"] * 100

asset_rets <- list(PFE = pfe_ret, JNJ = jnj_ret, MRK = mrk_ret)

# Function for computing the loglikelihood for given GARCH-M model
ll_garch_m <- function(par, data){
    # Define parameters
    mu <- par[1]
    lambda <- par[2]
    omega <- par[3]
    alpha <- par[4]
    beta <- par[5]
    v <- par[6]

    # Check constraints
    if (beta < 0 || omega <= 0 || v <= 2) {
        return(1e10)
    }

    # Implementing statistical model
    data <- data[["RET"]]
    n <- length(data)
    sig2 <- rep(0,n)
    x_sub <- data[1:50]
    xbar <- mean(x_sub)
    sig2[1] <- mean((x_sub - xbar)^2)

    # Recursive variance loop
    for (t in 2:n){
        sig2[t] <- omega + alpha * ((data[t-1] - mu - lambda * sig2[t-1]) / sqrt(sig2[t-1]))^2 + beta * sig2[t-1]
    }

    # Compute loglikelihood
    ll <- n * (lgamma((v + 1) / 2) - lgamma(v / 2) - 0.5 * log(v * pi)) - 0.5 * sum(log(sig2)) - ((v + 1) / 2) * sum(log(1 + (data - mu - lambda * sig2)^2 / (v * sig2)))

    return(-ll)
}

# Function for computing AIC and BIC
compute_AIC_BIC <- function(logL, n, k){
    aic = 2 * k - 2 * logL 
    bic =  k * log(n) - 2 * logL
    return(c(aic, bic))
}

# Test using Table 1 optimal parameters for M2 APPL
test_opt_par <- c(mu = 0.072, lambda = 0.061, omega = 0.037, alpha = 0.089, beta = 0.875, v = 4.138)
test_est <- optim(par = test_opt_par, fn = function(p) ll_garch_m(p, appl_ret), method = "BFGS")
test_aic_bic <- compute_AIC_BIC(-test_est$value, length(appl_ret[["RET"]]), length(test_opt_par))
print("TEST RUN: Ticker, Estimated Parameters: mu, lambda, omega, alpha, beta, v, LogL, AIC, BIC")
cat("APPL", round(test_est$par, 3), round(-test_est$value, 0), round(test_aic_bic[1], 0), round(test_aic_bic[2], 0), "\n", "\n")

print("Ticker, Estimated Parameters: mu, lambda, omega, alpha, beta, v, LogL, AIC, BIC")

# Estimate parameters, AIC and BIC for each ticker using initial parameters
for(ticker in names(asset_rets)){
    # Define initial parameters for GARCH-M model
    ini_s2 <- var(asset_rets[[ticker]][["RET"]])
    garch_m_par <- c(mu = 0, lambda = 0, omega = ini_s2/50, alpha = 0.05, beta = 0.9, v = 10)
    est_garch_m <- optim(par = garch_m_par, fn = function(p) ll_garch_m(p, asset_rets[[ticker]]), method = "BFGS")
    aic_bic_garch_m <- compute_AIC_BIC(-est_garch_m$value, nrow(asset_rets[[ticker]]), length(garch_m_par))
    cat(ticker, round(est_garch_m$par, 3), round(-est_garch_m$value, 0), round(aic_bic_garch_m[1], 0), round(aic_bic_garch_m[2], 0), "\n")
}
