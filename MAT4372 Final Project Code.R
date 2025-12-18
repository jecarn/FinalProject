# Importing libraries
library(Matrix)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(ggplot2)
library(patchwork)
library(corrplot)
library(plotly)
library(reshape2)
library(akima)
library(latex2exp)

# Defining the Starting Period

# Defining the last sample period
t_0 <- "2025-11-01"

# Defining the number of sample periods (months)
n <- 60

# Defining the option maturity (years)
T <- 1

# Obtaining Price Data and Calculating Returns

# Stock tickers vector
tickers <- c("NVDA", "AMD")

# Pulling stock price data from Yahoo Finance
prices_daily <- tickers %>%
  tq_get(get = "stock.prices")

# Transforming the daily price data to monthly prices
prices_monthly <- prices_daily %>%
  group_by(symbol) %>%
  tq_transmute(select = close, mutate_fun = to.monthly, indexAt = "firstof")
prices_monthly <- data.frame(prices_monthly)
prices_monthly <- reshape(prices_monthly, idvar = "date", timevar = "symbol", direction = "wide")

# Creating an empty dataframe for monthly returns
returns_monthly <- tail(prices_monthly[1], -1)

# Calculating monthly log returns for each stock
for (i in 1:length(tickers)){
  log_returns <- with(prices_monthly, diff(log(prices_monthly[,i+1])))
  returns_monthly <- cbind(returns_monthly, log_returns)
}
colnames(returns_monthly) <- c("date", tickers)

# Reversing order of dataframes
prices_monthly <- prices_monthly[nrow(prices_monthly):1,]
returns_monthly <- returns_monthly[nrow(returns_monthly):1,]

# Excluding periods out of sample
returns_monthly <- head(returns_monthly[returns_monthly$date <= t_0,], n)

# Transforming returns structure
returns_long <- gather(returns_monthly[,-1], factor_key=TRUE)

# Extracting asset prices at the final sample period
prices_0 <- t(head(prices_monthly[prices_monthly$date <= t_0,], 1)[,c(-1)])
colnames(prices_0) <- "price_0"

# Calculating standard deviation of monthly log returns
volatility_summary <- returns_long %>% group_by(key) %>%
  summarise(annualized_vol = sd(value) * sqrt(12),
            mean_return = mean(value) * sqrt(12)) # annualized volatility
asset_summary <- cbind(volatility_summary, prices_0)

returns_correlation <- cor(returns_monthly$NVDA, returns_monthly$AMD)

# Returns Plot

# Transforming the monthly returns to a wide format containing the date
returns_long_date <- returns_monthly %>%
  pivot_longer(
    cols = c(NVDA, AMD),
    names_to = "stock",
    values_to = "monthly_return"
  )

stock_colours <- c("AMD" = "#1F968BFF", "NVDA" = "#404788FF")

# Creating a plot of the monthly returns
returns_plot <- ggplot(returns_long_date) + 
  aes(x = date, y = monthly_return, group = stock, colour = stock) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgray") +
  geom_line(lwd = 0.6) +
  labs(
    title = "Monthly Logarithmic Returns",
    x = "Date",
    y = "Monthly Log Return",
    color = "Stock"
  ) +
  scale_color_manual(values = stock_colours)

# Saving the plot
# ggsave("returns.png", plot = returns_plot, width = 7, height = 4)

# Obtaining Risk-Free Rate Data

# Loading risk free rate data from FRED
rf_monthly <- tq_get("TB3MS", get = "economic.data")

# Converting rates to percentage
rf_monthly$price <- rf_monthly$price / 100

# Averaging the 3-month rate over the sample period
rf <- mean(tail(rf_monthly[rf_monthly$date <= t_0,], n)$price)

# Simulation Parameters

# Initializing pseudo RNG seed
set.seed(1)

# Defining simulation parameters
M <- 10000
N_rho <- 20 # even number
N_time <- 100
price_range <- 0.6 # %
price_dif <- 0.01 # %
vol_range <- 0.3 # %
vol_dif <- 0.01 # %

# Drawing independent random normal values
Z_1 <- rnorm(M)
Z_2 <- rnorm(M)

# Defining independent Brownian motions
W_1_T <- Z_1 * sqrt(T)
W_2_T <- Z_2 * sqrt(T)

# Defining initial asset prices
NVDA_0 <- asset_summary[[1,4]]
AMD_0 <- asset_summary[[2,4]]

# Defining asset volatility
NVDA_vol <- asset_summary[[1,2]]
AMD_vol <- asset_summary[[2,2]]

# Defining strike prices as initial values
basket_strike <- (NVDA_0 + AMD_0) / 2
spread_strike <- AMD_0 - NVDA_0

# Partitioning correlation values
rho_partition <- seq(-1, 1, length.out = N_rho + 1)

# Creating increments of the initial asset prices
price_incremements <- seq(-price_range, price_range, price_dif)

# Partitioning the option duration (time to maturity)
time_partition <- seq(0, 1 + (1 / N_time), 1 / N_time)

# Creating increments of the volatility of returns
volatility_increments <- seq(-vol_range, vol_range, vol_dif)

# Defining the payoff function at maturity
payoff <- function(x){
  max(x, 0)
}

# Defining a Surface Plot Function

# Defining a function to create surface plots
surface_3d_plot <- function(df, x, y, z, x_title, y_title, z_title){
  # Creating a matrix of values
  data_matrix <- df[,c(x, y, z)] %>%
    pivot_wider(names_from = y, values_from = z) %>%
    select(-x) %>%
    as.matrix()
  
  # Obtaining unique x and y values
  x_vals <- unique(df[[x]])
  y_vals <- unique(df[[y]])
  
  # Plotting surface
  p <- plot_ly(x = x_vals, y = y_vals, z = t(data_matrix),
               type = "surface", showscale = FALSE, opacity = 0.8, colorscale = "Viridis")
  
  # Adding y-axis wireframe
  for (i in seq_along(y_vals)){
    p <- add_trace(p, x = x_vals, y = rep(y_vals[i], length(x_vals)), z = t(data_matrix)[i,],
                   type = "scatter3d", mode = "lines", line = list(color = "black"), showlegend = FALSE)
  }
  
  # Adding x-axis wireframe
  for (j in round(seq(1, length(x_vals), length.out = N_rho))){
    p <- add_trace(p, x = rep(x_vals[j], length(y_vals)), y = y_vals, z = t(data_matrix)[, j],
                   type = "scatter3d", mode = "lines", line = list(color = "black"), showlegend = FALSE)
  }
  
  # Adding axis titles
  p <- p %>% layout(scene = list(
    xaxis = list(title = x_title),
    yaxis = list(title = y_title),
    zaxis = list(title = z_title)))
  
  return(p)
}

# Delta and Gamma

# Defining empty vectors to capture simulation results
b_call_vec <- c()
b_put_vec <- c()
b_0_vec <- c()
b_rho_vec <- c()

s_call_vec <- c()
s_put_vec <- c()
s_0_vec <- c()
s_rho_vec <- c()

# Iterating over correlation values
for (rho in rho_partition){
  # Defining asset price paths based on correlation
  W_NVDA_T <- W_1_T
  W_AMD_T <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - NVDA_vol^2 / 2) * T + NVDA_vol * W_NVDA_T)
  AMD_path <- exp((rf - AMD_vol^2 / 2) * T + AMD_vol * W_AMD_T)
  
  # Iterating over initial asset prices
  for (percentage_change in price_incremements){
    # Calculating initial asset prices
    basket_0 <- (NVDA_0 * (1 + percentage_change) + AMD_0 * (1 + percentage_change)) / 2
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * (1 + percentage_change) * NVDA_path
    AMD_t <- AMD_0 * (1 + percentage_change) * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    # Discounting average payoffs at maturity
    basket_call <- exp(-rf * T) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put <- exp(-rf * T) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec <- c(b_put_vec, basket_put)
    b_0_vec <- c(b_0_vec, basket_0)
    b_rho_vec <- c(b_rho_vec, rho)
  }
}

# Iterating over correlation values
for (rho in rho_partition){
  # Defining asset price paths based on correlation
  W_NVDA_T <- W_1_T
  W_AMD_T <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - NVDA_vol^2 / 2) * T + NVDA_vol * W_NVDA_T)
  AMD_path <- exp((rf - AMD_vol^2 / 2) * T + AMD_vol * W_AMD_T)
  
  # Iterating over initial asset prices
  for (percentage_change in price_incremements){
    # Calculating initial asset prices
    spread_0 <- AMD_0 * (1 + percentage_change) - NVDA_0 * (1 - percentage_change)
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * (1 - percentage_change) * NVDA_path
    AMD_t <- AMD_0 * (1 + percentage_change) * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    # Discounting average payoffs at maturity
    spread_call <- exp(-rf * T) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put <- exp(-rf * T) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec <- c(s_put_vec, spread_put)
    s_0_vec <- c(s_0_vec, spread_0)
    s_rho_vec <- c(s_rho_vec, rho)
  }
}

# Defining a dataframe containing basket prices
df_basket_prices <- data.frame(
  cbind(b_call_vec, b_put_vec, b_0_vec, b_rho_vec))

# Defining a dataframe to capture estimated basket Delta and Gamma
df_basket_delta_gamma <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  b_0_vec = numeric(),
  b_rho_vec = numeric(),
  b_call_delta = numeric(),
  b_put_delta = numeric())

# Defining a dataframe containing spread prices
df_spread_prices <- data.frame(
  cbind(s_call_vec, s_put_vec, s_0_vec, s_rho_vec))

# Defining a dataframe to capture estimated spread Delta and Gamma
df_spread_delta_gamma <- data.frame(
  s_call_vec = numeric(),
  s_put_vec = numeric(),
  s_0_vec = numeric(),
  s_rho_vec = numeric(),
  s_call_delta = numeric(),
  s_put_delta = numeric())

# Iterating over correlation values
for (rho in rho_partition){
  # Estimating basket Delta and Gamma using centered finite differences
  df_options_temp <- df_basket_prices[df_basket_prices$b_rho_vec == rho,]
  df_options_temp <- df_options_temp %>%
    mutate(b_call_delta = (lead(b_call_vec) - lag(b_call_vec)) / (lead(b_0_vec) - lag(b_0_vec)),
           b_put_delta = (lead(b_put_vec) - lag(b_put_vec)) / (lead(b_0_vec) - lag(b_0_vec)),
           b_call_gamma = (lead(b_call_vec, 10) - 2 * b_call_vec + lag(b_call_vec, 10)) / ((20 * (lead(b_0_vec) - b_0_vec))^2),
           b_put_gamma = (lead(b_put_vec, 10) - 2 * b_put_vec + lag(b_put_vec, 10)) / ((20 * (lead(b_0_vec) - b_0_vec))^2))
  df_options_temp <- tail(head(df_options_temp, -1), -1)
  df_basket_delta_gamma <- rbind(df_basket_delta_gamma, df_options_temp)
}

# Iterating over correlation values
for (rho in rho_partition){
  # Estimating spread Delta and Gamma using centered finite differences
  df_options_temp <- df_spread_prices[df_spread_prices$s_rho_vec == rho,]
  df_options_temp <- df_options_temp %>%
    mutate(s_call_delta = (lead(s_call_vec) - lag(s_call_vec)) / (lead(s_0_vec) - lag(s_0_vec)),
           s_put_delta = (lead(s_put_vec) - lag(s_put_vec)) / (lead(s_0_vec) - lag(s_0_vec)),
           s_call_gamma = (lead(s_call_vec, 10) - 2 * s_call_vec + lag(s_call_vec, 10)) / ((20 * (lead(s_0_vec) - s_0_vec))^2),
           s_put_gamma = (lead(s_put_vec, 10) - 2 * s_put_vec + lag(s_put_vec, 10)) / ((20 * (lead(s_0_vec) - s_0_vec))^2))
  df_options_temp <- tail(head(df_options_temp, -1), -1)
  df_spread_delta_gamma <- rbind(df_spread_delta_gamma, df_options_temp)
}

# Basket Call Price Plot (Price)

b_call_price_plot <- surface_3d_plot(
  df_basket_delta_gamma,
  "b_0_vec", "b_rho_vec", "b_call_vec",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Price")

b_call_price_plot <- b_call_price_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

b_call_price_plot

# Basket Put Price Plot (Price)

b_put_price_plot <- surface_3d_plot(
  df_basket_delta_gamma,
  "b_0_vec", "b_rho_vec", "b_put_vec",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Price")

b_put_price_plot <- b_put_price_plot %>% layout(scene = list(
  xaxis = list(autorange = "reversed"),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

b_put_price_plot

# Basket Call Delta Plot

b_call_delta_plot <- surface_3d_plot(
  df_basket_delta_gamma,
  "b_0_vec", "b_rho_vec", "b_call_delta",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Delta (Δ)")

b_call_delta_plot <- b_call_delta_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

b_call_delta_plot

# Basket Put Delta Plot

b_put_delta_plot <- surface_3d_plot(
  df_basket_delta_gamma,
  "b_0_vec", "b_rho_vec", "b_put_delta",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Delta (Δ)")

b_put_delta_plot <- b_put_delta_plot %>% layout(scene = list(
  yaxis = list(range = c(1, -1.1)),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

b_put_delta_plot

# Basket Call Gamma Plot

b_call_gamma_plot <- surface_3d_plot(
  df_basket_delta_gamma[!is.na(df_basket_delta_gamma$b_call_gamma),],
  "b_0_vec", "b_rho_vec", "b_call_gamma",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Gamma (Γ)")

b_call_gamma_plot <- b_call_gamma_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = 0.9, y = -2.2, z = 0.5))))

b_call_gamma_plot

# Basket Put Gamma Plot

b_put_gamma_plot <- surface_3d_plot(
  df_basket_delta_gamma[!is.na(df_basket_delta_gamma$b_call_gamma),],
  "b_0_vec", "b_rho_vec", "b_put_gamma",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Gamma (Γ)")

b_put_gamma_plot <- b_put_gamma_plot %>% layout(scene = list(
  xaxis = list(range = c(98, 310)),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -0.9, y = 2.2, z = 0.5))))

b_put_gamma_plot

# Spread Call Price Plot (Price)

s_call_price_plot <- surface_3d_plot(
  df_spread_delta_gamma,
  "s_0_vec", "s_rho_vec", "s_call_vec",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Price")

s_call_price_plot <- s_call_price_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

s_call_price_plot

# Spread Put Price Plot (Price)

s_put_price_plot <- surface_3d_plot(
  df_spread_delta_gamma,
  "s_0_vec", "s_rho_vec", "s_put_vec",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Price")

s_put_price_plot <- s_put_price_plot %>% layout(scene = list(
  xaxis = list(autorange = "reversed"),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

s_put_price_plot

# Spread Call Delta Plot

s_call_delta_plot <- surface_3d_plot(
  df_spread_delta_gamma,
  "s_0_vec", "s_rho_vec", "s_call_delta",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Delta (Δ)")

s_call_delta_plot <- s_call_delta_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

s_call_delta_plot

# Spread Put Delta Plot

s_put_delta_plot <- surface_3d_plot(
  df_spread_delta_gamma,
  "s_0_vec", "s_rho_vec", "s_put_delta",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Delta (Δ)")

s_put_delta_plot <- s_put_delta_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -1.4, y = -1.9, z = 0.8))))

s_put_delta_plot

# Spread Call Gamma Plot

s_call_gamma_plot <- surface_3d_plot(
  df_spread_delta_gamma[!is.na(df_spread_delta_gamma$s_call_gamma),],
  "s_0_vec", "s_rho_vec", "s_call_gamma",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Gamma (Γ)")

s_call_gamma_plot <- s_call_gamma_plot %>% layout(scene = list(
  xaxis = list(range = c(-157, 240)),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -0.9, y = 2.2, z = 0.5))))

s_call_gamma_plot

# Spread Put Gamma Plot

s_put_gamma_plot <- surface_3d_plot(
  df_spread_delta_gamma[!is.na(df_spread_delta_gamma$s_put_gamma),],
  "s_0_vec", "s_rho_vec", "s_put_gamma",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Gamma (Γ)")

s_put_gamma_plot <- s_put_gamma_plot %>% layout(scene = list(
  xaxis = list(range = c(-165, 240)),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = 0.9, y = -2.2, z = 0.5))))

s_put_gamma_plot

# Theta

# Defining empty vectors to capture simulation results
b_call_vec <- c()
b_put_vec <- c()
b_time_vec <- c()
b_rho_vec <- c()

s_call_vec <- c()
s_put_vec <- c()
s_time_vec <- c()
s_rho_vec <- c()

# Iterating over correlation values
for (rho in rho_partition){
  # Iterating over time to maturity
  for (t in time_partition){
    # Defining asset price paths based on correlation and time to maturity
    W_1_t <- Z_1 * sqrt(t)
    W_2_t <- Z_2 * sqrt(t)
    
    W_NVDA_t <- W_1_t
    W_AMD_t <- rho * W_1_t + sqrt(1 - rho^2) * W_2_t
    
    NVDA_path <- exp((rf - NVDA_vol^2 / 2) * t + NVDA_vol * W_NVDA_t)
    AMD_path <- exp((rf - AMD_vol^2 / 2) * t + AMD_vol * W_AMD_t)
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * NVDA_path
    AMD_t <- AMD_0 * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    # Discounting average payoffs at maturity
    basket_call <- exp(-rf * t) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put <- exp(-rf * t) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec <- c(b_put_vec, basket_put)
    b_time_vec <- c(b_time_vec, t)
    b_rho_vec <- c(b_rho_vec, rho)
  }
}

# Iterating over correlation values
for (rho in rho_partition){
  # Iterating over time to maturity
  for (t in time_partition){
    # Defining asset price paths based on correlation and time to maturity
    W_1_t <- Z_1 * sqrt(t)
    W_2_t <- Z_2 * sqrt(t)
    
    W_NVDA_t <- W_1_t
    W_AMD_t <- rho * W_1_t + sqrt(1 - rho^2) * W_2_t
    
    NVDA_path <- exp((rf - NVDA_vol^2 / 2) * t + NVDA_vol * W_NVDA_t)
    AMD_path <- exp((rf - AMD_vol^2 / 2) * t + AMD_vol * W_AMD_t)
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * NVDA_path
    AMD_t <- AMD_0 * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    # Discounting average payoffs at maturity
    spread_call <- exp(-rf * t) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put <- exp(-rf * t) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec <- c(s_put_vec, spread_put)
    s_time_vec <- c(s_time_vec, t)
    s_rho_vec <- c(s_rho_vec, rho)
  }
}

# Defining a dataframe containing basket prices
df_basket_prices <- data.frame(
  cbind(b_call_vec, b_put_vec, b_time_vec, b_rho_vec))

# Defining a dataframe to capture estimated spread Theta
df_basket_theta <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  time_vec = numeric(),
  rho_vec = numeric(),
  b_call_theta = numeric(),
  b_put_theta = numeric())

# Defining a dataframe containing spread prices
df_spread_prices <- data.frame(
  cbind(s_call_vec, s_put_vec, s_time_vec, s_rho_vec))

# Defining a dataframe to capture estimated spread Theta
df_spread_theta <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  time_vec = numeric(),
  rho_vec = numeric(),
  b_call_theta = numeric(),
  b_put_theta = numeric())

# Iterating over correlation values
for (rho in rho_partition){
  # Estimating basket Theta using forward finite differences
  options_df_temp <- df_basket_prices[df_basket_prices$b_rho_vec == rho,]
  options_df_temp <- options_df_temp %>%
    mutate(b_call_theta = (b_call_vec - lead(b_call_vec)) / (lead(b_time_vec) - b_time_vec),
           b_put_theta = (b_put_vec - lead(b_put_vec)) / (lead(b_time_vec) - b_time_vec))
  options_df_temp <- head(options_df_temp, -1)
  df_basket_theta <- rbind(df_basket_theta, options_df_temp)
}

# Iterating over correlation values
for (rho in rho_partition){
  # Estimating spread Theta using forward finite differences
  options_df_temp <- df_spread_prices[df_spread_prices$s_rho_vec == rho,]
  options_df_temp <- options_df_temp %>%
    mutate(s_call_theta = (s_call_vec - lead(s_call_vec)) / (lead(s_time_vec) - s_time_vec),
           s_put_theta = (s_put_vec - lead(s_put_vec)) / (lead(s_time_vec) - s_time_vec))
  options_df_temp <- head(options_df_temp, -1)
  df_spread_theta <- rbind(df_spread_theta, options_df_temp)
}

# Basket Call Price Plot (Time)

b_call_price_time_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_call_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Call Price")

b_call_price_time_plot <- b_call_price_time_plot %>% layout(scene = list(
  xaxis = list(range = c(1.05, 0)),
  camera = list(
    eye = list(x = 1.2, y = -2.1, z = 0.8))))

b_call_price_time_plot

# Basket Put Price Plot (Time)

b_put_price_time_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_put_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Put Price")

b_put_price_time_plot <- b_put_price_time_plot %>% layout(scene = list(
  xaxis = list(range = c(1.05, 0)),
  camera = list(
    eye = list(x = 1.2, y = -2.1, z = 0.8))))

b_put_price_time_plot

# Basket Call Theta Plot (Time)

b_call_theta_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_call_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Call Theta (Θ)")

b_call_theta_plot <- b_call_theta_plot %>% layout(scene = list(
  zaxis = list(range = c(-150, -5)),
  camera = list(
    eye = list(x = -1, y = 2.3, z = 0.5))))

b_call_theta_plot

# Basket Put Theta Plot (Time)

b_put_theta_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_put_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Put Theta (Θ)")

b_put_theta_plot <- b_put_theta_plot %>% layout(scene = list(
  zaxis = list(range = c(-150, -5)),
  camera = list(
    eye = list(x = -1, y = 2.3, z = 0.5))))

b_put_theta_plot

# Spread Call Price Plot (Time)

s_call_price_time_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_call_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Call Price")

s_call_price_time_plot <- s_call_price_time_plot %>% layout(scene = list(
  xaxis = list(range = c(1.05, 0)),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = 1, y = -2.2, z = 0.6))))

s_call_price_time_plot

# Spread Put Price Plot (Time)

s_put_price_time_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_put_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Put Price")

s_put_price_time_plot <- s_put_price_time_plot %>% layout(scene = list(
  xaxis = list(range = c(1.05, 0)),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = 1, y = -2.2, z = 0.6))))

s_put_price_time_plot

# Spread Call Theta Plot (Time)

s_call_theta_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_call_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Call Theta (Θ)")

s_call_theta_plot <- s_call_theta_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  zaxis = list(range = c(-150, -1)),
  camera = list(
    eye = list(x = -1, y = 2.3, z = 0.5))))

s_call_theta_plot

# Spread Put Theta Plot (Time)

s_put_theta_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_put_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Put Theta (Θ)")

s_put_theta_plot <- s_put_theta_plot %>% layout(scene = list(
  yaxis = list(autorange = "reversed"),
  zaxis = list(range = c(-150, -1)),
  camera = list(
    eye = list(x = -1, y = 2.3, z = 0.5))))

s_put_theta_plot

# Vega

# BASKET

# Defining empty vectors to capture simulation results
b_call_vec <- c()
b_put_vec <- c()
b_0_vec <- c()
b_rho_vec <- c()
b_vol_vec <- c()

# Iterating over correlation values
for (rho in rho_partition){
  # Defining asset price paths based on correlation and higher volatility
  W_NVDA_T <- W_1_T
  W_AMD_T <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol + vol_dif)^2 / 2) * T + (NVDA_vol + vol_dif) * W_NVDA_T)
  AMD_path <- exp((rf - (AMD_vol + vol_dif)^2 / 2) * T + (AMD_vol + vol_dif) * W_AMD_T)
  
  # Iterating over initial asset prices
  for (percentage_change in price_incremements){
    # Calculating initial asset prices
    basket_0 <- (NVDA_0 * (1 + percentage_change) + AMD_0 * (1 + percentage_change)) / 2
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * (1 + percentage_change) * NVDA_path
    AMD_t <- AMD_0 * (1 + percentage_change) * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    # Calculating basket return volatility
    basket_vol <- sd(log(basket_t / basket_0))
    
    # Discounting average payoffs at maturity
    basket_call <- exp(-rf * T) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put <- exp(-rf * T) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec <- c(b_put_vec, basket_put)
    b_0_vec <- c(b_0_vec, basket_0)
    b_rho_vec <- c(b_rho_vec, rho)
    b_vol_vec <- c(b_vol_vec, basket_vol)
  }
}

# Defining a dataframe containing basket prices (higher volatility)
df_basket_prices_vol_up <- data.frame(
  cbind(b_call_vec, b_put_vec, b_0_vec, b_rho_vec, b_vol_vec))

# Defining empty vectors to capture simulation results
b_call_vec <- c()
b_put_vec <- c()
b_0_vec <- c()
b_rho_vec <- c()
b_vol_vec <- c()

# Iterating over correlation values
for (rho in rho_partition){
  # Defining asset price paths based on correlation and lower volatility
  W_NVDA_T <- W_1_T
  W_AMD_T <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol - vol_dif)^2 / 2) * T + (NVDA_vol - vol_dif) * W_NVDA_T)
  AMD_path <- exp((rf - (AMD_vol - vol_dif)^2 / 2) * T + (AMD_vol - vol_dif) * W_AMD_T)
  
  # Iterating over initial asset prices
  for (percentage_change in price_incremements){
    # Calculating initial asset prices
    basket_0 <- (NVDA_0 * (1 + percentage_change) + AMD_0 * (1 + percentage_change)) / 2
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * (1 + percentage_change) * NVDA_path
    AMD_t <- AMD_0 * (1 + percentage_change) * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    # Calculating basket return volatility
    basket_vol <- sd(log(basket_t / basket_0))
    
    # Discounting average payoffs at maturity
    basket_call <- exp(-rf * T) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put <- exp(-rf * T) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec <- c(b_put_vec, basket_put)
    b_0_vec <- c(b_0_vec, basket_0)
    b_rho_vec <- c(b_rho_vec, rho)
    b_vol_vec <- c(b_vol_vec, basket_vol)
  }
}

# Defining a dataframe containing basket prices (lower volatility)
df_basket_prices_vol_down <- data.frame(
  cbind(b_call_vec, b_put_vec, b_0_vec, b_rho_vec, b_vol_vec))

# Combining basket prices varying by volatility
df_basket_prices <- rbind(df_basket_prices_vol_up, df_basket_prices_vol_down)

# Ordering columns to calculate finite differences
df_basket_prices <- df_basket_prices[order(
  df_basket_prices$b_rho_vec,
  df_basket_prices$b_0_vec,
  df_basket_prices$b_vol_vec),]

# Defining a dataframe to capture estimated basket Vega
df_basket_vega <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  b_0_vec = numeric(),
  b_rho_vec = numeric(),
  b_vol_vec = numeric(),
  b_call_vega = numeric(),
  b_put_vega = numeric())

# Iterating over correlation values
for (rho in rho_partition){
  # Estimating basket Vega using centered finite differences (higher/lower volatility)
  df_options_temp <- df_basket_prices[df_basket_prices$b_rho_vec == rho,]
  df_options_temp <- df_options_temp %>%
    mutate(b_call_vega = (lead(b_call_vec) - b_call_vec) / (lead(b_vol_vec) - b_vol_vec),
           b_put_vega = (lead(b_put_vec) - b_put_vec) / (lead(b_vol_vec) - b_vol_vec))
  df_basket_vega <- rbind(df_basket_vega, df_options_temp)
}

# Re-indexing
rownames(df_basket_vega) <- NULL

# Removing every second row
df_basket_vega <- df_basket_vega[seq(1, nrow(df_basket_vega), 2),]

# SPREAD

# Defining empty vectors to capture simulation results
s_call_vec <- c()
s_put_vec <- c()
s_0_vec <- c()
s_rho_vec <- c()
s_vol_vec <- c()

# Iterating over correlation values
for (rho in rho_partition){
  # Defining asset price paths based on correlation and higher volatility
  W_NVDA_T <- W_1_T
  W_AMD_T <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol + vol_dif)^2 / 2) * T + (NVDA_vol + vol_dif) * W_NVDA_T)
  AMD_path <- exp((rf - (AMD_vol + vol_dif)^2 / 2) * T + (AMD_vol + vol_dif) * W_AMD_T)
  
  # Iterating over initial asset prices
  for (percentage_change in price_incremements){
    # Calculating initial asset prices
    spread_0 <- AMD_0 * (1 + percentage_change) - NVDA_0 * (1 - percentage_change)
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * (1 - percentage_change) * NVDA_path
    AMD_t <- AMD_0 * (1 + percentage_change) * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    # Calculating terminal spread volatility
    spread_vol <- sd(spread_t)
    
    # Discounting average payoffs at maturity
    spread_call <- exp(-rf * T) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put <- exp(-rf * T) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec <- c(s_put_vec, spread_put)
    s_0_vec <- c(s_0_vec, spread_0)
    s_rho_vec <- c(s_rho_vec, rho)
    s_vol_vec <- c(s_vol_vec, spread_vol)
  }
}

# Defining a dataframe containing spread prices (higher volatility)
df_spread_prices_vol_up <- data.frame(
  cbind(s_call_vec, s_put_vec, s_0_vec, s_rho_vec, s_vol_vec))

# Defining empty vectors to capture simulation results
s_call_vec <- c()
s_put_vec <- c()
s_0_vec <- c()
s_rho_vec <- c()
s_vol_vec <- c()

# Iterating over correlation values
for (rho in rho_partition){
  # Defining asset price paths based on correlation and lower volatility
  W_NVDA_T <- W_1_T
  W_AMD_T <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol - vol_dif)^2 / 2) * T + (NVDA_vol - vol_dif) * W_NVDA_T)
  AMD_path <- exp((rf - (AMD_vol - vol_dif)^2 / 2) * T + (AMD_vol - vol_dif) * W_AMD_T)
  
  # Iterating over initial asset prices
  for (percentage_change in price_incremements){
    # Calculating initial asset prices
    spread_0 <- AMD_0 * (1 + percentage_change) - NVDA_0 * (1 - percentage_change)
    
    # Simulating terminal asset prices
    NVDA_t <- NVDA_0 * (1 - percentage_change) * NVDA_path
    AMD_t <- AMD_0 * (1 + percentage_change) * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    # Calculating terminal spread volatility
    spread_vol <- sd(spread_t)
    
    # Discounting average payoffs at maturity
    spread_call <- exp(-rf * T) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put <- exp(-rf * T) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    # Capturing simulation results
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec <- c(s_put_vec, spread_put)
    s_0_vec <- c(s_0_vec, spread_0)
    s_rho_vec <- c(s_rho_vec, rho)
    s_vol_vec <- c(s_vol_vec, spread_vol)
  }
}

# Defining a dataframe containing spread prices (lower volatility)
df_spread_prices_vol_down <- data.frame(
  cbind(s_call_vec, s_put_vec, s_0_vec, s_rho_vec, s_vol_vec))

# Combining spread prices varying by volatility
df_spread_prices <- rbind(df_spread_prices_vol_up, df_spread_prices_vol_down)

# Ordering columns to calculate finite differences
df_spread_prices <- df_spread_prices[order(
  df_spread_prices$s_rho_vec,
  df_spread_prices$s_0_vec,
  df_spread_prices$s_vol_vec),]

# Defining a dataframe to capture estimated spread Vega
df_spread_vega <- data.frame(
  s_call_vec = numeric(),
  s_put_vec = numeric(),
  s_0_vec = numeric(),
  s_rho_vec = numeric(),
  s_vol_vec = numeric(),
  s_call_vega = numeric(),
  s_put_vega = numeric())

# Iterating over correlation values
for (rho in rho_partition){
  # Estimating spread Vega using centered finite differences (higher/lower volatility)
  df_options_temp <- df_spread_prices[df_spread_prices$s_rho_vec == rho,]
  df_options_temp <- df_options_temp %>%
    mutate(s_call_vega = (lead(s_call_vec) - s_call_vec) / (lead(s_vol_vec) - s_vol_vec),
           s_put_vega = (lead(s_put_vec) - s_put_vec) / (lead(s_vol_vec) - s_vol_vec))
  df_spread_vega <- rbind(df_spread_vega, df_options_temp)
}

# Re-indexing
rownames(df_spread_vega) <- NULL

# Removing every second row
df_spread_vega <- df_spread_vega[seq(1, nrow(df_spread_vega), 2),]

# Using linear interpolation to replace negative values
df_spread_vega <- df_spread_vega %>%
  mutate(
    s_call_vega = ifelse(
      s_call_vega < 0,
      (lag(s_call_vega) + lead(s_call_vega)) / 2,
      s_call_vega
    ),
    s_put_vega = ifelse(
      s_put_vega < 0,
      (lag(s_put_vega) + lead(s_put_vega)) / 2,
      s_put_vega
    )
  )

# Basket Call Vega Plot

b_call_vega_plot <- surface_3d_plot(
  df_basket_vega,
  "b_0_vec", "b_rho_vec", "b_call_vega",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Vega (ν)")

b_call_vega_plot <- b_call_vega_plot %>% layout(scene = list(
  xaxis = list(autorange = "reversed"),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -0.8, y = 2.2, z = 1))))

b_call_vega_plot

# Basket Put Vega Plot

b_put_vega_plot <- surface_3d_plot(
  df_basket_vega,
  "b_0_vec", "b_rho_vec", "b_put_vega",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Vega (ν)")

b_put_vega_plot <- b_put_vega_plot %>% layout(scene = list(
  xaxis = list(autorange = "reversed"),
  yaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -0.8, y = 2.2, z = 1))))

b_put_vega_plot

# Spread Call Vega Plot

s_call_vega_plot <- surface_3d_plot(
  df_spread_vega,
  "s_0_vec", "s_rho_vec", "s_call_vega",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Vega (ν)")

s_call_vega_plot <- s_call_vega_plot %>% layout(scene = list(
  xaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -0.4, y = 2.3, z = 0.7))))

s_call_vega_plot

# Spread Put Vega Plot

s_put_vega_plot <- surface_3d_plot(
  df_spread_vega,
  "s_0_vec", "s_rho_vec", "s_put_vega",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Vega (ν)")

s_put_vega_plot <- s_put_vega_plot %>% layout(scene = list(
  xaxis = list(autorange = "reversed"),
  camera = list(
    eye = list(x = -0.4, y = 2.3, z = 0.7))))

s_put_vega_plot
