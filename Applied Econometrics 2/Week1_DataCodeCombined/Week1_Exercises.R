# Week 1 Exercises -- BEEM012.

# Adapted from exercises and examples in Hanck, Arnold, Gerber, and Schmelzer (2020) 
# Introduction to Econometrics with R. Accessible here. This book is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. (HA)

### Section 1: Manipulating Time Series Data

### Section 1.1
# Install the following packages: 
# (Once you have installed these, you can comment out these lines)
#install.packages("readxl")
#install.packages("AER")
#install.packages("xts")
#install.packages("dynlm")

# Load useful libraries we will use for time series.
library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)

## Section 1.2: Load & Format Data

# Change this line to the path for your working directory on your computer:
setwd("~/Dropbox/ExeterTeaching/BEEM012J/RScripts/")

# load UK macroeconomic data
UK_gdp <- read_excel(path = "UK_GDPpc_Quarterly-Full.xls")

# Now, we want to reformat our data so that the "Date" column
# is loaded in as a recognized date. To understand the format string
# format = "%Y Q%q" load the raw xls file and look at how dates are stored
# in that file.
UK_gdp$Date <- as.yearqtr(UK_gdp$Date, format = "%Y Q%q")

# We're going to convert our data into an xts formatted object, so we can use
# time series tools easily:
GDP_full_xts <- xts(UK_gdp$GDPpc, UK_gdp$Date)

# Let's plot some of this data to get a sense of the patterns that exist:
plot(as.zoo(GDP_full_xts),col = "steelblue",
     lwd = 2,
     ylab = "Quarterly GDP pc",
     xlab = "Date",
     main = "U.K. Quarterly GDP/Head")

plot(as.zoo(log(GDP_full_xts)),col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Log U.K. Quarterly GDP/Head")

# We're also going to learn how to take subsets of our data. 
# It looks like we've got huge outliers in 2020-2021 (obviously!) so we are going to look at data up to 2019 instead.
# We can remove this outlier if it's convenient to do so like this when it is near the end of our time series,
# or we could include it and just take account of it when doing our analysis.
GDP_xts = GDP_full_xts["1960::2019"]

# Let's plot some of this data to get a sense of the patterns that exist:
plot(as.zoo(GDP_xts),col = "steelblue",
     lwd = 2,
     ylab = "Quarterly GDP pc",
     xlab = "Date",
     main = "U.K. Quarterly GDP/Head")

plot(as.zoo(log(GDP_xts)),col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Log U.K. Quarterly GDP/Head")

### Section 1.3: Lags and Differences 
# # Lags are very important for understanding time series.
# Uncomment the line below to explore the details of the lag() operator
# we use for time series data:
# ?lag

# To take the lag of a variable, we use the lag() operator, where the second 
# argument is the order of the lag:
GDP_xts_lag2 = lag(GDP_xts,2)
# Here we are taking the 2nd-order lag of Y_t. 
# When you look at the first few periods, what do you notice about missing values?
print(GDP_xts_lag2["1960::1961"])

# Let's plot these lags to make sure we know what is going on:
par(mar=c(4.5, 4.5, 8, 2), xpd=TRUE)
plot(as.zoo(GDP_xts["1990::2000"]), col = "steelblue",
     lwd = 2,
     ylab = "Quarterly GDP pc.",
     xlab = "Date",
     mgp = c(2.5, 1, 0),
     mar= c(3,3,3,3), 
     width=5,
     height=5,
     main = "U.K. Quarterly GDP/Head")
lines(as.zoo(GDP_xts_lag2["1990::2000"]),
      type = "l", lwd =2,
      col = "orange")
legend("top",
       c("GDP Growth", "GDP Growth, Lag 2"), 
       col = c("steelblue", "orange"), 
       lty = c(1, 1),
       inset=c(0,-0.4),
       cex = 0.8
)

# In addition to lags, we are also interested in differences:
GDP_diff_xts <- diff(GDP_xts)
# We can also take lags of differences:
GDP_diff_lag1_xts <- lag(GDP_diff_xts,1)

plot(as.zoo(GDP_diff_xts["1990::2000"]), col = "steelblue",
     lwd = 1,
     ylab = "Quarterly GDP pc.",
     xlab = "Date",
     main = "U.K. Quarterly GDP/Head")
lines(as.zoo(GDP_diff_lag1_xts),
      type = "l", lwd =1,
      col = "orange")
legend("top",
       c("GDP First Difference", "GDP First Difference, Lag 2"), 
       col = c("steelblue", "orange"), 
       lty = c(1, 1),
       inset=c(0,-0.4),
       cex = 0.8
)

# We might also want to think of the Growth rate, which is useful for analysis and interpretation.
GDPGrowth_xts <- xts(400 * log(GDP_xts/lag(GDP_xts)))

### Section 2.1: Autocovariance & Autocorrelation:
# let's take a subset of the data to work with and generate the lags of 
GDP_1990_2005 <- GDP_xts["1990::2005"]

GDPGR_1990_2005 <- GDPGrowth_xts["1990::2005"]
GDPGR_1990_2005_lag1 <- lag(GDPGR_1990_2005)

# We drop the periods where we cannot observe GDP growth lags.
# take a look at the dataframe with our computed growth rates and lags,
# Where do we have missing values?
print(GDPGR_1990_2005_lag1["1990::1991"])
# We can't compute the lag in the first period (1990 Q1), because we need 
# to observe the prior period.
# So, we fix this by dropping the first observation of each.
# Now we compute the sample covariance and variance:
autocov_1 = cov(GDPGR_1990_2005[-1], GDPGR_1990_2005_lag1[-1])
var_yt = var(GDPGR_1990_2005[-1])
# With these two values, use Equation 1 from the exercise handout to compute the 
# the sample estimate of first-order autocorrelation:
autocor_yt_lag1 = autocov_1 / var_yt
print(autocor_yt_lag1)

# Now, use the 'acf' function to compute the first four autocorrelations in one step:

# Compute these autocorrelations for 1965-1990:

# Compute these autocorrelations for 2005-2020:

### Section 3: Autoregressions

### Section 3.1: Data Setup
# Let's set up data and lags manually so we can run an autoregression.
# length of data set

N <-length(GDPGR_1990_2005)
# So to manually create lags, we drop the first observation of our Y_t,
# which has the effect of ''shifting'' all observations by one. 
# To ensure we keep the same number of observations, we drop the final
# observation of the time series to get our series of lags

## IF THIS SEEMS CONFUSING, DON'T WORRY! In practice you will always use 
# the lag() function to compute lags, but I thought it would be good to
# demonstrate how we can do this without any special functions.

# Recall: using the [-N] index removes the Nth item from the array
GDPGR_level <- as.numeric(GDPGR_1990_2005[-1])
# Now drop the last value to get a series of lags
GDPGR_lags <- as.numeric(GDPGR_1990_2005[-N])
# estimate the linear model
armod <- lm(GDPGR_level ~ GDPGR_lags)
coeftest(armod)

# We can use the lag() operator to do this in one step:
# This is the same as lm(Y_t ~ Y_(t-1))
GDPGR_AR1_lm <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1))
coeftest(GDPGR_AR1_lm)
summary(GDPGR_AR1_lm)$r.squared

# Using the lag notation above, compute the 2nd and 3rd order AR models:
# This is the same as lm(Y_t ~ Y_(t-1) + Y_(t-2)) which is equivalent
# to estimating Y_t = \beta_0 + \beta_1 Y_(t-1) + \beta_2 Y_(t-2)
GDPGR_AR2_lm <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1) + lag(GDPGR_1990_2005,2))
coeftest(GDPGR_AR2_lm)
summary(GDPGR_AR2_lm)$r.squared

GDPGR_AR2_lm <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1:2))
coeftest(GDPGR_AR2_lm)
summary(GDPGR_AR2_lm)$r.squared

# Now, we can compute this with a single step without even having to write out
# the individual lags using the ar.ols() module:
arpackage_model <- ar.ols(GDPGR_1990_2005, 
                          demean = FALSE,
                          aic = FALSE,
                          order.max = 3,
                          intercept = TRUE)
print(arpackage_model)
arpackage_model_coef = as.numeric(arpackage_model$ar[1])
arpackage_model_coef_se = as.numeric(arpackage_model$asy.se.coef$ar[2])
arpackage_model_coef_tstat = arpackage_model_coef / arpackage_model_coef_se
print(arpackage_model_coef_tstat)
# This package is a bit of a "black-box" because it automatically adjusts for 
# some more advanced settings we haven't used set, so we will continue manually
# setting up our AR regressions. You might find this more convenient to use in
# the future, however.

### Section 4: Forecasting & Forecast Error
# the terms in the square brackets in GDPGrowth_xts["2005"][4] are used to 
# select a particular observation - here we are choosing Q4 of 2005.
# Recall that with AR1 model the forecast of Y_t is:
# Y_t = \beta_0_AR1 + \beta_1_AR1 * Y_(t-1)
# Here, to forecast we want to predict the first observation outside of our data
# used to estimate the model, which ends in 2005, Q4. So here, T+1 = 2006 Q1,
# so we use the observed Y_T from 2005 Q4 and the estimated coefficients to 
# forecast:
beta_0_AR1 <- coef(GDPGR_AR1_lm)[1]
beta_1_AR1 <- coef(GDPGR_AR1_lm)[2]
forecasted_value_AR1 = beta_0_AR1 + (beta_1_AR1 %*% GDPGrowth_xts["2005"][4])
# Now, let's see how accurate this is:
forecast_error_AR1 = forecasted_value_AR1 - GDPGrowth_xts["2006"][1]
cat("Forecasted Value AR(1):", forecasted_value_AR1, "\n")
cat("Actual Value AR(1):", GDPGrowth_xts["2006"][1], "\n")
cat("Forecast Error AR(1):", forecast_error_AR1, "\n")

# With AR2, the forecast of Y_t is:


# With AR3, the forecast of Y_t is:


### 
GDPGrowth_xts <- xts(400 * log(GDP_xts/lag(GDP_xts)))
GDPGR_1990_2005 <- GDPGrowth_xts["1990::2005"]

# Now, add the unemployment rate:
UK_UnemploymentRate <- read_excel(path = "UK_UnemploymentRate.xls")
UK_UnemploymentRate$Date <- as.yearqtr(UK_UnemploymentRate$Date, format = "%Y Q%q")
UK_UnemploymentRate_xts <- xts(UK_UnemploymentRate$UnemploymentRate, UK_UnemploymentRate$Date)
UK_UnemploymentRate_1990_2005 <- UK_UnemploymentRate_xts["1990::2005"]
UK_UnemploymentRate_diff = diff(UK_UnemploymentRate_1990_2005)

# Take the code above to estimate the ADL11 model:
GDPGR_ADL11_lm_unemp <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1) + lag(UK_UnemploymentRate_diff,1))
coeftest(GDPGR_ADL11_lm_unemp)
summary(GDPGR_ADL11_lm_unemp)$r.squared

# Take the code above to estimate the ADL22 model:
GDPGR_ADL22_lm_unemp <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1) + lag(GDPGR_1990_2005,2) + lag(UK_UnemploymentRate_diff,1) + lag(UK_UnemploymentRate_diff,2))
coeftest(GDPGR_ADL22_lm_unemp)
summary(GDPGR_ADL22_lm_unemp)$r.squared

