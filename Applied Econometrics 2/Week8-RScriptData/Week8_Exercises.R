# Week 8 Exercises -- BEEM012.

# Adapted from exercises and examples in Hanck, Arnold, Gerber, and Schmelzer (2020) 
# Introduction to Econometrics with R. Accessible here. This book is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. (HA)


### Section 1.1 Set-Up and Loading Data
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
library(urca)

# Change this line to the path for your working directory on your computer:
setwd("~/Dropbox/ExeterTeaching/RScripts/")

# Load Additional Macroeconomic Data:
# Source: Bank of England (http://www.bankofengland.co.uk/boeapps/iadb/NewInterMed.asp)
UK_TermYields <- read_excel(path = "UK_yield_3mo_5yr_10yr.xls")
UK_TermYields$Date <- as.yearqtr(UK_TermYields$Date, format = "Q%q %Y")

UK_Yield3mo <- xts(UK_TermYields$Y3mo, UK_TermYields$Date)["1982::2007"]
UK_Yield10yr <- xts(UK_TermYields$Y10yr, UK_TermYields$Date)["1982::2007"]

UK_TermSpread_10yr_3mo <- UK_Yield10yr - UK_Yield3mo

### Plot:
# plot both interest series
plot(as.zoo(UK_Yield10yr),
     plot.type = "single",
     lty = 2,
     lwd = 2,
     col = "blue",
     xlab = "Date",
     ylab = "Percent per annum",
     ylim = c(-5, 17),
     main = "Interest Rates and Term Spread")
lines(as.zoo(UK_Yield3mo),
      col = "orange",
      lwd = 2,
      xlab = "Date",
      ylab = "Percent per annum",
      main = "Term Spread")
# add the term spread series:
lines(as.zoo(UK_TermSpread_10yr_3mo),
      col = "purple",
      lwd = 2,
      xlab = "Date",
      ylab = "Percent per annum",
      main = "Term Spread")
legend("topright",
       lty = c(2, 1, 1),
       lwd = c(2, 2, 2),
       cex = 0.8,
       col = c("blue", "orange", "purple"),
       legend = c("10 Year Bond Yield", "3 Month Bond Yield", "Term Spread"))

## Section 1.1.1: Standard Dickey-Fuller

# test for nonstationarity of 3-month treasury bills using ADF test with one lag
# and allowing for a time trend, as there appears to be one in the data:
ADFtest_UnitRoot_3mo <- ur.df(UK_Yield3mo,
      lags = 1,
      selectlags = "Fixed",
      type = "trend")
summary(ADFtest_UnitRoot_3mo)

### Section 1.1.2: Manual GLS Dickey-Fuller Test

# Now, we're moving on to the improved GLS-ADF test:
# Manual GLS-ADF test. The first thing we do is set up all of our terms for the 
#   GLS first stage where we de-mean and de-trend the three month interest rate
#   time series.
P = length(UK_Yield3mo)
a_star = "WRITE IN THE FORMULA FOR a_star USING P in the place of T"
V_1 = "ADD THE TERM TO COMPLETE THE FORMULA FOR V_1" - (a_star*lag(UK_Yield3mo))
V_1[1] = UK_Yield3mo[1]

X_1 = array(1-a_star,dim = c(P))
X_1[1] = 1

t = seq(1,P,1)
X_2 = t - (a_star*"ADD THE TERM TO COMPLETE THE FORMULA FOR X_2")
X_2[1] = 1

#GLS-ADF first-stage:
GLS_DF_FirstStage <- lm(V_1 ~ X_1 + X_2 - 1)
coeftest(GLS_DF_FirstStage)
# Store the required estimated parameters:
delta0_hat = GLS_DF_FirstStage$coefficients[1]
delta1_hat = GLS_DF_FirstStage$coefficients[2]

# Now, we use these estimated parameters to de-mean and de-trend the 3 month interest
#   rate time series:
Y_DemeanDetrend = "USE UK_Yield3mo AND THE ESTIMATED delta0_hat AND delta1_hat TO COMPUTE Y_DemeanDetrend"

#Now, we're going to plot the original data and the de-trended data to see how it looks.
#   Does this help explain the intuition behind the "filtering" of the time series?
plot(as.zoo(UK_Yield3mo),
     lty = c(2, 1),
     lwd = 2,
     xlab = "Date",
     col = "red",
     ylab = "Percent per annum",
     ylim = c(-5, 17),
     main = "Interest Rates")
lines(as.zoo(Y_DemeanDetrend),
      col = "purple",
      lwd = 2,
      xlab = "Date",
      ylab = "Percent per annum",
      main = "Term Spread")
legend("topright",
       lty = c(2, 1),
       lwd = c(2, 2),
       cex = 0.8,
       col = c("red", "purple"),
       legend = c("Original Time Series", "De-Trended, De-Meaned, Time Series"))

# Now, we use the de-meaned and de-trended data for a GLS-DF test:
Manual_GLS_DFtest_3mo <- ur.df(Y_DemeanDetrend,
                              lags = 1,
                              selectlags = "Fixed",
                              type = "none")
summary(Manual_GLS_DFtest_3mo)

## SECTION 1.1.3: Automatic GLS Dickey-Fuller Test
# test for nonstationarity of 3-month treasury bills using the automatic DF-GLS test
GLS_DFtest_UnitRoot_3mo <- ur.ers(UK_Yield3mo,
                           lag.max = 1,
                           type = "DF-GLS",
                           model = "trend")
summary(GLS_DFtest_UnitRoot_3mo)

GLS_DFtest_UnitRoot_10yr <- ur.ers(UK_Yield10yr,
                            lag.max = 1,
                            type = "DF-GLS", 
                            model="trend")
summary(GLS_DFtest_UnitRoot_10yr)

#### SECTION 1.2: Testing for Cointegration

### Section 1.2.1: Known theta.

# Theory suggests theta = 1, so we just use the term spread as our residual
# z = UK_TermSpread_10yr_3mo
ADFtest_TermSpread_10yr_3mo <- ur.df(UK_TermSpread_10yr_3mo,
                               lags = 1,
                               selectlags = "AIC",
                               type = "none")
summary(ADFtest_TermSpread_10yr_3mo)

# GLS_DFtest_TermSpread_10yr_3mo <- ur.ers(UK_TermSpread_10yr_3mo,
#                                    lag.max = 1, type="DF-GLS",
#                                    model = "constant")
# summary(GLS_DFtest_TermSpread_10yr_3mo)

### Section 1.2.2: Unknown theta.
# Now, let's try the case where we didn't know theta:

# First step: estimate theta
CointegrationFirstStep_lm <- lm("FILL IN THE REGRESSION FORMULA TO ESTIMATE THETA")
coeftest(CointegrationFirstStep_lm)
theta_hat = CointegrationFirstStep_lm$coefficients[2]

# Now use this estimated theta_hat to generate z_hat:
z_hat = "FILL IN THE FORMULA TO COMPUTE z_hat USING UK_Yield3mo AS Y_t, UK_Yield10yr AS X_t AND theta_hat"

# Now, run the ADF test on z_hat, to test for stationarity:
ADFtest_z_hat <- ur.df(z_hat, lags = 1, selectlags = "BIC", type = "none")
summary(ADFtest_z_hat)

# GLS_DFtest_z_hat <- ur.ers(z_hat, lag.max = 1, type="DF-GLS", model = "constant")
# summary(GLS_DFtest_z_hat)
  
  
  
  