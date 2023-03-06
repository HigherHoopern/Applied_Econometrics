# Week 5 Exercises -- BEEM012.

# Adapted from exercises and examples in Hanck, Arnold, Gerber, and Schmelzer (2020) 
# Introduction to Econometrics with R.  
# This book is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. (HA)


### Section 1.1 Set-Up and Loading Data
# Install the following packages: 
# (Once you have installed these, you can comment out these lines)
#install.packages("readxl")
#install.packages("AER")
#install.packages("xts")
#install.packages("dynlm")

# We will also be using the "orcutt" package to automatically
# run the iterated Cochrane-Orcutt estimator, so uncomment this line
# to install the first time you run:
#install.packages("orcutt")

# Load useful libraries we will use for time series.
library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)
library(urca)
library(orcutt)


### Section 2: HAC Errors

#### ===========================================================================
# Section 2.1: Set-Up 
# As before, I am clearly separating out the set-up section 
# where I simulate time series, etc. This is good to understand, but less 
# important than the analysis section that comes below.

# First, we will simulate a time series with serially correlated errors:
set.seed(250) ## set seed to replicate randomization

N <- 100
y_AutoCorrelatedErrors <- array(c(0),dim = c(N)) # Set up a vector where we store our simulated Y
# Here we use the arima package to simulate our errors, since this is not as simple as drawing from a random normal.
# Here the  model = list(ar = 0.5) sets up the parameters for the error correlation we want to simulate
# Here, the key part is "ar = 0.5" which means we want autocorrelated errors where the 
#   autocorrelation coefficient is 0.5. This is similar to setting phi1 = 0.5 in
#   the model we use when we set up our GLS quasi-differenced variables.
u <- arima.sim(n = N , model = list(ar = 0.5)) 
d <- as.Date(1:N)

# The beta1 for this time series (the coefficient on X_t) is 0.5
# In order to usethe simple formula we derived in lecture we will just be using
# a Distributed Lag model with the contemporaneous value of X_t:
# Y_t = \beta_0 + \beta_1 X_t + u_t
beta1 <- 0.5

# Here we simulate our X's: they are drawn randomly from a uniform distribution
# from 1 to 10.
X <- runif(N, 1, 10)

# Now we simulate our time series: at each time period, we set Y_t = beta1 X_t + u_t
# where u_t comes from the vector of autocorrelated errors we simulated above.
for (t in 1:N) {
  y_AutoCorrelatedErrors[t] = (beta1 * X[t]) + u[t]
}

# Now we convert this into a time series using the vector d of simulated dates
y_AutoCorrelatedErrors_xts <- xts(y_AutoCorrelatedErrors, d)
# We also set up a time series of X_t:
X_xts <- xts(X,d)

#### ===========================================================================

# Section 2.2: Computing HAC Errors Manually
# First, we estimate our distributed lag model as is:
### Use the variables y_AutoCorrelatedErrors_xts and X_xts to set up our DL1 regression below
DL1_lm <- lm("SET UP DL1 REGRESSION MODEL HERE IN THE BRACKETS")

# Now we need to turn our residuals into a time series so we can measure the 
# autocorrelation:
OLS_u_hat <- DL1_lm$residuals
# We will also compute the v term we used in lecture slides:
v <- (X - mean(X)) * OLS_u_hat

# Let's look at these residuals - do they look autocorrelated?
plot(as.zoo(OLS_u_hat), main ="DL1 Model Residuals", ylab="u_t", xlab="t")
# Now let's directly plot the autocorrelations - each item on the X-axis
# records the lag length and the Y-axis tells us the autocorrelation at that lag:
acf(OLS_u_hat, main="Residual Autocorrelations")
# As you can see the autocorrelation at lag=1 is 0.5, just as we set up when
# we simulated the u_t

X_bar = mean(X)
# This isn't exactly the formula for var(beta1) we use in lecture slides:
# it's a heteroskedasticity-robust variance estimator:
var_beta_hat <- 1/N * (1/(N-2) * sum((X - mean(X))^2 * OLS_u_hat^2) ) /
  (1/N * sum((X - mean(X))^2))^2

# We also compute the autocorrelations of v_t to use in computing f_T_hat:
rho_list <- acf(v, plot=FALSE)$acf[-1]

# rule of thumb truncation parameter
m <- "INSERT FORMULA FOR TRUNCATION PARAMETER HERE" #### write in the formula from lectures/homework instructions

# This sum is a bit confusing, so let's break it down.
# First, we focus on the sum() term -- the first part of 1 + (2* sum())
# should be clearly identifiable in the function
# We use the sum() function, which just takes the sum of a list of outcomes.
# We use sapply() to set up our usual sum over values of a function as in lecture notes.
# The first term of sapply() is the limits on which we want to apply the function,
#   so here we use sapply(1:(m-1), ) to set up a sum over j=1 to j=m-1
#   next we set up the "inside" of this sum, the function of j we want to compute at each value of j
#   that's why the second term of sapply() is function(j) ((m-j)/m)*rho_j_list[j]
#   We tell R that this is a function over j, and we want to compute ((m-j)/m)*rho_list[j]
f_hat_T = 1 + (2 * sum(sapply(1:(m-1), function(j) ((m-j)/m)*rho_list[j])))

# unadjusted standard error. If var_beta_hat is the variance of beta_1, what is the 
#     standard error of beta_1? Recall the sqrt() function takes a square root.
unadjusted_StdErr <- sqrt(var_beta_hat)
cat("Unadjusted Standard Error: ", unadjusted_StdErr, "\n")

# compute Newey-West HAC estimate of the standard error
# Now we have the unadjusted standard error var_beta_hat and we have estimated f_hat_T, 
#     how do we get our adjusted NeweyWest standard error?
NeweyWest_manual_StdErr <- " INSERT THE FORMULA FOR THE Newey-West Stsndard Error" ### Use the quantities estimated above, fill in the formula for the N-W standard error.
cat("Manual Newey-West Standard Error: ", NeweyWest_manual_StdErr, "\n")

# Using the automatic NeweyWest() function:
NW_VCOV <- NeweyWest(lm(y_AutoCorrelatedErrors_xts ~ X),
                     lag = m - 1, prewhite = F,
                     adjust = T)
# compute standard error
NeweyWest_auto_StdErr <- sqrt(diag(NW_VCOV))[2]
cat("Automatic Newey-West Standard Error: ", NeweyWest_auto_StdErr, "\n")

### SECTION 3 -- Dynamic Causal Effects w. Strict Exogeneity

# Now we are going to look at the two ways of estimating a model where we can
# assume strict exogeneity.
# Our starting point for this is a DL2 model, with X_t and X_(t-1) as regressors

### ============================================================================
# This is a set-up section: as usual, it is good to try and understand, but your
# priority should be the analysis section:
N <- 500
y_AutoCorrelatedErrors_DL2 <- array(c(0),dim = c(N))
d <- as.Date(1:c(N-1))

# As before, we simulate X from a normal distribution, 
# and u with an arima distribution with phi1 = 0.5
X <- runif(N, 1, 10)
u <- arima.sim(n = N , model = list(ar = 0.5))

# Our dynamic multipliers here are 0.1 and 0.25.
beta1 <- 0.1
beta2 <- 0.25

y_AutoCorrelatedErrors_DL2[1] = (beta1 * X[1]) + u[1]
for (t in 2:N) {
  y_AutoCorrelatedErrors_DL2[t] = (beta1 * X[t]) + (beta2 * X[t-1]) + u[t]
}
# We skip the first period (since we don't have the lagged values)
# and set up xts objects with our simulated time series and regressor:
y_AutoCorrelatedErrors_DL2_xts <- xts(y_AutoCorrelatedErrors_DL2[-1], d)
X_xts <- xts(X[-1],d)

### ============================================================================

### Section 3.2: Analysis of Model Residuals
# Now, on to the analysis section.
# First, let's look at our residuals from a simple estimation of this DL2 model
# has serially correlated errors:
DL2_lm <- lm(y_AutoCorrelatedErrors_DL2_xts ~ X_xts + lag(X_xts,1))
OLS_u_hat <- DL2_lm$residuals
acf(OLS_u_hat)

### Section 3.3: ADL without Autocorrelated Errors
# Now, we estimate the ADL(2,1) representation of the distributed lag model
### USING y_AutoCorrelatedErrors_DL2_xts and X_xts, write in the proper regression
###   model for the ADL21 model without autocorrelation
adl21_lm <- lm(y_AutoCorrelatedErrors_DL2_xts ~ "INSERT MISSING REGRESSORS HERE FOR THE ADL21 WITHOUT AUTOCORRELATION")
# plot the sample autocorrelations of residuals from this transformed model
#   do they appear to be autocorrelated?
acf(adl21_lm$residuals, main = "ADL21: Autocorrelations")

# We now recover the estimated coefficients we need to compute the first two dynamic multipliers
delta_0_hat <- adl21_lm$coefficients[3]
delta_1_hat <- adl21_lm$coefficients[4]
phi_1_hat <- adl21_lm$coefficients[2]

# Using the formula on slide 15 of Lecture 12 or in the Homework Instructions,
#   compute the first two dynamic multipliers
c("hat_beta_1" = "USE THE FORMULA FROM LECTURE AND THE HOMEWORK INSTRUCTIONS FOR BETA 1", ### BE CAREFUL TO KEEP THE COMMA IN THE CORRECT SPOT!
  "hat_beta_2" = "USE THE FORMULA FROM LECTURE AND THE HOMEWORK INSTRUCTIONS FOR BETA 2")

### feasible GLS:
# For the feasible GLS, estimate phi_1.
# First, we estimate the normal DL2 model and un the regression on residuals
# to estimate phi_1
DL2_lm <- lm(y_AutoCorrelatedErrors_DL2_xts ~ X_xts + lag(X_xts,1))
# Store residuals:
DL2_residuals <- DL2_lm$residuals
# Regress residuals on lag of residuals:
residuals_lm <- lm("WRITE IN THE APPROPRIATE REGRESSION ON RESIDUALS TO ESTIMATE phi_1")
# Store the coefficient on the lag of residuals -- this is our estimated phi_1
coeftest(residuals_lm)
phi1_hat <- residuals_lm$coefficients[2]
cat("Phi_1_hat from manual Cochrane-Orcutt: ", phi1_hat, "\n")

# Now, we generate our quasi-diff variables using the estimated phi1:
Y_tilde_xts <- y_AutoCorrelatedErrors_DL2_xts - (phi1_hat* lag(y_AutoCorrelatedErrors_DL2_xts))
X_tilde_xts <- X_xts - (phi1_hat* lag(X_xts))
Xlag1_tilde_xts <- lag(X_xts) - (phi1_hat* lag(X_xts,2))

# Using these quasi-differenced variables, we run the GLS model:
fGLS_DL_lm <- lm("USE THE TERMS ABOVE (Y_tilde_xts, X_tilde_xts,Xlag1_tilde_xts) TO SET UP GLS REGRESSION")
# we can recover the estimates from this, which are our dynamic multipliers:
coeftest(fGLS_DL_lm)
acf(fGLS_DL_lm$residuals, main = "GLS Residuals: Autocorrelations")

# We can compare this to the results from the iterated cochrane-orcutt method:
# Unfortunately, the cochrane.orcutt model with lm() doesn't work with the 
#   lag operator, so we need to set this up manually.
y_cochrane = c(y_AutoCorrelatedErrors_DL2[-1])
x_cochrane = c(X[-1])
xl1_cochrane = c(X[-N])
iterated_cochrane_orcutt_model <- cochrane.orcutt(lm(y_cochrane ~ x_cochrane + xl1_cochrane))
summary(iterated_cochrane_orcutt_model)
cat("Phi_1_hat from Iterated Cochrane-Orcutt: ", iterated_cochrane_orcutt_model$rho, "\n")

