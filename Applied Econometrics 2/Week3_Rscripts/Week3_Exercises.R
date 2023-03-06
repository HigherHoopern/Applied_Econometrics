# Week 2 Exercises -- BEEM012.

# Adapted from exercises and examples in Hanck, Arnold, Gerber, and Schmelzer (2020) 
# Introduction to Econometrics with R. This book is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. (HA)

# Load useful libraries we will use for time series.
library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)

#install.packages("urca")
library(urca)

### Section 1: Testing for a Unit Root Process

#===============================================================================
# SET-UP SECTION
# This section between the ====== lines is set-up, where we simulate some
# time series we use below so we can see how the test for a unit root process works.
# If you want to look through this section and see how it works, great!
# If not, you can skip this and focus on the section below where we do the test 
# and analyze results.

# First, we set up a unit root process with beta_1 = 1.

num_iterations <- c(1000) # Here we are simulating 50 time series
num_periods <- c(200)

random_walk <- array(c(0),dim = c(num_iterations,num_periods))
beta_1_hat <- array(c(0),dim = c(num_iterations))
d <- as.Date(1:num_periods)
DickeyFuller_teststats <- array(c(0),dim = c(num_iterations)) # We are also setting up an empty vector to record the t-statistics for beta_1_hat

set.seed(250) ## set seed to replicate randomization

# This first time series we simulate is a unit root, with \beta_1 = 1.

for (i in 1:num_iterations) {   
  u <- rnorm(num_periods)   
  random_walk[i,1] = u[1]
  for (t in 2:num_periods) {
    random_walk[i,t] = (1 * random_walk[i,t-1]) + u[t]
  }
  y_unit_root = random_walk[i,]
  y_unit_root_xts <- xts(y_unit_root, d)
  AR1_DickeyFuller_lm <- lm(diff(y_unit_root_xts) ~ lag(y_unit_root_xts,1))
  DickeyFuller_teststats[i] = coeftest(AR1_DickeyFuller_lm)[2,3]
}

# Now, for comparison, we simulate time series where beta = 0.25:

stationary_process <- array(c(0),dim = c(num_iterations,num_periods))
d <- as.Date(1:num_periods)
DickeyFuller_teststats_Stationary <- array(c(0),dim = c(num_iterations)) # We are also setting up an empty vector to record the t-statistics for beta_1_hat

for (i in 1:num_iterations) {   
  u <- rnorm(num_periods)   
  stationary_process[i,1] = u[1]
  for (t in 2:num_periods) {
    stationary_process[i,t] = (0.5 * stationary_process[i,t-1]) + u[t]
  }
  
  y_stationary = stationary_process[i,]
  y_stationary_xts <- xts(y_stationary, d)
  AR1_DickeyFuller_lm <- lm(diff(y_stationary_xts) ~ lag(y_stationary_xts,1))
  DickeyFuller_teststats_Stationary[i] = coeftest(AR1_DickeyFuller_lm)[2,3]
}

#### Now, we will simulate an AR2 time series so we can use an Augmented Dickey Fuller Test.
y_unit_root_AR2 <- array(c(0),dim = c(num_periods))

set.seed(250) ## set seed to replicate randomization
# This second time series will be an AR2 unit root process
# so \beta_1 + \beta_2 = 1 for a unit root.
# Let's therefore set beta_1 = 0.7 and beta_2 = 0.3
beta1 <- 0.7
beta2 <- 0.3

u <- rnorm(num_periods)   
y_unit_root_AR2[1] = u[1]
y_unit_root_AR2[2] = (beta1 * y_unit_root_AR2[1]) + u[2]
for (t in 3:num_periods) {
  y_unit_root_AR2[t] = (beta1 * y_unit_root_AR2[t-1]) + (beta2 * y_unit_root_AR2[t-2]) + u[t]
}
y_unit_root_AR2
y_unit_root_AR2_xts <- xts(y_unit_root_AR2, d)

#### Now, we will simulate an AR2 time series WITH a Time Trend so we can use
# an Augmented Dickey Fuller Test of stationarity AROUND a trend
y_AR2_TrendStationary <- array(c(0),dim = c(num_periods))

set.seed(150) ## set seed to replicate randomization
# This second time series will be an AR2 unit root process
# so \beta_1 + \beta_2 = 1 for a unit root.
# Let's therefore set beta_1 = 0.7 and beta_2 = 0.3
alpha <- 0.5
beta1 <- 0.3
beta2 <- 0.1

u <- rnorm(num_periods)   
y_AR2_TrendStationary[1] = alpha + u[1]
y_AR2_TrendStationary[2] = (alpha*2) + (beta1 * y_AR2_TrendStationary[1]) + u[2]
for (t in 3:num_periods) {
  y_AR2_TrendStationary[t] = (alpha*t) + (beta1 * y_AR2_TrendStationary[t-1]) + (beta2 * y_AR2_TrendStationary[t-2]) + u[t]
}
y_AR2_TrendStationary_xts <- xts(y_AR2_TrendStationary, d)

# Let's take the first of our iterations as an example:
y_stationary = stationary_process[1,]
y_stationary_xts <- xts(y_stationary, d)
y_unit_root = random_walk[1,]
y_unit_root_xts <- xts(y_unit_root, d)
#===============================================================================

####### Section 1.2: Plotting Time Series
### To show that the Dickey-Fuller Test Statistics are not normal:

t_stdnormal <- rnorm(1000) # This draw of 1000 outcomes from a standard normal will be compared to the distribution of t-statistics

par(mar=c(4.5, 4.5, 8, 2), xpd=TRUE)
plot(density(DickeyFuller_teststats), 
     col = "purple",      
     xlim = c(-3,3),
     lwd = 3,
     main = "Dickey-Fuller T-Stat",
     xlab = "Estimates")
lines(density(t_stdnormal), 
      col = "steelblue",    
      xlim = c(-3,3),      
      lwd = 3)
legend("top",   
       c("T-Stats", "Std. Normal"), 
       col = c("purple", "steelblue"), 
       lty = c(1, 1),
       inset=c(0,-0.5),
       cex = 0.8
)

par(mar=c(4.5, 4.5, 8, 2), xpd=TRUE)
plot(as.zoo(y_unit_root_xts), 
     col = "purple",      
     lwd = 2,
     main = "Stationary & Unit Root",
     xlab = "Estimates")
lines(as.zoo(y_stationary_xts), 
      col = "steelblue",    
      lwd = 2)
legend("top",   
       c("Unit Root Process (beta_1 = 1)", "Stationary Process (beta_1 = 0.5)"), 
       col = c("purple", "steelblue"), 
       lty = c(1, 1),
       inset=c(0,-0.5),
       cex = 0.8
)

par(mar=c(4.5, 4.5, 8, 2), xpd=TRUE)
plot(as.zoo(y_unit_root_xts), 
     col = "purple",      
     lwd = 2,
     main = "Unit Root & First Difference",
     xlab = "Estimates")
lines(as.zoo(diff(y_unit_root_xts)), 
      col = "orange",    
      lwd = 2)
legend("top",   
       c("Unit Root Process (beta_1 = 1)", "First Difference of Unit Root"), 
       col = c("purple", "orange"), 
       lty = c(1, 1),
       inset=c(0,-0.5),
       cex = 0.8
)
#-------------------------------------------------------------------------------

####### Section 1.3: Implementing the Dickey-Fuller Test

## Section 1.3.1: Dickey-Fuller Test for AR(1) Time Series Data
# In the section above, we have set up our unit root process: y_unit_root_xts,
#     where we know that beta_1 = 1.
#     (in other words, it is a random walk)

# We will now run the ADF test and see how it works:
# We run the exact linear model that is in the Homework Handout:
AR1_DickeyFuller_lm <- lm(diff(y_unit_root_xts) ~ lag(y_unit_root_xts,1)) # This line of code implements a linear model representing the Dickey-Fuller Test
coeftest(AR1_DickeyFuller_lm)     # This line prints the coefficients and their test statistics
# Recall that the Dickey-Fuller test statistic is the t-statistic on the lagged term in the model above:
DickeyFuller_teststat_manual = coeftest(AR1_DickeyFuller_lm)[2,3]

# Now, let's compare this to the output when we use the R function that directly
#   runs the Unit Root test for us. This uses the df (for Dickey-Fuller) command from the 
#   ur package (referring to Unit Root). 
#   Here we are testing the y_unit_root_xts time series, so this is the first argument of the ur.df() command
#   Here we are implementing the model from our lecture notes with 
#   no additional lags, and with an intercept, but no time trend,
#   so we set lags=0 (since we don't include any additional lags for the ordinary
#   Dickey-Fuller test) and with type='drift' to indicate we want to use the critical values for
#   the model with an intercept only.
unit_root_test = ur.df(y_unit_root_xts, lags=0, type='drift')
# These two commands retrieve the parameters of interest we care about:
DickeyFuller_teststat_automatic = unit_root_test@teststat[1]
DickeyFuller_CriticalValues = unit_root_test@cval[1,]
# If you want to see the full output from this function, run summary(unit_root_test).
# This includes more detail than we need right now though

# Now, let's look at what we find:
print(c("Manually Computed Dickey Fuller Test Statistic:", round(DickeyFuller_teststat_manual,2)))
print(c("Automatic Dickey Fuller Test Statistic:", round(DickeyFuller_teststat_automatic,2)))
# The test-statistic we calculate from our manually set-up lm() is the exact same
# as the one from the ur.df() function!
# Now, let's look at the Critical Values from the ur.df() function. 
# As we showed above, the t-statistics are not normal, so we need to use special
# Critical Values to test:
print("Dickey-Fuller Intercept-Only Critical Values:")
print(DickeyFuller_CriticalValues)
# As you can see, the Dickey-Fuller test statistic for our unit root process
# does NOT reject the null, so we cannot conclude it is stationary.

# Now, let's repeat the same for a Stationary time series, and see what happens!
# Take the exact code from above, but replace the y_unit_root_xts with 
# our xts object for a stationary time series: y_stationary_xts
# Stationary Time Series:
Stationary_DickeyFuller_lm <- lm("INSERT MODEL HERE") ### Adapt the code from the AR1_DickeyFuller_lm command on line 185.
coeftest(Stationary_DickeyFuller_lm) ### Look at this coefficient table, and identify which is the appropriate Dickey-Fuller test statistic.
### Use the coordinates of the correct coefficient and use these to update the line below where it says "INSERT ROW HERE"
### and "INSERT COLUMN HERE" below.
DF_stationary_teststat_manual = coeftest(Stationary_DickeyFuller_lm)["INSERT ROW HERE","INSERT COLUMN HERE"]

unit_root_test_Stationary = ur.df(y_stationary_xts, lags=0, type='drift')
DF_stationary_teststat_automatic = unit_root_test_Stationary@teststat[1]
DF_stationary_CriticalValues = unit_root_test_Stationary@cval[1,]

# Now, let's look at what we find:
print(c("Manual Dickey Fuller Test Statistic for Stationary Series:", round(DF_stationary_teststat_manual,2)))
print(c("Automatic Dickey Fuller Test Statistic for Stationary Series:", round(DF_stationary_teststat_automatic,2)))
# Now, let's look at the Critical Values from the ur.df() function. 
# As we showed above, the t-statistics are not normal, so we need to use special
# Critical Values to test:
print("Dickey-Fuller Intercept-Only Critical Values:")
print(DF_stationary_CriticalValues)
# Can we reject the null hypothesis? What can you conclude from this?

## Section 1.3.2: ADF Test for AR(2)
# Now, let's use the AR2 Unit Root process we simulated above: y_unit_root_AR2_xts
#     and let's run an ADF test
#     Recall from the notes or Homework Handout that for an ADF test with p=2, we 
#     include two lags of the difference.
#     We take the lag of a difference just like any other lag, but putting the
#     diff() function inside a lag() function: so the order-p lag of the diff is lag(diff(y_unit_root_AR2_xts),p)
#     Add the required lagged difference terms to the appropriate linear model to run an ADF test on this AR2 process.
#     Replace " REPLACE NECESSARY TERMS HERE " with the terms required to make this an appropriate ADF test model.
UnitRootAR2_DickeyFuller_lm <- lm(diff(y_unit_root_AR2_xts) ~ lag(y_unit_root_AR2_xts,1) + " REPLACE NECESSARY TERMS HERE ")
ADF_AR2_teststat_manual = coeftest(UnitRootAR2_DickeyFuller_lm)[2,3]

# Let's check we get the same outcome when we use the automatic Unit Root Test:
#   We are using a model with an intercept, so type='drift' again.
#   We are using a model with two lags in this Augmented Dickey Fuller test, so lags=2
unit_root_test_AR2 = ur.df(y_unit_root_AR2_xts, lags=2, type='drift')
ADF_AR2_teststat_automatic = unit_root_test_AR2@teststat[1]
ADF_AR2_CriticalValues = unit_root_test_AR2@cval[1,]

# Now, let's look at what we find:
print(c("Manual Augmented Dickey Fuller Test Statistic:", round(ADF_AR2_teststat_manual,2)))
print(c("Automatic Augmented Dickey Fuller Test Statistic:", round(ADF_AR2_teststat_automatic,2)))
# Now, let's look at the Critical Values from the ur.df() function. 
# As we showed above, the t-statistics are not normal, so we need to use special
# Critical Values to test:
print("Dickey-Fuller Intercept-Only Critical Values:")
print(ADF_AR2_CriticalValues)
# Can we reject the null hypothesis? What can you conclude from this?

## Section 1.3.3: Testing for Stationarity Around Trend:
#   Now, we are going to use the time series we simulated above which is stationary around a deterministic time trend
#   Recall, we simulated a stationary AR2, with an added time trend.
#   For this, we use the version of the ADF test that includes a time trend
#   In R, we can add a time trend by including a term that reflects the time period of a given observation
#   This is written as time(y_AR2_TrendStationary_xts), and is equivalent to including t in our model as a regressor
TrendStationaryAR2_Trend_lm <- lm(diff(y_AR2_TrendStationary_xts) ~ time(y_AR2_TrendStationary_xts) + lag(y_AR2_TrendStationary_xts,1) + lag(diff(y_AR2_TrendStationary_xts),1) +  lag(diff(y_AR2_TrendStationary_xts),2))
coeftest(TrendStationaryAR2_Trend_lm)
ADF_TrendStationaryAR2_manual = coeftest(TrendStationaryAR2_Trend_lm)[3,3]

# Let's check we get the same test statistic when we use the automatic Unit Root Test:
#   We are using a model with an intercept, so type='trend' again.
#   We are using a model with two lags in this Augmented Dickey Fuller test, so lags=2
#   Remember, we are now testing the y_AR2_TrendStationary_xts time series, so we update the first argument of the ur.df() function
unit_root_test_TrendStationaryAR2 = ur.df(y_AR2_TrendStationary_xts, lags=2, type='trend')
unit_root_test_TrendStationaryAR2@teststat[1]
unit_root_test_TrendStationaryAR2@cval[1,]

# Try now, without a trend. To do this, take the linear model from above, and remove the time()
#     term, which indicated we include a time trend.
TrendStationaryAR2_NoTrend_lm <- lm( "REPLACE THIS WITH MODEL of ADF TEST from line 278 but WITHOUT THE TIME TREND TERM")
coeftest(TrendStationaryAR2_NoTrend_lm)[2,3]
URtest_NoTrend_TrendStationaryAR2 <- ur.df(y_AR2_TrendStationary_xts, lags=2, type='drift')
URtest_NoTrend_TrendStationaryAR2@teststat[1]
URtest_NoTrend_TrendStationaryAR2@cval[1,]

# What do we conclude from this test about our time series being stationary? 
# Given what we know about there being a trend in our data, is this correct?
#     Recall that if we reject the null hypothesis, then our time series is stationary.
#     Given all this, does the result of this test make sense?

########### Section 2: Testing for a Break

#===============================================================================
# SET-UP SECTION
# This section between the ====== lines is set-up, where we simulate some
# time series we use below so we can see how the test for a break works.
# If you want to look through this section and see how it works, great!
# If not, you can skip this and focus on the section below where we do the test 
# and analyze results.

# First, we set up a time series with a break.

num_iterations <- c(1000) # Here we are simulating 10 time series
num_periods <- c(100)
break_period <- c(50)

beta0 <- c(1)
delta0 <- c(0)
beta1 <- c(0.15)
delta1 <- c(0.75)

time_series_break50 <- array(c(0),dim = c(num_iterations,num_periods))
beta_0_hat <- array(c(0),dim = c(num_iterations))
beta_1_hat <- array(c(0),dim = c(num_iterations))
delta0_hat <- array(c(0),dim = c(num_iterations))
delta1_hat <- array(c(0),dim = c(num_iterations))

beta_0_hat_naive <- array(c(0),dim = c(num_iterations))
beta_1_hat_naive <- array(c(0),dim = c(num_iterations))

d <- as.Date(1:num_periods)
DickeyFuller_teststats <- array(c(0),dim = c(num_iterations)) # We are also setting up an empty vector to record the t-statistics for beta_1_hat

set.seed(100) ## set seed to replicate randomization

for (i in 1:num_iterations) {   
  u <- rnorm(num_periods)   
  time_series_break50[i,1] = u[1]
  for (t in 2:break_period) {
    time_series_break50[i,t] = beta0 + (beta1 * time_series_break50[i,t-1]) + u[t]
  }
  for (t in (break_period + 1):num_periods) {
    time_series_break50[i,t] = (beta0 + delta0) + ((beta1+delta1) * time_series_break50[i,t-1]) + u[t]
  }
  
  y_break50 = time_series_break50[i,]
  y_break50_xts <- xts(y_break50, d)
  
  D <- 1*(time(y_break50_xts) >= time(y_break50_xts)[50])
  Chow_test_model = lm(y_break50_xts ~ lag(y_break50_xts,1) + D + (D*lag(y_break50_xts,1)))
  beta_0_hat[i] = coeftest(Chow_test_model)[1,1]
  beta_1_hat[i] = coeftest(Chow_test_model)[2,1]
  delta0_hat[i] = coeftest(Chow_test_model)[3,1]
  delta1_hat[i] = coeftest(Chow_test_model)[4,1]  
  
  naive_model <- lm(y_break50_xts ~ lag(y_break50_xts,1))
  beta_0_hat_naive[i] = coeftest(naive_model)[1,1]
  beta_1_hat_naive[i] = coeftest(naive_model)[2,1]
  }

y_break50 = time_series_break50[1,]
y_break50_xts <- xts(y_break50, d)

par(mar=c(4.5, 4.5, 5, 2), xpd=TRUE)
plot(as.zoo(y_break50_xts), 
     col = "purple",      
     lwd = 3,
     main = "Time Series with Break",
     xlab = "Estimates")
abline(v=as.Date(c(50)), xpd=FALSE,
       col = "red",      
       lwd = 2, lty = 2,
       )


#===============================================================================


### Section 1: Chow Test (where we think we know the break)

# The first thing we need to do before running a Chow test is to set up our
#   dummy or indicator variable. This is a variable equal to one if after the
#   period where we think there is a break. Here, we know the break is at period t=50
#   because that's how we simulated it!
# The middle term is the part that sets up the dummy variable.
#   the condition time(y_break50_xts) > time(y_break50_xts)[50]
#   is TRUE if the time of the observation is after the 50th observation  
#   and FALSE if it is before the 50th observation. 
#   So if we multiply this by 1, we get a time series that is zero for the first
#   fifty observations, then equal to one afterwards
D <- 1*(time(y_break50_xts) > time(y_break50_xts)[50]) 
print(D)

# Now, we add this indicator function D to our regression model.
#   Recall the function for the Chow Test: we want to include D as a regressor
#   by itself, as well as a term for D * Y_(t-1).
#### ONE TERM IS MISSING FROM THE BELOW MODEL -- ADD THE NECESSARY TERM FOR A CHOW TEST
Chow_test_model = lm(y_break50_xts ~ lag(y_break50_xts,1) + (D*lag(y_break50_xts,1)))
coeftest(Chow_test_model)
# Now, the Chow test statistic is an F-test for the terms including our indicator function.
#   This means the null hypothesis is that the coefficient of D is zero,
#   and that the coefficient of D * Y_(t-1) is also zero.
#   This gives the constraint "D=0", "lag(y_break50_xts, 1):D=0" we use in the linearHypothesis below.
#   As before, we are using an F-test and using homoskedasticity only errors.
linearHypothesis(Chow_test_model, c("D=0", "lag(y_break50_xts, 1):D=0"), test="F", white.adjust = FALSE)
# From this F-test, can we reject the null hypothesis?
# What can we conclude from this test of whether our time series has a break at t=50?

### Section 2.2: QLR Test
tau_zero =round(0.15*num_periods,digits=1)
tau_one =round((1-0.15)*num_periods,digits=1)
num_tests <- tau_one - tau_zero + 1
tau <- seq(tau_zero, tau_one)

chow_test_statistics <- array(c(0), dim = c(num_tests)) # Set up an empty array where we will store the Chow test statistics
for (i in 1:num_tests) {  # Loop through the code below once for each period in tau_0 to tau_1
  D <- 1*(time(y_break50_xts) > time(y_break50_xts)[tau[i]])  # Set up the indicator function 
  # remember we are looping through the possible tau's, so tau[i] is the potential break date for this particular loop.
  # With this iteration of the indicator variable, estimate the chow model:
  #### Edit the line below to be the Chow Test model
  Chow_test_model = lm("WRITE IN THE CHOW TEST MODEL FROM ABOVE")
  #coeftest(Chow_test_model)
  # Save the Chow Test Statistic ---- CORRECT THIS LINE OF CODE WITH THE APPROPRIATE RESTRICTION FOR THE CHOW TEST
  chow_test = linearHypothesis(Chow_test_model, c("INSERT APPROPRIATE CONSTRAINT HERE"), test="F", white.adjust = FALSE)
  # Store it in the appropriate place in our vector of Chow test stats:
  chow_test_statistics[i] = chow_test$F[2]
}
# Put the tau's and the chow test statistics in one dataframe where we can see them together:
data.frame("Level" = tau, "F-stat" = chow_test_statistics)

QLR_test_stat = max(chow_test_statistics)   # The QLR test stat is the max of the chow test stats
tau_hat <- tau[which.max(chow_test_statistics)] # Tau_hat is the tau with the largest value of the chow test statistics

# Use the QLR test statistics from the assignment handout to conduct the test

### Section 2.3: Problems with Breaks

cat("Indicator Variable model, Mean beta_0_hat : ", mean(beta_0_hat), "\n",
          "Indicator Variable model, Mean beta_1_hat : ", mean(beta_1_hat), "\n",
          "Indicator Variable model, Mean delta_0_hat : ", mean(delta0_hat), "\n",
          "Indicator Variable model, Mean delta_1_hat : ", mean(delta1_hat), "\n",
          "Naive Model Without Indicator Variable, Mean beta_0_hat : ", mean(beta_0_hat_naive), "\n", 
          "Naive Model Without Indicator Variable, Mean beta_1_hat : ", mean(beta_1_hat_naive), "\n")



