# Week 2 Exercises -- BEEM012.

# Adapted from exercises and examples in Hanck, Arnold, Gerber, and Schmelzer (2020) 
# Introduction to Econometrics with R. This book is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. (HA)

# Load useful libraries we will use for time series.
library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)

# Section 1: Granger Causality
# First we will show how to conduct a Granger causality test, using the F-statistic.

# Change this line to the path for your working directory on your computer:
setwd("~/Dropbox/ExeterTeaching/RScripts/")

# load UK macroeconomic data and set it up, just as we did in the last tutorial:
UK_gdp <- read_excel(path = "UK_GDPpc_Quarterly-Full.xls")
UK_gdp$Date <- as.yearqtr(UK_gdp$Date, format = "%Y Q%q")
GDP_xts <- xts(UK_gdp$GDPpc, UK_gdp$Date)["1960::2016"]

GDPGrowth_xts <- xts(400 * log(GDP_xts/lag(GDP_xts)))
GDPGR_1985_2015 <- GDPGrowth_xts["1985::2015"]

UK_TermYields <- read_excel(path = "UK_yield_3mo_5yr_10yr.xls")
UK_TermYields$Date <- as.yearqtr(UK_TermYields$Date, format = "Q%q %Y")

UK_Yield3mo <- xts(UK_TermYields$Y3mo, UK_TermYields$Date)["1982::2016"]
UK_Yield5yr <- xts(UK_TermYields$Y5yr, UK_TermYields$Date)["1982::2016"]
UK_Yield10yr <- xts(UK_TermYields$Y10yr, UK_TermYields$Date)["1982::2016"]

UK_TermSpread_10yr_3mo <- UK_Yield10yr - UK_Yield3mo
UK_TermSpread_5yr_3mo <- UK_Yield5yr - UK_Yield3mo

UK_TermSpread_10yr_3mo_1985_2015 <- UK_TermSpread_10yr_3mo["1985::2015"]
UK_TermSpread_5yr_3mo_1985_2015 <- UK_TermSpread_5yr_3mo["1985::2015"]

UK_UnemploymentRate <- read_excel(path = "UK_UnemploymentRate.xls")
UK_UnemploymentRate$Date <- as.yearqtr(UK_UnemploymentRate$Date, format = "%Y Q%q")
UK_UnemploymentRate_xts <- xts(UK_UnemploymentRate$UnemploymentRate, UK_UnemploymentRate$Date)
UK_UnemploymentRate_1985_2015 <- UK_UnemploymentRate_xts["1985::2015"]

# Take the code above to estimate the ADL22 model:
GDPGR_ADL22 <- lm(GDPGR_1985_2015 ~ lag(GDPGR_1985_2015,1) + lag(GDPGR_1985_2015,2) + lag(UK_UnemploymentRate_1985_2015,1) + lag(UK_UnemploymentRate_1985_2015,2))
coeftest(GDPGR_ADL22)
SSR_unrestricted <- sum(GDPGR_ADL22$residuals^2) # Save out the SSR of this unrestricted model for use in the F-statistic
q = c(2) # Here we are looking at a test with TWO restrictions, so q=2
df <- GDPGR_ADL22$df.residual # Save out the degrees of freedom (df = n - q - 1) for use in the F-stat computation.

# Manual F-Test for unemployment:
# First, we need to estimate the restricted model, without the two terms for unemployment.
# Modify the lm() function above estimating the GDPGR_ADL22 linear model so 
# that it estimates the restricted model for a joint test of significance of 
# unemployment rate terms, and store it as GDPGR_ADL22_restrict_unemployment
GDPGR_ADL22_restrict_unemployment <- ## Insert your modified lm() function call here!
coeftest(GDPGR_ADL22_restrict_unemployment)
SSR_restricted <- sum(GDPGR_ADL22_restrict_unemployment$residuals^2) # Save out the SSR of this restricted model for use in the F-statistic

# Use the formula from the homework handout or lecture slides to compute the 
# f-statistic, using the values computed above:
Fstatistic = ### INSERT F-STATISTIC FORMULA HERE
cat("Manually Computed F-statistic", Fstatistic, "\n")

# Now, use an F-Test to check whether the two lags of Unemployment 
# are jointly significant:
linearHypothesis(GDPGR_ADL22, c("lag(UK_UnemploymentRate_1985_2015, 1)=0", "lag(UK_UnemploymentRate_1985_2015, 2)=0"), test ="F", white.adjust=FALSE)


### Section 2: Selecting Lag Length
# First, try computing this using the t-test on the last lag: 
# To make things simpler, you can use the notation 1:6 as the argument
# of lag() to include all lags from 1:6 
GDPGR_AR6 <- lm(GDPGR_1985_2015 ~ lag(GDPGR_1985_2015,1:6))
coeftest(GDPGR_AR6)

GDPGR_AR5 <- lm(GDPGR_1985_2015 ~ lag(GDPGR_1985_2015,1:5))
coeftest(GDPGR_AR5)

### COPY THE CODE ABOVE TO DO THE SAME FOR p=4, p=3, p=2.

# Next: compute BIC and AIC for AR models using a convenient function.
IC <- function(model) {
  ssr <- sum(model$residuals^2) # This retrieves the sum of squared residuals
  t <- length(model$residuals) # This retrieves the LENGTH of the residuals - this is the same as the number of observations
  p <- length(model$coef) - 1 # Here we take the length of the list of coefficients and subtract 1 to get p - since this list includes the intercept.
  return(
    round(c("p" = p,
            # Insert code here to compute the Bayes Information Criterion
            # Refer to the Homework Handout or lecture slides for the formula.
            # Remember - put the ',' at the end of the line - without the 
            # comma at the end of the line, the function won't be formatted properly.
            "BIC" = , #### INSERT THE FORMULA FOR BIC HERE - TO THE **LEFT** OF THE COMMA
            # Now, do the same as above for the Akaike Information Criterion:
            # Remember to add a comma at the end of the line!
            "AIC" = , #### INSERT THE FORMULA FOR AIC HERE - TO THE **LEFT** OF THE COMMA
            "R2" = summary(model)$r.squared), 
          4) # This last line just says to round to 4 decimal points.
  )
}

# Here we manually run this for each of p = 1,2,3,4,5,6.
IC(lm(xts(GDPGR_1985_2015) ~ 1))
IC(lm(xts(GDPGR_1985_2015) ~ lag(xts(GDPGR_1985_2015),1:2)))
IC(lm(xts(GDPGR_1985_2015) ~ lag(xts(GDPGR_1985_2015),1:3)))
IC(lm(xts(GDPGR_1985_2015) ~ lag(xts(GDPGR_1985_2015),1:4)))
IC(lm(xts(GDPGR_1985_2015) ~ lag(xts(GDPGR_1985_2015),1:5)))
IC(lm(xts(GDPGR_1985_2015) ~ lag(xts(GDPGR_1985_2015),1:6)))

# loop ICs over models of different orders from p=1 to p=6.
for (p in 1:6) {
  print(IC(lm(GDPGR_1985_2015 ~ lag(GDPGR_1985_2015,1:p))))
}

### Section 2.2: Lag Length and ADL(2,2)
### Now, loop ICs over ADL models of different orders. Use the exact same code 
### as above and modify so the model inside the loop also includes up to p lags 
### of the unemployment rate as well.
for (p in 1:6) {
  print(IC(lm(GDPGR_1985_2015 ~ lag(GDPGR_1985_2015,1:p) ))) ### MODIFY THIS LINE TO MAKE IT AN ADL(2,2) Model!
}

# Section 3 -- Stationarity

### Section 3.1: Distribution of Unit Root over Time
num_iterations <- c(1000) # Set the number of iterations
num_periods <- c(5) # Set the number of periods to simulate

random_walk <- array(c(0),dim = c(num_iterations,num_periods)) # Set up an empty array to store our outcomes

set.seed(100) ## set seed to replicate randomization

for (i in 1:num_iterations) {    ## create a loop replicating simulation for the number of iterations we set
  u <- rnorm(num_periods)     ## draw a vector of errors, one for each of the periods
  random_walk[i,1] = u[1]     # Set the first y_0 = u_0
  for (t in 2:num_periods) {
    random_walk[i,t] = random_walk[i,t-1] + u[t]  # Compute y_t = y_(t-1) + u_t for remaining periods
  }
}

#### MODIFY THE COMMAND TO GET THE LIST of y_1 to GET y_3:
y_1 <- random_walk[,1] # Here we take the first column of the array - this is a list of all the y_1's across all iterations
y_3 <-  # Here take the third column of the array - this is a list of all the y_3's across all iterations
y_T <- random_walk[,num_periods] # Here we take the last column of the array - this is a list of all the y_T's across all iterations

plot(density(y_1),    
     col = "purple",  
     xlim = c(-5,5),  
     lwd = 3,
     main = "Density of Random Walk",
     xlab = "Estimates")
lines(density(y_3),   # This lines() command adds another line to our graph
      col = "orange", # the first argument is density(y_3) which tells it to plot the density of the vector y_3
      lwd = 3)
lines(density(y_T),   
      col = "steelblue",
      lwd = 3)
legend("topright",      # Add a legend so we can see which line is which.
       c("T=1", "T=3", "T=5"), 
       col = c("purple", "orange", "steelblue"), 
       lty = c(1, 1),
       cex = 0.8
)

### Section 3.2: Coefficients Biased Towards Zero

##### Section 3.2.1: 50 Periods
num_iterations <- c(100)
num_periods <- c(50)

random_walk <- array(c(0),dim = c(num_iterations,num_periods))
beta_1_hat <- array(c(0),dim = c(num_iterations)) # Now, we are setting up an empty vector to record the estimated beta_1_hat's
t_statistics <- array(c(0),dim = c(num_iterations)) # We are also setting up an empty vector to record the t-statistics for beta_1_hat

set.seed(100) ## set seed to replicate randomization

for (i in 1:num_iterations) {   
  u <- rnorm(num_periods)     
  random_walk[i,1] = u[1]
  for (t in 2:num_periods) {
    random_walk[i,t] = random_walk[i,t-1] + u[t]
  }
  y_unit_root = random_walk[i,] # In addition to what we do in our previous loop, we take the current iteration of the outcomes we simulated
  d <- as.Date(1:num_periods) # We create a date vector so that we can set up an xts object as before
  y_unit_root_xts <- xts(y_unit_root, order.by=d) # Using the outcomes from just this iteration and the date vector, we set up an xts object
  AR1_lm <- lm(y_unit_root_xts ~ lag(y_unit_root_xts,1)) # Now, we estimate an AR1 model on this iteration using the xts object above
  beta_1_hat[i] = AR1_lm$coefficients[2] # Beta_1_hat is the 2nd element in the coefficient vector (as beta_0_hat is the first)
  t_statistics[i] = coeftest(AR1_lm)[2,3] # Similarly, we save the value of the beta_1_hat t-stat. To see why we use the [2,3] element, run a coeftest on an AR1 and look at the array of outcomes.
}

##### COMPUTE THE ANALYTICAL EXPECTED VALUE OF beta_1_hat AS IN THE HOMEWORK HANDOUT
cat("Mean of Beta_1_hat: ", mean(beta_1_hat), "\n") # Now, we take the mean over the vector of beta_1_hats we estimated, one for each iterated simulation
expected_value_analytical = # INSERT the analytical formula from the lectures or homework handout to compute the analytical expected value of beta_1_hat
cat("Analytical Expected Value of Beta_1_hat: ", expected_value_analytical, "\n") # print the above value so we can compare to the actual mean

##### Section 3.2.1: 100 Periods
### This is the exact same as above, but we will now simulate 100 periods for each time series.
num_iterations <- c(100)
num_periods <- c(100)

random_walk <- array(c(0),dim = c(num_iterations,num_periods))
beta_1_hat <- array(c(0),dim = c(num_iterations))
t_statistics_100T <- array(c(0),dim = c(num_iterations))

set.seed(100) ## set seed to replicate randomization

for (i in 1:num_iterations) { 
  u <- rnorm(num_periods)     
  random_walk[i,1] = u[1]
  for (t in 2:num_periods) {
    random_walk[i,t] = (1 * random_walk[i,t-1]) + u[t]
  }
  y_unit_root = random_walk[i,]
  d <- as.Date(1:num_periods)
  y_unit_root_xts <- xts(y_unit_root, order.by=d)
  AR1_lm <- lm(y_unit_root_xts ~ lag(y_unit_root_xts,1))
  beta_1_hat[i] = AR1_lm$coefficients[2]
  t_statistics_100T[i] = coeftest(AR1_lm)[2,3]
}

cat("Mean of Beta_1_hat: ", mean(beta_1_hat), "\n")
expected_value_analytical = -(1 - (5.3 / num_periods))
cat("Expected Value of Beta_1_hat: ", expected_value_analytical, "\n")

t_statistics_mean = mean(t_statistics) # Here we compute the mean of the t-statistics
t_statistics_sd = sd(t_statistics) # here we compute the standard deviation of the t-statistics
t_statistics_z = (t_statistics - t_statistics_mean) / t_statistics_sd # here we compute the z-score of the t-statistics

#### NOW, USE THE SAME CODE AS ABOVE TO COMPUTE THE z-scores for the time series with T=100
t_statistics_100T_mean = ### INSERT CODE HERE TO COMPUTE THE MEAN
t_statistics_100T_sd = ### INSERT CODE HERE TO COMPUTE THE STANDARD DEVIATION
t_statistics_100T_z = ### INSERT CODE HERE TO COMPUTE THE Z-SCORE USING MEAN AND STANDARD DEVIATION

### Compare to a normal var:
t_stdnormal <- rnorm(1000) # This draw of 1000 outcomes from a standard normal will be compared to the distribution of t-statistics
  
### Section 3.3: Non-Normally Distributed T-statistics
par(mar=c(4.5, 4.5, 8, 2), xpd=TRUE)
plot(density(t_statistics_z), 
     col = "purple",      
     xlim = c(-3,3),
     lwd = 3,
     main = "Density of Random Walk T-Stat",
     xlab = "Estimates")
lines(density(t_statistics_100T_z),
      col = "orange",     
      lwd = 3)
lines(density(t_stdnormal), 
      col = "steelblue",    
      xlim = c(-3,3),      
      lwd = 3)
legend("top",   
       c("T-Stat, 50 Periods", "T-Stat, 100 Periods", "Std. Normal"), 
       col = c("purple", "orange", "steelblue"), 
       lty = c(1, 1),
       inset=c(0,-0.6),
       cex = 0.8
)

### Section 3.4: Spurious Regressions
num_iterations <- c(2) # Here we are just comparing two time series, so num_iterations = 2
num_periods <- c(100)

random_walk <- array(c(0),dim = c(num_iterations,num_periods))
random_walk_errors <- array(c(0),dim = c(num_iterations,num_periods))
beta_1_hat <- array(c(0),dim = c(num_iterations))

set.seed(150) ## set seed to replicate randomization

for (i in 1:num_iterations) {   
  u <- rnorm(num_periods)   
  random_walk[i,1] = u[1]
  random_walk_errors[i,1] = u[1]
  for (t in 2:num_periods) {
    random_walk_errors[i,t] = u[t]
    random_walk[i,t] = (1 * random_walk[i,t-1]) + u[t]
  }
}

y_series1 = random_walk[1,] # Now we pull out just the values from the first time series
y_series2 = random_walk[2,] # Now we pull out just the values from the second time series
d <- as.Date(1:num_periods)
y_series1_xts <- xts(y_series1, order.by=d) # Here we make these both into xts objects
y_series2_xts <- xts(y_series2, order.by=d)

### Now, plot these two time series constructed from unit root processes with independently drawn errors.
plot(x = zoo(y_series1), ylab = "y_t", main = "Spurious Regression", 
     xlab = "t",
     ylim=c(-10,15),
     col = c("darkgreen"), screens = 1)
lines(x = zoo(y_series2),
       col = c("darkred")
      )
legend("top",       
       c("Random Walk 1", "Random Walk 2"), 
       col = c("darkgreen", "darkred"), 
       lty = c(1, 1),
       inset=c(0,-0.6),
       cex = 0.8
)

# Now, explore the relationship in a regression framework. Run a regression of 
# Y1_t = \beta_0 + \beta_1 Y2_(t-1)
# Does the lag of Y2 predict Y1?
AR1_lm <- lm(y_series1_xts ~ ) #### TO REGRESS Y1_t on Y2_(t-1), INSERT THE APPROPRIATE REGRESSOR INTO THE lm() COMMAND
coeftest(AR1_lm)

