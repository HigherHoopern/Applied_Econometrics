# Week 4 Exercises -- BEEM012.

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

# Change this line to the path for your working directory on your computer:
setwd("~/Dropbox/ExeterTeaching/RScripts/")

# load UK macroeconomic data
UK_gdp <- read_excel(path = "UK_GDPpc_Quarterly-Full.xls")

# Now, we want to reformat our data so that the "Date" column
# is loaded in as a recognized date. To understand the format string
# format = "%Y Q%q" load the raw xls file and look at how dates are stored
# in that file.
UK_gdp$Date <- as.yearqtr(UK_gdp$Date, format = "%Y Q%q")

# We're going to convert our data into an xts formatted object, so we can use
# time series tools easily:
GDP_xts <- xts(UK_gdp$GDPpc, UK_gdp$Date)
GDPGrowth_xts <- xts(400 * log(GDP_xts/lag(GDP_xts)))["1985::2016"]

####### Section 1.2 Set up our list of End Dates

# This line sets up the split of our sample into within-sample and out-of-sample
# This might seem a bit confusing, but when we want to create a sequence of
# dates at the year-quarter level, as in our data, we use a sequence where 
# we write the first quarter as .00, second quarter as 0.25,
# third quarter as 0.50, and the fourth quarter as 0.75
# So, the second quarter of 2005 is 2005.50. 
# In this format, we can create a nice, regular sequence of dates.

# Let's use the period from 2007 to the end of 2013 as our out-of sample period,
# and start with 1985 to the end of 2006 as our within-sample.
end_dates <- seq(2010.00, 2013.75, 0.25)   
# We record the length of this list of dates and store this as P, the number
# of observations we hold out as our pseudo-out-of sample observations.
P <- length(end_dates)

### Section 1.3: Set up empty arrays where we store our outcomes:

# initialize vector where we will store forecasts, errors, the true outcomes and
# the standard errors of our regressions.
# As usual, these are arrays of zeros of length P - one for each out of sample observation
forecasts <- array(c(0),dim = c(P))
true_outcomes <- array(c(0),dim = c(P))
PsOOSF_errors <- array(c(0),dim = c(P))
SER <- array(c(0),dim = c(P))

# Section 1.4 Loop Through End Dates and Estimate PsOOSFs:

for (i in 1:P) {
   EndDate_YearQuarter = as.yearqtr(end_dates[i])  # First, we convert our data back into the Year-Quarter format we are used to.
   # Now, run our regression on the limited sample.
   # First, we limit our sample to observations whose index (i.e the DATE of that observation) comes before the designated EndDate_YearQuarter
   GDPGrowth_estimating_sample <- GDPGrowth_xts[index(GDPGrowth_xts) <  EndDate_YearQuarter]
   AR2_lm <- lm(GDPGrowth_estimating_sample ~ lag(GDPGrowth_estimating_sample) + lag(GDPGrowth_estimating_sample,2))
   SER[i] <- summary(AR2_lm)$sigma
   print(coeftest(AR2_lm))
   # Save Coefficients from the AR2 above:
   beta_0_hat = AR2_lm$coefficients[1]    # The first coefficient is the intercept, etc.
   beta_1_hat = AR2_lm$coefficients[2]
   beta_2_hat = AR2_lm$coefficients[3]

   true_outcome <- GDPGrowth_xts[EndDate_YearQuarter]  # Find the true value of the outcome:
   # compute forecast: we use %*% to do a MATRIX MULTIPLICATION, this is just because the 
   # way we extract GDPGrowth_xts[EndDate_YearQuarter - 0.25] is a matrix, so we need to use
   # a matrix multiplication to get something that we can add up
   pseudo_forecast <- (beta_0_hat + (beta_1_hat %*% GDPGrowth_xts[EndDate_YearQuarter - 0.25]) 
                       + (beta_2_hat %*% GDPGrowth_xts[EndDate_YearQuarter - 0.50]))  
   pseudo_error <- true_outcome  - pseudo_forecast # Compute the error
   
   true_outcomes[i] <- true_outcome      # Store the true outcome, the forecast and the error in the arrays we initialized earlier
   forecasts[i] <- pseudo_forecast
   PsOOSF_errors[i] <- pseudo_error
}

true_outcomes_xts <- xts(true_outcomes, as.yearqtr(end_dates))
forecasts_xts <- xts(forecasts, as.yearqtr(end_dates))

###  Section 1.5: Analysis of PsOOOF Errors
SER_within_sample <- SER[1] # Here we take the first value from our array of SER's. This is the original SER of our first within-sample regression.
Estimated_RMSFE_OutOfSample <- sd(PsOOSF_errors) # Here we estimate the RMSFE by using the standard error of our pseudo out of sample forecast errors

cat("Within-Sample Errors: ", SER_within_sample, "\n")
cat("Estimated RMSFE: ", Estimated_RMSFE_OutOfSample, "\n")

#### Use the mean(), sd() and sqrt() functions to fill in the formula for the t-statistic for our t-test:
t_stat_PsOOSF_errors = mean(PsOOSF_errors) / (sd(PsOOSF_errors) / sqrt(P))
print(t_stat_PsOOSF_errors)

# Now compare this to the manual t-test for mean-zero:
t.test(PsOOSF_errors)

###  Section 1.6: Plotting PsOOSF Errors

# plot the GDP growth time series
plot(as.zoo(true_outcomes_xts),
     col = "purple",
     lwd = 4,
     ylab = "Percent",
     main = "AR2: Pseudo-Out-Of-Sample Forecasts of UK GDP Growth")
# add the series of pseudo-out-of-sample forecasts
lines(as.zoo(forecasts_xts),
      lwd = 4,
      lty = 2)
# shade area between curves (the pseudo forecast error)
polygon(x= c(time(true_outcomes_xts), rev(time(forecasts_xts))),
        y= c(true_outcomes, rev(forecasts)),
        col = "grey85",
        border = NA)
# add a legend
legend("bottomright",
       lty = c(1, 2, 1),
       lwd = c(2, 2, 10),
       inset=c(0,0),
       cex = 0.8,
       col = c("purple", "black", "grey85"),
       legend = c("Actual GDP growth rate",
                  "Forecasted GDP growth rate",
                  "Pseudo forecast Error"))