# Week 9 Exercises -- BEEM012.

# Adapted from exercises and examples in Hanck, Arnold, Gerber, and Schmelzer (2020) 
# Introduction to Econometrics with R.  
# This book is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. (HA)


### Section 1.1 Set-Up and Loading Data
# Install the following packages: 
# (Once you have installed these, you can comment out these lines)
install.packages("readxl")
install.packages("AER")
install.packages("xts")
install.packages("dynlm")

# We will also be using the "orcutt" package to automatically
# run the iterated Cochrane-Orcutt estimator, so uncomment this line
# to install the first time you run:
install.packages("orcutt")
install.packages("vars")
install.packages("quantmod")

# Load useful libraries we will use for time series.
library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)
library(urca)
library(orcutt)
library(vars)

### Section 1.1: Install the fGarch and quantmod package------------------------
install.packages("fGarch")
install.packages("quantmod")
library(fGarch)
library(quantmod)

### Section 1.2: Load & Format Data---------------------------------------------
# Change this line to the path for your working directory on your computer:
setwd("/Users/jason/Desktop/Applied_Ecoometrics_2/week9/Week9-DataRScript")

# import data on the Wilshire 5000 stock price index
W5000 <- read_xlsx("WILL5000.xlsx")

# transform the columns (this is your usual step to make sure the formats are right)
W5000$DATE <- as.Date(W5000$observation_date)
W5000$WILL5000INDFC <- as.numeric(W5000$WILL5000INDFC)

# there are a few missing values so we use this command to remove NAs,
# where NA just means there is a missing observation:
W5000 <- na.omit(W5000)

# compute daily percentage changes. Here we can use the Delt function from the
# package quantmod
N<-length(W5000$WILL5000INDFC)
W5000_PC_xts =  xts(100 * Delt(W5000$WILL5000INDFC)[-1], W5000$DATE[-N])
# We can also compute this manually using the diff() tool we have use before:
#W5000_PC_xts = xts(100*(diff(W5000$WILL5000INDFC)/W5000$WILL5000INDFC[-N]), W5000$DATE[-1])

# Now, set up a list of dates that we will use to convert our data into xts objects:
dates <- W5000$DATE[-N]

#### Section 1.3: Plot Percentage Changes in Stock-Price Index: ----------------
# First, we will plot the percentage changes. Does it look like it follows a predictable pattern?
# Does it look like the noisiness of this data is more predictable?
plot(as.zoo(W5000_PC_xts), 
     ylab = "Percent",
     xlab = "Date",
     main = "Daily Percentage Changes \n Wilshire 5000 Stock Price Index",
     type="l", 
     col = "steelblue", 
     lwd = 0.5)
# add horizontal line at y = 0
abline(0, 0)

#### Section 1.4: Fit ARMA(1,1)-GARCH(1,0) Model -------------------------------
# Now we're going to fit a model to our data, where we specify what type of ARMA
#     model we want to use to model the values of our outcome, and what type of 
#     GARCH model we want to use to model the conditional heteroskedasticity:
#     We use the "data = W5000_PC_xts" command to tell it what data to use,
#     and "trace = F" just tells it to hide details of the procedure we don't
#     care about.
GARCH_Wilshire <- garchFit( ~ arma(1,1) + garch(1,0), data = W5000_PC_xts, trace = F)
# Now, let's look at the output from this model fit. Scroll to the "Error Analysis"
#     section of the output to interpret the significance of the coefficients.
#     Which of the GARCH parameters are statistically significant?
summary(GARCH_Wilshire)

# Look at our model predictions of the actual value of Percentage Change
W5000_PC_fitted_xts = xts(GARCH_Wilshire@fitted, dates)

# So we can see what's going on, let's narrow in on one small time period.
# Let's focus on 2008-2009:
W5000_PC_2008to2009_xts <- W5000_PC_xts["2008::2009"]
W5000_PC_fitted_2008to2009_xts <- W5000_PC_fitted_xts["2008::2009"]

# plot percentage changes, as well as our model-fitted values
#     Does it look like we do a good job predicting the value or direction 
#     of the actual percentage change?
plot(as.zoo(W5000_PC_2008to2009_xts), 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "ARMA(1,1)-GARCH(1,0) \n Predicted Percentage Change",
     lwd = 0.25)
# add horizontal line at y = 0
abline(0, 0)
# add the model-fitted values of the percentage change to the plot:
lines(as.zoo(W5000_PC_fitted_2008to2009_xts), 
      col = "forestgreen", 
      lwd = 1.5)
legend("topright",
       lty = c(1, 1),
       lwd = c(0.2, 1),
       cex = 0.8,
       col = c("steelblue", "forestgreen"),
       legend = c("Actual Percentage Change", "Predicted Percentage Change"))

# compute deviations of the percentage changes from their mean
demean_W5000_PC <- W5000_PC_xts - GARCH_Wilshire@fit$coef[1]
# The demean_W5000_PC_Pcsd time series indicates the mean estimated by our model,
#     plus the conditional standard error: this is our model fit of the
#     conditional standard error at a given time, based on other values
demean_W5000_PC_Pcsd <- GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t
# The demean_W5000_PC_Mcsd time series indicates the mean estimated by our model,
#     minus the conditional standard error: this is our model fit of the
#     conditional standard error at a given time, based on other values
demean_W5000_PC_Mcsd <- GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t
# Together, the plus/minus one conditional standard error gives a confidence
#     interval for the predicted volatility of the time series.

# Now, let' convert these into xts objects so we can use our usual tools:
demean_W5000_PC_xts <- xts(demean_W5000_PC, dates)
demean_W5000_PC_Pcsd_xts <- xts(demean_W5000_PC_Pcsd, dates)
demean_W5000_PC_Mcsd_xts <- xts(demean_W5000_PC_Mcsd, dates)

# So we can see what's going on, let's narrow in on one small time period.
# Again, let's focus on 2008-2009:
demean_W5000_PC_2008to2009 <- demean_W5000_PC_xts["2008::2009"]
demean_W5000_PC_Pcsd_2008to2009 <- demean_W5000_PC_Pcsd_xts["2008::2009"]
demean_W5000_PC_Mcsd_2008to2009 <- demean_W5000_PC_Mcsd_xts["2008::2009"]

# plot deviation of percentage changes from mean
plot(as.zoo(demean_W5000_PC_2008to2009), 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "ARMA(1,1)-GARCH(1,0) \n Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)
# add horizontal line at y = 0
abline(0, 0)
# add GARCH confidence bands (one standard deviation) to the plot
lines(as.zoo(demean_W5000_PC_Pcsd_2008to2009), 
      col = "darkred", 
      lwd = 0.5)
lines(as.zoo(demean_W5000_PC_Mcsd_2008to2009), 
      col = "darkred", 
      lwd = 0.5)
legend("topright",
       lty = c(1, 1),
       lwd = c(0.5, 0.5),
       cex = 0.7,
       col = c("steelblue", "darkred"),
       legend = c("Actual Percentage Change", "+/- Conditional Standard Deviation"))



#### Section 1.5: Fit ARMA(1,1)-GARCH(1,1) Model -------------------------------
# Now we're going to fit a model to our data, where we specify what type of ARMA
#     model we want to use to model the values of our outcome, and what type of 
#     GARCH model we want to use to model the conditional heteroskedasticity:
#     We use the "data = W5000_PC_xts" command to tell it what data to use,
#     and "trace = F" just tells it to hide details of the procedure we don't
#     care about.
##### EDIT THE FOLLOWING LINE TO PROPERLY ESTIMATE THE ARCH(1,1)-GARCH(1,1) MODEL:
GARCH_Wilshire <- garchFit("FILL IN THE COMMAND TO RUN AN ARCH(1,1)-GARCH(1,1) MODEL", data = W5000_PC_xts, trace = F)
# Now, let's look at the output from this model fit. Scroll to the "Error Analysis"
#     section of the output to interpret the significance of the coefficients.
#     Which of the GARCH parameters are statistically significant?
summary(GARCH_Wilshire)

# Look at our model predictions of the actual value of Percentage Change
W5000_PC_fitted_xts = xts(GARCH_Wilshire@fitted, dates)

# So we can see what's going on, let's narrow in on one small time period.
# Let's focus on 2008-2009:
W5000_PC_2008to2009_xts <- W5000_PC_xts["2008::2009"]
W5000_PC_fitted_2008to2009_xts <- W5000_PC_fitted_xts["2008::2009"]

# # plot percentage changes, as well as our model-fitted values
# #     Does it look like we do a good job predicting the value or direction 
# #     of the actual percentage change?
# plot(as.zoo(W5000_PC_2008to2009_xts), 
#      type = "l", 
#      col = "steelblue",
#      ylab = "Percent", 
#      xlab = "Date",
#      main = "ARMA(1,1)-GARCH(1,1) \n Predicted Percentage Change",
#      lwd = 0.25)
# # add horizontal line at y = 0
# abline(0, 0)
# # add the model-fitted values of the percentage change to the plot:
# lines(as.zoo(W5000_PC_fitted_2008to2009_xts), 
#       col = "forestgreen", 
#       lwd = 1.5)
# legend("topright",
#        lty = c(1, 1),
#        lwd = c(0.2, 1),
#        cex = 0.8,
#        col = c("steelblue", "forestgreen"),
#        legend = c("Actual Percentage Change", "Predicted Percentage Change"))

# compute deviations of the percentage changes from their mean
demean_W5000_PC <- W5000_PC_xts - GARCH_Wilshire@fit$coef[1]
# The demean_W5000_PC_Pcsd time series indicates the mean estimated by our model,
#     plus the conditional standard error: this is our model fit of the
#     conditional standard error at a given time, based on other values
demean_W5000_PC_Pcsd <- GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t
# The demean_W5000_PC_Mcsd time series indicates the mean estimated by our model,
#     minus the conditional standard error: this is our model fit of the
#     conditional standard error at a given time, based on other values
demean_W5000_PC_Mcsd <- GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t
# Together, the plus/minus one conditional standard error gives a confidence
#     interval for the predicted volatility of the time series.

# Now, let' convert these into xts objects so we can use our usual tools:
demean_W5000_PC_xts <- xts(demean_W5000_PC, dates)
demean_W5000_PC_Pcsd_xts <- xts(demean_W5000_PC_Pcsd, dates)
demean_W5000_PC_Mcsd_xts <- xts(demean_W5000_PC_Mcsd, dates)

# So we can see what's going on, let's narrow in on one small time period.
# Again, let's focus on 2008-2009:
demean_W5000_PC_2008to2009 <- demean_W5000_PC_xts["2008::2009"]
demean_W5000_PC_Pcsd_2008to2009 <- demean_W5000_PC_Pcsd_xts["2008::2009"]
demean_W5000_PC_Mcsd_2008to2009 <- demean_W5000_PC_Mcsd_xts["2008::2009"]

# plot deviation of percentage changes from mean
plot(as.zoo(demean_W5000_PC_2008to2009), 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "ARMA(1,1)-GARCH(1,1) \n Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)
# add horizontal line at y = 0
abline(0, 0)
# add GARCH confidence bands (one standard deviation) to the plot
lines(as.zoo(demean_W5000_PC_Pcsd_2008to2009), 
      col = "darkred", 
      lwd = 0.5)
lines(as.zoo(demean_W5000_PC_Mcsd_2008to2009), 
      col = "darkred", 
      lwd = 0.5)
legend("topright",
       lty = c(1, 1),
       lwd = c(0.5, 0.5),
       cex = 0.7,
       col = c("steelblue", "darkred"),
       legend = c("Actual Percentage Change", "+/- Conditional Standard Deviation"))

#### Section 1.6: Fit ARMA(3,3)-GARCH(3,3) Model -------------------------------
# Now we're going to fit a model to our data, where we specify what type of ARMA
#     model we want to use to model the values of our outcome, and what type of 
#     GARCH model we want to use to model the conditional heteroskedasticity:
#     We use the "data = W5000_PC_xts" command to tell it what data to use,
#     and "trace = F" just tells it to hide details of the procedure we don't
#     care about.
##### EDIT THE FOLLOWING LINE TO PROPERLY ESTIMATE THE ARCH(1,1)-GARCH(1,1) MODEL:
GARCH_Wilshire <- "ADD THE garchFit() command here to estimate an ARCH(3,3)-GARCH(3,3) MODEL"

# Now, let's look at the output from this model fit. Scroll to the "Error Analysis"
#     section of the output to interpret the significance of the coefficients.
#     Which of the GARCH parameters are statistically significant?
summary(GARCH_Wilshire)

# Look at our model predictions of the actual value of Percentage Change
W5000_PC_fitted_xts = xts(GARCH_Wilshire@fitted, dates)

# So we can see what's going on, let's narrow in on one small time period.
# Let's focus on 2008-2009:
W5000_PC_2008to2009_xts <- W5000_PC_xts["2008::2009"]
W5000_PC_fitted_2008to2009_xts <- W5000_PC_fitted_xts["2008::2009"]

# plot percentage changes, as well as our model-fitted values
#     Does it look like we do a good job predicting the value or direction 
#     of the actual percentage change?
plot(as.zoo(W5000_PC_2008to2009_xts), 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "ARMA(3,3)-GARCH(3,3) \n Predicted Percentage Change",
     lwd = 0.25)
# add horizontal line at y = 0
abline(0, 0)
# add the model-fitted values of the percentage change to the plot:
lines(as.zoo(W5000_PC_fitted_2008to2009_xts), 
      col = "forestgreen", 
      lwd = 1.5)
legend("topright",
       lty = c(1, 1),
       lwd = c(0.2, 1),
       cex = 0.8,
       col = c("steelblue", "forestgreen"),
       legend = c("Actual Percentage Change", "Predicted Percentage Change"))

# compute deviations of the percentage changes from their mean
demean_W5000_PC <- W5000_PC_xts - GARCH_Wilshire@fit$coef[1]
# The demean_W5000_PC_Pcsd time series indicates the mean estimated by our model,
#     plus the conditional standard error: this is our model fit of the
#     conditional standard error at a given time, based on other values
demean_W5000_PC_Pcsd <- GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t
# The demean_W5000_PC_Mcsd time series indicates the mean estimated by our model,
#     minus the conditional standard error: this is our model fit of the
#     conditional standard error at a given time, based on other values
demean_W5000_PC_Mcsd <- GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t
# Together, the plus/minus one conditional standard error gives a confidence
#     interval for the predicted volatility of the time series.

# Now, let' convert these into xts objects so we can use our usual tools:
demean_W5000_PC_xts <- xts(demean_W5000_PC, dates)
demean_W5000_PC_Pcsd_xts <- xts(demean_W5000_PC_Pcsd, dates)
demean_W5000_PC_Mcsd_xts <- xts(demean_W5000_PC_Mcsd, dates)

# So we can see what's going on, let's narrow in on one small time period.
# Again, let's focus on 2008-2009:
demean_W5000_PC_2008to2009 <- demean_W5000_PC_xts["2008::2009"]
demean_W5000_PC_Pcsd_2008to2009 <- demean_W5000_PC_Pcsd_xts["2008::2009"]
demean_W5000_PC_Mcsd_2008to2009 <- demean_W5000_PC_Mcsd_xts["2008::2009"]

# plot deviation of percentage changes from mean
plot(as.zoo(demean_W5000_PC_2008to2009), 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "ARMA(3,3)-GARCH(3,3) \n Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)
# add horizontal line at y = 0
abline(0, 0)
# add GARCH confidence bands (one standard deviation) to the plot
lines(as.zoo(demean_W5000_PC_Pcsd_2008to2009), 
      col = "darkred", 
      lwd = 0.5)
lines(as.zoo(demean_W5000_PC_Mcsd_2008to2009), 
      col = "darkred", 
      lwd = 0.5)
legend("topright",
       lty = c(1, 1),
       lwd = c(0.5, 0.5),
       cex = 0.7,
       col = c("steelblue", "darkred"),
       legend = c("Actual Percentage Change", "+/- Conditional Standard Deviation"))