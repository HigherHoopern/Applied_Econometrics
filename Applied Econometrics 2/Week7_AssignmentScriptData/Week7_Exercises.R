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

####### Section 1.2 Set up our list of Prediction Dates

# Let's run our forecasting process beginning around the largest negative shock 
# associated with the financial crisis, at the beginning of 2009.
prediction_start_date = 2009.00
# We can also run our forecasting beginning with a large positive shock in 2005
# to see how this shock propagates according to our prediction.
# prediction_start_date = 2006.00

# We will finish our predictions in 2019.
prediction_end_date = 2019.00

prediction_dates <- seq(prediction_start_date, prediction_end_date, 0.25)   
# We record the length of this list of dates and store this as P, the number
# of periods where we need to compute forecasts.
P <- length(prediction_dates)

### Section 1.3: Iterated Multiperiod Forecasts
# Set up empty arrays where we store our outcomes:

# initialize vector where we will store iterated and direct forecasts
# As usual, these are arrays of zeros of length P - one for each forecast
iterated_forecasts <- array(c(0),dim = c(P))

# Here we set up our estimating sample - this is similar to Pseudo Out-Of-Sample
# forecasting.
GDPGrowth_estimating_sample <- GDPGrowth_xts[index(GDPGrowth_xts) <  prediction_start_date]

# We will see how an AR(2) model performs, so we define this model here:
AR2_lm <- lm(GDPGrowth_estimating_sample ~ "INSERT REGRESSORS HERE FOR AN AR(2) MODEL OF GDPGrowth_estimating_sample")
print(coeftest(AR2_lm))

# Save Coefficients from the AR2 above. Recall that for the Iterated Forecast,
#     we are using the same coefficients for every forecast.
beta_0_hat = AR2_lm$coefficients[1]    # The first coefficient is the intercept, etc.
beta_1_hat = AR2_lm$coefficients[2]
beta_2_hat = AR2_lm$coefficients[3]

# Here we identify the first time period where we are forecasting.
FirstForecast_YearQuarter = as.yearqtr(prediction_start_date)  # First, we convert our data back into the Year-Quarter format we are used to.

# For the first two periods, our forecast involves true values,
#     so I have set these up for you.
# Here, FirstForecast_YearQuarter is the first period we predict, so it is
#     the same as T+1 in our usual notation. This means that to get the observation for
#     period T, we use [FirstForecast_YearQuarter - (0.25)].
iterated_forecast <- (beta_0_hat + (beta_1_hat %*% GDPGrowth_xts[FirstForecast_YearQuarter- 0.25]) 
                      + (beta_2_hat %*% GDPGrowth_xts[FirstForecast_YearQuarter - 0.50]))  
iterated_forecasts[1] <- iterated_forecast

iterated_forecast <- (beta_0_hat + (beta_1_hat %*% iterated_forecasts[1]) 
                      + (beta_2_hat %*% GDPGrowth_xts[FirstForecast_YearQuarter - 0.25]))  
iterated_forecasts[2] <- iterated_forecast

# For periods 3 and onwards, you are only using forecasts to 
#        continue iterating into the future.

# For each observation i that we forecast, we will use the previous two
#     forecasts as Y_{t-1} and Y_{t-1}
#### IF OUR OUTCOME IN EACH LOOP IS iterated_forecasts[i], update the formula
#### below to get the appropriate Y_{t-1} and Y_{t-2} for forecasting.
for (i in 3:P) {
   iterated_forecast <- (beta_0_hat + (beta_1_hat %*% iterated_forecasts[i-1]) 
                       + (beta_2_hat %*% "INSERT APPROPRIATE REFERENCE TO Y_{t-2} HERE!"))  
   iterated_forecasts[i] <- iterated_forecast
}

# Now, let's save this as an xts so we can process it as a time series.
iterated_forecasts_xts <- xts(iterated_forecasts, as.yearqtr(prediction_dates))

### Section 1.4: Direct Multiperiod Forecasts
direct_forecasts <- array(c(0),dim = c(P))

for (i in 1:P) {
   # Here, for every period we forecast in the future, we need to run regressions
   #     with the appropriate lag. So to predict 5 periods in the future, the 
   #     appropriate AR(2) includes regressors Y_{t-5} and Y_{t-6}.
   ##### FOR FORECASTING i PERIODS INTO THE FUTURE, WHICH LAGS DO WE USE?
   ##### INSERT APPROPRIATE LAGS INTO COMMAND BELOW USING NOTATION REFERRING TO i.
   AR2_DirectForecast_lm <- lm(GDPGrowth_estimating_sample ~ lag(GDPGrowth_estimating_sample,("INSERT LAG HERE")) + lag(GDPGrowth_estimating_sample,("INSERT LAG HERE")))

   # Every forecasting period is a separate regression, so we need to use
   #     new parameter estimates each time.
   beta_0_hat = AR2_DirectForecast_lm$coefficients[1]    # The first coefficient is the intercept, etc.
   beta_1_hat = AR2_DirectForecast_lm$coefficients[2]
   beta_2_hat = AR2_DirectForecast_lm$coefficients[3]
   
   # Here, FirstForecast_YearQuarter is the first period we predict, so it is
   #     the same as T+1 in our usual notation. This means that to get the observation for
   #     period T, we use [FirstForecast_YearQuarter - (0.25)].
   #### FILL IN THE APPROPRIATE COEFFICIENTS SAVED ABOVE 
   direct_forecasts[i] <- ("INSERT COEFFICIENT HERE" + ("INSERT COEFFICIENT HERE" %*% GDPGrowth_xts[FirstForecast_YearQuarter - (0.25)]) 
                       + ("INSERT COEFFICIENT HERE" %*% GDPGrowth_xts[FirstForecast_YearQuarter - (0.50)]))  
}
direct_forecasts_xts <- xts(direct_forecasts, as.yearqtr(prediction_dates))

### Section 1.5: Plotting Forecasts

# Here we are just selecting the part of the series we will plot.
GDPGrowth_forecasting_region <- GDPGrowth_xts[index(GDPGrowth_xts) <  prediction_end_date & index(GDPGrowth_xts) > (prediction_start_date - 10)]

# plot the GDP growth time series
plot(as.zoo(GDPGrowth_forecasting_region), 
     col = "purple",
     lwd = 4,
     ylab = "Percent",
     main = "Multiperiod AR(2) Forecasts of UK GDP Growth")
# add the series of pseudo-out-of-sample forecasts
lines(as.zoo(direct_forecasts_xts),
      lwd = 4,
      col= "green",
      lty = 2)
lines(as.zoo(iterated_forecasts_xts),
      lwd = 4,
      col= "blue",
      lty = 2)
legend("bottomleft",
       lty = c(1, 2, 1),
       lwd = c(2, 2, 10),
       cex = 0.8,
       col = c("purple", "green", "blue"),
       legend = c("GDP-GR", "Direct Forecast", "Iterated Forecast"))