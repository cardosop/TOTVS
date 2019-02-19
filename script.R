# Installing Libraries
# install.packages("jsonlite")
# install.packages("DataExplorer")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("sarima")
# install.packages("lmtest")
# install.packages("FitAR")
# install.packages("forecast")


# Loading Libraries
library(jsonlite)
library(DataExplorer)
library(lubridate)
library(stringr)
library(tseries)
library(sarima)
library(lmtest)
library(FitAR)
library(forecast)


# Loading JSON Data into Dataframe
df <- fromJSON("challenge.json", simplifyDataFrame = TRUE)


# Data Exploration I
# Ploting Missing Data - the amount of missing data is almost irrelevant, only 0,06% of is_churn column
plot_missing(df)
# Histogram Plotting - its possible to say that a single variable can be discarted because it holds a single value on all entries. branch_id can be discarted because it has the value 0 on all entries
plot_histogram(df)
# Plotting the Correlation Matrix - presents a low rate of correlations between the variables. Highlights, is_churn_0 has a negative correlation of -0.72 with group_code, on another side, is_churn_1 has the second highest correlation presented on the dataset as 0.64 with group_code. The highest positive correlation is presented between item_total_price and quantity as 0.81
plot_correlation(df)

# Data cleansing
# Dropping brand_id column
df <- subset(df, select = -c(branch_id))
# Cleansing register_date column, by removing hour, minute and second 
df$register_date <- str_split_fixed(df$register_date, fixed("T"),2)[,1]
# Creating columns of Year and Month 
df$year <- year(df$register_date)
df$month <- month(df$register_date)


# Data Exploration II
# Histogram Plotting - now its possible to visualise the distribution of the purchases on the week of the year and on the day of the week
plot_histogram(df)


# Aggregating Sum of total_price by month and year
df1 <- aggregate(df$total_price, by = list(df$year,df$month), FUN = sum)
# Fixing column names
colnames(df1) <- c("year", "month", "sum_total_price")



# Time Series
# Creating timeseries base dataframe
df2 <- df1[3]
dfTs <- ts(df2, start = c(2008,1), frequency = 12)
# Plot the time series
plot(dfTs)
# Decomposing components
decomp <- decompose(dfTs)
plot(decomp)
# Adjusting seasonality
dfAd <- dfTs - decomp$seasonal
plot(dfAd)
# Testing coefficient
fitARIMA <- arima(dfAd, order=c(1,0,0)), period = 12,method="ML")
coeftest(fitARIMA) 
# Rscluding insignificant coefficients
confint(fitARIMA)
# Checking best model
auto.arima(dfAd, trace=TRUE) 
# predicting 6 months ahead
plot(forecast(fitARIMA,6))
