library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)

# load the Dataset
AllData <- read_excel("Project_BFD/AnnualData_all.xlsx")


### EXPLORATORY DATA ANALYSIS ###

# check for missing values
any(is.na(AllData))
# check for NaN values
any(is.nan(as.matrix(AllData)))

# create variables for each predictor in the Dataset
# and plot them w.r.t. years, then look at Autocorrelation function

# Years
year <- AllData$Year

# GSPC
GSPC <- AllData$GSPC
plot(year, GSPC, xlab = 'Years', ylab = 'GSPC')
acf(GSPC)

# VIX
VIX <- AllData$VIX
plot(year, VIX, xlab = 'Years', ylab = 'VIX')
acf(VIX)

# GNI
GNI <- AllData$GNI
plot(year, GNI, xlab = 'Years', ylab = 'GNI')
acf(GNI)

# Population
population <- AllData$Population
plot(year, population, xlab = 'Years', ylab = 'Population')
acf(population)

# Inflation
inflation <- AllData$Inflation
plot(year, inflation, xlab = 'Years', ylab = 'Inflation')
acf(inflation)

# Real Interest Rate
interest_rate <- AllData$`Interest rate`
plot(year, interest_rate, xlab = 'Years', ylab = 'Real Interest Rate')
acf(interest_rate)

# GDP
GDP <- AllData$GDP
plot(year, GDP, xlab = 'Years', ylab = 'GDP')
acf(GDP)


# since we have more predictors, we search for
# possible correlation among them

library(ellipse)
# plot correlation ellipses
plotcorr(cor(AllData))


# compute VIFs
library(car)

model <- lm(GSPC~VIX+GNI+population+inflation+interest_rate+GDP)
GSPC_vs_all <- VIF(model)
GSPC_vs_all


model <- lm(VIX~GSPC+GNI+population+inflation+interest_rate+GDP)
VIX_vs_all <- VIF(model)
VIX_vs_all


model <- lm(GNI~GSPC+VIX+population+inflation+interest_rate+GDP)
GNI_vs_all <- VIF(model)
GNI_vs_all


model <- lm(population~GSPC+VIX+GNI+inflation+interest_rate+GDP)
population_vs_all <- VIF(model)
population_vs_all


model <- lm(inflation~GSPC+VIX+GNI+population+interest_rate+GDP)
inflation_vs_all <- VIF(model)
inflation_vs_all


model <- lm(interest_rate~GSPC+VIX+GNI+population+inflation+GDP)
interest_rate_vs_all <- VIF(model)
interest_rate_vs_all


model <- lm(GDP~GSPC+VIX+GNI+population+inflation+interest_rate)
GPD_vs_all <- VIF(model)
GPD_vs_all
