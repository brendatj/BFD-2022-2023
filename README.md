# BFD-2022-2023
Business and financial Data Guidolin
---
title: "Business Economic and Financial Data Project"
author: "Brenda Eloísa Téllez Juárez"
date: "2022-11-27"
output:
  powerpoint_presentation: default
  slidy_presentation: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```
##Setting the working directory
```{r}
bren<-setwd("~/UNIPD/Business, economics and financial data")
```

## Packages
```{r}
#Dataset
library(readxl)
#Create the pdf
library(knitr)
library(markdown)
library(commonmark)
#missing values
library(naniar)
#Visualizations
library(ggplot2)
library(tidyverse)
library(plotly)
#Inspecting the dataframe
library(inspectdf)
#Time series
library(fpp2)
library(forecast)
library(lmtest)
#Mine
library(ts.extend)
library(tseries)

set.seed(50)
```



#Dataset
```{r}
database1 <- read_excel("database1.xlsx")
data<-database1
attach(data)
```
#Dataset
```{r}
head(data, n=5)
tail(data, n=5)
```


#Understanding the data
```{r}
summary(data)
str(data)
sapply(data, class)
```
```{r}
print(start(GSPC))
print(end(GSPC))
print(frequency(GSPC))
print(deltat(GSPC))
print(cycle(GSPC))
```

#Data engineering

##Searching for null values
```{r}
sapply(data, function(x) round((sum(is.na(x))/length(x))*100,2))
#Visualizing them
gg_miss_var(data)
```

#Data transformations

##Renaming columns
```{r}
colnames(data) <- c('Year', "VIX",'VIX returns',  "Inflation",'Population', "Real interest rate", "GDP", "GDP per capita", "GNI",'GSPC returns', "GSPC")
columns <- colnames(data)
```

##Copy of the original dataframe
```{r}
data2 <- data.frame(data)
```

#Data engineering

##Null values
```{r}
data <-replace(data, is.na(data), 0)
```

##Checking null values
```{r}
sapply(data, function(x) round((sum(is.na(x))/length(x))*100,2))
```
##Visualizing them
```{r}
gg_miss_var(data)
```
#Exploratory Data Analysis

##Dimension of the dataset
```{r}
dim(data)
```
##Exploring the dataset
```{r}
head(data)
tail(data)
```
#Univariate Analysis

##Visualizing distributions
```{r}
dens_vix <- density(log(data$VIX))
 
plot_ly(
  data = data,
  x = ~log(VIX),
  type = "histogram",
  name = "Histogram") %>% 
  add_lines(x = dens_vix$x, y = dens_vix$y, yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                       side = "right", #Adds the density axis on the right side
                       rangemode = "tozero")) #Forces both y-axes to start at 0

```
```{r}
dens_gspc <- density(log(data$GSPC))
 
plot_ly(
  data = data,
  x = ~log(GSPC),
  type = "histogram",
  name = "Histogram") %>% 
  add_lines(x = dens_gspc$x, y = dens_gspc$y, yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                       side = "right", #Adds the density axis on the right side
                       rangemode = "tozero")) #Forces both y-axes to start at 0
```
```{r}
dens_gdp <- density(log(data$GDP))
 
plot_ly(
  data = data,
  x = ~log(GDP),
  type = "histogram",
  name = "Histogram") %>% 
  add_lines(x = dens_gdp$x, y = dens_gdp$y, yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", #Adds the dual y-axis
                       side = "right", #Adds the density axis on the right side
                       rangemode = "tozero")) #Forces both y-axes to start at 0
```

#Bivariate Analysis

##Analizing the relationship between VIX and GSPC
```{r}
data %>% 
  ggplot() + 
  aes(x = VIX, y = GSPC) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple') 
```
```{r}
ggplot(data, 
       aes(x = VIX, 
           y = GSPC)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "purple")
```
##Analizing the relationship between the returns of VIX and GSPC
```{r}
data %>% 
  ggplot() + 
  aes(x = `VIX returns`, y = `GSPC returns`) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple', formula = y ~ poly(x, 2)) 
```


##Analizing the relationship between GDP and GSPC
```{r}
data %>% 
  ggplot() + 
  aes(x = GDP, y = GSPC) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple') 
```
```{r}
data %>% 
  ggplot() + 
  aes(x = VIX, y = GSPC) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple',formula=y ~ poly(x, 2)) 
```
##Analizing the relationship between the returns of GDP and GSPC
```{r}
data %>% 
  ggplot() + 
  aes(x = `VIX returns`, y = `GSPC returns`) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple',formula=y ~ poly(x, 2)) 
```


##Analizing the relationship between VIX and GDP
```{r}
data %>% 
  ggplot() + 
  aes(x = VIX, y = GDP) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple') 
```
```{r}
data %>% 
  ggplot() + 
  aes(x = VIX, y = GDP) + 
  geom_point(color= "steelblue") +
  geom_smooth(method = 'lm', color = 'purple', formula=y ~ poly(x, 2)) 
```


#Time series analysis: Linear regression and Arima models.

##Plot GSPC
```{r}
x <- year
y <- GSPC
data_graph1 <- data.frame(x, y)

fig <- plot_ly(data_graph1, x = ~x, y = ~y, type = 'scatter', mode = 'lines')

fig
```
##Plot GSPC returns
```{r}
x <- year
y <- `GSPC returns`
data_graph1 <- data.frame(x, y)

fig <- plot_ly(data_graph1, x = ~x, y = ~y, type = 'scatter', mode = 'lines')

fig
```

#AFC GSPC
```{r}
Acf_GSPC<-ggAcf(GSPC, col='blue')
Acf_GSPC+ ggtitle("Autocorrelation Function ACF") + xlab("Lag") + ylab("ACF")
```
#PACF GSPC
```{r}
Pacf_GSPC<-ggAcf(GSPC, col='blue')
Pacf_GSPC+ ggtitle("Partial Autocorrelation Function PACF GSPC") + xlab("Lag") + ylab("PACF")
```
##Plot VIX
```{r}
x <- year
y <- VIX
data_graph2 <- data.frame(x, y)

fig <- plot_ly(data_graph2, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
fig
```
##Plot VIX returns
```{r}
x <- year
y <- `VIX returns`
data_graph2 <- data.frame(x, y)

fig <- plot_ly(data_graph2, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
fig
```

##AFC VIX
```{r}
Acf_GSPC<-ggAcf(VIX, col='blue')
Acf_GSPC+ ggtitle("Autocorrelation Function VIX") + xlab("Lag") + ylab("ACF")
```
##PACF VIX
```{r}
Pacf_GSPC<-ggAcf(VIX, col='blue')
Pacf_GSPC+ ggtitle("Partial Autocorrelation Function PACF VIX") + xlab("Lag") + ylab("PACF")
```
##Plot GDP
```{r}
x <- year
y <- GDP
data_graph3 <- data.frame(x, y)

fig <- plot_ly(data_graph3, x = ~x, y = ~y, type = 'scatter', mode = 'lines')

fig
```

##AFC GDP
```{r}
Acf_GSPC<-ggAcf(GDP, col='blue')
Acf_GSPC+ ggtitle("Autocorrelation Function GDP") + xlab("Lag") + ylab("ACF")
```

##PACF GDP

```{r}
Pacf_GSPC<-ggAcf(GDP, col='blue')
Pacf_GSPC+ ggtitle("Partial Autocorrelation Function PACF GDP") + xlab("Lag") + ylab("PACF")
```

#Time Series

```{r}
#Creating the time series
ts1_GSPC<-ts(GSPC, start = 1990, end=2021, frequency = 1)
ts2_VIX<-ts(VIX, start = 1990, end=2021, frequency = 1)
ts3_GDP<-ts(GDP, start = 1990, end=2021, frequency = 1)
is.ts(ts1_GSPC)
```

```{r}
#Returns
ts1_GSPC_returns<-ts(`GSPC returns`, start = 1990, end=2021, frequency = 1)
ts2_VIX_returns<-ts(`VIX returns`, start = 1990, end=2021, frequency = 1)
```

#Transforming the time series with log and differenciating them
##Aplying the log transformation

```{r}
#Applying log transformations to try to stabilize the variance
ts1_GSPC_log<-log(ts1_GSPC)
par(mfrow=c(1,2))
ts.plot(ts1_GSPC_log)
ts.plot(ts1_GSPC)
```

```{r}
#Applying log transformations
ts2_VIX_log<-log(ts2_VIX)
par(mfrow=c(1,2))
ts.plot(ts2_VIX_log)
ts.plot(ts2_VIX)
```
##Aplying the diff transformation

```{r}
#Applying diff transformations to eliminate the linear trend
ts1_GSPC_diff<-diff(ts1_GSPC)
par(mfrow=c(1,2))
ts.plot(ts1_GSPC_diff)
ts.plot(ts1_GSPC)
```

```{r}
#Applying diff transformations
ts2_VIX_diff<-diff(ts2_VIX)
par(mfrow=c(1,2))
ts.plot(ts2_VIX_diff)
ts.plot(ts2_VIX)
```


##Aplying the diff seasonal transformation

```{r}
#Applying diff transformations to eliminate the linear trend and seasonality
ts1_GSPC_diff_s<-diff(ts1_GSPC, s=2)
par(mfrow=c(1,2))
ts.plot(ts1_GSPC_diff_s)
ts.plot(ts1_GSPC_diff)
ts.plot(ts1_GSPC)
```
```{r}
#Applying diff seasonality transformations
ts2_VIX_diff_s<-diff(ts2_VIX, lag=12)
par(mfrow=c(1,2))
ts.plot(ts2_VIX_diff_s)
ts.plot(ts2_VIX_diff)
ts.plot(ts2_VIX)
```

```{r}
#Exploring the length
length(ts1_GSPC)
length(ts1_GSPC_diff)
length(ts2_VIX)
length(ts2_VIX_diff)
```

#Estimating the white noise


```{r}
#Estimating white noise for GSPC
arima(ts1_GSPC, order=c(0,0,0))
```
```{r}
#Estimating white noise for VIX
arima(ts2_VIX, order=c(0,0,0))
```


```{r}
#Applying differentiated model GSPC
model_wn_gspc<-arima(ts1_GSPC_diff, order=c(0,0,0))
int_wn_gspc <- model_wn_gspc$coef
```

```{r}
ts.plot(ts1_GSPC)
abline(0,int_wn_gspc)
```

```{r}
#Applying differentiated model VIX
model_wn_vix<-arima(ts2_VIX_diff, order=c(0,0,0))
int_wn_vix<- model_wn_vix$coef
```

```{r}
ts.plot(ts2_VIX)
abline(0,int_wn_vix)
```

##Plotting the two indexes together

```{r}
ts.plot(cbind(ts1_GSPC, ts2_VIX))
```

```{r}
#Log returns for both indexes
plot(cbind(ts1_GSPC_returns, ts2_VIX_returns))
```

```{r}

```



