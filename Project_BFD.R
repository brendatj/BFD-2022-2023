library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)

GSPC <- read_excel("Project_BFD/GSPC.xlsx")
VIX <- read_excel("Project_BFD/VIX.xlsx")
AllData <- read_excel("Project_BFD/Alldata.xlsx")

#Exploratory Data Analysis

#To find missing values
which(is.na(AllData))

plot(x= AllData$Year, y= AllData$`Population, total` )
plot(AllData$`GNI (current US$)`)  #here probably there is a problem
