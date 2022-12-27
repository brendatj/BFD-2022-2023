# Install and load the quantmod package
install.packages("quantmod")
library(quantmod)
library(base)
install.packages("fredr")
library(fredr)
install.packages("dplyr")
library(dplyr)
install.packages("xts")
library(xts)


#Here I import the data
getSymbols("^GSPC", src = "yahoo",  from = "1970-01-01")
fredr_set_key("d304734dbb4b7de7a09f270ac682a2b8")
gdp_data <- fredr("GDP",frequency = 'q', realtime_start = as.Date("2022-01-01"), realtime_end = as.Date("2022-12-16"))
vix_data <- fredr("VIXCLS", realtime_start = as.Date("2022-01-01"), realtime_end = as.Date("2022-12-16"))
#Median consumer price index is a measure of core inflation
MCPI_data <- fredr("MEDCPIM158SFRBCLE", realtime_start = as.Date("2022-01-01"), realtime_end = as.Date("2022-12-16"))
#Data on the total population
Pop_data <- fredr("POPTHM", realtime_start = as.Date("2022-01-01"), realtime_end = as.Date("2022-12-16"))
#Fed interest rates
DFF_data <- fredr("DFF", realtime_start = as.Date("2022-01-01"), realtime_end = as.Date("2022-12-16"))
#GNI data
GNI_data <- fredr("MKTGNIUSA646NWDB", realtime_start = as.Date("2022-01-01"), realtime_end = as.Date("2022-12-16"))

#Here I clean the GNI data, and normalize the data dividing by 10^9
GNI_clean <- subset(GNI_data, select = -c(series_id, realtime_start, realtime_end))
GNI_clean <- as.data.frame(GNI_clean)
colnames(GNI_clean)[c(1,2)] <- c("Date","GNI")
GNI_clean$GNI <- GNI_clean$GNI / 10^9
GNI_xts_clean <- as.xts(GNI_clean, order.by = GNI_clean$Date)

#Here I clean the Fed interest rate
DFF_clean <- subset(DFF_data, select = -c(series_id, realtime_start, realtime_end))
DFF_clean <- as.data.frame(DFF_clean)
colnames(DFF_clean)[c(1,2)] <- c("Date","FED_interest_rate")
DFF_xts_clean <- as.xts(DFF_clean, order.by = DFF_clean$Date)

#Here I clean the population data
Pop_clean <- subset(Pop_data, select = -c(series_id, realtime_start, realtime_end))
Pop_clean <- as.data.frame(Pop_clean)
colnames(Pop_clean)[c(1,2)] <- c("Date","Population")
Pop_xts_clean <- as.xts(Pop_clean, order.by = Pop_clean$Date)

#Here I clean MCPI and make them daily from montly:
MCPI_clean <- subset(MCPI_data, select = -c(series_id, realtime_start, realtime_end))
MCPI_clean <- as.data.frame(MCPI_clean)
colnames(MCPI_clean)[c(1,2)] <- c("Date","MCPI")
MCPI_xts_clean <- as.xts(MCPI_clean, order.by = MCPI_clean$Date)

#Here I clean the GSPC and transform it in a data.frame
GSPC_xts_clean <- subset(GSPC, select = -c(GSPC.Open, GSPC.Low, GSPC.High, GSPC.Adjusted ))
GSPC_df <- as.data.frame(GSPC)
GSPC_df[["Date"]] <- rownames(GSPC_df)
rownames(GSPC_df) <- NULL 
GSPC_df_clean <- subset(GSPC_df, select = -c(GSPC.Open, GSPC.Low, GSPC.High, GSPC.Adjusted ))

#Here I clean the gdp in order to merge 
gdp_data_clean <- subset(gdp_data, select = -c(realtime_start,realtime_end,series_id))
colnames(gdp_data_clean)[c(1,2)] <- c("Date","GDP")
gdp_xts_clean <- as.xts(gdp_data_clean, order.by = gdp_data_clean$Date)


#Here I clean the VIX
vix_data_clean <- subset(vix_data, select = -c(realtime_start, realtime_end, series_id))
colnames(vix_data_clean)[c(1,2)] <- c("Date","VIX")
vix_data_clean <- as.data.frame(vix_data_clean)
vix_xts_clean <- as.xts(vix_data_clean, order.by = vix_data_clean$Date)


#Merging stuff
merged_dataset <- merge(GSPC_xts_clean, vix_xts_clean)
merged_dataset <- merge(merged_dataset, gdp_xts_clean)
merged_dataset <- merge(merged_dataset, MCPI_xts_clean)
merged_dataset <- merge(merged_dataset, Pop_xts_clean)
merged_dataset <- merge(merged_dataset, DFF_xts_clean)
merged_dataset <- merge(merged_dataset, GNI_xts_clean)


merged_df <- as.data.frame(merged_dataset)


#Function to fill the dataset of quarterly data with the nearest values
fill_na_with_nearest <- function(data, col) {
  # Find the indices of the NA values in the specified column
  na_indices <- which(is.na(data[, col]))
  
  # Loop through the NA indices and fill them with the nearest available value
  for (i in na_indices) {
    # Skip the NA value if it is at the first row of the xts object
    if (i == 1) {
      next
    }
    
    # Find the nearest available value before the current index
    before_val <- data[(i - 1):1, col][!is.na(data[(i - 1):1, col])][1]
    
    # Find the nearest available value after the current index
    after_val <- data[(i + 1):nrow(data), col][!is.na(data[(i + 1):nrow(data), col])][1]
    
    # If both before and after values are available, take the average
    if (!is.na(before_val) && !is.na(after_val)) {
      data[i, col] <- (before_val + after_val) / 2
    }
    # If only the before value is available, use it
    else if (!is.na(before_val)) {
      data[i, col] <- before_val
    }
    # If only the after value is available, use it
    else if (!is.na(after_val)) {
      data[i, col] <- after_val
    }
  }
  
  # Return the modified data
  return(data)
}


merged_df <- fill_na_with_nearest(merged_df, "GDP")
merged_df <- fill_na_with_nearest(merged_df, "MCPI")
merged_df <- fill_na_with_nearest(merged_df, "Population")
merged_df <- fill_na_with_nearest(merged_df, "GNI")

#Cleaning the merged dataframe

df_clean <- subset(merged_df, select = -c(Date, Date.1, Date.2, Date.3, Date.4, Date.5))

#Here I reset the indices and create a new column Date

df_clean[["Date"]] <- rownames(df_clean)
rownames(df_clean) <- NULL

#Removing NA rows

df <- na.omit(df_clean)
rownames(df) <- NULL

df$Date <- as.Date(df$Date, format = "X%Y.%m.%d")

#Writing the data in an excel file
# First, install and load the openxlsx package
#install.packages("openxlsx")
library(openxlsx)
write.xlsx(df, file = "df.xlsx", sheetName = "Sheet1", rowNames = TRUE, colNames = TRUE)
