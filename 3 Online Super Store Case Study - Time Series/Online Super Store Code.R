################ Including libraries ################################################
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forecast)
library(tseries)
library(graphics)

################ Cleaning env and importing data ###################################
## Cleaning environment
rm(list = ls())

# setting working directory
setwd(
  "C:/Users/kkannan/Box Sync/PGDDA/TimeSeries/CaseStudy"
)

# importing data file
onlineRetail <-
  read.csv("Global Superstore.csv", stringsAsFactors = TRUE)

dim(onlineRetail)  # 51290*24

################ EDA ###############################################################
table(onlineRetail$Market) # 7 levels
# Africa   APAC Canada   EMEA     EU  LATAM     US
# 4587  11002    384   5029  10000  10294   9994

table(onlineRetail$Segment) # 3 levels
# Consumer   Corporate Home Office
# 26518       15429        9343

sum(is.na(onlineRetail))

sapply(onlineRetail, function(x) {
  sum(is.na(x))
})
# Only Postal.Code has null values and as we donot require this column
# so it can be neglected
sum(duplicated(onlineRetail)) # 0

## Creating graph between Market and Profit 
onlineRetail %>%
  group_by(Market) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales)
  ) %>%
  ggplot(aes(x = Market, y = NetProfit, fill = Market)) + 
  geom_bar(stat = "identity")+
  theme(legend.position = "none")

## Creating graph between Segment and Profit Percentage
onlineRetail %>%
  group_by(Segment) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales)
  ) %>%
  ggplot(aes(x = Segment, y = NetProfit, fill = Segment)) +
  geom_bar(stat = "identity")+
  theme(legend.position = "none")

## Creating graph between Market, Segment and Profit Percentage
onlineRetail %>%
  group_by(Market, Segment) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales)
  ) %>%
  ggplot(aes(x = Market, y = NetProfit, fill = Segment)) + geom_bar(stat = "identity")

################ Data Preparation ##################################################
## Separating month and year data for all the rows in
##onlineRetail dataframe based on order data
onlineRetail$ordMonth <-
  as.Date(onlineRetail$Order.Date, format = "%d-%m-%Y") %>% month()
onlineRetail$ordYear <-
  as.Date(onlineRetail$Order.Date, format = "%d-%m-%Y") %>% year()

### Creating separate datasets for each of the 21 segments
dataAfricaCon <-
  onlineRetail %>%
  filter(Market == "Africa" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataAfricaCon$months <- seq(1:48)

dataAfricaCorp <-
  onlineRetail %>%
  filter(Market == "Africa" & Segment == "Corporate") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataAfricaCorp$months <- seq(1:48)

dataAfricaHO <-
  onlineRetail %>%
  filter(Market == "Africa" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataAfricaHO$months <- seq(1:48)

dataAPACCon <-
  onlineRetail %>%
  filter(Market == "APAC" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataAPACCon$months <- seq(1:48)

dataAPACCorp <-
  onlineRetail %>%
  filter(Market == "APAC" & Segment == "Corporate")  %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataAPACCorp$months <- seq(1:48)

dataAPACHO <-
  onlineRetail %>%
  filter(Market == "APAC" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataAPACHO$months <- seq(1:48)

dataCanadaCon1 <-
  onlineRetail %>%
  filter(Market == "Canada" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
## dataCanadaCon1 have lesser number of rows than 48, so other
## rows needs to be added with 0 value
dataCanadaCon1$monYear <-
  str_c(dataCanadaCon1$ordMonth, "_", dataCanadaCon1$ordYear)
dataCanadaCon <-
  dataAfricaCon[, which(names(dataAfricaCon) %in% c("ordMonth", "ordYear", "months"))]
dataCanadaCon$monYear <-
  str_c(dataCanadaCon$ordMonth, "_", dataCanadaCon$ordYear)
dataCanadaCon <-
  merge(dataCanadaCon,
        dataCanadaCon1,
        by = "monYear",
        all.x = TRUE) %>% arrange(months)
dataCanadaCon <-
  dataCanadaCon[, -which(names(dataCanadaCon) %in% c("ordMonth.x", "ordMonth.y", "ordYear.x", "ordYear.y"))]
dataCanadaCon$NetProfit[is.na(dataCanadaCon$NetProfit)] <-
  0 ## Imputing 0 to missing values
dataCanadaCon$NetSales[is.na(dataCanadaCon$NetSales)] <-
  0 ## Imputing 0 to missing values
dataCanadaCon$NetQty[is.na(dataCanadaCon$NetQty)] <-
  0 ## Imputing 0 to missing values

dataCanadaCorp1 <-
  onlineRetail %>%
  filter(Market == "Canada" & Segment == "Corporate") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
## dataCanadaCorp1 have lesser number of rows than 48, so other
## rows needs to be added with 0 value
dataCanadaCorp1$monYear <-
  str_c(dataCanadaCorp1$ordMonth, "_", dataCanadaCorp1$ordYear)
dataCanadaCorp <-
  dataAfricaCon[, which(names(dataAfricaCon) %in% c("ordMonth", "ordYear", "months"))]
dataCanadaCorp$monYear <-
  str_c(dataCanadaCorp$ordMonth, "_", dataCanadaCorp$ordYear)
dataCanadaCorp <-
  merge(dataCanadaCorp,
        dataCanadaCorp1,
        by = "monYear",
        all.x = TRUE) %>% arrange(months)
dataCanadaCorp <-
  dataCanadaCorp[, -which(names(dataCanadaCorp) %in% c("ordMonth.x", "ordMonth.y", "ordYear.x", "ordYear.y"))]
dataCanadaCorp$NetProfit[is.na(dataCanadaCorp$NetProfit)] <-
  0 ## Imputing 0 to missing values
dataCanadaCorp$NetSales[is.na(dataCanadaCorp$NetSales)] <-
  0 ## Imputing 0 to missing values
dataCanadaCorp$NetQty[is.na(dataCanadaCorp$NetQty)] <-
  0 ## Imputing 0 to missing values

dataCanadaHO1 <-
  onlineRetail %>%
  filter(Market == "Canada" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
## dataCanadaHO1 have lesser number of rows than 48, so other
## rows needs to be added with 0 value
dataCanadaHO1$monYear <-
  str_c(dataCanadaHO1$ordMonth, "_", dataCanadaHO1$ordYear)
dataCanadaHO <-
  dataAfricaCon[, which(names(dataAfricaCon) %in% c("ordMonth", "ordYear", "months"))]
dataCanadaHO$monYear <-
  str_c(dataCanadaHO$ordMonth, "_", dataCanadaHO$ordYear)
dataCanadaHO <-
  merge(dataCanadaHO, dataCanadaHO1, by = "monYear", all.x = TRUE) %>% arrange(months)
dataCanadaHO <-
  dataCanadaHO[, -which(names(dataCanadaHO) %in% c("ordMonth.x", "ordMonth.y", "ordYear.x", "ordYear.y"))]
dataCanadaHO$NetProfit[is.na(dataCanadaHO$NetProfit)] <-
  0 ## Imputing 0 to missing values
dataCanadaHO$NetSales[is.na(dataCanadaHO$NetSales)] <-
  0 ## Imputing 0 to missing values
dataCanadaHO$NetQty[is.na(dataCanadaHO$NetQty)] <-
  0 ## Imputing 0 to missing values

dataEMEACon <-
  onlineRetail %>%
  filter(Market == "EMEA" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataEMEACon$months <- seq(1:48)

dataEMEACorp <-
  onlineRetail %>%
  filter(Market == "EMEA" & Segment == "Corporate") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataEMEACorp$months <- seq(1:48)

dataEMEAHO <-
  onlineRetail %>%
  filter(Market == "EMEA" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataEMEAHO$months <- seq(1:48)

dataEUCon <-
  onlineRetail %>%
  filter(Market == "EU" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataEUCon$months <- seq(1:48)

dataEUCorp <-
  onlineRetail %>%
  filter(Market == "EU" & Segment == "Corporate") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataEUCorp$months <- seq(1:48)

dataEUHO <-
  onlineRetail %>%
  filter(Market == "EU" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataEUHO$months <- seq(1:48)

dataLATAMCon <-
  onlineRetail %>%
  filter(Market == "LATAM" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataLATAMCon$months <- seq(1:48)

dataLATAMCorp <-
  onlineRetail %>%
  filter(Market == "LATAM" & Segment == "Corporate") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataLATAMCorp$months <- seq(1:48)

dataLATAMHO <-
  onlineRetail %>%
  filter(Market == "LATAM" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataLATAMHO$months <- seq(1:48)

dataUSCon <-
  onlineRetail %>%
  filter(Market == "US" & Segment == "Consumer") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataUSCon$months <- seq(1:48)

dataUSCorp <-
  onlineRetail %>%
  filter(Market == "US" & Segment == "Corporate") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataUSCorp$months <- seq(1:48)

dataUSHO <-
  onlineRetail %>%
  filter(Market == "US" & Segment == "Home Office") %>%
  group_by(ordMonth, ordYear) %>%
  summarise(
    NetProfit = sum(Profit),
    NetSales = sum(Sales),
    NetQty = sum(Quantity)
  ) %>%
  arrange(ordYear, ordMonth)
dataUSHO$months <- seq(1:48)

################ Aggregating data for profit from all 21 segments###########
datasets <-
  list(
    dataAfricaCon,
    dataAfricaCorp,
    dataAfricaHO,
    dataAPACCon,
    dataAPACCorp,
    dataAPACHO,
    dataCanadaCon,
    dataCanadaCorp,
    dataCanadaHO,
    dataEMEACon,
    dataEMEACorp,
    dataEMEAHO,
    dataEUCon,
    dataEUCorp,
    dataEUHO,
    dataLATAMCon,
    dataLATAMCorp,
    dataLATAMHO,
    dataUSCon,
    dataUSCorp,
    dataUSHO
  )

profitDS <- as.data.frame(dataAfricaCon$months)
names(profitDS) <- "months"

for (i in datasets) {
  profitDS <- merge(profitDS,
                    i[, which(names(i) %in% c("months", "NetProfit"))],
                    by.x = "months",
                    by.y = "months",
                    all.x = TRUE)
}

names(profitDS) <- c(
  "months",
  "dataAfricaCon",
  "dataAfricaCorp",
  "dataAfricaHO",
  "dataAPACCon",
  "dataAPACCorp",
  "dataAPACHO",
  "dataCanadaCon",
  "dataCanadaCorp",
  "dataCanadaHO",
  "dataEMEACon",
  "dataEMEACorp",
  "dataEMEAHO",
  "dataEUCon",
  "dataEUCorp",
  "dataEUHO",
  "dataLATAMCon",
  "dataLATAMCorp",
  "dataLATAMHO",
  "dataUSCon",
  "dataUSCorp",
  "dataUSHO"
)

################ Finding top 2 segements based on Profit data #############
sapply(profitDS, sd) / sapply(profitDS, mean)
## dataAPACCon, dataEUCon has lowest coefficient of variation for Profit

sapply(profitDS, mean)
## dataAPACCon, dataEUCon has highest average for the Profit

profitComparisonDF <- data.frame(segment = names(profitDS))
profitComparisonDF$ProfitSum <- sapply(profitDS, sum)
profitComparisonDF$COVProfit <-
  sapply(profitDS, sd) / sapply(profitDS, mean)

profitComparisonDF <-
  profitComparisonDF[-1,] %>% arrange(COVProfit)
# segment AvgProfit COVProfit
# 1   dataEUCon  3930.994 0.6243052
# 2 dataAPACCon  4642.033 0.6321323

profitComparisonDF %>% head(2)
# Removing months and then checking top2

ggplot(profitComparisonDF,
       aes(x = segment, y = COVProfit, fill = segment)) +
  geom_col() +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) + scale_y_continuous(breaks = seq(.5, 6, .2))+
  theme(legend.position = "none")

ggplot(profitComparisonDF, aes(x = segment, y = ProfitSum, fill = segment)) +
  geom_col() +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) + scale_y_continuous(breaks = seq(0, 5000, 400))+
  theme(legend.position = "none")

## dataAPACCon, dataEUCon are being chosen as they have highes profit as
# well as lowest coefficient of variation

################ Model Buidling ############################################
###################################################################################
############## For APAC Consumers on Sales Time Series ##############################
salesAPACts <- ts(dataAPACCon$NetSales)
plot(salesAPACts)

#Smoothing the series - Moving Average Smoothing
w <- 1
smoothedAPACSalesSeries <- stats::filter(
  salesAPACts[1:42],
  filter = rep(1 / (2 * w + 1), (2 * w + 1)),
  method = 'convolution',
  sides = 2
)

#Smoothing left end of the time series
diff <-
  smoothedAPACSalesSeries[w + 2] - smoothedAPACSalesSeries[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedAPACSalesSeries[i] <- smoothedAPACSalesSeries[i + 1] - diff
}

#Smoothing right end of the time series
n <- length(salesAPACts[1:42])
diff <-
  smoothedAPACSalesSeries[n - w] - smoothedAPACSalesSeries[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedAPACSalesSeries[i] <- smoothedAPACSalesSeries[i - 1] + diff
}

lines(smoothedAPACSalesSeries, col = "red", lwd = 2)

################ Creating prediction model for APAC Sales TS ####
smoothedAPACSalesDF <-
  as.data.frame(cbind(dataAPACCon$months[1:42], as.vector(smoothedAPACSalesSeries)))
colnames(smoothedAPACSalesDF) <- c('months', 'NetSales')

lmAPACSalesFit <-
  lm(NetSales ~ sin(0.5 * months) + poly(months, 1),
     data = smoothedAPACSalesDF)
summary(lmAPACSalesFit)

globalPredAPACSalesVal <-
  predict(lmAPACSalesFit, data.frame(months = seq(1:42)))
summary(globalPredAPACSalesVal)
lines(globalPredAPACSalesVal, col = 'blue', lwd = 2)

################ Handling local part of time series for APAC Sales TS ####
localTSalesAPAC <- salesAPACts[1:42] - globalPredAPACSalesVal
plot(localTSalesAPAC, type = "l")
acf(localTSalesAPAC)
acf(localTSalesAPAC, type = "partial")
localTsalesAPACArima <- auto.arima(localTSalesAPAC)

tsdiag(localTsalesAPACArima)
localTsalesAPACArima
# Remaining series is having 0 mean, so checking if its white noise
adf.test(localTSalesAPAC)
kpss.test(localTSalesAPAC)

################ Predicting value for future months for APAC Sales TS #####
testDataAPACSales <- dataAPACCon[43:54, ]
testDataAPACSales$predictedSales <-
  predict(lmAPACSalesFit, data.frame(months = c(43:54)))

MAPE_auto_arima_APACSales1 <-
  accuracy(testDataAPACSales$predictedSales[1:6],
           testDataAPACSales$NetSales[1:6])[5]
MAPE_auto_arima_APACSales1  #21.21971

cdPredAPACSales <-
  c(globalPredAPACSalesVal,
    ts(testDataAPACSales$predictedSales))
plot(salesAPACts, xlim = c(1,54))
abline(v = 48)
lines(cdPredAPACSales, col = "red")

################ Applying auto arima on APAC Sales TS #####################
salesAPACArima <- auto.arima(salesAPACts[1:42])

tsdiag(salesAPACArima)
salesAPACArima
testDataAPACSales$predictedSales2 <-
  (predict(salesAPACArima, n.ahead = 12))$pred

resiAPACSalesArima <- salesAPACts[1:42] - fitted(salesAPACArima)

adf.test(resiAPACSalesArima, alternative = "stationary")
kpss.test(resiAPACSalesArima)

MAPE_auto_arima_APACSales2 <-
  accuracy(testDataAPACSales$predictedSales2[1:6],
           testDataAPACSales$NetSales[1:6])[5]
MAPE_auto_arima_APACSales2  #27.68952

################ Plotting lines for APAC sales TS #######################
arimaPredAPACSales <-
  c(fitted(salesAPACArima),
    ts(testDataAPACSales$predictedSales2))

labels <-
  c("Raw",
    "Smoothed",
    "Classical Decomposition Pred",
    "Auto Arima Pred")
cols <- c("black", "green", "red", "blue")
plot(ts(dataAPACCon$NetSales), xlim = c(1,54))
abline(v = 48)
lines(smoothedAPACSalesSeries, col = "green", lwd = 2)
lines(cdPredAPACSales, col = "red" , lwd = 2)
lines(arimaPredAPACSales, col = "blue" , lwd = 2)
legend("topleft", labels, col = cols, lwd = 2)

###################################################################################
################ For APAC Consumers on Quantity Time Series ##############################
qtyAPACts <- ts(dataAPACCon$NetQty)
plot(qtyAPACts)

#Smoothing the series - Moving Average Smoothing
w <- 1
smoothedAPACQtyseries <- stats::filter(
  qtyAPACts[1:42],
  filter = rep(1 / (2 * w + 1), (2 * w + 1)),
  method = 'convolution',
  sides = 2
)

#Smoothing left end of the time series
diff <- smoothedAPACQtyseries[w + 2] - smoothedAPACQtyseries[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedAPACQtyseries[i] <- smoothedAPACQtyseries[i + 1] - diff
}

#Smoothing right end of the time series
n <- length(qtyAPACts[1:42])
diff <-
  smoothedAPACQtyseries[n - w] - smoothedAPACQtyseries[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedAPACQtyseries[i] <- smoothedAPACQtyseries[i - 1] + diff
}

lines(smoothedAPACQtyseries, col = "red", lwd = 2)

################ Creating prediction model for APAC Qty TS #########
smoothedAPACConQtydf <-
  as.data.frame(cbind(dataAPACCon$months[1:42], as.vector(smoothedAPACQtyseries)))
colnames(smoothedAPACConQtydf) <- c('months', 'NetQty')

lmAPACQtyFit <-
  lm(NetQty ~ sin(0.5 * months) + poly(months, 1) +
       cos(0.5 * months) * poly(months, 1),
     data = smoothedAPACConQtydf)
summary(lmAPACQtyFit)

globalPredAPACQtyVal <-
  predict(lmAPACQtyFit, data.frame(months = seq(1:42)))
summary(globalPredAPACQtyVal)
lines(globalPredAPACQtyVal, col = 'blue', lwd = 2)

################ Handling local part of time series for APAC Qty TS #####
localTAPACQty <- qtyAPACts[1:42] - globalPredAPACQtyVal
plot(localTAPACQty, col = 'red', type = "l")
acf(localTAPACQty)
acf(localTAPACQty, type = "partial")
localTqtyAPACArima <- auto.arima(localTAPACQty)
tsdiag(localTqtyAPACArima)
localTqtyAPACArima
# Remaining series is having 0 mean, so checking if its white noise

adf.test(localTAPACQty, alternative = "stationary")
kpss.test(localTAPACQty)

################ Predicting value for future months for APAC Qty TS ####
testDataAPACQty <- dataAPACCon[43:54, ]
testDataAPACQty$predictedQty <-
  predict(lmAPACQtyFit, data.frame(months = c(43:54)))

MAPE_auto_arima_APACQty1 <-
  accuracy(testDataAPACQty$predictedQty[1:6], dataAPACCon$NetQty[43:48])[5]
MAPE_auto_arima_APACQty1 #27.03118

cdPredAPACQty <-
  c(globalPredAPACQtyVal, ts(testDataAPACQty$predictedQty))
plot(ts(dataAPACCon$NetQty),  xlim = c(1,54))
abline(v =48)
lines(cdPredAPACQty, col = "red", type = "l")

################ Applying auto arima on APAC Qty TS #####################
qtyAPACArima <- auto.arima(qtyAPACts[1:42])
tsdiag(qtyAPACArima)
qtyAPACArima
testDataAPACQty$predictedQty2 <-
  (predict(qtyAPACArima, n.ahead = 12))$pred

resiAPACSalesArima <- qtyAPACts[43:48] - fitted(qtyAPACArima)

adf.test(resiAPACSalesArima, alternative = "stationary")
kpss.test(resiAPACSalesArima)

MAPE_auto_arima_APACQty2 <-
  accuracy(testDataAPACQty$predictedQty2[1:6], testDataAPACQty$NetQty[1:6])[5]
MAPE_auto_arima_APACQty2  #26.24458

arimaPredAPACQty <-
  c(fitted(qtyAPACArima), ts(testDataAPACQty$predictedQty2))

################ Plotting lines for APAC Qty TS #######################
labels <-
  c("Raw",
    "Smoothed",
    "Classical Decomposition Pred",
    "Auto Arima Pred")
cols <- c("black", "green", "red", "blue")
plot(ts(dataAPACCon$NetQty),  xlim = c(1,54))
abline(v = 48)
lines(smoothedAPACQtyseries, col = "green", lwd = 2)
lines(cdPredAPACQty, col = "red" , lwd = 2)
lines(arimaPredAPACQty, col = "blue" , lwd = 2)
legend("topleft", labels, col = cols, lwd = 2)

##########################################################################
################ For EU Consumers on Sales Time Series ##############################
salesEUts <- ts(dataEUCon$NetSales)
plot(salesEUts[1:42], type = "l")

#Smoothing the series - Moving Average Smoothing #
w <- 1
smoothedEUSalesSeries <- stats::filter(
  salesEUts[1:42],
  filter = rep(1 / (2 * w + 1), (2 * w + 1)),
  method = 'convolution',
  sides = 2
)

#Smoothing left end of the time series
diff <-
  smoothedEUSalesSeries[w + 2] - smoothedEUSalesSeries[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedEUSalesSeries[i] <- smoothedEUSalesSeries[i + 1] - diff
}

#Smoothing right end of the time series
n <- length(salesEUts[1:42])
diff <-
  smoothedEUSalesSeries[n - w] - smoothedEUSalesSeries[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedEUSalesSeries[i] <- smoothedEUSalesSeries[i - 1] + diff
}

lines(smoothedEUSalesSeries, col = "red", lwd = 2)

################ Creating prediction model for APAC Sales TS ########
smoothedEUSalesDF <-
  as.data.frame(cbind(dataEUCon$months[1:42], smoothedEUSalesSeries))
colnames(smoothedEUSalesDF) <- c('months', 'NetSales')

lmEUSalesFit <-
  lm(NetSales ~ sin(.5 * months) + poly(months, 2) +
       cos(0.5 * months),
     data = smoothedEUSalesDF)
summary(lmEUSalesFit)

globalPredEUSalesVal <- predict(lmEUSalesFit, months = seq(1:42))
summary(globalPredEUSalesVal)
lines(globalPredEUSalesVal, col = 'blue', lwd = 2)

################ Handling local part of time series for EU Sales #######
localTEUSales <- salesEUts[1:42] - globalPredEUSalesVal
plot(localTEUSales, col = 'red', type = "l")
acf(localTEUSales)
acf(localTEUSales, type = "partial")
localTARMAEUSalesFit <- auto.arima(localTEUSales)
tsdiag(localTARMAEUSalesFit)
localTARMAEUSalesFit

adf.test(localTEUSales, alternative = "stationary")
kpss.test(localTEUSales)

################ Predicting value for future months for EU Sales TS ####
testDataEUSales <- dataEUCon[43:54, ]
testDataEUSales$predictedSales <-
  predict(lmEUSalesFit, data.frame(months = c(43:54)))

MAPE_auto_arima_EUSales1 <-
  accuracy(testDataEUSales$predictedSales[1:6], dataAPACCon$NetSales[43:48])[5]
MAPE_auto_arima_EUSales1 #28.31765

cdPredEUSales <-
  c(globalPredEUSalesVal, ts(testDataEUSales$predictedSales))
plot(salesEUts, xlim = c(1,54))
abline(v=48)
lines(cdPredEUSales, col = "red")

################ Applying auto arima on EU Sales TS #####################
salesEUArima <- auto.arima(salesEUts[1:42])

tsdiag(salesEUArima)
salesEUArima
testDataEUSales$predictedSales2 <-
  (predict(salesEUArima, n.ahead = 12))$pred

resiEUSalesArima <- salesEUts[1:42] - fitted(salesEUArima)

adf.test(resiEUSalesArima, alternative = "stationary")
kpss.test(resiEUSalesArima)

MAPE_auto_arima_EUSales2 <-
  accuracy(testDataEUSales$predictedSales2[1:6], testDataEUSales$NetSales[1:6])[5]
MAPE_auto_arima_EUSales2  #28.9226

################ Plotting lines for EU Sales TS #######################
arimaPredEUSales <-
  c(fitted(salesEUArima), ts(testDataEUSales$predictedSales2))
labels <-
  c("Raw",
    "Smoothed",
    "Classical Decomposition Pred",
    "Auto Arima Pred")
cols <- c("black", "green", "red", "blue")
plot(ts(dataEUCon$NetSales), xlim = c(1,54))
abline(v = 48)
lines(smoothedEUSalesSeries, col = "green", lwd = 2)
lines(cdPredEUSales, col = "red" , lwd = 2)
lines(arimaPredEUSales, col = "blue" , lwd = 2)
legend("topleft", labels, col = cols, lwd = 2)

##########################################################################
################ For EU Consumers on Qty Time Series ##############################
qtyEUts <- ts(dataEUCon$NetQty)
plot(qtyEUts)
##### Smoothing the series - Moving Average Smoothing
w <- 1
smoothedEUQtyseries <- stats::filter(qtyEUts[1:42],
                                     filter = rep(1 / (2 * w + 1), (2 * w + 1)),
                                     method = 'convolution',
                                     sides = 2)

#Smoothing left end of the time series

diff <- smoothedEUQtyseries[w + 2] - smoothedEUQtyseries[w + 1]
for (i in seq(w, 1, -1)) {
  smoothedEUQtyseries[i] <- smoothedEUQtyseries[i + 1] - diff
}

#Smoothing right end of the time series

n <- length(qtyEUts[1:42])
diff <- smoothedEUQtyseries[n - w] - smoothedEUQtyseries[n - w - 1]
for (i in seq(n - w + 1, n)) {
  smoothedEUQtyseries[i] <- smoothedEUQtyseries[i - 1] + diff
}

smoothedEUQtydf <-
  as.data.frame(cbind(dataEUCon$months[1:42], as.vector(smoothedEUQtyseries)))
colnames(smoothedEUQtydf) <- c('months', 'NetQty')
lines(smoothedEUQtyseries, col = "red", lwd = 2)

################ Creating prediction model for EU Qty TS ##########
lmEUQtyFit <- lm(NetQty ~  cos(months * .6) + poly(months, 2),
                 data = smoothedEUQtydf)
summary(lmEUQtyFit)

globalPredEUQtyVal <- predict(lmEUQtyFit, months = seq(1:42))
summary(globalPredEUQtyVal)
plot(qtyEUts)
lines(smoothedEUQtyseries, col = "red", lwd = 2)
lines(globalPredEUQtyVal, col = 'blue', lwd = 2)

local_pred <- qtyEUts[1:42] - globalPredEUQtyVal
plot(local_pred, col = 'red', type = "l")
acf(local_pred)
acf(local_pred, type = "partial")
armafitEUQty <- auto.arima(local_pred)

tsdiag(armafitEUQty)
armafitEUQty
## Has 0 mean so checking whether its just white noise

adf.test(local_pred, alternative = "stationary")
kpss.test(local_pred)

############## Predicting value for future months for EU Sales TS ####
testDataEUQty <- dataEUCon[43:54, ]
testDataEUQty$predictedQty1 <-
  predict(lmEUQtyFit, data.frame(months = c(43:54)))

MAPE_auto_arima_EUQty1 <-
  accuracy(testDataEUQty$predictedQty1[1:6], dataEUCon$NetQty[43:48])[5]
MAPE_auto_arima_EUQty1 #39.91605

cdPredEUQty <- c(globalPredEUQtyVal, ts(testDataEUQty$predictedQty1))
plot(qtyEUts, xlim = c(1,54))
lines(cdPredEUQty, col = "red")

################ Applying auto arima on EU Sales TS #####################
qtyEUArima <- auto.arima(qtyEUts[1:42])

tsdiag(qtyEUArima)
qtyEUArima
testDataEUQty$predictedQty2 <-
  (predict(qtyEUArima, n.ahead = 12))$pred

resiEUQtyArima <- qtyEUts[1:42] - fitted(qtyEUArima)

adf.test(resiEUQtyArima, alternative = "stationary")
kpss.test(resiEUQtyArima)

MAPE_auto_arima_EUQty2 <-
  accuracy(testDataEUQty$predictedQty2[1:6], testDataEUQty$NetQty[1:6])[5]
MAPE_auto_arima_EUQty2  #30.13319

################ Plotting lines for EU Sales TS #######################
arimaPredEUQty <-
  c(fitted(qtyEUArima), ts(testDataEUQty$predictedQty2))
labels <-
  c("Raw",
    "Smoothed",
    "Classical Decomposition Pred",
    "Auto Arima Pred")
cols <- c("black", "green", "red", "blue")
plot(ts(dataEUCon$NetQty), xlim = c(1, 54))
abline( v= 48)
lines(smoothedEUQtyseries, col = "green", lwd = 2)
lines(cdPredEUQty, col = "red" , lwd = 2)
lines(arimaPredEUQty, col = "blue" , lwd = 2)
legend("topleft", labels, col = cols, lwd = 2)

#######################################################################
