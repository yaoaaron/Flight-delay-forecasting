# On-Time Performance in Chicago O'Hare Airport

library(tidyverse)
library(lubridate)
library(dplyr)
library(nlme)
library(ggplot2)
library(ggfortify)
library(scales)
library(xts)

# read in flight performance data:
flight_raw <- read.csv('./flight_raw.csv')
# filter for ORD only: 
flight_raw <- flight_raw[(flight_raw$ORIGIN == 'ORD' | flight_raw$DEST == 'ORD'), ]
# flight_raw$id <- seq.int(nrow(flight_raw))
# check missing
summary(flight_raw)  # part of missing due to cancelled and diverted
summary(flight_raw[flight_raw$CANCELLED != 1, ])  # 123 NA's in DEP_DELAY, remove
## 123 NA's in DEP_DELAY, remove
flight_clean <- flight_raw[!(flight_raw$CANCELLED != 1 & is.na(flight_raw$DEP_DELAY)), ]
## 34 NA's in ARR_DELAY, remove
flight_clean <- flight_clean[!(flight_clean$CANCELLED != 1 & is.na(flight_clean$ARR_DELAY) & 
                        flight_clean$DIVERTED != 1), ]
# segment cancelled/diverted/on-time/delay/early_dept
cancel_flights <- flight_clean[flight_clean$CANCELLED == 1, ]
divert_flights <- flight_clean[flight_clean$DIVERTED == 1, ]
ontime_delay <- flight_clean[!(flight_clean$DIVERTED == 1 | flight_clean$CANCELLED == 1), c(1:9)]
ontime_flights <- ontime_delay[(ontime_delay$ARR_DELAY < 15 & ontime_delay$ARR_DELAY >=-5), ]
delay_flights <- ontime_delay[ontime_delay$ARR_DEL15 == 1, ]
early_flights <- ontime_delay[ontime_delay$ARR_DELAY < -5, ]

# EDA 
# by carrier, figure 1
delay_carrier <- delay_flights %>% group_by(OP_UNIQUE_CARRIER) %>% summarise(delay_flights=n())
flight_clean %>% group_by(OP_UNIQUE_CARRIER) %>% summarise(total_flights=n()) %>% 
  left_join(delay_carrier, by="OP_UNIQUE_CARRIER") %>% mutate(ratio=delay_flights/total_flights) %>% 
  ggplot(aes(x=reorder(OP_UNIQUE_CARRIER, -ratio), y=ratio)) + 
  geom_bar(stat="identity", fill="steelblue") + xlab("Carrier") + ylab("Ratio") + 
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18))
# by year, figure 2
delay_yeargrp <- delay_flights %>% mutate(yeargrp = ifelse(FL_DATE >= '2013-06-01' & FL_DATE <= '2014-05-31', "1stR12",
                                                           ifelse(FL_DATE >= '2014-06-01' & FL_DATE <= '2015-05-31', "2ndR12",
                                                                  ifelse(FL_DATE >= '2015-06-01' & FL_DATE <= '2016-05-31', "3rdR12",
                                                                         ifelse(FL_DATE >= '2016-06-01' & FL_DATE <= '2017-05-31', "4thR12",
                                                                                ifelse(FL_DATE >= '2017-06-01' & FL_DATE <= '2018-05-31', "5thR12",
                                                                                       ifelse(FL_DATE >= '2018-06-01' & FL_DATE <= '2019-05-31', "6thR12", NA))))))
                                          ) %>% group_by(yeargrp) %>% summarise(delay_flights=n())
flight_clean %>% mutate(yeargrp = ifelse(FL_DATE >= '2013-06-01' & FL_DATE <= '2014-05-31', "1stR12",
                                        ifelse(FL_DATE >= '2014-06-01' & FL_DATE <= '2015-05-31', "2ndR12",
                                               ifelse(FL_DATE >= '2015-06-01' & FL_DATE <= '2016-05-31', "3rdR12",
                                                      ifelse(FL_DATE >= '2016-06-01' & FL_DATE <= '2017-05-31', "4thR12",
                                                             ifelse(FL_DATE >= '2017-06-01' & FL_DATE <= '2018-05-31', "5thR12",
                                                                    ifelse(FL_DATE >= '2018-06-01' & FL_DATE <= '2019-05-31', "6thR12", NA))))))
                        ) %>% group_by(yeargrp) %>% summarise(total_flights=n()) %>%
  left_join(delay_yeargrp, by="yeargrp") %>% mutate(ratio=delay_flights/total_flights) %>%
  ggplot(aes(x=yeargrp, y=ratio)) + 
  geom_bar(stat="identity", fill="steelblue") + xlab("Year - R12") + ylab("Ratio") + 
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18))
# by month, figure 3
delay_mon <- delay_flights %>% mutate(month = format(FL_DATE, '%m')) %>% group_by(month) %>% summarise(delay_flights=n())
flight_clean %>% mutate(month = format(FL_DATE, '%m')) %>% group_by(month) %>% summarise(total_flights=n()) %>%
  left_join(delay_mon, by="month") %>% mutate(ratio=delay_flights/total_flights) %>%
  ggplot(aes(x=month, y=ratio)) + 
  geom_bar(stat="identity", fill="steelblue") + xlab("Month") + ylab("Ratio") + 
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18))
# combine data
flights = flight_clean %>% mutate(y_mon=format(FL_DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% summarise(total_flights=n()) %>% 
  left_join(ontime_flights %>% mutate(y_mon=format(FL_DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% summarise(ontime_flights=n())) %>%
  left_join(delay_flights %>% mutate(y_mon=format(FL_DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% summarise(delay_flights=n())) %>%
  left_join(early_flights %>% mutate(y_mon=format(FL_DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% summarise(early_flights=n())) %>%
  left_join(cancel_flights %>% mutate(y_mon=format(FL_DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% summarise(cancel_flights=n())) %>%
  left_join(divert_flights %>% mutate(y_mon=format(FL_DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% summarise(divert_flights=n()))
flights$y_mon <- as.Date(flights$y_mon)
# plot type of flights, figure 4
sumdata = data.frame(value=apply(flights[,c(3:7)], 2, sum, na.rm=TRUE))
sumdata$type = rownames(sumdata)
ggplot(data=sumdata, aes(x=type, y=value, fill=type)) + geom_bar(colour="black", stat="identity") + 
  geom_text(aes(label=format(value, big.mark=",", scientific=FALSE)), vjust=-0.3, size=4.5) + 
  xlab("Type of Flights") + ylab("Number of Flights") + scale_y_continuous(labels=comma) + 
  theme(legend.position = "none",
        axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18))

# flights exploration
str(flights)  
head(flights, n=5)
class(flights$y_mon)
range(flights$y_mon)

# Convert flights to an xts object
flights_xts <- as.xts(flights[ , -1], order.by = flights$y_mon)
class(flights_xts)
head(flights_xts, 5)
periodicity(flights_xts)
nmonths(flights_xts)
# total daily flights over time
plot.xts(flights_xts$total_flights, main="Total Daily Flights - ORD")
# daily delayed flights over time
plot.xts(flights_xts$delay_flights, main="Delayed Daily Flights = ORD")
# plot all types, figure 5
plot.zoo(flights_xts, plot.type = "multiple", xlab="Date", ylab=c("Total", "On-Time", "Delay", "Early", "Cancel", "Divert"))

# Calculate percentage
flights_xts$pct_delay <- (flights_xts$delay_flights / flights_xts$total_flights) * 100
plot.xts(flights_xts$pct_delay)  # fluctuate a lot
flights_xts$pct_cancel <- (flights_xts$cancel_flights / flights_xts$total_flights) * 100
flights_xts$pct_divert <- (flights_xts$divert_flights / flights_xts$total_flights) * 100
# assess flight trends
plot.zoo(x = flights_xts[ , c("pct_delay", "pct_cancel", "pct_divert")])

# model pct_delay
pct_delay <- ts(flights_xts[, "pct_delay"], start = c(2013, 6), freq = 12)
start(pct_delay)
end(pct_delay)
frequency(pct_delay)
time(pct_delay) 

plot(decompose(pct_delay, type='additive'))  # figure 6
pct_delay_rand <- na.omit(decompose(pct_delay, type='additive')$random)
acf(pct_delay_rand, na.action = na.pass)  # 2nd and 13th lag sig

delay_pre <- window(pct_delay, start = c(2013, 6), end = c(2019, 2))
Actual <- window(pct_delay, start = c(2019, 3), end = c(2019, 5))
delay.hw <- HoltWinters(delay_pre, seasonal="multi")
Forecast <- predict(delay.hw, n.ahead = 3)
# ts.plot(delay_post, delayforc, lty = 1:2, gpars=list(col=c('red', 'blue')))
# legend("bottomright", legend = c("Actual", "Forecast"), lty = 1:2, col = c("red", "blue"), cex=0.5, pt.cex=5)
autoplot(cbind(Actual, Forecast), facets=FALSE, size=1) + xlab("Month") + ylab("Percent Delay") + 
  scale_color_manual(labels = c("Actual", "HW Forecast"), values=c("black", "red")) + 
  aes(linetype = plot_group, size = plot_group) + 
  scale_linetype_manual(labels = c("Actual", "HW Forecast"), values = c(1, 2)) + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(size=15), axis.title.y = element_text(size=15),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))
# MAPE
mean(abs((Actual-Forecast)/Actual)*100)  # 10.15345
# Forecast June using all historical data
delay.hw <- HoltWinters(pct_delay, seasonal="multi")
delayforc <- predict(delay.hw, n.ahead=3)
ts.plot(pct_delay, delayforc, lty = 1:2, gpars=list(col=c('red', 'blue')))  
points(tail(time(pct_delay), 1) + 1/12, delayforc, pch=19, col="blue")

# regression
Time <- time(delay_pre)
delay.lmt <- lm(delay_pre ~ Time)
summary(delay.lmt)  # non sig
Seas <- factor(cycle(delay_pre))
delay.lmts <- lm(delay_pre ~ 0 + Time + Seas)
summary(delay.lmts) 
coef(delay.lmts)
max(coef(delay.lmts)[2:13])  # -2559.296
### based on the estimates of the model coefficients Time and all 12 seasonal dummy variables are statistically significant
### time trend is positive and upward, there is a seasonal component
### Seas6, June is expected to be the largest because it has the largest coef, consistent with above 
delay.lmts.resid <- resid(delay.lmts)
acf(delay.lmts.resid)  # sig, need to use gls
acf(delay.lmts.resid)[1]  # 0.443, figure 7
pacf(delay.lmts.resid) 
# forecast
new.Time <- seq(Time[length(Time)]+1/12, len = 3, by=1/12)
new.Seas <- c(3,4,5)
new.data <- data.frame(Time = new.Time, Seas=factor(new.Seas))
predict.lmts <- predict(delay.lmts, new.data)
Forecast <- predict.lmts[1:3]
# MAPE
mean(abs((Actual-Forecast)/Actual)*100)  # 8.85%, 17.9
# gls
delay.gls <- gls(delay_pre ~ 0 + Time + Seas, cor = corAR1(0.443))
coef(delay.gls)
AIC(delay.lmts)
AIC(delay.gls) # better
delay.gls.resid <- resid(delay.gls)
acf(delay.gls.resid) # no improvements
# forecast
alpha <- coef(delay.gls)[1]
beta <- coef(delay.gls)[4:6]
predict.gls <- alpha * new.Time + beta
Forecast <- predict.gls
# MAPE
mean(abs((Actual-predict.gls)/Actual)*100)  # 14.19
autoplot(cbind(Actual, Forecast), facets=FALSE, size=1) + xlab("Month") + ylab("Percent Delay") + 
  scale_color_manual(labels = c("Actual", "LM Forecast"), values=c("black", "red")) + 
  aes(linetype = plot_group, size = plot_group) + 
  scale_linetype_manual(labels = c("Actual", "LM Forecast"), values = c(1, 2)) + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(size=15), axis.title.y = element_text(size=15),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))
# percent delay in June
print(predict.lmts[4])  # 29%

# AR?, figure 8
par(mfrow=c(1,2))
acf(pct_delay)  # 1st lag sig, sine/cosine pattern, not random walk, shows seasonal pattern, no geometric decay
pacf(pct_delay)  # 1st lag sig, not a single AR process

# ARIMA
best.order <- c(0, 0, 0)
best.aic <- Inf
for (p in 0:2) for (d in 0:2) for (q in 0:2) {
  fit.aic <- AIC(arima(delay_pre, order = c(p, d, q)))
  if (fit.aic < best.aic) {
    best.order <- c(p, d, q)
    best.arima <- arima(delay_pre, order = best.order)
    best.aic <- fit.aic
  }}
best.arima
best.order # (2,1,2)  

# extract residual
best.arima.resid <- resid(best.arima)
par(mfrow=c(1,2))
acf(best.arima.resid)  # lag 9 sig, figure 9
pacf(best.arima.resid)  # non sig
# forecast and compare
ARIMA.Forecast <- predict(best.arima, n.ahead=3)$pred
autoplot(cbind(Actual, ARIMA.Forecast), facets=FALSE, size=1) + xlab("Month") + ylab("Percent Delay") + 
  scale_color_manual(labels = c("Actual", "ARIMA Forecast"), values=c("black", "red")) + 
  aes(linetype = plot_group, size = plot_group) + 
  scale_linetype_manual(labels = c("Actual", "ARIMA Forecast"), values = c(1, 2)) + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(size=15), axis.title.y = element_text(size=15),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))
# MAPE
mean(abs((Actual-ARIMA.Forecast)/Actual)*100)  # 22.4%

# SARIMA
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

delay.best.sarima <- get.best.arima(delay_pre, maxord = c(5,5,5,1,1,1))
delay.best.sarima[[3]]  # 4 1 4 1 1 1

# extract residual
best.sarima.resid <- resid(delay.best.sarima[[2]])
par(mfrow=c(1,2))
acf(best.sarima.resid)  # non sig, figure 10
pacf(best.sarima.resid)  # non sig
# forecast and compare
SARIMA.Forecast <- predict(delay.best.sarima[[2]], n.ahead=3)$pred
autoplot(cbind(Actual, SARIMA.Forecast), facets=FALSE, size=1) + xlab("Month") + ylab("Percent Delay") + 
  scale_color_manual(labels = c("Actual", "SARIMA Forecast"), values=c("black", "red")) + 
  aes(linetype = plot_group, size = plot_group) + 
  scale_linetype_manual(labels = c("Actual", "SARIMA Forecast"), values = c(1, 2)) + 
  theme(legend.title = element_blank(), axis.title.x = element_blank(),
        axis.text.x = element_text(size=15), axis.title.y = element_text(size=15),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))
# MAPE
mean(abs((Actual-SARIMA.Forecast)/Actual)*100)  # 10.776%, 8.09% the best!

# add weather
weather_raw <- read.csv('./ORD_weather_20130101_20190727.csv')
# remove columns 
weather_raw <- subset(weather_raw, select = -c(STATION, NAME, PGTM, SNOW, SNWD, PRCP, WDF2, WDF5, WSF2, WSF5))
weather_raw$DATE <- as.Date(weather_raw$DATE)
weather_raw <- subset(weather_raw, DATE >= as.Date("2013-06-01") & DATE <= as.Date("2019-05-31"))
rownames(weather_raw) <- NULL
summary(weather_raw)

weather_xts <- as.xts(weather_raw[ , -1], order.by = weather_raw$DATE)
plot.xts(weather_xts[,2:4], main="Daily Min/Max/AVG Temp - ORD")  # figure 11
# convert to monthly data. stationid = "GHCND:USW00094846"
weather_mon <- weather_raw %>% mutate(y_mon=format(DATE, '%Y-%m-01')) %>% group_by(y_mon) %>% 
  summarise(AWND=mean(AWND), TAVG=mean(TAVG), TMAX=mean(TMAX), TMIN=mean(TMIN))
weather_mon_xts <- as.xts(weather_mon[,-1], order.by=as.Date(weather_mon$y_mon))
# compare flight and weather data, figure 12, 13, 14
flights_wea <- merge(flights_xts, weather_mon_xts)
autoplot(flights_wea[, c("pct_delay", "TAVG")], facets=FALSE, size=1) + xlab("Month")  + 
  scale_color_manual(labels = c("Pct. Delay", "Temperature"), values=c("black", "red")) + 
  aes(linetype = plot_group, size = plot_group) +
  scale_linetype_manual(labels = c("Pct. Delay", "Temperature"), values = c(1, 2)) + 
  theme(legend.title = element_blank(), axis.text.x = element_text(size=15), axis.title.x = element_blank(),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))

autoplot(flights_wea[, c("pct_delay", "pct_cancel", "TAVG")], facets=FALSE, size=1) + xlab("Month")  + 
  scale_color_manual(labels = c("Pct. Cancel", "Pct. Delay", "Temperature"), values=c("black", "gray52", "red")) + 
  aes(linetype = plot_group, size = plot_group) +
  scale_linetype_manual(labels = c("Pct. Cancel", "Pct. Delay",  "Temperature"), values = c(1, 1, 2)) + 
  theme(legend.title = element_blank(), axis.text.x = element_text(size=15), axis.title.x = element_blank(),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))

autoplot(flights_wea[, c("pct_delay", "pct_cancel", "AWND")], facets=FALSE, size=1) + xlab("Month")  + 
  scale_color_manual(labels = c("Wind", "Pct. Cancel", "Pct. Delay"), values=c("red", "black", "gray52")) + 
  aes(linetype = plot_group, size = plot_group) +
  scale_linetype_manual(labels = c("Wind", "Pct. Cancel", "Pct. Delay"), values = c(2, 1, 1)) + 
  theme(legend.title = element_blank(), axis.text.x = element_text(size=15), axis.title.x = element_blank(),
        axis.text.y = element_text(size=15), legend.text=element_text(size=15))

# cross correlaiton
tavg <- ts(weather_mon$TAVG, start = c(2015, 6), freq = 12)
flignts_xts_df = data.frame(date=index(flights_xts), coredata(flights_xts))
pct_cancel <- ts(flignts_xts_df$pct_cancel, start = c(2015, 6), freq = 12)
acf(ts.union(pct_cancel, tavg))
ccf(pct_cancel, tavg)
### lag 0 and lag 1 are significant, indicating two sereis evolve almost concurrently, the correlation is negative, when 
### one increases the other decreases. The correlation is not too strong (around 0.3)
### postive lags means temps goes to future, negative lages means temps goes to the past, 
### positve lags shows whether future value of temperature has explanatory power to explain current pct_cancel
### negative lags of temps shows whether past value of temps can explain current pct_cancel
### here current temperature can explain current pct_cancel, future one month ahead temperature can explain current pct_cancel
### avg leads cancel, pct_cancel precede temp, and vise versa

# pct_cancel analysis
start(pct_cancel)
end(pct_cancel)
frequency(pct_cancel)
time(pct_cancel) 

acf(pct_cancel)  
pacf(pct_cancel)  # white noise, cannot be predicted

