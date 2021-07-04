?ts
dat <- glucose$`patient 10`
dat_ts <- ts(dat, start = dat$Date)
head(dat_ts)
autoplot(dat)

library(lubridate)
library(data.table)
##
set.seed(123)
Data <- data.frame(
  date=as.Date(ymd(20130904))+0:364,
  x=as.numeric(sample(1:3,365,replace=TRUE)),
  y=as.numeric(sample(1:3,365,replace=TRUE)))
setDT(Data)
##
head(Data)
min(Data$date)
max(Data$date)
xSpan <- seq.Date(
  from=as.Date("2013-10-01"),
  to=as.Date("2014-04-09"),
  by="day")

Data %>%
  filter(date < "2013-10-01")

GlucoRx_measurement_1_ %>% 
  filter(seq.Date(from = ))
  group_by(Period) %>%
  summarise(n(), means = mean(`Glucose(mmol/L)`),median = median(`Glucose(mmol/L)`),
            std = sd(`Glucose(mmol/L)`))
