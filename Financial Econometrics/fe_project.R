setwd("C:/Users/annie/Desktop")
dat<-read.csv("일별데이터.csv",header=T,stringsAsFactors=F)
View(dat)

##### Explore the data #####
library(xts)
time_index<-seq(from = as.POSIXct("2016-11-01"), 
                  to = as.POSIXct("2017-10-31"), by = "day")
call<- xts(dat$call, order.by = time_index)
plot.xts(call,col="navy") # plot of call

minmax<-c(dat$date[which(call==min(call))],dat$date[which(call==max(call))])
names(minmax)<-c("min","max");minmax
library(dplyr)
filter(dat,call %in% unique(sort(call))[1:2])

# descriptive statistics
library(TSA)
des<-c(mean(call),var(call),skewness(call),kurtosis(call))
names(des)<-c("mean","variance","skewness","kurtosis");des 

# calls by day
dat$day<-factor(dat$day,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
library(ggplot2)
ggplot(data=aggregate(call~day,dat,mean),aes(x=day,y=call,group=1))+
  geom_line(size=1,col="navy")+geom_point()

# ACF and PACF
library(forecast)
par(mfrow=c(1,2))
Acf(call,lag=50);Pacf(call,lag=50)
par(mfrow=c(1,1))

plot.xts(call[1:30],col="navy") # seasaonality : week

ss.diff<-diff(call,lag=7) # seasonal difference
par(mfrow=c(1,2))
Acf(ss.diff,lag=50);Pacf(ss.diff,lag=50)
par(mfrow=c(1,1))

##### SARIMA #####
fit1<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T);fit1
fit2<-arima(call,order=c(1,0,1),seasonal=list(order=c(2,1,1),period=7),include.mean = T);fit2

# weather variable
max.temp<-xts(dat$max.temp, order.by = time_index)
  dmax.temp<-diff(max.temp,lag=7)
min.temp<-xts(dat$min.temp, order.by = time_index)
  dmin.temp<-diff(min.temp,lag=7)
temp.diff<-xts(dat$temp.diff, order.by = time_index)
  dtemp.diff<-diff(temp.diff,lag=7)
avg.temp<-xts(dat$avg.temp, order.by = time_index)
  davg.temp<-diff(avg.temp,lag=7)
wind<-xts(dat$wind, order.by = time_index)
  dwind<-diff(wind,lag=7)
water<-xts(dat$water, order.by = time_index)
  dwater<-diff(water,lag=7)
visibility<-xts(dat$visibility, order.by = time_index)
  dvisibility<-diff(visibility,lag=7)
dust<-xts(dat$dust, order.by = time_index)
  ddust<-diff(dust,lag=7)

# fitting models 
fit1_1<-arima(x=call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
             xreg=dmax.temp);fit1_1
fit1_2<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=dmin.temp);fit1_2
fit1_3<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=dtemp.diff);fit1_3
fit1_4<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=davg.temp);fit1_4
fit1_5<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=dwind);fit1_5
fit1_6<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=dwater);fit1_6
fit1_7<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=dvisibility);fit1_7
fit1_8<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=ddust);fit1_8
fit1_9<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=model.matrix(~dat$day)[,-1]);fit1_9

fit1_19<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
              xreg=cbind(dmax.temp,model.matrix(~dat$day)[,-1]));fit1_19
fit1_69<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
               xreg=cbind(dwater,model.matrix(~dat$day)[,-1]));fit1_69
fit1_169<-arima(call,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
               xreg=cbind(dmax.temp,dwater,model.matrix(~dat$day)[,-1]));fit1_169

# comparing AIC
AIC<-NULL
models<-c("fit1_1","fit1_2","fit1_3","fit1_4","fit1_5","fit1_6","fit1_7","fit1_8","fit1_9")
for (i in 1:9){
  AIC[i]<-get(models[i])$aic
}
names(AIC)<-models;AIC

AIC<-NULL
models<-c("fit1_19","fit1_69","fit1_169")
for (i in 1:3){
  AIC[i]<-get(models[i])$aic
}
names(AIC)<-models;AIC

# Ljung-test on residuals
a<-Box.test(residuals(fit1_19),lag=7,type="Ljung-Box")$p.value
s<-Box.test(residuals(fit1_19),lag=14,type="Ljung-Box")$p.value
d<-Box.test(residuals(fit1_19),lag=21,type="Ljung-Box")$p.value
f<-Box.test(residuals(fit1_19),lag=28,type="Ljung-Box")$p.value
g<-Box.test(residuals(fit1_19),lag=35,type="Ljung-Box")$p.value

data.frame(lag=c("Q(7)","Q(14)","Q(21)","Q(28)","Q(35)"),p.value=c(a,s,d,f,g))

a<-Box.test(residuals(fit1_69),lag=7,type="Ljung-Box")$p.value
s<-Box.test(residuals(fit1_69),lag=14,type="Ljung-Box")$p.value
d<-Box.test(residuals(fit1_69),lag=21,type="Ljung-Box")$p.value
f<-Box.test(residuals(fit1_69),lag=28,type="Ljung-Box")$p.value
g<-Box.test(residuals(fit1_69),lag=35,type="Ljung-Box")$p.value

data.frame(lag=c("Q(7)","Q(14)","Q(21)","Q(28)","Q(35)"),p.value=c(a,s,d,f,g))

a<-Box.test(residuals(fit1_169),lag=7,type="Ljung-Box")$p.value
s<-Box.test(residuals(fit1_169),lag=14,type="Ljung-Box")$p.value
d<-Box.test(residuals(fit1_169),lag=21,type="Ljung-Box")$p.value
f<-Box.test(residuals(fit1_169),lag=28,type="Ljung-Box")$p.value
g<-Box.test(residuals(fit1_169),lag=35,type="Ljung-Box")$p.value

data.frame(lag=c("Q(7)","Q(14)","Q(21)","Q(28)","Q(35)"),p.value=c(a,s,d,f,g))

# forecasting
nf<-15   # number of forecast

f0<-matrix(0,nf,1)
f1<-matrix(0,nf,1)
f2<-matrix(0,nf,1)
f3<-matrix(0,nf,1)

for (i in 1:nf){
  call2<-call[i:(349+i)]
  
  dmax.temp2<-dmax.temp[i:(349+i)]
  dwater2<-dwater[i:(349+i)]
  day2<-dat$day[i:(349+i)]
  
  fit1_f<-arima(call2,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T)
  fit19_f<-arima(call2,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
                 xreg=cbind(dmax.temp2,model.matrix(~day2)[,-1]),transform.pars = F)
  fit69_f<-arima(call2,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
                 xreg=cbind(dwater2,model.matrix(~day2)[,-1]))
  fit169_f<-arima(call2,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=7),include.mean = T,
                  xreg=cbind(dmax.temp2,dwater2,model.matrix(~day2)[,-1]))
  
  f0[i]<-predict(fit1_f,h=1)$pred
  f1[i]<-predict(fit19_f,newxreg=cbind(dmax.temp,model.matrix(~dat$day)[,-1])[350+i,],nrow=1)$pred
  f2[i]<-predict(fit69_f,newxreg=cbind(dwater,model.matrix(~dat$day)[,-1])[350+i,],nrow=1)$pred
  f3[i]<-predict(fit169_f,newxreg=cbind(dwater,dmax.temp,model.matrix(~dat$day)[,-1])[350+i,],nrow=1)$pred
}

origin<-call[(365-14):365]

rmse0=sqrt(mean((origin-f0)^2))
rmse1=sqrt(mean((origin-f1)^2))
rmse2=sqrt(mean((origin-f2)^2))
rmse3=sqrt(mean((origin-f3)^2))

RMSE=c(rmse0,rmse1,rmse2,rmse3)
names(RMSE)<-c("Model0","Model1","Model2","Model3");RMSE 

error0<-origin-f0
error1<-origin-f1
error2<-origin-f2
error3<-origin-f3

dm.test(error0,error1,alternative = "greater")
dm.test(error0,error2,alternative = "greater")
dm.test(error0,error3,alternative = "greater")
dm.test(error1,error2)
dm.test(error1,error3)
dm.test(error2,error3)

##### VAR #####
library(vars)
call<-ts(dat$call,frequency = 365)
max.temp<-ts(dat$max.temp,frequency = 365)
water<-ts(dat$water,frequency = 365)
avg.temp<-ts(dat$avg.temp,frequency = 365)
dust<-ts(dat$dust,frequency = 365)
dx = cbind(dust,water,max.temp,avg.temp,call)

#Lag optimisation
VARselect(dx, lag.max = 10, type = "both")

#Vector autoregression with lags set according to results of lag optimisation. 
var = VAR(dx, p=8)
serial.test(var, lags.pt = 25, type = "PT.asymptotic")
summary(var)

#Forecasting
nf<-15   # number of forecast
f4<-matrix(0,nf,1)

for (i in 1:nf){
  call2<-call[i:(349+i)]
  max.temp2<-max.temp[i:(349+i)]
  avg.temp2<-avg.temp[i:(349+i)]
  water2<-water[i:(349+i)]
  dust2<-dat$dust[i:(349+i)]
  
  dx2 = cbind(dust2,water2,max.temp2,avg.temp2,call2)
  var = VAR(dx2, p=8)
  
  f4[i]<-predict(var, n.ahead = 1)$fcst[[5]][1]
}

origin<-call[(365-14):365]

rmse4=sqrt(mean((origin-f4)^2))

