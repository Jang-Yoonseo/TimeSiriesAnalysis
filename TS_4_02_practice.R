##############  package
library(forecast)  #ma
library(TTR) #sma
library(lmtest) #dwtest

#단순이동평균과 중심이동 평균

z <-    scan("mindex.txt")
mindex <- ts(z, start = c(1986, 1), frequency = 12)

mindex

SMA(mindex,n=5) #window size = 5
ma(mindex, order=5, centre = TRUE)

plot.ts(mindex, ylab="", xlab="") 
lines(SMA(mindex,n=5), col='red', lwd=2)
lines(ma(mindex, order=5, centre =    TRUE), col='blue',lty=2, lwd=2) 
legend('topright', lty=c(1,1,2), col=c('black', 'red', 'blue'),
       lwd=c(1,1,2),
       c('원시계열', "SMA(m=5)", "CSMA(l=2)"), 
       bty='n')

#이동평균을 이용한 분해법
z <-scan("food.txt") 
t <-1:length(z)
food <- ts(z, start=c(1981,1), frequency=12)
log_food <- log(food) #이분산성 제거를 위한 로그변환

plot.ts(log_food)
lines(ma(log_food,3), col='blue', lwd=2) 
lines(ma(log_food,12), col='red', lwd=2)
legend('topleft', lty=c(1,1,1), col=c('black', 'blue', 'red'), 
       lwd=c(1,1,2),
       c('원시계열', "CSMA(m=3)", "CSMA(l=12)"), 
       bty='n')

ts.plot(log_food,ma(log_food,3),ma(log_food,12),col=c('black', 'red', 'blue')) 
legend('topleft', lty=c(1,1,1), col=c('black', 'red', 'blue'),
       lwd=c(1,1,2),
       c('원시계열',"CSMA(m=3)", "CSMA(l=12)"), 
       bty='n')

#1 추세성분: 계절주기와 동일한 m을 이용한 중심이동평균
trend = ma(log_food, 12) 
plot.ts(trend, lwd=2)

#2 계절성분: 추세가 조정된 시계열에서
#각 계절성분의 평균을 구한 후, 평균을
#0으로 조정

adj_trend <- log_food - trend 
plot.ts(adj_trend, lwd=2)

seasonal <- tapply(adj_trend, cycle(adj_trend), function(y) mean(y,na.rm=T)) 
seasonal

summary(lm(adj_trend~0+as.factor(cycle(adj_trend))))

mean(seasonal)

seasonal <- seasonal - mean(seasonal)  #평균을  0으로 수정 
seasonal

St = rep(seasonal, 12) 
plot.ts(St)

#3 불규칙 성분
irregular <- log_food - trend - St 
plot.ts(irregular)
abline(h=0, lty=2)

t.test(irregular)
dwtest(lm(irregular~1))

#4 추정
fit_ <- trend + St
ts.plot(log_food, fit_, lty=1:2, col=1:2, lwd=2:3)

#decompose 함수를 이용한 분해법
#위에서 설명한 평활법을 이용한 분해법과 동일
dec_fit <- decompose(log_food, 'additive') 
dec_fit

## 비교  -  Trend 
trend[1:15]
dec_fit$trend[1:15]

## 비교  - Seasonal 
St[1:12]
dec_fit$seasonal[1:12]

plot(dec_fit)

dec_fit2 <-    decompose(food, type =    "multiplicative") 
dec_fit2
plot(dec_fit2)

## 가법모형  vs. 승법모형
pred_dec <-dec_fit$trend+dec_fit$seasonal 
pred_dec2 <-dec_fit2$trend*dec_fit2$seasonal
ts.plot(exp(pred_dec), pred_dec2, col=1:2, lty=1:2, ylab="food", xlab="time") 
legend("topleft", lty=1:2, col=1:2, c("가법모형 ", "승법모형 "))

sum((food-exp(pred_dec))^2, na.rm=T)  #SSE - 가법 
sum((food-pred_dec2)^2, na.rm=T)  #SSE - 승법

#stl 함수를 이용한 분해법
stl_fit1 <- stl(log_food, s.window=12) 
stl_fit1

head(stl_fit1$time.series)

t.test(stl_fit1$time.series[,3])
dwtest(lm(stl_fit1$time.series[,3]~1), alternative="two.sided")

plot(stl_fit1)

## stl vs. decompose
pred_stl <- stl_fit1$time.series[,1]+stl_fit1$time.series[,2]
ts.plot(pred_stl, pred_dec, col=1:2, lty=1:2, lwd=2:3, ylab="food", xlab="time",
        main="stl vs. decomse")
legend("topleft", lty=1:2, col=1:2, c("stl", "decomse"))

### SSE : 1-시차 후  예측 오차 제곱합 
sum((log_food-pred_stl)^2) #144
sum((log_food-pred_dec)^2, na.rm=T) #144-12=132

### MSE : 1-시차 후  예측 오차 제곱합의 평균 
sum((log_food-pred_stl)^2)/144
sum((log_food-pred_dec)^2, na.rm=T)/132

### MSE : 1-시차 후  예측 오차 제곱합의 평균 
mean((log_food-pred_stl)^2)
mean((log_food-pred_dec)^2, na.rm=T)
