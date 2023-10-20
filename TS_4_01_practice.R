##############  package
library(forecast)  #ma
library(TTR) #sma
library(lmtest) #dwtest

z <- scan("food.txt")
t <- 1:length(z)
food <- ts(z, start = c(1981, 1), frequency = 12)
plot.ts(food, lwd = 2, main = "Time Series Plot for food data")

## 이분산성 제거를 위한 변수 변환
log_food <- log(food)
plot.ts(log_food, lwd = 2, main = "Time Series Plot for log(food) data")

fit <- lm(log_food ~ t)
summary(fit)

hat_Tt <- fitted(fit)
ts.plot(
  log_food,
  hat_Tt,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "log변환한 시계열과 분해법 의 추세성분 "
)
legend(
  "topleft",
  lty = 1:2,
  col = 1:2,
  lwd = 2:3,
  c("ln(z)", "추 세 성 분 ")
)

## 원시계열에서 추세성분  조정
adjtrend = log_food - hat_Tt
plot.ts(adjtrend, lwd = 2)

## 지시함수를 이용한 계절성분 추정정
y = factor(cycle(adjtrend))  #범주형 변수로 변환
fit1 <- lm(adjtrend ~ 0 + y)
summary(fit1)

hat_St <- fitted(fit1)
ts.plot(hat_St, main = "추정된 계절성분 ")

#3 불규칙 성분
hat_It <- log_food - hat_Tt - hat_St
ts.plot(hat_It)
abline(h = 0)

t.test(hat_It)  #H0 : mu=E(It)=0

dwtest(lm(hat_It ~    1),
       alternative =    'two.sided')

#4 추정
pred_a <- hat_Tt + hat_St
ts.plot(
  log_food,
  pred_a,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "log변환한 시계열과 분해법에 의한 추정값 "
)
legend("topleft",
       lty = 1:2,
       col = 2:3,
       c("ln(z)", "추정값 "))

SSE = sum((food - exp(pred_a)) ^ 2)
MSE = mean((food - exp(pred_a)) ^ 2)
SSE
MSE

## 추세를 이용한 분해법 - 승법 모형
plot.ts(food, lwd = 2)

#1 추세성분 추정 Zt = beta0 + beta1*t + epsion 적합
fit3 <- lm(food ~ t)
summary(fit3)

ts.plot(
  food,
  fitted(fit3) ,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "원시계열과 분해법에 의한 추세성분"
)
legend('topleft' ,
       lty = 1:2 ,
       col = 1:2,
       c('원시계열', '추세성분'))

fit4 <- lm(food ~  t + I(t ^ 2))#곡선형 추세를 잡겠다.
summary(fit4)

ts.plot(
  food,
  fitted(fit3),
  fitted(fit4),
  col = 1:3,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "원시계열과 분해법에 의한 추세성분"
)
legend("topleft",
       lty = 1:2,
       col = 1:3,
       c("원시계열", "추세성분", "2차 추세성분"))

#2 계절성분 추정
## 원시계열에서 추세성분  조정
trend_1 = fitted(fit3)
adjtrend_1 = food / trend_1
plot.ts(adjtrend_1)

## 원시계열에서 2차 추세성분 조정
trend_2 = fitted(fit4)
adjtrend_2 = food / trend_2
plot.ts(adjtrend_2)
abline(h = 1, lty = 2)

## 지시함수를 이용한 계절성분 추정

y = factor(cycle(adjtrend_2))
fit5 <- lm(adjtrend_2 ~ 0 + y)
summary(fit5)

seasonal <- fitted(fit5) #적합된 계절성분
ts.plot(seasonal, main = "추정된 계절성분 ")

seasonal <- fitted(fit5)
ts.plot(
  adjtrend_2,
  seasonal,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  main = '추세조정된 시계열과 추정된 계절성분'
)

#3 불규칙 성분
irregular <- food / (trend_2 * seasonal)
ts.plot(irregular)
abline(h = 1, lty = 2)

t.test(irregular, mu = 1)

dwtest(lm(irregular ~ 1), alternative = 'two.sided')

#4 추정
pred_m <- trend_2 * seasonal
ts.plot(
  food,
  pred_m,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "원시계열과 분해법에  의한 추정값"
)
legend("topleft",
       lty = 1:2,
       col = 1:2,
       c("원시계열 ", "추정값 "))

#pred_a #가법
#pred #승법
ts.plot(
  food,
  pred_a,
  pred_m,
  col = 1:3,
  lty = c(1, 1, 2),
  lwd = c(2, 3, 3),
  ylab = "food",
  main = "원시계열과 분해법에 의한 추정값 "
)

#가법
sum((food - exp(pred_a)) ^ 2)  ##SSE
mean((food - exp(pred_a)) ^ 2)  ##MSE

#승법
sum((food - pred_m) ^ 2) #SSE
mean((food - pred_m) ^ 2) #MSE
