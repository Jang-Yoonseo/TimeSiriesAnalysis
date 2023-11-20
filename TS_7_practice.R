library(tidyverse)
library(forecast)
library(MASS)

z<- AirPassengers

z %>% head() #TS객채

class(z)

plot.ts(z) #이분산성이 보인다.
#box - cox transformation 실시
#BoxCox - ts객채를 받는다.
BoxCox.lambda(z, method = 'guerrero') #게레로..?
#최적화 방법중 하나..
BoxCox(z, lambda = BoxCox.lambda(z, method = 'loglik'))
#최적화의 목적함수에 따라 달라진다!!
BoxCox(z,lambda = 'auto') #guerrero와 같다고 한다. 

t <- seq_along(z)
boxcox(z~t) #from MASS package , 모델을 받는다.
boxcox(z~1) #가능도함수를 가장 크게 하는 lambda 선정
#가운데 점선은 최댓값이고 좌우는 95% 신뢰구간을 의미미

bc <- boxcox(z~t)
bc #자료 관찰
which.max(bc$y)
bc$x[which.max(bc$y)]

#boxcox(z~1), boxcox(z~t) 둘다 신뢰구간에 0포함
#유의수준 95%에서 0이나 다름 없다는 것..
#따라서 그냥 람다가 0인 변환 즉 로그변환을 하면 됨
#아니면 그냥 나온 그 값을 써도 되는데 간단하게 로그가 좋다!

z_boxcox <- BoxCox(z, lambda=bc$x[which.max(bc$y)]) #boxcox -> BoxCox
par(mfrow=c(2,2))
plot.ts(z, main = "Original")
plot.ts(log(z), main = "log(z)") #좋고
plot.ts(sqrt(z),, main = "sqrt(z)")
plot.ts(z_boxcox, main = "Boxcox_lambda=0.06") #좋다

#간단한 로그변환을 선정
par(mfrow = c(1,1))

e <- round(rnorm(10),2)
z <- cumsum(e)
plot.ts(z)

e <- round(rnorm(1000),2)
z <- cumsum(e)
tsdisplay(z, lag.max=24) #SACF 가 엄청 천천히 감소, 확률적 추세이므로 차분

tsdisplay(diff(z), lag.max=24) #차분하게 차분

#diff의 메커니즘을 잘 알아둘것!!
#암튼 차분 결과 추세가 사라지고 WN화 되었다.
#결론: 확률보행과정을 차분하면 WN이 된다.

logz <- log(AirPassengers)
plot.ts(logz) #분해가 좋겠지만 학습을 위해 차분 진행

par(mfrow=c(1,2))
acf(logz, lag.max = 72) #조금 튀어나온 놈이 보인다. 왜 ? 계절성분 때문에!!
pacf(logz) #1주기의 다음에 음의 PACF가 유의하다.

#graphics.off()
#차분 진행 
d_log_z = diff(logz)
plot.ts(d_log_z)

tsdisplay(d_log_z, lag.max=36)
#선형 추세는 어찌저찌 사라진 것 같지만 아직 계절 성분이 남아있다
#SACF를 보면 판단이 가능하다.
#따라서 계절 차분을 실행한다.

ds_d_log_z <- diff(d_log_z, 12) #시도포의 주기만큼 차분
plot.ts(ds_d_log_z) #도로 이분산성이 생긴거 같은데 기분탓...?

tsdisplay(ds_d_log_z, lag.max=36) #썩 잘 잡히진 않은 것 같기도,,
#ps 계절형 ARIMA 적합에서 cover 가능

#절차에 맞게 진행.. 계절차분 -> 추세차분

ds_log_z <- diff(logz, 12)
ts.plot(ds_log_z) #뭔가 추세가 있는듯 없는듯 애매.. ACF, PACF 보기

tsdisplay(ds_log_z, lag.max=36)
#확률적 추세가 있는 ACF같다..
#혹은 AR모형에서 ACF가 지수적으로 감소하는 형태라고 할 수도 있다.
#근데 계절성분은 확실히 아직 남아있다.
#여러 모형을 적합하고 비교 측도를 통해 모델을 선정하는 것이 적절

z <-scan("depart.txt")
plot.ts(z) #추세성분 and 계절성분 with 이분산성 한스푼

log_z <- log(z)
tsdisplay(log_z, lag.max=36)
#역시 계절 차분이 필요하다.

ds_log_z <- diff(log_z, 12) #계절 차분
tsdisplay(ds_log_z, lag.max=36) #확률적 추세일까 지수적 감소일까...
#나중에 적합해두고 비교측도 이용~

#Simulations

n <- 200
x = arima.sim(n=n, list(order=c(1,0,0), ar=0.5)) #AR(1)
z = arima.sim(n=n, list(order=c(1,1,0), ar=0.5)) #ARIMA(1,1,0)

tsdisplay(x, lag.max=36, main = "AR(1)") #AR(1)
forecast::tsdisplay(z, lag.max=36, main = "ARIMA(1,1,0)") #ARIMA(1,1,0)
#확률적 추세가 있어보이는 ACF.. 따라서 차분 진행!
tsdisplay(diff(z), lag.max=36, main = "(1-B)z") #확률적 추세 소거:AR(1)

n <- 200
x = arima.sim(n=n, list(order=c(0,0,1), ma=-0.8)) #MA(1)
z = arima.sim(n=n, list(order=c(0,1,1), ma=-0.8)) #ARIMA(1,1,0)

tsdisplay(x, lag.max=36, main = "MA(1)")
tsdisplay(z, lag.max=36, main = "ARIMA(0,1,1)") #역시 확률적 추세 존재

tsdisplay(diff(z), lag.max=36, main = "(1-B)z") #MA(1)

n <- 1000
x = arima.sim(n=n, list(order=c(1,0,1), ar=0.5, ma=-0.8)) #ARMA(1,1)
z = arima.sim(n=n, list(order=c(1,1,1), ar=0.5, ma=-0.8)) #ARIMA(1,1,1)

tsdisplay(x, lag.max=36, main = "ARMA(1,1)")
tsdisplay(z, lag.max=36, main = "ARIMA(1,1,1)") #한번 차분하면 ARMA(1,1) 

tsdisplay(diff(z), lag.max=36, main = "(1-B)z") #ARMA(1,1)

#과대차분

## ARIMA(0,1,1)
z = arima.sim(n=1000, list(order=c(0,1,1), ma=0.5))
tsdisplay(z, lag.max=36, main = "ARIMA(0,1,1)")

#한 번 차분
d_z <- diff(z)
forecast::tsdisplay(d_z, lag.max=36, main = "ARIMA(0,1,1)")

# 한 번 더 차분
d2_z <- diff(d_z)
forecast::tsdisplay(d2_z, lag.max=36, main = "ARIMA(0,1,1)") #MA(2가 되었다.
#정상 차분 -> 정상 근데 복잡도 올라감

#분산(표준편차) 비교
sd(z)
sd(diff(z))
sd(diff(diff(z))) #분산이 올라간게 느껴지십니까?

#내가 과대차분을 했나? 판단법 -> 분산을 찍어본다.
