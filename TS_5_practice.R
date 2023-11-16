library(forecast)
library(tidyverse) 
library(data.table)

z <- rnorm(200) # z  ~ normal(0,1)

cor(z , shift(z,1)) #NA가 나옴 다른 옵션을 지정할 필요 ㅇ

cor(z, shift(z,1), use = 'pairwise.complete.obs') #na.rm = T와 비슷한 기능
#ACF에 시각화되는 점들임

plot.ts(z, lwd = 2)
abline(h= 0 , lty = 2)

par(mfrow = c(1,2))
acf(z, ylim=c(-1,1),lag.max=24) 
pacf(z, ylim=c(-1,1),lag.max=24)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) 
plot.ts(z,col='steelblue',
        main =    paste0(' iid N(0,1)')) 
abline(h=0, col='grey', lty=2)
acf(z)
pacf(z) #백색잡음이라 할 수 있다!

#위의 과정을 한방에
tsdisplay(z) #1
ggtsdisplay(z) #ggplot으로 시각화 한번에

#특징확인에 아주 용이한 그림!!!

#확률보행과정 - 각각의 오차를 순차적으로 더하는 과정정
(a <- 1:4) 
cumsum(a) #확률보행과정 생성에 유용한 함수 cumulative sum

z = rnorm(200, 0, 1) 
rw = cumsum(z) #Random Walk
plot.ts(rw)
c(sd(z), sd(rw)) #분산이 달라짐!

z = rnorm(200, 0, 1)
rw = cumsum(z)
tsdisplay(rw,
            main = "Random Walk Process")

#ACF가 계속 높다. PACF는 절삭이 잘 되어있다.
diff_rw <- diff(rw,1) #차분 ! Zt - Zt-1
tsdisplay(diff_rw) #WN이 되었다.(정상시계열)

#절편이 있는 확률보행과정

rw_wd <- c() 
delta <- 0.3
rw_wd[1] <- delta + rnorm(1) 
for(k in 2:200){
  rw_wd[k] <- delta + rw_wd[k-1] + rnorm(1) 
}
plot.ts(rw_wd) #확실히 추세가 있어보임

rw_wd_afc <- acf(rw_wd)
rw_wd_afc[1]

#또 다른 방식 위와 같다.
n <-  200
rw_wd <-(1:n)*delta + cumsum(rnorm(n)) 
forecast::tsdisplay(rw_wd,
                    main = "Random Walk Process with Drift")
#ACF가 천천히 감소 -> 확률적 추세 존재.
#PACF 는 1차만 있고 절삭됨
# 절편의 부호에 따라서 증감 추세가 바뀜!! 기댓값의 정의..

#여러가지 데이터에 대한 시도표,ACF, PACF
t <- 1:100
z <- sin(t*pi/3)  #주 기 6 
plot.ts(z)
points(t,z, pch=16)

tsdisplay(z) #꼴을 보고 주기함수임을 느껴야한다.

#오차 부여
s <- 12
z <- sin(2*t*pi/s) + rnorm(100,0,0.4) 
tsdisplay(z)

#오차가 너무 커지면 주기성이 사라지는것처럼 보임

s <- 12
z <- sin(2*t*pi/s) + rnorm(100,0,2) 
tsdisplay(z) #WN이랑 별반 다를게 없다.

#추세가 있는 주기함수 
s <- 12
z <- 0.2*t + 2*sin(2*t*pi/s) + rnorm(100,0,0.5) 
tsdisplay(z) #ACF를 보니 확률적 추세가 있는 것 같다
#근데 이미 모형에 추세를 부여하였으니 결정적추세가 맞다.

z <- scan("population.txt") 
tsdisplay(z)
