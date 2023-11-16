library(forecast)
library(tidyverse) 
library(data.table)


#5
WN <- rnorm(10000) #백색소음

mod1 <- c()
mod1[1] <- 0 
mod1[2] <- 0 #초깃값 임의로 부여

for (i in 3:10000){ #모형에 맞게 반복문으로 자료 생성
  mod1[i] <- 9.5 + WN[i] - 1.3*WN[i-1] + 0.6*WN[i-2]
}

mod2 <- c()
mod2[1] <- 0

for (i in 2:10000){ #모형에 맞게 반복문으로 자료 생성
  mod2[i] <- 0.6*mod2[i-1] + 38 + 0.9*WN[i-1] + WN[i]
}

mod3 <- c()
mod3[1] <- 0
mod3[2] <- 0

for (i in 3:10000){ #모형에 맞게 반복문으로 자료 생성
  mod3[i] <- 26 + 0.6*mod3[i-1] + WN[i] + 0.2*WN[i-1] + 0.5*WN[i-2]
}

mod4 <- c()
mod4[1] <- 0
mod4[2] <- 0 

for ( i in 3:10000){ #모형에 맞게 반복문으로 자료 생성
  mod4[i] = 1.5*mod4[i-1] -0.7*mod4[i-1] + 100 + WN[i] - WN[i-1]
}

#5-3
acf1 <- acf(mod1)[[1]] %>% as.vector() #SACF의 내용만 벡터에 저장
acf2 <- acf(mod2)[[1]] %>% as.vector()
acf3 <- acf(mod3)[[1]] %>% as.vector()
acf4 <- acf(mod4)[[1]] %>% as.vector()

acf_df <- data.frame(acf1 = acf1[1:10],
                     acf2 = acf2[1:10],
                     acf3 = acf3[1:10],
                     acf4 = acf4[1:10])
acf_df #SACF 1 ~ 10 까지 저장 


pacf1 <- pacf(mod1)[[1]] %>% as.vector() #SPACF의 내용만 벡터에 저장
pacf2 <- pacf(mod1)[[1]] %>% as.vector()
pacf3 <- pacf(mod1)[[1]] %>% as.vector()
pacf4 <- pacf(mod1)[[1]] %>% as.vector()

pacf_df <- data.frame(pacf1 = pacf1[1:10],
                      pacf2 = pacf2[1:10],
                      pacf3 = pacf3[1:10],
                      pacf4 = pacf4[1:10])
pacf_df #SPACF 1 ~ 10 까지 저장

#5-5
par(mfrow = c(2, 2)) #한번에 보고싶다.
acf(mod1)
acf(mod2)
acf(mod3)
acf(mod4)

pacf(mod1)
pacf(mod2)
pacf(mod3)
pacf(mod4)


## 7 
par(mfrow = c(1,1))

set.seed(22034)
wn <- rnorm(100)

z <- c() #빈 벡터
z[1] = 10
for (i in 1:100) {
  z[i + 1] = 1 + z[i] * 0.9 + wn[i]
} #반복문을 통한 AR(1) Sample 생성
z <- z[-1] #관심은 t= 1 ~ 100

#7-1
plot.ts(z) #시도표

#7-2
z_acf <- acf(z)
z_acf[1:10] #값
acf(z, lag.max = 10) #시각화

#7-3
z_pacf <- pacf(z)
z_pacf[1:10] #값
pacf(z, lag.max = 10) #시각화

#7-4
plot(shift(z, 1),
     z,
     xlab = 'Z(t-1)',
     ylab = 'Z(t)',
     main = 'Lagged plot')

#1차 표본상관계수?
cor(z, shift(z, 1), use = 'pairwise.complete.obs')
#Zt 와 Zt-1 사이의 양의 상관관계를 나타낸다.
#산점도에서 보이는 점들의 상관관계가 0.903이다.

#7-5

plot(shift(z, 2),
     z,
     xlab = 'Z(t-2)',
     ylab = 'Z(t)',
     main = 'Lagged plot')
cor(z, shift(z, 2), use = 'pairwise.complete.obs')
#Zt와 Zt-2 사이의 양의 상관관계를 나타낸다.
#산점도에서 보이는 점들의 상관관계가 0.829이다.