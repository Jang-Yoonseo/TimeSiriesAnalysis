#시계열자료분석
#Chapter 03 평활법

#install.packages(c('forecast','data.table','lmtest','TTR'))

library(forecast) #ses 단순지수수 평활
library(data.table)
library(ggplot2)
library(lmtest) #dwtest
library(TTR)

getwd()
options(repr.plot.width = 15, repr.plot.height = 8)

### 이동평균법
#### Kings data
kings=scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

kingstimeseries <- ts(kings) #ts로 변환
plot(kingstimeseries, lwd=2)

## window = 3
kingstimeseriesSMA3 <- SMA(kingstimeseries, n=3)
plot.ts(kingstimeseries, lwd=2) #시도표
lines(kingstimeseriesSMA3, col = 'red', lty= 2, lwd = 2) #window =3 이동평균법 
legend("topleft", #범례례
       legend = c('original', expression(m==3)),
       col = c('black','red'),
       lty = c(1,2),lwd=2)

kingstimeseries #시계열
kingstimeseriesSMA3 #m=3 이동평균법

##window =3 vs 10
plot.ts(kingstimeseries, lwd=2)
lines(kingstimeseriesSMA3, col='red', lty=2, lwd=2)
lines(SMA(kingstimeseries,n=10), col='blue', lty=2, lwd=2)
legend("topleft",
       legend=c("original", expression(m==3), expression(m==10)), 
       col=c("black","red","blue"),
       lty=c(1,2,2), lwd=2)

#### 모형평가
##m = 3
mean((kings[-1] - kingstimeseriesSMA3[-42])^2, na.rm = T) ##MSE
mean(abs(kings[-1] - kingstimeseriesSMA3[-42]), na.rm = T) ##MAE
mean(abs(kings[-1] - kingstimeseriesSMA3[-42])/kings[-1], na.rm = T)*100 ##MAPE
#여기 부분 다시 공부해보기

##m = 10
mean((kings[-1] - SMA(kingstimeseries,n=10)[-42])^2, na.rm = T) ##MSE
mean(abs(kings[-1] - SMA(kingstimeseries,n=10)[-42]), na.rm = T) ##MAE
mean(abs(kings[-1] - SMA(kingstimeseries,n=10)[-42])/kings[-1], na.rm = T)*100 ##MAPE

#비교는 예측에 좀 더 비중이 있다.
#smoothing을 잘 할수록 오차가 커진다.

### mindex data
z <- scan('depart.txt')
depart <- ts(z, start = c(1986,1), frequency = 12)
depart %>% head()
ts.plot(depart)

#### 평활 하기
mindex_sma3 <- SMA(depart, n= 3) #평활지수 3
mindex_sma12 <- SMA(depart, n = 12) #평활지수 10

plot.ts(depart , lwd =2 )
lines(depart_sma3 , col = 'red',lty = 2, lwd = 2)
lines(depart_sma12 , col = 'blue',lty = 2, lwd = 2) #계절 조정  
#여름과 계절이 다 들어간 1년치 값들임(한 값들이) 그래서 flat하게 나타남
#원자료에서 depart_sma12를 빼면 계절 성분만 남게 됨

plot.ts(mindex - depart_sma12) #약간의 이분산성이 있어 보이지만..

### 단순지수평활법
z <- scan('mindex.txt')#mindex data이용용
mindex <- ts(z, start = c(1986,1), frequency = 12)

tmp.dat <- data.frame(day = seq.Date(as.Date('1986-01-01'),
                                     by = 'month',
                                     length.out = length(z)),
                      ind = z)
head(tmp.dat)

####시도표

ggplot(tmp.dat, aes(day, ind))+geom_line(col='skyblue', lwd=2) + 
  geom_point(col='steelblue', cex=2)+
  ggtitle("중간재 출하지수")+ 
  theme_bw()+
  theme(plot.title = element_text(size=30),
        axis.title = element_blank())

#### 단순지수 평활. alpha = 0.3
fit0 <- HoltWinters(mindex,
                    alpha = 0.3,
                    beta = F, gamma = F)
fit0
#11.2236에 해당하는 값은 마지막 평활값 1994.April을 사용하여 평활한
#1994.May의 예측값.
fit0$fitted #마지막 April값을 이용하여보자
0.3*11.4 + 0.7*11.147995

#그리고 수준만 추정하였으므로 level = x_hat
fit0$SSE #SSE 값

plot(fit0, lwd=3, cex.main = 2) #한번에 그려준다 !!
legend("topright", legend=c("observed", "ses"), lty=1,lwd=c(1,3), col=1:2)

#### 또 다른 기능 ses
##Exponential smoothing forecasts
fit01 <- ses(mindex,
             alpha = 0.3,  #평활상수 
             initial = 'simple',
             h = 10)  #앞으로 예측하고 싶은 개수

summary(fit01) #예측
#initial states: l = 9.3은 초깃값을 의미한다.
plot(fit01 , cex.main = 2) #예측에 대한 정보를 너무 잘 제공!

w <-c(seq(0.1,0.8,0.1), seq(0.81, 0.99, 0.01)) #여러 평활 상수

#평활 상수에 따른 SSE 비교
SSE_ses <- sapply(w, function(alpha) HoltWinters(mindex,
                                                 alpha=alpha, 
                                                 beta=FALSE,
                                                 gamma=FALSE)$SSE)
plot(w,SSE_ses, type="o", xlab="weight", ylab="SSE", pch=16, 
     main="1 시차 후 예측오차의 제곱합",
     cex.main = 2, cex.lab=2)

#0.8 ~ 1.0 어딘가에서 가장 SSE가 작은 평활상수값이 존재한다.
which.min(SSE_ses) #최솟값 index
w[which.min(SSE_ses)] #0.9 에서 SSE가 제일 작다. 즉 예측성능이 제일 좋다

#조금 더 구간을 tight하게 잡아서 보자
plot(w[-(1:7)],SSE_ses[-(1:7)], type="o", xlab="weight", ylab="SSE", pch=16, 
     main="1 시차 후 예측오차의 제곱합",
     cex.main = 2, cex.lab=2) #0.9에서 제일 작은것이 잘 보인다.

fit1 <- ses(mindex, alpha=w[which.min(SSE_ses)],
            initial = "simple", 
            h=6) #가장 SSE를 작게 해주는 평활상수를 이용해서 6개 예측

plot(fit1, xlab="", ylab="",
     main="중간재 출하지수와 단순지수평활값 alpha=0.9", 
     lty=1,col="black",
     cex.main = 2, cex.lab=2 ) #시도표
lines(fitted(fit1), col="red", lty=2) #예측값
legend("topright", legend=c("Mindex", expression(alpha==0.9)), 
       lty=1:2,col=c("black","red"))


fit0_w <- HoltWinters(mindex,
                      beta=FALSE, 
                      gamma=FALSE)
#alpha를 지정하지 않았다.. 알아서 한 시차후 예측 오차 제곱을
#가장 작게 해주는 값을 알려준다.

fit1 <-ses(mindex, alpah = fit0_w$alpha, h= 12)

####잔차그림
plot(fit1$residuals, ylab="residual",
     main="예측 오차의 시계열그림 : alpha=0.9", 
     cex.main = 2, cex.lab=2); abline(h=0)

#### 오차의 평균이 0인가?
t.test(fit1$residual) #p-value 엄청 큼,, 0이라고 할 수 있음
#### 오차는 독립인가?
dwtest(lm(fit1$residual~1), alternative = 'two.sided') 
#모형을 넣어야 하므로 그냥 설명변수 1에 적합
#p-값이 거의 0.94.. 독립이라고 볼 수 있다.

fit2 <- ses(mindex, alpha=0.3, h=6) #alpha = 0.3으로 설정정
plot(fit2, xlab="year", ylab="mindex",
     main=expression("중간재 출하지수와 단순지수평활값 "~alpha==0.3), 
     lty=1,col="black",
     cex.main = 2, 
     cex.lab=2)
lines(fitted(fit2), col="red", lty=2)  #적합된 값들(예측값)
legend("topright",
       legend=c("Mindex",expression(alpha==0.3)), 
       lty=1:2,
       col=c("black","red"))

plot(fit2$residuals, ylab="residual",
     main=expression("중간재 출하지수와 단순지수평활값 "~alpha==0.3), 
     cex.main = 2,
     cex.lab=2); abline(h=0)

#### 오차의 평균이 0인가?
t.test(fit2$residuals) #p-값 0.85 0이라고 볼 수 있다.
#### 오차가 독립인가?
dwtest(lm(fit2$residuals ~ 1),
       alternative = 'two.sided') 
#p-값이 매우작음.. 따라서 양의 자기상관관계가 있음을 알 수 있다.

### 이중지수 평활법
z<-scan('stock.txt')
stock <- ts(z, start = c(1984,1), frequency = 12 )

#시도표
plot(stock, main = "월별주가지수", cex.main = 2, cex.lab = 2)

#### 1모수 이중지수평활(alpha = beta)
fit4 <- holt(stock, alpha = 0.6, beta= 0.6 , h= 6)
fit4$model

#l = 115.6009 , b= 6.8098 은 아마 초깃값..
plot(fit4, ylab="", xlab="",  lty=1, col="black", 
     main="주가지수와 이중지수평활값 : alpha=beta=0.6" 
) #시도표와 6개의 예측값과 구간들
lines(fitted(fit4), col="red", lty=2) #평활값들(예측값들)
legend("topleft", lty=1:2, col=c("black","red"), 
       c("Index", "Double"), bty = "n")

#### 이중시주 평활상수 추정
fit5 = holt(stock, h= 6)
fit5$model
#위의 holt 함수는 a = 0.9999, beta = 0.1071이라는 값을 부여
#최적화의 기준은 HoltWinters 함수와 약간 다른듯 하다.

fit50 <- HoltWinters(stock,
                     gamma = F)
fit50 # 얘는 a = 1 , 0.1094451로 예측.. 비슷하지만 살짝 다르다.
#마지막 다음의 평활값 예측 a = 625.06 , b = -7.097122 
#즉 Sn(1) = 625.06 , Sn(2) = -7.097122 

#HoltWinters 함수는 예측 친화적이지만 예측값을 잘 제공해주진 않는다
#직접 predict 함수로 예측값을 구하자

predict(fit50, n.ahead = 12 , prediction.interval = T, level =0.95)
fit5 #약간은 다르지만 거의 비슷하다.

plot(fit5, ylab="Index", xlab="year",  lty=1, col="black",
     main=expression("중간재 출하지수와 이중지수평활값 : "~alpha~","~beta~ " : estimated" ),
     cex.main=2) #시도표, 마지막으로부터 예측값 6개와 예측구간
lines(fitted(fit5), col="red", lty=2) #평활된 값의 시도표표
legend("topleft", lty=1:2, col=c("black","red"), c("Index", "Double"))

#잔차 그림
plot(resid(fit5), main = expression("residual plot: "~alpha~","
                                    ,~beta~": estimated"),
     cex.main = 5)

#오차들이 서로 독립일까?
dwtest(lm(fit5$residuals ~ 1), alternative = 'two.sided')
#p-값이 매우 작게 나왔다. 오차상관관계가 존재한다.

### 계절 지수 평활
z <- scan("koreapass.txt")
pass <- ts(z, start=c(1981,1), frequency=12)

plot.ts(pass) #시도표 추세와 계절성이 모두 보인다.

#### Holt Winters additive model 
fit_hw <- HoltWinters(pass, seasonal = 'additive') #가법 모형
#계절성분은 decompose가 들어가기 때문에 frequency가 있는 ts 객체만 받음
fit_hw #한 시차 후의 예측 오차의 제곱합을 작게 해주는 평활상수 최적화.

plot(fit_hw, lwd= 2) #원래 시계열 자료와 평활한 값들의 시도표 

predict(fit_hw, n.ahead=12, prediction.interval = T, level=0.95)
#한 행을 관찰해보자
#Jan 1990 338971.3 362386.4 315556.2

#단순히 생각하자면 Xhat = level + trend + season 과 같다.

fit_60 <- hw(pass,initial = 'simple', h= 12)
fit_60 %>% summary() #최적화된 평활상수 확인
fit_60 #hw 함수로 예측한 값들의 첫 행을 관찰해보자
#Jan 1990 339004.8로 HoltWinters 함수를 사용한 예측값과 거의 비슷
#우리의 목적과 부합하는 fit_hw 의 계수를 이용하여
#hw함수를 통해 예측구간을 시각화 하자.

fit6= hw(pass,
          alpha = fit_hw$alpha, 
          beta = fit_hw$beta, 
          gamma = fit_hw$gamma, 
          seasonal="additive", 
          initial="simple",
          h=12)
fit6$model

plot(fit6,  ylab="passenger", xlab="year", lty=1, col="blue", 
     main="Winters 가법계절지수평활된 자료의 시계열 그림", 
     cex.main=2) #예측구간이 잘 생기게 되었다.
lines(fit6$fitted, lwd = 3 , col = 'red' , lty = 2)
#fit6를 통해 평활한 값 

ts.plot(fit6$residuals, ylab = 'residual',
        main = '가법 모형의 잔차 그림',
        cex.main = 2) #잔차그림은 알아서 수평선을 그려주는군..

#### dw test  오차는 독립인가?
dwtest(lm(fit6$residual~1), alternative = 'two.sided')
#독립이다.

### Holt Winters multiplicative model
fit_hw_m <- HoltWinters(pass, seasonal="multiplicative") 
fit_hw_m

#Holt Winters의 승법모형을 이용한 예측
predict(fit_hw_m, n.ahead=12, prediction.interval = T, level=0.95)
fit_hw_m$fitted

#첫 행 관찰
#Jan 1982 128439.2 135375.1  769.6180 0.9434021
#Xhat = (level + trend) * seasonal trend를 고려한 수준..
(135375.1 + 769.6180)*0.9434021 #= 128439.2 

#평활 계수들이 잘 적합되었다.
#hw 함수를 이용하여 예측을 해보자

fit7= hw(pass,
         alpha = fit_hw_m$alpha, 
         beta = fit_hw_m$beta, 
         gamma = fit_hw_m$gamma, 
         seasonal= 'multiplicative',
         initial = 'simple',
         h = 12)
fit7$model

plot(fit7,  ylab="passenger", xlab="year", lty=1, col="blue", 
     main="Winters 승법계절지수평활된 자료의 시계열 그림", 
     cex.main=2) #시도표와 예측값, 예측구간
lines(fit7$fitted, col="red", lty=2) #평활값
legend("topleft", lty=1:2, col=c("blue","red"), c("Pass", "Multiplicative"))

#### 예측성능 비교
fit_hw$SSE ; fit_hw_m$SSE
#1번 즉, 가법 모형의 예측성능이 더 우수하다.