library(forecast) #ses 단순지수 평활
library(data.table)
library(ggplot2)
library(lmtest) #dwtest
library(TTR)
library(tidyverse)

######################TSDA hw 2
### 1번
#### 1-1
z <- scan('mindex.txt')
mindex <- ts(z, start = c(1986, 1), frequency = 12)
tmp.dat <- data.frame(day = seq.Date(
  as.Date('1986-01-01'),
  by = 'month',
  length.out = length(z)
),
ind = z)

#시도표, 불규칙 성분만 있는 것 처럼 보인다.
ggplot(tmp.dat, aes(day, ind)) + geom_line(col = 'skyblue', lwd = 2) +
  geom_point(col = 'steelblue', cex = 2) +
  ggtitle("중간재 출하지수") +
  theme_bw() +
  theme(plot.title = element_text(size = 30),
        axis.title = element_blank())

#for w = 0.6
S0_1 = 15 #초기 평활값
w = 0.6 #평활상수
S1_1 = w * z[1] + (1 - w) * S0_1 #단순지수 평활의 논리
S1_1 #시점 1에서의 1시차 후 예측값

S2_1 = w * z[2] + (1 - w) * S1_1 #이를 S2_1 에 대하여 적용
S2_1 #시점 2에서의 1시차 후 예측값값

#판단.
#S99_1을 구하려면 그 전까지의 모든 값을 알아야한다.
#for문을 이용하여 직접 구해보자

Sn_1 <- c()  # n 길이의 NA로 초기화된 벡터 생성
Sn_1

n <- length(z)

Sn_1[1] = 15 #초깃값 삽입입

for (i in 2:(n + 1)) {
  #단순지수 평활법을 적용, 첫 자리에 초기 평활이 들어갔으므로 루핑을 1번 더 해준다.
  Sn_1[i] = w * z[i - 1] + (1 - w) * Sn_1[i - 1]
}

Sn_1
#이와 같은 계산을 반복한다면.. 99와 100번째 벡터의 원소가
#각각 시점 9에서 예측한 한 시차 후의 값, 시점 99에서 예측한 한 시차 후의 값이다.
Sn_1[c(2, 3, 99, 100)] #Answer

#w= 0.2 인 경우
Sn_1_0.2 <- c()  # n 길이의 NA로 초기화된 벡터 생성

Sn_1_0.2[1] = 15 #초깃값 삽입입

w = 0.2

for (i in 2:(n + 1)) {
  #단순지수 평활법을 적용, 첫 자리에 초기 평활이 들어갔으므로 루핑을 1번 더 해준다.
  Sn_1_0.2[i] = w * z[i - 1] + (1 - w) * Sn_1_0.2[i - 1]
}

Sn_1_0.2 #w= 0.2 일 때 단순지수평활통계값들.
Sn_1_0.2[c(2, 3, 99, 100)] #Answer

### 2번
female <- scan('female.txt')
export <- scan('export.txt')
#### 2-1
#각각에 대해서 일단 시도표를 그려보자
plot.ts(female)
#불규칙 성분과 추세성분으로 구성되어있다.

female <- ts(female) #ts 객체로 변환
#예측력을 비교하는 것이 문제의 논점.
#따라서 예측오차 제곱합을 최저로 해주는
#HoltWinters를 통하여 평활 후 분석.
#이중지수평활 통계량의 초기값

fit2_1 <- holt(female)

#위 모델에서 alpha 와 beta가 최적화 되었다
plot(
  fit2_1,
  xlab = '',
  ylab = '',
  lty = 1,
  main = '원 시계열과 이중지수 평활값'
)
lines(fit2_1$fitted, col = 'red', lty = 2)
legend(
  'topleft',
  lty = 1:2,
  col = c('black', 'red'),
  legend = c('female' , 'double')
)

female_resid <- fit2_1$residuals #잔차.

#평활값과 시계열 자료의 차이인 예측 오차.

#잔차그림
plot(female_resid, type = 'l')
abline(h = 0, col = 'red')

#예측 오차는 독립일까?
dwtest(lm(female_resid ~ 1), alternative = 'two.sided')
#p-값이 0.232 이므로 오차들은 독립이다.

#예측 오차의 평균은 0일까?
t.test(female_resid)
#p-값이 0.946 이므로 오차들의 평균은 0이다.

#bptest를 시행할 순 없지만 (형식이 맞지 않는 것 같음..)
#눈으로 확인하기에 분산이 변하는 것 같진 않고
#일정 경계안에서 잔차가 있는 것 같다.

#따라서 적용한 평활법은 적절하다고 판단할 수 있다.

#### 2-2
plot.ts(export) #시도표
#불규칙 성분, 추세성분과 계절성분으로 이루어져 있다.
export <- ts(export, frequency = 12)
#계절성 고려 주기 부여

#가법 모형과 승법 모형 비교

fit2_2_add <- HoltWinters(export) #alpha,beta,gamma 모두 구함함
fit2_2_mult <- HoltWinters(export,
                           seasonal = 'multiplicative')

par(mfrow = c(1, 2))
plot(fit2_2_add) #원시계열과 가법모형을 이용한 평활값
plot(fit2_2_mult) #원시계열과 승법모형을 이용한 평활값
par(mfrow = c(1, 1))

#예측의 관점에서 최적의 모형을 찾고 싶으므로
#SSE 즉 한 시점후 오차제곱합이 낮은 모형을 선택

c(fit2_2_add$SSE, fit2_2_mult$SSE)
fit2_2_mult #이 승법모형이 더 예측력이 좋다.


#저번 과제 6번의 모형과 예측력이 서로 어떠한지 비교
ts_data <- data.frame(
  z = export,
  freq = as.factor(as.integer(cycle(export))),
  #지시함수의 주기(가변수)
  trend = 1:length(export)
)

reg <- lm(z ~ 0 + trend + freq, data = ts_data) #beta0 = 0 이라는 제약조건
sum((reg$residuals) ^ 2) #저번 과제 모형의 SSE

#SSE를 비교하려니 적합값들의 길이가 다르다.
#MSE로 비교해보자
c(fit2_2_mult$SSE / 74, sum((reg$residuals) ^ 2) / 86)
#MSE 상으로 보았을 때
#각 각 mse는 14.16571 , 15.949
#승법모형을 통한 예측이 더 정확하다고 볼 수 있다.

### 3번
z <- read.csv('data1.csv')
data1 <- ts(z$z)#시계열 데이터로 변환
t <- z$t #시점.
par(mfrow = c(1, 1))
#### 3-1
plot(data1) #불규칙 성분과 추세성분이 있다.
fit3_1 <- lm(data1 ~ t) #추세모형 적합
fit3_1$coefficients
#적합된 추세모형
#Zt_hat = 0.81893207+ 0.09961891*t 이다.

plot(data1) #잘 적합이 되었는지 시각화
abline(fit3_1, col = 'red')

#### 3-2
#위에서 적합한 선형추세모형으로 예측을 시행하자.
prdtn <- predict(fit3_1, newdata =
                   data.frame(t = 100 + (1:10)),
                 interval = 'prediction') %>% data.frame()
prdtn$fit #마지막 시점점으로부터 10개의 예측값

#### 3-3
plot(data1) #불규칙 성분과 추세성분이 있다.
#따라서 이중지수평활법을 적용한다.
fit3_3 <- HoltWinters(data1,
                      gamma = F)
fit3_3 #alpha = 0.496601 , beta = 0.396225
#적합된 식은
#Z_n(l) = 0.496601 + 0.396225*(n + l) 이다.

#alpha와 beta를 holt 함수에 적용하여 잔차를 얻자
fit3_3_holt <- holt(data1,
                    alpha = 0.496601,
                    beta = 0.396225,
                    h = 10)
plot(fit3_3_holt$residuals)
abline(h = 0, col = 'red')
#잔차분석

#잔차의 평균은 0 인가?
t.test(fit3_3_holt$residuals)
#p-value = 0.8962 이므로 잔차의 평균은 0이라 볼 수 있다.

#잔차들을 서로 독립인가?
dwtest(lm(fit3_3_holt$residuals ~ 1), alternative = 'two.sided')
#p-value = 0.008974 이며 DW 값이 4에 가까우므로
#음의 상관관계가 있음을 알 수 있다.
#아직 잔차에 예측에 대한 정보가 남아있음을 의미한다.

#### 3-4
holt_prdct <- fit3_3_holt %>%
  data.frame() #예측값, 구간 데이터 프레임에 삽입
holt_prdct$Point.Forecast #예측값 관찰

plot(
  fit3_3_holt,
  ylab = "",
  xlab = "",
  lty = 1,
  col = "black"
)
lines(fitted(fit3_3_holt), col = "red", lty = 2)
#예측 하기 전 마지막 부분의 시계열 자료가
#감소하는것을 반영하여 앞으로 관측값들은
#감소할 것이라 예측하고 있다.

#### 3-5
z_ <- read.csv('data1_new.csv')
data1_new <- ts(z_$z) #실제값.
SSE_holt <-
  sum((data1_new - holt_prdct$Point.Forecast) ^ 2) #이중평활지수 모형의 잔차
SSE_trend <- sum((data1_new - prdtn$fit) ^ 2) #선형추세모형의 잔차

SSE_trend < SSE_holt #선형 추세모형의 오차제곱합이 더 작으므로 더 적합한 모형이다.


### 4번
z4 <- scan('export.txt')

ts.plot(export) # 불규칙성분 , 추세성분과 계설성분까지 모두 있는 시계열자료
ts.plot(log(export)) #별반 나아질 게 없는 것 같다. 일단 이분산성은 없다고 하자.
#하지만 직선형 추세는 아니고 약간 곡선형 추세를 갖는다.
t <- 1:length(z4)
export <- ts(z4, frequency = 12)

#### 4-1
fit_trend <- lm(export ~ t + I(t ^ 2)) #이차항을 추가
summary(fit_trend)
#회귀계수의 추정치 : beta0 = 24.382314 beta1 = 0.870551 , beta2 = -0.004889
#적합된 추세성분은 : 24.382314 + 0.870551*t  - 0.004889*t^2 이다.
Tt_hat <- fit_trend$fitted.values # 적합된 추세모형의 fitted values

ts.plot(
  export ,
  Tt_hat,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = 'export',
  main = '추정된 추세성분'
)
#적합된 추세가 원시계열 자료를 잘 따라감을 확인할 수 있다.
#계절 성분과 불규칙 성분만 남기자(추세 성분 조정)
adjtrend <- export - Tt_hat
plot.ts(adjtrend) #계절 성분과 불규칙 성분만 남았다.

ind <- factor(cycle(adjtrend)) #가변수 생성
fit_season <- lm(adjtrend ~ 0 + ind) #beta0 = 0 이라는 제약조건건
summary(fit_season)
fit_season$coefficients #각 계수들은 각 월별 자료에 대한 평균이다.
#추정된 계절 성분은 다음과 같다.

St_hat <- fit_season$fitted.values
plot.ts(St_hat , main = '추정된 계절성분') #주기성이 한 눈에 들어온다.

#불규칙성분에 대해서 추정하여보자
It_hat <- export - Tt_hat - St_hat
ts.plot(It_hat , main = '추정된 성분')

#각 성분의 분해가 모두 끝났다.

#### 4-2

#추정된 불규칙 성분(잔차)의 평균이 0인가?
t.test(It_hat) #p-value = 1 평균이 0이라고 할 수 있다.

#추정된 불규칙 성분(잔차)는 서로 무상관인가?
dwtest(lm(It_hat ~ 1), alternative = 'two.sided')
#양측검정을 하였을 때 DW-value가 0에 가까우며 p-값은 2.2e-16 보다도 작다.
#즉 불규칙 성분(잔차)가 양의 상관관계를 갖는다고 보아야 한다.
#아직 불규치기 성분엔 예측에 대한 정보가 남아있다.

#### 4-3
export
ma(export, order = 12) #centre는 TRUE가 디폴트
#window를 주기와 같게하여 추세만 남김

#시도표와 이동평균법으로 평활된 값들을 그려보자
plot.ts(export , ylab = '', xlab = '')
lines(ma(export, order = 3) , col = 2, lty = 2) #계절 성분이 남아있다
lines(ma(export, order = 12, centre = TRUE),
      col = 3 ,
      lty = 3) #추세만 남는다
legend(
  'topleft',
  lty = 1:3,
  col = 1:3,
  c('원시계열', "CSMA(l=3)", "CSMA(l=12)"),
  bty = 'n'
)

#1 추세성분
trend = ma(export , 12)
plot.ts(trend) #추세만 남음

#2 계절성분
adj_trend = export - trend
plot.ts(adj_trend) #계절성분이 남게 되었다.
#계절별 평균을 구하자

fit44 <- lm(adj_trend ~ 0 + as.factor(cycle(adj_trend)))
fit44$coefficients #각 계절별 자료의 평균이 나왔다.

season_coef <- fit44$coefficients %>% as.vector() #벡터에 저장
mean(season_coef)

seasonal = season_coef - mean(season_coef)
#export는 7년치 자료에 8월치 몇개가 들어간 정보이다.

seasonal
St = rep(seasonal , 8) #8년치를 해두고 10개의 값을 버리면 될 것 같다.

St <- St[-(n - 0:9)] #마지막 10개의 값을 버림.

#3.불규칙 성분
irr <- export - trend - St
plot.ts(irr)

#이동평균법을 통해 모든 성분의 분해가 끝났다.

#### 4-4
plot.ts(irr)
abline(h = 0, col = 'red')

#하나의 이상점을 제외한다면 어떤 일정한 경계안에 값들이 잘 들어있는 것 같다.
#즉 이분산성은 없어보인다.

#잔차의 평균은 0인가?
t.test(irr) # p-value = 0.9412 즉 잔차 평균이 0이라고 할 수 있다.

#잔차는 서로 무상관일까?
dwtest(lm(irr ~ 1), alternative = 'two.sided') #p-값이 2.2e-16 보다 작다.
#그리고 값이 0 에 가까우므로 양의 상관관계가 있다.
#잔차에 아직예측에 대한 정보가 남아있으므로 조치가 필요하다.

#### 4-5
#각 잔차들의 제곱합을 통해 예측력을 비교하자
#근데 적용한 모형의 차이 때문에 잔차들의 개수가 다르다.
#SSE 비교보다는 MSE 비교가 더 적절한 비교 측도.
mean(It_hat ^ 2) > mean(irr ^ 2, na.rm = T)
#각각 MSE 는 11.08395 , 6.383534 이므로 이동평균법을 이용한 분해가
#예측 성능이 더 우수하다.


### 5번
z <- scan('usapass.txt')
plot.ts(z) #불규칙성분, 추세성분 그리고 계절성분이 있는 것 처럼 보이며
#갈수록 분산이 커지는 이분산성을 가지는 것으로 보인다
#### 5-1
#갈수록 분산이 커지는 이분산성을 갖고있는 자료이기 때문에
#로그를 취해서 이분산성을 없애고 분석을 시행하는 것이 적절하다.
plot.ts(log(z)) #보다 이분산성이 사라진 모습.


#### 5-2
usapass <- ts(z, frequency = 12)
log_usapass <- log(usapass)
t <- 1:length(z)
tmp.data <- data.frame(t = t, #시점과 
                       z = z) #시계열 자료 데이터프레임에 저장

tmp.data %>% head()

tmp.data$log_usapass <- log(z)
tmp.data$y <-
  as.factor(as.integer(cycle(usapass))) #지시함수 사용을 위한 주기

#tibble에 저장장
tmp.data <- tibble(tmp.data)

#지시함수를 이용하여 모형 적합
reg1 <- lm(log_usapass ~ 0 + t + y, data = tmp.data)
summary(reg1) #b_0 = 0이라는 제약조건 사용
reg1$coefficients #적합된 계수들
# 시점이 1 증가할 때 마다 log_usapass 자료는 0.01009988 씩 오르며
#y1, y2, ... y12 는 각 월별 로그 평균이다.
#적합된 모형은 다음과 같다
#log(Zt) = 0.01009988*t + 4.72469818*INDt_1 + ... 4.70328291*INDt_12
#또한 각 계수가 모두 유의한 것을 볼 수 있다.

#적합값과 원시계열 비교
ggplot(data = tmp.data, aes(x = t)) +
  geom_line(aes(y = log_usapass, color = "Log_usapass"), size = 1, linetype = "solid", alpha = 0.7) +
  geom_line(aes(y = fitted(reg1), color = "Fitted"), size = 1, linetype = "dashed", alpha = 0.7) +
  labs(title = "시계열과 회귀 모델 적합 비교") +
  scale_x_continuous(breaks = seq(1, length(z), by = 12), labels = 1:12) +
  scale_color_manual(values = c("Log_usapass" = "blue", "Fitted" = "red")) +
  theme_minimal()
#적합이 잘 된 것 같다.

#잔차 분석
plot(
  tmp.data$t ,
  resid(reg1),
  pch = 16,
  cex = 1,
  xaxt = 'n',
  type = 'b', #dot connected plot
  xlab = "",
  ylab = "",
  main = "잔차의 시계열 그림",
  cex.main = 2
)
abline(
  h = 0,
  lty = 2 ,
  lwd = 2,
  col = 'red'
)

#잔차는 무상관일까?
dwtest(reg1 , alternative = 'two.sided')
#DW 값이 0.40831이며 p-value가 2.2e-16 보다 작다.
#따라서 양의 자기상관이 있다는 것을 알 수 있다.
#아직 잔차에 예측에 대한 정보가 들어있다.

#shapiro test
hist(resid(reg1)) #0을 기준으로 대칭인 것 같다.

shapiro.test(resid(reg1))
#p-값이 0.8828 로 잔차들이 정규분포를 따른다.

#bp test, 이분산성 
bptest(reg1) #p-value = 0.9202이므로 이분산성은 없다.

t.test(resid(reg1)) #p-value가 1 따라서 잔차의 평균은 0이다.

#### 5-3

#### Holt Winters additive model
fit_hw <- HoltWinters(log_usapass, seasonal = 'additive') #로그를 취한 가법모형
fit_hw

plot(fit_hw, lwd = 2) #원래 시계열 자료와 평활한 값들의 시도표

fit_hw$alpha;fit_hw$beta;fit_hw$gamma

fit_hw2 = hw(
  log_usapass,
  alpha = fit_hw$alpha,
  beta = fit_hw$beta,
  gamma = fit_hw$gamma,
  seasonal = "additive",
  initial = "simple",
  h = 12
)
fit_hw2$model


plot(
  fit_hw2,
  ylab = "log usapass",
  xlab = "",
  lty = 1,
  col = "blue",
  main = "Winters 가법계절지수평활된 자료의 시계열 그림",
  cex.main = 2
) #예측구간이 잘 생기게 되었다.
lines(fit_hw2$fitted,
      lwd = 3 ,
      col = 'red' ,
      lty = 2)
#fit_hw2를 통해 평활한 값

ts.plot(fit_hw2$residuals) #잔차그림
abline(h=0, col = 'red')
#우뚝 솟은 두개의 값을 제외하면 이분산성을 갖지 않는 것 같다.

hist(fit_hw2$residuals) #이상점을 제외하면 잔차가 대칭인 것 같다

#### dw test  오차는 무상관인가?
dwtest(lm(fit6$residual ~ 1), alternative = 'two.sided')
#DW = 1.9794, p-value = 0.9139 이므로 독립이다.

#잔차의 평균은 0 인가?
t.test(fit_hw2$residuals)
#p-value =  0.4702 이므로 잔차의 평균은 0 이다.

#### 5-4

#모델 비교를 위해 이동평균과 추세를 이용해 각각 분해법을 적용하였습니다.

#추세에 의한 분해법
log_usapass <- ts(log(z), frequency =12)
t <- 1:length(z)
ts.plot(log_usapass) #이분산성이 사라진 모습.

fit <- lm(log_usapass ~ t) #추세 적합.
summary(fit)
#적합된 회귀계수는 beta0 = 4.8131072, beta1 = 0.0100802 이다.
#적합된 추세는 Z_t_hat = 4.8131072 + beta1*t (Z_t_hat은 로그변환 되어있다.)

hat_Tt <- fitted(fit) #추세의 적합값
ts.plot(
  log_usapass,
  hat_Tt,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "log변환한 시계열과 분해법의 추세성분 "
)
legend(
  "topleft",
  lty = 1:2,
  col = 1:2,
  lwd = 2:3,
  c("ln(z)", "추세성분 ")
)
#시각화를 해보았는데 로그변환된 관측치와 추정된 추세가 
#더 개선될 여지가 보인다. 따라서 이차항을 추가하여 적합하여보자

fit1 <- lm(log_usapass ~ t + I(t^2)) #추세 적합.
summary(fit1)
#적합된 회귀계수는 beta0 = 4.734e+00, beta1 = 1.334e-02, beta2 =-2.246e-05이다.
#적합된 추세는 Z_t_hat = 4.734e+00 + 1.334e-02*t - 2.246e-05 * t^2
#(Z_t_hat은 로그변환 되어있다.)

hat_Tt <- fitted(fit1) #추세의 적합값
ts.plot(
  log_usapass,
  hat_Tt,
  col = 1:2,
  lty = 1:2,
  lwd = 2:3,
  ylab = "food",
  xlab = "time",
  main = "log변환한 시계열과 분해법의 추세성분(이차항추가)"
)
legend(
  "topleft",
  lty = 1:2,
  col = 1:2,
  lwd = 2:3,
  c("ln(z)", "추세성분 ")
)
#보다 더 원시계열의 추세를 잘 적합하였다.


## 원시계열에서 추세성분  조정
adjtrend = log_usapass - hat_Tt
plot.ts(adjtrend, lwd = 2) #계절성분과 불규칙 성분만 남았다.

## 지시함수를 이용한 계절성분 추정
y = factor(cycle(adjtrend))  #범주형 변수로 변환
fit2 <- lm(adjtrend ~ 0 + y) #beta0 = 0 이라는 제약조건.
summary(fit2) #각 적합된 계수들은 주기별(월별) 평균값을 의미한다.
fit2$coefficients
#따라서 적합된 계절은
#St_hat = -0.08668*IND1 - 0.10897*IND2 ... -0.10788*IND12 이다.

hat_St <- fitted(fit2) #계절성분의 적합값.
ts.plot(hat_St, main = "추정된 계절성분 ")

#3 불규칙 성분
hat_It <- log_usapass - hat_Tt - hat_St
ts.plot(hat_It) #적합된 불규칙성분(잔차)들이 일정 경계안에 들어있는 것 같다.
#이분산성이 보이진 않는다.
abline(h = 0,col = 'red')

t.test(hat_It)  #H0 : mu=E(It)=0
#p-value = 1 이므로 잔차의 평균은 0이다.

dwtest(lm(hat_It ~ 1),
       alternative =  'two.sided')
#DW-값이 0에 가까우며 p-value가 2.2e-16 보다 작으므로
#잔차에 양의자기상관관계가 있음을 알 수 있다.
#따라서 잔차를 무상관하게 만들어줄 조치가 필요하다.

#이동평균을 이용한 분해법 

#1 추세성분: 계절주기와 동일한 m을 이용한 중심이동평균
trend = ma(log_usapass, 12)
plot.ts(trend, lwd = 2) #추세성분만 남게 되었다.

#2 계절성분: 추세가 조정된 시계열에서
#각 계절성분의 평균을 구한 후, 평균을
#0으로 조정

trend #앞 6게 뒤 6개 NA 값

adj_trend <- log_usapass - trend
plot.ts(adj_trend, lwd = 2) #불규칙 성분과 계절성분이 남게 되었다.

seasonal <- #계절성분만 남기도록 하자. beta = 0 인 제약조건을 사용한 회귀적합과 같다.
  tapply(adj_trend, cycle(adj_trend), function(y)
    mean(y, na.rm = T))
seasonal #적합된 계절성분 (계절별 평균)

summary(lm(adj_trend ~ 0 + as.factor(cycle(adj_trend)))) #위와 같다.
#각 계수들은 로그 시계열 자료의 월별 평균이다.

mean(seasonal)

seasonal <- seasonal - mean(seasonal)  #평균을  0으로 수정
seasonal #평균 조정이 끝난 계절성분

St = rep(seasonal, 12) #12년치 적합된 계절성분 
plot.ts(St) 

#3 불규칙 성분
irregular <- log_usapass - trend - St
plot.ts(irregular) #잔차 plot
abline(h = 0, lty = 2, col = 'red')

#잔차분석 
hist(irregular) #0을 중심으로 거의 대칭이다.
t.test(irregular) #평균이 0인가?
#p-value =0.7774 로 평균이 0 이라고 볼 수 있다.

dwtest(lm(irregular ~ 1), alternative = 'two.sided') #오차는 무상관인가?
#DW = 1.1441, p-value = 7.258e-07 이므로 잔차는 양의 자기상관관계를 갖는다.
#아직 잔차에 예측에 대한 정보가 남아있는 것으로 보인다.

#### 5-5
mean(exp(resid(reg1))^2) #추세모형 적합을 통한 잔차 = 1.007058
mean(exp(fit$residuals)^2) #평활법을 이용한 잔차 = 1.041101
mean((exp(hat_It))^2) #추세에 의한 분해법 = 1.004763
mean(exp(irregular)^2,na.rm = T) #평활에 의한 분해법 = 1.000719

#평활에 의한 분해법이 1 시차 후 예측오차의 제곱합 관점에서
#가장 예측이 잘 된 모형이다.
#그리고 추세모형, 평활법, 추세에 의한 분해법 순서로 예측이 잘 되어있다.

