library(tidyverse)
library(reshape2)
library(lmtest)

######################TSDA hw 1
###1

#### 1-1 
z <- c(52,46,46,52,50,50,48,45,41,53)
mean(z)

#### 1-2
mean(z) #all the predict values are 48.3

#### 1-3
sd_z <- sd(z) #sample standard deviation
z_h <- mean(z) #sample mean
t_z <- qt(0.975,9) #confidence level
el <- t_z*sqrt((11/10)*(sd_z^2)) #error limit
el
z_h + c(-el,el) #predict interval
###2

#### 2-1
# Zt = beta_0 + beta_1*t + epsilon_t , epsilon_t ~ i.i.d N(0,sigma^2)

#### 2-3
z_2 <- c(303,298,303,314,303,314,310,324,317,327,323,324,331,330,332)
t <- ts(1:15)
z_2 %>% mean()

reg <- lm(z_2 ~ t)
reg %>% summary()
reg %>% coef()
297.780952 + 2.385714*t #fitted model

b0 <- 297.780952 #intercept
b1 <- 2.385714 #slope

mean(1:100)

#### 2-4
c(16,17,18,19,20)*b1 + b0 #predicted values

### 3

#### 3-1
z_1 <- 100 + rnorm(100,0,1) # 100 + N(0,1)인 모의자료
z_2 <- 500 + rnorm(100,0,1) # 100 + N(0,1)인 모의자료
z_3 <- 100 + rnorm(100,0,10) # 100 + N(0,1)인 모의자료
z_4 <- 100 + seq(1,100)*rnorm(100,0,1) #100 + t*N(0,1)인 모의자료
t <- 1:100
#하나의 데이터 프레임으로 묶고 모의 시계열 자료로 만들자
ts_df <- data.frame(z_1,z_2,z_3,z_4) %>% as.ts()
ts_df #시계열 자료가 되었다.

ts_df %>% class() #출력이 mts, ts, matrix, array.. 시계열 자료로 잘 변환되었다.
#한번에 시각화

# ggplot을 사용하기 쉽게 long data frame으로 변경
ts_df_long <- ts_df %>% melt()

# ggplot으로 시각화
ggplot(ts_df_long, aes(x = Var1, y = value, color = Var2)) +
  geom_line() +
  labs(x = "ID", y = "Value", color = "Variable") +
  theme_minimal() +
  ggtitle("모의시계열자료 시각화")


#z_1과 z_2 데이터는 상수 평균모형으로 비교적 적은 분산과 각각 100 과 500근처의 값을 취한다.
#반면 z_3은 같은 상수평균모형이지만 분산이 커서 변동이 심하며
#z_4는 처음엔 분산이 크지 않지만 갈 수록 오차항에 붙은 계수가 커져 분산이 커지는 모습을 보인다.

#이론적인 평균.
# z_1= 100, z_2 = 500 , z_3 = 100, z_4 = 100
#이론적인 분산
# z_1 = 1, z_2 = 1, z_3 = 100, z_4 = 수식으로 대변 
#분산에 상수항은 영향을 끼치지 않는 점을 이용..

### 4번
t<- 1:100

z_1 <- 100 + rnorm(100,0,1) %>% ts()
z_2 <- 100 + t + rnorm(100,0,1) %>% ts()
z_3 <- 100 + 0.3*t + sin(2*pi*t/12) + cos(2*pi*t/12) + rnorm(100,0,1) %>% ts()

# 적합을 하라는 건 아닌 것 같음..
#### 4-1
par(mfrow=c(3,1))
plot(z_1,xlab = '',ylab='',main = 'z_1 시도표와 모형의 기댓값')
abline(h=100,col='blue')
#상수평균모형으로 불규칙성분만을 갖는다.

#### 4-2
plot(z_2,xlab = '',ylab='',main = 'z_2 시도표와 모형의 기댓값')
abline(a= 100, b= 1,col='green')
#선형추세모형으로 직선형의 추세를 갖는다.

#### 4-3
plot(z_3,xlab= '',ylab='', main = 'z_3 시도표와 모형의 기댓값')
curve(100 + 0.3*x + sin(2*pi*x/12) + cos(2*pi*x/12), from =1 , to = 100,add = T,col = 'red')
#선형계절추세모형으로 추세 및 계절성분을 갖는다.

### 5
books <- scan('book.txt') %>% as.ts()
t1 <- 1:30
t2 <- t1^2
ts_df <- data.frame(books,t1,t2)

#### 5-1
ggplot(data = ts_df, aes(x= t1, y=books)) +
  geom_line(col='red',lwd = 3) +
  geom_point(col = 'black', cex =3) +
  theme_minimal() +
  xlab('t') + 
  ylab('books') +
  ggtitle('시계열 그림')

#### 5-2
#이 시계열 자료는 선형추세 성분을 갖는 것으로 보인다.

predict()
#### 5-3
m1 <- lm(books ~ t1, data = ts_df)
m1 %>% coef()
#따라서 적합된 모형은 8.190805 + 3.075862*t

#### 5-4 예측값들
new_data <- data.frame(t1 = 31:42) #predict 함수를 쓰려면 데이터프레임으로 ..
predict(m1, newdata = new_data)

#### 5-5 예측구간
predict_result <- predict(m1, newdata = new_data , interval = 'predict') %>% data.frame()
ts_df

plot(t1, books,
     main = 'fitted values and predict interval',
     xlab = '', ylab = '',
     xlim = c(1,42), ylim = c(8,150),
     type='l',
     lwd=2)
lines(31:42,predict_result$lwr,col = 'skyblue',lwd = 2)
lines(31:42,predict_result$fit,col = 'darkorange',lwd = 2)
lines(31:42,predict_result$upr,col = 'skyblue',lwd = 2)

### 6
par(mfrow=c(1,1))
z <- scan('export.txt')
export <- ts(z, frequency = 12)
t<- 1:length(z)
#### 6-1
plot.ts(export , main = '월별 수출액 시계열 그림',
     xlab= '', ylab ='')

#### 6-2
#추세 및 계절성분을 갖는 것으로 보인다.

#### 6-3
ts_data <- data.frame(z = export,
                      freq = as.factor(as.integer(cycle(export))), #지시함수의 주기(가변수)
                      trend = 1:length(export)) #시간 변수


plot(t,export,type = 'l',main = '월별 수출액 시계열그림')

#모형 적합
reg <- lm(z ~ 0 + trend + freq,data = ts_data) #beta0 = 0 이라는 제약조건 
summary(reg)
reg$coefficients

#### 6-4 
reg$coefficients
#trend는 시간이 1달 씩 흐를 때 마다 0.4372092씩 수출액이 증가한다는 뜻이며
#각 회귀 계수들은 계절별 평균 수출액을 의미한다.

#잔차 분석
par(mfrow= c(1,1))
plot(ts_data$trend, resid(reg),
     pch= 16 , cex =2 , xaxt = 'n', type = 'b',
     xlab= '', ylab ='', main = '잔차의 시계열 그림')
abline(h=0,col = 'red')

shapiro.test(resid(reg)) #정규분포를 따른다고 볼 수 있다.

dwtest(reg , alternative = 'greater') #양의 상관관계 존재

bptest(reg) #유의수준 10%에서도 이분산성을 띄지 않는다 할 수 있다.

#### 6-5
new_data <- data.frame(trend = 87:98,freq = factor(1:12)) #predict 함수를 쓰려면 데이터프레임으로 ..
predict(reg, newdata = new_data)

#### 6-6
predict_result <- predict(reg, newdata = new_data, interval = 'predict') %>% data.frame()
predict_result
#### 6-7
plot(ts_data$trend, ts_data$z , type = 'l',
     main = 'fitted values and predict interval',
     xlab = '', ylab = '',
     xlim = c(1,98), ylim = c(15,100),
     lwd=2)

lines(87:98,predict_result$lwr,col = 'skyblue',lwd = 2)
lines(87:98,predict_result$fit,col = 'darkorange',lwd = 2)
lines(87:98,predict_result$upr,col = 'skyblue',lwd = 2)

plot(ts_data$trend, ts_data$z , type = 'l')
summary(reg)
anova(reg)
sqrt(19)

qt(0.975,8)