#chapter 02 추세모형 1
#install packages

#install.packages('lmtest')
library(lmtest)
library(tidyverse)
library(lubridate)

getwd()
setwd("/home/tjdbswkd4321@naver.com/TimeSiriesAnalysis")

pop <- scan('population.txt')
head(pop)

#단위를 만단위로 만들자
pop <- round(pop / 10000)
head(pop)

tmp.data <- data.frame(
  day = seq(ymd('1960-01-01'), by = 'year',
            length.out = length(pop)),
  pop = pop,
  t = 1:length(pop),
  t2 = (1:length(pop)) ^ 2
)

tmp.data %>% head()
tmp.data %>% class()

#시도표 그리기
ggplot(data = tmp.data, aes(day, pop)) +
  geom_line(col = 'skyblue', lwd = 3) +
  geom_point(col = 'steelblue', cex = 3) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 20))

#축에 이름 추가
ggplot(data = tmp.data, aes(day, pop)) +
  geom_line(col = 'skyblue', lwd = 3) +
  geom_point(col = 'steelblue', cex = 3) +
  theme_bw() +
  theme(axis.title = element_text(size = 30),
        axis.text = element_text(size = 10))

#1차 선형 추세 모형

m1 <- lm(pop ~ t, data = tmp.data)
summary(m1) #모형 아주 유의

#fitted value와 real value 같이 그리기
plot(
  pop ~ day,
  tmp.data,
  main = 'Population : opservation vs fitted value',
  xlab = '',
  ylab = '',
  type = 'l',
  col = 'skyblue',
  lwd = 2 ,
  cex.axis = 2 ,
  cex.main = 2
) +
  points(
    pop ~ day ,
    tmp.data,
    col = 'steelblue',
    cex = 2,
    pch = 16
  ) +
  lines(
    tmp.data$day,
    fitted(m1),
    col = 'red',
    lty = 2,
    lwd = 2
  )

#residual Analysis
plot(
  tmp.data$day,
  resid(m1),
  pch = 16,
  cex = 2,
  xaxt = 'n',
  xlab = '',
  ylab = '',
  main = 'residual plot',
  cex.main = 2
)
#Add horizontal line to residual plot!
abline(
  h = 0,
  lty = 2 ,
  lwd = 2,
  col = 'red'
)

#DW test for Independence
dwtest(m1) #Positive AC..
dwtest(m1, alternative = 'two.sided') #Both side

#not adequate for this data
dwtest(m1, alternative = 'less') #for Negative

#Normal distribution test
hist(resid(m1)) # It doens't look like Normal D
shapiro.test(resid(m1)) #Do a test to residual!
#as p-value is 0.000669 it isn't folling ND

#Hetero or Homo Variation
bptest(m1)
#p-value is 0.9384 we can't say Variations are different

#Quadratic linear model
m2 <- lm(pop ~ t + t2, data = tmp.data)
summary(m2) #model is well-fitted

# Real value and fitted value plot
plot(
  pop ~ day,
  tmp.data,
  main = 'Population : opservation vs fitted value',
  xlab = '',
  ylab = '',
  type = 'l',
  col = 'skyblue',
  lwd = 2 ,
  cex.axis = 2 ,
  cex.main = 2
) +
  points(
    pop ~ day ,
    tmp.data,
    col = 'steelblue',
    cex = 2,
    pch = 16
  ) +
  lines(
    tmp.data$day,
    fitted(m2),
    col = 'red',
    lty = 2,
    lwd = 2
  )

#It seems like more fitted than m1

#Residual analysis
plot(
  tmp.data$day,
  resid(m2),
  pch = 16,
  cex = 2,
  xaxt = 'n',
  xlab = '',
  ylab = '',
  main = 'residual plot',
  cex.main = 2
)
abline(
  h = 0,
  lty = 2 ,
  lwd = 2,
  col = 'red'
)

#DW test
dwtest(m2, alternative = 'greater')
# residuals are not independent
# and have positive auto-corr

#following Normal D?
qqnorm(resid(m2), pch = 16, cex = 2)
qqline(resid(m2), col = 'red')

#I can't say something bout this.. it's ambiguous..
#Let's take a shapiro test

hist(resid(m2)) #it seems symmetric

shapiro.test(resid(m2))
#p-value is 0.1007 it follows normal

#bp test
bptest(m2)
#p-value = 0.0162..

#Log transformation
m3 <- lm(log(pop) ~ t + t2, data = tmp.data)
summary(m3) #well fitted


# Real value and fitted value plot
plot(
  log(pop) ~ day,
  tmp.data,
  main = 'Population : opservation vs fitted value',
  xlab = '',
  ylab = '',
  type = 'l',
  col = 'skyblue',
  lwd = 2 ,
  cex.axis = 2 ,
  cex.main = 2
) +
  points(
    pop ~ day ,
    tmp.data,
    col = 'steelblue',
    cex = 2,
    pch = 16
  ) +
  lines(
    tmp.data$day,
    fitted(m3),
    col = 'red',
    lty = 2,
    lwd = 2
  )

#lets take exp again..
# Real value and fitted value plot
plot(
  pop ~ day,
  tmp.data,
  main = 'Population : opservation vs fitted value',
  xlab = '',
  ylab = '',
  type = 'l',
  col = 'skyblue',
  lwd = 2 ,
  cex.axis = 2 ,
  cex.main = 2
) +
  points(
    pop ~ day ,
    tmp.data,
    col = 'steelblue',
    cex = 2,
    pch = 16
  ) +
  lines(
    tmp.data$day,
    exp(fitted(m3)),
    col = 'red',
    lty = 2,
    lwd = 2
  )

#Residual analysis
plot(
  tmp.data$day,
  resid(m3),
  pch = 16,
  cex = 2,
  xaxt = 'n',
  xlab = '',
  ylab = '',
  main = 'residual plot',
  cex.main = 2
)
abline(
  h = 0,
  lty = 2 ,
  lwd = 2,
  col = 'red'
)

#dw test
dwtest(m3, alternative = 'greater')

#Normal?
qqnorm(resid(m3), pch = 16, cex = 2)
qqline(resid(m3), col = 2 , lwd =  2)

hist(resid(m3))
shapiro.test(resid(m3))
#p- value = 0.5922
#can't say it isn't follow normal

#bp test
bptest(m3)
#p - value = 0.011

#등분산성 문제는 log or exp 변환
#양의 상관관계 문제는 5장에서..
