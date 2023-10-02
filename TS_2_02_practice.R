library(lmtest)
library(lubridate)
library(tidyverse)
library(gridExtra)

getwd()
setwd("/home/tjdbswkd4321@naver.com/TimeSiriesAnalysis")


#omitted trigonometric functions..
#some visualization.
x <- seq(0, 48, 0.01)

par(mfrow = c(3, 1))
s <- 12
plot(
  x ,
  sin(2 * pi * x / s),
  type = 'l',
  col = 'steelblue',
  lwd = 2,
  xlab = '',
  ylab = '',
  main = paste0('sin::', "frequency=", s)
)
abline(h = 0, lty = 2)
abline(v = seq(0, 48, by = s), lty = 2)

s <- 6
plot(
  x ,
  sin(2 * pi * x / s),
  type = 'l',
  col = 'steelblue',
  lwd = 2,
  xlab = '',
  ylab = '',
  main = paste0('sin::', "frequency=", s)
)
abline(h = 0, lty = 2)
abline(v = seq(0, 48, by = s), lty = 2)

plot(
  x ,
  sin(2 * pi * x / s) + sin(2 * pi * x / s),
  type = 'l',
  col = 'steelblue',
  lwd = 2,
  xlab = '',
  ylab = '',
  main = paste0('sin::', "frequency=", s)
)
abline(h = 0, lty = 2)
abline(v = seq(0, 48, by = s), lty = 2)

#department store sales data
#par(mfrow = c(1,1))
dep <- scan('depart.txt')
head(dep)

plot(dep)

tmp.data <- data.frame(day = seq(ymd("1984-01-01"),
                                 by = '1 month' , length.out = length(dep)),
                       dep = dep)

tmp.data %>% head()

tmp.data$lndep <- log(dep)
tmp.data$y <-
  as.factor(as.integer(cycle(dep))) #frequency for indicator function
tmp.data$trend <- 1:length(dep) #create time variable

#I think tibble is more useful
tmp.data <- tibble(tmp.data)

tmp.data %>% head()

p1 <- ggplot(tmp.data, aes(day, dep)) +
  geom_line(col = 'skyblue', lwd = 2) +
  geom_point(col = 'steelblue', cex = 1.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  ggtitle("백화점 월별 매출액의 시도표") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_blank(),
    title = element_text(size = 15)
  )

p2 <- ggplot(tmp.data, aes(day, lndep)) +
  geom_line(col = 'skyblue', lwd = 2) +
  geom_point(col = 'steelblue', cex = 1.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
  ggtitle("로그 변환 후 백화점 월별 매출액의 시도표") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_blank(),
    title = element_text(size = 15)
  )

grid.arrange(p1, p2, nrow = 2)

#log model fitting by indicator function

reg1 <- lm(lndep ~ 0 + trend + y, data = tmp.data)
summary(reg1) #b_0 = 0

reg2 <- lm(lndep ~ trend + y , data = tmp.data)
summary(reg2) #b_1 = 0

contrasts(tmp.data$y) #범주간 대비 행렬

reg3 <- lm(lndep ~ trend + y ,
           data = tmp.data,
           contrasts = list(y = 'contr.sum')) #sum beta_i =0
# b1 + b2 + ... + b12 = 0 <=> b12 = -(b1 + b2 + ... + b11)

summary(reg3)

data.frame(
  hat_y1 = fitted(reg1),
  hat_y2 = fitted(reg2),
  hat_y3 = fitted(reg3)
) %>% head() #fitted values

#par(mfrow = c(1,1))
#dep 값이 시계열로 들가면 안됨,,
plot(
  lndep ~ day,
  tmp.data,
  main = 'obsevation vs fitted value',
  xlab = '',
  ylab = '',
  type = 'l',
  col = 'skyblue',
  lwd = 2,
  cex.axis = 2 ,
  cex.main = 2
) +
  points(
    lndep ~ day ,
    tmp.data ,
    col = 'steelblue',
    cex = 2,
    pch = 16
  ) +
  lines(
    tmp.data$day ,
    fitted(reg1),
    col = 'red',
    lty = 2 ,
    lwd = 2
  )

#Residual analysis
plot(
  tmp.data$day ,
  resid(reg),
  pch = 16,
  cex = 2,
  xaxt = 'n',
  type = 'b',
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

#dw test
dwtest(reg1 , alternative = 'greater')

#shapiro test
hist(resid(reg1))

shapiro.test(resid(reg1))
#bp test
bptest(reg1)

#fitted MSE
hat_z <- exp(fitted(reg)) #exp(log) = original value
mse_reg_indicator <- sum((z - hat_z) ^ 2) / length(z)
mse_reg_indicator

#fitting with trigonometric functions
tmp.data_2 <- data.frame(lndep = tmp.data$lndep,
                         trend = tmp.data$trend)

dt1 <-
  Reduce(cbind.data.frame, lapply(as.list(1:5), function(i)
    sin(2 * pi * i / 12 * tmp.data_2$trend)))
names(dt1) <- paste("sin", c(12, 6, 4, 3, 2.4), sep = "_")
head(dt1)

dt2 <-
  Reduce(cbind.data.frame, lapply(as.list(1:5), function(i)
    cos(2 * pi * i / 12 * tmp.data_2$trend)))
names(dt2) <- paste("cos", c(12, 6, 4, 3, 2.4), sep = "_")

tmp.data_3 <- cbind.data.frame(tmp.data_2, dt1 , dt2)
tmp.data_3 %>% dim()
tmp.data_3 %>% head()

#fitting model..

reg_sin <- lm(lndep ~ . , tmp.data_3) #all of variables by .
summary(reg_sin)


plot(
  lndep ~ tmp.data$day ,
  tmp.data_3,
  main = 'obervation vs. fitted value',
  xlab = '',
  ylab = '',
  type = 'l' ,
  col = 'skyblue',
  lwd = 2 ,
  cex.axis = 2 ,
  cex.main = 2
) +
  points(
    lndep ~ day ,
    tmp.data,
    col = 'steelblue',
    cex = 2,
    pch = 16
  ) +
  lines(
    tmp.data$day ,
    fitted(reg_sin),
    col = 'red',
    lty = 2,
    lwd = 2
  )

dwtest(reg_sin, alternative = 'two.sided')

mse_reg_indicator <- sum((z - hat_z) ^ 2) / length(z)
mse_reg_indicator #353.6691

mse_reg_sin <- sum((z  - exp(fitted(reg_sin))) ^ 2) / length(z)
mse_reg_sin #1928.976

new_data <-   data.frame(trend =    61:72,
                         y =    as.factor(1:12))
new_data

reg #by indicator b0 = 0

predict_result <- #Revert to original units by exp()
  predict(reg, newdata = new_data, interval = 'predict') %>% exp() %>% data.frame()

predict_result

#prediction visualization
plot(
  z ~ tmp.data$trend,
  tmp.data,
  main =    'observation vs. fitted value',
  xlab = "",
  ylab = "",
  xlim = c(1, 72),
  ylim = c(400, 3000),
  type = 'l',
  lwd = 2
)
lines(61:72,
      predict_result$fit,
      col = 'darkorange',
      lwd = 2)
lines(
  61:72,
  predict_result$lwr,
  col = 'steelblue',
  lwd = 2,
  lty = 2
)
lines(
  61:72,
  predict_result$upr,
  col = 'steelblue',
  lwd = 2,
  lty = 2
)
