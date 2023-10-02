# ts 함수 알아가기
library(tidyverse)

x <- 1:10
class(x) # 클래스는 숫자 벡터

x <- ts(x)
x
class(x) # 클래스가 ts가 됨

x <- ts(rnorm(20), start = c(2000, 1) , frequency = 4)
cycle(x) #몇번째 사이클인지 보여줌. 즉 몇분기인지 알려줌

time(x) #비슷한데 별로임

x <- rnorm(100)
class(x)

t <- 1:100
plot(t, x, type = 'l') #실선
plot(t, x, type = 'b') #닷 커넥티드 플랏

y <- as.ts(x)
class(y)
plot(y) #ts는 알아서 실선그래프 그림
plot.ts(x) #일반 벡터를 ts 그래프로
ts.plot(x) #같음

z <-
  matrix(rnorm(300), 100, 3) %>% ts(start = c(2000), frequency = 12)
z %>% plot() #매트릭스를 ts화
#그림은 3개를 나눠줌 (열별로 그림)

plot.ts(z) #같다

#한번에 그림 그리기
plot.ts(z,
        plot.type = 'single',
        col = 1:3,
        lty = 1:3) #그림을 한번에

ts.plot(z , col = 1:3) #같다
head(iris)

t.iris <- as.ts(iris[, -5])
t.iris %>% class() #행렬시계열,시계열,행렬,어레이의 모든 특성을 갖는다.
plot.ts(t.iris) #각 변수별로 시도표 그리기
ts.plot(t.iris , col = 1:4) #색 변경

#날짜 다루기
Sys.Date()
date()
a <- Sys.Date()
class(a) #Date라는 클래스
a <- "2023-09-12"
class(a) #얘는 일반 문자열
a_date <- as.Date(a) #Date로 만들어줌
a_date %>% class()

a <- as.Date(c('2023-09-12', '2024-01-01'))
a
a[2] - a[1] #사칙연산이 가능하다!

a_chr <- as.character(a)
a_chr

a_chr[2] - a_chr[1] #문자열 뺄셈은 당연히 안됨

a[1] + 7 #날짜에 7일이 더해짐
a[1] - 7 #날짜에 7일이 빠짐
a[1]

a[1] #2023-09-12
format(a[1], #September/09/12/23/2023
       format = "%b-%D-%Y")

format(a[1],
       format = "%b-%d-%y")

#format 알기
#B : September ,b: Sep
#D : 월,연,일 다 적음 , d: 일만 적음
#Y : 2023 y:23

b <- as.Date("09/12/23" , format = '%m/%d/%y')
b #입력의 자리값을 알려주면 연월일로 정렬
weekdays(b)

################################
#lubridate 알기
library(lubridate)
a <- today()
a %>% class() #Date라는 클래스

year(a)
month(a)
day(a)
wday(a) #몇요일인가요 ? 1이 일요일
wday(a , label = T) #숫자말고
now() #시간도 반환
class(now()) #??
#POSIXct: 날짜 형태 , #POSIXt: 전세계 규격 시간
now <- now()

hour(now) #몇시?
minute(now) #몇분?
second(now) #몇초?

#문자열을 순서에 맞게 Date로 변경
ymd('2023-09-12') %>% class()
mdy('10-11-12') #월-일-연 을 정렬
ymd(230106) #숫자도 받는다

a <- ymd_hms("2023-09-12 09:00:00",
             tz = "Asia/Seoul")
a

#교수님이 주신 코드가 왜 안먹지..?

ym(202309) #ym만 하면 1일이 디폴트
hms('11:30:00') #뒤에 알파벳 매칭
hm("11:30")
now <- now()

update(now, years = 2000) #연도를 2000으로 변경

ys <- ts('2023-09-12')
format(a[1], format = '%y/%b/%d')

#as.Date와 format은 상당히 제한적..
as.Date('10/02/23', format = '%m/%d/%y')
