#### 독립표본 t-test#

#01. 데이터 불러오기
ca <- read.csv("./data.csv",
               header = TRUE,
               na.strings = ".")
str(ca)

ca$ca_yn <- factor(ca$ca_yn,
                   levels = c(0,1),
                   labels = c("무","유"))
str(ca)

attach(ca) #객체연결 (attach ->detach)

#02. 기본통계치 확인 : summary or describe(psych 패키지 이용)
#tapply(total, ca_yn,summary)

library(psych)
describeBy(dead, ca_yn, mat = T)

#03. 그래프 그리기(박스그래프, 히스토그램)
opar <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,3),2,2, byrow = TRUE)) #화면분할
boxplot(dead ~ ca_yn)
hist(dead[ca_yn == "무"])
hist(dead[ca_yn == "유"])
par(opar)

#04. 통계분석
# 등분산 검정
var.test(dead ~ ca_yn, data = ca) #0%가 동질하다 ie) 이분산임.

t.test(dead ~ ca_yn,
       data = ca,
       alternative = c("two.sided"),
       var.equal = FALSE,
       conf.level = 0.95)
detach(ca)

#05. 통계결과 그래프
mu=3.2142857
se=0.27
data <- rnorm(1000, mu,se) #
data <- sort(data)
plot(data, dnorm(data,mu,se), 
     col = "blue", 
     type = 'l',
     main = "무인카메라 설치 여부와 사망자수의 관계",
     xlim = c(0,10),   #
     ylim = c(0,5))   #
abline(v=mu, col = "blue",lty = 3)

par(new=T) 

#
mu=0.5434783
se=0.09

data <- rnorm(1000, mu,se) #
data <- sort(data)
plot(data, dnorm(data,mu,se), 
     col = 'red', 
     type = 'l',
     xlim = c(0,10),   #
     ylim = c(0,5))   #
abline(v=mu, col = "red",lty = 3)


