##상관분석
#01. 데이터 불러오기
ca <- read.csv("./회귀분석데이터.csv",
               header = TRUE,
               na.strings = ".")
str(ca)

attach(ca)
#02. 기본통계치 확인: describe(psych패키지 이용)
library(psych)
describe(ca)
pairs.panels(ca)

plot(occur ~ num_of_cam, data = ca)
abline(lm(occur ~ num_of_cam, data = ca),
       col = "red",
       lty = 4
       )
#03. 상관분석
ca <- ca[c(-86:-87:-88:-89:-90:-91:-92),]
cor(ca, use = "complete.obs", #결측치 
    method = c("pearson")) #상관분석 돌릴 때 방법 3가지중에 피어슨상관분석을 사용한다는 뜻임.
plot(occur ~ num_of_cam, data =ca)
abline(lm(occur ~ num_of_cam, data = ca), col = "red", lty = 4)

detach(ca)


####단순회귀분석


#04. 단순회귀분석
regModel <- lm(occur ~ num_of_cam, data = ca)
anova(regModel)
summary(regModel)

#05. 회귀분석 가정 검정
#정규성 : Normal Q-Q
#선형성 : Residuals vs Fitted
#등분산성 : Scale-Location
#이상치검정: Residuals vs Leverage(cook's distance) 4/n-k-1

opar <- par(no.readonly = TRUE)
  par(mfrow = c(2,2)) #화면분할 
  plot(regModel)
par(opar) #원상복귀 


# 이상치 검정, sd, hat, d 통합검정 
library(car)
influencePlot(regModel, id.method = "edentify")

detach(ca)





# 잔차의 정규분포 검정
shapiro.test(regModel$residuals)    # 정규분포가 틀리다는겨?

# 이상치 검정, sd, hat, d 통합검정 
library(car)
influencePlot(regModel, id.method = "edentify")

detach(ca)
