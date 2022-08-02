# 다중회귀분석

#01. 데이터 불러오기
ca <- read.csv("./Mdata.csv",
               header = TRUE,
               na.strings = ".")
str(ca)

attach(ca)

#02. 기본통계치 확인
library(psych)
describe(ca)
pairs.panels(ca)

#03. 다중회귀분석
mregModel <- lm(num_of_cam ~ occur + dead + ser_inj + inj)
anova(mregModel)
summary(mregModel)
library(car)
vif(mregModel) ##변수들이 서로 영향력이 있는지 보는것.(기준 10. 10보다 크면 문제가 있음)

# backword : 변수제거
mregModel1 <- lm(num_of_cam ~ 1, ca)
summary(mregModel1) 
mregModel1 <- step(mregModel, 
                   direction = "backward", trace = T)  
  # trace=F : 최종 결과만 보여줌. 
  # trace=T : 변수 들어가고 나오는것 추적해서 보여줌

# forword: 변수제거
mregModel2 <- lm(num_of_cam ~ 1, ca)
summary(mregModel2)
mregModel2 <- step(mregModel2, 
                   direction = "forward", 
                   scope =(num_of_cam ~ occur+dead+ser_inj+inj),
                   trace = T)



# 04. 회귀분석 가정 검정
# 정규성: Nomal Q-Q
# 선형성: Residuals vs Fitted
# 등분산성: Scale-Location
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1
# 2sd 이상(이하)일 경우, 

opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(mregModel)
par(opar)

# 잔차의 정규분포 검정 
shapiro.test(mregModel$residuals)

#이상치 검정, sd, hat, d 통합검정
library(car)
influencePlot(mregModel, id.method="identify")

detach(ca)
