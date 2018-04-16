# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#               
# 수업내용 : 일표본 검정 (one sample test)         #
# 작성자 : Jospeh Ahn                              #
# 작성일자 : 2018/03/27 Tuesday                    #
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

# 언제 
# 하나의 모집단의 양적 자료의 평균이 기존에 알고 있던 있던 거보다 커졌는지, 작아졌는지, 달라졌는지를 통계적으로 검정하는 방법 
 

# 1. 일표본 검정 ----
# 귀무가설 : 성인들이 평균 키는 170cm이다 
# 대립가설 : 성인들의 평균 키는 174cm보다 크다 

# 1 단계 : 정규성 검정 (Normality Test) ---- 
# 귀무가설 : 정규분포를 따른다 
# 대립가설 : 정규분포를 따르지 않는다 
# Shapiro-Wilk test: shapiro.test(data$variable) 
height <- c(180, 175, 170, 170, 165, 184, 164, 159, 181, 167, 182, 186)
shapiro.test(height)
# 결론 : 유의확률이 .478이므로 유의수준 .05에서 height는 정규분포를 따른다고 가정할 수 있다 


# 2, 일표본 T 검정 (One sample t-test) : 연구소에서는 많이 사용되나 현재는 machine learning 이나 regression 을 씀으로 one-sample t-test 는 많이 사용되지는 않응! 
# 귀무가설 : 성인들이 평균 키는 170cm이다 
# 대립가설 : 성인들의 평균 키는 174cm보다 크다 

# 유의수준: 0.05(%)
height <- c(180, 175, 170, 170, 165, 184, 164, 159, 181, 167, 182, 186)

# t.test(data$variable, mu = , alternative = )
# mu : 귀무가설의 모평균 
# alternative : 대립가설, "greater", "less", "two.sided" - 유의수준을 결정함 

height.test <- t.test(height, mu = 170, alternative = "greater")
height.test 
str(height.test)
height.test$statistic #  t 
height.test$parameter # degrees of freedom 
height.test$p.value # p-value 
height.test$conf.int # 95% confidence interval (신뢰구간)
height.test$estimate # x bar (추정치 - point estimation)
height.test$null.value # 귀무가설 
height.test$alternative # 대립가설 
height.test$method # test 이름  
height.test$data.name # 데이타 이름 

T <- (mean(height) - 170)/ (sd(height)/sqrt(11))
# 결론 : 유의확률이 .096이므로 유의수준 .05에서 성인들의 키는 통계적으로 유의하게(statistically significant) 커지지 않았다. 성인들의 키는 변화가 없다 

# 귀무가설: 성인들의 평균 용돈은 200만원이다. 
# 대립가설 : 성인들의 평균 용돈은 200만원보다 작다. 
# 유의 수준 : .05

money <- c(45, 40, 40, 50, 50, 50, 40 ,100, 50)
sd(money)
t.test(money, mu = 200, alternative = "less") 

# 2. 윌콕슨의 부호순위검정 (Wilcoxon's signed rank test) ---- 
# 정규성 가정 (Normality Assumption) 이 깨졌을 경우, Wilcoxon's signed rank test 
# wilcox.test(data$variable, mu = , alternative = )

# wilcox.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, exact = NULL, correct = TRUE, conf.int = FALSE, conf.level = 0.95, ...)

``
# 예제 : 용돈 데이타 사용 
# 1 단계 : 정규성 검정 (Normality Test)
options(scipen = 100) # 지수 표현식 사용하지 않음 
shapiro.test(money) # p-value < .05, does not satisfy the normality assumption

# 2 단계 : Wilcoxon's Signed Rank Test 
wilcox.test(money, mu = 200, alternative = "less", conf.int = TRUE, conf.level =.95) #p-value <.05, accept H1 (성인들의 평균 용돈은 200만원보다 작다). 통계적으로 유의하게 성인들의 용돈이 줄어 들었다 

# if, if ~ else, if ~elseif ~ else 

x <- 200 
if(x > 50){
  print("Large Number!!!")
}

x <- 200
if(x > 50) {
  print("Large Number!!!")
}else{
  print("Small Number!!!")
}

x <- 20
if(x > 50) {
  print("Large Number!!!")
}else if(x > 10) {
  print("Medium Number!!!")
}else{
  print("Small Number!!!")
}

x <- c(100, 10, 5, 40)
for(i in 1:length(x)){
  if(x[i] > 50) {
      print("Large Number!!!")
    }else if(x[i] > 10) {
      print("Medium Number!!!")
    }else{
      print("Small Number!!!")
    }
}


# 예제 : 귀무가설 : 모든 변수의 평균은 5이다 / 대립가설 : 평균이 5가 아니다 
# bedrooms, bathrooms, floors, waterfront, view, condition, grade 
install.packages("nortest")
library(nortest)
library(readxl)
housingDF <- readxl::read_excel(path = "Data Science/Class Data /kc_house_data.xlsx",
                                sheet = 1, 
                                col_names = TRUE)
housingDF 
analysis.variables <- c("bedrooms", "bathrooms", "floors", "waterfront", "view", "condition", "grade")

var.names <- c()
tv <- c()
pvalue <- c()
test.type <- c()

for ( i in analysis.variables) {
    norm.test <- nortest::ad.test(unlist(housingDF[ , i]))
    if(norm.test$p.value > .05) {
      result.t <- t.test(unlist(housingDF[ , i]), mu = 5, alternative = "two.sided")
      tv <- c(tv, result.t$statistic)
      pvalue <- c(pvalue, result.t$p.value)
      test.type <- c(test.type, result.t$method)
      
    }else {
      result.wilcox <- wilcox.test(unlist(housingDF[ , i]),mu = 5, alternative = "two.sided")
      tv <- c( tv, result.wilcox$statistic)
      pvalue <- c(pvalue, result.wilcox$p.value)
      test.type <- c(test.type, result.wilcox$method)
    }
}


resultDF <- data.frame(analysis.variables, tv, pvalue, test.type)
library(writexl)
writexl::write_xlsx(resultDF, path = "Class Data /resultDF.xlsx")



