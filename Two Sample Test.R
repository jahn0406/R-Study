#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# 수업내용 : 독립 2표본 검정(Indepenent Two Smple Test) #       
#                                                       #
# 작 성 자 : 이부일                                     #
# 작성일자 : 2018년 3월 28일 수요일                     #
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

install.packages("readxl")
install.packages("nortest")
install.packages("writexl")
library(readxl)
library(nortest)
library(writexl)


setwd("d:/da/")


twosampleDF <- readxl::read_excel(path      = "twosample.xlsx",
                                  sheet     = 1,
                                  col_names = TRUE)

View(twosampleDF)
str(twosampleDF)
twosampleDF$group <- as.factor(twosampleDF$group)


# 언제
# 두 개의 독립적인 집단의 평균이 같은지 다른지를
# 통계적으로 검정하는 방법

# 질적 자료(1개) : 두 집단
# 양적 자료(1개) : 

# 귀무가설 : 비졸업과 졸업 간에 용돈에 차이가 없다(mu1 = mu2).
# 대립가설 : 비졸업과 졸업 간에 용돈에 차이가 있다(mu1 is not equal to mu2).

# 1단계 : 정규성 검정(Normality Test)
by(twosampleDF$money, twosampleDF$group, shapiro.test)
# 결론 : 두 집단 모두 정규성 가정이 깨짐
# 2단계로 Wilcoxon's rank sum test를 실시


# 2단계 : 정규성 가정이 만족이 되면
# 등분산성 검정(Equality of Variance Test)
# 귀무가설 : 등분산이다.
# 대립가설 : 이분산이다.
# var.test(data$variable ~ data$variable)
# var.test(양적 자료 ~ 질적 자료)
var.test(twosampleDF$money ~ twosampleDF$group)
by(twosampleDF$money, twosampleDF$group, var)
# 결론 : 유의확률이 0.025이므로 유의수준 0.05에서
# 이분산이다.


# 3단계 : 이분산이 가정된 독립 2표본 t검정
# t.test(data$variable ~ data$vairable,
#        alternative = c("greater", "less", "two.sided"),
#        var.equal   = FALSE)
t.test(twosampleDF$money ~ twosampleDF$group,
       alternative = "two.sided",
       var.equal   = FALSE)
# 결론 : 유의확률이 0.831이므로 유의수준 0.05에서
# 비졸업자과 졸업자의 용돈에는 통계적으로 유의한 차이는 없는 것으로 나타났다.


# 3단계 : 등분산이 가정된 독립 2표본 t검정
# t.test(data$variable ~ data$vairable,
#        alternative = c("greater", "less", "two.sided"),
#        var.equal   = TRUE)
t.test(twosampleDF$money ~ twosampleDF$group,
       alternative = "two.sided",
       var.equal   = TRUE)
# 결론 : 유의확률이 0.835이므로 유의수준 0.05에서
# 비졸업자과 졸업자의 용돈에는 통계적으로 유의한 차이는 없는 것으로 나타났다.


# 2단계 : 윌콕슨의 순위합 검정(Wilcoxon's rank sum test)
# wilcox.test(data$variable ~ data$variable,
#             alternative = c("two.sided", "greater", "less"))
wilcox.test(twosampleDF$money ~ twosampleDF$group,
            alternative = "two.sided")
# 결론 : 유의확률이 0.459이므로 유의수준 0.05에서
# 비졸업자과 졸업자의 용돈에는 통계적으로 유의한 차이는 없는 것으로 나타났다.



houseDF <- readxl::read_excel(path      = "kc_house_data.xlsx",
                              sheet     = 1,
                              col_names = TRUE)
View(houseDF)
table(houseDF$yr_built)

# 문제1
# yr_built : 1900이상 ~ 2000미만 : group = "old"
# yr_built : 2000이상            : group = "new"
# 귀무가설 : old와 new 간에 price에 차이가 없다.
# 대립가설 : old가 new보다 price가 작다.
houseDF$group <-  cut(houseDF$yr_built,
                      breaks = c(1900, 2000, 2020),
                      right  = FALSE)
levels(houseDF$group) <- c("old", "new")
by(houseDF$price, houseDF$group, ad.test)
wilcox.test(houseDF$price ~ houseDF$group,
            alternative = "less")

result <- var.test(houseDF$price ~ houseDF$group)
result$p.value
# 문제2
# id, date, yr_built를 제외한 모든 변수에 대해서
# 
# 귀무가설 : old와 new는 같다.
# 대립가설 : new와 old는 같지 않다.
# 에 대한 가설검정을 하시오.
# 
# 최종 결과는 다음과 같습니다.
# variableName	Normaility	Method	    Equality	TW	   pvalue
# price	         yes	    t.test	     yes	   1.234	0.123
# bedrooms	     no	        wilcox.test	 non	   1.234	0.123
analysis.variable <- colnames(houseDF)[-grep("^id|^date|^yr_built|^group", 
                                             colnames(houseDF))]

Normality <- c()
Method    <- c()
Equality  <- c()
TW        <- c()
PValue    <- c()　

for(i in analysis.variable){
    result.normality <- by(unlist(houseDF[ , i]), houseDF$group, ad.test)
    if((result.normality$old$p.value < 0.05) | (result.normality$new$p.value < 0.05)){
        Normality <- c(Normality, "No")
        Method    <- c(Method, "wilcox.test")
        Equality  <- c(Equality, "Non")
        result.wilcox <- wilcox.test(unlist(houseDF[ , i])~ houseDF$group,
                                     alternative = "two.sided")
        TW     <- c(TW, result.wilcox$statistic)
        PValue <- c(PValue, result.wilcox$p.value)
    }else{
        Normality <- c(Normality, "Yes")
        Method    <- c(Method, "t.test")
        result.equality <- var.test(unlist(houseDF[ , i])~ houseDF$group)
        if(result.equality$p.value < 0.05){
            Equality  <- c(Equality, "No")
            result.ttest <- t.test(unlist(houseDF[ , i])~ houseDF$group,
                                   alternative = "two.sided",
                                   var.equal   = FALSE)
            TW     <- c(TW, result.ttest$statistic)
            PValue <- c(PValue, result.ttest$p.value)
        }else{
            Equality  <- c(Equality, "Yes")
            result.ttest <- t.test(unlist(houseDF[ , i])~ houseDF$group,
                                   alternative = "two.sided",
                                   var.equal   = TRUE)
            TW     <- c(TW, result.ttest$statistic)
            PValue <- c(PValue, result.ttest$p.value)
        }
    }
}


outputTest <- data.frame(Variable = analysis.variable,
                         Normality,
                         Method,
                         Equality,
                         TW,
                         PValue)
writexl::write_xlsx(outputTest, path = "outputTest.xlsx")
