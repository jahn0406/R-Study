# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#               
# �������� : ��ǥ�� ���� (one sample test)         #
# �ۼ��� : Jospeh Ahn                              #
# �ۼ����� : 2018/03/27 Tuesday                    #
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

# ���� 
# �ϳ��� �������� ���� �ڷ��� ����� ������ �˰� �ִ� �ִ� �ź��� Ŀ������, �۾�������, �޶��������� ��������� �����ϴ� ��� 
 

# 1. ��ǥ�� ���� ----
# �͹����� : ���ε��� ��� Ű�� 170cm�̴� 
# �븳���� : ���ε��� ��� Ű�� 174cm���� ũ�� 

# 1 �ܰ� : ���Լ� ���� (Normality Test) ---- 
# �͹����� : ���Ժ����� ������ 
# �븳���� : ���Ժ����� ������ �ʴ´� 
# Shapiro-Wilk test: shapiro.test(data$variable) 
height <- c(180, 175, 170, 170, 165, 184, 164, 159, 181, 167, 182, 186)
shapiro.test(height)
# ��� : ����Ȯ���� .478�̹Ƿ� ���Ǽ��� .05���� height�� ���Ժ����� �����ٰ� ������ �� �ִ� 


# 2, ��ǥ�� T ���� (One sample t-test) : �����ҿ����� ���� ���ǳ� ����� machine learning �̳� regression �� ������ one-sample t-test �� ���� �������� ����! 
# �͹����� : ���ε��� ��� Ű�� 170cm�̴� 
# �븳���� : ���ε��� ��� Ű�� 174cm���� ũ�� 

# ���Ǽ���: 0.05(%)
height <- c(180, 175, 170, 170, 165, 184, 164, 159, 181, 167, 182, 186)

# t.test(data$variable, mu = , alternative = )
# mu : �͹������� ����� 
# alternative : �븳����, "greater", "less", "two.sided" - ���Ǽ����� ������ 

height.test <- t.test(height, mu = 170, alternative = "greater")
height.test 
str(height.test)
height.test$statistic #  t 
height.test$parameter # degrees of freedom 
height.test$p.value # p-value 
height.test$conf.int # 95% confidence interval (�ŷڱ���)
height.test$estimate # x bar (����ġ - point estimation)
height.test$null.value # �͹����� 
height.test$alternative # �븳���� 
height.test$method # test �̸�  
height.test$data.name # ����Ÿ �̸� 

T <- (mean(height) - 170)/ (sd(height)/sqrt(11))
# ��� : ����Ȯ���� .096�̹Ƿ� ���Ǽ��� .05���� ���ε��� Ű�� ��������� �����ϰ�(statistically significant) Ŀ���� �ʾҴ�. ���ε��� Ű�� ��ȭ�� ���� 

# �͹�����: ���ε��� ��� �뵷�� 200�����̴�. 
# �븳���� : ���ε��� ��� �뵷�� 200�������� �۴�. 
# ���� ���� : .05

money <- c(45, 40, 40, 50, 50, 50, 40 ,100, 50)
sd(money)
t.test(money, mu = 200, alternative = "less") 

# 2. ���۽��� ��ȣ�������� (Wilcoxon's signed rank test) ---- 
# ���Լ� ���� (Normality Assumption) �� ������ ���, Wilcoxon's signed rank test 
# wilcox.test(data$variable, mu = , alternative = )

# wilcox.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, exact = NULL, correct = TRUE, conf.int = FALSE, conf.level = 0.95, ...)

``
# ���� : �뵷 ����Ÿ ��� 
# 1 �ܰ� : ���Լ� ���� (Normality Test)
options(scipen = 100) # ���� ǥ���� ������� ���� 
shapiro.test(money) # p-value < .05, does not satisfy the normality assumption

# 2 �ܰ� : Wilcoxon's Signed Rank Test 
wilcox.test(money, mu = 200, alternative = "less", conf.int = TRUE, conf.level =.95) #p-value <.05, accept H1 (���ε��� ��� �뵷�� 200�������� �۴�). ��������� �����ϰ� ���ε��� �뵷�� �پ� ����� 

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


# ���� : �͹����� : ��� ������ ����� 5�̴� / �븳���� : ����� 5�� �ƴϴ� 
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


