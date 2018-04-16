#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# �������� : ���� 2ǥ�� ����(Indepenent Two Smple Test) #       
#                                                       #
# �� �� �� : �̺���                                     #
# �ۼ����� : 2018�� 3�� 28�� ������                     #
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


# ����
# �� ���� �������� ������ ����� ������ �ٸ�����
# ��������� �����ϴ� ���

# ���� �ڷ�(1��) : �� ����
# ���� �ڷ�(1��) : 

# �͹����� : �������� ���� ���� �뵷�� ���̰� ����(mu1 = mu2).
# �븳���� : �������� ���� ���� �뵷�� ���̰� �ִ�(mu1 is not equal to mu2).

# 1�ܰ� : ���Լ� ����(Normality Test)
by(twosampleDF$money, twosampleDF$group, shapiro.test)
# ��� : �� ���� ��� ���Լ� ������ ����
# 2�ܰ�� Wilcoxon's rank sum test�� �ǽ�


# 2�ܰ� : ���Լ� ������ ������ �Ǹ�
# ��л꼺 ����(Equality of Variance Test)
# �͹����� : ��л��̴�.
# �븳���� : �̺л��̴�.
# var.test(data$variable ~ data$variable)
# var.test(���� �ڷ� ~ ���� �ڷ�)
var.test(twosampleDF$money ~ twosampleDF$group)
by(twosampleDF$money, twosampleDF$group, var)
# ��� : ����Ȯ���� 0.025�̹Ƿ� ���Ǽ��� 0.05����
# �̺л��̴�.


# 3�ܰ� : �̺л��� ������ ���� 2ǥ�� t����
# t.test(data$variable ~ data$vairable,
#        alternative = c("greater", "less", "two.sided"),
#        var.equal   = FALSE)
t.test(twosampleDF$money ~ twosampleDF$group,
       alternative = "two.sided",
       var.equal   = FALSE)
# ��� : ����Ȯ���� 0.831�̹Ƿ� ���Ǽ��� 0.05����
# �������ڰ� �������� �뵷���� ��������� ������ ���̴� ���� ������ ��Ÿ����.


# 3�ܰ� : ��л��� ������ ���� 2ǥ�� t����
# t.test(data$variable ~ data$vairable,
#        alternative = c("greater", "less", "two.sided"),
#        var.equal   = TRUE)
t.test(twosampleDF$money ~ twosampleDF$group,
       alternative = "two.sided",
       var.equal   = TRUE)
# ��� : ����Ȯ���� 0.835�̹Ƿ� ���Ǽ��� 0.05����
# �������ڰ� �������� �뵷���� ��������� ������ ���̴� ���� ������ ��Ÿ����.


# 2�ܰ� : ���۽��� ������ ����(Wilcoxon's rank sum test)
# wilcox.test(data$variable ~ data$variable,
#             alternative = c("two.sided", "greater", "less"))
wilcox.test(twosampleDF$money ~ twosampleDF$group,
            alternative = "two.sided")
# ��� : ����Ȯ���� 0.459�̹Ƿ� ���Ǽ��� 0.05����
# �������ڰ� �������� �뵷���� ��������� ������ ���̴� ���� ������ ��Ÿ����.



houseDF <- readxl::read_excel(path      = "kc_house_data.xlsx",
                              sheet     = 1,
                              col_names = TRUE)
View(houseDF)
table(houseDF$yr_built)

# ����1
# yr_built : 1900�̻� ~ 2000�̸� : group = "old"
# yr_built : 2000�̻�            : group = "new"
# �͹����� : old�� new ���� price�� ���̰� ����.
# �븳���� : old�� new���� price�� �۴�.
houseDF$group <-  cut(houseDF$yr_built,
                      breaks = c(1900, 2000, 2020),
                      right  = FALSE)
levels(houseDF$group) <- c("old", "new")
by(houseDF$price, houseDF$group, ad.test)
wilcox.test(houseDF$price ~ houseDF$group,
            alternative = "less")

result <- var.test(houseDF$price ~ houseDF$group)
result$p.value
# ����2
# id, date, yr_built�� ������ ��� ������ ���ؼ�
# 
# �͹����� : old�� new�� ����.
# �븳���� : new�� old�� ���� �ʴ�.
# �� ���� ���������� �Ͻÿ�.
# 
# ���� ����� ������ �����ϴ�.
# variableName	Normaility	Method	    Equality	TW	   pvalue
# price	         yes	    t.test	     yes	   1.234	0.123
# bedrooms	     no	        wilcox.test	 non	   1.234	0.123
analysis.variable <- colnames(houseDF)[-grep("^id|^date|^yr_built|^group", 
                                             colnames(houseDF))]

Normality <- c()
Method    <- c()
Equality  <- c()
TW        <- c()
PValue    <- c()��

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