#1.单因素方差分析 ANOVA
library(multcomp)
table(cholesterol$trt)  
class(trt)
#trt是factor

aggregate(response, by=list(trt), FUN=mean) 
#分组。计算均值
aggregate(response, by=list(trt), FUN=sd) 
#分组。计算标准差

fit <- aov(response ~ trt,data =cholesterol )                                  
summary(fit)
#p 值小于显著性水平说明 response 在不同 trt 组之间存在显著差异。

#事后检验
tukey_result <- TukeyHSD(fit)
print(tukey_result)

#2.单因素协方差分析 ANCOVA 它是扩展了单因素方差分析 ，包含了一个或多个连续型协变量。
#ANOVA 中都是分类变量 不包含协变量。而 ANCOVA中会包含至少一个连续变量+分类变量
#老鼠的体重为因变量，自变量是不同剂量（4个组），怀孕时间为协变量

#协变量（gesttime）：连续变量，用于控制它对体重的影响。
#协变量的作用：如果不考虑 gesttime 的影响，dose 对 weight 的作用可能会被 gesttime 的变异所掩盖。
#将 gesttime 作为协变量，可以更精确地估计不同剂量对体重的影响。

data(litter, package="multcomp")
attach(litter)
#频数表
table(dose) 
table(gesttime)

aggregate(weight, by=list(dose), FUN=mean)
fit <- aov(weight ~ gesttime + dose)                             
summary(fit)
#结果显示两个变量都是显著的  F检验表明 gesttime和weight是相关的，而控制gesttime,dose和weight也相关的

#去除协变量效应后的组均值
library(effects)
effect("dose",fit)


#3.双因素协方差分析 two-way-anova
#双因素方差分析考虑的是两个分类因素的影响
#本例子中 牙齿长度是因变量 
attach(ToothGrowth)
ToothGrowth
table(supp,dose)
class(ToothGrowth$dose) #num
class(ToothGrowth$supp) #factor
#dose 原来是num的类型 单如果不变成因子 也就是分组变量的话。会被当成协变量
ToothGrowth$dose <- factor(ToothGrowth$dose)

fit <- aov(len~ supp*dose,data=ToothGrowth)
summary(fit)
#结果表明主效应（supp 和 dose ）和因子之间的交互效应都非常显著

#4.多元方差分析 MANOVA
#当因变量不止一个 就可以使用多元方差分析
library(MASS)
attach(UScereal)
#依旧是必须要先将shelf这个自变量转换为因子变量 factor
class(shelf)
UScereal$shelf <- as.factor(UScereal$shelf)

y <- cbind(calories, fat, sugars)
fit <- manova(y ~ shelf)
summary(fit)
#结果是显著的 shelf这个分组变量是显著的。可以说是三个组的卡路里之类的是有不同的是有区别的！
#这个命令将显示每个因变量的单独方差分析结果，给出每个因变量与自变量 shelf 的关系及其 p 值。这对于理解各个因变量的影响是非常有用的。
summary.aov(fit)

#针对多响应变量，分开分析 
#来判断对于对于每个因变量，不同的货架上的对比
fit1 <- aov(calories ~ shelf)
fit2 <- aov(fat ~ shelf)
fit3 <- aov(sugars ~ shelf)
tukey_result1 <- TukeyHSD(fit1)
tukey_result2 <- TukeyHSD(fit2)
tukey_result3 <- TukeyHSD(fit3)

