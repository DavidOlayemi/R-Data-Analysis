#Import the csv file
diabetes_file = read.csv("diabetes.csv", header = TRUE)
library("dplyr")
library("tidyr")
attach(diabetes_file)
#deleting or discarding the pregnancies column
diabetes_file = subset(diabetes_file, select = -c(Pregnancies))
#Changing all 0 entries to NA in each column
diabetes_file = diabetes_file %>%
  mutate_at(c('Glucose', 'BloodPressure', 'SkinThickness', 'Insulin', 'BMI', 'DiabetesPedigreeFunction', 'Age'), ~na_if(.,0))
#Visualization of each variable with there summary Statistics.
par(mfrow = c(2,4))
boxplot(Glucose, main = "Glucose", las = 1)
boxplot(BloodPressure, main = "Blood Pressure", las =1)
boxplot(SkinThickness, main = "Skin thickness", las =2)
boxplot(Insulin, main = "Insulin", las = 1)
boxplot(BMI, main = "BMI", las = 1)
boxplot(diabetes_file$DiabetesPedigreeFunction, main = "Diabetes Pedigree Function", las = 1)
boxplot(Age, main = "Age", las = 1)

#Summary Statistics of each variable
summary(diabetes_file)

#Question D
#SPlitting the predictors with the outcome 
Diab <- diabetes_file[which(Outcome == 1),]
None_Diab <- diabetes_file[which(Outcome == 0),]
Diab
None_Diab

#test for Normality for Glucose data
#QQ plot
par(mfrow = c(3,5))
qqnorm(Diab$Glucose, main = "Glucose with diabetes")
qqline(Diab$Glucose)

#KS Test
ks.test(Diab$Glucose, "pnorm", mean = mean(Diab$Glucose, na.rm = TRUE), sd = sd(Diab$Glucose, na.rm = TRUE))

#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$Glucose
#D = 0.064621, p-value = 0.2166
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$Glucose, main = "Glucose without diabetes")
qqline(None_Diab$Glucose)

#KS Test
ks.test(None_Diab$Glucose, "pnorm", mean = mean(None_Diab$Glucose, na.rm = TRUE), sd = sd(None_Diab$Glucose, na.rm = TRUE))

#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$Glucose
#D = 0.067712, p-value = 0.02098
#alternative hypothesis: two-sided

#Test for difference
wilcox.test(Diab$Glucose, None_Diab$Glucose, alternative = "two.sided")
#Wilcoxon rank sum test with continuity correction

#data:  Diab$Glucose and None_Diab$Glucose
#W = 104809, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


#Test normality for Blood pressure
#QQ Plot
qqnorm(Diab$BloodPressure, main = "Blood pressure with Diabetes")
qqline(Diab$BloodPressure)

#KS Test
ks.test(Diab$BloodPressure, "pnorm", mean = mean(Diab$BloodPressure, na.rm = TRUE), sd = sd(Diab$BloodPressure, na.rm = TRUE))

#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$BloodPressure
#D = 0.05666, p-value = 0.3935
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$BloodPressure, main = "Blood pressure without diabetes")
qqline(None_Diab$BloodPressure)
#KS test
ks.test(None_Diab$BloodPressure, "pnorm", mean = mean(None_Diab$BloodPressure, na.rm = TRUE), sd = sd(None_Diab$BloodPressure, na.rm = TRUE))

#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$BloodPressure
#D = 0.043632, p-value = 0.3191
#alternative hypothesis: two-sided

#Test for difference in blood pressure
t.test(Diab$BloodPressure, None_Diab$BloodPressure, paired = FALSE)

#Welch Two Sample t-test

#data:  Diab$BloodPressure and None_Diab$BloodPressure
#t = 4.6643, df = 504.72, p-value = 3.972e-06
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  2.572156 6.316023
#sample estimates:
#  mean of x mean of y 
#75.32143  70.87734 

#Test normality for Skin Thickness
#QQ Plot
qqnorm(Diab$SkinThickness, main = "Skin thickness with diab")
qqline(Diab$SkinThickness)
#KS Test
ks.test(Diab$SkinThickness, "pnorm", mean = mean(Diab$SkinThickness, na.rm = TRUE), sd = sd(Diab$SkinThickness, na.rm = TRUE))

#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$SkinThickness
#D = 0.061111, p-value = 0.5121
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$SkinThickness, main = "Skin thickness without diab")
qqline(None_Diab$SkinThickness)

#KS Test
ks.test(None_Diab$SkinThickness, "pnorm", mean = mean(None_Diab$SkinThickness, na.rm = TRUE), sd = sd(None_Diab$SkinThickness, na.rm = TRUE))

#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$SkinThickness
#D = 0.065305, p-value = 0.09199
#alternative hypothesis: two-sided

#Test for difference in blood pressure
t.test(Diab$SkinThickness, None_Diab$SkinThickness, paired = FALSE)

#Welch Two Sample t-test

#data:  Diab$SkinThickness and None_Diab$SkinThickness
#t = 6.1766, df = 348.51, p-value = 1.826e-09
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  3.928955 7.600131
#sample estimates:
#  mean of x mean of y 
#33.00000  27.23546 

#QQ Plot
qqnorm(Diab$Insulin, main = "Insulin with diab")
qqline(Diab$Insulin)

#KS Test
ks.test(Diab$Insulin, "pnorm", mean = mean(Diab$Insulin, na.rm = TRUE), sd = sd(Diab$Insulin, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$Insulin
#D = 0.17702, p-value = 0.0005789
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$Insulin, main = "Insulin without diab")
qqline(None_Diab$Insulin)

#KS Test
ks.test(None_Diab$Insulin, "pnorm", mean = mean(None_Diab$Insulin, na.rm = TRUE), sd = sd(None_Diab$Insulin, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$Insulin
#D = 0.16679, p-value = 8.348e-07
#alternative hypothesis: two-sided

#Difference test
wilcox.test(Diab$Insulin, None_Diab$Insulin, alternative = "two.sided")

#Wilcoxon rank sum test with continuity correction

#data:  Diab$Insulin and None_Diab$Insulin
#W = 25110, p-value = 7.477e-14
#alternative hypothesis: true location shift is not equal to 0

#QQ Plot
qqnorm(Diab$BMI, main = "BMI with diab")
qqline(Diab$BMI)

#KS Test
ks.test(Diab$BMI, "pnorm", mean = mean(Diab$BMI, na.rm = TRUE), sd = sd(Diab$BMI, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$BMI
#D = 0.092185, p-value = 0.02176
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$BMI, main = "BMI with diab")
qqline(None_Diab$BMI)

#KS Test
ks.test(None_Diab$BMI, "pnorm", mean = mean(None_Diab$BMI, na.rm = TRUE), sd = sd(None_Diab$BMI, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$BMI
#D = 0.05834, p-value = 0.07071
#alternative hypothesis: two-sided

wilcox.test(Diab$BMI, None_Diab$BMI, alternative = "two.sided")
#Wilcoxon rank sum test with continuity correction

#data:  Diab$BMI and None_Diab$BMI
#W = 89731, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

#QQ Plot
qqnorm(Diab$DiabetesPedigreeFunction, main = "DiabetesPedigreeFunction with diab")
qqline(Diab$DiabetesPedigreeFunction)

#KS Test
ks.test(Diab$DiabetesPedigreeFunction, "pnorm", mean = mean(Diab$DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(Diab$DiabetesPedigreeFunction, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$DiabetesPedigreeFunction
#D = 0.12063, p-value = 0.00082
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$DiabetesPedigreeFunction, main = "DiabetesPedigreeFunction with diab")
qqline(None_Diab$DiabetesPedigreeFunction)

#KS Test
ks.test(None_Diab$DiabetesPedigreeFunction, "pnorm", mean = mean(None_Diab$DiabetesPedigreeFunction, na.rm = TRUE), sd = sd(None_Diab$DiabetesPedigreeFunction, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$DiabetesPedigreeFunction
#D = 0.13209, p-value = 5.29e-08
#alternative hypothesis: two-sided

wilcox.test(Diab$DiabetesPedigreeFunction, None_Diab$DiabetesPedigreeFunction, alternative = "two.sided")

#Wilcoxon rank sum test with continuity correction

#data:  Diab$DiabetesPedigreeFunction and None_Diab$DiabetesPedigreeFunction
#W = 81231, p-value = 1.197e-06
#alternative hypothesis: true location shift is not equal to 0

#QQ Plot
qqnorm(Diab$Age, main = "Age with diab")
qqline(Diab$Age)

#KS Test
ks.test(Diab$Age, "pnorm", mean = mean(Diab$Age, na.rm = TRUE), sd = sd(Diab$Age, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  Diab$Age
#D = 0.099835, p-value = 0.009569
#alternative hypothesis: two-sided

#QQ Plot
qqnorm(None_Diab$Age, main = "Age without diab")
qqline(None_Diab$Age)

#KS Test
ks.test(None_Diab$Age, "pnorm", mean = mean(None_Diab$Age, na.rm = TRUE), sd = sd(None_Diab$Age, na.rm = TRUE))
#Asymptotic one-sample Kolmogorov-Smirnov test

#data:  None_Diab$Age
#D = 0.19973, p-value < 2.2e-16
#alternative hypothesis: two-sided

wilcox.test(Diab$Age, None_Diab$Age, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# 
# data:  Diab$Age and None_Diab$Age
# W = 92050, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

#Visualization of result
par(mfrow = c(2,4))
library(lattice)
boxplot(Glucose~Outcome, main = "Glucose")
boxplot(BloodPressure~Outcome, main = "Blood Pressure")
boxplot(SkinThickness~Outcome, main = "Skin Thickness")
boxplot(Insulin~Outcome, main= "Insulin")
boxplot(BMI~Outcome, main="BMI")
boxplot(diabetes_file$DiabetesPedigreeFunction~Outcome, main = "Diabetes Pedigress Function")
boxplot(Age~Outcome, main = "Age")

#Question E
plot(diabetes_file[,1:7], main = "Scatter plot to show the correlation coefficient")

#Test variable for normality
ks.test(Glucose, "pnorm", mean=mean(Glucose, na.rm = TRUE), sd=sd(Glucose, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  Glucose
# D = 0.072695, p-value = 0.0006291
# alternative hypothesis: two-sided

ks.test(BloodPressure, "pnorm", mean=mean(BloodPressure, na.rm = TRUE), sd=sd(BloodPressure, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  BloodPressure
# D = 0.046018, p-value = 0.08969
# alternative hypothesis: two-sided

ks.test(SkinThickness, "pnorm", mean=mean(SkinThickness, na.rm = TRUE), sd=sd(SkinThickness, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  SkinThickness
# D = 0.046219, p-value = 0.1981
# alternative hypothesis: two-sided

ks.test(Insulin, "pnorm", mean=mean(Insulin, na.rm = TRUE), sd=sd(Insulin, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  Insulin
# D = 0.14464, p-value = 1.384e-07
# alternative hypothesis: two-sided

ks.test(BMI, "pnorm", mean=mean(BMI, na.rm = TRUE), sd=sd(BMI, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  BMI
# D = 0.035051, p-value = 0.3102
# alternative hypothesis: two-sided

ks.test(diabetes_file$DiabetesPedigreeFunction, "pnorm", mean=mean(diabetes_file$DiabetesPedigreeFunction, na.rm = TRUE), sd=sd(diabetes_file$DiabetesPedigreeFunction, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  diabetes_file$DiabetesPedigreeFunction
# D = 0.12397, p-value = 1.121e-10
# alternative hypothesis: two-sided

ks.test(Age, "pnorm", mean=mean(Age, na.rm = TRUE), sd=sd(Age, na.rm=TRUE))
# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  Age
# D = 0.15643, p-value < 2.2e-16
# alternative hypothesis: two-sided

cor(diabetes_file[,1:7], use = "pairwise.complete.obs")

#Carrying out correlation test
cor.test(Glucose, BloodPressure, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  Glucose and BloodPressure
# t = 6.1694, df = 726, p-value = 1.139e-09
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1530103 0.2911332
# sample estimates:
#       cor 
# 0.2231918 
cor.test(Glucose, SkinThickness, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  Glucose and SkinThickness
# t = 5.4123, df = 534, p-value = 9.411e-08
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1461743 0.3068098
# sample estimates:
#       cor 
# 0.2280432 
cor.test(Glucose, Insulin, use="pairwise.complete.obs", method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Glucose and Insulin
# S = 3451562, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.6588133 
cor.test(Glucose, BMI, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  Glucose and BMI
# t = 6.5547, df = 750, p-value = 1.037e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1640065 0.2992833
# sample estimates:
#       cor 
# 0.2327705 
cor.test(Glucose, diabetes_file$DiabetesPedigreeFunction, use="pairwise.complete.obs", method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Glucose and Age
# S = 53057917, p-value = 1.499e-15
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2833146 
cor.test(Glucose, Age, use="pairwise.complete.obs", method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Glucose and Age
# S = 53057917, p-value = 1.499e-15
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2833146 
cor.test(BloodPressure, SkinThickness, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BloodPressure and SkinThickness
# t = 5.3973, df = 537, p-value = 1.017e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1451642 0.3054433
# sample estimates:
#       cor 
# 0.2268391 
cor.test(BloodPressure, Insulin, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BloodPressure and Insulin
# t = 1.9552, df = 392, p-value = 0.05127
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.0005291407  0.1951736728
# sample estimates:
#       cor 
# 0.0982723 
cor.test(BloodPressure, BMI, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BloodPressure and BMI
# t = 8.1467, df = 727, p-value = 1.63e-15
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2212643 0.3544003
# sample estimates:
#       cor 
# 0.2892303
cor.test(BloodPressure, diabetes_file$DiabetesPedigreeFunction, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BloodPressure and diabetes_file$DiabetesPedigreeFunction
# t = -0.075826, df = 731, p-value = 0.9396
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.07520380  0.06962416
# sample estimates:
#          cor 
# -0.002804527 
cor.test(BloodPressure, Age, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BloodPressure and Age
# t = 9.4551, df = 731, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2640038 0.3931245
# sample estimates:
#       cor 
# 0.3301074 
cor.test(SkinThickness, Insulin, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  SkinThickness and Insulin
# t = 3.7248, df = 392, p-value = 0.0002241
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.08769396 0.27859581
# sample estimates:
#       cor 
# 0.1848884 
cor.test(SkinThickness, BMI, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  SkinThickness and BMI
# t = 19.727, df = 537, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5964087 0.6946414
# sample estimates:
#       cor 
# 0.6482139 
cor.test(SkinThickness, diabetes_file$DiabetesPedigreeFunction, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  SkinThickness and diabetes_file$DiabetesPedigreeFunction
# t = 2.6881, df = 539, p-value = 0.007408
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.03101767 0.19740195
# sample estimates:
#       cor 
# 0.1150164 
cor.test(SkinThickness, Age, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  SkinThickness and Age
# t = 3.9279, df = 539, p-value = 9.683e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.08369321 0.24763294
# sample estimates:
#       cor 
# 0.1668158 
cor.test(Insulin, BMI, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  Insulin and BMI
# t = 4.6314, df = 391, p-value = 4.951e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1321084 0.3197587
# sample estimates:
#       cor 
# 0.2280502 

cor.test(Insulin, diabetes_file$DiabetesPedigreeFunction, use="pairwise.complete.obs", method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Insulin and diabetes_file$DiabetesPedigreeFunction
# S = 8865907, p-value = 0.009641
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1302618 
cor.test(Insulin, Age, use="pairwise.complete.obs", method = "spearman")
# Spearman's rank correlation rho
# 
# data:  Insulin and Age
# S = 7467580, p-value = 7.042e-08
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2674365 

cor.test(BMI, diabetes_file$DiabetesPedigreeFunction, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BMI and diabetes_file$DiabetesPedigreeFunction
# t = 4.322, df = 755, p-value = 1.754e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.0850669 0.2241566
# sample estimates:
#       cor 
# 0.1553817 
cor.test(BMI, Age, use="pairwise.complete.obs", method = "pearson")
# Pearson's product-moment correlation
# 
# data:  BMI and Age
# t = 0.71029, df = 755, p-value = 0.4777
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.04549903  0.09691970
# sample estimates:
#        cor 
# 0.02584146 
cor.test(diabetes_file$DiabetesPedigreeFunction, Age, use="pairwise.complete.obs", method = "spearman")
# Spearman's rank correlation rho
# 
# data:  diabetes_file$DiabetesPedigreeFunction and Age
# S = 72257860, p-value = 0.2349
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.04290859 

#Question F
Reg_diab <- glm(Outcome~(.),family = binomial, data = diabetes_file)
summary(Reg_diab)
exp(coefficients(Reg_diab))
exp(confint(Reg_diab))
# Call:  glm(formula = Outcome ~ (.), family = binomial, data = diabetes_file)
# 
# Coefficients:
#   (Intercept)                   Glucose  
# -1.016e+01                 3.819e-02  
# BloodPressure             SkinThickness  
# -1.085e-03                 1.169e-02  
# Insulin                       BMI  
# -9.424e-04                 6.660e-02  
# DiabetesPedigreeFunction                       Age  
# 1.079e+00                 5.203e-02  
# 
# Degrees of Freedom: 391 Total (i.e. Null);  384 Residual
# (376 observations deleted due to missingness)
# Null Deviance:	    498.1 
# Residual Deviance: 346.2 	AIC: 362.2

#****************************************************
# Call:
#   glm(formula = Outcome ~ (.), family = binomial, data = diabetes_file)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7814  -0.6675  -0.3699   0.6474   2.5697  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -1.016e+01  1.209e+00  -8.409  < 2e-16 ***
#   Glucose                   3.819e-02  5.783e-03   6.605 3.97e-11 ***
#   BloodPressure            -1.085e-03  1.174e-02  -0.092 0.926379    
# SkinThickness             1.169e-02  1.715e-02   0.681 0.495593    
# Insulin                  -9.424e-04  1.327e-03  -0.710 0.477683    
# BMI                       6.660e-02  2.712e-02   2.456 0.014046 *  
#   DiabetesPedigreeFunction  1.079e+00  4.228e-01   2.551 0.010729 *  
#   Age                       5.203e-02  1.425e-02   3.652 0.000261 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 498.10  on 391  degrees of freedom
# Residual deviance: 346.24  on 384  degrees of freedom
# (376 observations deleted due to missingness)
# AIC: 362.24
# 
# Number of Fisher Scoring iterations: 5

# > exp(coefficients(Reg_diab))
# (Intercept)                  Glucose 
# 3.849535e-05             1.038934e+00 
# BloodPressure            SkinThickness 
# 9.989157e-01             1.011754e+00 
# Insulin                      BMI 
# 9.990580e-01             1.068865e+00 
# DiabetesPedigreeFunction                      Age 
# 2.940785e+00             1.053409e+00 

# > exp(confint(Reg_diab))
# Waiting for profiling to be done...
# 2.5 %       97.5 %
#   (Intercept)              3.191755e-06 0.0003696799
# Glucose                  1.027617e+00 1.0512608404
# BloodPressure            9.762987e-01 1.0224769462
# SkinThickness            9.781720e-01 1.0463902667
# Insulin                  9.964640e-01 1.0016870072
# BMI                      1.014225e+00 1.1285080203
# DiabetesPedigreeFunction 1.306789e+00 6.8578092932
# Age                      1.024953e+00 1.0840173675

#Question F
#Using simple linear regression model
simple_diab <- lm(Glucose ~ Age)
simple_diab

# Call:
#   lm(formula = Glucose ~ Age)
# 
# Coefficients:
#   (Intercept)          Age  
# 98.6324       0.6929  

null_data <- diabetes_file %>%
  filter(is.na(Glucose))

null_data
predict(simple_diab, newdata = null_data)
# 1        2        3        4        5 
# 113.8767 113.1837 113.8767 124.2705 127.0421 






plot(diabetes_file[,1:7], main = "Scatter plot to show the correlation coefficient")
cor(diabetes_file[,1:7], use = "pairwise.complete.obs")


write.csv(Diab, "Diab.csv")
write.csv(None_Diab, "None_Diab.csv")
library(lattice)
histogram(~Glucose|Outcome)
t.test(Glucose_Diab$Glucose, Glucose_NoneDiab$Glucose, paired = FALSE)

ks.test(Glucose_Diab$Glucose, "pnorm", mean= mean(Glucose_Diab$Glucose), sd = sd(Glucose_Diab$Glucose))

Glucose_Diab
Glucose_NoneDiab
ks.test(Glucose, "pnorm", mean = mean(Glucose, na.rm = TRUE), sd = sd(Glucose, na.rm = TRUE), alternative = "less")

?ks.test
