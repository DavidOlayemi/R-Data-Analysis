diabetes_file = read.csv("diabetes.csv", header = TRUE)
diabetes_file

diabetes_file["Glucose", "BloodPressure", "SkinThickness"][diabetes_file["Glucose", "BloodPressure", "SkinThickness"] == 0]==NaN
head(diabetes_file)

summary(diabetes_file)
attach(diabetes_file)
#deleting or discarding the pregnancies column
diabetes_file = subset(diabetes_file, select = -c(Pregnancies))
head(diabetes_file)
#Changing all 0 entries to NAN in each column
diabetes_file$Glucose = ifelse(diabetes_file$Glucose == 0, NaN, diabetes_file$Glucose)
diabetes_file$BloodPressure = ifelse(diabetes_file$BloodPressure == 0, NaN, diabetes_file$BloodPressure)
diabetes_file$SkinThickness = ifelse(diabetes_file$SkinThickness == 0, NaN, diabetes_file$SkinThickness)
diabetes_file$Insulin = ifelse(diabetes_file$Insulin == 0, NaN, diabetes_file$Insulin)
diabetes_file$BMI = ifelse(diabetes_file$BMI == 0, NaN, diabetes_file$BMI)
diabetes_file$DiabetesPedigreeFunction = ifelse(diabetes_file$DiabetesPedigreeFunction == 0, NaN, diabetes_file$DiabetesPedigreeFunction)
diabetes_file$Age = ifelse(diabetes_file$Age == 0, NaN, diabetes_file$Age)
head(diabetes_file)

#Visualization of each variable with there suummary Statistics.
par(mfrow = c(2,4))
hist(Glucose)
summary(Glucose)
hist(BloodPressure)
summary(BloodPressure)
hist(SkinThickness)
summary(SkinThickness)
hist(Insulin)
summary(Insulin)
hist(BMI)
summary(BMI)
hist(diabetes_file$DiabetesPedigreeFunction)
summary(DiabetesPedigreeFunction)
hist(Age)
summary(Age)
#Carrying out KS test for normality
boxplot(Glucose)
is.na(diabetes_file)
ks.test(Glucose, "pnorm", mean=mean(Glucose), sd = sd(Glucose))
ks.test(Insulin, "pnorm", mean=mean(Insulin), sd = sd(Insulin))
detach(diabetes_file)
