#800 families with 5 children
#Task 1a: 5 boys
#dbinom(x, size, prob, log = FALSE)
?dbinom
?plot
dbinom(5, 5, 0.5)
plot(0:5, dbinom(0:5, 5, 0.5), type = 'h', ylim = c(0, 0.5), lwd = 5)
?lines
lines(c(5,5), c(0, dbinom(5,5,0.5)), col = "red1", lwd = 10)

#Task 1b: 2 girls
dbinom(2, 5, 0.5)
plot(0:5, dbinom(0:5, 5, 0.5), type = 'h', ylim = c(0, 0.5), lwd = 3)
lines(c(2,2), c(0, dbinom(2, 5, 0.5)), col = 'red1', lwd = 5)

#Task 1c: either 2 or 3 girls
Task = dbinom(2, 5, 0.5)  + dbinom(3, 5, 0.5)
Task
plot(0:5, dbinom(0:5, 5, 0.5), type = 'h', ylim = c(0, 0.5), lwd = 3)
lines(c(2,2), c(0, dbinom(2, 5, 0.5)), col = 'red1', lwd = 5)
lines(c(3,3), c(0, dbinom(3, 5, 0.5)), col = 'red1', lwd = 5)

#Task 2.1
obesityData <- read.csv("obesity_data.csv", head = TRUE)
obesityData
attach(obesityData)
?par
## Visually test the normality of drug_WL and placebo_WL
#   QQ Plot

par(mfrow = c(1, 2)) #allow more than one graph to be displayed on one screen
?qqnorm
qqnorm(drug_WL, main = "Weight loss with drug")
#   Add the QQ line
?qqline
qqline(drug_WL)
#   QQ Plot
qqnorm(placebo_WL, main = "weight loss with placebo")
#   Add the QQ line
qqline(placebo_WL)
# Kolmogorov-Smirnov test
ks.test(drug_WL, "pnorm", mean = mean(drug_WL), sd = sd(drug_WL))

ks.test(placebo_WL,"pnorm", mean = mean(placebo_WL), sd = sd(placebo_WL))

## T test
#
t.test(drug_WL,placebo_WL,paired = TRUE)
## 

detach(obesityData)
obesityData$diff <- obesityData$drug_WL - obesityData$placebo_WL
obesityData
qqnorm(obesityData$diff, main = "Difference btw drug and place")
qqline(obesityData$diff)
ks.test(obesityData$diff,"pnorm", mean = mean(obesityData$diff), sd = sd(obesityData$diff))
t.test(obesityData$diff)

#Task1.3
b <- sample(1:1000, size = 10000, replace = TRUE)
par(mfrow = c(1, 1))
hist(b)
c <- sample(1:1000, size = 10000, replace = TRUE)
d <- b + c
hist(d)
e <- sample(1:1000, size = 10000, replace = TRUE)
f <- sample(1:1000, size = 10000, replace = TRUE)
a <- sample(1:1000, size = 10000, replace = TRUE)
q <- a + e + f +b +c
hist(q)

#Task2.2
ab <- rnorm(15, mean = 10, sd = 2)
ks.test(ab,"pnorm", mean = 10, sd = 2)
ab
t.test(ab, mu = 10)


#Task2.3
endorphin_data <- read.csv("endorphin_concentration.csv", header = TRUE)
endorphin_data
attach(endorphin_data)
par(mfrow = c(1, 2))
qqnorm(Joggers, main = "Joggers")
qqline(Joggers)
qqnorm(New_entrants, main = "New Entrants")
qqline(New_entrants)
ks.test(Joggers, "pnorm", mean = mean(Joggers, na.rm = TRUE), sd = sd(Joggers, na.rm = TRUE))
ks.test(New_entrants, "pnorm", mean = mean(New_entrants), sd = sd(New_entrants))
t.test(Joggers, New_entrants, paired = FALSE) #why are we using FALSE for paired
detach(endorphin_data)
