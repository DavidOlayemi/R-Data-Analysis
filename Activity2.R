data <- sample(1:10, size=7)
summary(data)
sd(data)
range(data)
drug <- read.csv("drug_placebo.csv", header = TRUE)
drug
#pnorm(180, mean = 200, sd = 20)
QA <- pnorm(200, mean = 200, sd = 20) - pnorm(180, mean = 200, sd = 20)
m = 200
sd = 20
#curve(dnorm(QA, mean = m, sd = 20), lwd = 2)

#curve(dnorm(x, mean = m, sd = sd), lwd = 2, from = 120, to = 280, col = "black", ylab = "Normal Density", xlab = "X", cex.lab = 1.5)
#x <- seq(180, 200, length= 100)
#x
#rev(x)
#y.high <- dnorm(x, m, sd)
#y.high
#y.low <- rep(0, length(x)) #set 0 as lower boundary of area
#polygon(c(x, rev(x)), c(y.high, y.low), col = "royalblue4", border = "black", lwd = 2)
#as <- c(y.high, y.low)
#as
?polygon
#Task 2.3b
m = 200
sd = 20
pnorm(225, mean = 200, sd = 20)
curve(dnorm(x, mean = m, sd = sd), lwd = 2, from = 120, to = 280, col = "black", ylab = "Normal density", xlab = "X")
x<- seq(225, 250, length=100)
y.high2 <- dnorm(x, m, sd)
y.low2 <- rep(0, length(x))

polygon(c(x, rev(x)), c(y.high2, y.low2), col = "yellow4", border = "blue3", lwd = 2)

#Task 2.3c
pnorm(150, mean = 200, sd = 20)
m = 200
sd = 20
curve(dnorm(x, m, sd), lwd = 2, from = 120, to = 280, col = "black", ylab =  "Normal density", xlab = "X")
x <- seq(150 - 20, 150, length = 100)
y.high3 = dnorm(x, m, sd)
y.low3 = rep(0, length(x))

polygon(c(x, rev(x)), c(y.high3, y.low3), col = "brown3", border = "blue", lwd= 2)

pnorm(215, mean = 200, sd = 20) - pnorm(205, mean = 200, sd = 20)
m = 200
sd = 20
curve(dnorm(x, mean = m, sd = sd), lwd =2, col = "red", from = 120, to = 280, ylab = "Normal Density", xlab = "X")
x = seq(205, 215, length = 100)
y.high4 = dnorm(x, m, sd)
y.low4 = rep(0, length(x))

polygon(c(x, rev(x)), c(y.high4, y.low4), col = "purple3", lwd = 2, border = "yellow")