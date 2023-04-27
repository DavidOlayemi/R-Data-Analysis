data_file <- read.csv("weight_loss_data2.csv", header = TRUE)
data_file
library(lattice)
histogram(~weight_loss|Gender)
histogram(data_file$weight_loss|Gender)
histogram(data_file$weight_loss|data_file$Gender)
histogram(~data_file$weight_loss|data_file$Gender)
boxplot(data_file$weight_loss~data_file$Gender)
#show the age category we have and there frequency
table(data_file$Age_category)
barplot(table(data_file$Age_category))
#SHow the age category and there frequency in different genger
table(data_file$Gender, data_file$Age_category)
barplot(table(data_file$Gender, data_file$Age_category), beside = FALSE, legend = c("Male","Female"), ylim =c(0,7))
hist(rnorm(100,4,1.14), breaks = 7)
qqnorm(data_file$weight_loss)
qqline(data_file$weight_loss)
ks.test(data_file$weight_loss,"pnorm", mean = mean(data_file$weight_loss), sd = sd(data_file$weight_loss))
