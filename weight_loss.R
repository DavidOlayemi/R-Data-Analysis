Weight_Loss <- read.csv("weight_loss_data.csv", header = TRUE)
head(Weight_Loss)
deciles <- seq(20, 70, by = 10)
cut <- cut(Weight_Loss$Age, breaks = deciles)
cut
Weight_Loss$Age_Category <- rep(0,16)#number of cases = 16
Weight_Loss$Age_Category[which(cut == levels(cut)[1])] <- 1
Weight_Loss$Age_Category[which(cut == levels(cut)[2])] <- 2
Weight_Loss$Age_Category[which(cut == levels(cut)[3])] <- 3
Weight_Loss$Age_Category[which(cut == levels(cut)[4])] <- 4
Weight_Loss$Age_Category[which(cut == levels(cut)[5])] <- 5
Weight_Loss
Weight_Loss$weight_loss_col <- Weight_Loss$Initial_weight - Weight_Loss$Final_weight
Weight_Loss
Weight_Loss_Male <- Weight_Loss[Weight_Loss$Gender=="male",]
Weight_Loss_Male

#Generating random numbers
RS <- sample(1:49, size = 6)
RS
sort(RS)
attach(Weight_Loss)
range_weight_loss <- max(weight_loss_col)-min(weight_loss_col)
range_weight_loss
wl_edges = seq(-1.25, 18.75, by = 2.5)
wl_cuts = cut(weight_loss_col, breaks=wl_edges)
wl_cuts
table(wl_cuts)
hist(weight_loss_col, breaks = wl_edges)
summary(Weight_Loss)
boxplot(weight_loss_col)
library(lattice)
histogram(~weight_loss_col|Gender)
boxplot(weight_loss_col~Gender)
table(Age_Category)
barplot(table(Age_Category))
table(Gender, Age_Category)
barplot(table(Gender, Age_Category), beside = TRUE, legend = c("Male", "Female"), ylim = c(0,7))
