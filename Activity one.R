seminar1 = read.csv("weight_loss_data.csv", header = TRUE)
seminar1
attach(seminar1)
#Create a new category for the age so each falls within an interval
category <- cut(seminar1$Age, breaks = seq(10,80, by = 10))
category
seminar1$Age_category <- rep(0,16)
attach(seminar1)
#assign number to each category of age in the data table
seminar1$Age_category[which(category == levels(category)[1])] = 1 #10-20
seminar1$Age_category[which(category == levels(category)[2])] = 2 #20-30
seminar1$Age_category[which(category == levels(category)[3])] = 3 #30-40
seminar1$Age_category[which(category == levels(category)[4])] = 4 #40-50
seminar1$Age_category[which(category == levels(category)[5])] = 5 #50-60
seminar1$Age_category[which(category == levels(category)[6])] = 6 #60-70
seminar1$Age_category[which(category == levels(category)[7])] = 7 #60-80
seminar1

#Create weight loss (weight difference) column from initial and final weight
#Initial weight is the weight befor the drug was taken
#final weight is the wight after the drug has been taken

seminar1$weight_loss <- seminar1$Final_weight - seminar1$Initial_weight
seminar1

#save a file you've worked on
write.csv(seminar1, file = "weight_loss_data2.csv", row.names = FALSE) 

#Task 3
#Selecting Cases: select every row where gender is male and all columns
seminar1_male = seminar1[seminar1$Gender == 'male',]
seminar1_male

#Generating Random numbers and sorting them

Random_Num = sample(1:49, size = 6)
Random_Num
#sort the values generated
sort(Random_Num, decreasing = TRUE)


#Exercise 1
#1000 Random between 1 and 25 and sort in descending order
rand_ex <- sample(1:25, size = 1000, replace = TRUE)
rand_ex
sort(rand_ex, decreasing = TRUE)
division = cut(rand_ex, breaks = seq(0,25, by = 5))
division
category = rep(0,25)
category[which(division == levels(division)[1])] = 1
category[which(division == levels(division)[2])] = 2
category[which(division == levels(division)[3])] = 3
category[which(division == levels(division)[4])] = 4
category[which(division == levels(division)[5])] = 5
category
hist(category)
