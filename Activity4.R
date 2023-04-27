install.packages("BSDA")
install.packages("e1071")
library(BSDA)
obesity_data <- read.csv("obesity_data.csv", header = TRUE)
attach(obesity_data)
obesity_data
diff <- drug_HR - placebo_HR
diff
SIGN.test(diff, md = 0, alternative = "two.sided")
SIGN.test(diff, md = 0, alternative = "less")
SIGN.test(diff, md=0, alternative = "greater")

#Task 3
diff2 <- drug_WL - placebo_WL
diff2
sort(diff2)
rank(diff2)
order(diff2)
wilcox.test(diff2, md = 0, alternative = "two.sided")
wilcox.test(diff2, md = 0, alternative = "less")
wilcox.test(diff2, md = 0, alternative = "greater")
detach(obesity_data)
obesity_data
obesity_data$diff
