## biostats assignment 1

library(dgof)
library(ggplot2)
library (wPerm)
library(coin)

## exercise 6



## exercise 7

data <- matrix(c(1, 6.7, 7.0, -0.3,
                 2, 7.4, 7.4, 0,
                 3, 9.2, 8.6, 0.6,
                 4, 9.6, 8.1, 1.5,
                 5, 7.4, 6.8, 0.6,
                 6, 8.1, 7.0, 1.1,
                 7, 10.8, 8.5, 2.3,
                 8, 7.1, 7.7, -0.6,
                 9, 7.9, 9.7, -1.8,
                 10, 10.8, 7.7, 3.1), ncol = 4, byrow = TRUE)

# comparison of means after one year of treatment 
mean(data[,2]) - mean(data[,3])
# the average HgbA1c level after one year is 0.65 higher than the previous


## exercise 8

data <- matrix(data = c(50, 70, 90, 120, 40, 100, 150, 110, 75, 160, 
                        7, 8, 10.5, 11, 9, 10.8, 12, 10, 9.5, 11.9), ncol = 2)
# test for normality
shapiro.test(data)
# low p-value of 0.0018
ks.test(data[,1], data[,2])
# low p-value of 1.083e-05
# reject H0, we conclude the data is not normally distributed
# we should use a non parametric test

# Mann Whitney test for numeric non gaussian data
wilcox.test(data[,1], data[,2])
# p value of 1.083e-05
# we reject H0, we conclude that the two variables follow different distributions

## exercise 9

data <- read.delim(file = "datasets/dbp.txt", header = TRUE, sep = "")

# BDP of treatment A vs treatment B
# difference of means
mean(data$DBP[data$TRT=="A"])
mean(data$DBP[data$TRT=="B"])
# we see that the average BDP for patients with treatment A is about 5 lower than for treatment B
# test for independence
wilcox.test(data$DBP[data$TRT=="A"], data$DBP[data$TRT=="B"])
# low p value of 4.009e-08 implies that DBP for treatments A and B follow different distributions

ggplot()+ aes(x = data$month, y= data$DBP, color = data$TRT) + geom_point() + 
  labs(title = "Treatments A and B over time", x = "Month", y = "DBP", color = "Treatment")
# our plot shows that DBP decreases over time for both treatments, but treatment A has a more negative gradient
# decrease in DBP appears linear with relation to time for both treatments
# we see increased variance with time for treatment A, but homocedasticity for treatment B

# difference in means over time
difference_mean <- c(1:5)
for (i in 1:5) {
  difference_mean[i] <- mean(data$DBP[data$month == i&data$TRT=="A"]) - mean(data$DBP[data$month == i&data$TRT=="B"]) 
}
difference_mean
# we see that the difference of means between the two treatment groups increases with time at a fairly constant rate
plot(difference_mean, main = "Difference of means of treatment", ylab = "Difference")


       