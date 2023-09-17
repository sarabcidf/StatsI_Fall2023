#### Options & libraries ####

rm(list=ls()) # Making sure the environment is empty when starting new script
setwd("/Users/sarabcidf/Desktop/ASDS/Statistics/Problem sets") # Setting wd for the PS

library (tidyverse) # I will be using tidyverse which I already know :) 
library (ggplot2) # I will also be using ggplot for the last task of Question 2

#### Question 1 ####

# 1. Calculating the CI: #

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98) # Copying vector of IQs ; n = 25

# Calculating mean and sd

meany <- mean(y) # Sample mean of 98.44
sdy <- sd(y) # Sample sd of 13.09

# I think we must assume that IQs are normally distributed, and we do not know 
# the mean or the sd for the population 
# (like Setting 2 here: http://www.stat.ucla.edu/~rgould/110as02/bsci). 

# Since we do not know sd for the population, we need a t test instead of a z test

# If 90% CI, alpha = 0.10
# So xbar +- t(alpha/2, df) * s/sqrt(n)
# alpha/2 = .05 and 1-.05 = 0.5
# df = n-1 = 25-1 = 24

sqrtn <- sqrt(25)

me <- qt(.95,24) * sdy/sqrtn
me

lower_90 <- meany - me
upper_90 <- meany + me 

confint_90 <- c(lower_90, upper_90)
confint_90 # meanschool falls somewhere between these 

# QUESTIONS # 

# From the slides (but we cannot use z because n < 30 
# and because we do not know sd ?)
# What does the n < 30 have to do with the choice between T or Z ? 

lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n))) 
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95)



# 2. Seeing if AvgIQSchool > 100 (testing hypothesis)

# I want to test if meanschool is > 100
# Null Hyp : meanschool <= 100 ; 
# Alt Hyp : meanschool > 100 
# All the assumptions from the CI apply and I use a T test
# alpa = 0.05, Conf level = 95

# From here https://data-flair.training/blogs/hypothesis-testing-in-r/
# I see that the function is t.test, and that I must specify mu = 100 and
# Alternative = gretaer, following the problem and the test I wish to carry out

test <- t.test(y, mu = 100, alternative = "greater") 
test

#### Question 2 ####

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

head(expenditure)
summary(expenditure)

# 1. Initial plots #

plot_YX1 <- plot(expenditure$X1,expenditure$Y)
plot_YX1

plot_YX2 <- plot(expenditure$X2,expenditure$Y)
plot_YX2

plot_YX3 <- plot(expenditure$X3,expenditure$Y)
plot_YX3

# 2. Region plot #

plot_Reg <- plot(expenditure$Region,expenditure$Y)
plot_Reg

# Confirming 

reg_highest_exp <- expenditure %>% group_by(Region) %>% summarise(MeanExp = mean(Y)) # Region 4 has the highest

# 3. X1 and Y, by Region #

## AQUI ME QUEDE ## ****************************


