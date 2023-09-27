#### Options & libraries ####

rm(list=ls()) # Making sure the environment is empty when starting new script
setwd("/Users/sarabcidf/Desktop/ASDS/Statistics/GitRep/problemSets/PS01/my_answers") # Setting wd for the PS

library (tidyverse) # I might be using tidyverse
library (ggplot2) # I might be using ggplot for the last task of Question 2
library(knitr) # I will use this to insert chunks of code into my latex doc 

#### Question 1 ####

# Exercise 1. Calculating the CI: #

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98) # Copying vector with sample of IQs
length(y) # n = 25

# For the IC, since we don't know the SD for the population, plus n < 30,
# We must do a T test instead of a Z test
# I see here that we can use qt instead of qnorm:
# https://www.statology.org/working-with-the-student-t-distribution-in-r-dt-qt-pt-rt/

# T statistic = (sample mean - pop mean) / (s/sqrt(n))
# CI : - T(n-1, alpha/2) <= T stat <= T(n-1, alpha/2)
# Solving for pop mean, the CI is: 
# [Sample mean - T(n-1, alpha/2)(s/sqrt(n)) , sample mean + T(n-1, alpha/2)(s/sqrt(n))]
# Making all the calculations: 

meany <- mean(y) # I see there is a sample mean of 98.44
sdy <- sd(y) # I see there is a sample sd of 13.09
sqrtn <- sqrt(25) # I calculate the sqrt of the sample size, which is = 5
alpha = 1-0.9 

# MI DUDA ES POR QUE PONGO ALPHA / 2 EN LUGAR DE .9 / 2... SE VOLTEAN LOS SIGNOS DEL INTERVALO SUPERIOR E INFERIOR.... Tanto Normal como T son simetricas... O por que ponemos lower.tail = F

t90 <- qt(alpha/2,24,lower.tail = F) # Critical value T(n-1, alpha/2)
t90

T90 <- t90*sdy/sqrtn # T90 * (s/sqrt(n)). This I add and substract from sample mean
T90

lower_90 <- meany - T90
upper_90 <- meany + T90 

confint_90 <- c(lower_90, upper_90)
confint_90 # Mean of school falls somewhere between these values 

# Confirming with t.test()

test2 <- t.test(y,conf.level = .90)
test2 # The IC is identical

# I would also like to see what happens if I use qnorm instead of qt for a n = 25

z90 <- qnorm(alpha/2,lower.tail = F)
lower_90_b <- meany - (z90 * (sdy/sqrtn)) 
upper_90_b <- meany + (z90 * (sdy/sqrtn))
confint90_b <- c(lower_90_b, upper_90_b)
confint90_b # It does change quite a bit using qnorm ... 

# Exercise 2. Seeing if AvgIQSchool > 100 (testing hypothesis) # 

# I want to test if meanschool is > 100
# Null Hyp (H0) : meanschool <= 100 ; 
# Alt Hyp (Ha) : meanschool > 100 
# All the assumptions from the CI apply and I use a T test
# alpa = 0.05, Conf level = 95

# If T > T(n-1, alpha), I may reject H0 at the 95 confidence level

alpha2 <- 0.05

t95 <- qt(alpha2,24,lower.tail = F) # I am looking in the upper tail 
t95 # Critical value of -1.71 (if T => -1.71, I may reject H0)

T95 <- (meany - 100)/(sdy/sqrtn)
T95 # T is NOT greater than -1.71, so I may not reject the H0 that mean school <= 100 at the 95 confidence level 

# Calculating the P Value

p_val <- pt(abs(T95),df = 24, lower.tail = T)
p_val

# To confirm with t.test():

test <- t.test(y, mu = 100, alternative = "greater")
test

# From here https://data-flair.training/blogs/hypothesis-testing-in-r/ I see that I must specify mu = 100 and alternative = greater, given my H0 & Ha, for the confirmation with t.test that I used above.

#### Question 2 ####

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

head(expenditure)
summary(expenditure)

# Exercise 1. Initial plots

plot_X1Y <- plot(expenditure$X1,expenditure$Y,
                 xlab = "Per capita personal income in state",
                  ylab = "Number of people per thousand residing in urban areas in state")
plot_X1Y

plot_X2Y <- plot(expenditure$X2,expenditure$Y,
                 xlab = "Number of residents per 100,000 that are ”financially insecure” in state",
                 ylab = "Per capita expenditure in shelters/housing assistance in state")
plot_X2Y

plot_X3Y <- plot(expenditure$X3,expenditure$Y,
                 xlab = "Per capita personal income in state",
                 ylab = "Number of people per thousand residing in urban areas in state")
plot_X3Y

plot_X1X2 <- plot(expenditure$X1,expenditure$X2,
                  xlab = "Per capita personal income in state",
                  ylab = "Number of residents per 100,000 that are ”financially insecure” in state")
plot_X1X2

plot_X1X3 <- plot(expenditure$X1,expenditure$X3,
                  xlab = "Per capita personal income in state",
                  ylab = "Number of people per thousand residing in urban areas in state")
plot_X1X3

plot_X2X3 <- plot(expenditure$X2,expenditure$X3,
                  xlab = "Number of residents per 100,000 that are ”financially insecure” in state",
                  ylab = "Number of people per thousand residing in urban areas in state")
plot_X2X3

# I used this cheat sheet because I did not remember how to set the x and y labels: 
# https://publish.illinois.edu/johnrgallagher/files/2015/10/BaseGraphicsCheatsheet.pdf

# Exercise 2. Region plot #

# I think it makes sense to have Region as factor, otherwise R will do a dots plot for this: 

expenditure$Region <- factor(expenditure$Region)
plot_Reg <- plot(expenditure$Region,expenditure$Y,
                 xlab = "Region",
                 ylab = "Per capita expenditure in shelters/housing assistance in state")
plot_Reg

# Exercise Confirming 

reg_highest_exp <- expenditure %>% group_by(Region) %>% summarise(MeanExp = mean(Y)) # Region 4 has the highest

# 3. X1 and Y, by Region #

plotX1Y_Reg <- ggplot(expenditure,aes(x = X1, y = Y, shape = Region, colour = Region)) +
  geom_point()+
  labs(x = "Per capita personal income in state",
       y = "Number of people per thousand residing in urban areas in state")+
  theme_minimal()
plotX1Y_Reg

# Please note: I could not find how to make the base R scatteplot work to give different dot shapes to different regions (my attempt is below), so I did it with ggplot. 
# I already knew ggplot well, however I did consult the following sources to remember exactly which arguments I needed to make the plot do what I wanted it to do: 
# https://r-graphics.org/recipe-colors-setting (different colors)
# https://r-graphics.org/recipe-scatter-shapes (different point shapes)

plotX1Y_Reg <- plot(expenditure$X1,expenditure$Y,
col = factor(expenditure$Region),
pch = factor(expenditure$Region)) # Does not work


