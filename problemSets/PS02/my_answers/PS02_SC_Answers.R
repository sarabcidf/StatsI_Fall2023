#### Libraries, options, wd ####
#### Question 1: Chi Squared Test ####

# ** a) ** 

# First I create my matrix:

bribes <- matrix (c(14,6,7,7,7,1), nrow=2, byrow =T) # Got from Dr. Ziegler's Slides
colnames(bribes) <- c("NS","B","W") # Got from Link 1 below
rownames(bribes) <- c("U","L") # Got from Link 2 below

# https://www.geeksforgeeks.org/change-column-name-of-a-given-dataframe-in-r/
# https://www.geeksforgeeks.org/how-to-change-row-names-of-dataframe-in-r/

# I know the formula for the Chi^2 statistic (from Dr. Ziegler's slides)
# And I know that expected value = (rowtot)(columntot)/(grandtot)

# Just saving my f0 values: 

f0_ns_u <- bribes[1,1]
f0_ns_l <- bribes[2,1]

f0_b_u <- bribes[1,2]
f0_b_l <- bribes[2,2]

f0_w_u <- bribes[1,3]
f0_w_l <- bribes[2,3]

# Calculating totals:

print(bribes)

colSums(bribes) # Col tots
rowSums(bribes) # Row tots

sum(colSums(bribes)) # Grand tot

# Calculating expected values: 

fe_ns_u <- (21*27)/42
fe_ns_l <- (21*15)/42

fe_b_u <- (13*27)/42
fe_b_l <- (13*15)/42

fe_w_u <- (8*27)/42
fe_w_l <- (8*15)/42

# Calculating squared difference over expected: 

dif_ns_u <- ((f0_ns_u - fe_ns_u)^2)/fe_ns_u
dif_ns_l <- ((f0_ns_l - fe_ns_l)^2)/fe_ns_l

dif_b_u <- ((f0_b_u - fe_b_u)^2)/fe_b_u
dif_b_l <- ((f0_b_l - fe_b_l)^2)/fe_b_l

dif_w_u <- ((f0_w_u - fe_w_u)^2)/fe_w_u
dif_w_l <- ((f0_w_l - fe_w_l)^2)/fe_w_l

# Sum of all: 

statistic <- dif_ns_u + dif_ns_l + dif_b_u + dif_b_l + dif_w_u + dif_w_l # I get 3.79

# Double checking test (I got the chisq() function from Hannah's code): 

chi_test <- chisq.test(bribes,correct = F) 
chi_test # Here I get 3.79 as well

# ** b) **

# P value calculation 
# I know my df must be (nrow-1)(ncol-1), which is simply 1*2 = 2
# I see in this link I can use the pchisq() function
# https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-chi-square-statistic-in-r/

pchisq(statistic, 2, lower.tail = F) 

# I get 0.15, which also coincides with what the chisq() function generated 
# Since 0.15 > 0.1, I have not found enough evidence to reject H0, that class and bribing 
# are independent. In other words, there is not enough evidence to support the Ha that the 
# two variables are dependent. 

# ** c) **

# To calculate the standardized residual, I also follow Dr. Ziegler's formula from his Week 3 slides
# I also check against the results of using my chisq() function and it works 

std_res_ns_u <- (f0_ns_u - fe_ns_u)/sqrt(fe_ns_u*(1-(27/42))*(1-(21/42)))
std_res_ns_l <- (f0_ns_l - fe_ns_l)/sqrt(fe_ns_l*(1-(15/42))*(1-(21/42)))

std_res_b_u <- (f0_b_u - fe_b_u)/sqrt(fe_b_u*(1-(27/42))*(1-(13/42)))
std_res_b_l <- (f0_b_l - fe_b_l)/sqrt(fe_b_l*(1-(15/42))*(1-(13/42)))

std_res_w_u <- (f0_w_u - fe_w_u)/sqrt(fe_w_u*(1-(27/42))*(1-(8/42)))
std_res_w_l <- (f0_w_l - fe_w_l)/sqrt(fe_w_l*(1-(15/42))*(1-(8/42)))

# ** d) ** Interpretation (in PDF only) 

#### Question 2: Bivariate regression ####

# Reading the data

data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

summary(data) # We re interested in water specifically

# ** a) **

# My model: 
# x: reserved / not (I assume 1 means reserved for women, 0 means not)
# y: no. of new or repaired water facilities
# y ~ x 

# My hypotheses:

# H0 (for b): the reservation policy has no effect on the no. of new/repaired water facilities ...
# Ha (for b): the policy does have an effect on the no. of new/repaired facilities ...
# Also hypothesize for a.

# ** b) ** 

# Trying to run the regression "by hand", following Dr. Ziegler's formula of the estimators
# for b0 and b1, and his code (both from Week 4 slides)

b1 <- sum((data$water - mean(data$water)) * (data$reserved - mean(data$reserved)))/
  sum((data$reserved - mean(data$reserved))^2) 

b0 <- mean(data$water) - b1*mean(data$reserved)

# Confirming with lm() (I knew the lm function well from before the course)

bivar_reg <- lm(water ~ reserved, data = data) 
bivar_reg # I do get the same results ! 

# ** c) ** Interpretation (in PDF only) 