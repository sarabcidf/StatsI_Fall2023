#### Libraries, options, data ... ####

install.packages(car)
library(car)
library(tidyverse) # I want to use functions from dplyr to transform data
library(stargazer) # I will use stargazer to report regression results

#### Question 1 #### 

# Loading data: 

data(Prestige)
help(Prestige)
summary(Prestige)

# Study whether more income = more prestige
# Study whether profs = more prestige than bc or wc

# a) New variable "professional" based on "type"
# I am using mutate() from dplyr, along with ifelse()
# What I am telling R is that the new variable professional
# will take the value 1 if type is equal to "prof" and 0
# if type is anything else. 

Prestige <- Prestige %>% mutate(
  professional = ifelse(type == "prof", 1,0)
)

# Checking that NAs are still NAs and not 0 
# (so I'm coding professional correctly):

count(Prestige, professional) 

# b) Running the first model: 
# prestige ~ income + professional + interaction 

m1 <- lm(prestige~income+professional+income*professional, Prestige)
summary(m1)

# Using the same code as last PS, with stargazer, to 
# report results: 

stargazer(
  m1,
  title = "Model 1 Regression Results",
  type = "latex",
  dep.var.caption = "Prestige",
  dep.var.labels.include = FALSE,
  covariate.labels = c("Income", "Professional"),
  omit.stat = c("ser","f"),
  star.cutoffs = c(0.05, 0.01, 0.001)
)

# Calculations: 

# Going from prof = 0 to prof = 1 for income = 6000

# One way: 
pres1.1 = 21.14 + 0.0032 * 6000
pres2.1 = 58.92 + 0.0009 * 6000
change_a = pres2.1 - pres1.1
change_a

# Another way: 
change_b = 37.78 - 0.0023 * 6000
change_b

# Increasing income by 1000 for prof = 1: 

# One way: 
pres1.2 = 58.92 + 0.0009*0
pres2.2 = 58.92 + 0.0009*1000
change_c = pres2.2 - pres1.2
change_c

# Another way: 
change_d = 0.0009*1000
change_d





#### Question 2 ####

# a) Hypothesis test for signs coefficient: 

# The general form for our test statistic is: 
# (observed value - expected if null is true) / SE

# We will also need our sample size and degrees of 
# freedom handy: 

n = 131
df = n - 2

# Substituting with the given results: 

t1 = (0.042 - 0) / 0.016
t1

# And calculating the p-value using pt(): 

p1 = 2 * pt (abs(t1), df, lower.tail=F)
p1

# b) Hypothesis test for adjacent coefficient: 

# Substituting with the given results: 

t2 = (0.042 - 0) / 0.013
t2

# And calculating the p-value using pt(): 

p2 = 2 * pt (abs(t2), df, lower.tail=F)
p2

# d) Overall f test 

# First we need our R squared value handy:

r2 = 0.094

# I will also now define k: 

k = 2 

# We already have our sample size and degrees of freedom
# And we can use the test statistic from slide 17, week 10, 
# as well as the code in slide 19, week 10:

f = ((r2/(k-1))) / ((1-r2)/(df))
f

df1 = k - 1
df2 = n - k - 1 

pf = df(f, df1, df2)
pf 

# End


