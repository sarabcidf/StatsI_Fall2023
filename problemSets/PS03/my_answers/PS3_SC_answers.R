#### Libraries, options, wd, clearing #### 

# setting wd for current folder (github fork folder)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# clearing 
rm(list=ls())

# f to detach all libraries -> avoid conflicts; clear environment
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# f to load libraries -> checks if they are installed, if not it installs and loads
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# loading -> checking, installing and loading several packages in one line 
lapply(c("stringr","tidyverse"), pkgTest)

#### Reading and exploring data #### 

# reading in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

head(inc.sub)
summary(inc.sub)

#### Question 1 ####

# Creating model using lm (I have used lm a lot in the past): 
model1 <- lm(voteshare~difflog, inc.sub)

# Exploring model
summary(model1)

# Saving residuals separately: 
resid1 <- model1$residuals

# And creating the scatterplot using ggplot:
plot1 <- ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(color = "darkslategray4", alpha = 0.7) +  # Editing point color and transparency 
  geom_smooth(method = "lm", se = FALSE, color = "grey") +  # Adding regression line (no SE)
  theme_minimal() +  # Applying theme 
  labs(x = "Difference in campaign spending between incumbent and challenger", 
       y = "Incumbent vote Share", 
       title = "Scatterplot with Regression Line, Model 1") + # Fixing labels
  theme(panel.grid = element_blank())  # Getting rid of the grid

#Saving plot: 
ggsave("plot1.png", plot = plot1, dpi = 300)

#### Question 2 ####

# Creating and exploring model:  
model2 <- lm(presvote~difflog, inc.sub)
summary(model2)

# Saving residuals separately: 
resid2 <- model2$residuals

# And creating the scatterplot using ggplot:
plot2 <- ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(color = "darkseagreen4", alpha = 0.7) +  # Editing point color and transparency 
  geom_smooth(method = "lm", se = FALSE, color = "grey") +  # Adding regression line (no SE)
  theme_minimal() +  # Applying theme 
  labs(x = "Difference in campaign spending between incumbent and challenger", 
       y = "Presidential candidate vote share (incumbent's party)", 
       title = "Scatterplot with Regression Line, Model 2") + # Fixing labels
  theme(panel.grid = element_blank())  # Getting rid of the grid

#Saving plot: 
ggsave("plot2.png", plot = plot2, dpi = 300)


#### Question 3 ####

# Creating and exploring model:  
model3 <- lm(voteshare~presvote, inc.sub)
summary(model3)

# Creating the scatterplot using ggplot:
plot3 <- ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(color = "lightpink4", alpha = 0.7) +  # Editing point color and transparency 
  geom_smooth(method = "lm", se = FALSE, color = "grey") +  # Adding regression line (no SE)
  theme_minimal() +  # Applying theme 
  labs(x = "Presidential candidate vote share (incumbent's party)", 
       y = "Incumbent's vote share", 
       title = "Scatterplot with Regression Line, Model 3") + # Fixing labels
  theme(panel.grid = element_blank())  # Getting rid of the grid

#Saving plot: 
ggsave("plot3.png", plot = plot3, dpi = 300)

#### Question 4 #### 

# Creating and exploring model:  
model4 <- lm(resid1~resid2, inc.sub)
summary(model4)

# Creating the scatterplot using ggplot:
plot4 <- ggplot(inc.sub, aes(x = resid2, y = resid1)) +
  geom_point(color = "thistle3", alpha = 0.7) +  # Editing point color and transparency 
  geom_smooth(method = "lm", se = FALSE, color = "grey") +  # Adding regression line (no SE)
  theme_minimal() +  # Applying theme 
  labs(x = "Residuals from Q2: Variation in PresVote not explained by difference in spending", 
       y = "Residuals from Q1: Variation in VoteShare not explained by difference in spending", 
       title = "Scatterplot with Regression Line, Model 4") + # Fixing labels
  theme(panel.grid = element_blank())  # Getting rid of the grid

#Saving plot: 
ggsave("plot4.png", plot = plot4, dpi = 300)

#### Question 5 #### 

# Creating and exploring model:  
model5 <- lm(voteshare~difflog+presvote, inc.sub)
summary(model5)

# Comparing Model 4 and Model 5:
# I think the residuals between the two models are going to be the same
# I will check for this.

# Naming my residuals from both models: 
resid4 <- model4$residuals
resid5 <- model5$residuals

# I see here I can use the function identical (https://statisticsglobe.com/compare-vectors-and-find-differences-in-r)
identical(resid4,resid5) # Not identical 

# But perhaps they are not identical to the very last decimal point,
# however if I give a certain tolerance level (for example 0.001)
# the residuals will be the same.
# Here I see how to create the following function to compare each element
# in my residuals vectors, allowing for a small difference between them 
# (https://stackoverflow.com/questions/49237304/compare-two-vectors-of-numbers-based-on-threshold-of-tolerance-±-of-0-5)
# And I just have to sum the output
sum(mapply(function(element1, element2) abs(element1 - element2) <= 0.001, 
           resid4, resid5))

# Double checking residuals length -> this means all residuals in M4 and M5 are the same
length(resid4) 
length(resid5)


