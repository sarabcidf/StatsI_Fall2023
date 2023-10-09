# Applied Statistical Analysis I      
# Tutorial 4: Bivariate regression, inference & prediction                     

# Get working directory
getwd()

# Set working directory 
setwd("/Users/sarabcidf/Desktop/ASDS/Statistics/GitRep/tutorials/04")
getwd()

#############################
### RECAP Chi-square test ###
#############################

# Research questions: Is there a relationship between
# movie genre and rating?

# Load data
df <- readRDS("/Users/sarabcidf/Desktop/ASDS/Statistics/GitRep/datasets/movies.rds")
View(df)

# Dataframe subsetting: df[rows, columns]
df_s <- df[df$genre=="Comedy" |
             df$genre=="Drama" |
             df$genre=="Documentary", ]
df_s$genre <- droplevels(df_s$genre)
View(df_s)

# Run Chi squared test
chisq.test(df_s$genre, 
           df_s$critics_rating)

# Check p-value
sprintf("%.20f",1.097e-12) # Its to print all decimals

# Step 1: Assumptions
# Categorical variable genre; categorical value ratings
# Step 2: Hypotheses
# H0: That the two variables are independent
# Ha: That the two variables are NOT independent
# Step 3: Test statistic
# We see it
# Step 4: P-value
# We see it
# Step 5: Conclusion
# We can reject the H0 that the two variables are independent, and we found evidence that
# the two variables are NOT independent; there is a relationship between genre and rating

### Look at standardized residuals ###

# Save chi-square test in object
chi_test <- chisq.test(df_s$genre, df_s$critics_rating)

# List objects inside chi_test
?ls()
ls(chi_test)
chi_test$observed # f_o (observed frequencies)
chi_test$expected # f_e (expected frequencies under the assumption of H0,
# under the assumption that two variables are independent)

# Pearson residuals, 
# (observed - expected) / sqrt(expected)
chi_test$residuals 

# **Standardized** residuals,
# (observed - expected) / sqrt(V), where V is the residual cell variance
chi_test$stdres # Jeff gives examples of how to manually calculate this!
# I would still like to know how to interpret varying residuals per category... 
# Revise this 

# How can we interpret the standardized residuals? 
# The way to interpret them is there is a LARGE difference between what we see and what we
# would expect if they were independent, vs what we observe. The discrepancy is high, 
# so that's why we reject H0
# In the Chi Squared test, high error = good thing

# Agenda 
# (a.) Correlation
# (b.) Bivariate regression 

# Research questions: 
# Is there a relationship between education and income?

# (a.) Correlation -----

# Load data 
df <- read.csv("/Users/sarabcidf/Desktop/ASDS/Statistics/GitRep/datasets/fictional_data.csv")
View(df)

# Scatter plot 
plot(df$income,df$edu)

# Calculate correlation
cor(df$income,df$edu)

# Add to scatter plot
text(1200, 7, sprintf("Correlation=%s", round(cor(df$income,df$edu),4)))

# Improve visualization and save
png(file="/Users/sarabcidf/Desktop/ASDS/Statistics/GitRep/tutorials/04/scatter_plot.png")
plot(df$income,
     df$edu,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The Relationship between education and income") 
text(1200, 8, sprintf("Correlation=%s", round(cor(df$income,df$edu),4)))
dev.off()

# t-test for the correlation coefficient
cor.test(df$income, df$edu)

# Check p-value
sprintf("%.20f",7.52e-07)

# Step 1: Assumptions
# We now have continuous 
# Step 2: Hypotheses
# H0: No association; cor coeff = 0 
# Ha: The two are correlated; there is an association; correl coef =NOT 0
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion
# We can reject the H0 and find evidence that income and education are associated. There is
# a correltion and this correlation is positive. 

# (b.) Bivariate regression  -----

# Fit linear regression model
summary(lm(df$income~df$edu))
summary(lm(income~edu, data=df))

# Save model as object
model <- lm(income~edu, data=df)

# t-test for the slope of a regression line
summary(model)
250.64/33.06 

# Check p-value
sprintf("%.20f",2.17e-06)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion
# We reject the null hypothesis that there is no linear relationship between
# X and Y
# I dont think we can say anything about *effects* yet... No controls, no nothing
# nmms theres no right or wrong ...

# Confidence intervals 
confint(model, level=0.95)
confint(model, level=0.99)

# Plot
plot(x=df$edu, y=df$income) # Scatter plot
abline(model) # Add regression line

# Step by step
plot(x=df$edu, y=df$income) # Scatter plot (changed y and x over)
abline(v=4)  # Either specify single value (v for vertical)
abline(976.16, 250.64) # Or intercept and slope
abline(model) # Use intercept and slope in model object
abline(model, col="red") # Change color

# What is the prediction equation?
summary(model)
# income_pred = 976.16 + 250.64 * education

# Make predictions for first observation in df
head(df)
976.16 +  250.64 * 1 # predicted outcome
model$fitted.values
1520 - (976.16 +  250.64 * 1) # error
model$residuals

# Make predictions for a range of x values
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Step by step
predict(model) # Predicted outcomes
model$fitted.values # Predicted outcomes
unique(df$edu) # Unique values of x
seq(min(df$edu), max(df$edu), by=1) # Specify a sequences for which
# predictions are to be returned
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Add standard errors
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)))
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), se.fit=TRUE)

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="confidence", level=0.95)

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="prediction", level=0.95)
# more variability in individual responses --> wider intervals

# Make predictions for x values not in data
predict(model, newdata=data.frame(edu = mean(df$edu))) # Mean education
mean(df$edu)
unique(df$edu) # Unique values of x
predict(model, newdata=data.frame(edu = 9)) # **But don't extrapolate**

# Plot predictions
plot(x=df$edu, y=df$income) # Scatter plot
points(df$edu, model$fitted.values, # Add another scatter plot on top
       col="green")

# Plot, regression line with confidence intervals
# Adopted from: https://stackoverflow.com/questions/46459620/plotting-a-95-confidence-interval-for-a-lm-object

# Save confidence intervals
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
plot(df$edu, df$income) # Scatter plot
abline(model) # Add regression line
# Add lower bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
# Add upper bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")

# Step by step
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
ci # Save confidence intervals in object
# Dataframe subsetting: df[rows, columns]
ci[,2] # second column, lower bound, lwr
ci[,3] # third column, upper bound, upr

# Improve visualization and save
png(file="/Users/sarabcidf/Desktop/ASDS/Statistics/GitRep/tutorials/04/reg_plot.png")
plot(df$edu,
     df$incom,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The Relationship between education and income")
abline(model) # Add regression line
# Add confidence intervals
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")
# Add legend
legend(0, 3000, # x and y position of legend
       legend=c("Predictions", "95% Confidence intervals"),
       col=c("black","gray"),
       pch=1) 
dev.off()


