# Chapter 3 ---- Linear Regression 

#####3.6 - Lab: linear regression ----
### lab based on the MASS and Boston datasets, in order to explore the concept of linear regression 
library(MASS) ###library already present in the base R package
head(Boston)
?Boston
lm.fit = lm(medv ~ lstat, Boston)
summary(lm.fit)
names(lm.fit)
coef(lm.fit) ###directly viewing coefficients for lm.fit
confint(lm.fit) ###obtaining confidence intervals 
?confint
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence") ###confidence intervals of medv for  arbitrary values of lstat    
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction") ### same but with prediction intervals 

attach(Boston) ###attaching Boston for simpler syntax
plot(lstat, medv, pch = 20) ###
abline(lm.fit, col = 'red', lwd = 2)

par(mfrow = c(2, 2)) ###splitting screen into 4 
plot(lm.fit) ###visualizing all 4 diagnostic plots in the same screen

par(mfrow = c(1,1)) ###going back to default graphic setting 
plot(predict(lm.fit), residuals(lm.fit)) ###plotting the fitted values against the residuals manually
plot(predict(lm.fit), rstudent(lm.fit)) ###Same thing but using studentized residuals 
plot(hatvalues(lm.fit))
hatvalues(lm.fit)

lm.fit = lm(medv ~ lstat + age, Boston) ###adding a new predictor (age) to lm.fit
summary(lm.fit)

lm.fit = lm(medv ~ ., Boston) ### useful way to do a regression with all of our predictors without writing all of them
?summary.lm
install.packages('car')
vif(lm.fit) ### very useful function from the car package to compute variance inflation factors
lm.fit1 = lm(medv ~ . - age, Boston) ###simple way to exclude one predictor from the regression, in this case 'age' because of high p-value 
summary(lm.fit1)
lm.fit1 = update(lm.fit, ~ . - age) ### alternative way to do it, it just modifies the first element according to a specified parameter

?update

#### 3.6.4: interaction terms 
summary(lm(medv ~ lstat * age, Boston)) ### learning to use the '*' sign to include also the interaction term, normally done with ':'

#### 3.6.5: Non-Linear transformations of the predictors 
lm.fit2 = lm(medv ~ lstat + I(lstat^2)) ###including lstat^2 as a predictor 
summary(lm.fit2) # p-value is minimal, suggesting an improved model 

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit2) # performing anova to check whether adding lstat^2 leads to an improved model (it does)

par(mfrow = c(2, 2))
plot(lm.fit2)

lm.fit5 = lm(medv ~ poly(lstat, 5)) #learning to use the 'poly' command, in order to add polynomials in the analysis 

#### 3.6.6: Qualitative predictors
library(ISLR2)
head(Carseats)
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, Carseats) #running a linear model and adding some random interaction terms 
#lab keeps the name for every regression as 'lm.fit', I don't know if I like it, but I'll probably keep it this way to avoid too many variables in the environment 
summary(lm.fit)

contrasts (Carseats$ShelveLoc) #very useful! tells us how it creates dummy variables for categorical variables

#### 3.6.7: Writing functions 
LoadLibraries = function() {
  library(ISLR2)
  library(MASS)
  print('The libraries have been loaded')
} #### creating a function that reads the 2 libraries, very basic stuff 


##### 3.7 - Applied exercises ----
# Ex. 8.a
attach(Auto)
summary(lm(mpg ~ horsepower)) ###regressing horsepower against miles per gallon

# 8.a.i yes, there is a negative relationship between the two 
# 8.a.ii the relationship is moderately strong with an adjusted R squared of 0.649
# 8.a.iii the relationship is negative (already said) 
# 8.a.iv for a horsepower of 98 predicted mpg is 24.46708. Associated confidence and prediction intervals are 24-25 and 14.8-34.12
c98df = data.frame(c98)
predict(mpghp, newdata = data_frame(horsepower = 98))
predict(mpghp, newdata = data_frame(horsepower = 98), interval = 'confidence')
predict(mpghp, newdata = data_frame(horsepower = 98), interval = 'prediction')

#8b - Plot the response and the predictor. Use the abline() function to display the least squares regression line.
par(mfrow = c(1, 1))
plot(horsepower, mpg)
abline(mpghp, col = 'red', lwd = 2)

#8c - Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit.
par(mfrow = c(2,2))
plot(mpghp)
### the relationship appears to be non linear, its especially obvious from the residuals plot. There is one observation 
### that might be an outlier on the residual vs leverage plot. ALso, leverage values are especially bunched up in the 0.005-0.010 range.

#9a - Produce a scatterplot matrix which includes all of the variables in the data set.
plot(Auto)

#9b - Compute the matrix of correlations between the variables using the function cor(). 
#You will need to exclude the name variable, which is qualitative.
cor(Auto[1:8]) ###need to look of a way to exclude variables by name from a correlation 

#9c
lm_mpg_Auto = lm(mpg ~ . -name, Auto)
summary(lm_mpg_Auto)
# i with a multiple R-squared of 0.8, and an extremely low p-value on the f-statistic, there seems to be some relationship between the predictors and the response
# ii the highest p values are those of cylinders, horsepower and acceleration, all the other are low to extremely low
# iii the coefficient for year suggests a positive relationship

#9d 
par(mfrow = c(2,2))
plot(lm_mpg_Auto)
#yes, there is a few observations that are unusually large, most likely outliers. Also there is at least one predictor with high leverage (14)

#9e
#I'll look for interaction effects between the lowest p values first
summary(lm(mpg ~ year*origin, Auto)) #pretty weak interaction at 0.06
summary(lm(mpg ~ year*weight, Auto)) #year and weight have an extremely significant interaction, interesting 

#9f

#10a - Fit a multiple regression model to predict Sales using Price, Urban, and US.
lm_PUS = lm(Sales ~ Price + Urban + US, Carseats)
summary(lm_PUS) #Price and US have extremely high significance levels, Urban basically non-existent 

#10b - Provide an interpretation of each coefficient in the model. Be careful—some of the variables in the model are qualitative!
##Price (-0.05) shows a weak negative relationship, Urban(-0.02) as well. US (1.2) shows a pretty strong positive correlation,
##meaning that, all other factors being equal, the store being in the US is associated with an increase of 1200 units sold

#10c - 

#10d - we can reject the null hypothesis for Price and US

#10e/f - On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.
lm_PUS = lm(Sales ~ Price + US, Carseats)
summary(lm_PUS) #f-statistic increases from 42 to 62, meaning better fit! All the other parameters stay the same more or less.

#10g - Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(lm_PUS)

#10h - Is there evidence of outliers or high leverage observations in the model from (e)?
plot(lm_PUS)
#yes, we can see one/three high leverage observations 


#11
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)

#11a 
summary(lm(y ~ x -1))
# coefficient estimate: 1.9, t statistic: 18.73, p value: < 2e-16. There is a positive relationship, and given the t and p values
#there seems to be strong evidence against the null hypothesis 

#11b
summary(lm(x~y - 1))
#coeff. estimate: 0.39, t value: 18.73, p value: 2e-16. Positive but not as strong relationship, still very strong evidence against 
#the null hypothesis 


#12a - Q: Recall that the coefficient estimate βˆ for the linear regression of Y onto X without an intercept is given by (3.38). Under what 
#circumstance is the coefficient estimate for the regression of X onto Y the same as the coefficient estimate for the regression of Y onto X?
#ANSWER: the two coefficients would be the same only in the case in which they would both be of value 1, indicating a one-unit increase
#in Y for every one-unit increase in X and viceversa

#12b - Q: Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is different 
#from the coefficient estimate for the regression of Y onto X.
#ANSWER: y = 2x would be the simplest example that comes to mind, with x being a rnorm(100) 


#12c - Q: Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is the same as 
#the coefficient estimate for the regression of Y onto X.

#ANSWER: in this case x would be the same as y, unless I'm making some weird non obvious mistake 


#13a 
set.seed(1)
x = rnorm(100, 0, 1)
#13b 
eps = rnorm(100, 0, sqrt(0.25))
#13c
y = -1 + 0.5 * x + eps
length(y) #length of y is 100, slope is -1, intercept is 0.5
#13d
xy = lm(y ~ x)
par(mfrow = c(1,1))
plot(x,y, legend = TRUE)
abline (xy, col = 'red')
#13e
summary(xy) #pretty good estimates at 0.49 for the slope and -1.01 for the intercept 
#13f - I already drew the model line, so I'll just draw the population regression line 
abline(a = -1, b = 0.5, col = 'blue') #they look basically the same, but why? Because we used all of the 100 observations to train 
#our model!!! So they are gonna look pretty much the same 
legend('bottomright', legend = c('blue:population', 'red: least squares'), cex = 0.6)
#13g - 
x2y = lm(y ~ x + x^2)
summary(x2y) #the polynomial model doesnt seem to have a better fit, probably for the reasons stated in 13f
#13h/i/j
eps1 = rnorm(100, 0, sqrt(0.10)) #creating a less noisy error term by decreasing the variance of the distribution
eps2 = rnorm(100, 0, sqrt(0.50)) #creating a noisier error term by increasing the variance of the distribution
y1 = -1 + 0.5 * x + eps1 #less noisy equation
y2 = -1 + 0.5 * x + eps2 #noisier equation 
xy1 = lm(y1 ~ x) #less noisy linear model 
xy2 = lm(y2 ~ x) #noisier linear model 
plot(x, y1)
plot(x, y2)
confint(xy)
confint(xy1)
confint(xy2)
#as expected, the noisier the model, the wider the confidence interval 

#14a 
set.seed(1)
x1 <- runif (100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
#14b
plot(x1,x2)
#14c
summary(lm(y ~ x1 + x2))
#we can reject the null hypothesis for x1 (p = 0.04), but not for x2(p = 0.38)
#14d/e
summary(lm(y ~ x1)) #null hypothesis can be rejected 
summary(lm(y ~ x2)) #null hypothesis can be rejected 
#14f - the results don't really contradict themselves, but might just indicate presence of collinearity between the two variables,
#which causes the standard error for the coefficients to grow 
#14g 
x1 <- c(x1, 0.1) 
x2 <- c(x2, 0.8) 
y <- c(y, 6)
summary(lm(y ~ x1 + x2))
summary(lm(y ~ x1)) 
summary(lm(y ~ x2))
#interesting, in the combined regression we can reject H0 for x2 but not for x1, while we can reject it for both of them in the 
#individual regressions 

plot(x2, y) #the new obs. in x2 (0.8) has high leverage, but I wouldnt say its an outlier 
plot(x1, y) #new obs. in x1 seems to be an outlier, with a very low value 

#15a
head(Boston)
?Boston

summary(lm(crim ~ zn)) #p = 5.51^-06, so significant 
summary(lm(crim ~ indus)) #p = <2^-16, so significant 
summary(lm(crim ~ chas)) #p = 0.209, so not significant 
summary(lm(crim ~ nox)) #p < 2^-16, so significant
summary(lm(crim ~ rm)) #p = 6.35^-07, so significant
summary(lm(crim ~ age)) #p = 2.85^-16, so significant 
summary(lm(crim ~ dis)) #p < 2^-16, so significant 
summary(lm(crim ~ rad)) #p < 2^-16, so significant 
summary(lm(crim ~ tax))  #p < 2^-16, so significant 
summary(lm(crim ~ ptratio)) #p = 2.94e-11, so significant 
summary(lm(crim ~ lstat)) #p < 2^-16, so significant 
summary(lm(crim ~ medv)) #p < 2^-16, so significant 

#15b
Boston_mult = summary(lm(crim ~ ., Boston))
#in this case, the only significant predictors are 'dis', 'rad' and 'medv', with 'zn', 'nox' and 'lstat' being moderately significant 




# creating a ist of predictor variables
predictors <- c("zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "lstat", "medv")

# fitting a linear regression models for each predictor and extracting the coefficients
coefficients_list <- lapply(Boston_predictors, function(predictor) {
  model <- lm(formula = paste("crim ~", predictor), data = Boston)  # Change 'your_data' to your actual dataset name
  summary_model <- summary(model)
  coefficients <- summary_model$coefficients[2,'Estimate']  # Extract coefficients
  return(coefficients)
})

# data frame
single_coefficients_df <- do.call(rbind, coefficients_list)
rownames(single_coefficients_df) <- Boston_predictors  # Set row names to predictors

Boston.coeff = tibble(
  predictors = colnames(Boston),
  multiple.coeffs = Boston_mult$coefficients[,'Estimate']
)

Boston.coeff = Boston.coeff[-1,]
Boston.coeff = as.data.frame(Boston.coeff)
row.names(Boston.coeff) = Boston.coeff[,1]
Boston.coeff = Boston.coeff[,-1, drop = FALSE]

Boston.final = merge(Boston.coeff, single_coefficients_df, by = 'row.names')

row.names(Boston.final) = Boston.final[,1]
Boston.final = Boston.final[,-1]
colnames(Boston.final[2]) = 'single regression coefficients'

colnames(Boston.final)[colnames(Boston.final) == "V1"] <- "single regression coefficients"

attach(Boston.final)

#15c 
plot(`single regression coefficients`, multiple.coeffs)

Boston.final.nnox = Boston.final[-7,]

plot(Boston.final.nnox$`single regression coefficients`, Boston.final.nnox$multiple.coeffs)

#15d
summary(lm(crim ~ zn + zn^2 + zn^3)) #p = 5.51^-06, so significant 
summary(lm(crim ~ indus)) #p = <2^-16, so significant 
summary(lm(crim ~ chas)) #p = 0.209, so not significant 
summary(lm(crim ~ nox)) #p < 2^-16, so significant
summary(lm(crim ~ rm)) #p = 6.35^-07, so significant
summary(lm(crim ~ age)) #p = 2.85^-16, so significant 
summary(lm(crim ~ dis)) #p < 2^-16, so significant 
summary(lm(crim ~ rad)) #p < 2^-16, so significant 
summary(lm(crim ~ tax))  #p < 2^-16, so significant 
summary(lm(crim ~ ptratio)) #p = 2.94e-11, so significant 
summary(lm(crim ~ lstat)) #p < 2^-16, so significant 
summary(lm(crim ~ medv)) #p < 2^-16, so significant 