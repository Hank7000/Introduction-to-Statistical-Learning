##Chapter 4 ----
#### 4.7 Lab: Classification methods ----

#initial analysis of the dataset 
library(ISLR2)
names(Smarket)
dim(Smarket)
?Smarket
summary(Smarket)
pairs(Smarket) #kinda useless, need better visualization method
cor(Smarket) #returns error because 'Direction' is qualitative
cor(Smarket[,-9])
max(cor(Smarket[,-9])[cor(Smarket[,-9]) != 1])
attach(Smarket)
plot(Volume)

## 4.7.2 Logistic Regression
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket , family = binomial )
summary(glm.fits)
coef(glm.fits) #accessing the specific coefficients 
summary(glm.fits)$coef #getting the summary for the coefficients only 
summary(glm.fits)$coef[,4] #getting the summary for only the p-values of the coefficients

glm.probs = predict(glm.fits, type = 'response') #predictions for the elements of glm.fits. Will the market go Up or Down based on the predictors?
glm.probs[1:10] 

glm.pred = rep("Down", 1250) #creating vector with as many 'Down' as the observations 
glm.pred[glm.probs > .5] = "Up" #transforming those 'Down' in up based on our predictions 

table(glm.pred, Direction) #confusion table telling us how well our predictions fared against reality 
mean(glm.pred == Direction) #to be fair, we used all of the observations to do this. so this would just be the training error rate 

train = (Year < 2005) #a vector corresponding to observations from 2001 to 2004, used as a training set 
Smarket.2005 = Smarket[!train, ] #the set of all observation of 2005, used as a test set
dim(Smarket.2005)
Direction.2005 = Direction[!train] #the market directions for 2005

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket , family = binomial , subset = train ) #creating linear model using 'train' boolean as a subset
glm.probs = predict(glm.fits, Smarket.2005, type = "response") #predicting Smarket.2005 using our training set

glm.pred = rep("Down", 252) #same method as used before, line 410
glm.pred[glm.probs > .5] = "Up" 
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005) #very disappointing! the test error rate is 0.52, worse than flipping a coin

glm.fits = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial , subset = train) #trying with only the predictors that had slightly better p values 
glm.probs = predict(glm.fits, Smarket.2005, type = "response") #Each element of glm.probs represents the probability of the corresponding observation belonging to the positive class in the logistic regression model.
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

## 4.7.3 Linear Discriminant Analysis
library(MASS)

lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit               
plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

table(lda.pred$class, Direction.2005) #comparing the predictions of the LDA model with the actual values of the market's directions
mean(lda.pred$class == Direction.2005)

#recreating the prediction stored in lda$pred.class 
sum(lda.pred$posterior[,1] >= .5) #market going up: 70
sum(lda.pred$posterior[,1] <= .5) #market going down: 182

##4.7.4 Quadratic Discriminant Analysis 
qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class

table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) #mean is almost 0.6, pretty good for market predictions 

##4.7.5 Naive Bayes 
download.packages(e1071, )
library(e1071)
nb.fit = naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
nb.fit
class(Direction)

nb.class = predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005) #0.59: pretty good! close to QDA and still better than LDA

##4.7.6 K-Nearest Neighbors
library(class)
train.X = cbind(Lag1, Lag2)[train,] #creating a matrix containing our training set 
test.X = cbind(Lag1, Lag2)[!train,] #creating matrix for the test set
train.Direction = Direction[train] #vector containing all the predictions made in the training set

set.seed(1) #setting seed for reproducibility
knn1.pred = knn(train.X, test.X, train.Direction, k = 1) #here is our model, with k = 1, for now 
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) #disappointing!!! 0.5, basically like flipping a coin 

knn3.pred = knn(train.X, test.X, train.Direction, k = 3) #trying again with k=3
table(knn3.pred, Direction.2005)
mean(knn3.pred == Direction.2005) #0.53: slight improvement but nothing crazy

# KNN does not perform that well on this dataset, so the book suggests to try it on a different one, named Caravan 

dim(Caravan)
attach(Caravan)

standardized.X = scale(Caravan[, -86]) #first thing first, lets normalize the data! We exclude the 86th column, which is our output, Purchase

test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != 'No')
table(knn.pred, test.Y)

#testing for k=3
knn3.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn3.pred, test.Y)
mean(test.Y != knn3.pred)

#testing for k=5
knn5.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn5.pred, test.Y)
mean(test.Y != knn5.pred)


##4.7.7 Poisson Regression
attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)

#starting with a least squares linear regression model
mod.lm = lm(bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare )
summary(mod.lm)

#fitting the Poisson model
mod.pois = glm(bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare , family = poisson )
summary(mod.pois)

coef.mnth = c(coef(mod.pois)[2:12], -sum(coef(mod.pois)[2:12]))
plot(coef.mnth, xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J" , "J", "A", "S", "O", "N", "D"))

coef.hours = c(coef(mod.pois)[13:35], -sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19, type = "o")

##### 4.8 Applied Exercises ----
#Ex. 13
head(Weekly)
attach(Weekly)
levels(as.factor(Weekly$Year))
dim(Weekly)
#13a. initial numerical and graphical summaries to test the waters 
summary(Weekly)
cor(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Today")])

#some histograms, could use a for loop but faster this way, sorry
par(mfrow = c(3, 3)) 
hist(Weekly$Lag1, main = "Lag1")
hist(Weekly$Lag2, main = "Lag2")
hist(Weekly$Lag3, main = "Lag3")
hist(Weekly$Lag4, main = "Lag4")
hist(Weekly$Lag5, main = "Lag5")
hist(Weekly$Volume, main = "Volume")
hist(Weekly$Today, main = "Today")
#all the data from the previous weeks seem to follow a simil-normal distribution, except Volume, which has no negative values 
pairs(~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume + Today, data = Weekly) #big mess, nothing seems very interesting for now 

#13b first logistic regression 
glm.Weekly = glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = "binomial", data = Weekly)
summary(glm.Weekly)


#13c confusion matrix etc.
#whole procedure always a bit confusing, its gonna need some practice. Wonder how can it be automated?
glm.probs.Weekly =  predict(glm.Weekly, type = "response")
glm.pred.Weekly = rep('Down', 1089)
glm.pred.Weekly[glm.probs.Weekly > .5] = 'Up'
table(glm.pred.Weekly, Weekly$Direction)
head(glm.probs.Weekly)

#13d
train.Weekly = Weekly$Year < 2008 #this is a boolean vector, Trues are the obs. we included and Falses are the ones we didnt 
test.Weekly = Weekly[!train.Weekly,] #this is the whole dataset, but without the training observations 
test.Weekly.dir = Weekly$Direction[!train.Weekly] #this is the collection of Direction values from the test set, we are gonna use it later in our table 


glm.Weekly.Lag2 = glm(formula = Direction ~ Lag2, family = "binomial", data = Weekly, subset = train.Weekly)
glm.Weekly.Lag2a = glm(formula = Direction ~ Lag2, family = "binomial", data = Weekly[train.Weekly,]) #another way to do the same thing, instead of using the subset argument 

glm.Weekly.prob = predict(glm.Weekly.Lag2, test.Weekly, type = 'response') #our predictions on the test set  

glm.Weekly.pred = ifelse(glm.Weekly.prob > 0.5, "Up", "Down") #MUCH BETTER! another way to create a vector containing the predictions 

table(glm.Weekly.pred, test.Weekly.dir) #the table should contain the vector containing the predictions and the values from the test set 

#13e
lda.Weekly.Lag2 = lda(Direction ~ Lag2, data = Weekly, subset = train.Weekly)
lda.Weekly.prob = predict(lda.Weekly.Lag2, newdata = test.Weekly)$posterior[, "Up"]
lda.Weekly.pred = ifelse(lda.Weekly.prob > 0.5, "Up", "Down")

conf_matrix_lda = table(lda.Weekly.pred, test.Weekly.dir)


correct_predictions_lda <- sum(diag(conf_matrix_lda))
total_predictions_lda <- sum(conf_matrix_lda)
fraction_correct_lda <- correct_predictions_lda / total_predictions_lda
print(paste("Overall fraction of correct predictions (LDA):", fraction_correct_lda))

#13f
qda.Weekly.Lag2 = qda(Direction ~ Lag2, data = Weekly, subset = train.Weekly)
qda.Weekly.prob = predict(qda.Weekly.Lag2, newdata = test.Weekly)$posterior[, "Up"]
qda.Weekly.pred = ifelse(qda.Weekly.prob > 0.5, "Up", "Down")

conf_matrix_qda = table(Predicted = qda.Weekly.pred, Actual = test.Weekly.dir)
print(conf_matrix_qda)

correct_predictions_qda = sum(diag(conf_matrix_qda))
total_predictions_qda = sum(conf_matrix_qda)
fraction_correct_qda = correct_predictions_qda / total_predictions_qda
print(paste("Correct predictions (QDA):", fraction_correct_qda))

#13g
knn.Weekly.Lag2 = knn(train = train.Weekly$Lag2, test = test.Weekly$Lag2, cl = train.Weekly$Direction, k = 1)

conf_matrix_knn = table(Predicted = knn.Weekly.Lag2, Actual = test.Weekly.dir)
print(conf_matrix_knn)

correct_predictions_knn = sum(diag(conf_matrix_knn))
total_predictions_knn = sum(conf_matrix_knn)
fraction_correct_knn = correct_predictions_knn / total_predictions_knn
print(paste("Correct predictions (kNN, k=1):", fraction_correct_knn))