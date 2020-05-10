#Min Kyaw Thu ECON 178 Final Project
data_tr <- read.table("R/data_tr.txt", header = TRUE, sep = "\t", dec = ".")[,-1]
set.seed(88)
#setting seed as 88 so it is saved

#Going to inspect the data set
dim(data_tr) 
#dimension gives me the number of observations and variables

head(data_tr)
#shows me the summary of the table for the first 6 

names(data_tr) 
#names of variables shows me the name of them and the proper way to address or use

summary (data_tr$tw)
#shows me the summary of total wealth, its max, min, avg and interquartile numbers

boxplot(data_tr$tw)
boxplot(data_tr)
boxplot(data_tr$nifa)
#shows me box plots and to identify outliers 

hist(data_tr$tw, breaks=100, main="Histogram of Total Wealth", xlab="Total Wealth") 
#Histogram to show the frequency 

pairs(data_tr[,c(1,2,3)])
pairs(data_tr)
#shows 18^2 graphs and the correlation and dependence between all of them
#shows the dependence between the first three covariates
#shows (assumes correlation) between the variables

par(mfrow=c(2,3))
#allows me to plot graphs side by side (2x3) and compare it easily 

plot(data_tr$age, data_tr$tw, main="Age VS Total Wealth", xlab="Age", ylab="Total Wealth") 
plot(data_tr$fsize, data_tr$tw, main="Family Size VS Total Wealth", xlab="Family Size", ylab="Total Wealth") 
plot(data_tr$ira, data_tr$tw, main="Individual Retirement Account VS Total Wealth", xlab="Invididual Retirement Account", ylab="Total Wealth") 
plot(data_tr$inc, data_tr$tw, main="Income VS Total Wealth", xlab="Income", ylab="Total Wealth") 
plot(data_tr$educ, data_tr$tw, main="No.of years in Educ VS Total Wealth", xlab="#No. of yrs in Educ", ylab="Total Wealth") 
plot(data_tr$nifa, data_tr$tw, main="non-401k fin assets VS Total Wealth", xlab="non-401k financial assets", ylab="Total Wealth") 
#plot total wealth as a result of these 6 chosen variables and having graph labels

dataremoved = subset(data_tr, tw > -500000 & tw < 1500000 & nifa < 800000 & inc < 235000 & fsize < 10)
#removed leverage point and outliers from observation

dim(dataremoved)
#shows the dimentsion after removing those outliers

plot(dataremoved$age, dataremoved$tw, main="Age VS Total Wealth", xlab="Age", ylab="Total Wealth") 
plot(dataremoved$fsize, dataremoved$tw, main="Family Size VS Total Wealth", xlab="Family Size", ylab="Total Wealth") 
plot(dataremoved$ira, dataremoved$tw, main="Individual Retirement Account VS Total Wealth", xlab="Invididual Retirement Account", ylab="Total Wealth") 
plot(dataremoved$inc, dataremoved$tw, main="Income VS Total Wealth", xlab="Income", ylab="Total Wealth") 
plot(dataremoved$educ, dataremoved$tw, main="No.of years in Educ VS Total Wealth", xlab="#No. of yrs in Educ", ylab="Total Wealth") 
plot(dataremoved$nifa, dataremoved$tw, main="non-401k fin assets VS Total Wealth", xlab="non-401k financial assets", ylab="Total Wealth") 
#plots those graphs again without those outliers and see if it looks smoother

#forward
library(MASS)
full <- lm(tw ~ ., data=dataremoved)
null <- lm(tw ~ 1, data=dataremoved)
a <- stepAIC(null, scope=list(lower=null, upper=full), direction='forward')
# forward stepwise - AIC
b <- stepAIC(full, scope=list(lower=null, upper=full), direction='backward')
# backward stepwise - AIC
coef(a)
# Look at the coefficients to know which variables were selected
coef(b)

stepwiseforward <- predict(a, newdata=dataremoved)
stepwisebackward <- predict(b, newdata=dataremoved)

extractAIC(a)
extractAIC(b)
#same results from both ways. does not matter which is chosen

#best subset from running stepwise
#ira + e401 + nifa + inc + hmort + hval + male + twoearn + age + marr
Model1 = lm(tw ~ ira + e401 + nifa+ inc + hmort + hval + male + twoearn + age + marr, data = dataremoved)
Model1
summary(Model1)
#looking and compareing the p-values. all are close to zero except marr which gives 0.1
#assume significance level is 0.05

#creating covariates amongst the chosen 6 continous variables
CreateCov <- cbind(dataremoved$ira,dataremoved$e401,dataremoved$nifa,dataremoved$inc,dataremoved$hmort,dataremoved$hval,dataremoved$male,dataremoved$twoearn,dataremoved$age,dataremoved$marr)
CC <- cov(CreateCov)
X = as.data.frame(CC)
Xd <- model.matrix(~. ,data=X)[,-1]

#running lasso, ridge
#ridege first
#both uses glmnet package as library
library(glmnet)

lambdasridger <- exp(seq(-1, 10, length = 100))
y <- dataremoved$tw
x <- as.matrix(dataremoved[,-1])

fit <- glmnet(x, y, alpha = 0, lambda  = lambdasridger)
summary(fit)

ridgeCV <- cv.glmnet(x, y, lambda = lambdasridger,alpha = 0)
#alpha=0 for Ridge penalty
bestlambdaRR <- ridgeCV$lambda.min
bestlambdaRR
#value of lambda that gives minimum mean cross-validated error
ridgePred <- predict(ridgeCV, x, s = bestlambdaRR ) 
#Predict on the training data set using lambdamin.

#lasso
lassoCV <- cv.glmnet(x, y, lambda = lambdasridger,alpha = 1) 
#alpha=1 for LASSO penalty
bestlambdaLS <- lassoCV$lambda.min
bestlambdaLS
#value of lambda that gives minimum mean cross-validated error
lassoPred <- predict(lassoCV, x, s = lassoCV$lambda.min )
#Predict on the training data set using lambdamin.
#Cross Validation: Ridge vs Lasso
c(min(lassoCV$cvm), min(ridgeCV$cvm))
which.min(c(min(lassoCV$cvm), min(ridgeCV$cvm)))
#lasso cv gives lower mspe which is what we want
n <- length(y)
k <- 10
ii <- sample(rep(1:k, length= n))
pr.stepwise_backward <- pr.stepwise_forward <- pr.lasso <- pr.ridge <- rep(NA, length(y))

mspeLasso <- mean((lassoPred-y)^2)
msperidge <- mean((ridgePred-y)^2)
mspestepbackward <- mean((stepwisebackward-y)^2)
mspestepforward <- mean((stepwiseforward-y)^2)
cbind(mspeLasso,msperidge,mspestepbackward,mspestepforward)
which.min(cbind(mspeLasso,msperidge,mspestepbackward,mspestepforward))

#polynomial
#Find best degree 
#decide to use the 6 continuous variables
#running loops for all to decide which degree is the best for each var
y = dataremoved$tw
len = length(y)
k = 10
ii = sample(rep(1:k, length= len))
max_poly = 10 
poly_cv = matrix(data=NA,nrow=length(y),ncol=max_poly)

#classified based on the 6 vars 

#ira
mse_cv1 = rep(NA, max_poly)
for (s in 1:max_poly){    
  for (j in 1:k){
    hold = (ii == j)
    train = (ii != j)
    pol = lm(tw ~ poly(ira,s), data=dataremoved[train,])
    poly_cv[hold,s] = predict(pol, newdata=dataremoved[hold,])
  }
  mse_cv1[s] <- mean((y - poly_cv[,s])**2)
}
mse_cv1
plot(mse_cv1)
which.min(mse_cv1)
#nifa
mse_cv2 = rep(NA, max_poly)
for (s in 1:max_poly){    
  for (j in 1:k){
    hold = (ii == j)
    train = (ii != j)
    pol = lm(tw ~ poly(nifa,s), data=dataremoved[train,])
    poly_cv[hold,s] = predict(pol, newdata=dataremoved[hold,])
  }
  mse_cv2[s] <- mean((y - poly_cv[,s])**2)
}
mse_cv2
plot(mse_cv2)
which.min(mse_cv2)

#inc
mse_cv3 = rep(NA, max_poly)
for (s in 1:max_poly){    
  for (j in 1:k){
    hold = (ii == j)
    train = (ii != j)
    pol = lm(tw ~ poly(inc,s), data=dataremoved[train,])
    poly_cv[hold,s] = predict(pol, newdata=dataremoved[hold,])
  }
  mse_cv3[s] <- mean((y - poly_cv[,s])**2)
}
mse_cv3
plot(mse_cv3)
which.min(mse_cv3)

#hmort
mse_cv4 = rep(NA, max_poly)
for (s in 1:max_poly){    
  for (j in 1:k){
    hold = (ii == j)
    train = (ii != j)
    pol = lm(tw ~ poly(hmort,s), data=dataremoved[train,])
    poly_cv[hold,s] = predict(pol, newdata=dataremoved[hold,])
  }
  mse_cv4[s] <- mean((y - poly_cv[,s])**2)
}
mse_cv4
plot(mse_cv4)
which.min(mse_cv4)

#hval
mse_cv5 = rep(NA, max_poly)
for (s in 1:max_poly){    
  for (j in 1:k){
    hold = (ii == j)
    train = (ii != j)
    pol = lm(tw ~ poly(hval,s), data=dataremoved[train,])
    poly_cv[hold,s] = predict(pol, newdata=dataremoved[hold,])
  }
  mse_cv5[s] <- mean((y - poly_cv[,s])**2)
}
mse_cv5
plot(mse_cv5)
which.min(mse_cv5)

#age
mse_cv6 = rep(NA, max_poly)
for (s in 1:max_poly){    
  for (j in 1:k){
    hold = (ii == j)
    train = (ii != j)
    pol = lm(tw ~ poly(age,s), data=dataremoved[train,])
    poly_cv[hold,s] = predict(pol, newdata=dataremoved[hold,])
  }
  mse_cv6[s] <- mean((y - poly_cv[,s])**2)
}
mse_cv6
plot(mse_cv6)
which.min(mse_cv6)

#plug the degrees into polynomial model
BestPolynomial = lm(tw ~ poly(ira,8) + poly(nifa, 6) + poly(inc, 2) + poly(hmort, 5) + poly(hval,2) + poly(age,1), data = dataremoved)
BestPolynomial
summary(BestPolynomial)
MSPEPoly <- mean(resid(BestPolynomial)^2)
MSPEPoly

#checking polynomial with no lower degrees
BestPolyI = lm(tw ~ I(ira^8) + I(nifa^6) + I(inc^2) + I(hmort^5) + I(hval^2)+ poly(age,1), data = dataremoved)
BestPolyI
summary(BestPolyI)
MSPEPolyI <- mean(resid(BestPolyI)^2)
MSPEPolyI

c(MSPEPoly,MSPEPolyI)
which.min(c(MSPEPoly,MSPEPolyI))

which.min(cbind(mspeLasso,msperidge,mspestepbackward,mspestepforward,MSPEPoly,MSPEPolyI))

#complete polynomial regression with 6 var and more
BestPolynomialC = lm(tw ~ poly(ira,8) + poly(nifa, 6) + poly(inc, 2) + poly(hmort, 5) + poly(hval,2) + poly(age,1) + male + twoearn + age + marr, data = dataremoved)
BestPolynomialC
summary(BestPolynomialC)
MSPEPolyC <- mean(resid(BestPolynomialC)^2)
MSPEPolyC

which.min(cbind(mspeLasso,msperidge,mspestepbackward,mspestepforward,MSPEPoly,MSPEPolyI,MSPEPolyC))

#spline method
library(ISLR)
library(splines)

NaturalSpline <- lm(tw ~ ns(ira,7)+ns(nifa,6)+ns(inc,4)+ns(hmort,5)+ns(hval,2) + ns(age,1) + male + twoearn + age + marr, data=dataremoved)
summary(NaturalSpline)
par(mfrow=c(2,2))
plot(NaturalSpline)
MSPENaturalSpline <- mean(resid(NaturalSpline)^2)
MSPENaturalSpline


which.min(c(mspeLasso,msperidge,mspestepbackward,mspestepforward,MSPEPoly,MSPEPolyI,MSPEPolyC,MSPENaturalSpline))
#compare models with lowest mspe
#it shows that the complete polynomial regression with all 6 var and more gives the lowest mspe and hence I will choose this model to predict total wealth
