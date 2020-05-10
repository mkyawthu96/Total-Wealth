data_te <- read.table("R/data_for_prediction.txt", header = TRUE, sep = "\t", dec = ".")[,-1]

BestPolynomialC = lm(tw ~ poly(ira,8) + poly(nifa, 6) + poly(inc, 2) + poly(hmort, 5) + poly(hval,2) + poly(age,1) + male + twoearn + age + marr, data = dataremoved)
BestPolynomialC
summary(BestPolynomialC)
MSPEPolyC <- mean(resid(BestPolynomialC)^2)
MSPEPolyC

#my best model is a polynomial and I have chosen it due to having the lowest mspe

predFinal <- predict(BestPolynomialC, newdata = data_te)

write.table(predFinal, file = 'R/my_predictions.txt')
#created a text file that shows 1982 predictions saved as my_predictions
#checked the values and it seems feasible to be considered total wealth
