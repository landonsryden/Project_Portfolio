# Landon Ryden
# STAT 3366
# Exam 1B

# Set up your working directory
setwd("/Users/landonryden/Desktop/Spring 2023/STAT 3366 Data")

# Import Data
cars <- read.table('cars.txt', header=T)
attach(cars)

# Part 1: MLR Model
cars_reg <- lm(Price ~ CarType + Mileage, data=cars)
summary(cars_reg)
# Price = 60.3 -8.05*(CarTypeJaguar) + 9.95*(CarTypePorsche) - 0.57*(Mileage)

# Part 2: Residual Analysis
plot(cars_reg$fitted.values, cars_reg$residuals, xlab='yhat', ylab='residual', 
     pch=20, cex=1.5, main='residual vs. yhat')
abline(0,0, lty=2)
# Non-constant Variance

plot(Mileage, cars_reg$residuals, xlab='Mileage', ylab='residual', pch=20, cex=1.5,
     main='residual vs. mileage')
abline(0,0, lty=2)

# Part 3: New Model with Log-transformation
logcars_reg <- lm(log(Price) ~ CarType + Mileage)
summary(logcars_reg)

plot(logcars_reg$fitted.values, logcars_reg$residuals, xlab='yhat', ylab='residual', 
     pch=20, cex=1.5, main='log(residual) vs. yhat')
abline(0,0, lty=2)

plot(Mileage, logcars_reg$residuals, xlab='Mileage', ylab='residual', pch=20, cex=1.5,
     main='log(residual) vs. mileage')
abline(0,0, lty=2)
# Non-constant variance of residuals solved.

# Test the log-transform of Mileage, see if an improvement to the model is made.
log2cars_reg <- lm(log(Price) ~ CarType + log(Mileage))
summary(log2cars_reg)

# Result is found to be less accurate than previous model (higher residual 
#   standard error, lower adjusted R^2, less significant variables).  
#   Therefore, logcars_reg is our most accurate model.

# Final Model:
#   log(Price) = 4.172 -0.27*(CarTypeJaguar) + 0.245*(CarTypePorsche) - 0.016*(Mileage)

# Part 4: Interpretation
#   Given a fixed mileage, my model finds that Porsche vehicles are sold at the 
#   highest price, and Jaguar vehicles are solde at the lowest price of the three.

# Part 5: Prediction
#   Price of BMW, given Mileage=40
BMW <- 1000 * exp(4.172311 - 0.269803*(0) + 0.245131*(0) - 0.015803*40)
#   Exponential to undo the log-transformation, price is in units of $1000.
#   Conclusion: At 40,000 miles, my model gives a BMW's predicted value 
#      to be $34,473.5

# Find the 95% Prediction Interval
X_95 <- data.frame(Mileage=c(40),CarType=c('BMW'))
cbind(X_95,exp(predict(logcars_reg, newdata=X_95, interval="prediction", level=0.95)))

# Part 6: Plot
plot(Mileage,log(Price),xlab="Mileage")
points(Mileage[CarType=="Jaguar"],log(Price[CarType=="Jaguar"]),col=2,pch=19)
points(Mileage[CarType=="BMW"],log(Price[CarType=="BMW"]),col=3,pch=19)
points(Mileage[CarType=="Porsche"],log(Price[CarType=="Porsche"]),col=4,pch=19)
abline(coef=logcars_reg$coef[c(1,4)],col=3,lwd=2)
abline(coef=c(logcars_reg$coef[1]+logcars_reg$coef[2], logcars_reg$coef[4]),col=2,lwd=2)
abline(coef=c(logcars_reg$coef[1]+logcars_reg$coef[3], logcars_reg$coef[4]),col=4,lwd=2)
legend("topright", legend=c("CarType = Jaguar", "CarType = BMW", 
                            "CarType = Porsche"), col=c(2,3,4), lty = 1, lwd=2)


