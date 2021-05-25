# Load the data
empData <- read.csv(file.choose(), header = T)

View(empData)
empData <- as.data.frame(empData)

# Exploratory data analysis
summary(empData)

install.packages("Hmisc")
library(Hmisc)
describe(empData)
?describe

install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(empData$Salary_hike, main = "Dot Plot of Salary_hike Circumferences")
dotplot(empData$Churn_out_rate, main = "Dot Plot")

?boxplot
boxplot(empData$Salary_hike, col = "dodgerblue4")
boxplot(empData$Churn_out_rate, col = "red", horizontal = T)

hist(empData$Salary_hike)
hist(empData$Churn_out_rate)

# Normal QQ plot
qqnorm(empData$Salary_hike)
qqline(empData$Salary_hike)

qqnorm(empData$Churn_out_rate)
qqline(empData$Churn_out_rate)

hist(empData$Salary_hike, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(empData$Salary_hike))             # add a density estimate with defaults
lines(density(empData$Salary_hike, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(empData$Churn_out_rate, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(empData$Churn_out_rate))             # add a density estimate with defaults
lines(density(empData$Churn_out_rate, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(empData$Salary_hike, empData$Churn_out_rate, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Salary_hike Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)

?plot

## alternate simple command
plot(empData$Salary_hike, empData$Churn_out_rate)

attach(empData)

# Correlion Coefficient
cor(Salary_hike, Churn_out_rate)

# Covariance
cov(Salary_hike, Churn_out_rate)

# Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike, data = empData) # Y ~ X
?lm
summary(reg)

confint(reg, level = 0.95)
?confint

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)
?predict

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = empData, aes(Salary_hike, Churn_out_rate) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = empData, aes(x = Salary_hike, y = Churn_out_rate)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = empData, aes(x = Salary_hike, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, empData$Churn_out_rate)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques

# input = log(x); output = y

plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

reg_log <- lm(Churn_out_rate ~ log(Salary_hike), data = empData)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, empData$Churn_out_rate)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = empData, aes(log(Salary_hike), Churn_out_rate) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = empData, aes(x = log(Salary_hike), y = Churn_out_rate)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = empData, aes(x = log(Salary_hike), y = pred$fit))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))

reg_log1 <- lm(log(Churn_out_rate) ~ Salary_hike, data = empData)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, empData$Churn_out_rate)

res_log1 = Churn_out_rate - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = empData, aes(Salary_hike, log(Churn_out_rate)) ) +
     geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = empData, aes(x = Salary_hike, y = log(Churn_out_rate))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = empData, aes(x = Salary_hike, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike), data = empData)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, empData$Churn_out_rate)

res2 = Churn_out_rate - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = empData, aes(Salary_hike, log(Churn_out_rate)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = empData, aes(x = Salary_hike + I(Salary_hike*Salary_hike), y = log(Churn_out_rate))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = empData, aes(x = Salary_hike + I(Salary_hike^2), y = predlog$fit))


# Data Partition

# Random Sampling
n <- nrow(empData)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- empData[train_ind, ]
test <-  empData[-train_ind, ]

# Non-random sampling
train <- empData[1:7, ]
test <- empData[8:10, ]

plot(train$Salary_hike, log(train$Churn_out_rate))
plot(test$Salary_hike, log(test$Churn_out_rate))

model <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike * Salary_hike), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Churn_out_rate - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Churn_out_rate - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

