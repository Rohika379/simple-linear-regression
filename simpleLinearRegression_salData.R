# Load the data
salary_data <- read.csv(file.choose(), header = T)

View(salary_data)
salary_data <- as.data.frame(salary_data)

# Exploratory data analysis
summary(salary_data)

install.packages("Hmisc")
library(Hmisc)
describe(salary_data)
?describe

install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(salary_data$YearsExperience, main = "Dot Plot of YearsExperience Circumferences")
dotplot(salary_data$Salary, main = "Dot Plot")

?boxplot
boxplot(salary_data$YearsExperience, col = "dodgerblue4")
boxplot(salary_data$Salary, col = "red", horizontal = T)

hist(salary_data$YearsExperience)
hist(salary_data$Salary)

# Normal QQ plot
qqnorm(salary_data$YearsExperience)
qqline(salary_data$YearsExperience)

qqnorm(salary_data$Salary)
qqline(salary_data$Salary)

hist(salary_data$YearsExperience, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(salary_data$YearsExperience))             # add a density estimate with defaults
lines(density(salary_data$YearsExperience, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(salary_data$Salary, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(salary_data$Salary))             # add a density estimate with defaults
lines(density(salary_data$Salary, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(salary_data$YearsExperience, salary_data$Salary, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "YearsExperience Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)

?plot

## alternate simple command
plot(salary_data$YearsExperience, salary_data$Salary)

attach(salary_data)

# Correlion Coefficient
cor(YearsExperience, Salary)

# Covariance
cov(YearsExperience, Salary)

# Linear Regression model
reg <- lm(Salary ~ YearsExperience, data = salary_data) # Y ~ X
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

ggplot(data = salary_data, aes(YearsExperience, Salary) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = salary_data, aes(x = YearsExperience, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, salary_data$Salary)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques

# input = log(x); output = y

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience), data = salary_data)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, salary_data$Salary)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = salary_data, aes(log(YearsExperience), Salary) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = salary_data, aes(x = log(YearsExperience), y = Salary)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = salary_data, aes(x = log(YearsExperience), y = pred$fit))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))

reg_log1 <- lm(log(Salary) ~ YearsExperience, data = salary_data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, salary_data$Salary)

res_log1 = Salary - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = salary_data, aes(YearsExperience, log(Salary)) ) +
     geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = salary_data, aes(x = YearsExperience, y = log(Salary))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = salary_data, aes(x = YearsExperience, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience), data = salary_data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, salary_data$Salary)

res2 = Salary - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = salary_data, aes(YearsExperience, log(Salary)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = salary_data, aes(x = YearsExperience + I(YearsExperience*YearsExperience), y = log(Salary))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = salary_data, aes(x = YearsExperience + I(YearsExperience^2), y = predlog$fit))


# Data Partition

# Random Sampling
n <- nrow(salary_data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- salary_data[train_ind, ]
test <-  salary_data[-train_ind, ]

# Non-random sampling
train <- salary_data[1:21, ]
test <- salary_data[22:30, ]

plot(train$YearsExperience, log(train$Salary))
plot(test$YearsExperience, log(test$Salary))

model <- lm(log(Salary) ~ YearsExperience + I(YearsExperience * YearsExperience), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Salary - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Salary - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

