# Load the data
delivery <- read.csv(file.choose(), header = T)
View(delivery)
delivery <- as.data.frame(delivery)

# Exploratory data analysis
summary(delivery)

install.packages("Hmisc")
library(Hmisc)
describe(delivery)
?describe

install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(delivery$Sorting.Time, main = "Dot Plot of Sorting.Time Circumferences")
dotplot(delivery$Delivery.Time, main = "Dot Plot")

?boxplot
boxplot(delivery$Sorting.Time, col = "dodgerblue4")
boxplot(delivery$Delivery.Time, col = "red", horizontal = T)

hist(delivery$Sorting.Time)
hist(delivery$Delivery.Time)

# Normal QQ plot
qqnorm(delivery$Sorting.Time)
qqline(delivery$Sorting.Time)

qqnorm(delivery$Delivery.Time)
qqline(delivery$Delivery.Time)

hist(delivery$Sorting.Time, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(delivery$Sorting.Time))             # add a density estimate with defaults
lines(density(delivery$Sorting.Time, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(delivery$Delivery.Time, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(delivery$Delivery.Time))             # add a density estimate with defaults
lines(density(delivery$Delivery.Time, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(delivery$Sorting.Time, delivery$Delivery.Time, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Sorting.Time Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)

?plot

## alternate simple command
plot(delivery$Sorting.Time, delivery$Delivery.Time)

attach(delivery)

# Correlion Coefficient
cor(Sorting.Time, Delivery.Time)

# Covariance
cov(Sorting.Time, Delivery.Time)

# Linear Regression model
reg <- lm(Delivery.Time ~ Sorting.Time, data = delivery) # Y ~ X
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

ggplot(data = delivery, aes(Sorting.Time, Delivery.Time) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = delivery, aes(x = Sorting.Time, y = Delivery.Time)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = delivery, aes(x = Sorting.Time, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, delivery$Delivery.Time)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques

# input = log(x); output = y

plot(log(Sorting.Time), Delivery.Time)
cor(log(Sorting.Time), Delivery.Time)

reg_log <- lm(Delivery.Time ~ log(Sorting.Time), data = delivery)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, delivery$Delivery.Time)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = delivery, aes(log(Sorting.Time), Delivery.Time) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = delivery, aes(x = log(Sorting.Time), y = Delivery.Time)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = delivery, aes(x = log(Sorting.Time), y = pred$fit))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time, log(Delivery.Time))

reg_log1 <- lm(log(Delivery.Time) ~ Sorting.Time, data = delivery)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, delivery$Delivery.Time)

res_log1 = Delivery.Time - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = delivery, aes(Sorting.Time, log(Delivery.Time)) ) +
     geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = delivery, aes(x = Sorting.Time, y = log(Delivery.Time))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = delivery, aes(x = Sorting.Time, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time), data = delivery)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, delivery$Delivery.Time)

res2 = Delivery.Time - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = delivery, aes(Sorting.Time, log(Delivery.Time)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = delivery, aes(x = Sorting.Time + I(Sorting.Time*Sorting.Time), y = log(Delivery.Time))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = delivery, aes(x = Sorting.Time + I(Sorting.Time^2), y = predlog$fit))


# Data Partition

# Random Sampling
n <- nrow(delivery)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- delivery[train_ind, ]
test <-  delivery[-train_ind, ]

# Non-random sampling
train <- delivery[1:15, ]
test <- delivery[16:21, ]

plot(train$Sorting.Time, log(train$Delivery.Time))
plot(test$Sorting.Time, log(test$Delivery.Time))

model <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time * Sorting.Time), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Delivery.Time - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Delivery.Time - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

