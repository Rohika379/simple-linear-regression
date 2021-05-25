# Load the data
food <- read.csv(file.choose(), header = T)
View(food)

# Exploratory data analysis
summary(food)

install.packages("Hmisc")
library(Hmisc)
describe(food)
?describe

install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(food$Weight.gained..grams., main = "Dot Plot of Weight.gained..grams. Circumferences")
dotplot(food$Calories.Consumed, main = "Dot Plot of Adipose Tissue Areas")

?boxplot
boxplot(food$Weight.gained..grams., col = "dodgerblue4")
boxplot(food$Calories.Consumed, col = "red", horizontal = T)

hist(food$Weight.gained..grams.)
hist(food$Calories.Consumed)

# Normal QQ plot
qqnorm(food$Weight.gained..grams.)
qqline(food$Weight.gained..grams.)

qqnorm(food$Calories.Consumed)
qqline(food$Calories.Consumed)

hist(food$Weight.gained..grams., prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(food$Weight.gained..grams.))             # add a density estimate with defaults
lines(density(food$Weight.gained..grams., adjust = 2), lty = "dotted")   # add another "smoother" density

hist(food$Calories.Consumed, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(food$Calories.Consumed))             # add a density estimate with defaults
lines(density(food$Calories.Consumed, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(food$Weight.gained..grams., food$Calories.Consumed, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Weight.gained..grams. Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)

?plot

## alternate simple command
plot(food$Weight.gained..grams., food$Calories.Consumed)

attach(food)

# CorrelCalories.Consumedion Coefficient
cor(Weight.gained..grams., Calories.Consumed)

# Covariance
cov(Weight.gained..grams., Calories.Consumed)

# Linear Regression model
reg <- lm(Calories.Consumed ~ Weight.gained..grams., data = food) # Y ~ X
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

ggplot(data = food, aes(Weight.gained..grams., Calories.Consumed) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = food, aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = food, aes(x = Weight.gained..grams., y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, food$Calories.Consumed)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques

# input = log(x); output = y

plot(log(Weight.gained..grams.), Calories.Consumed)
cor(log(Weight.gained..grams.), Calories.Consumed)

reg_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.), data = food)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, food$Calories.Consumed)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = food, aes(log(Weight.gained..grams.), Calories.Consumed) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = food, aes(x = log(Weight.gained..grams.), y = Calories.Consumed)) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = food, aes(x = log(Weight.gained..grams.), y = pred$fit))



# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Weight.gained..grams., log(Calories.Consumed))
cor(Weight.gained..grams., log(Calories.Consumed))

reg_log1 <- lm(log(Calories.Consumed) ~ Weight.gained..grams., data = food)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, food$Calories.Consumed)

res_log1 = Calories.Consumed - pred$fit
rmse <- sqrt(mean(res_log1^2))
rmse

# Regression line for data
ggplot(data = food, aes(Weight.gained..grams., log(Calories.Consumed)) ) +
     geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = food, aes(x = Weight.gained..grams., y = log(Calories.Consumed))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = food, aes(x = Weight.gained..grams., y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(Calories.Consumed) ~ Weight.gained..grams. + I(Weight.gained..grams.*Weight.gained..grams.), data = food)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, food$Calories.Consumed)

res2 = Calories.Consumed - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = food, aes(Weight.gained..grams., log(Calories.Consumed)) ) +
     geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = food, aes(x = Weight.gained..grams. + I(Weight.gained..grams.*Weight.gained..grams.), y = log(Calories.Consumed))) + 
     geom_point(color = 'blue') +
     geom_line(color = 'red', data = food, aes(x = Weight.gained..grams. + I(Weight.gained..grams.^2), y = predlog$fit))


# Data Partition

# Random Sampling
n <- nrow(food)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- food[train_ind, ]
test <-  food[-train_ind, ]

# Non-random sampling
train <- food[1:10, ]
test <- food[11:14, ]

plot(train$Weight.gained..grams., log(train$Calories.Consumed))
plot(test$Weight.gained..grams., log(test$Calories.Consumed))

model <- lm(log(Calories.Consumed) ~ Weight.gained..grams. + I(Weight.gained..grams. * Weight.gained..grams.), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Calories.Consumed - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Calories.Consumed - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

