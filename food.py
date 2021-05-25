# Importing necessary libraries
import pandas as pd # deals with data frame 
import numpy as np  # deals with numerical values

food = pd.read_csv("C:/Users/Bibhuti/OneDrive/Desktop/360digiTMG assignment/SLR/calories_consumed.csv")
food=food.rename(columns={'Weight gained (grams)':'weight_gained','Calories Consumed':'calories'})

# Explorory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

food.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

plt.bar(height = food.calories, x = np.arange(1, 15, 1))
plt.hist(food.calories) #histogram
plt.boxplot(food.calories) #boxplot

plt.bar(height = food.weight_gained, x = np.arange(1, 15, 1))
plt.hist(food.weight_gained) #histogram
plt.boxplot(food.weight_gained) #boxplot

# Scatter plot
plt.scatter(x = food['weight_gained'], y = food['calories'], color = 'green') 

# correlation
np.corrcoef(food.weight_gained, food.calories) 

# Covariance
# NumPy does not have a function to calculate the covariance between two variables directly. 
# Function for calculating a covariance matrix called cov() 
# By default, the cov() function will calculate the unbiased or sample covariance between the provided random variables.

cov_output = np.cov(food.weight_gained, food.calories)[0, 1]
cov_output

# food.cov()


# Import library
import statsmodels.formula.api as smf

# Simple Linear Regression
model = smf.ols('calories ~ weight_gained', data = food).fit()
model.summary()

pred1 = model.predict(pd.DataFrame(food['weight_gained']))

# Regression Line
plt.scatter(food.weight_gained, food.calories)
plt.plot(food.weight_gained, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res1 = food.calories - pred1
res_sqr1 = res1 * res1
mse1 = np.mean(res_sqr1)
rmse1 = np.sqrt(mse1)
rmse1

######### Model building on Transformed Data
# Log Transformation
# x = log(weight_gained); y = at

plt.scatter(x = np.log(food['weight_gained']), y = food['calories'], color = 'brown')
np.corrcoef(np.log(food.weight_gained), food.calories) #correlation

model2 = smf.ols('calories ~ np.log(weight_gained)', data = food).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(food['weight_gained']))

# Regression Line
plt.scatter(np.log(food.weight_gained), food.calories)
plt.plot(np.log(food.weight_gained), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res2 = food.calories - pred2
res_sqr2 = res2 * res2
mse2 = np.mean(res_sqr2)
rmse2 = np.sqrt(mse2)
rmse2


#### Exponential transformation
# x = weight_gained; y = log(at)

plt.scatter(x = food['weight_gained'], y = np.log(food['calories']), color = 'orange')
np.corrcoef(food.weight_gained, np.log(food.calories)) #correlation

model3 = smf.ols('np.log(calories) ~ weight_gained', data = food).fit()
model3.summary()

pred3 = model3.predict(pd.DataFrame(food['weight_gained']))
pred3_at = np.exp(pred3)
pred3_at

# Regression Line
plt.scatter(food.weight_gained, np.log(food.calories))
plt.plot(food.weight_gained, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res3 = food.calories - pred3_at
res_sqr3 = res3 * res3
mse3 = np.mean(res_sqr3)
rmse3 = np.sqrt(mse3)
rmse3


#### Polynomial transformation
# x = weight_gained; x^2 = weight_gained*weight_gained; y = log(at)

model4 = smf.ols('np.log(calories) ~ weight_gained+ I(weight_gained*weight_gained)', data = food).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(food))
pred4_at = np.exp(pred4)
pred4_at

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = food.iloc[:, 0:1].values
X_poly = poly_reg.fit_transform(X)
# y = food.iloc[:, 1].values


plt.scatter(food.weight_gained, np.log(food.calories))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()


# Error calculation
res4 = food.calories - pred4_at
res_sqr4 = res4 * res4
mse4 = np.mean(res_sqr4)
rmse4 = np.sqrt(mse4)
rmse4


# Choose the best model using RMSE
data = {"MODEL":pd.Series(["SLR", "Log model", "Exp model", "Poly model"]), "RMSE":pd.Series([rmse1, rmse2, rmse3, rmse4])}
table_rmse = pd.DataFrame(data)
table_rmse

###################
# The best model

from sklearn.model_selection import train_test_split

train, test = train_test_split(food, test_size = 0.3)

finalmodel = smf.ols('np.log(calories) ~ weight_gained + I(weight_gained*weight_gained)', data = train).fit()
finalmodel.summary()

# Predict on test data
test_pred = finalmodel.predict(pd.DataFrame(test))
pred_test_calories = np.exp(test_pred)
pred_test_calories

# Model Evaluation on Test data
test_res = test.calories - pred_test_calories
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
pred_train_calories = np.exp(train_pred)
pred_train_calories

# Model Evaluation on train data
train_res = train.calories - pred_train_calories
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse

