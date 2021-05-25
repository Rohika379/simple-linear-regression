

# Importing necessary libraries
import pandas as pd # deals with data frame  
import numpy as np  # deals with numerical values

spa = pd.read_csv("C:\\Users\\Bibhuti\\OneDrive\\Desktop\\360digiTMG assignment\\SLR\\SAT_GPA.csv")

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

spa.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

plt.bar(height = spa.SAT_Scores, x = np.arange(1, 201, 1))
plt.hist(spa.SAT_Scores) #histogram
plt.boxplot(spa.SAT_Scores) #boxplot

plt.bar(height = spa.GPA, x = np.arange(1, 201, 1))
plt.hist(spa.GPA) #histogram
plt.boxplot(spa.GPA) #boxplot

# Scatter plot
plt.scatter(x = spa['GPA'], y = spa['SAT_Scores'], color = 'green') 

# correlation
np.corrcoef(spa.GPA, spa.SAT_Scores) 

# Covariance
# NumPy does not have a function to calculate the covariance between two variables directly. 
# Function for calculating a covariance matrix called cov() 
# By default, the cov() function will calculate the unbiased or sample covariance between the provided random variables.

cov_output = np.cov(spa.GPA, spa.SAT_Scores)[0, 1]
cov_output

# spa.cov()


# Import library
import statsmodels.formula.api as smf

# Simple Linear Regression
model = smf.ols('SAT_Scores ~ GPA', data = spa).fit()
model.summary()

pred1 = model.predict(pd.DataFrame(spa['GPA']))

# Regression Line
plt.scatter(spa.GPA, spa.SAT_Scores)
plt.plot(spa.GPA, pred1, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res1 = spa.SAT_Scores - pred1
res_sqr1 = res1 * res1
mse1 = np.mean(res_sqr1)
rmse1 = np.sqrt(mse1)
rmse1

######### Model building on Transformed Data
# Log Transformation
# x = log(GPA); y = SAT_Scores

plt.scatter(x = np.log(spa['GPA']), y = spa['SAT_Scores'], color = 'brown')
np.corrcoef(np.log(spa.GPA), spa.SAT_Scores) #correlation

model2 = smf.ols('SAT_Scores ~ np.log(GPA)', data = spa).fit()
model2.summary()

pred2 = model2.predict(pd.DataFrame(spa['GPA']))

# Regression Line
plt.scatter(np.log(spa.GPA), spa.SAT_Scores)
plt.plot(np.log(spa.GPA), pred2, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res2 = spa.SAT_Scores - pred2
res_sqr2 = res2 * res2
mse2 = np.mean(res_sqr2)
rmse2 = np.sqrt(mse2)
rmse2


#### Exponential transformation
# x = GPA; y = log(SAT_Scores)

plt.scatter(x = spa['GPA'], y = np.log(spa['SAT_Scores']), color = 'orange')
np.corrcoef(spa.GPA, np.log(spa.SAT_Scores)) #correlation

model3 = smf.ols('np.log(SAT_Scores) ~ GPA', data = spa).fit()
model3.summary()

pred3 = model3.predict(pd.DataFrame(spa['GPA']))
pred3_at = np.exp(pred3)
pred3_at

# Regression Line
plt.scatter(spa.GPA, np.log(spa.SAT_Scores))
plt.plot(spa.GPA, pred3, "r")
plt.legend(['Predicted line', 'Observed data'])
plt.show()

# Error calculation
res3 = spa.SAT_Scores - pred3_at
res_sqr3 = res3 * res3
mse3 = np.mean(res_sqr3)
rmse3 = np.sqrt(mse3)
rmse3


#### Polynomial transformation
# x = GPA; x^2 = GPA*GPA; y = log(SAT_Scores)

model4 = smf.ols('np.log(SAT_Scores) ~ GPA + I(GPA*GPA)', data = spa).fit()
model4.summary()

pred4 = model4.predict(pd.DataFrame(spa))
pred4_at = np.exp(pred4)
pred4_at

# Regression line
from sklearn.preprocessing import PolynomialFeatures
poly_reg = PolynomialFeatures(degree = 2)
X = spa.iloc[:, 1:2].values
X_poly = poly_reg.fit_transform(X)
# y = spa.iloc[:, 1].values


plt.scatter(spa.GPA, np.log(spa.SAT_Scores))
plt.plot(X, pred4, color = 'red')
plt.legend(['Predicted line', 'Observed data'])
plt.show()


# Error calculation
res4 = spa.SAT_Scores - pred4_at
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

train, test = train_test_split(spa, test_size = 0.3)

finalmodel = smf.ols('np.log(SAT_Scores) ~ GPA + I(GPA*GPA)',  data= train).fit()
finalmodel.summary()

# Predict on test dSalarya
test_pred = finalmodel.predict(pd.DataFrame(test))
pred_test_Salary = np.exp(test_pred)
pred_test_Salary

# Model Evaluation on Test data
test_res = test.SAT_Scores - pred_test_Salary
test_sqrs = test_res * test_res
test_mse = np.mean(test_sqrs)
test_rmse = np.sqrt(test_mse)
test_rmse


# Prediction on train data
train_pred = finalmodel.predict(pd.DataFrame(train))
pred_train_AT = np.exp(train_pred)
pred_train_AT

# Model Evaluation on train data
train_res = train.SAT_Scores - pred_train_AT
train_sqrs = train_res * train_res
train_mse = np.mean(train_sqrs)
train_rmse = np.sqrt(train_mse)
train_rmse





