# -*- coding: utf-8 -*-
"""
"""
import pandas as pd
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.linear_model import LinearRegression, Ridge
from sklearn.ensemble import RandomForestRegressor
from sklearn.svm import  LinearSVR
from sklearn.preprocessing import MinMaxScaler
from sklearn.pipeline import Pipeline
import matplotlib.pyplot as plt
import numpy as np


train = pd.read_csv("c:/itwill/4_python-ii/data/fifa_train.csv")
train.info()
train.shape # (8932, 12)
train.isnull().sum() # 결측치 확인
'''
 0   id                8932 non-null   int64  
 1   name              8932 non-null   object 
 2   age               8932 non-null   int64  
 3   continent         8932 non-null   object 
 4   contract_until    8932 non-null   object 
 5   position          8932 non-null   object 
 6   prefer_foot       8932 non-null   object 
 7   reputation        8932 non-null   float64
 8   stat_overall      8932 non-null   int64  
 9   stat_potential    8932 non-null   int64  
 10  stat_skill_moves  8932 non-null   float64
 11  value             8932 non-null   float64
'''
train.head()

# x, y split
idx = list(train.columns)
train.id.dtype == "int64"
x_idx = [i for i in idx[1:11] if train[i].dtype == "int64" or train[i].dtype == "float64"]
X = train[x_idx]
y = train["value"]

# train, test split
train_x, test_x, train_y, test_y = train_test_split(X, y, test_size = 0.3)

# 기본 pipeline 생성
pipe = Pipeline([("scaler", MinMaxScaler()), 
                 ("regressor", RandomForestRegressor())])

# param_grid
param_grid = [
    {"regressor":[LinearRegression()], "scaler":[MinMaxScaler()]},
    {"regressor":[RandomForestRegressor(max_features = 5)], "scaler":[None],
     "regressor__n_estimators":[100,200],
     "regressor__max_depth":[3, 6, 8, 10],
     "regressor__min_samples_split":[2, 3, 4, 5],
     "regressor__min_samples_leaf":[1,3,5,7], "regressor__max_features":[5]}]


# Grid Search
gs = GridSearchCV(pipe, param_grid, cv = 5, n_jobs=-1)
model = gs.fit(train_x, train_y)
model.best_score_ # 0.9833333333333332
model.best_params_
model.predict(test_x)
model.feature_importance_
'''
{'max_depth': 6,
 'min_samples_leaf': 1,
 'min_samples_split': 3,
 'n_estimators': 400}
'''

# mse, score

df = pd.DataFrame({"y_true":test_y, "y_pred":y_pred})
df.corr() # 0.79

plt.style.use("ggplot")
plt.plot(np.array(test_y)[:100], c= "r", ls = "-", marker = "", label = "real_value")
plt.plot(y_pred[:100], c= "b", ls = "-", marker = "", label = "predicted value")
plt.legend(loc = "best")






test = pd.read_csv("c:/itwill/4_python-ii/data/fifa_test.csv")
test.info()
test.shape # (3828, 11)
test.isnull().sum() # 결측치 확인
'''
 0   id                3828 non-null   int64  
 1   name              3828 non-null   object 
 2   age               3828 non-null   int64  
 3   continent         3828 non-null   object 
 4   contract_until    3828 non-null   object 
 5   position          3828 non-null   object 
 6   prefer_foot       3828 non-null   object 
 7   reputation        3828 non-null   float64
 8   stat_overall      3828 non-null   int64  
 9   stat_potential    3828 non-null   int64  
 10  stat_skill_moves  3828 non-null   float64
'''
X = test[x_idx]
y_pred = model.predict(X)
y_pred

submit = pd.read_csv("c:/itwill/4_python-ii/data/submission.csv")
submit.info()
submit["value"] = y_pred
submit.head()
submit.to_csv("c:/itwill/4_python-ii/data/submission.csv", index = None, encoding = "utf-8")
'''
 0   id      3828 non-null   int64
 1   value   3828 non-null   int64
'''