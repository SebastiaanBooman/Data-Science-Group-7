import pandas as pd
import numpy as np
from sklearn.linear_model import Lasso
#import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

#CONSTANTS
#TODO: Implement
TARGET =  ["rgdpna"]

#FUNCTIONS
def load_penn_world_table():
    return(pd.read_excel("../../Data/pwt1001.xlsx", sheet_name="Data"))

def generate_train_test_data(pwt: pd.DataFrame, predictors: list[str], target: str =TARGET) -> tuple:
    """TODO: If we want to use this function in future we should look into returning a specific dataclass type instead of tuple for clarity"""
    #Select all variables we are interested in (so that column lengths are equal after dropping NA)
    vars_subset = pwt[predictors + target].dropna()

    #print(vars_subset.head())
    train, test = train_test_split(vars_subset, test_size=.2)
    X_train = train[predictors]

    scaler = StandardScaler().fit(X_train)
    X_train = scaler.transform(X_train)
    X_test = scaler.transform(test[predictors])

    y_train = train[target]
    y_test = test[target]

    return (X_train, y_train), (X_test, y_test)

def lasso_regression(X_train, y_train, X_test, y_test, a=1.3) -> float:
    """returns the R2 based on the predictions on test data"""
    print(":: Creating Ridge model")
    model = Lasso(alpha=a)
    model.fit(X_train, y_train)
    print('R squared training set', round(model.score(X_train, y_train)*100, 2))
    print('R squared test set', round(model.score(X_test, y_test)*100, 2))

    # RMSE calculations
    # Training data
    pred_train = model.predict(X_train)
    mse_train = rmse(pred_train, y_train.to_numpy())
    print('RMSE training set', mse_train)

    # Test data
    pred_test = model.predict(X_test)
    mse_test = rmse(pred_test, y_test.to_numpy())
    print('RMSE test set', mse_test)

def rmse(predictions, real):
    residuals =  predictions - real
    rmse = np.sqrt(np.sum(residuals**2))**.5
    range = residuals.max() - residuals.min()
    rmse_std = rmse / range
    return  rmse_std

if __name__ == "__main__":
    pwt = load_penn_world_table()
    comb_data = generate_train_test_data(pwt, ["pop", "cn", "ccon", "rdana"]) #"hc", "rnna"])
    lasso_regression(comb_data[0][0], comb_data[0][1], comb_data[1][0], comb_data[1][1])