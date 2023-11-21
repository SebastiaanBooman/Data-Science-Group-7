"""Tests Ridge regression model using 
independent variable rgdpna and varying dependent variables
#TODO: Refactor
"""
import pandas as pd
import numpy as np
from sklearn.linear_model import Ridge
from sklearn.model_selection import train_test_split

#CONSTANTS
#TODO: Implement
PREDICTORS = ["hc", "pop"]
TARGET =  ["rgdpna"]

#FUNCTIONS
def load_penn_world_table():
    return(pd.read_excel("../../Data/pwt1001.xlsx", sheet_name="Data"))

def generate_train_test_data(pwt: pd.DataFrame, predictors: list[str] = PREDICTORS, target: str =TARGET) -> tuple:
    """TODO: If we want to use this function in future we should look into returning a specific dataclass type instead of tuple for clarity"""
    #Select all variables we are interested in (so that column lengths are equal after dropping NA)
    vars_subset = pwt[predictors + target].dropna()
    #print(vars_subset.head())
    train, test = train_test_split(vars_subset)

    X_train = train[predictors]
    y_train = train[target]

    X_test = test[predictors]
    y_test = test[target]

    return (X_train, y_train), (X_test, y_test)

def ridge_regression(X_train, y_train, X_test, y_test, a=1.0) -> None:
    print(":: Creating Ridge model")
    model = Ridge(alpha=a)
    model.fit(X_train, y_train)
    print(f":: Model coeff: {model.coef_}")
    print(f":: Model Y-intercept: {model.intercept_}\n")
    print(":: Predicting on test data")
    print(f":: Prediction R-squared: {model.score(X_test, y_test)}")

#TODO: lots of same code, necessary because we want to use avh_by_pop as a column (refactor if possible)
def dependent_hc_avh_by_pop(pwt: pd.DataFrame):
    vars_subset = pwt[["rgdpna", "pop", "emp", "avh", "hc"]].dropna()
    vars_subset["avh_by_pop"] = (vars_subset["avh"] * vars_subset["emp"]) / vars_subset["pop"]
    #print(vars_subset.head())
    train, test = train_test_split(vars_subset)

    X_train = train[["avh_by_pop", "hc"]]
    y_train = train["rgdpna"]

    X_test = test[["avh_by_pop", "hc"]]
    y_test = test["rgdpna"]

    ridge_regression(X_train, y_train, X_test, y_test)

if __name__ == "__main__":
    pwt = load_penn_world_table()
    #usa_data = pwt[pwt["countrycode"] == "USA"]
    #print(usa_data.head())

    #dependent_hc_avh_by_pop(pwt)

    #Test the Ridge regression using more than 2 independent variables
    #TODO: See what combination of variables would make the most sense
    test_data = generate_train_test_data(pwt, ["pop", "emp", "avh", "hc", "rnna"])
    ridge_regression(test_data[0][0], test_data[0][1], test_data[1][0], test_data[1][1])

    #vars_subset = pwt[["rgdpna", "pop", "emp", "avh", "hc", "labshm", "rtfpna"]].dropna()
    #vars_subset["avh_by_pop"] = (vars_subset["avh"] * vars_subset["emp"]) / vars_subset["pop"]


    #all_dependent(pwt)

    #pop    = "Population",
    #avh    = "Average_hours_worked",
    #cshm_i = "Capital_formation",
    #rkna   = "Capital_services",
    #labshm = "Labor_compensation",
    #hc     = "HCI",
    # rtfpna = "TFP"