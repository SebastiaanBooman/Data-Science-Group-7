"""Tests Ridge regression model using 
independent variable rgdpna and varying dependent variables
#TODO: Refactor
"""
import pandas as pd
import numpy as np
from sklearn.linear_model import Ridge
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

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
    train, test = train_test_split(vars_subset, test_size=.2)

    X_train = train[predictors]
    y_train = train[target]

    X_test = test[predictors]
    y_test = test[target]

    return (X_train, y_train), (X_test, y_test)

def ridge_regression(X_train, y_train, X_test, y_test, a) -> float:
    """returns the R2 based on the predictions on test data"""
    print(":: Creating Ridge model")
    model = Ridge(alpha=a)
    model.fit(X_train, y_train)
    print(f":: Model coeff: {model.coef_}")
    return model.coef_
    print(f":: Model Y-intercept: {model.intercept_}\n")
    print(":: Predicting on test data")
    print(f":: Prediction R-squared: {model.score(X_test, y_test)}")
    #return model.score(X_test, y_test)

#TODO: lots of same code, necessary because we want to use avh_by_pop as a column (refactor if possible)
def dependent_hc_avh_by_pop(pwt: pd.DataFrame, alpha: int) -> float:
    vars_subset = pwt[["rgdpna", "pop", "emp", "avh", "hc"]].dropna()
    vars_subset["avh_by_pop"] = (vars_subset["avh"] * vars_subset["emp"]) / vars_subset["pop"]
    #print(vars_subset.head())
    train, test = train_test_split(vars_subset, test_size=.2)

    X_train = train[["avh_by_pop", "hc"]]
    y_train = train["rgdpna"]

    X_test = test[["avh_by_pop", "hc"]]
    y_test = test["rgdpna"]

    return ridge_regression(X_train, y_train, X_test, y_test)


def plot_ridge(alphas, coefs):
    ax = plt.gca()
    ax.plot(alphas, coefs)
    ax.set_xscale("log")
    ax.set_xlim(ax.get_xlim()[::-1])  # reverse axis
    plt.xlabel("alpha")
    plt.ylabel("weights")
    plt.title("Ridge coefficients as a function of the regularization")
    plt.axis("tight")
    plt.show()


if __name__ == "__main__":
    pwt = load_penn_world_table()
    #usa_data = pwt[pwt["countrycode"] == "USA"]
    #print(usa_data.head())

    #dependent_hc_avh_by_pop(pwt)
    alpha_range =  np.arange(0,1000,10)
    #alpha_range = np.logspace(-10, -2, 200)
    #alpha_range =  np.arange(0,1,0.01)
    #dependent_res = []
    #for alpha in alpha_range:
    #    dependent_res.append(dependent_hc_avh_by_pop(pwt, alpha))
    #Test the Ridge regression using more than 2 independent variables
    #TODO: See what combination of variables would make the most sense
    comb_res = []
    comb_data = generate_train_test_data(pwt, ["pop", "emp", "avh", "rnna"]) #"hc", "rnna"])
    #comb_data = generate_train_test_data(pwt, ["ccon", "cda"])#, "cgdpo", "cn"])
    #ridge_regression(comb_data[0][0], comb_data[0][1], comb_data[1][0], comb_data[1][1])
    for alpha in alpha_range:
        print(alpha)
        comb_res.extend(ridge_regression(comb_data[0][0], comb_data[0][1], comb_data[1][0], comb_data[1][1], alpha))


    plot_ridge(alpha_range, comb_res)
    print("")

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