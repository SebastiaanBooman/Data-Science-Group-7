import pandas as pd
from statsmodels.tsa.api import VAR
from sklearn.metrics import mean_squared_error
from dataclasses import dataclass
from collections.abc import MutableSequence

TRENDS = ['n', 'c', 'ct', 'ctt']

@dataclass
class var_params:
    lag: int
    trend: str
    score: float

    def __init__(self, lag:int, trend:str, score:float):
        self.lag = lag
        self.trend = trend
        self.score = score


def var_parameter_search(model: VAR, target_column:str, df_train:pd.DataFrame, df_diff:pd.DataFrame, df_test:pd.DataFrame, maxlag = 10, max_queue_length: int = 10) -> MutableSequence[var_params]:
    best_rmse = float('inf')
    best_parameters = []
    for lag in range(1,maxlag+1, 1):
        for trend in TRENDS:
            results = model.fit(
                maxlags=lag,
                method= 'ols',
                ic = None,
                trend=trend
            )

            new_rmse = __model_forecast(results= results,
                                        target_column=target_column,
                                        df_train=df_train,
                                        df_diff=df_diff,
                                        df_test=df_test,
                                        lag=lag)

            if new_rmse < best_rmse:
                print(f"better params found with lag: {lag}, and trend: {trend}")
                best_rmse = new_rmse
                best_parameters =  __append_parameters(best_params=best_parameters,
                                    params= var_params(lag, trend, best_rmse),
                                    max_queue_length=max_queue_length)
    
    #built-in reverse didn't work so this will have to do
    best_parameters = __reverse(best_parameters)
    return best_parameters

def __append_parameters(best_params: MutableSequence[var_params], params: var_params, max_queue_length = 10) -> MutableSequence[var_params]:
    #append at end of list
    best_params.append(params)
    
    #remove excess parameters from list
    while len(best_params) > max_queue_length:
        best_params.pop(0)
    
    return best_params

def __model_forecast(results, target_column:str, df_train:pd.DataFrame, df_diff:pd.DataFrame, df_test:pd.DataFrame, lag:int) -> float:
    horizon = len(df_test)
    alpha = 0.05

    # Do the forecast
    mid, lower, upper = results.forecast_interval(df_diff.values[-lag:], steps = horizon, alpha = alpha)

    # Put it into a dataframe
    df_forecast_diff = pd.DataFrame(mid, columns = df_diff.columns, index = df_test.iloc[:horizon].index)

    # Reverse the differencing
    df_forecast = df_forecast_diff.cumsum() + df_train[df_train.index < df_forecast_diff.index[0]].iloc[-1]
    
    rmse = mean_squared_error(df_forecast[target_column], df_test[target_column].values[:horizon], squared = False)

    return rmse

def __reverse(array):
    new_array = []
    for i in range(len(array)-1, -1, -1):
        new_array.append(array[i])

    return new_array