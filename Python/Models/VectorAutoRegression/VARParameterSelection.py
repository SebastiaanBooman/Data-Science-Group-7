import pandas as pd
from statsmodels.tsa.api import VAR
from VARDataClasses import VARHyperParams
from VARModel import VARModel

class VARParameterSelection:
    def var_parameter_search(model: VAR, target_column:str, df_train:pd.DataFrame, df_diff:pd.DataFrame, df_test:pd.DataFrame, maxlag = 10, max_queue_length: int = 10) -> list[VARHyperParams]:
        best_rmse = float('inf')
        best_parameters = []
        trends = ['n', 'c', 'ct', 'ctt']
        for lag in range(1,maxlag+1, 1):
            for trend in trends:
                model_res = model.fit(
                    maxlags=lag,
                    method= 'ols',
                    ic = None,
                    trend=trend
                )

                new_rmse = VARModel.forecast_using_var_model(model_res, df_train, df_test, df_diff, maxlag, target_column).rmse
                if new_rmse < best_rmse:
                    print(f"better params found with lag: {lag}, and trend: {trend}")
                    best_rmse = new_rmse
                    best_parameters =  VARParameterSelection.__append_parameters(best_params=best_parameters,
                                        params= VARHyperParams(lag, trend, best_rmse),
                                        max_queue_length=max_queue_length)
        
        #built-in reverse didn't work so this will have to do 
        best_parameters = VARParameterSelection.__reverse(best_parameters)
        return best_parameters

    def __append_parameters(best_params: list[VARHyperParams], params: VARHyperParams, max_queue_length = 10) -> list[VARHyperParams]:
        best_params.append(params)
        
        #remove excess parameters from list
        while len(best_params) > max_queue_length:
            best_params.pop(0)
        
        return best_params

    def __reverse(array):
        new_array = []
        for i in range(len(array)-1, -1, -1):
            new_array.append(array[i])

        return new_array