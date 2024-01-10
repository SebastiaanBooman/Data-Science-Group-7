import pandas as pd
from statsmodels.tsa.api import VAR
from VARDataClasses import VARPredictionResult, VARHyperParams
from VARModel import VARModel

class VARParameterSelection:
    def var_parameter_search(
            model: VAR, 
            target_column:str, 
            df_train:pd.DataFrame, 
            df_diff:pd.DataFrame, 
            df_test:pd.DataFrame, 
            stationary_itas: int,
            maxlag = 10, 
            max_queue_length: int = 10,
        ) -> list[tuple[VARPredictionResult, pd.DataFrame]]:
        #TODO: Proper docstring
        #TODO: Want to change the return tuple to a dataclass also? Or this creates too much indirection?
        """
        Uses data to forecast on VAR models created with different hyperparameters. Returns a list with <= `maxlag` amount of results.
        First element of tuple is hyperparams and rmse, second is the undifferenced forecast corresponding to the parameters
        """

        best_rmse = float('inf')
        best_parameters = []

        for lag in range(1,maxlag+1, 1):
            for trend in ['n', 'c', 'ct', 'ctt']:
                model_res = model.fit(
                    maxlags=lag,
                    method= 'ols',
                    ic = None,
                    trend=trend
                )

                forecast_res = VARModel.forecast_using_var_model(model_res, df_train, df_test, df_diff, maxlag, stationary_itas, target_column)
                if forecast_res.rmse < best_rmse:
                    print(f"better params found with lag: {lag}, and trend: {trend}")
                    best_rmse = forecast_res.rmse
                    best_parameters =  VARParameterSelection.__append_parameters(best_params=best_parameters,
                                        #TODO: Might be slow to create this many objects
                                        params= [VARPredictionResult(best_rmse, VARHyperParams(trend, lag)), forecast_res.pred],
                                        max_queue_length=max_queue_length)
        
        #TODO built-in reverse didn't work so this will have to do 
        #best_params_copy = best_parameters.copy()
        best_parameters = VARParameterSelection.__reverse(best_parameters)
        #best_params_test = best_params_copy.reverse()
        return best_parameters

    def __append_parameters(best_params: list[tuple[VARPredictionResult, pd.DataFrame]], params: tuple[VARPredictionResult, pd.DataFrame], max_queue_length: int) -> list[VARPredictionResult]:
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
