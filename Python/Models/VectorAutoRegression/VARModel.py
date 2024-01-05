import pandas as pd
from statsmodels.tsa.api import VAR
from sklearn.metrics import mean_squared_error
from dataclasses import dataclass
from VARDataClasses import VARHyperParams

@dataclass
class ForecastResult:
    rmse: float
    pred: pd.DataFrame

class VARModel:
    def create_var_model(train_df):
        return VAR(train_df, freq = train_df.index.inferred_freq)

    def fit_var_model(model: VAR, var_params: VARHyperParams):
        return model.fit(
            maxlags = var_params.lag,
            method  = 'ols',
            ic      = None,
            trend   = var_params.trend
        )

    def forecast_using_var_model(var_model, undiff_train_data: pd.DataFrame, 
                                 undiff_test_data: pd.DataFrame, diff_train_df: pd.DataFrame, maxlag: int, dependent_name) -> ForecastResult:
                                    
        data_test_len = len(undiff_test_data)
        forecast = var_model.forecast(diff_train_df.values[-maxlag:], steps = data_test_len)
        df_forecast_diff = pd.DataFrame(forecast, columns = undiff_train_data.columns, index = undiff_test_data.iloc[:data_test_len].index)
        # Reverse the differencing
        #TODO: Are we allowed to just take [-1] as value (commented part is original)
        #TODO: In some folds the test/prediction indexes are in front of the last train data year. Does undifferencing still work like this then?
        df_forecast = df_forecast_diff.cumsum() + undiff_train_data.iloc[-1] # data.train[data.train.index < df_forecast_diff.index[0]].iloc[-1]
        return ForecastResult(mean_squared_error(df_forecast[dependent_name], undiff_test_data[dependent_name].values[:data_test_len], squared = False),
                              df_forecast)