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
    @staticmethod
    def create_var_model(train_df):
        return VAR(train_df, freq = train_df.index.inferred_freq)

    @staticmethod
    def fit_var_model(model: VAR, var_params: VARHyperParams):
        return model.fit(
            maxlags = var_params.lag,
            method  = 'ols',
            ic      = None,
            trend   = var_params.trend
        )

    @staticmethod
    def diff_inv(forecast_diff, original, passes):
        df_temp = forecast_diff.copy()

        for i in range(passes, 0, -1):
            df_orig = original.iloc[-1]

            for j in range(2, i + 1):
                df_orig = df_orig - original.iloc[-j]

            df_temp = df_orig + df_temp.cumsum()

        return df_temp

    @staticmethod
    def forecast_using_var_model(var_model,
                                 undiff_train_data: pd.DataFrame,
                                 undiff_test_data: pd.DataFrame,
                                 diff_train_df: pd.DataFrame,
                                 maxlag: int,
                                 stationary_itas: int,
                                 dependent_name) -> ForecastResult:

        data_test_len = len(undiff_test_data)
        forecast = var_model.forecast(diff_train_df.values[-maxlag:], steps = data_test_len)
        df_forecast_diff = pd.DataFrame(forecast, columns = undiff_train_data.columns, index = undiff_test_data.iloc[:data_test_len].index)
        # Reverse the differencing

        df_forecast = VARModel.diff_inv(df_forecast_diff, undiff_train_data, stationary_itas)
        return ForecastResult(mean_squared_error(df_forecast[dependent_name], undiff_test_data[dependent_name].values[:data_test_len], squared = False),
                              df_forecast)
