import pandas as pd
from dataclasses import dataclass
from statsmodels.tsa.stattools import adfuller

@dataclass
class MakeStationaryResult:
    df: pd.DataFrame
    fully_stationary: bool
    itas: int

class Stationary:
    def make_dataframe_stationary(df: pd.DataFrame) -> MakeStationaryResult:
        """
        Attempts to make all columns from dataframe stationary by executing one or two differencing iterations. Uses the ad fuller test to verify wether the dataframe is stationary.
        Each iteration will shrink the dataframe by one due to the differencing. 
    
        Parameters
        ----------
        df : pandas.DataFrame
            the dataframe to difference.
    
        Returns
        -------
        MakeStationaryResult 
            The result of the attempt to make the dataframe stationary attempt
        """

        if len(df) <= 19:
            return MakeStationaryResult(df, False, 0)

        diff_pass = 1
        data_completely_stationary = False
        while not Stationary.__are_all_col_stationary(df, True):
            print(f'One or more series are non-stationary, differencing... (pass = {diff_pass})')
            df = df.diff().dropna()
            diff_pass += 1

            # Dataframe smaller than 20 does not work for adfuller test 
            if diff_pass > 2 or len(df) < 20:
                print('Unable to make all series stationary')
                break
        else:
            data_completely_stationary = True
            print('All series are stationary')
        return MakeStationaryResult(df, data_completely_stationary, diff_pass) 

    def __are_all_col_stationary(data: pd.DataFrame, verbose = False) -> bool:
        maxlag = round(12 * pow(len(data) / 100, 1/4))
        alpha = 0.05
        results = pd.DataFrame()
        all_cols_are_stationary = True

        for name, series in data.items():
            result = adfuller(
                series,
                maxlag     = maxlag,
                regression = 'c', # Constant regression
                autolag    = 'AIC', 
                store      = False,
                regresults = False
            )
            col_val_is_stationary = result[1] <= alpha

            if not col_val_is_stationary:
                all_cols_are_stationary = False

            results[name] = {
                'P-Value':     round(result[1], 3),
                'Number lags': result[2],
                'Stationary':  'Yes' if col_val_is_stationary else 'No'
            }

        if verbose:
            print(results.transpose())

        return all_cols_are_stationary