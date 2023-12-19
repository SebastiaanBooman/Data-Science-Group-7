# This makes sure Pandas keeps it mouth shut
import warnings
warnings.simplefilter(action = 'ignore', category = FutureWarning)
import pandas as pd
import numpy as np
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt 

def stationarity_test(data: pd.DataFrame, verbose = False) -> bool:
    maxlag = round(12 * pow(len(data) / 100, 1/4))
    alpha = 0.05
    results = pd.DataFrame()
    stationary = True

    for name, series in data.items():
        result = adfuller(
            series,
            maxlag     = maxlag,
            regression = 'c', # Constant regression
            autolag    = 'AIC', 
            store      = False,
            regresults = False
        )
        results[name] = {
            'P-Value':     round(result[1], 3),
            'Number lags': result[2],
            'Stationary':  'Yes' if stationary else 'No'
        }

        if result[1] > alpha:
            stationary = False
        
    if verbose:
        print(results.transpose())

    return stationary

def make_dataframe_stationary(df: pd.DataFrame):
    diff_pass = 1

    while not stationarity_test(df, True):
        print(f'One or more series are non-stationary, differencing... (pass = {diff_pass})')
        df = df.diff().dropna()
        diff_pass += 1

        #TODO: How should we go about this?
        # Dataframe smaller than 20 does not work for adfuller test 
        if diff_pass > 10 or len(df) < 20:
            print('Unable to make all series stationary')
            break
    else:
        print('All series are stationary')
    
    return df

def create_VAR_for_model(train_df, maxlag):
    model = VAR(train_df, freq = train_df.index.inferred_freq)
    #maxlag = model.select_order().selected_orders['bic']
    #print(model.select_order().selected_orders)
    #print(f'Max. lag = {maxlag}')

    return model.fit(
        maxlags = maxlag,
        method  = 'ols',
        ic      = None,
        trend   = 'c' # Add constant test change to n
    )

def get_mean_var_rmse_by_dev_status(unique_countrycodes: list[str], df: pd.DataFrame, maxlag: int) -> list[float]:
    rmse_list = []
    alpha = 0.05
    for countrycode in unique_countrycodes:
        ctry_df = df.loc[df['countrycode'] == countrycode]
        ctry_df = ctry_df.drop(columns=["countrycode"])
        train_length = int(len(ctry_df) * 0.75)
        df_train = ctry_df[:train_length]
        if len(ctry_df) > 19:
            df_train = make_dataframe_stationary(df_train)

        var_model = create_VAR_for_model(df_train, maxlag)

        df_test = ctry_df[train_length:]
        df_test_len= len(df_test)

        # Do the forecast
        mid, lower, upper = var_model.forecast_interval(df_train.values[-maxlag:], steps = df_test_len, alpha = alpha)

        # Put it into a dataframe
        df_forecast_diff = pd.DataFrame(mid, columns = df_train.columns, index = df_test.iloc[:df_test_len].index)

        # Reverse the differencing
        df_forecast = df_forecast_diff.cumsum() + df_train[df_train.index < df_forecast_diff.index[0]].iloc[-1]

        unique_ctry_rmse = mean_squared_error(df_forecast['cgdpo'], df_test['cgdpo'].values[:df_test_len], squared = False)

        rmse_list.append(unique_ctry_rmse)
    mean_rmse = np.mean(rmse_list)
    return mean_rmse

def plot_dev_status_var_results(df):
    #TODO: Want to plot country amount per dev status
    dev_status = df["Development status"].tolist()
    rmse = df["Mean RMSE"].tolist()
    #TODO: Fix, kind of ugly
    #Remove least developed because it is an extreme outlier
    dev_status.pop(1)
    rmse.pop(1)
    
    fig = plt.figure(figsize = (10, 5))

    plt.bar(dev_status, rmse, color ='maroon', 
            width = 0.4)
    
    plt.xlabel("Development status")
    plt.ylabel("Mean VAR RMSE")
    plt.title("Mean VAR RMSE by Development status")
    plt.show()

def VAR_pipeline(df_list: list[pd.DataFrame]):
    #for country data in list make var model, after which take the mean rmse as a result for VAR model for that dev status
    maxlag = 8 # FIXME: what is the right max lag???
    res_df: pd.DataFrame = pd.DataFrame(columns = ['Development status', "Country amount", 'Mean RMSE'])     #TODO: want to use save more columns for each test
    for df in df_list:
        dev_status = df["economy"][0]
        df = df.drop(columns=["economy"])
        unique_countrycodes = df["countrycode"].unique()
        country_amt = len(unique_countrycodes)
        
        #TODO: Implement Different measurements
        mean_rmse = get_mean_var_rmse_by_dev_status(unique_countrycodes, df, maxlag)

        res_df = pd.concat([res_df, pd.DataFrame([{"Development status" : dev_status, "Country amount": country_amt, "Mean RMSE": mean_rmse }])], ignore_index=True)

    plot_dev_status_var_results(res_df)
    res_df.to_csv("./VAR dev status results.csv", index=False)

def create_pwt_dev_status_subset(dev_statuses: list[str], dev_status_df: pd.DataFrame, pwt: pd.DataFrame) -> list[pd.DataFrame]:
    """For each dev status create a subset dataframe using pwt, return all the dataframes in a list"""
    dev_stat_df_list = []
    joined_df = pwt.join(dev_status_df.set_index("countrycode"), on=["countrycode"], how="inner")

    for dev_stat in dev_statuses:
        dev_stat_df_list.append(joined_df.loc[joined_df["economy"] == dev_stat])
    return dev_stat_df_list

def init_data():
    pwt = pd.read_excel('../../Data/pwt1001.xlsx',
                    sheet_name = 'Data',
                    parse_dates = ['year'],
                    index_col = 3)
    gdp = 'cgdpo'
    pwt = pwt[[gdp, 'ccon', 'rdana', "countrycode"]]
    pwt = pwt.dropna()

    country_dev_status_df = pd.read_csv('../../Data/dev_status.csv')
    unique_eco_statuses = country_dev_status_df["economy"].unique()

    pwt_by_dev_status_df_list = create_pwt_dev_status_subset(unique_eco_statuses, country_dev_status_df, pwt)

    return pwt_by_dev_status_df_list

if __name__ == "__main__":
    pwt_by_dev_status_df_list = init_data()
    VAR_pipeline(pwt_by_dev_status_df_list)