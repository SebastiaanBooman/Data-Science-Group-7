# This makes sure Pandas keeps it mouth shut
import warnings
warnings.simplefilter(action = 'ignore', category = FutureWarning)

import pandas as pd
import numpy as np
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt 
from sklearn.model_selection import KFold
from dataclasses import dataclass
from enum import Enum

#TODO: Move parts to different files for clarity/readability
@dataclass
class MeanVARResults:
    mean_rmse: float
    mean_stationary_itas: float
    mean_fully_stationary: float
    mean_train_length: float
    mean_test_length: float

@dataclass
class FoldVARResults:
    rmse: float
    is_fully_stationary: bool
    stationary_itas: int
    train_length: int
    test_length: int

@dataclass
class MakeStationaryResult:
    df: pd.DataFrame
    is_fully_stationary: bool
    iterations: int

@dataclass 
class TrainTestData:
    train: pd.DataFrame
    test: pd.DataFrame


def stationarity_test(data: pd.DataFrame, verbose = False) -> bool:
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

def make_dataframe_stationary(df: pd.DataFrame) -> MakeStationaryResult:
    diff_pass = 1
    data_completely_stationary = False
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
        data_completely_stationary = True
        print('All series are stationary')
    return MakeStationaryResult(df, data_completely_stationary, diff_pass) 

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

def create_train_test_data(df: pd.DataFrame, folds: int)-> list[TrainTestData]: 
    """Takes data and splits it up in `folds` folds for training and testing sets"""
    kf = KFold(n_splits=folds, random_state=None, shuffle= False)
    train_test_list = [] 
    for (train_index, test_index) in kf.split(df):
        train_test_list.append(TrainTestData(
                    train = df.take(train_index),
                    test = df.take(test_index)))
    return train_test_list

def plot_country_results(df_train, df_test, train_length, test_length, df_forecast, gdp, rmse, country_name):  
    fig, ax = plt.subplots(1, 1)

    ax.plot(df_train[-train_length:][gdp], color='black', label='Train')
    ax.plot(df_test[:test_length][gdp], color='tab:blue', label = 'Test')
    ax.plot(df_forecast[gdp], color = 'tab:orange', label = 'Forecast')
    #ax.fill_between(df_forecast[gdp].index, df_lower_error[gdp].values, df_upper_error[gdp].values, color = 'bisque', label='95% CI')

    ax.xaxis.set_ticks_position('none')
    ax.yaxis.set_ticks_position('none')
    ax.spines['top'].set_alpha(0)
    ax.tick_params(labelsize = 6)
    ax.set_title(f'{country_name} GDP')
    ax.set_title(f'RMSE: {rmse}', loc = 'left', x = 0.04, y = 0.9, fontsize = 'small')
    ax.legend(loc = 'lower right')
    ax.spines[['right', 'top']].set_visible(False)

    plt.show()
    #plt.clf()


def get_fold_var_res(data: TrainTestData, maxlag: int, countrycode: str, dependent_name: str) -> FoldVARResults:
    """Using a train test split `data` creates a VAR model and calculates rmse"""
    #TODO: What to do with folds which train set contain less than 20 values?
    df_train_stationary_itas = 0
    df_train_fully_stationary = False
    if len(data.train) > 19:
        make_df_train_stationary_res = make_dataframe_stationary(data.train)
        data.train = make_df_train_stationary_res.df
        df_train_stationary_itas = make_df_train_stationary_res.iterations
        df_train_fully_stationary = make_df_train_stationary_res.is_fully_stationary

    var_model = create_VAR_for_model(data.train, maxlag)
    data_test_len = len(data.test)
    # Do the forecast
    mid = var_model.forecast(data.train.values[-maxlag:], steps = data_test_len)
    # Put it into a dataframe
    df_forecast_diff = pd.DataFrame(mid, columns = data.train.columns, index = data.test.iloc[:data_test_len].index)
    # Reverse the differencing
    #TODO: Are we allowed to just take [-1] as value (commented part is original)
    df_forecast = df_forecast_diff.cumsum() + data.train.iloc[-1] # data.train[data.train.index < df_forecast_diff.index[0]].iloc[-1]
    rmse = mean_squared_error(df_forecast[dependent_name], data.test[dependent_name].values[:data_test_len], squared = False)

    plot_country_results(data.train, data.test, len(data.train), data_test_len, df_forecast, dependent_name, rmse, countrycode)
    return FoldVARResults(rmse, df_train_fully_stationary, df_train_stationary_itas, len(data.train), data_test_len)

def get_var_res_by_country(ctry_df: pd.DataFrame, maxlag: int, countrycode: str, dependent_name: str, folds: int = 4) -> MeanVARResults:
    """TODO: Docstring Using KFold cross validation"""

    train_test_list = create_train_test_data(ctry_df, folds)

    fold_var_res_list = [get_fold_var_res(data, maxlag, countrycode, dependent_name) for data in train_test_list]

    fold_rmse_list = [fr.rmse for fr in fold_var_res_list]
    fold_fully_stationary_list = [fr.is_fully_stationary for fr in fold_var_res_list]
    fold_stationary_itas_list= [fr.stationary_itas for fr in fold_var_res_list]
    fold_train_length_list= [fr.train_length for fr in fold_var_res_list]
    fold_test_length_list= [fr.test_length for fr in fold_var_res_list]

    mean_rmse_folds = np.mean(fold_rmse_list)
    mean_fully_stationary_folds = np.mean(fold_fully_stationary_list)
    mean_stationary_itas_folds = np.mean(fold_stationary_itas_list)
    mean_train_length_folds = np.mean(fold_train_length_list)
    mean_test_length_folds = np.mean(fold_test_length_list)

    return MeanVARResults(mean_rmse_folds, mean_stationary_itas_folds, mean_fully_stationary_folds, mean_train_length_folds, mean_test_length_folds)

def extract_country_and_generate_var_res(countrycode: str, df: pd.DataFrame, maxlag:int, dependent_name: str) -> MeanVARResults:
        """Given a countrycode, dataframe containing multiple country data and var parameters creates MeanVARResults"""
        ctry_df = df.loc[df['countrycode'] == countrycode]
        ctry_df = ctry_df.drop(columns=["countrycode"])
        return get_var_res_by_country(ctry_df, maxlag, countrycode, dependent_name)

def get_var_res_by_dev_status(unique_countrycodes: list[str], df: pd.DataFrame, maxlag: int, dependent_name: str):
    alpha = 0.05

    countrys_res = [extract_country_and_generate_var_res(code, df, maxlag, dependent_name) for code in unique_countrycodes]
    
    rmse_list = [res.mean_rmse for res in countrys_res]
    fully_stationary_list = [res.mean_fully_stationary for res in countrys_res]
    stationary_itas_list = [res.mean_stationary_itas for res in countrys_res]
    train_length_list = [res.mean_train_length for res in countrys_res]
    test_length_list = [res.mean_test_length for res in countrys_res]

    mean_rmse = np.mean(rmse_list)
    mean_fully_stationary = np.mean(fully_stationary_list)
    mean_stationary_itas = np.mean(stationary_itas_list)
    mean_train_length = np.mean(train_length_list)
    mean_test_length = np.mean(test_length_list)

    return MeanVARResults(mean_rmse, mean_stationary_itas, mean_fully_stationary, mean_train_length, mean_test_length)

def plot_simple_bar(x_data: list, y_data: list, x_label: str, y_label: str, title: str):
    fig = plt.figure(figsize = (10, 5))

    plt.bar(x_data, y_data, color ='maroon', 
            width = 0.4)
    
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title(title)
    plt.show()

def plot_dev_status_var_results(df):
    #TODO: Want to plot country amount per dev status
    dev_status = df["Development status"].tolist()
    rmse = df["Mean RMSE"].tolist()
    country_amt = df["Country amount"].tolist()
    statonary_itas = df["Mean stationary itas"].tolist()
    fully_stationary = df["Mean fully stationary"].tolist()
    train_length = df["Mean train length"].tolist()
    test_length = df["Mean test length"].tolist()
    
    #TODO: Fix, kind of ugly
    #Remove least developed because it is an extreme outlier
    #dev_status.pop(1)
    #rmse.pop(1)

    plot_simple_bar(dev_status, rmse, "Development status", "Mean VAR RMSE", "Mean VAR RMSE by Development status")
    plot_simple_bar(dev_status, country_amt, "Development status", "Country amount", "Country amount by Development status")
    plot_simple_bar(dev_status, statonary_itas, "Development status", "Mean training differencing itas", "Mean differencing itas by Development status")
    plot_simple_bar(dev_status, fully_stationary, "Development status", "% of training data stationary", "% of stationary training data by Development status")
    plot_simple_bar(dev_status, train_length, "Development status", "Mean training length", "Mean training length by Development status")
    plot_simple_bar(dev_status, test_length, "Development status", "Mean testing length", "Mean testing length by Development status")


def VAR_pipeline(df_list: list[pd.DataFrame], dependent_name: str):
    #for country data in list make var model, after which take the mean rmse as a result for VAR model for that dev status
    maxlag = 8 # FIXME: what is the right max lag???
    #TODO: want to use save more columns for each test
    res_df: pd.DataFrame = pd.DataFrame(columns = ['Development status', "Country amount", 'Mean RMSE', "Mean stationary itas", "Mean fully stationary", "Mean train length", "Mean test length"]) 
    for df in df_list:
        dev_status = df["economy"][0]
        df = df.drop(columns=["economy"])
        unique_countrycodes = df["countrycode"].unique()
        country_amt = len(unique_countrycodes)
        
        dev_stat_var_res = get_var_res_by_dev_status(unique_countrycodes, df, maxlag, dependent_name)

        res_df = pd.concat([res_df, pd.DataFrame([
            {
                "Development status" : dev_status, 
                "Country amount": country_amt, 
                "Mean RMSE": dev_stat_var_res.mean_rmse,
                "Mean stationary itas" : dev_stat_var_res.mean_stationary_itas,
                "Mean fully stationary": dev_stat_var_res.mean_fully_stationary,
                "Mean train length" : dev_stat_var_res.mean_train_length,
                "Mean test length" : dev_stat_var_res.mean_test_length
                }])], ignore_index=True)

    plot_dev_status_var_results(res_df)
    res_df.to_csv("./VAR dev status results.csv", index=False)

def create_pwt_dev_status_subset(dev_statuses: list[str], dev_status_df: pd.DataFrame, pwt: pd.DataFrame) -> list[pd.DataFrame]:
    """For each dev status create a subset dataframe using pwt, return all the dataframes in a list"""
    dev_stat_df_list = []
    joined_df = pwt.join(dev_status_df.set_index("countrycode"), on=["countrycode"], how="inner")

    for dev_stat in dev_statuses:
        dev_stat_df_list.append(joined_df.loc[joined_df["economy"] == dev_stat])
    return dev_stat_df_list

class DevStatusLevel(Enum):
    ALL = 1
    ONLY_DEV_UNDEVELOPED = 2
    MERGED_SUBSET = 3
    ONLY_DEVELOPED = 4

def remove_suffix_dev_status(dev_status: str) -> str:
    split_dev_status = dev_status.split()
    if split_dev_status[-2].endswith(":"):
        dev_status = dev_status.rsplit(" ", 1)[0].rstrip(":")
        print()
    return dev_status

def filter_only_dev_and_undeveloped_status(dev_status: str) -> str:
    cleaned_status = remove_suffix_dev_status(dev_status)
    if not cleaned_status.startswith("Developed region"):
        cleaned_status = "Undeveloped region"
    return cleaned_status 

def only_developed_status(dev_status: str) -> str:
    cleaned_status = remove_suffix_dev_status(dev_status)
    if not cleaned_status.startswith("Developed region"):
        cleaned_status = "Undeveloped region"
    return cleaned_status 

def init_data(dev_sat_level: DevStatusLevel):
    print("Importing Penn World Table...")
    pwt = pd.read_excel('./Data/pwt1001.xlsx',
                    sheet_name = 'Data',
                    parse_dates = ['year'],
                    index_col = 3)
    gdp_type = 'rgdpna' #'cgdpo'
    dependent_var = "gdp_growth"
    pwt[f'{gdp_type}_lag'] = pwt.groupby(['countrycode'])[gdp_type].shift(1)
    pwt[dependent_var] = pwt.apply(lambda row : (row[gdp_type] - row[f'{gdp_type}_lag']) / row[f'{gdp_type}_lag'], axis = 1)
    pwt = pwt[[dependent_var, 'ccon', 'rdana', "countrycode"]]
    pwt = pwt.dropna()

    country_dev_status_df = pd.read_csv('./Data/dev_status.csv')

    match dev_sat_level:
        case DevStatusLevel.ALL: 
            pass #Default data         
        case DevStatusLevel.ONLY_DEV_UNDEVELOPED:
            country_dev_status_df["economy"] = country_dev_status_df["economy"].apply(filter_only_dev_and_undeveloped_status)
        case DevStatusLevel.MERGED_SUBSET:
            country_dev_status_df["economy"] = country_dev_status_df["economy"].apply(remove_suffix_dev_status)
        case DevStatusLevel.ONLY_DEVELOPED:
            country_dev_status_df = country_dev_status_df[country_dev_status_df["economy"].apply(lambda x: x.startswith("Developed region"))]
        case _:
            raise NotImplementedError
        
    unique_eco_statuses = country_dev_status_df["economy"].unique()

    return create_pwt_dev_status_subset(unique_eco_statuses, country_dev_status_df, pwt)

if __name__ == "__main__":
    pwt_by_dev_status_df_list = init_data(DevStatusLevel.ONLY_DEVELOPED) 
    VAR_pipeline(pwt_by_dev_status_df_list, "gdp_growth")
