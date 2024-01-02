# This makes sure Pandas keeps it mouth shut
import warnings
warnings.simplefilter(action = 'ignore', category = FutureWarning)
import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from VARDataClasses import FoldVARResults, MeanVARResults, TrainTestData 
from PWTDevStatus import PWTDevStatusGenerator, DevStatusLevel
from VARModel import VARModel
from StationaryFunctions import Stationary
from VARExportResults import ExportVARResults

class VARByDevStatusKFold:
    def create_train_test_data(df: pd.DataFrame, folds: int)-> list[TrainTestData]: 
        """Takes data and splits it up in `folds` folds for training and testing sets"""
        kf = KFold(n_splits=folds, random_state=None, shuffle= False)
        train_test_list = [] 
        for (train_index, test_index) in kf.split(df):
            train_test_list.append(TrainTestData(
                        train = df.take(train_index),
                        test = df.take(test_index)))
        return train_test_list

    def make_data_frame_stationary(df: pd.DataFrame):
        pass

    def get_fold_var_res(data: TrainTestData, maxlag: int, countrycode: str, dependent_name: str, plot_res: bool) -> FoldVARResults:
        """Using a train test split `data` creates a VAR model and calculates rmse"""

        df_train_copy = data.train.copy()
        df_train_stationary_res = Stationary.make_dataframe_stationary(df_train_copy)
        df_train_diff = df_train_stationary_res.df

        #TODO: Parameter selection: see which model is best for each dev status, by seeing which is best for each country, by seeing which is best for each fold
        var_model = VARModel.create_var_model(df_train_diff, maxlag)
        forecast = VARModel.forecast_using_var_model(var_model, data.train, data.test, df_train_diff, maxlag, dependent_name)

        if plot_res:
            ExportVARResults.plot_country_results(
                data.train, 
                data.test,
                len(data.train),
                len(data.test),
                forecast.df,
                dependent_name,
                forecast.rmse,
                countrycode
            )

        return FoldVARResults(
            forecast.rmse,
            df_train_stationary_res.fully_stationary, 
            df_train_stationary_res.itas, 
            len(data.train), 
            len(data.test)
        )

    def get_var_res_by_country(ctry_df: pd.DataFrame, maxlag: int, countrycode: str, dependent_name: str, plot_res: bool, folds: int = 4 ) -> MeanVARResults:
        """TODO: Docstring Using KFold cross validation"""

        train_test_list = VARByDevStatusKFold.create_train_test_data(ctry_df, folds)

        fold_var_res_list = [VARByDevStatusKFold.get_fold_var_res(data, maxlag, countrycode, dependent_name, plot_res) for data in train_test_list]

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

    def extract_country_and_generate_var_res(countrycode: str, df: pd.DataFrame, maxlag:int, dependent_name: str, plot_res: bool) -> MeanVARResults:
        """Given a countrycode, dataframe containing multiple country data and var parameters creates MeanVARResults"""
        ctry_df = df.loc[df['countrycode'] == countrycode]
        ctry_df = ctry_df.drop(columns=["countrycode"])
        return VARByDevStatusKFold.get_var_res_by_country(ctry_df, maxlag, countrycode, dependent_name, plot_res)

    def get_var_res_by_dev_status(unique_countrycodes: list[str], df: pd.DataFrame, maxlag: int, dependent_name: str, plot_res: bool):
        countrys_res = [VARByDevStatusKFold.extract_country_and_generate_var_res(code, df, maxlag, dependent_name, plot_res) for code in unique_countrycodes]
        
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

        ExportVARResults.plot_simple_bar(dev_status, rmse, "Development status", "Mean VAR RMSE", "Mean VAR RMSE by Development status")
        ExportVARResults.plot_simple_bar(dev_status, country_amt, "Development status", "Country amount", "Country amount by Development status")
        ExportVARResults.plot_simple_bar(dev_status, statonary_itas, "Development status", "Mean training differencing itas", "Mean differencing itas by Development status")
        ExportVARResults.plot_simple_bar(dev_status, fully_stationary, "Development status", "% of training data stationary", "% of stationary training data by Development status")
        ExportVARResults.plot_simple_bar(dev_status, train_length, "Development status", "Mean training length", "Mean training length by Development status")
        ExportVARResults.plot_simple_bar(dev_status, test_length, "Development status", "Mean testing length", "Mean testing length by Development status")


    def VAR_pipeline(df_list: list[pd.DataFrame], dependent_name: str, plot_res: bool, export_csv: bool) -> None:
        #for country data in list make var model, after which take the mean rmse as a result for VAR model for that dev status
        maxlag = 8 # FIXME: what is the right max lag???
        #TODO: want to use save more columns for each test
        res_df: pd.DataFrame = pd.DataFrame(columns = ['Development status', "Country amount", 'Mean RMSE', "Mean stationary itas", "Mean fully stationary", "Mean train length", "Mean test length"]) 
        for df in df_list:
            dev_status = df["economy"][0]
            df = df.drop(columns=["economy"])
            unique_countrycodes = df["countrycode"].unique()
            country_amt = len(unique_countrycodes)
            
            dev_stat_var_res = VARByDevStatusKFold.get_var_res_by_dev_status(unique_countrycodes, df, maxlag, dependent_name, plot_res)

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
        
        if plot_res:
            VARByDevStatusKFold.plot_dev_status_var_results(res_df)
        
        if export_csv:
            res_df.to_csv("./VAR dev status results.csv", index=False)

if __name__ == "__main__":
    pwt_by_dev_status_df_list = PWTDevStatusGenerator.subset_pwt_by_dev_stat(DevStatusLevel.ONLY_DEVELOPED) 
    VARByDevStatusKFold.VAR_pipeline(pwt_by_dev_status_df_list, "gdp_growth", False, True)