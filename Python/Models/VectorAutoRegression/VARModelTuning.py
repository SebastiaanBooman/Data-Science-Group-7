# This makes sure Pandas keeps it mouth shut
import warnings
warnings.simplefilter(action = 'ignore', category = FutureWarning)
import pandas as pd
import numpy as np
from statsmodels.tsa.api import VAR
from sklearn.model_selection import KFold
from VARDataClasses import VARPredictionResult, FoldVARResults, MeanVARResults, TrainTestData, AggregatedFoldVARResults, VARHyperParams, DevStatusResult, CountryVARResult, VARExportClass
from dataclasses import asdict
from PWTDevStatus import PWTDevStatusGenerator, DevStatusLevel
from StationaryFunctions import Stationary
from VARExportResults import ExportVARResults
from VARParameterSelection import VARParameterSelection
from VARModel import VARModel

class VARModelTuning:
    """Contains functionality to tune and compare different VAR countries using hyper parameter selection on development status, country, and fold basis"""
    def create_train_test_data(df: pd.DataFrame, folds: int)-> list[TrainTestData]: 
        """Takes data and splits it up in `folds` folds for training and testing sets"""
        kf = KFold(n_splits=folds, random_state=None, shuffle= False)
        train_test_list = [] 
        for (train_index, test_index) in kf.split(df):
            train_test_list.append(TrainTestData(
                        train = df.take(train_index),
                        test = df.take(test_index)))
        return train_test_list

    def get_fold_var_res(data: TrainTestData, maxlag: int, countrycode: str, dependent_name: str, fold_ita: int, plot_res: bool, dev_status: str) -> FoldVARResults:
        """Using a train test split `data` creates a VAR model and calculates FoldVARResults"""

        df_train_copy = data.train.copy()
        df_train_stationary_res = Stationary.make_dataframe_stationary(df_train_copy)
        best_fit_pred_res = VARPredictionResult(0, VARHyperParams("NA", -1))

        if df_train_stationary_res.fully_stationary:
            df_train_diff = df_train_stationary_res.df
            var_model = VAR(df_train_diff, freq = df_train_diff.index.inferred_freq) 
            #TODO: Call var_param_search with queue length of one, maybe even deprecate ability to get bigger result if not necessary
            best_fit_res = VARParameterSelection.var_parameter_search(var_model, dependent_name, data.train, df_train_diff, data.test, df_train_stationary_res.itas, maxlag)[0]
            best_fit_pred_res = best_fit_res[0]

            if plot_res and fold_ita == 4:#dev_status in["Developing region"]: #["Developed region"]:
                ExportVARResults.plot_country_results(
                    df_train=data.train, 
                    df_test=data.test,
                    train_length=len(data.train),
                    test_length=len(data.test),
                    df_forecast=best_fit_res[1],
                    gdp=dependent_name,
                    rmse=best_fit_pred_res.rmse,
                    country_name=countrycode
                )

        return FoldVARResults(
            fold_ita,
            df_train_stationary_res.fully_stationary, 
            df_train_stationary_res.itas, 
            len(data.train), 
            len(data.test),
            best_fit_pred_res
        )

    def get_var_res_by_country(ctry_df: pd.DataFrame, maxlag: int, countrycode: str, dependent_name: str, plot_res: bool, folds: int, dev_status) -> CountryVARResult:
        """TODO: Docstring Using KFold cross validation"""

        train_test_data = VARModelTuning.create_train_test_data(ctry_df, folds)

        fold_var_res = [VARModelTuning.get_fold_var_res(data, maxlag, countrycode, dependent_name, i, plot_res, dev_status) for i, data in enumerate(train_test_data)]

        fold_fully_stationary_list = [fr.is_fully_stationary for fr in fold_var_res]

        stationary_fold_var_res = [fold_res for fold_res in fold_var_res if fold_res.is_fully_stationary]

        fold_rmse_list = [fr.pred_res.rmse for fr in stationary_fold_var_res]
        fold_stationary_itas_list= [fr.stationary_itas for fr in stationary_fold_var_res]
        fold_train_length_list= [fr.train_length for fr in stationary_fold_var_res]
        fold_test_length_list= [fr.test_length for fr in stationary_fold_var_res]

        mean_rmse_folds = np.mean(fold_rmse_list)
        mean_fully_stationary_folds = np.mean(fold_fully_stationary_list)
        mean_stationary_itas_folds = np.mean(fold_stationary_itas_list)
        mean_train_length_folds = np.mean(fold_train_length_list)
        mean_test_length_folds = np.mean(fold_test_length_list)
        mean_var_res = MeanVARResults(mean_rmse_folds, mean_stationary_itas_folds, mean_fully_stationary_folds, mean_train_length_folds, mean_test_length_folds)

        return CountryVARResult(stationary_fold_var_res, mean_var_res) 

    def calc_mean_from_country_var_res(country_var_res: CountryVARResult) -> MeanVARResults:
        """TODO: Docstring"""
        rmse_list = [res.mean_fold_res.mean_rmse for res in country_var_res]
        fully_stationary_list = [res.mean_fold_res.mean_fully_stationary for res in country_var_res]
        stationary_itas_list = [res.mean_fold_res.mean_stationary_itas for res in country_var_res]
        train_length_list = [res.mean_fold_res.mean_train_length for res in country_var_res]
        test_length_list = [res.mean_fold_res.mean_test_length for res in country_var_res]

        mean_rmse = np.mean(rmse_list)
        mean_fully_stationary = np.mean(fully_stationary_list)
        mean_stationary_itas = np.mean(stationary_itas_list)
        mean_train_length = np.mean(train_length_list)
        mean_test_length = np.mean(test_length_list)

        return MeanVARResults(mean_rmse, mean_stationary_itas, mean_fully_stationary, mean_train_length, mean_test_length)

    def calculate_fold_var_res(country_var_res: CountryVARResult, folds: int) -> AggregatedFoldVARResults:
        """TODO: Docstring"""
        res_by_fold = []
        for fold in range(folds):
            fold_amt = 0
            fold_lags = []
            fold_trends = []
            fold_rmses = []
            for cr in country_var_res: 
                for fold_res in cr.folds_res:
                    if fold_res.fold_ita == fold:
                        fold_amt += 1
                        fold_lags.append(fold_res.pred_res.hyper_params.lag)
                        fold_trends.append(fold_res.pred_res.hyper_params.trend)
                        fold_rmses.append(fold_res.pred_res.rmse)

            mode_trend = max(fold_trends, key = fold_trends.count) if len(fold_trends) > 0 else "No data"
            mode_lag = max(fold_lags, key = fold_lags.count) if len(fold_lags) > 0 else "No data"
            mean_rmse = np.mean(fold_rmses) if len(fold_rmses) > 0 else "No data"

            res_by_fold.append(
                AggregatedFoldVARResults(
                    (fold + 1),
                    fold_amt,
                    mean_rmse,
                    VARHyperParams(
                        mode_trend,
                        mode_lag
                    )
                )
            )
        return res_by_fold

    def extract_country_and_generate_var_res(countrycode: str, df: pd.DataFrame, maxlag: int, dependent_name: str, plot_res: bool, folds: int, dev_status) -> CountryVARResult:
        """Given a countrycode, dataframe containing multiple country data and var parameters creates CountryVARResult"""
        ctry_df = df.loc[df['countrycode'] == countrycode]
        ctry_df = ctry_df.drop(columns=["countrycode"])

        var_res_by_country = VARModelTuning.get_var_res_by_country(ctry_df, maxlag, countrycode, dependent_name, plot_res, folds, dev_status)
        return var_res_by_country

    def get_var_res_by_dev_status(country_amount: int, dev_status: str, unique_countrycodes: list[str], df: pd.DataFrame, maxlag: int, dependent_name: str, plot_res: bool) -> DevStatusResult:
        """TODO: Docstring"""
        folds = 4
        countrys_res = [VARModelTuning.extract_country_and_generate_var_res(code, df, maxlag, dependent_name, plot_res, folds, dev_status) for code in unique_countrycodes]

        res_by_fold = VARModelTuning.calculate_fold_var_res(countrys_res, folds)
        
        return DevStatusResult(dev_status, country_amount, res_by_fold) 

    def VAR_pipeline(df_list: list[pd.DataFrame], dependent_name: str, indep_names: list[str], plot_res: bool, export_csv: bool, export_json: bool) -> None:
        """TODO: Docstring"""
        maxlag = 8
        res_df: pd.DataFrame = pd.DataFrame(columns = ['Development status', "Country amount", 'Mean RMSE', "Mean stationary itas", "Mean fully stationary", "Mean train length", "Mean test length"]) 
        dev_status_var_res_list = []
        for df in df_list:
            dev_status = df["economy"][0]
            df = df.drop(columns=["economy"])
            unique_countrycodes = df["countrycode"].unique()
            country_amt = len(unique_countrycodes)
            
            dev_stat_var_res = VARModelTuning.get_var_res_by_dev_status(country_amt, dev_status, unique_countrycodes, df, maxlag, dependent_name, plot_res)
            dev_status_var_res_list.append(dev_stat_var_res)

            #res_df = pd.concat([res_df, pd.DataFrame([
            #    {
            #        "Development status" : dev_status, 
            #        "Country amount": country_amt, 
            #        "Mean RMSE": dev_stat_var_res.mean_var_results.mean_rmse,
            #        "Mean stationary itas" : dev_stat_var_res.mean_var_results.mean_stationary_itas,
            #        "Mean fully stationary": dev_stat_var_res.mean_var_results.mean_fully_stationary,
            #        "Mean train length" : dev_stat_var_res.mean_var_results.mean_train_length,
            #        "Mean test length" : dev_stat_var_res.mean_var_results.mean_test_length
            #        }])], ignore_index=True)

        if export_json:
            ExportVARResults.save_json(asdict(VARExportClass(dependent_name, indep_names, dev_status_var_res_list)), "./VAR dev status results.json")

        if plot_res:
            ExportVARResults.plot_dev_status_var_results(res_df)
        
        if export_csv:
            res_df.to_csv("./VAR dev status results.csv", index=False)

class BaseModelTuning:
    def create_train_test_data(df: pd.DataFrame, folds: int)-> list[TrainTestData]: 
        """Takes data and splits it up in `folds` folds for training and testing sets"""
        kf = KFold(n_splits=folds, random_state=None, shuffle= False)
        train_test_list = [] 
        for (train_index, test_index) in kf.split(df):
            train_test_list.append(TrainTestData(
                        train = df.take(train_index),
                        test = df.take(test_index)))
        return train_test_list

    def get_fold_baseline_res(data: TrainTestData, maxlag:int, countrycode: str, dependent_name: str, fold_ita: int, plot_res: bool) -> FoldVARResults:
        """Using a train test split `data` creates a VAR model and calculates FoldVARResults"""

        df_train_copy = data.train.copy()
        df_train_stationary_res = Stationary.make_dataframe_stationary(df_train_copy)
        baseline_res = VARPredictionResult(0, VARHyperParams("NA", -1))

        if df_train_stationary_res.fully_stationary:
            df_train_diff = df_train_stationary_res.df
            var_model = VAR(df_train_diff, freq = df_train_diff.index.inferred_freq) 
            
            baseline_model = VARModel.fit_base_model(model=var_model)
            baseline_res = VARModel.forecast_using_var_model(var_model=baseline_model,
                                                            undiff_train_data=data.train,
                                                            undiff_test_data=data.test,
                                                            diff_train_df=df_train_diff,
                                                            maxlag=maxlag,
                                                            stationary_itas=df_train_stationary_res.itas,
                                                            dependent_name=dependent_name
                                                            )

            if plot_res:
                ExportVARResults.plot_country_results(
                    df_train=data.train, 
                    df_test=data.test,
                    train_length=len(data.train),
                    test_length=len(data.test),
                    df_forecast=baseline_res.pred,
                    gdp=dependent_name,
                    rmse=baseline_res.rmse,
                    country_name=countrycode
                )

        return FoldVARResults(
            fold_ita,
            df_train_stationary_res.fully_stationary, 
            df_train_stationary_res.itas, 
            len(data.train),
            len(data.test),
            VARPredictionResult(baseline_res.rmse, VARHyperParams('c', 8))
        )

    def get_var_res_by_country(ctry_df: pd.DataFrame, maxlag:int, countrycode: str, dependent_name: str, plot_res: bool, folds: int) -> CountryVARResult:
        """TODO: Docstring Using KFold cross validation"""

        train_test_data = BaseModelTuning.create_train_test_data(ctry_df, folds)

        baseline_res = [BaseModelTuning.get_fold_baseline_res(data, maxlag, countrycode, dependent_name, i+1, plot_res) for i, data in enumerate(train_test_data)]

        fold_fully_stationary_list = [fr.is_fully_stationary for fr in baseline_res]

        stationary_fold_var_res = [fold_res for fold_res in baseline_res if fold_res.is_fully_stationary]

        fold_rmse_list = [fr.pred_res.rmse for fr in stationary_fold_var_res]
        fold_stationary_itas_list= [fr.stationary_itas for fr in stationary_fold_var_res]
        fold_train_length_list= [fr.train_length for fr in stationary_fold_var_res]
        fold_test_length_list= [fr.test_length for fr in stationary_fold_var_res]

        mean_rmse_folds = np.mean(fold_rmse_list)
        mean_fully_stationary_folds = np.mean(fold_fully_stationary_list)
        mean_stationary_itas_folds = np.mean(fold_stationary_itas_list)
        mean_train_length_folds = np.mean(fold_train_length_list)
        mean_test_length_folds = np.mean(fold_test_length_list)
        mean_var_res = MeanVARResults(mean_rmse_folds, mean_stationary_itas_folds, mean_fully_stationary_folds, mean_train_length_folds, mean_test_length_folds)

        return CountryVARResult(stationary_fold_var_res, mean_var_res) 

    def calc_mean_from_country_var_res(country_var_res: CountryVARResult) -> MeanVARResults:
        """TODO: Docstring"""
        rmse_list = [res.mean_fold_res.mean_rmse for res in country_var_res]
        fully_stationary_list = [res.mean_fold_res.mean_fully_stationary for res in country_var_res]
        stationary_itas_list = [res.mean_fold_res.mean_stationary_itas for res in country_var_res]
        train_length_list = [res.mean_fold_res.mean_train_length for res in country_var_res]
        test_length_list = [res.mean_fold_res.mean_test_length for res in country_var_res]

        mean_rmse = np.mean(rmse_list)
        mean_fully_stationary = np.mean(fully_stationary_list)
        mean_stationary_itas = np.mean(stationary_itas_list)
        mean_train_length = np.mean(train_length_list)
        mean_test_length = np.mean(test_length_list)

        return MeanVARResults(mean_rmse, mean_stationary_itas, mean_fully_stationary, mean_train_length, mean_test_length)

    def calculate_fold_var_res(country_var_res: CountryVARResult, folds: int) -> AggregatedFoldVARResults:
        """TODO: Docstring"""
        res_by_fold = []
        for fold in range(folds):
            fold_amt = 0
            fold_lags = []
            fold_trends = []
            fold_rmses = []
            for cr in country_var_res: 
                for fold_res in cr.folds_res:
                    if fold_res.fold_ita == fold:
                        fold_amt += 1
                        fold_lags.append(fold_res.pred_res.hyper_params.lag)
                        fold_trends.append(fold_res.pred_res.hyper_params.trend)
                        fold_rmses.append(fold_res.pred_res.rmse)

            mode_trend = max(fold_trends, key = fold_trends.count) if len(fold_trends) > 0 else "No data"
            mode_lag = max(fold_lags, key = fold_lags.count) if len(fold_lags) > 0 else "No data"
            mean_rmse = np.mean(fold_rmses) if len(fold_rmses) > 0 else "No data"

            res_by_fold.append(
                AggregatedFoldVARResults(
                    fold,
                    fold_amt,
                    mean_rmse,
                    VARHyperParams(
                        mode_trend,
                        mode_lag
                    )
                )
            )
        return res_by_fold

    def extract_country_and_generate_var_res(countrycode: str, df: pd.DataFrame, maxlag:int, dependent_name: str, plot_res: bool, folds: int) -> CountryVARResult:
        """Given a countrycode, dataframe containing multiple country data and var parameters creates CountryVARResult"""
        ctry_df = df.loc[df['countrycode'] == countrycode]
        ctry_df = ctry_df.drop(columns=["countrycode"])

        var_res_by_country = BaseModelTuning.get_var_res_by_country(ctry_df, maxlag, countrycode, dependent_name, plot_res, folds)
        return var_res_by_country

    def get_var_res_by_dev_status(country_amount: int, dev_status: str, unique_countrycodes: list[str], df: pd.DataFrame, maxlag:int, dependent_name: str, plot_res: bool) -> DevStatusResult:
        """TODO: Docstring"""
        folds = 4
        countrys_res = [BaseModelTuning.extract_country_and_generate_var_res(code, df, maxlag, dependent_name, plot_res, folds) for code in unique_countrycodes]

        res_by_fold = BaseModelTuning.calculate_fold_var_res(countrys_res, folds)
        
        return DevStatusResult(dev_status, country_amount, res_by_fold) 

    def VAR_pipeline(df_list: list[pd.DataFrame], dependent_name: str, indep_names: list[str], plot_res: bool, export_csv: bool, export_json: bool) -> None:
        '''The start of the VAR forecasting process'''
        maxlag = 8
        res_df: pd.DataFrame = pd.DataFrame(columns = ['Development status', "Country amount", 'Mean RMSE', "Mean stationary itas", "Mean fully stationary", "Mean train length", "Mean test length"]) 
        dev_status_var_res_list = []
        for df in df_list:
            dev_status = df["economy"][0]
            df = df.drop(columns=["economy"])
            unique_countrycodes = df["countrycode"].unique()
            country_amt = len(unique_countrycodes)
            
            dev_stat_var_res = BaseModelTuning.get_var_res_by_dev_status(country_amt, dev_status, unique_countrycodes, df, maxlag, dependent_name, plot_res)
            dev_status_var_res_list.append(dev_stat_var_res)


        if export_json:
            ExportVARResults.save_json(asdict(VARExportClass(dependent_name, indep_names, dev_status_var_res_list)), "./Baseline_VAR dev status results.json")

        if plot_res:
            ExportVARResults.plot_dev_status_var_results(res_df)
        
        if export_csv:
            res_df.to_csv("./Baseline_VAR dev status results.csv", index=False)


if __name__ == "__main__":
    indep_vars =  ['rdana', 'rtfpna', 'emp', 'cda']
    pwt_by_dev_status_df_list = PWTDevStatusGenerator.subset_pwt_by_dev_stat(DevStatusLevel.MERGED_SUBSET, list(indep_vars)) 
    #BaseModelTuning.VAR_pipeline(pwt_by_dev_status_df_list, "gdp_growth", indep_vars, False, True, True)
    VARModelTuning.VAR_pipeline(pwt_by_dev_status_df_list, "gdp_growth", indep_vars, False, False, True)