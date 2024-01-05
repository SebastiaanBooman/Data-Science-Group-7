from dataclasses import dataclass
from pandas import DataFrame

@dataclass
class VARHyperParams:
    trend: str
    lag: int

@dataclass
class VARPredictionResult:
    rmse: float
    hyper_params: VARHyperParams

@dataclass
class MeanVARResults:
    mean_rmse: float
    mean_stationary_itas: float
    mean_fully_stationary: float
    mean_train_length: float
    mean_test_length: float

@dataclass
class FoldVARResults:
    fold_ita: int
    is_fully_stationary: bool
    stationary_itas: int
    train_length: int
    test_length: int
    pred_res: VARPredictionResult

@dataclass
class AggregatedFoldVARResults:
    fold_ita: int
    mean_rmse: float
    mode_var_params: VARHyperParams

@dataclass
class CountryVARResult:
    folds_res: list[FoldVARResults]
    mean_fold_res: MeanVARResults 

@dataclass 
class TrainTestData:
    train: DataFrame
    test: DataFrame

@dataclass
class DevStatusResult:
    development_status: str
    country_amount: int
    mean_var_results: MeanVARResults
    fold_results: list[AggregatedFoldVARResults]
    
#TODO: Better name for this class
@dataclass
class VARExportClass:
    dependent_variable: str
    independent_variables: list[str]
    dev_status_results: list[DevStatusResult]