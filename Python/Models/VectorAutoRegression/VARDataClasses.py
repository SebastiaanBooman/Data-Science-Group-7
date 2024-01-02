from dataclasses import dataclass
from pandas import DataFrame

@dataclass
class VARHyperParams:
    lag: int
    trend: str
    score: float

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
class TrainTestData:
    train: DataFrame
    test: DataFrame