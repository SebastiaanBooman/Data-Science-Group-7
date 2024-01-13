from enum import Enum
import pandas as pd

class DevStatusLevel(Enum):
    ALL = 1
    ONLY_DEV_UNDEVELOPED = 2
    MERGED_SUBSET = 3
    ONLY_DEVELOPED = 4

class PWTDevStatusGenerator:
    def subset_pwt_by_dev_stat(dev_stat_level: DevStatusLevel, indep_vars: list[str], gdptype:str = 'rgdpna'):
        print("Importing Penn World Table...")
        pwt = pd.read_excel('../../../Data/pwt1001.xlsx',
                        sheet_name = 'Data',
                        parse_dates = ['year'],
                        index_col = 3)
        gdp_type = gdptype #'cgdpo'
        dependent_var = "gdp_growth"
        pwt[f'{gdp_type}_lag'] = pwt.groupby(['countrycode'])[gdp_type].shift(1)
        pwt[dependent_var] = pwt.apply(lambda row : (row[gdp_type] - row[f'{gdp_type}_lag']) / row[f'{gdp_type}_lag'], axis = 1)
        indep_vars.extend([dependent_var, "countrycode"])
        pwt = pwt[indep_vars]
        pwt = pwt.dropna()

        country_dev_status_df = pd.read_csv('../../../Data/dev_status.csv')

        match dev_stat_level:
            case dev_stat_level.ALL: 
                pass #Default data         
            case dev_stat_level.ONLY_DEV_UNDEVELOPED:
                country_dev_status_df["economy"] = country_dev_status_df["economy"].apply(PWTDevStatusGenerator.__filter_only_dev_and_undeveloped_status)
            case dev_stat_level.MERGED_SUBSET:
                country_dev_status_df["economy"] = country_dev_status_df["economy"].apply(PWTDevStatusGenerator.__remove_suffix_dev_status)
            case dev_stat_level.ONLY_DEVELOPED:
                #TODO: implement __filter_only_developed_status 
                country_dev_status_df = country_dev_status_df[country_dev_status_df["economy"].apply(lambda x: x.startswith("Developed region"))]
            case _:
                raise NotImplementedError
            
        unique_eco_statuses = country_dev_status_df["economy"].unique()

        return PWTDevStatusGenerator.__create_pwt_dev_status_subset(unique_eco_statuses, country_dev_status_df, pwt)
        
    def __create_pwt_dev_status_subset(dev_statuses: list[str], dev_status_df: pd.DataFrame, pwt: pd.DataFrame) -> list[pd.DataFrame]:
        """For each dev status create a subset dataframe using pwt, return all the dataframes in a list"""
        dev_stat_df_list = []
        joined_df = pwt.join(dev_status_df.set_index("countrycode"), on=["countrycode"], how="inner")

        for dev_stat in dev_statuses:
            dev_stat_df_list.append(joined_df.loc[joined_df["economy"] == dev_stat])
        return dev_stat_df_list

    def __remove_suffix_dev_status(dev_status: str) -> str:
        split_dev_status = dev_status.split()
        if split_dev_status[-2].endswith(":"):
            dev_status = dev_status.rsplit(" ", 1)[0].rstrip(":")
        return dev_status

    def __filter_only_dev_and_undeveloped_status(dev_status: str) -> str:
        cleaned_status = PWTDevStatusGenerator.__remove_suffix_dev_status(dev_status)
        if not cleaned_status.startswith("Developed region"):
            cleaned_status = "Undeveloped region"
        return cleaned_status 

    def __filter_only_developed_status(dev_status: str) -> str:
        cleaned_status = PWTDevStatusGenerator.__remove_suffix_dev_status(dev_status)
        if not cleaned_status.startswith("Developed region"):
            cleaned_status = "Undeveloped region"
        return cleaned_status 