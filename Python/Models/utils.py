import pandas as pd
import numpy as np
import os

def get_directory() -> str:
    return os.getcwd()


def load_penn_world_table(path_to_working_directory: str) -> pd.DataFrame:
    file_location = "Data/pwt1001.xlsx"
    print(f"searching for file at location: {path_to_working_directory}/{file_location}")
    try:
        d = pd.read_excel(f"{path_to_working_directory}/{file_location}", sheet_name="Data")
        print("succesfully loaded PWT dataset")
        return d
    except:
        print("importing PWT data failed as file was not found or openpyxl is not installed")
        print(f"search location was: {path_to_working_directory}/file_location")
        return pd.DataFrame()


def get_data_on_country(data: pd.DataFrame, country_name: [str] = [""], start_year: int = 1950, end_year: int = 2019) -> pd.DataFrame:
    df = pd.DataFrame(columns=data.columns)

    for c_name in country_name:
        try:
            selection = data["country"]==c_name
            df = pd.concat([df, data[selection]], ignore_index=True)
        except:
            print(f"country: {c_name} was not found")

    if(start_year <= end_year):
        selection = df["year"]>= start_year
        df = df[selection]

        selection = df["year"]<=end_year
        df = df[selection]

    return df


def calculate_percentage_difference(old: int, new: int) -> pd.DataFrame:
    return ((new-old)/old)*100  


def split_on_country(data:pd.DataFrame) -> [pd.DataFrame]:
    country_selection = data.country.unique()
    df = []

    for country in country_selection:
        df.append(get_data_on_country(data=data, country_name=[country]))
    return df


def get_rates(data:pd.DataFrame, columns: [str] = [""]) -> [pd.DataFrame]:
    if(len(data) < 2):
        print(f"data of insufficient length, must at least be of length 2. current length {len(data)}")
        return 
    
    
    df_country_split = split_on_country(data=data)
    dfs = []

    for df_country in df_country_split:
        for column in columns:
            col_i = df_country.columns.get_loc(column)
            values = []
            for index_i in range(1, len(df_country)):
                index_old = index_i-1
                values.append(calculate_percentage_difference(df_country.iloc[index_old, col_i], df_country.iloc[index_i, col_i]))
            
            
            values.append(None)
            df_country[f"{column}_rate"] = values
        dfs.append(df_country)

    return dfs

