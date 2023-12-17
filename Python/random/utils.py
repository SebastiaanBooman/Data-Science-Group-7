import pandas as pd
import os

def get_directory() -> str:
    """return current working directory"""
    return os.getcwd()


def load_penn_world_table(path_to_working_directory: str) -> pd.DataFrame:
    """
    MAKE SURE YOU HAVE "openpyxl" INSTALLED, took me a solid hour to figure that one out.. smh

    function for loading in the PWT dataset.

    args:
        path_to_working_directory (string): the string to your directory, this is absolute path because working directory changes based on where you run it, which is very annoying.

    returns:
        pd.Dataframe: a dataframe containing the entire PWT
    """



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
    """
    returns all data (including NaN) given certain parameters

    args:
        data (pd.Dataframe): the PWT data
        country_name ( [string] ): the list of countries that are to be returned, list is concatenated into one dataframe.
        start_year (int): the year from which the data starts (inclusive)
        end_year (int): the year at which the data ends (inclusive)

    returns:
        pd.Dataframe: one dataframe containing the data of a list of countries between start_year and end_year
    """
    
    
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
    """
    simple calculation for getting %change from 2 numbers

    args:
        old (int): the first number to be given
        new (int): the second number to be given

    returns:
        ((new-old)/old)*100
    """
    return ((new-old)/old)*100  


def split_on_country(data:pd.DataFrame) -> [pd.DataFrame]:
    """
    given the PWT dataframe or subset of, will split the dataframe into multiple dataframe with each country getting its own dataframe.\n
    the column "country" must be present for this method to work.

    args:
        data (pd.Dataframe): the PWT dataframe or subset of, provided the column "country" is present

    returns:
        [pd.Dataframe]: an array of dataframes with each dataframe containing all data related to one country.
    """


    country_selection = data.country.unique()
    df = []

    for country in country_selection:
        df.append(get_data_on_country(data=data, country_name=[country]))
    return df


def get_rates(data:pd.DataFrame, columns: [str] = [""]) -> [pd.DataFrame]:
    """
    splits the dataframe, creating a dataframe for each country with extra column(s) for the rates to be stored in.\n
    the column with "country" needs to be present within the data

    args:
        data (pd.Dataframe): the PWT data or a subset of the PWT data\n
        columns ( [string] ): the columns for which the rates must be calculated, columns must contain numerical values

    returns:
        [pd.Dataframe]: an array containing a dataframe for each country with the added columns
    """


    #length must be at least 2 or a %change cannot be calculated
    if(len(data) < 2):
        print(f"data of insufficient length, must at least be of length 2. current length {len(data)}")
        return 
    
    #split the dataframe into multiple with each country a seperate dataframe
    df_country_split = split_on_country(data=data)
    dfs = [] #dataframe storage

    for df_country in df_country_split:
        for column in columns: #foreach column provided
            col_i = df_country.columns.get_loc(column) #get column index
            values = []

            #find old and new values and calculate the %change and add to value storage
            for index_i in range(1, len(df_country)):
                index_old = index_i-1
                values.append(calculate_percentage_difference(df_country.iloc[index_old, col_i], df_country.iloc[index_i, col_i]))
            
            #cannot get last value as there is no "new" new value so last value is set to none, otherwise pandas will complain about length
            values.append(None)
            #add column with the values
            df_country[f"{column}_rate"] = values
        #append new dataframe to the array of country-dataframes
        dfs.append(df_country)

    return dfs


def get_import_export_numerics(cgdpo_dataframe: pd.DataFrame, import_share_dataframe: pd.DataFrame, export_share_dataframe:pd.DataFrame) -> pd.DataFrame :
    '''
    gets the numerical values from the share values that are in the PWT dataset for import and export.

    args:
        cgdpo_dataframe (pd.Dataframe): a dataframe containing the CGDPo values of a country
        import_share_dataframe (pd.Dataframe): a dataframe containing the import shares (csh_x) of a country
        export_share_dataframe (pd.Dataframe): a dataframe containing the export shares (csh_m) of a country

    returns:
        pd.Dataframe: a dataframe containing two columns ['import-value', 'export-value'] with the numerical values for the import and the export
    '''
    
    import_export_df = pd.DataFrame(columns=["import-value", "export-value"])

    for i in range(len(cgdpo_dataframe)):
        cgdpo_value = cgdpo_dataframe.iloc[i,0]
        import_value = abs(import_share_dataframe.iloc[i,0] * cgdpo_value)
        export_value = abs(export_share_dataframe.iloc[i,0] * cgdpo_value)
        

        ndf = pd.DataFrame(data={"import-value" : [import_value], "export-value": [export_value]})
        ndf.index = [f"{i}"]
        import_export_df = pd.concat([import_export_df, ndf])

    return import_export_df