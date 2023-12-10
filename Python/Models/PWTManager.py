import pandas as pd

class PWTManager():
    """Stores and manages transformations for DataFrames containing Penn World Table"""
    __slots__ = ["PWT_df", "PWT_is_modified", "PWT_loc"]

    def __init__(self):
        self.PWT_df = pd.DataFrame()
        self.PWT_is_modified = False
        self.PWT_loc= "../../Data/pwt1001.xlsx"

    #def get_directory() -> str:
    #    """return current working directory"""
    #    return os.getcwd()
    
    #TODO: Really necessary to give work dir?
    def get_clean_pwt(self) -> pd.DataFrame:
        """Lazy initializes the Penn World Table, returns empty `pd.DataFrame` if cannot open PWT file"""
        if not self.PWT_df.empty and not self.PWT_is_modified:
            return self.clean_PWT
        print("Loading PWT")
        pwt = self.__load_penn_world_table()
        if not pwt.empty:
            self.PWT_df = pwt
            self.PWT_is_modified = False
        return pwt
        
    def get_subset_on_country(self, country_names: list[str] = [""], start_year: int = 1950, end_year: int = 2019) -> pd.DataFrame:
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
        
        df = pd.DataFrame(columns=self.PWT_df.columns)

        for c_name in country_names:
            try:
                selection = self.PWT_df["country"]==c_name
                df = pd.concat([df, self.PWT_df[selection]], ignore_index=True)
            except:
                print(f"country: {c_name} was not found")

        if(start_year <= end_year):
            selection = df["year"]>= start_year
            df = df[selection]

            selection = df["year"]<=end_year
            df = df[selection]

        return df
    

    def get_rates(self, columns: list[str] = [""]) -> [pd.DataFrame]:
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
        if(len(self.PWT_df) < 2):
            print(f"data of insufficient length, must at least be of length 2. current length {len(data)}")
            return 
        
        #split the dataframe into multiple with each country a seperate dataframe
        df_country_split = self.__split_on_country()
        dfs = [] #dataframe storage

        for df_country in df_country_split:
            for column in columns: #foreach column provided
                col_i = df_country.columns.get_loc(column) #get column index
                values = []

                #find old and new values and calculate the %change and add to value storage
                #TODO: use pandas lag 
                for index_i in range(1, len(df_country)):
                    index_old = index_i-1
                    values.append(self.__calculate_percentage_difference(df_country.iloc[index_old, col_i], df_country.iloc[index_i, col_i]))
                
                #cannot get last value as there is no "new" new value so last value is set to none, otherwise pandas will complain about length
                values.append(None)
                #add column with the values
                df_country[f"{column}_rate"] = values
            #append new dataframe to the array of country-dataframes
            dfs.append(df_country)

        return dfs
    

    def __load_penn_world_table(self) -> pd.DataFrame:
        """
        MAKE SURE YOU HAVE "openpyxl" INSTALLED, took me a solid hour to figure that one out.. smh

        function for loading in the PWT dataset.

        args:
            path_work_dir (string): the string to your directory, this is absolute path because working directory changes based on where you run it, which is very annoying.

        returns:
            pd.Dataframe: a dataframe containing the entire PWT | False if it cannot find 
        """

        print(f"searching for file at location: /{self.PWT_loc}")
        try:
            d = pd.read_excel(f"{self.PWT_loc}", sheet_name="Data")
            print("succesfully loaded PWT dataset")
            return d
        except:
            print("importing PWT data failed as file was not found or openpyxl is not installed")
            print(f"search location was: {self.PWT_loc}")
            return pd.DataFrame()

    def __calculate_percentage_difference(self, old: int, new: int) -> pd.DataFrame:
        """
        simple calculation for getting %change from 2 numbers

        args:
            old (int): the first number to be given
            new (int): the second number to be given

        returns:
            ((new-old)/old)*100
        """
        return ((new-old)/old)*100  


    def __split_on_country(self) -> list[pd.DataFrame]:
        """
        given the PWT dataframe or subset of, will split the dataframe into multiple dataframe with each country getting its own dataframe.\n
        the column "country" must be present for this method to work.

        args:
            data (pd.Dataframe): the PWT dataframe or subset of, provided the column "country" is present

        returns:
            [pd.Dataframe]: an array of dataframes with each dataframe containing all data related to one country.
        """

        country_selection = self.PWT_df.country.unique()
        df = []

        for country in country_selection:
            df.append(self.get_subset_on_country(country_names=[country]))
        return df



if __name__ == "__main__":
    # The client code.

    PWT_manager = PWTManager()
    #s2 = PWTManager()

    data = PWT_manager.get_clean_pwt()
    print(data.shape)
    print(data.head())

    countries = ["Netherlands", "Aruba"]
    df = PWT_manager.get_subset_on_country(country_names=countries, start_year=1995, end_year=2000)
    print(df.shape)
    print(df.head())


    dfr = PWT_manager.get_rates(columns=["rgdpo"])
    print(dfr[0])








    #if id(s1) == id(s2):
    #    print("Singleton works, both variables contain the same instance.")
    #    #print(s1.a)
    #    #print(s2.a)
    #else:
    #    print("Singleton failed, variables contain different instances.")