import matplotlib.pyplot as plt 
import json

class ExportVARResults:
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

    def plot_simple_bar(x_data: list, y_data: list, x_label: str, y_label: str, title: str, export_path: str):
        fig = plt.figure(figsize = (10, 5))

        plt.bar(x_data, y_data, color ='maroon', 
                width = 0.4)
        
        plt.xlabel(x_label)
        plt.ylabel(y_label)
        plt.title(title)
        #plt.show()
        plt.savefig(export_path, dpi=150)
    
    def plot_dev_status_var_results(df) -> None:
        """TODO: Docstring"""
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

    def save_json(obj: object, path: str) -> None:
        with open(path, "w") as f:
            json.dump(obj, f)
    
    def load(path) -> {}:
        with open(path, "r") as f:
            return json.load(f)

