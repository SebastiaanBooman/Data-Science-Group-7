import matplotlib.pyplot as plt 

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

    def plot_simple_bar(x_data: list, y_data: list, x_label: str, y_label: str, title: str):
        fig = plt.figure(figsize = (10, 5))

        plt.bar(x_data, y_data, color ='maroon', 
                width = 0.4)
        
        plt.xlabel(x_label)
        plt.ylabel(y_label)
        plt.title(title)
        plt.show()
