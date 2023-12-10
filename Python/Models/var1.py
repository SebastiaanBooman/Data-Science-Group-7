#!/usr/bin/env python3

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import grangercausalitytests
from statsmodels.tsa.vector_ar.vecm import coint_johansen
import os
import sys

DEP = 'rgdpna'

def test_country(df, country):
    df = df[df['countrycode'] == country]

    #df = df[[DEP, 'ccon', 'rdana', 'cda', 'cn', 'rnna', 'rconna']]
    df = df[[DEP, 'ccon', 'rdana']]
    #df = df[[DEP, 'rdana', 'ccon']]
    df = df.dropna()
    #print(df)

    # TODO scale data?

    train_length = int(len(df) * 0.75)
    df_train = df[:train_length]
    df_test = df[train_length:]

    # Make data stationary (differencing)
    df_diff = df_train.diff().dropna()

    def test():
        # TODO perform ~grangers causation hypothesis test~
        # TODO perform cointegration test
        # TODO perform multi-colinearity test
        # TODO scale data?

        cols = df_diff.columns
        maxlag = 12

        # Granger's Causation Test
        def causation_test():
            matrix = pd.DataFrame(np.zeros((len(cols), len(cols))), columns=cols, index=cols)

            for c in matrix.columns:
                for r in matrix.index:
                    result = grangercausalitytests(df_diff[[r, c]], maxlag = maxlag)
                    p_values = [round(result[i + 1][0]['ssr_chi2test'][1], 4) for i in range(maxlag)]

                    matrix.loc[r, c] = np.min(p_values)

            matrix.columns = [var + '_x' for var in cols]
            matrix.index = [var + '_y' for var in cols]

            print("> Granger's Causation Test")
            print(matrix)
            print()

            # TODO check if all p-values are lower than the 0.05 (95%) significance level

        # Johanson's Cointegration Test
        def cointegration_test():
            alpha = 0.05
            result = coint_johansen(df_diff, -1, 1)

            d = {
                '0.90': 0,
                '0.95': 1,
                '0.99': 2
            }

            traces = result.lr1
            cvts = result.cvt[:, d[str(1 - alpha)]]

            def adjust(val, length = 6):
                return str(val).ljust(length)

            print("> Johanson's Cointegration Test")
            print('Name   ::  Test Stat > C(95%)    =>   Signif  \n', '--'*20)
            for col, trace, cvt in zip(cols, traces, cvts):
                print(adjust(col), ':: ', adjust(round(trace,2), 9), ">", adjust(cvt, 8), ' =>  ' , trace > cvt)
            print()

        print()
        causation_test()
        cointegration_test()

    def forecast():
        # Create VAR model
        model = VAR(df_diff)

        # Fit the model based on the Bayesian Information Criterion (BIC) lag order
        lag_order = int(model.select_order().selected_orders['bic'])
        #lag_order = 3
        results = model.fit(lag_order)

        # Forecast
        horizon = len(df_test)
        forecast = results.forecast(df_diff.values[-lag_order:], steps = horizon)
        df_forecast = pd.DataFrame(forecast,
                                columns = df_diff.columns,
                                index = df_test.iloc[:horizon].index)

        # Reverse differencing
        df_forecast_original = df_forecast.cumsum() + df[df.index < df_forecast.index[0]].iloc[-1]

        def plot(ax, var):
            residuals = df_forecast_original[var].values - df_test[var].values[:horizon]
            rmse = np.mean((residuals)**2)**.5
            range = residuals.max() - residuals.min()
            rmse_std = rmse / range
            #rmse_std = np.sqrt((residuals**2).mean())

            ax.plot(df_train[-train_length:][var], color='white', label='Train')
            ax.plot(df_test[:horizon][var], color='tab:blue', label = 'Test')
            ax.plot(df_forecast_original[var], color = 'tab:orange', label = 'Forecast')

            ax.xaxis.set_ticks_position('none')
            ax.yaxis.set_ticks_position('none')
            ax.spines['top'].set_alpha(0)
            ax.tick_params(labelsize = 6)
            ax.set_title(f'{var} Test vs Forecast for {country}')
            ax.set_title(f'RMSE: {rmse_std}', loc = 'left', x = 0.02, y = 0.9, fontsize = 'medium')
            ax.legend(loc = 'lower right')

        # Print accuracy of the model

        #print(f'RMSE: {rmse_std}')

        # Plot
        fig, (ax1, ax2, ax3) = plt.subplots(1, 3)
        plot(ax1, DEP)
        plot(ax2, 'ccon')
        plot(ax3, 'rdana')

    if len(sys.argv) > 1 and sys.argv[1] == 'test':
        test()
    else:
        forecast()

df = pd.read_excel(f'{os.path.dirname(os.path.realpath(__file__))}/../../Data/pwt1001.xlsx',
                   sheet_name = 'Data',
                   parse_dates = ['year'],
                   index_col = 3)

plt.style.use('dark_background')
#for country in df['countrycode'].unique():
#    if country == 'CUW' or country == 'GUY' or country == 'SRB' or country == 'SXM':
#        continue
#    print(country)
#    test_country(df, country)
test_country(df, 'USA')
plt.show()
