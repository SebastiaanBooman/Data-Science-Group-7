#!/usr/bin/env python3

import pandas as pd
#import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.api import VAR

COUNTRY = 'USA'
DEP = 'rgdpna'

df = pd.read_excel('Data/pwt1001.xlsx',
                   sheet_name = 'Data',
                   parse_dates = ['year'],
                   index_col = 3)
df = df[df['countrycode'] == COUNTRY]
df = df[[DEP, 'ccon', 'rdana', 'cda', 'cn', 'rnna', 'rconna']]
df = df.dropna()

# TODO perform grangers causation hypothesis test
# TODO perform cointegration test
# TODO scale data?

train_length = int(len(df) * 0.75)
df_train = df[:train_length]
df_test = df[train_length:]

# Make data stationary (differencing)
df_diff = df_train.diff().dropna()

# Create VAR model
model = VAR(df_diff)

# Fit the model based on the Bayesian Information Criterion (BIC) lag order
lag_order = int(model.select_order().selected_orders['bic'])
results = model.fit(lag_order)

#print(results.summary())

# Forecast
horizon = len(df_test)
forecast = results.forecast(df_diff.values[-lag_order:], steps = horizon)
df_forecast = pd.DataFrame(forecast,
                           columns = df_diff.columns,
                           index = df_test.iloc[:horizon].index)

# Reverse differencing
df_forecast_original = df_forecast.cumsum() + df[df.index < df_forecast.index[0]].iloc[-1]

# Plot
fig, ax = plt.subplots()
ax.plot(df_train[-train_length:][DEP], color='red', label='Train')
ax.plot(df_test[:horizon][DEP], color='orange', label = 'Test')
ax.plot(df_forecast_original[DEP], color = 'blue', label = 'Forecast')

ax.xaxis.set_ticks_position('none')
ax.yaxis.set_ticks_position('none')
ax.spines['top'].set_alpha(0)
ax.tick_params(labelsize=6)
ax.set_title(f'{DEP} forecast')
ax.legend()

plt.show()
