# Data Science Group 7 (Python)
## Description

Files within the Python codebase have been written for the final milestone within the Hogeschool Rotterdam Data Science Minor. (CMIBOD). After defining the dependencies, the different functionality and use cases are explained.
Any file to be generated will need to be in place and cannot be renamed. Doing so will ensure that the program won't be able to find the necessary files and will refuse to run.
There are multiple files, there is NO implementation to run all the files in one go, thus it is recommended to run the files one by one.
This file will provide guidance on how to produce the results from the report and will give a brief explainer what each relevant file does. I say this as there are a couple of old files that are there for savekeeping and are deprecated / unused.

### Dependencies

* Python 3.12
* Module dependencies can be found in [requirements.txt](requirements.txt)

```
pip install -r requirements.txt
```

### File run order
Not all files can be run from the start. Some files produce results that are necessary by other files. This is true for getting the plots because first the results would have to be generated.
For ease the order at which the files are documented here is the same as the desired running order.

1. [VARModelTuning.py](Models/VectorAutoRegression/VARModelTuning.py)
2. [VARImportResults.py](Models/VectorAutoRegression/VARImportResults.py)

The other file that produces data is the [VAR.ipynb](Models/VectorAutoRegression/VAR.ipynb). This data however is not stored in the directory but is kept within the notebook. This data has no necessity anywhere and can be run whenever. It does rely on a couple of functions. What this file produces at the end are the forecast plots from the report.

#### VAR results
[VARModelTuning.py](Models/VectorAutoRegression/VARModelTuning.py) contains the final model together with the baseline model. Running this file will produce 2 *.json* files, one containing the results for the baseline mode and containing the results for the final model.

#### VAR plots
[VARImportResult.py](Models/VarImportResults.py) contains the methods necassary for creating the bar plots. These plot are created from the contents of the *.json* files that were created from VARModelTuning. Both the *.json* files are necessary for the plot function to work as it runs twice and expects both files to be present.

The forecast plots are made in [VAR.ipynb](/Models/VectorAutoRegression/VAR.ipynb). This file produces the forecast plots for a couple of developed regions.

### Miscellaneous files
The files listed here can fit under two categories which are supporting files and deprecated files.

#### Penn World Table(PWT) data
The data by the PWT is handled by [PWTDevStatus.py](Models/VectorAutoRegression/PWTDevStatus.py). This is a supporting file that manages the data from the PWT. It can return a couple of collection of data such as only lesser developed country or a merged dataset.

#### Stationary functions
[StationaryFunctions.py](/Models/VectorAutoRegression/StationaryFunctions.py) is a support file containing the functions necessary for making data stationary and testing if the data is stationary.

#### Parameter Selection
[ParameterSelection.py](Models/VectorAutoRegression/VARParameterSelection.py) is a support file that finds the optimal parameters which produce the lowest RMSE. It does this by iteratively going over every possible trend and lag, doing a forecast and testing the model to get an RMSE value. The RMSE then gets compared to previous iterations and a couple of the lowest results get stored. The method in this file returns a list of parameters, the RMSE, and the dataframe containing the forecast.

#### Data classes
Almost all data classes are are stored within [VARDataClasses.py](Models/VectorAutoRegression/VARDataClasses.py) with the exception of perhaps 1 or 2 dataclasses. these dataclasses made it a lot easier for storing data together into custom made objects.