# Data Science Group 7 (Python)
## Description

Files within the Python codebase have been written for the final milestone within the Hogeschool Rotterdam Data Science Minor. (CMIBOD). After defining the dependencies, the differnet functionality and use cases are explained. 

### Dependencies

* Python 3.12
* Module dependencies can be found in TODO

### Milestone 4: Data mining tool utilization

For milestone 4 we decided to use Vector Auto Regression (VAR) as a model for the presentation. Later, we used this same model (slightly adjusted) as a baseline for the report. Furthermore the final model has been made by running a parameter selection of VAR. These files can be found in the following places:

#### Baseline model
[/Models/VectorAutoRegression/VARBaselineModel.py](Models/VectorAutoRegression/VARBaselineModel.py) contains the baseline model. Sourcing the file like the following example will generate baseline model results. These results consist of various plots and CSV file which will be automatically exported.

```
python ./Models/VectorAutoRegression/VARBaselineModel.py
```

#### Parameter Selection
[/Models/VectorAutoRegression/VARModelTuning.py](Models/VectorAutoRegression/VARModelTuning.py) contains the parameter selection functionality. Additionaly this file contains functionality to use an existing model and just get it's prediction results.
Running the following command will execute the Python file.
```
python ./Models/VectorAutoRegression/VARModelTuning.py
```
#TODO: What will happen after executing this file?

### Miscellaneous files
Remaining files within the R code base have either been expiremental or are used as a dependency by the files mentioned above.