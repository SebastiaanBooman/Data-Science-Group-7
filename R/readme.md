# Data Science Group 7 (R codebase)
## Description

Files within the R codebase have been written for the first three milestones within the Hogeschool Rotterdam Data Science Minor. (CMIBOD). After defining the dependencies, the functionality is explained on milestone basis.

### Dependencies

* Pacman 

### Milestone 1: Data Storytelling
To visualize a narrative from the Penn World Table three map graphs and one linechart have been put together. These plots have been used for the presentation of data storytelling.
#### Map graph
The first plot consists of a map chart with GDP per capita data. 
For the milestone 1 presentation these plots were created from this file:
- GDP per capita in 2019 (in billion USD)
- Average GDP per capita growth between 1990 and 2019 (in %)
- GDP per capita growth between 2008 and 2009 (in %)

A GDP per capita plot can be created by sourcing the 
[gdp.R](Maps/gdp.R) file in the sub directory [/Maps](Maps). Note that the file requires that the current working directory is [/R](/R).
Running the following commands will generate a GDP growth per capita map plot. 

```
setwd("R")
source("Maps/gdp.R")
```

In order to generate plots using mean values (based on beginning and end year of each country) instead of calculating the growth the `mean` hyperparameter may be set to `TRUE` in the `gdpmap()` function.

#### Line chart
The second plot is a linegraph showing the average GDP growth fluctuations by year based on countries development statuses. This plot can be created by sourcing [gdp_on_dev_status.R](linegraphs/gdp_on_dev_status.R) file in the sub directory [/linegraphs](linegraphs). Note that the file requires that the current working directory is [/R](/R).
Running the following commands will generate an average GDP growth fluctuations by year based on countries development statuses linegraph.

```
setwd("R")
source("linegraphs/gdp_on_dev_status.R")
```
### Milestone 2: Correlation 

The directory [/Correlation](Correlation) contains all files used for milestone 2. The contents of this directory are the bulk of the R segment for Group 7's project. Docstrings have been added to each function in these files to improve clarity.

Two root functionalities have been worked out when it comes to linear correlation testing in R:
- Linear correlation pipeline
- Linear correlation matrix

The linear correlation pipeline calculates for each country  (or merged groups based by development status) various linear correlation tests (assumptions) (defined in [statistical_tests.R](Correlation/statistical_tests.R)). As some tests require visual judgement a variety of plots also get generated and automatically exported (defined in [visual_tests.R](Correlation/visual_tests.R))

To generate linear correlation tests an X and Y variable have to be defined. Hardcoded these values are:
- (Y): Gross Domestic Product per capita
- (X): Population, Average hours worked, capital formation, capital services, labor compensation, Human Capital Index and Total Factor Productivity.

Note that for each X the linear correlation get calculate for the Y (simple y ~ model). this means that the pipeline calculates many linear models + assumptions

By sourcing [correlation_pipeline.R](Correlation/correlation_pipeline.R) the output will be generated.

```
setwd("R")
source("Correlation/correlation_pipeline.R")
```

The linear correlation matrix does not calculate any correlation assumptions. Instead, the functionality consists of calculating the Pearson R squared value for each y ~ x combination. this plot can be created by sourcing [correlation_matrices.R](Correlation/correlation_matrices.R).

```
setwd("R")
source("Correlation/correlation_matrices.R")
```
Note that during the course of the project it became clear that predicting GDP growth was not going to be a simple linear correlation problem. This is the reason Vector Auto Regression has ultimately been used instead (see the [Python](../Python) codebase. All these correlation tests have therefore not been documented in the project report. The graphs have been used in the milestone 2 presentation.

### Milestone 3: Background research 
 
 For the third milestone a simple map graph has been created for the presentation highlighting whether a country is developed (in green) or undeveloped (in red). The development status has been taken from the Natural Earth Dataset.

 By sourcing [developed_countries.R](Maps/developed_countries.R) this graph will be plotted and immediately saved to disk.

```
setwd("R")
source("Maps/developed_countries.R")
```

### Miscellaneous files
Remaining files within the R code base have either been expiremental or are used as a dependency by the files mentioned above.