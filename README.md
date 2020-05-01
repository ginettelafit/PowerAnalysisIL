# PowerAnalysisIL

## Shiny app and R package to perform a power analysis to select the number of participants in intensive longitudinal studies

Users can download the app and run locally on their computer by executing the following commands in R or Rstudio. 

```
library(htmltools)
library(shiny)
library(DT)
library(nlme)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)
library(dplyr)
library(formattable)
library(tidyr)
library(MASS)
library(plyr)
library(parallel)
library(shinyjs)
library(compiler)
library(future.apply)

library(devtools)
devtools::install_github("ginettelafit/PowerAnalysisIL", force = T)

library(PowerAnalysisIL)
PowerAnalysisIL::RunShiny()

# Using Gist: users can launch this app with:
shiny::runGist('6bac9d35c2521cc4fd91ce4b82490236')
```

## Screenshot of PowerAnalysisIL a shiny app to perform power analysis to select the number of participants in intensive longitudinal studies

<img src="Image_1.png">

## How the app works in a nutshell

Consider a researcher who is planning an Intensive Longitudinal (IL) study to test the hypothesis that depression (level-2 continuous variable) is positively related to negative affect (Affect).

![](https://latex.codecogs.com/gif.latex?%5Ctext%7BLevel%201%3A%7D%20%5C%3B%20%5Ctext%7BAffect%7D_%7Bit%7D%20%3D%20%5Cgamma_%7B0i%7D%20&plus;%20%5Cepsilon_%7Bit%7D%20%5C%3B%20%5Ctext%7Bfor%7D%20%5C%3B%20i%3D1%2C%5Cldots%2CN%20%5C%3B%20%5Ctext%7Band%7D%20%5C%3B%20t%3D1%2C%5Cldots%2CT)
![](https://latex.codecogs.com/gif.latex?%5Ctext%7BLevel%202%3A%7D%20%5C%3B%20%5Cgamma_%7B0i%7D%20%3D%20%5Cbeta_%7B00%7D%20&plus;%20%5Cbeta_%7B01%7D%5Ctext%7BDepression%7D_i%20&plus;%20%5Cnu_%7B0i%7D)

where the level-1 and level-2 errors are mutually independent and normally distributed
![](https://latex.codecogs.com/gif.latex?%5Cepsilon_%7Bit%7D%20%5Csim%20N%280%2C%5Csigma_%7B%5Cepsilon%7D%5E2%29)
![](https://latex.codecogs.com/gif.latex?%5Cnu_%7B0i%7D%20%5Csim%20N%280%2C%5Csigma_%7B%5Cnu_0%7D%5E2%29)

The data will be collected using an IL design, including 70 measurement occasions per individual. How many participants does she need to involve? 

### Step 1: App Input

To perform simulation-based power analysis, we need to specify the parameter values of Model of interest. We select Model 2 (i.e., effect of a level-2 continuous predictor on the mean level of the outcome variable). We indicate that we want to consider the following sample sizes: 15, 30, 45, 60, 80, 100. We set the number of measurement occasions to 70.

<img src="Image_2.png" width=500>

Next, we fill in likely values of the model parameters. We start with the fixed effects: the fixed intercept is set to 43.01, and the effect of the level-2 continuous variable is set to 1.50.  Next, we set the standard deviation and autocorrelation of the within-individual errors, given by 12.62 and 0.46, respectively. The standard deviation of the random intercept is set to 12.90. For the variable depression, we fix the value of the mean to 15.70  and the standard deviation to 5.00. We select the options *Center the level-2 variable W* and *Estimated AR(1) correlated errors*. We set the Type I error to 0.05, and the number of Monte Carlo replicates to 1000. To estimate the multilevel model, we choose the option *Restricted Maximum Likelihood*. Finally, we click on *Compute Power*.

<img src="Image_3.png" width=500>

### Step 1: Visualize the power curve and inspect app output.

The app provides the power curves as a function of the sample sizes indicated above. The next Figure shows the estimated power curve to test the effect of depression on negative affect. 

<img src="Image_5.png">

The app also provides information about the distribution of the estimates of the fixed and random effects across the Monte Carlo replicates. The following figure shows the summary statistics for the fixed effects. 

<img src="Image_6.png">

The app also provides summary statistics of the estimated parameters of the standard deviation and autocorrelation of the level-1 errors, and the estimated parameters of the distribution random effects. 

<img src="Image_7.png">

As well as the distribution of the estimate parameters across the Monte Carlo replicates when the number of participants is 100.

![](Image_8.png)
