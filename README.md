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
```
