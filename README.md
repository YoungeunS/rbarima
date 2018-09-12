
## rbarima

Title: rbarima\
Version: 0.0.0.9001\
Author: Youngeun Shin <shinyoungeun1130@gmail.com>\
Description: R shiny forecasting tool for Bass diffusion and ARIMA

## Installation

``` r
library(devtools)
install_github("YoungeunS/rbarima")
```

## Imports

Depends:\
	R (>= 3.4.4),\
	shiny (>= 0.13)
	
``` r
library(shiny);library(readr);library(rbarima);library(dplyr);library(tidyr);library(tseries);library(forecast);library(data.table);library(nlstools);library(minpack.lm);library(propagate);library(ggplot2);library(shinydashboard);library(reshape2);library(rsconnect);library(cowplot);library(grid);library(ggthemes);library(DT);library(stats);library(plotly);library(formattable)
```


## Example code - Facebook MAU data 

``` r
fb <- read_csv("data/Facebook_MAU_2005-2017.csv")
rbarima(fb, Measure = "MAU", Time = c("Year", "Quarter"), Pred.time = 4,launch.browser = TRUE)
```
