# mortr
Basic plotting of total mortality data

## Usage (time series)
```
Rscript --vanilla wmd-plot.R SWE FIN DNK NOR ISL GBR USA
```

The above command requires `R` with `tidyverse` installed. It automatically downloads a data file from another `github` repository (https://github.com/akarlinsky/world_mortality) and generates PNG and PDF plots for the requested countries. To see the available countries and their three letter codes run
```
Rscript --vanilla wmd-plot.R list
```

## Usage (overall excess)
To generate a basic plot of normalized excess mortality for all countries do
```
Rscript --vanilla wmd-plot.R excess
```

Present version of basic excess plot:
![excess-summary](excess-summary.png) 
