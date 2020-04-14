# covid_shiny

A [`shiny`](https://github.com/rstudio/shiny) app for investigating COVID-19 data. Built by [templardrake](https://github.com/templardrake) and [awwsmm](https://github.com/awwsmm).

## Thanks

Thanks to [pomber](https://github.com/pomber/covid19) for maintaining an up-to-date, JSON-formatted version of [Johns Hopkins' COVID-19 data](https://github.com/CSSEGISandData/COVID-19). Thanks to [samayo](https://github.com/samayo) for the demographic data.

## To Do List:

- Temporal Plot:
    - add (optional) projections based on best-fit logistic curve
    - add (optional) confidence intervals on curves
    - add (optional) markers on curves
    - maybe allow the user to select between deaths / recovered / cases

- create "Bhatia mode" (https://www.youtube.com/watch?v=54XLXg4fYsc)
    - (logarithmic) x-axis is cumulative deaths
    - (logarithmic) y-axis is new deaths (past week)
    - slider for time
    - maybe a separate plot because so different
    - maybe a best-fit curve for all data
