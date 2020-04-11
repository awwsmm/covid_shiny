# covid_shiny
An R Shiny app for investigating COVID-19 mortality data.

## to do:

- clean up repo, move tutorial stuff elsewhere

- add (optional) projections based on best-fit logistic curve
- add (optional) confidence intervals on curves
- add (optional) markers on curves
- make normalization optional, add option to normalize based on population
- maybe allow the user to select between deaths / recovered / cases

- create "Bhatia mode" (https://www.youtube.com/watch?v=54XLXg4fYsc)
    - (logarithmic) x-axis is cumulative deaths
    - (logarithmic) y-axis is new deaths (past week)
    - slider for time
    - maybe a separate plot because so different
    - maybe a best-fit curve for all data
