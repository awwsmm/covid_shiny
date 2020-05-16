# covid_shiny

A [`shiny`](https://github.com/rstudio/shiny) app for investigating COVID-19 data.

Built by [templardrake](https://github.com/templardrake) and [awwsmm](https://github.com/awwsmm).

## Thanks

Thanks to [pomber](https://github.com/pomber/covid19) for maintaining an up-to-date, JSON-formatted version of [Johns Hopkins' COVID-19 data](https://github.com/CSSEGISandData/COVID-19).

Thanks to [samayo](https://github.com/samayo) for the demographic data.

## To Do List:

- finish Bhatia page
    - use new modules
    - best-fit curve on Bhatia plot

- add "days since" selection into x-axis title

- fix weird loading of table page

- add warning about forecast

- make accumulation of data optional in data controls box

- add errors functionality; depends on "Plot Line"
    - interp => statistical band
    - logistic curve => +/- 1.96 * se.fit
    - none => statistical bars

- add "postcast"
    - previous forecasts compared to current data

- use animationOptions() with sliderInput() on rewind data

- mirror pomber's JSON file
