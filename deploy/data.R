# Data provided by Johns Hopkins University:
#   github.com/CSSEGISandData/COVID-19

# JSON-formatted by pomber:
#   github.com/pomber/covid19

# Population data provided by samayo:
#   github.com/samayo/country-json

library(rjson)
library(reshape2)

getData <- function() {
  
  #-------------------------------------------------------------------------------
  #  matching the two datasets country-by-country
  #-------------------------------------------------------------------------------
  
  # get the COVID-19 data from this URL (JSON format)
  data_covid <- rjson::fromJSON(file="https://bit.ly/2UMgpf7")
  
  # get the dates from the first country (repeated in each country)
  dates <- sapply(data_covid[[1]], function(x) x$date)
  
  # get the names of the countries present in the COVID-19 data
  covid_countries <- names(data_covid)
  
  # get country-by-country population density (also JSON)
  awwsmm <- "https://raw.githubusercontent.com/awwsmm/country-json/master/src/"
  data_dens <- rjson::fromJSON(file=paste0(awwsmm, "country-by-population-density.json"))
  
  # population densities by country (0 if unknown)
  densities <- sapply(data_dens, function(x) { p <- x$density; if (is.null(p)) 0 else as.numeric(p) })
  
  # get the names of the countries present in the population data
  dens_countries <- sapply(data_dens, function(x) x$country)
  
  # get 1-to-1 matches in country names between datasets
  countries <- intersect(dens_countries, covid_countries)
  
  # map to missing COVID countries from dens countries where possible
  map <- list()
  
  # pop data country name <- covid data country name
  map[["Cape Verde"]] <- "Cabo Verde"
  map[["Congo"]] <- "Congo (Brazzaville)"
  map[["The Democratic Republic of Congo"]] <- "Congo (Kinshasa)"
  map[["Ivory Coast"]] <- "Cote d'Ivoire"
  map[["Czech Republic"]] <- "Czechia"
  map[["Swaziland"]] <- "Eswatini"
  map[["Fiji Islands"]] <- "Fiji"
  map[["Holy See (Vatican City State)"]] <- "Holy See"
  map[["South Korea"]] <- "Korea, South"
  map[["Russian Federation"]] <- "Russia"
  map[["SriLanka"]] <- "Sri Lanka"
  map[["Taiwan"]] <- "Taiwan*"
  map[["United States"]] <- "US"
  map[["Palestine"]] <- "West Bank and Gaza"
  map[["Myanmar"]] <- "Burma"
  
  # convert dens-style country names to covid-style ones
  convert <- function(name) {
    tmp <- map[[name]]
    if (is.null(tmp)) name
    else tmp
  }
  
  # move pop-style names to covid-style names
  dens_countries <- sapply(dens_countries, convert)
  names(densities) <- dens_countries
  
  # now, to get countries that appear in both lists, do
  countries <- intersect(dens_countries, covid_countries)
  
  # filter populations list to only include these countries
  densities <- densities[countries]
  
  # some countries are missing data in the population density dataset,
  # so we need to filter out those countries, as well
  no_dens <- names(densities[densities == 0])
  countries <- setdiff(countries, no_dens)
  
  # filter again
  densities <- densities[countries]
  
  # ...these are the ~150 countries we'll be able to use for our analysis
  
  #-------------------------------------------------------------------------------
  #  combine relevant data into a single dataset
  #-------------------------------------------------------------------------------
  
  # get COVID deaths per country ordered by day, convert to data.frame
  #   confirmed and recovered cases also available
  deaths <- do.call(rbind, lapply(data_covid, function(x) sapply(x, function(y) y$deaths)))
  
  # set column names to dates
  colnames(deaths) <- dates
  
  # get cumulative deaths rather than daily deaths
  deaths_cumul <- t(apply(deaths, 1, cumsum))
  
  # filter to only use ~150 countries found above
  deaths_cumul <- deaths_cumul[countries, ]
  
  # get cumulative deaths normalized to population density
  (deaths_cumul / densities)

}