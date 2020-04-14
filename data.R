# Data provided by Johns Hopkins University:
#   github.com/CSSEGISandData/COVID-19

# JSON-formatted by pomber:
#   github.com/pomber/covid19

# Population data provided by samayo:
#   github.com/samayo/country-json

library(rjson)
library(reshape2)

getData <- function(
    statistic = c("deaths", "confirmed", "recovered"),
    normalization = c("population-density", "none", "population")
  ) {
  
  #-------------------------------------------------------------------------------
  #  matching the two datasets country-by-country
  #-------------------------------------------------------------------------------
  
  # get the COVID-19 data from this URL (JSON format)
  data_covid <- rjson::fromJSON(file="https://bit.ly/2UMgpf7")
  
  # get the dates from the first country (repeated in each country)
  dates <- sapply(data_covid[[1]], function(x) x$date)
  
  # get the names of the countries present in the COVID-19 data
  countries_covid <- names(data_covid)
  
  # gather normalization data, if necessary
  norm <- normalization[1]
  if (norm == "population" || norm == "population-density") {
  
    # get country-by-country demographic data (also JSON-formatted)
    awwsmm_github <- "https://raw.githubusercontent.com/awwsmm/country-json/master/src/"
    
    # get the specific normalization file
    file_name <- paste0("country-by-", norm, ".json")
    
    # get the JSON file
    data_norm <- rjson::fromJSON(file=paste0(awwsmm_github, file_name))
  
    # normalization factor by country (0 if unknown)
    norm_factors <- sapply(data_norm, function(x) { p <- x[[2]]; if (is.null(p)) 0 else as.numeric(p) })
    
    # get the names of the countries present in the demographic data
    countries_norm <- sapply(data_norm, function(x) x$country)
  
    # get 1-to-1 matches in country names between datasets
    countries <- intersect(countries_norm, countries_covid)
    
    # map to missing COVID countries from demographic countries where possible
    map <- list()
    
    # manually change demographic data country name <- covid data country name
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
    
    # convert demographic-style country names to covid-style ones
    convert <- function(name) {
      tmp <- map[[name]]
      if (is.null(tmp)) name
      else tmp
    }
    
    # move demographic-style names to covid-style names
    countries_norm <- sapply(countries_norm, convert)
    names(norm_factors) <- countries_norm
    
    # now, to get countries that appear in both lists, do
    countries <- intersect(countries_norm, countries_covid)
    
    # filter populations list to only include these countries
    norm_factors <- norm_factors[countries]
    
    # some countries are missing data in the population density dataset,
    # so we need to filter out those countries, as well
    no_data <- names(norm_factors[norm_factors == 0])
    countries <- setdiff(countries, no_data)
    
    # filter again... these are the countries we'll be able to use for our analysis
    norm_factors <- norm_factors[countries]
    
  # if no normalization, just set countries <- countries_covid
  } else countries <- countries_covid
  
  #-------------------------------------------------------------------------------
  #  combine relevant data into a single dataset
  #-------------------------------------------------------------------------------
  
  # do we want confirmed deaths, confirmed cases, or confirmed recoveries?
  stat <- statistic[1]
  
  # get COVID statistics per country ordered by day, convert to data.frame
  df <- do.call(rbind, lapply(data_covid, function(x) sapply(x, function(y) y[[stat]])))
  
  # set column names to dates
  colnames(df) <- dates
  
  # get cumulative statistics rather than daily statistics
  df_sum <- t(apply(df, 1, cumsum))
  
  # filter to only use countries found above
  df_sum <- df_sum[countries, , drop=FALSE]
  
  # get cumulative statistics normalized to population density
  if (norm == "none") df_sum else (df_sum / norm_factors)

}