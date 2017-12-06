# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("dplyr", "purrr", "darksky")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

cityLocations <- read.csv("/app/cityLocations.csv", stringsAsFactors = FALSE, header = TRUE)
testList <- read.csv("/app/testList.csv", stringsAsFactors = FALSE, header = TRUE)


getWeather <- function(citySelect, territorySelect){
  coordinates <- cityLocations %>% 
    filter(City == citySelect & Territory == territorySelect)
  
  weather <- get_current_forecast(coordinates$latitude, coordinates$longitude, exclude = c("currently,minutely,hourly,alerts,flags")) 
  
  weather2 <- weather$daily %>% 
    mutate(city = citySelect,
           territory = territorySelect,
           latitude = coordinates$latitude,
           longitude = coordinates$longitude,
           day = row_number()) %>%
    filter(day == 1) %>% 
    select(c('city', 'territory', 'latitude', 'longitude', 'time', 'summary', 'temperatureMin', 'temperatureMax', 'windSpeed', 'pressure'))
}

safeWeather <- possibly(getWeather, otherwise = NA)

#todayWeather <- map2_dfr(cityLocations$City, cityLocations$Territory, safeWeather)
todayWeather <- map2_dfr(testList$City, testList$Territory, safeWeather)

write.csv(todayWeather, "todayWeather.csv", row.names = FALSE)