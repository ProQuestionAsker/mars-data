library(dplyr)
library(purrr)
library(darksky)

print("Running")

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

safeWeather <- purrr::possibly(getWeather, otherwise = NA)

#todayWeather <- map2_dfr(cityLocations$City, cityLocations$Territory, safeWeather)
todayWeather <- purrr::map2_dfr(testList$City, testList$Territory, safeWeather)

write.csv(todayWeather, "todayWeather.csv", row.names = FALSE)