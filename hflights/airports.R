
# pkgs <- c('purrr', 'readr'. 'ggplot2', 'svglite')
# install.packages(pkgs)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(svglite)


###
# Check if the data file is present
# if not download it.
###
if (!file.exists("data-raw/airports.dat")) {
  download.file(
    "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
    "data-raw/airports.dat"
  )
}

###
# Read the data file into R
### 
raw <- read_csv("data-raw/airports.dat",
                col_names = c("id", "name", "city", "country", "faa", "icao", "lat", "lon", "alt", "tz", "dst", "tzone")
)

###
# List of unique countries in the dataset
###
unique(raw$country)

###
# List of airports in India
###
airports <- raw %>%
  filter(country == "India", faa != "") %>%
  select(faa, name, lat, lon, alt, tz, dst, tzone) %>%
  group_by(faa) %>% slice(1) %>% ungroup() # take first if duplicated

###
# Set all the timezone to Asia/Calcutta
###
airports$tzone <- 'Asia/Calcutta'


###
# Visualization of teh airport data
###
airports %>%
  filter(lon > 0) %>%
  ggplot(aes(lon, lat)) +
  geom_point(aes(colour = factor(tzone)), show.legend = TRUE) +
  coord_quickmap()  + theme_void()

###
# Save the ggplot as svg format
###
ggsave("data-raw/airports.svg", width = 8, height = 6)

###
# Save the dataset in CSV format
###
write_csv(airports, "data-raw/airports.csv")
