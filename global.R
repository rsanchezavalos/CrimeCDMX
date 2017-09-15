rm(list = ls())

# Install 
install <- function(paquete) {
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c('rgdal','ggplot2','ggmap','downloader','knitr','tidyverse', 'leaflet',
              'RColorBrewer', 'scales', 'lattice')# usage
ipak(packages)


# Get Raw
print('Downloading Raw Data')

# Lat - long
lat_long = tempfile("crime-lat-long", fileext = ".csv")
download("https://s3.amazonaws.com/utils-rsa/crime-lat-long.csv", lat_long)
# lat_long <- 'data/crime-lat-long.csv'
data_lat_long <- read_csv(lat_long) %>% filter(!is.na(lat) & !is.na(long)) %>% 
  rename(latitude=lat, longitude=long) %>% add_rownames('id')

v_cuadrante = tempfile("v_cuadrante", fileext = ".csv")
download('https://s3.amazonaws.com/utils-rsa/cuadrantes-hoyodecrimen.csv', v_cuadrante)
# v_cuadrante <- './data/cuadrantes-hoyodecrimen.csv'
violencia_cuadrantes <- read_csv(v_cuadrante) 

# Get Geoms
print('Getting Geoms')
# tempory file to save Cuadrantes
tmp_cuadrantes = tempfile("cuads", fileext = ".json")
download("https://s3.amazonaws.com/utils-rsa/cuadrantes_geom", tmp_cuadrantes)
# tmp_cuadrantes <- './data/cuadrantes_geom'
cuadrantes <- readOGR(tmp_cuadrantes, "OGRGeoJSON", verbose = FALSE)
# proj4string(cuadrantes) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Set ID for map queries
print('Preprocess')
cuadrantes$id <- cuadrantes$cuadrantes 
poblacion <- select(violencia_cuadrantes, cuadrante, population) %>% unique()
data_cuadrante <- data_lat_long %>% group_by(cuadrante, date, crime, year, month) %>% summarise(count_n = n())  %>% 
  left_join(as.tibble(cuadrantes))  %>% left_join(poblacion, by = 'cuadrante') %>% add_rownames('id')
