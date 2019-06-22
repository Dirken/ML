install.packages(c("rgdal", "leaflet", "rgeos", "raster", "httr"))

#libraries:
library(data.table)
library(rgdal)
library(leaflet)
library(rgeos)
library(raster)
library(httr)

#Read train set :
path <- "c:/Users/Meyerhofer/Desktop/UNI/ML"
load(paste0(path, "/data/train.rds"))
train <- train.data[1:100,]

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

#http://geojson.io
#mydf2 <- data.frame(group = c("A", "B"),
#                    lat = c(mydf$InitialLat, mydf$NewLat),
#                    long = c(mydf$InitialLong, mydf$NewLong))

#don't run yet. way too many points i will just group them somehow by radius i guess, bc theya re were too many.
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 9 ) %>%
  addProviderTiles("CartoDB.Positron")  %>%
  addTiles() %>%
  addPolygons(popup = ~neighborhood) %>%
  addLabelOnlyMarkers(lat = c(train$x0, train$y0), 
                    lng = c(train$x1, train$y1), 
                    label = as.character(train.data$duration),
                    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))


####################################### Visualització David #######################################

## Univariate
plot(density(train$distance),
     xlab='Distance (km)',
     main='Distance Density')

plot(density(train$duration, adjust=2),
     xlab='Trip Duration (min)',
     main='Trip Duration Density')

## Bivariate

plot(ordered(round(train$distance)), train$duration,
     pch=19, cex=0.5,
     xlab='Distance', ylab='Duration',
     main='Duration vs Distance', outline=FALSE)

####################################### Visualització David #######################################
