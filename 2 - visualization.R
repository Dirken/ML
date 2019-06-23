install.packages(c("rgdal","leaflet","rgeos","raster","httr","dbscan","fpc","ggplot"))




#libraries:
library(data.table)
library(rgdal)
library(leaflet)
library(rgeos)
library(raster)
library(httr)
library(dbscan)
library(fpc)
library(ggplot)
library(tidyverse)
library(ggplot2)

#downloading some geometries of neighbourhoods
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

#Read train set :
path <- "c:/Users/Meyerhofer/Desktop/UNI/ML"
#load(paste0(path, "/data/train.rds"))
#train <- train.data[1:100,]
train <- read.csv(paste0(path, "/data/train.csv"))


#Pickup and Dropoff locations

P3 <- ggplot() + geom_point(aes(x = pickup_longitude, y = pickup_latitude), 
                            data = train, color = "dark green", size = 0.1, alpha =0.1) + xlim(-74.1,-73.7)+ ylim(40.55,40.9) + ggtitle("Pickup Locations")

P4 <- ggplot() + geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), 
                            data = train, color = "red", size = 0.1, alpha =0.1) + xlim(-74.1,-73.7)+ ylim(40.55,40.9) + ggtitle("Dropoff Locations")



p1 <- train %>%
  filter(pickup_longitude > -74.05 & pickup_longitude < -73.7) %>%
  ggplot(aes(pickup_longitude)) +
  geom_histogram(fill = "red", bins = 40)

p2 <- train %>%
  filter(dropoff_longitude > -74.05 & dropoff_longitude < -73.7) %>%
  ggplot(aes(dropoff_longitude)) +
  geom_histogram(fill = "blue", bins = 40)

p3 <- train %>%
  filter(pickup_latitude > 40.6 & pickup_latitude < 40.9) %>%
  ggplot(aes(pickup_latitude)) +
  geom_histogram(fill = "red", bins = 40)

p4 <- train %>%
  filter(dropoff_latitude > 40.6 & dropoff_latitude < 40.9) %>%
  ggplot(aes(dropoff_latitude)) +
  geom_histogram(fill = "blue", bins = 40)
)
set.seed(123)
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7940
#https://cran.r-project.org/web/packages/dbscan/vignettes/dbscan.pdf
##########################################
#Clustering.
##########################################
#since our data is way too much to be represented in a easy manner and not collapsing RStudio,
#if we gather points to some clusters, we might be able to see how trends are.
#kmeans
kmeans <- kmeans(train[,6:9], 10000)
kmeans2000 <- kmeans(train[,6:9], 2000)
kmeans1000 <- kmeans(train[,6:9], 1000)
kmeans500 <- kmeans(train[,6:9], 500)
kmeans200 <- kmeans(train[,6:9], 200)
kmeans50 <- kmeans(train[,6:9], 50)
kmeans25 <- kmeans(train[,6:9], 25)
kmeans5 <- kmeans(train[,6:9], 5)



#optics

##########################################
# Visualization k-means k = 5
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans5$centers[,2], 
    lng = kmeans5$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) %>%
  addCircleMarkers(
    lat = kmeans5$centers[,4], 
    lng = kmeans5$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) 
##########################################
# Visualization k-means k = 25
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans25$centers[,2], 
    lng = kmeans25$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) %>%
  addCircleMarkers(
    lat = kmeans25$centers[,4], 
    lng = kmeans25$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) 


##########################################
# Visualization k-means k = 50
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans50$centers[,2], 
    lng = kmeans50$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) %>%
  addCircleMarkers(
    lat = kmeans50$centers[,4], 
    lng = kmeans50$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) 

##########################################
# Visualization k-means k = 200
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans200$centers[,2], 
    lng = kmeans200$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 1
  ) %>%
  addCircleMarkers(
    lat = kmeans200$centers[,4], 
    lng = kmeans200$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 1
  ) 

##########################################
# Visualization k-means k = 500
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans500$centers[,2], 
    lng = kmeans500$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 1
  ) %>%
  addCircleMarkers(
    lat = kmeans500$centers[,4], 
    lng = kmeans500$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 1
  ) 


##########################################
# Visualization k-means k = 1000
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans1000$centers[,2], 
    lng = kmeans1000$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 1
  ) %>%
  addCircleMarkers(
    lat = kmeans1000$centers[,4], 
    lng = kmeans1000$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 1
  ) 

##########################################
# Visualization k-means k = 2000
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans2000$centers[,2], 
    lng = kmeans2000$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 2
  ) %>%
  addCircleMarkers(
    lat = kmeans2000$centers[,4], 
    lng = kmeans2000$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 2
  ) 


##########################################
# Visualization k-means k = 10000
##########################################
leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = kmeans$centers[,2], 
    lng = kmeans$centers[,1],
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 2
  ) %>%
  addCircleMarkers(
    lat = kmeans$centers[,4], 
    lng = kmeans$centers[,3],
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 2
  ) 


##########################################
# Grouping data
##########################################

uniqueTrain <- train[1:100,]
groupedData <- data.frame(group = c("A", "B"),
                          lat = c(uniqueTrain$pickup_latitude, uniqueTrain$dropoff_latitude),
                          long = c(uniqueTrain$pickup_longitude, uniqueTrain$dropoff_longitude))

leaflet(nyc_neighborhoods) %>%
  setView(lat= 40.71 ,lng= -74.00, zoom = 10 ) %>% 
  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.35)) %>%
  addTiles() %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "#444444") %>%
  #addPolygons(nypopup = ~neighborhood) %>%%
  addCircleMarkers(
    lat = uniqueTrain$pickup_latitude, 
    lng = uniqueTrain$pickup_longitude,
    color = "red",
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) %>%
  addCircleMarkers(
    lat = uniqueTrain$dropoff_latitude, 
    lng = uniqueTrain$dropoff_longitude,
    color = "green", #pal()
    stroke = T, 
    fillOpacity = 0.5,
    radius = 3
  ) %>%
  addPolylines(data= groupedData, 
               lng = groupedData$long, 
               lat = groupedData$lat, 
               group = groupedData$group,
               color = "yellow",
               weight = 1
  )


######Optics

res <- optics(uniqueTrain[,6:9], eps = 6,  minPts = 10)

res <- extractDBSCAN(res, eps_cl = .065)
hullplot(uniqueTrain[,6:9], res)

res <- extractXi(res, xi = 0.05)
plot(res)
hullplot(uniqueTrain[,6:9], res)
##########################################
# Visualization
##########################################



##############################################################################
load(paste0(path, "/data/train.rds"))
train <- train.data
## Univariate
plot(density(train$distance),
     xlab='Distance (km)',
     main='Distance Density')

plot(density(train$duration, adjust=2),
     xlab='Trip Duration (min)',
     main='Trip Duration Density')

train %>%
  ggplot(aes(distance))+ 
  geom_density(fill = "blue", alpha = 0.5) 

train %>%
  ggplot(aes(duration))+ 
    geom_density(fill = "blue", alpha = 0.5) 
## Bivariate

plot(ordered(round(train$distance)), train$duration,
     pch=19, cex=0.5,
     xlab='Distance', ylab='Duration',
     main='Duration vs Distance', outline=FALSE)


train %>%
  ggplot(aes(ordered(round(train$distance)), train$duration, color="blue")) +
  geom_boxplot() +
  labs(x = "Distance", y="Duration", pch=19) +
  theme(legend.position = "none")


##############################################################################
