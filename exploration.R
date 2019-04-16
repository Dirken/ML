
# Load train data
train <- read.csv('train.csv')

##### Pre-processing
# No pre-processing is required

##### Feature Selection / Extraction

# 1) "id" is unique and irrelevant
train$id <- NULL

# 2) "dropoff_datetime" and "trip_duration" 
#   are the variables to predict. Since we
#   will focus on "trip_duration" as a target,
#   we will remove "dropoff_datetime".
train$dropoff_datetime <- NULL

# 3) dates should be converted to integers.
#   Proposal: "pickup_time" can be splitted
#   into Date and Hour:
times <- as.character(train$pickup_datetime)
times <- strsplit(times, ' ')

dates <- sapply(times, function(t) t[1])
dates <- strsplit(dates, '-')
years <- sapply(dates, function(d) as.integer(d[1]))
unique(years)
# All years are "2016", so ths is not required to be encoded
months <- sapply(dates, function(d) as.integer(d[2]))
unique(months)
# Months come from January to June
days <- sapply(dates, function(d) as.integer(d[3]))

daysum <- c(0, 31, 59, 90, 120, 151)
train$day <- daysum[months] + days
# Day is encoded as absolute day since the beggining of the year

hours <- sapply(times, function(t) t[2])
hours <- strsplit(hours, ':')
h <- sapply(hours, function(x) as.integer(x[1]))
m <- sapply(hours, function(x) as.integer(x[2]))
# Proposal:
#   - Seconds are probably not rellevant
#   - Minutes can be rounded to quarters
m <- 15*round(m/15)

train$hour <- h + m/60

train$pickup_datetime <- NULL

# 4) "vendor_id" is binary and should be converted into a factor

train$vendor <- factor(train$vendor_id, labels=c('A', 'B'))
train$vendor_id <- NULL

# 5) Latitudes and longitudes should be plotted
#   to see how much are they grouped together
#   Probably they should be centered
#   (maybe standarized or scaled too)

# There are clearly outliers
plt <- function(outliers) {
  if (missing(outliers)) {
    x0 <- train$pickup_longitude
    y0 <- train$pickup_latitude
    x1 <- train$dropoff_longitude
    y1 <- train$dropoff_latitude
  } else {
    x0 <- train$pickup_longitude[-outliers]
    y0 <- train$pickup_latitude[-outliers]
    x1 <- train$dropoff_longitude[-outliers]
    y1 <- train$dropoff_latitude[-outliers]
  }
  mxx <- max(x0, x1)
  mnx <- min(x0, x1)
  mxy <- max(y0, y1)
  mny <- min(y0, y1)
  
  plot(x0, y0,
       col='blue', pch=19,
       xlim=c(mnx, mxx), ylim=c(mny, mxy),
       xlab='longitude', ylab='latitude',
       main='Pickup/Dropoff Coordinates')
  points(x1, y1, col='orange', pch=19)
  legend('topleft',
         legend=c('Pickup', 'Dropoff'),
         fill=c('blue', 'orange'))
}

plt()
## 1) A single point with coordinates arround (-120, 38)
## 2) A single point with coordinates arround (-73, 55)
outliers <- which(train$pickup_longitude < -100 | train$pickup_latitude > 50)
plt(outliers)

## 1) Some points on the bottom right corner
## 2) Some points over latitude 42
outliers <- union(outliers,
              which((train$dropoff_longitude > -75 & train$dropoff_latitude < 40) | train$dropoff_latitude > 42))
plt(outliers)

## 1) Some points on the bottom left corner
## 2) Some points over latitude 42
outliers <- union(outliers,
              which(train$dropoff_longitude < -75 | train$dropoff_longitude < -75 | train$pickup_latitude < 39))
plt(outliers)

# We can consider removing arround 100 points (0.006%)

X <- cbind(train$pickup_longitude, train$pickup_latitude)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.100.pick <- sort(dist, decreasing=T, index.return=T)$ix[1:100]

X <- cbind(train$dropoff_longitude, train$dropoff_latitude)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.100.drop <- sort(dist, decreasing=T, index.return=T)$ix[1:100]

out.100 <- union(out.100.pick, out.100.drop)
plt(out.100)

train <- train[-out.100,]

# Re-arrange and re-name variables of the dataset
names(train) <- c('passengers', 'x0', 'y0', 'x1', 'y1', 'store_flag', 'duration', 'day', 'hour', 'vendor')
train <- train[,c('day', 'hour', 'passengers', 'x0', 'y0', 'x1', 'y1', 'vendor', 'store_flag', 'duration')]

## Center and scale coordinate variables
X0 = cbind(train$x0, train$y0)
X1 = cbind(train$x1, train$y1)
X <- rbind(X0, X1)
M <- colMeans(X)
S <- apply(X, 2, sd)

# Mu: (-74.00, 40.75)
# Sigma: (0.04, 0.03)
train$x0 <- (train$x0 + 74)/0.04
train$y0 <- (train$y0 - 40.75)/0.03
train$x1 <- (train$x1 + 74)/0.04
train$y1 <- (train$y1 - 40.75)/0.03

save(train, file='train.rds')

load('train.rds')

########## MORE IDEAS TO TRY #####################
# 1) Codify (x0, y0) in polar form               #
# 2) Codify (x1, y1) in polar form               #
# 3) Center hour arround 12 and scale to [-1, 1] #
# 3) Center and scale day                        #
# 4) Add new features:                           #
#     - Euclidean distance                       #
#     - Manhattan distance                       #
########## MORE IDEAS TO TRY #####################

##### Visualization


##### Clustering

