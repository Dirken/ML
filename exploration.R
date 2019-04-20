
# Load train data
train <- read.csv('train.csv')

##### Pre-processing + Feature Selection / Extraction

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
train$month <- factor(months, ordered=TRUE)
levels(train$month) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')

days <- sapply(dates, function(d) as.integer(d[3]))

daysum <- c(0, 31, 59, 90, 120, 151)
train$day <- days
train$day_of_year <- daysum[months] + days
train$day_of_week <- factor((days + 4) %% 7)
levels(train$day_of_week) <- c('Mon', 'Tue', 'Wen', 'Thu', 'Fri', 'Sat', 'Sun')
# day_of_year is encoded as absolute day since the beggining of the year
# Day 1 -> Friday

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

train$vendor <- factor(train$vendor_id, labels=c('Vendor A', 'Vendor B'))
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

# plt()

# We can consider removing arround 1000 points (0.06%)

X <- cbind(train$pickup_longitude, train$pickup_latitude)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.500.pick <- sort(dist, decreasing=T, index.return=T)$ix[1:500]

X <- cbind(train$dropoff_longitude, train$dropoff_latitude)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.500.drop <- sort(dist, decreasing=T, index.return=T)$ix[1:500]

out.500 <- union(out.500.pick, out.500.drop)
length(out.500) # Finally 837 points

# plt(out.500)

train <- train[-out.500,]

# 6) There are also outliers in the duration

100*sum(train$trip_duration > 3600*2)/nrow(train)
# 0.1% has more than 2h trip!
100*sum(train$trip_duration > 3600)/nrow(train)
# 0.8% has more than 1h trip

train <- train[train$trip_duration < 3600,]


# 7) Keep passengers as an ordered factor

# Re-arrange and re-name variables of the dataset
names(train) <- c('passengers', 'x0', 'y0', 'x1', 'y1', 'store_flag', 'duration',
                  'month', 'day', 'day_of_year', 'day_of_week', 'hour', 'vendor')
train <- train[,c('day_of_year', 'month', 'day', 'day_of_week', 'hour',
                  'passengers', 'x0', 'y0', 'x1', 'y1', 'vendor', 'store_flag', 'duration')]

vec <- cbind(train$x1 - train$x0, train$y1 - train$y0)
train$eucl <- apply(vec, 1, function(v) norm(as.matrix(v), 'f'))
train$manh <- apply(vec, 1, function(v) norm(as.matrix(v), '1'))

train$x <- (train$x0 + train$x1)/2
train$y <- (train$y0 + train$y1)/2

train$x0 <- NULL
train$x1 <- NULL
train$y0 <- NULL
train$y1 <- NULL

train <- train[,c('day_of_year', 'month', 'day', 'day_of_week', 'hour',
                  'passengers', 'x', 'y', 'eucl', 'manh', 'vendor', 'store_flag', 'duration')]

save(train, file='processed.rds')

load('processed.rds')

## Center and scale coordinate variables
#   In order to keep distances coherent with 
#   coordinates, they are scaled by the same value
s <- (sd(train$x) + sd(train$y))/2
train$x <- (train$x - mean(train$x))/s
train$y <- (train$y - mean(train$y))/s
train$eucl <- train$eucl/s
train$manh <- train$manh/s

save(train, file='processed_std.rds')

load('processed_std.rds')

##### Visualization

## Univariate
barplot(table(train$month),
        xlab='Months', ylab='Frequency',
        main='Month frequency')

barplot(table(train$day),
        xlab='Days', ylab='Frequency',
        main='Day frequency')
barplot(table(train$day_of_year),
        xlab='Days', ylab='Frequency',
        main='Day frequency')
barplot(table(train$day_of_week),
        xlab='Days', ylab='Frequency',
        main='Day frequency')
# Notice higher frequency on weekends
# Notice also that there is an outlier
which.max(table(train$day_of_year)) # March 1st

hours.3 <- c(train$hour - 24, train$hour, train$hour + 24)
plot(density(hours.3, adjust=1/2),
     xlim=c(0, 24),
     xlab='Hour', main='Hour density')
# Higher density between 8AM and 22 PM
# Lower density between 22PM and 8AM

barplot(table(train$passengers),
        xlab='# Passengers',
        ylab='Frequency')
# Most common is 1 passenger, by far

plot(train$x, train$y,
     col='blue', pch=19,
     xlab='longitude', ylab='latitude',
     main='Coordinates')

plot(density(train$eucl),
     xlab='Euclidean distance',
     main='Euclidean Distance Density')

plot(density(train$manh),
     xlab='Manhattan distance',
     main='Manhattan Distance Density')

plot(train$vendor, main='Vendor')
plot(train$store_flag, main='Store Flag')

plot(density(train$duration),
     xlab='Trip Duration',
     main='Trip Duration Density')

## Bivariate (duration vs rest)

plot(duration ~ factor(day_of_year), data=train,
     pch=19,
     xlab='Day', ylab='Duration',
     main='Duration vs Day of year')

plot(duration ~ month, data=train,
     pch=19,
     xlab='Month', ylab='Duration',
     main='Duration vs Month')

plot(duration ~ factor(day), data=train,
     pch=19,
     xlab='Day', ylab='Duration',
     main='Duration vs Day')

plot(duration ~ day_of_week, data=train,
     pch=19,
     xlab='Day', ylab='Duration',
     main='Duration vs Day of week')

plot(duration ~ factor(hour), data=train,
     pch=19,
     xlab='Hour', ylab='Duration',
     main='Duration vs Hour')

plot(duration ~ factor(passengers), data=train,
     pch=19,
     xlab='Passengers', ylab='Duration',
     main='Duration vs Passengers')

plot(duration ~ eucl, data=train,
     pch=19,
     xlab='Euclidean Distance', ylab='Duration',
     main='Duration vs Euclidean Distance')

plot(duration ~ manh, data=train,
     pch=19,
     xlab='Manhattan Distance', ylab='Duration',
     main='Duration vs Manhattan Distance')

plot(duration ~ vendor, data=train,
     pch=19,
     xlab='Vendor', ylab='Duration',
     main='Duration vs Vendor')

plot(duration ~ store_flag, data=train,
     pch=19,
     xlab='Store flag', ylab='Duration',
     main='Duration vs Store flag')

data.num <- train
data.num$month <- as.integer(train$month)
data.num$day_of_week <- as.integer(train$day_of_week)
data.num$vendor <- as.integer(train$vendor)
data.num$store_flag <- as.integer(train$store_flag)

save(data.num, file='processed_num.rds')

# PCA

load('processed_num.rds')

mat <- scale(data.num)
PC <- prcomp(mat)
FactoMineR::PCA(mat)

# Duration is higly correlated with distances and x-y coordinates.
# Most of the other features seem to be quite indepenedent.
# let's try without this geographical data

mat <- data.num
mat$x <- NULL
mat$y <- NULL
mat$manh <- NULL
mat$eucl <- NULL
mat <- scale(mat)

PC <- prcomp(mat)
FactoMineR::PCA(mat)

# Duration is very little correlated with the rest of variables

# Now let's see correlation with geographical data

mat <- data.num
mat$day_of_year <- NULL
mat$month <- NULL
mat$day <- NULL
mat$day_of_week <- NULL
mat$hour <- NULL
mat$passengers <- NULL
mat$vendor <- NULL
mat$store_flag <- NULL
mat <- scale(mat)

PC <- prcomp(mat)
FactoMineR::PCA(mat)

# Finally, let's see correlation between all variables except duration
# And correlation between PCA and duration.

mat <- data.num
mat$duration <- NULL
mat <- scale(mat)

PC <- prcomp(mat)
# First PC is geographical
# Second PC is temporal
# Third PC is # passangers, vendor and store_flag

FactoMineR::PCA(mat)

plot(PC$x[,1], data.num$duration,
     xlab='PC1', ylab='Duration',
     main='PC1 (Geographical data) vs Duration')
plot(PC$x[,2], data.num$duration,
     xlab='PC2', ylab='Duration',
     main='PC2 (Temporal data) vs Duration')
plot(PC$x[,3], data.num$duration,
     xlab='PC3', ylab='Duration',
     main='PC3 (Others) vs Duration')

# Final conclusion: Use only geographical data.
# The rest seems to be irrelevant or independent

################################# Final data #################################
########################### Only geographical data ###########################

# NOTE: Data comes without standarization.
# If required, it should be done by the
# learning algoritms

data <- read.csv('train.csv')
data <- data[,c('pickup_longitude', 'pickup_latitude',
                'dropoff_longitude', 'dropoff_latitude',
                'trip_duration')]

names(data) <- c('x0', 'y0', 'x1', 'y1', 'duration')

X <- cbind(data$x0, data$y0)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.500.pick <- sort(dist, decreasing=T, index.return=T)$ix[1:500]

X <- cbind(data$x1, data$y1)
M <- colMeans(X)
dist <- apply(X, 1, function(x) crossprod(x - M))
out.500.drop <- sort(dist, decreasing=T, index.return=T)$ix[1:500]

data <- data[-union(out.500.pick, out.500.drop),]

data <- data[data$duration < 3600,]

vec <- cbind(data$x1 - data$x0, data$y1 - data$y0)
data$eucl <- apply(vec, 1, function(v) norm(as.matrix(v), 'f'))
data$manh <- apply(vec, 1, function(v) norm(as.matrix(v), '1'))

data <- data[,c('x0', # pickup longitude
                'y0', # pickup latitude
                'x1', # dropoff longitude
                'y1', # dropoff latitude
                'eucl', # euclidean distance between (x0, y0) and (x1, y1)
                'manh', # manhattan distance between (x0, y0) and (x1, y1)
                'duration' # target variable
                )]

# NOTE: If the origin/destination district could be extracted,
#       it should be added to these features

save(data, file='processed.rds')
