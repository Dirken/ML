library(data.table)
load('data/train.rds')

colors <- c('red', 'green', 'blue',
            'purple', 'orange', 'brown',
            'yellow', 'black', 'grey', 'pink',
            'darkred', 'darkgreen', 'darkblue',
            'purple4', 'orange4', 'saddlebrown',
            'yellow4', 'darkgray', 'darkslategrey', 'pink4')
# 20 colors, per a poder pintar up to 20 clusters

xy <- train.data[,c('x0', 'y0', 'x1', 'y1')] # Agafem només les coordenades (x, y)
k <- 10
clust <- kmeans(xy, k)
cl <- clust$cluster

plot(xy[,1:2], col=colors[cl], pch=19, cex=0.5,
     xlim=c(-74.3, -73.6), ylim=c(40.5, 41.1),
     xlab='x', ylab='y',
     main='Clustering')
points(xy[,3:4], col=colors[cl], pch=19, cex=0.5)
legend('topright',
       legend=paste('cluster', 1:k),
       fill=colors[1:k])

plot(factor(cl), train.data$duration,
     xlab='cluster', ylab='duration',
     main='Duration by cluster', outline=FALSE)
