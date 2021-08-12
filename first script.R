library(tidyverse)
library(dplyr)
library(ggpubr)

formulation <- read.csv("ingredient.csv")
head(formulation)

require(MVN)

#multivariate normality test
mvn(formulation)
mvn(data = formulation, mvnTest = 'royston', univariateTest = 'SW', desc = TRUE)

#histogram for each ingredient
hist(a, freq = FALSE)
lines(density(a))

hist(b, freq = F)
lines(density(b))

hist(c, freq = F)
lines(density(c))

hist(d, freq = F)
lines(density(d))

hist(e, freq = F)
lines(density(e))

hist(f, freq = F)
lines(density(f))

hist(g, freq = F)
lines(density(g))

hist(i, freq = F)
lines(density(i))

hist(h, freq = F)
lines(density(h))

#Cluster analysis
library(cluster)
library(factoextra)

distance <- get_dist(formulation)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(formulation, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = formulation)

k3 <- kmeans(formulation, centers = 3, nstart = 25)
k4 <- kmeans(formulation, centers = 4, nstart = 25)
k5 <- kmeans(formulation, centers = 5, nstart = 25)


p1 <- fviz_cluster(k2, geom = "point", data = formulation) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = formulation) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = formulation) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = formulation) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(123)
fviz_nbclust(formulation, kmeans, method = "wss")
fviz_nbclust(formulation, kmeans, method = "silhouette")
gap_stat <- clusGap(formulation, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

final <- kmeans(formulation, 3, nstart = 25)
print(final)

fviz_cluster(final, data = formulation)

formulation %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
