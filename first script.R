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
