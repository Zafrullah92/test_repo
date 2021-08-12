library(tidyverse)
library(dplyr)
library(ggpubr)

formulation <- read.csv("ingredient.csv")
head(formulation)

require(MVN)

mvn(formulation)

mvn(data = formulation, mvnTest = 'royston', univariateTest = 'SW', desc = TRUE)
