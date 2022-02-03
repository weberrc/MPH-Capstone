###########################################
# Histogram of Shootings per year
# Author: Rachel Weber
# Created: April 2, 2020
#########################################


library(anytime)
library(tidyverse)
library(magrittr)
library(pander)
library(ggplot2)
library(MASS)

options(scipen=999)

# read in data
shoot <- read.csv(file = "C:/Users/rache/Documents/Capstone/Data/NYC_extracolumns_0619.csv")

shoot$OCCUR_DATE <- anytime(shoot$OCCUR_DATE)

shoot$year <- as.factor(shoot$year)

colourCount <- length(unique(shoot$year)) 
coul <- brewer.pal(4, "RdYlBu")

g1 <- ggplot(shoot, aes(x = factor(year), fill = year)) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust = -1) +
  ggtitle("Number of Shootings per Year") + theme_minimal() +
  xlab("Year") + ylab("Count") + ylim(0,2500) + guides(fill=FALSE) +
  scale_fill_manual(values = colorRampPalette(coul)(colourCount))


