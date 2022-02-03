#############################
# Change Point Analysis
# Rachel Weber
# Created: 10/23/2019
# last edited: 3/28/2020
#############################

library(anytime)
library(tidyverse)
library(magrittr)
library(pander)
library(ggplot2)
library(MASS)
library(metafor)

options(scipen=999)

# read in data
shoot <- read.csv(file = "C:/Users/rache/Documents/Capstone/Data/Historic Data 2006_2019.csv", 
                  sep = ",", na.strings=c("","NA"))

shoot$PRECINCT <- as.factor(shoot$PRECINCT)
shoot$OCCUR_DATE <- anytime(shoot$OCCUR_DATE)
shoot$month <- as.numeric(format(shoot$OCCUR_DATE, "%m"))
shoot$day <- as.numeric(format(shoot$OCCUR_DATE, "%d"))
shoot %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

all_dates <- shoot %>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE)) %>% 
  complete(OCCUR_DATE = seq.Date(min(OCCUR_DATE), max(OCCUR_DATE), by = "day"))

all_dates$weekday <- weekdays(all_dates$OCCUR_DATE)

# getSeason <- function(DATES) {
#   WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
#   SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
#   SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
#   FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
#   
#   d <- as.Date(strftime(DATES, format="2012-%m-%d"))
#   
#   ifelse (d >= WS | d < SE, "Winter",
#           ifelse (d >= SE & d < SS, "Spring",
#                   ifelse (d >= SS & d < FE, "Summer", "Fall")))
# }

# all_dates$season <- as.factor(getSeason(all_dates$OCCUR_DATE))
all_dates$month <- as.numeric(format(all_dates$OCCUR_DATE, "%m"))
all_dates$day <- as.numeric(format(all_dates$OCCUR_DATE, "%d"))
all_dates %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

sum_shoot <- all_dates %>% 
  group_by(year, month, day) %>%
  summarise(number = n()) 


# join other columns
test <- left_join(sum_shoot, all_dates[,c(5,19:22)])
sum_shoot <- test[!duplicated(test),]

# make zero days zero
sum_shoot[is.na(sum_shoot$PRECINCT),]$number <- 0

# remove precinct then remove duplicates
sum_shoot <- sum_shoot[-c(5)]
sum_shoot <- sum_shoot[!duplicated(sum_shoot),]

# add number for each month and day
sum_shoot <- sum_shoot %>% 
  mutate(md = row_number())

# how many days were there no shootings?
nrow(sum_shoot[!is.na(sum_shoot$number) & sum_shoot$number == 0,])

# what was the average number of shootings per day?
mean(sum_shoot$number)

median(sum_shoot$number)

############################################# Univariate Analyses -- Day Level ###############################
# m1 <- glm.nb(number ~ season, data = sum_shoot)
# summary(m1)
  # season is a significant predictor

m2 <- glm.nb(number ~ weekday, data = sum_shoot)
summary(m2)
  # day is a significant predictor

m3 <- glm.nb(number ~ year, data = sum_shoot)
summary(m3)
  # year is a significant predictor

m4 <- glm.nb(number ~ weekday + year, data = sum_shoot)

library(car)
vif(m4)
  # no vif above a rounded 1. We're good to use all in model

sum_shoot$md <-1:nrow(sum_shoot)

# January 15th 2013 is row 2572
prepost <- (sum_shoot$md > 2572) * 1
m11513 <- sum_shoot$md - 2572
sum_shoot$prepost <- prepost
sum_shoot$m11513 <- m11513
sum_shoot$m11513 <- sum_shoot$m11513 * sum_shoot$prepost

# change my time counters to measures at the yearly level
sum_shoot$md <- sum_shoot$md/365.25
sum_shoot$m11513 <- sum_shoot$m11513/365.25

nbm <- glm.nb(number ~ md + weekday + prepost + m11513 + as.factor(month), data = sum_shoot)
summary(nbm)

pval <- exp(summary(nbm)$coefficients[,1])
pval <- cbind(pval, round(coef(summary(nbm))[,4], 4))
pval <- cbind(pval, exp(summary(nbm)$coefficients[,1] + qnorm(c(0.025)) * summary(nbm)$coefficients[,2]))
pval <- cbind(pval, exp(summary(nbm)$coefficients[,1] + qnorm(c(0.975)) * summary(nbm)$coefficients[,2]))
pval <- as.data.frame(pval)
colnames(pval) <- c("Estimate", "P-Value", "LowerCI", "UpperCI")
pval$'95% CI' <- paste("(",round(pval$LowerCI, 2),", ",round(pval$UpperCI, 2),")", sep="")
pval <- pval[-c(3, 4)]


##################### make a forest plot -- Day Level #################################
pval <- exp(summary(nbm)$coefficients[,1])
pval <- cbind(pval, round(coef(summary(nbm))[,4], 4))
pval <- cbind(pval, exp(summary(nbm)$coefficients[,1] + qnorm(c(0.025)) * summary(nbm)$coefficients[,2]))
pval <- cbind(pval, exp(summary(nbm)$coefficients[,1] + qnorm(c(0.975)) * summary(nbm)$coefficients[,2]))
pval <- as.data.frame(pval)
rownames(pval) <- c("Intercept", "Baseline Slope", "Day (Mon)", "Day (Sat)", "Day (Sun)", 
                    "Day (Thurs)", "Day (Tues)", "Day (Wed)", "Pre/Post SAFE Act", "Slope Change",
                    "Month (Feb)", "Month (March)", "Month (April)", "Month (May)", "Month (June)", "Month (July)",
                    "Month (Aug)", "Month (Sept)", "Month (Oct)", "Month (Nov)", "Month (Dec)")
pval <- rownames_to_column(pval, "variable")

pval <- pval[-1,]

pval$variable <- factor(c("Baseline Slope", "Day (Mon)", "Day (Sat)", "Day (Sun)", 
                          "Day (Thurs)", "Day (Tues)", "Day (Wed)", "Pre/Post SAFE Act", "Slope Change",
                          "Month (Feb)", "Month (March)", "Month (April)", "Month (May)", "Month (June)", "Month (July)",
                          "Month (Aug)", "Month (Sept)", "Month (Oct)", "Month (Nov)", "Month (Dec)"),
                        levels = c("Month (Dec)", "Month (Nov)", "Month (Oct)", "Month (Sept)", "Month (Aug)",
                                   "Month (July)", "Month (June)", "Month (May)", "Month (April)", "Month (March)",
                                   "Month (Feb)", "Day (Sun)",  "Day (Sat)", "Day (Thurs)","Day (Wed)",
                                   "Day (Tues)", "Day (Mon)",  "Slope Change", "Pre/Post SAFE Act", "Baseline Slope"
                                   ))


ggplot(pval, aes(y = variable, x = pval, label = variable)) +
  geom_point(size = 3, shape = 19) +
  geom_errorbarh(aes(xmin = V3, xmax = V4), height = .3) +
  theme_minimal() +
  xlab("Estimated Change") +
  ylab("Variable") +
  geom_vline(xintercept = 1, col = "goldenrod4") +
  annotate("text", x = 0.8, y = 20, label= "Decrease", col = "goldenrod4") +
  annotate("text", x = 1.2, y = 20, label= "Increase", col = "goldenrod4")

forestplot::forestplot(pval$variable, 
                       mean = pval$pval, 
                       lower = pval$V3, 
                       upper = pval$V4,
                       zero = 1,
                       xticks = c(.8, 1, 1.2, 1.4, 1.6, 1.8, 2),
                       )

######################### Make a month level model ###################################

sum_shoot_month <- all_dates %>% 
  group_by(year, month) %>%
  summarise(number = n()) 

sum_shoot_month$md <-1:nrow(sum_shoot_month)

# January 2013 is row 85
prepost <- (sum_shoot_month$md > 84) * 1
m113 <- sum_shoot_month$md - 84
sum_shoot_month$prepost <- prepost
sum_shoot_month$m113 <- m113
sum_shoot_month$m113 <- sum_shoot_month$m113 * sum_shoot_month$prepost


nbm2 <- glm.nb(number ~ md + prepost + m113 + as.factor(month), data = sum_shoot_month)
summary(nbm2)

pval2 <- exp(summary(nbm2)$coefficients[,1])
pval2 <- cbind(pval2, round(coef(summary(nbm2))[,4], 4))
pval2 <- cbind(pval2, exp(summary(nbm2)$coefficients[,1] + qnorm(c(0.025)) * summary(nbm2)$coefficients[,2]))
pval2 <- cbind(pval2, exp(summary(nbm2)$coefficients[,1] + qnorm(c(0.975)) * summary(nbm2)$coefficients[,2]))
pval2 <- as.data.frame(pval2)
colnames(pval2) <- c("Estimate", "P-Value", "LowerCI", "UpperCI")
pval2$'95% CI' <- paste("(",round(pval2$LowerCI, 2),", ",round(pval2$UpperCI, 2),")", sep="")
pval2 <- pval2[-c(3, 4)]


pval2 <- exp(summary(nbm2)$coefficients[,1])
pval2 <- cbind(pval2, round(coef(summary(nbm2))[,4], 4))
pval2 <- cbind(pval2, exp(summary(nbm2)$coefficients[,1] + qnorm(c(0.025)) * summary(nbm2)$coefficients[,2]))
pval2 <- cbind(pval2, exp(summary(nbm2)$coefficients[,1] + qnorm(c(0.975)) * summary(nbm2)$coefficients[,2]))
pval2 <- as.data.frame(pval2)
rownames(pval2) <- c("Intercept", "Baseline Slope", "Pre/Post SAFE Act", "Slope Change",
                    "Month (Feb)", "Month (March)", "Month (April)", "Month (May)", "Month (June)", "Month (July)",
                    "Month (Aug)", "Month (Sept)", "Month (Oct)", "Month (Nov)", "Month (Dec)")
pval2 <- rownames_to_column(pval2, "variable")

pval2 <- pval2[-1,]

ggplot(pval2, aes(y = variable, x = pval2, label = variable)) +
  geom_point(size = 3, shape = 19) +
  geom_errorbarh(aes(xmin = V3, xmax = V4), height = .3) +
  theme_minimal() +
  xlab("Estimated Change") +
  ylab("Variable") +
  geom_vline(xintercept = 1, col = "goldenrod4") +
  annotate("text", x = 0.8, y = 1, label= "Decrease", col = "goldenrod4") +
  annotate("text", x = 1.2, y = 1, label= "Increase", col = "goldenrod4")
