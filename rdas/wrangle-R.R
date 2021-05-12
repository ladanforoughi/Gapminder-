library(dslabs)
library(tidyverse)
library(ggplot2)
library(dplyr)
data("gapminder")
#gapminder <- read_csv("gapminder.csv")
# 1. Is it a fair characterization of today’s world 
#to say it is divided into western rich nations and 
# the developing world in Africa, Asia and Latin America?
# 2. Has income inequality across countries worsened
# during the last 40 years?

# Q1
#  For each of the six pairs of countries below, 
#which country do you think had the highest child
#mortality rates in 2015? 
#Which pairs do you think are most similar?
# 1. Sri Lanka or Turkey
# 2. Poland or South Korea
#3. Malaysia or Russia
# 4. Pakistan or Vietnam
# 5. Thailand or South Africa

set1 <- gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>%
        select (country,infant_mortality)
ste2 <- gapminder %>% filter(year == 2015 & country %in% c("Poland", "South Korea")) %>%
        select (country,infant_mortality)
set3 <- gapminder %>% filter(year == 2015 & country %in% c("Malaysia","Russia")) %>%
        select (country,infant_mortality)
set4 <- gapminder %>% filter(year == 2015 & country %in% c("Pakistan","Vietnam")) %>%
        select (country,infant_mortality)
set5 <- gapminder %>% filter(year == 2015 & country %in% c("Thailand","South Africa")) %>%
        select (country,infant_mortality)

# Q2
# variation life expectancy and child mortality at year 1962
gapminder %>% filter(year == 1962) %>% 
  ggplot(aes(fertility,life_expectancy)) + geom_point()
ggsave("fig/fertility-vs-lifeexpectancy.png")


# Q3 
# variation life expectancy and child mortality at 
# year 1962 based on continet
gapminder %>% filter(year == 1962) %>% 
  ggplot(aes(fertility,life_expectancy, color= continent)) + geom_point()
ggsave("fig/fertility-vs-lifeexpectancy-continentcolor.png")

#Q4
#compare life expectancy and infant mortality for two years 2012 , 1962 at each continent
gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy, color= continent)) + geom_point()+
  facet_grid(continent~year)
ggsave("fig/fertility-vs-lifeexpectancy-continentcolor-basedon-year-continent.png")

#Q5
# compare life expectancy and infant mortality in two year
gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy, color= continent)) + geom_point()+
  facet_grid(.~year)
ggsave("fig/fertility-vs-lifeexpectancy-continentcolor-basedon-year.png")

#Q6
# compare the life expectancy and infant mortality at different year for two continant 
years <- c(1962,1972,1982,1992,2002,2012)
continents <- c("Europe","Asia")
gapminder %>% filter(year %in% years & continent %in% continents) %>% 
          ggplot(aes(fertility,life_expectancy, color = continent)) + geom_point()+
          facet_wrap(~ year)
ggsave("fig/fertility-vs-lifeexpectancy-continentcolor-basedon-years-continents.png")

#Q7
# times series plot
gapminder %>% filter (country == "United States") %>% 
  ggplot(aes(year, fertility)) + geom_point() + geom_line()
ggsave("fig/fertility-vs- years-for USA.png")

#Q8
# time series for two country
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>% 
  ggplot(aes(year, fertility , group = country)) + geom_line()
ggsave("fig/fertility-vs-years-for south korea and germany.png")

#Q9
# we can used color = country instead of group = country
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>% 
  ggplot(aes(year, fertility , color = country)) + geom_line()
ggsave("fig/fertility vs year for all countries.png")

#Q10
# using the label instead of legend
labels <- data.frame(country = countries , x = c(1970,1965), y= c(6,3))
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>% 
  ggplot(aes(year, fertility , color = country)) + geom_line()+
  geom_text(data = labels,aes(x,y,label=country), size = 3)+
  theme(legend.position = "none")
ggsave("fig/fertility vs year for two countries with label.png")

#Q11
# question show histogram for per day incomes from 1970
gapminder %>% mutate(dollar_per_day = gdp/population/365) %>% 
  filter (year == 1970 , !is.na(dollar_per_day))%>% 
  ggplot(aes(dollar_per_day)) + geom_histogram(binwidth = 1, color = "blue")

ggsave("fig/histogram dollar per day.png")

# Q12
# how many countries have average daily incomes of
# about $1 (extremely poor), $2 (very poor), 
#$4 (poor), $8 (middle), $16 (well off), $32 (rich), 
# $64 (very rich) per day                                                                                              rich) per day.
gapminder %>% mutate(dollar_per_day = gdp/population/365) %>% 
  filter (year == 1970 , !is.na(dollar_per_day))%>% 
  ggplot(aes(log2(dollar_per_day))) + geom_histogram(binwidth = 1, color = "blue")

ggsave("fig/histogram dollar per day with log scale.png")

#Q13
# base 10 makes more sense, consider population sizes.
gapminder %>% filter(year == 1970 , !is.na(population))%>% 
  ggplot(aes(log10(population))) +geom_histogram(binwidth = 0.5, color = "black")
# we can use scale_x_continues(trans = "log10")
ggsave("fig/hist-log-population.png")

#Q14
# A histogram showed us that the 1970 income distribution values show a dichotomy.
# Comparing multiple distributions with boxplots 
# and ridge plots 
gapminder %>% mutate(dollar_per_day = gdp/population/365) %>% 
  filter(year == 1970 & !is.na(gdp)) %>% 
  ggplot(aes(region , dollar_per_day)) + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.7))
ggsave("fig/point-region-vs-dpd.png")

#Q15
# Data exploration is often made easier if we
# order the strata by some interpretable quantity.
data("gapminder")
gapminder %>% mutate(dollar_per_day= gdp/population/365)%>% 
  mutate(region=reorder(region,gdp,FUN=median)) %>% 
  filter(year == 1970 & !is.na(dollar_per_day))%>%
  ggplot(aes(region,dollar_per_day)) + geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust=0.7)) +
  scale_y_continuous(trans = "log2")

ggsave("fig/point-reorderregion-vs-dpd.png")

#Q16
# make a group of region based on dollar per day
gapminder %>% mutate(dollar_per_day= gdp/population/365)%>% 
  mutate(group = case_when(
  region %in% c("Western Europe", "Northern Europe","Southern Europe",
                "Northern America", "Australia and New Zealand") ~ "West",
  region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
  continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
  TRUE ~ "Others"))%>% 
  mutate(group = factor(group,
                      levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West"))) %>%
  filter(year == 1970 & !is.na(gdp)) %>% 
  ggplot(aes(group,dollar_per_day)) +geom_boxplot()+
  scale_y_continuous(trans ="log2")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab(" ") + geom_point(alpha = 0.5)

ggsave("fig/boxplot-groupregion-vs-dpd.png")

#Q17
# we can not find the bimodal distributions we need to use the 
# package ggridges 
library(ggridges)
gapminder %>% mutate(dollar_per_day= gdp/population/365)%>% 
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",
                  "Northern America", "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))%>% 
  mutate(group = factor(group,
                        levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West"))) %>%
  filter(year == 1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollar_per_day,group)) + geom_density_ridges(jittered_points = TRUE)+
  scale_x_continuous(trans ="log2")+
   

ggsave("fig/geomdensity-groupregion-vs-dpd.png")





