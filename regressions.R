rm(list = ls())
cat("\f")
library(tidyverse)

# Prepare needed libraries
library.list <- c("tidyr", "plm", "tidyverse", "stargazer")
for (i in 1:length(library.list)) {
  if (!library.list[i] %in% rownames(installed.packages())) {
    install.packages(library.list[i])
  }
  library(library.list[i], character.only = TRUE)
}
rm(library.list, i)

# Set working directory
setwd("C:/Users/brian/Documents/GitHub/COVID19_Policy_Impact/data")

#Load Data
known_covid19_deaths <- read.csv("known_covid19_deaths.csv")


reg1 <- lm(deaths_per_million ~ lag(stringency_index, 14)+ hospital_beds_per_million +              
                                 + people_per_sq_km + gdp_percap + population_millions + age_percent_0_to_14                 
                                 +age_percent_15_to_64 + age_percent_65_UP + percent_smoking_prev                    
                                 + cases_per_million + spare_beds_per_million,
                                   data = known_covid19_deaths)
summary(reg1)


reg2 <- lm(log(deaths_per_million) ~ lag(stringency_index, 14)+ hospital_beds_per_million +              
             + people_per_sq_km + gdp_percap + population_millions + age_percent_0_to_14                 
           +age_percent_15_to_64 + age_percent_65_UP + percent_smoking_prev                    
           + cases_per_million + spare_beds_per_million,
           data = known_covid19_deaths)
summary(reg2)



reg3 <- plm(deaths_per_million ~ lag(stringency_index, 15) + spare_beds_per_million + lag(spare_beds_per_million,1) + lag(spare_beds_per_million,2) 
                                                                + lag(spare_beds_per_million,3) + people_per_sq_km
                                                                + gdp_percap + population_millions + age_percent_15_to_64
                                                                + age_percent_65_UP + percent_smoking_prev
            ,data = known_covid19_deaths, model = 'random', index=c('country_code', 'date'))
summary(reg3)


reg4 <- plm(log(deaths_per_million) ~ lag(stringency_index_for_display, 14) + spare_beds_per_million + lag(spare_beds_per_million,1) + lag(spare_beds_per_million,2) 
            + lag(spare_beds_per_million,3) + people_per_sq_km
            + gdp_percap + population_millions + age_percent_15_to_64
            + age_percent_65_UP + percent_smoking_prev
            ,data = known_covid19_deaths, model = 'random', index=c('country_code', 'date'))
summary(reg4)

reg5 <- plm(log(deaths_per_million) ~ lag(stringency_index, 14) + spare_beds_per_million + lag(spare_beds_per_million,1) + lag(spare_beds_per_million,2) 
            + lag(spare_beds_per_million,3) + people_per_sq_km
            + gdp_percap + population_millions + age_percent_15_to_64
            + age_percent_65_UP + percent_smoking_prev
            ,data = known_covid19_deaths, model = 'within', index=c('country_code', 'date'))
summary(reg5)

stargazer(reg1, reg2, reg3, reg4, title="Results", align=TRUE, results='asis', type = 'text')

