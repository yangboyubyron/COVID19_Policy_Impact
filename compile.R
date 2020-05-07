rm(list = ls())
cat("\f")


# Prepare needed libraries
library.list <- c("tidyr", "tidyverse", "zoo", "pracma")
for (i in 1:length(library.list)) {
  if (!library.list[i] %in% rownames(installed.packages())) {
    install.packages(library.list[i])
  }
  library(library.list[i], character.only = TRUE)
}
rm(library.list, i)

# Set working directory
setwd("C:/Users/brian/Documents/COVID_REGRESSIONS_DATA/data")

data <- read.csv(url('https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv'))


data <- data[, -grep("Flag", colnames(data))]

colnames(data) <- tolower(gsub("__", "_", substring(gsub("[ .]", "_", gsub('([[:upper:]])', ' \\1', colnames(data))), 2)))


variable_search <- function(df, search_range = 20){
  df <- data.frame(df)
  best_aval <- c()
  for(country_index in 1:nrow(df)){
    val <- na.omit(t(rev(df[country_index,(ncol(df)- search_range - 1):ncol(df)])))[1]
    best_aval <- c(best_aval, as.numeric(val))
  }
  df <- cbind(df, var_name = best_aval)
  return(df[c(2, ncol(df))])
}

#Append last known hospital bed data in past 20 years to main df, if not known in past 20 years -> NA
#Hospital beds per million for later comparisons
hospital_beds <- read.csv("API_SH.MED.BEDS.ZS_DS2_en_csv_v2_988924.csv")
hospital_beds <- variable_search(hospital_beds, search_range = 20)
names(hospital_beds)[names(hospital_beds) == "var_name"] <- "hospital_beds_per_million"
hospital_beds$hospital_beds_per_million <- hospital_beds$hospital_beds_per_million * 1000
data <- merge(data, hospital_beds, by.x="country_code", by.y="Country.Code", all.x = TRUE)
rm(hospital_beds)

#Population Density merge
people_per_sq_km <- read.csv("API_EN.POP.DNST_DS2_en_csv_v2_988966.csv")
people_per_sq_km <- variable_search(people_per_sq_km, search_range = 20)
names(people_per_sq_km)[names(people_per_sq_km) == "var_name"] <- "people_per_sq_km"
data <- merge(data, people_per_sq_km, by.x = "country_code", by.y = "Country.Code", all.x = TRUE)
rm(people_per_sq_km)

#GDP per capita merge
gdp_percap <- read.csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_988619.csv")
gdp_percap <- variable_search(gdp_percap, search_range = 20)
names(gdp_percap)[names(gdp_percap) == "var_name"] <- "gdp_percap"
data <- merge(data, gdp_percap, by.x="country_code", by.y="Country.Code", all.x = TRUE)
rm(gdp_percap)

#Population Total
pop <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_988606.csv")
pop <- variable_search(pop, search_range = 10)
names(pop)[names(pop) == "var_name"] <- "population_millions"
data <- merge(data, pop, by.x="country_code", by.y="Country.Code", all.x = TRUE)
data$population_millions = data$population_millions/1000000
rm(pop)

#Population demographics
pop0014 <- read.csv("API_SP.POP.0014.TO.ZS_DS2_en_csv_v2_998839.csv")
pop0014 <- variable_search(pop0014, search_range = 20)
names(pop0014)[names(pop0014) == "var_name"] <- "age_percent_0_to_14"
data <- merge(data, pop0014, by.x="country_code", by.y="Country.Code", all.x = TRUE)
data$age_percent_0_to_14 = data$age_percent_0_to_14
rm(pop0014)

pop1564 <- read.csv("API_SP.POP.1564.TO.ZS_DS2_en_csv_v2_988895.csv")
pop1564 <- variable_search(pop1564, search_range = 20)
names(pop1564)[names(pop1564) == "var_name"] <- "age_percent_15_to_64"
data <- merge(data, pop1564, by.x="country_code", by.y="Country.Code", all.x = TRUE)
data$age_percent_15_to_64 = data$age_percent_15_to_64
rm(pop1564)

pop65UP <- read.csv("API_SP.POP.65UP.TO.ZS_DS2_en_csv_v2_988979.csv")
pop65UP <- variable_search(pop65UP, search_range = 20)
names(pop65UP)[names(pop65UP) == "var_name"] <- "age_percent_65_UP"
data <- merge(data, pop65UP, by.x="country_code", by.y="Country.Code", all.x = TRUE)
data$age_percent_65_UP = data$age_percent_65_UP
rm(pop65UP)

smoking_prev <- read.csv("smoking_data.csv")
smoking_prev <- smoking_prev[c("name", "totalSmokingRate")]
names(smoking_prev)[names(smoking_prev) == "totalSmokingRate"] <- "percent_smoking_prev"
data <- merge(data, smoking_prev, by.x="country_name", by.y="name", all.x = TRUE)
rm(smoking_prev)



#Handle NULL values for cases/deaths by carrying forward last known value until next reported value is recorded
#First have to reorder data due to shuffling from pervious merges
data %>% arrange(country_code, date) -> data

main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  day_of_year <- 0
  vec <- c()
  for(day in df$date){
    day_of_year <- day_of_year + 1
    vec <- c(vec, day_of_year)
  }
  main <- append(main, vec)
}
data$day_of_year <- main

rm(df, day_of_year)
#Confirmed Cases
main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  last_recorded <- NA
  vec <- c()
  for(cases in df$confirmed_cases){
    if(is.na(cases) & is.na(last_recorded)){
      last_recorded <- 0
      vec <- c(vec, last_recorded)
    }
    else if(is.na(cases)){
      vec <- c(vec, last_recorded)
    }
    else{
      last_recorded <- cases
      vec <- c(vec, last_recorded)
    }
  }
  main <- append(main, vec)
}
data$confirmed_cases <- main

#Confirmed Deaths
main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  last_recorded <- NA
  vec <- c()
  for(deaths in df$confirmed_deaths){
    if(is.na(deaths) & is.na(last_recorded)){
      last_recorded <- 0
      vec <- c(vec, last_recorded)
    }
    else if(is.na(deaths)){
      vec <- c(vec, last_recorded)
    }
    else{
      last_recorded <- deaths
      vec <- c(vec, last_recorded)
    }
  }
  main <- append(main, vec)
}
data$confirmed_deaths <- main

rm(df, cases, country, deaths, last_recorded, main, vec, variable_search)
#COVID19 confirmed cases and deaths per million (As of 23 April 2020)
data$deaths_per_million <- data$confirmed_deaths/data$population_millions
data$cases_per_million <- data$confirmed_cases/data$population_millions


#ROUGH Estimates for hospitalization rates borrowed from: https://www.statista.com/statistics/1105402/covid-hospitalization-rates-us-by-age-group/
#Bed occupancy rate used is an average over 28 OECD countries: https://www.oecd-ilibrary.org/sites/0d67e02a-en/index.html?itemId=/content/component/0d67e02a-en
#1-BOR is an estimated proportion of hospital beds available for COVID-19 patients. Then using estimates for 
#hospitalization rates by age demographic to approximate number of COVID related hospitalizations to determine
#an approximate number of hospital beds remaining per million people.
data$spare_beds_per_million <- (data$hospital_beds_per_million *(1-0.752)) - (data$cases_per_million *(data$age_percent_65_UP/100)*0.445) - (data$cases_per_million*(data$age_percent_15_to_64/100)*0.2) - (data$cases_per_million*(data$age_percent_0_to_14/100)*0.01)




#New variable for %change in dependent vars. 
main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  last_recorded <- NA
  vec <- c()
  for(deaths in df$deaths_per_million){
    if(length(vec) == 0){
      prev_deaths <- deaths
      vec <- c(vec, last_recorded)
    }
    else{
      pct_change <- (deaths - prev_deaths)/prev_deaths
      last_recorded <- pct_change
      vec <- c(vec, last_recorded)
      prev_deaths <- deaths
    }
  }
  main <- append(main, vec)
}
data$percent_change_deaths_per_million <- main

#Calc. 5 day moving average 
main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  main <- c(main, movavg(df$percent_change_deaths_per_million, 5, type=c('s'))*100)
}
data$ma_percent_change_deaths_per_million <- main

data = subset(data, select = -c(percent_change_deaths_per_million) )

data$ma_percent_change_deaths_per_million[data$ma_percent_change_deaths_per_million == "NaN"] = NA
data$ma_percent_change_deaths_per_million[data$ma_percent_change_deaths_per_million == "Inf"] = NA


#Same, but for confirmed cases
main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  last_recorded <- NA
  vec <- c()
  for(cases in df$cases_per_million){
    if(length(vec) == 0){
      prev_cases <- cases
      vec <- c(vec, last_recorded)
    }
    else{
      pct_change <- (cases - prev_cases)/prev_cases
      last_recorded <- pct_change
      vec <- c(vec, last_recorded)
      prev_cases <- cases
    }
  }
  main <- append(main, vec)
}
data$percent_change_cases_per_million <- main

main <- c()
for(country in unique(data$country_code)){
  df <- subset(data, country_code == country)
  main <- c(main, movavg(df$percent_change_cases_per_million, 5, type=c('s'))*100)
}
data$ma_percent_change_cases_per_million <- main

data = subset(data, select = -c(percent_change_cases_per_million) )

data$ma_percent_change_cases_per_million[data$ma_percent_change_cases_per_million == "NaN"] = NA
data$ma_percent_change_cases_per_million[data$ma_percent_change_cases_per_million == "Inf"] = NA

rm(cases, country, day, deaths, last_recorded, main, pct_change, df, prev_cases, prev_deaths, vec)



#Constrain Data to only consider results after 1 Death
data_known_cases <- subset(data, data$confirmed_cases >= 1)
data_known_deaths <- subset(data, data$confirmed_deaths >= 1)

#New variable for indexing
main <- c()
for(country in unique(data_known_deaths$country_code)){
  df <- subset(data_known_deaths, country_code== country)
  days_since_first <- 0
  vec <- c()
  for(day in df$date){
    days_since_first <- days_since_first + 1
    vec <- c(vec, days_since_first)
  }
  main <- append(main, vec)
}
data_known_deaths$days_since_first_death <- main

main <- c()
for(country in unique(data_known_cases$country_code)){
  df <- subset(data_known_cases, country_code == country)
  days_since_first <- 0
  vec <- c()
  for(day in df$date){
    days_since_first <- days_since_first + 1
    vec <- c(vec, days_since_first)
  }
  main <- append(main, vec)
}
data_known_cases$days_since_first_case <- main

rm(country, day, days_since_first, main, vec, df)


data <- merge(data, data_known_cases[c('country_code', 'date', 'days_since_first_case')], by=c("country_code", "date"), all.x = TRUE)
data <- merge(data, data_known_deaths[c('country_code', 'date', 'days_since_first_death')], by=c("country_code", "date"), all.x = TRUE)


covid_country_level_data <- data
rm(data)

write.csv(covid_country_level_data, "C:/Users/brian/Documents/GitHub/COVID19_Policy_Impact/data/covid_country_level_data.csv")
write.csv(data_known_deaths, "C:/Users/brian/Documents/GitHub/COVID19_Policy_Impact/data/known_covid19_deaths.csv")
write.csv(data_known_cases, "C:/Users/brian/Documents/GitHub/COVID19_Policy_Impact/data/known_covid19_cases.csv")
