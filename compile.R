rm(list = ls())
cat("\f") 

# Prepare needed libraries
library.list <- c("tidyr")
for (i in 1:length(library.list)) {
  if (!library.list[i] %in% rownames(installed.packages())) {
    install.packages(library.list[i])
  }
  library(library.list[i], character.only = TRUE)
}
rm(library.list, i)

# Set working directory
setwd("C:/Users/brian/Documents/COVID_REGRESSIONS_DATA/data")

#Load Data
state_restric <- read.csv("OxCGRT_Download_230420_091240_Full.csv")
meta_data <- read.csv("./metadata/Metadata_Country_API_EN.POP.DNST_DS2_en_csv_v2_988966.csv")


#Drop some columns not needed for our purposes and reassign to main df
data <- state_restric[, -grep("Notes", colnames(state_restric))]
data <- subset(data, select = -c(X))

rm(state_restric)


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
hospital_beds <- read.csv("API_SH.MED.BEDS.ZS_DS2_en_csv_v2_988924.csv")
hospital_beds <- variable_search(hospital_beds, search_range = 20)
names(hospital_beds)[names(hospital_beds) == "var_name"] <- "hostpital_beds"
data <- merge(data, hospital_beds, by.x="CountryCode", by.y="Country.Code", all.x = TRUE)
rm(hospital_beds)

#Population Density merge
pop_density <- read.csv("API_EN.POP.DNST_DS2_en_csv_v2_988966.csv")
pop_density <- variable_search(pop_density, search_range = 20)
names(pop_density)[names(pop_density) == "var_name"] <- "pop_density"
data <- merge(data, pop_density, by.x = "CountryCode", by.y = "Country.Code", all.x = TRUE)
rm(pop_density)

#GDP per capita merge
gdp_percap <- read.csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_988619.csv")
gdp_percap <- variable_search(gdp_percap, search_range = 20)
names(gdp_percap)[names(gdp_percap) == "var_name"] <- "gdp_percap"
data <- merge(data, gdp_percap, by.x="CountryCode", by.y="Country.Code", all.x = TRUE)
rm(gdp_percap)

#Population data
pop <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_988606.csv")
pop <- variable_search(pop, search_range = 10)
names(pop)[names(pop) == "var_name"] <- "population_thousands"
data <- merge(data, pop, by.x="CountryCode", by.y="Country.Code", all.x = TRUE)
data$population_thousands = data$population_thousands/1000
rm(pop)


#Handle NULL values for cases/deaths by carrying forward last known value until next reported value is recorded


#COVID19 confirmed cases and deaths per thousand (As of 23 April 2020)
data$deaths_per_thousand = data$ConfirmedDeaths/data$population_thousands
data$cases_per_thousand = data$ConfirmedCases/data$population_thousands






#Population Age Demographics
#pop_age_demo <- read.csv("WPP2019_PopulationByAgeSex_Medium.csv")
#temp <- subset(pop_age_demo, Time == 2020)



#Temperature Statistics by Country
state_temps <- read.csv("monthly_temps_by_country.csv")
require(data.table)
state_temps =na.omit(as.data.table(state_temps), cols=c("Mean.Temp", "Precip...mm."))
state_temps <- state_temps[, .(median(Mean.Temp), median(Precip...mm.)), by = .(Country, Month)]

#Subset data by month in order to merge temperature statistics across months
library(lubridate)
data$month <- month(ymd(data$Date))
jan <- subset(data, month == 1)
feb <- subset(data, month == 2)
mar <- subset(data, month == 3)
apr <- subset(data, month == 4)

state_temps$Country <- strsplit(tolower(state_temps$Country), " ")
jan$CountryName <- strsplit(tolower(jan$CountryName), " ")


merge(state_temps, jan, by.x = "Country", by.y = "CountryName")



write.csv(acs.sales.cleaned, "C:/Users/brian/Documents/SoftwareTools/Project/CleanedData/03.acs.sales.cleaned.csv")
