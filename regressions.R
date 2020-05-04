rm(list = ls())
cat("\f")

# Prepare needed libraries
library.list <- c("tidyr", "plm", "tidyverse", "stargazer", "ggpubr")
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


################OLS###############################

reg1 <- lm(deaths_per_million ~ lag(s1_school_closing, 14) + lag(s2_workplace_closing, 14) +
                                lag(s3_cancel_public_events, 14) + lag(s4_close_public_transport, 14) +
                                lag(s5_public_information_campaigns, 14) + lag(s6_restrictions_on_internal_movement, 14) + 
                                lag(s7_international_travel_controls, 14) +
                                hospital_beds_per_million + people_per_sq_km + gdp_percap + 
                                population_millions + age_percent_15_to_64 + age_percent_65_UP +
                                percent_smoking_prev + cases_per_million + spare_beds_per_million,
        data = known_covid19_deaths)


reg2 <- lm(log(deaths_per_million) ~ lag(s1_school_closing, 14) + lag(s2_workplace_closing, 14) +
             lag(s3_cancel_public_events, 14) + lag(s4_close_public_transport, 14) +
             lag(s5_public_information_campaigns, 14) + lag(s6_restrictions_on_internal_movement, 14) + 
             lag(s7_international_travel_controls, 14) +
             hospital_beds_per_million + people_per_sq_km + gdp_percap + 
             population_millions + age_percent_15_to_64 + age_percent_65_UP +
             percent_smoking_prev + cases_per_million + spare_beds_per_million,
           data = known_covid19_deaths)


##############FIXED EFFECTS MODEL###################################
reg3 <- plm(deaths_per_million ~ lag(s1_school_closing, 14) + lag(s2_workplace_closing, 14) +
             lag(s3_cancel_public_events, 14) + lag(s4_close_public_transport, 14) +
             lag(s5_public_information_campaigns, 14) + lag(s6_restrictions_on_internal_movement, 14) + 
             lag(s7_international_travel_controls, 14) +
             + cases_per_million + spare_beds_per_million,
           data = known_covid19_deaths, model = 'within', index=c('country_code', 'date'))



#####The following code returns results as the lag variable changes from 1 to 20 days for the independent variables
#####The output is returned in a chart.
#####The same code is run twice, once for reg4 and again for reg6.
s1 <- c()
s1_upper <- c()
s1_lower <- c()
s2 <- c()
s2_upper <- c()
s2_lower <- c()
s3 <- c()
s3_upper <- c()
s3_lower <- c()
s4 <- c()
s4_upper <- c()
s4_lower <- c()
s5 <- c()
s5_upper <- c()
s5_lower <- c()
s6 <- c()
s6_upper <- c()
s6_lower <- c()
s7 <- c()
s7_upper <- c()
s7_lower <- c()
for(lag in 1:20){
  
  reg4 <- plm(log(deaths_per_million) ~ lag(s1_school_closing, lag) + lag(s2_workplace_closing, lag) +
                lag(s3_cancel_public_events, lag) + lag(s4_close_public_transport, lag) +
                lag(s5_public_information_campaigns, lag) + lag(s6_restrictions_on_internal_movement, lag) + 
                lag(s7_international_travel_controls, lag) +
                cases_per_million + spare_beds_per_million,
              data = known_covid19_deaths, model = 'within', index=c('country_code', 'date'))
  
  
  s1 <- c(s1, summary(reg4)$coefficients[2,1])
  s1_upper <- c(s1_upper, summary(reg4)$coefficients[2,1] + summary(reg4)$coefficients[2,2])
  s1_lower <- c(s1_lower, summary(reg4)$coefficients[2,1] - summary(reg4)$coefficients[2,2])
  
  s2 <- c(s2, summary(reg4)$coefficients[3,1])
  s2_upper <- c(s2_upper, summary(reg4)$coefficients[3,1] + summary(reg4)$coefficients[3,2])
  s2_lower <- c(s2_lower, summary(reg4)$coefficients[3,1] - summary(reg4)$coefficients[3,2])
  
  s3 <- c(s3, summary(reg4)$coefficients[4,1])
  s3_upper <- c(s3_upper, summary(reg4)$coefficients[4,1] + summary(reg4)$coefficients[4,2])
  s3_lower <- c(s3_lower, summary(reg4)$coefficients[4,1] - summary(reg4)$coefficients[4,2])
  
  s4 <- c(s4, summary(reg4)$coefficients[5,1])
  s4_upper <- c(s4_upper, summary(reg4)$coefficients[5,1] + summary(reg4)$coefficients[5,2])
  s4_lower <- c(s4_lower, summary(reg4)$coefficients[5,1] - summary(reg4)$coefficients[5,2])
  
  s5 <- c(s5, summary(reg4)$coefficients[6,1])
  s5_upper <- c(s5_upper, summary(reg4)$coefficients[6,1] + summary(reg4)$coefficients[6,2])
  s5_lower <- c(s5_lower, summary(reg4)$coefficients[6,1] - summary(reg4)$coefficients[6,2])
  
  s6 <- c(s6, summary(reg4)$coefficients[7,1])
  s6_upper <- c(s6_upper, summary(reg4)$coefficients[7,1] + summary(reg4)$coefficients[7,2])
  s6_lower <- c(s6_lower, summary(reg4)$coefficients[7,1] - summary(reg4)$coefficients[7,2])
  
  s7 <- c(s7, summary(reg4)$coefficients[8,1])
  s7_upper <- c(s7_upper, summary(reg4)$coefficients[8,1] + summary(reg4)$coefficients[8,2])
  s7_lower <- c(s7_lower, summary(reg4)$coefficients[8,1] - summary(reg4)$coefficients[8,2])
  
}

s1.data <- data.frame(1:20, s1, s1_upper, s1_lower)
s2.data <- data.frame(1:20, s2, s2_upper, s2_lower)
s3.data <- data.frame(1:20, s3, s3_upper, s3_lower)
s4.data <- data.frame(1:20, s4, s4_upper, s4_lower)
s5.data <- data.frame(1:20, s5, s5_upper, s5_lower)
s6.data <- data.frame(1:20, s6, s6_upper, s6_lower)
s7.data <- data.frame(1:20, s7, s7_upper, s7_lower)

s1_coef_plot <- ggplot(s1.data, aes(X1.20, s1))+
  geom_point()+
  geom_line(data=s1.data)+
  geom_ribbon(data=s1.data,aes(ymin=s1_upper,ymax=s1_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s2_coef_plot <- ggplot(s2.data, aes(X1.20, s2))+
  geom_point()+
  geom_line(data=s2.data)+
  geom_ribbon(data=s2.data,aes(ymin=s2_upper,ymax=s2_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s3_coef_plot <- ggplot(s3.data, aes(X1.20, s3))+
  geom_point()+
  geom_line(data=s3.data)+
  geom_ribbon(data=s3.data,aes(ymin=s3_upper,ymax=s3_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s4_coef_plot <- ggplot(s4.data, aes(X1.20, s4))+
  geom_point()+
  geom_line(data=s4.data)+
  geom_ribbon(data=s4.data,aes(ymin=s4_upper,ymax=s4_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s5_coef_plot <- ggplot(s5.data, aes(X1.20, s5))+
  geom_point()+
  geom_line(data=s5.data)+
  geom_ribbon(data=s5.data,aes(ymin=s5_upper,ymax=s5_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s6_coef_plot <- ggplot(s6.data, aes(X1.20, s6))+
  geom_point()+
  geom_line(data=s6.data)+
  geom_ribbon(data=s6.data,aes(ymin=s6_upper,ymax=s6_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')


s7_coef_plot <- ggplot(s7.data, aes(X1.20, s7))+
  geom_point()+
  geom_line(data=s7.data)+
  geom_ribbon(data=s7.data,aes(ymin=s7_upper,ymax=s7_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')


figure_fixed_effects <- ggarrange(s1_coef_plot, s2_coef_plot, s3_coef_plot, s4_coef_plot, s5_coef_plot, s6_coef_plot, s7_coef_plot,
                                   labels = c("School Closing", "Workplace Closing", "Cancel Pub. Event", 
                                              "Public Transprt. Closed", "Public Inf. Campaign",
                                              "Restict. on  Mvment.", "Controls on Intl. Travel"),
                                   ncol = 2, nrow = 4)

figure_fixed_effects <- annotate_figure(figure_fixed_effects,
                                         top = text_grob("Coefficients for Fixed Effects Model", face = "bold", size = 18),
                                         bottom = text_grob("Data source: \n Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira. (2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government.", color = "blue",
                                                            hjust = 1, x = 1, face = "italic", size = 6),
                                         fig.lab = "Figure 1", fig.lab.face = "bold")

## Redefine w/lag = 14 in order to compare across other regressions
reg4 <- plm(log(deaths_per_million) ~ lag(s1_school_closing, 14) + lag(s2_workplace_closing, 14) +
              lag(s3_cancel_public_events, 14) + lag(s4_close_public_transport, 14) +
              lag(s5_public_information_campaigns, 14) + lag(s6_restrictions_on_internal_movement, 14) + 
              lag(s7_international_travel_controls, 14) +
              cases_per_million + spare_beds_per_million,
            data = known_covid19_deaths, model = 'within', index=c('country_code', 'date'))


rm(s1_coef_plot, s1.data, s2_coef_plot, s2.data, s3_coef_plot, s3.data, 
   s4_coef_plot, s4.data, s5_coef_plot, s5.data, s6_coef_plot, s6.data, s7_coef_plot, s7.data,
   lag, s1, s1_lower, s1_upper,s2, s2_lower, s2_upper, s3, s3_lower, s3_upper, s4, s4_lower, s4_upper, 
   s5, s5_lower, s5_upper, s6, s6_lower, s6_upper, s7, s7_lower, s7_upper)
#####################RANDOM EFFECTS MODEL##########################################################
reg5 <- plm(deaths_per_million ~ lag(s1_school_closing, 14) + lag(s2_workplace_closing, 14) +
              lag(s3_cancel_public_events, 14) + lag(s4_close_public_transport, 14) +
              lag(s5_public_information_campaigns, 14) + lag(s6_restrictions_on_internal_movement, 14) + 
              lag(s7_international_travel_controls, 14) +
              people_per_sq_km + gdp_percap + population_millions + age_percent_15_to_64 + age_percent_65_UP +
              percent_smoking_prev + cases_per_million + spare_beds_per_million,
            data = known_covid19_deaths, model = 'random', index=c('country_code', 'date'))



s1 <- c()
s1_upper <- c()
s1_lower <- c()
s2 <- c()
s2_upper <- c()
s2_lower <- c()
s3 <- c()
s3_upper <- c()
s3_lower <- c()
s4 <- c()
s4_upper <- c()
s4_lower <- c()
s5 <- c()
s5_upper <- c()
s5_lower <- c()
s6 <- c()
s6_upper <- c()
s6_lower <- c()
s7 <- c()
s7_upper <- c()
s7_lower <- c()
for(lag in 1:20){
  
  reg6 <- plm(log(deaths_per_million) ~ lag(s1_school_closing, lag) + lag(s2_workplace_closing, lag) +
                lag(s3_cancel_public_events, lag) + lag(s4_close_public_transport, lag) +
                lag(s5_public_information_campaigns, lag) + lag(s6_restrictions_on_internal_movement, lag) + 
                lag(s7_international_travel_controls, lag) +
                people_per_sq_km + gdp_percap + population_millions + age_percent_15_to_64 + age_percent_65_UP +
                percent_smoking_prev + cases_per_million + spare_beds_per_million,
              data = known_covid19_deaths, model = 'random', index=c('country_code', 'date'))
  
  s1 <- c(s1, summary(reg6)$coefficients[2,1])
  s1_upper <- c(s1_upper, summary(reg6)$coefficients[2,1] + summary(reg6)$coefficients[2,2])
  s1_lower <- c(s1_lower, summary(reg6)$coefficients[2,1] - summary(reg6)$coefficients[2,2])
  
  s2 <- c(s2, summary(reg6)$coefficients[3,1])
  s2_upper <- c(s2_upper, summary(reg6)$coefficients[3,1] + summary(reg6)$coefficients[3,2])
  s2_lower <- c(s2_lower, summary(reg6)$coefficients[3,1] - summary(reg6)$coefficients[3,2])
  
  s3 <- c(s3, summary(reg6)$coefficients[4,1])
  s3_upper <- c(s3_upper, summary(reg6)$coefficients[4,1] + summary(reg6)$coefficients[4,2])
  s3_lower <- c(s3_lower, summary(reg6)$coefficients[4,1] - summary(reg6)$coefficients[4,2])
  
  s4 <- c(s4, summary(reg6)$coefficients[5,1])
  s4_upper <- c(s4_upper, summary(reg6)$coefficients[5,1] + summary(reg6)$coefficients[5,2])
  s4_lower <- c(s4_lower, summary(reg6)$coefficients[5,1] - summary(reg6)$coefficients[5,2])
  
  s5 <- c(s5, summary(reg6)$coefficients[6,1])
  s5_upper <- c(s5_upper, summary(reg6)$coefficients[6,1] + summary(reg6)$coefficients[6,2])
  s5_lower <- c(s5_lower, summary(reg6)$coefficients[6,1] - summary(reg6)$coefficients[6,2])
  
  s6 <- c(s6, summary(reg6)$coefficients[7,1])
  s6_upper <- c(s6_upper, summary(reg6)$coefficients[7,1] + summary(reg6)$coefficients[7,2])
  s6_lower <- c(s6_lower, summary(reg6)$coefficients[7,1] - summary(reg6)$coefficients[7,2])
 
  s7 <- c(s7, summary(reg6)$coefficients[8,1])
  s7_upper <- c(s7_upper, summary(reg6)$coefficients[8,1] + summary(reg6)$coefficients[8,2])
  s7_lower <- c(s7_lower, summary(reg6)$coefficients[8,1] - summary(reg6)$coefficients[8,2])
 
}

s1.data <- data.frame(1:20, s1, s1_upper, s1_lower)
s2.data <- data.frame(1:20, s2, s2_upper, s2_lower)
s3.data <- data.frame(1:20, s3, s3_upper, s3_lower)
s4.data <- data.frame(1:20, s4, s4_upper, s4_lower)
s5.data <- data.frame(1:20, s5, s5_upper, s5_lower)
s6.data <- data.frame(1:20, s6, s6_upper, s6_lower)
s7.data <- data.frame(1:20, s7, s7_upper, s7_lower)

s1_coef_plot <- ggplot(s1.data, aes(X1.20, s1))+
    geom_point()+
    geom_line(data=s1.data)+
    geom_ribbon(data=s1.data,aes(ymin=s1_upper,ymax=s1_lower),alpha=0.3)+
    labs(y='Coefficient', x='Lag Value (Days)')

s2_coef_plot <- ggplot(s2.data, aes(X1.20, s2))+
  geom_point()+
  geom_line(data=s2.data)+
  geom_ribbon(data=s2.data,aes(ymin=s2_upper,ymax=s2_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s3_coef_plot <- ggplot(s3.data, aes(X1.20, s3))+
  geom_point()+
  geom_line(data=s3.data)+
  geom_ribbon(data=s3.data,aes(ymin=s3_upper,ymax=s3_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s4_coef_plot <- ggplot(s4.data, aes(X1.20, s4))+
  geom_point()+
  geom_line(data=s4.data)+
  geom_ribbon(data=s4.data,aes(ymin=s4_upper,ymax=s4_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s5_coef_plot <- ggplot(s5.data, aes(X1.20, s5))+
  geom_point()+
  geom_line(data=s5.data)+
  geom_ribbon(data=s5.data,aes(ymin=s5_upper,ymax=s5_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s6_coef_plot <- ggplot(s6.data, aes(X1.20, s6))+
  geom_point()+
  geom_line(data=s6.data)+
  geom_ribbon(data=s6.data,aes(ymin=s6_upper,ymax=s6_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')

s7_coef_plot <- ggplot(s7.data, aes(X1.20, s7))+
  geom_point()+
  geom_line(data=s7.data)+
  geom_ribbon(data=s7.data,aes(ymin=s7_upper,ymax=s7_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)')


figure_random_effects <- ggarrange(s1_coef_plot, s2_coef_plot, s3_coef_plot, s4_coef_plot, s5_coef_plot, s6_coef_plot, s7_coef_plot,
                                labels = c("School Closing", "Workplace Closing", "Cancel Pub. Event", 
                               "Public Transprt. Closed", "Public Inf. Campaign",
                               "Restict. on Mvment.", "Controls on Intl. Travel"),
                                ncol = 2, nrow = 4)
                              

figure_random_effects <- annotate_figure(figure_random_effects,
                top = text_grob("Coefficients for Random Effects Model", face = "bold", size = 18),
                bottom = text_grob("Data source: \n Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira. (2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government.", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 6),
                fig.lab = "Figure 2", fig.lab.face = "bold")



## Redefine w/lag = 14 in order to compare across other regressions
reg6 <- plm(log(deaths_per_million) ~ lag(s1_school_closing, 14) + lag(s2_workplace_closing, 14) +
              lag(s3_cancel_public_events, 14) + lag(s4_close_public_transport, 14) +
              lag(s5_public_information_campaigns, 14) + lag(s6_restrictions_on_internal_movement, 14) + 
              lag(s7_international_travel_controls, 14) +
              people_per_sq_km + gdp_percap + population_millions + age_percent_15_to_64 + age_percent_65_UP +
              percent_smoking_prev + cases_per_million + spare_beds_per_million,
            data = known_covid19_deaths, model = 'random', index=c('country_code', 'date'))


rm(s1_coef_plot, s1.data, s2_coef_plot, s2.data, s3_coef_plot, s3.data, 
   s4_coef_plot, s4.data, s5_coef_plot, s5.data, s6_coef_plot, s6.data, s7_coef_plot, s7.data,
   lag, s1, s1_lower, s1_upper,s2, s2_lower, s2_upper, s3, s3_lower, s3_upper, s4, s4_lower, s4_upper, 
   s5, s5_lower, s5_upper, s6, s6_lower, s6_upper, s7, s7_lower, s7_upper)


#######SAVE RESULTS#############

figure_fixed_effects %>% ggexport(filename = "../figure_fixed_effects.pdf")
figure_random_effects %>% ggexport(filename = "../figure_random_effects.pdf")
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, title="Results", align=TRUE, type = 'html', out="../regression_results.html")

