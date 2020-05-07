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
known_covid19_cases <- read.csv("known_covid19_cases.csv")


################OLS###############################

reg1 <- lm(ma_percent_change_cases_per_million ~ lag(stringency_index_for_display, 14) +
             
             people_per_sq_km + gdp_percap + 
             population_millions + age_percent_15_to_64 + age_percent_65_UP +
             percent_smoking_prev + spare_beds_per_million,
           data = known_covid19_cases)


reg2 <- lm(ma_percent_change_cases_per_million ~  lag(c1_school_closing, 14) + lag(c2_workplace_closing, 14) +
             lag(c3_cancel_public_events, 14) + lag(c4_restrictions_on_gatherings, 14) +
             lag(c5_close_public_transport, 14) + lag(c6_stay_at_home_requirements, 14) + 
             lag(c7_restrictions_on_internal_movement, 14) + lag(c8_international_travel_controls, 14) +
             lag(h1_public_information_campaigns, 14) + lag(h2_testing_policy, 14) + lag(h3_contact_tracing, 14) +
             
             people_per_sq_km + gdp_percap + 
             population_millions + age_percent_15_to_64 + age_percent_65_UP +
             percent_smoking_prev + spare_beds_per_million,
           data = known_covid19_cases)


##############FIXED EFFECTS MODEL###################################
reg3 <- plm(ma_percent_change_cases_per_million ~  lag(stringency_index_for_display, 14) +
              
             + spare_beds_per_million,
            data = known_covid19_cases, model = 'within', index=c('country_code', 'date'))



#####The following code returns results as the lag variable changes from 1 to 20 days for the independent variables
#####The output is returned in a chart.
#####The same code is run twice, once for reg4 and again for reg6.
s1 <- c()
s1_upper <- c()
s1_lower <- c()
s1_sig <- c()
s2 <- c()
s2_upper <- c()
s2_lower <- c()
s2_sig <- c()
s3 <- c()
s3_upper <- c()
s3_lower <- c()
s3_sig <- c()
s4 <- c()
s4_upper <- c()
s4_lower <- c()
s4_sig <- c()
s5 <- c()
s5_upper <- c()
s5_lower <- c()
s5_sig <- c()
s6 <- c()
s6_upper <- c()
s6_lower <- c()
s6_sig <- c()
s7 <- c()
s7_upper <- c()
s7_lower <- c()
s7_sig <- c()
s8 <- c()
s8_upper <- c()
s8_lower <- c()
s8_sig <- c()
s9 <- c()
s9_upper <- c()
s9_lower <- c()
s9_sig <- c()
s10 <- c()
s10_upper <- c()
s10_lower <- c()
s10_sig <- c()
s11 <- c()
s11_upper <- c()
s11_lower <- c()
s11_sig <- c()
for(lag in 1:25){
  
  reg4 <- plm(ma_percent_change_cases_per_million ~  lag(c1_school_closing, lag) + lag(c2_workplace_closing, lag) +
                lag(c3_cancel_public_events, lag) + lag(c4_restrictions_on_gatherings, lag) +
                lag(c5_close_public_transport, lag) + lag(c6_stay_at_home_requirements, lag) + 
                lag(c7_restrictions_on_internal_movement, lag) + lag(c8_international_travel_controls, lag) +
                lag(h1_public_information_campaigns, lag) + lag(h2_testing_policy, lag) + lag(h3_contact_tracing, lag) +
                
               spare_beds_per_million,
              data = known_covid19_cases, model = 'within', index=c('country_code', 'date'))
  
  
  
  s1 <- c(s1, summary(reg4)$coefficients[2,1])
  s1_upper <- c(s1_upper, summary(reg4)$coefficients[2,1] + summary(reg4)$coefficients[2,2])
  s1_lower <- c(s1_lower, summary(reg4)$coefficients[2,1] - summary(reg4)$coefficients[2,2])
  s1_sig <- c(s1_sig, summary(reg4)$coefficients[2,4] < 0.001)
  
  s2 <- c(s2, summary(reg4)$coefficients[3,1])
  s2_upper <- c(s2_upper, summary(reg4)$coefficients[3,1] + summary(reg4)$coefficients[3,2])
  s2_lower <- c(s2_lower, summary(reg4)$coefficients[3,1] - summary(reg4)$coefficients[3,2])
  s2_sig <- c(s2_sig, summary(reg4)$coefficients[3,4] < 0.001)
  
  s3 <- c(s3, summary(reg4)$coefficients[4,1])
  s3_upper <- c(s3_upper, summary(reg4)$coefficients[4,1] + summary(reg4)$coefficients[4,2])
  s3_lower <- c(s3_lower, summary(reg4)$coefficients[4,1] - summary(reg4)$coefficients[4,2])
  s3_sig <- c(s3_sig, summary(reg4)$coefficients[4,4] < 0.001)
  
  s4 <- c(s4, summary(reg4)$coefficients[5,1])
  s4_upper <- c(s4_upper, summary(reg4)$coefficients[5,1] + summary(reg4)$coefficients[5,2])
  s4_lower <- c(s4_lower, summary(reg4)$coefficients[5,1] - summary(reg4)$coefficients[5,2])
  s4_sig <- c(s4_sig, summary(reg4)$coefficients[5,4] < 0.001)
  
  s5 <- c(s5, summary(reg4)$coefficients[6,1])
  s5_upper <- c(s5_upper, summary(reg4)$coefficients[6,1] + summary(reg4)$coefficients[6,2])
  s5_lower <- c(s5_lower, summary(reg4)$coefficients[6,1] - summary(reg4)$coefficients[6,2])
  s5_sig <- c(s5_sig, summary(reg4)$coefficients[6,4] < 0.001)
  
  s6 <- c(s6, summary(reg4)$coefficients[7,1])
  s6_upper <- c(s6_upper, summary(reg4)$coefficients[7,1] + summary(reg4)$coefficients[7,2])
  s6_lower <- c(s6_lower, summary(reg4)$coefficients[7,1] - summary(reg4)$coefficients[7,2])
  s6_sig <- c(s6_sig, summary(reg4)$coefficients[7,4] < 0.001)
  
  
  s7 <- c(s7, summary(reg4)$coefficients[8,1])
  s7_upper <- c(s7_upper, summary(reg4)$coefficients[8,1] + summary(reg4)$coefficients[8,2])
  s7_lower <- c(s7_lower, summary(reg4)$coefficients[8,1] - summary(reg4)$coefficients[8,2])
  s7_sig <- c(s7_sig, summary(reg4)$coefficients[8,4] < 0.001)
  
  s8 <- c(s8, summary(reg4)$coefficients[9,1])
  s8_upper <- c(s8_upper, summary(reg4)$coefficients[9,1] + summary(reg4)$coefficients[9,2])
  s8_lower <- c(s8_lower, summary(reg4)$coefficients[9,1] - summary(reg4)$coefficients[9,2])
  s8_sig <- c(s8_sig, summary(reg4)$coefficients[8,4] < 0.001)
  
  s9 <- c(s9, summary(reg4)$coefficients[10,1])
  s9_upper <- c(s9_upper, summary(reg4)$coefficients[10,1] + summary(reg4)$coefficients[10,2])
  s9_lower <- c(s9_lower, summary(reg4)$coefficients[10,1] - summary(reg4)$coefficients[10,2])
  s9_sig <- c(s9_sig, summary(reg4)$coefficients[10,4] < 0.001)
  
  s10 <- c(s10, summary(reg4)$coefficients[11,1])
  s10_upper <- c(s10_upper, summary(reg4)$coefficients[11,1] + summary(reg4)$coefficients[11,2])
  s10_lower <- c(s10_lower, summary(reg4)$coefficients[11,1] - summary(reg4)$coefficients[11,2])
  s10_sig <- c(s10_sig, summary(reg4)$coefficients[11,4] < 0.001)
  
  
  s11 <- c(s11, summary(reg4)$coefficients[12,1])
  s11_upper <- c(s11_upper, summary(reg4)$coefficients[12,1] + summary(reg4)$coefficients[12,2])
  s11_lower <- c(s11_lower, summary(reg4)$coefficients[12,1] - summary(reg4)$coefficients[12,2])
  s11_sig <- c(s11_sig, summary(reg4)$coefficients[12,4] < 0.001)       
}

s1.data <- data.frame(1:25, s1, s1_upper, s1_lower, s1_sig)
s2.data <- data.frame(1:25, s2, s2_upper, s2_lower, s2_sig)
s3.data <- data.frame(1:25, s3, s3_upper, s3_lower, s3_sig)
s4.data <- data.frame(1:25, s4, s4_upper, s4_lower, s4_sig)
s5.data <- data.frame(1:25, s5, s5_upper, s5_lower, s5_sig)
s6.data <- data.frame(1:25, s6, s6_upper, s6_lower, s6_sig)
s7.data <- data.frame(1:25, s7, s7_upper, s7_lower, s7_sig)
s8.data <- data.frame(1:25, s8, s7_upper, s8_lower, s8_sig)
s9.data <- data.frame(1:25, s9, s7_upper, s9_lower, s9_sig)
s10.data <- data.frame(1:25, s10, s7_upper, s10_lower, s10_sig)
s11.data <- data.frame(1:25, s11, s7_upper, s11_lower, s11_sig)


s1_coef_plot <- ggplot(s1.data, aes(X1.25, s1, color=s1_sig))+
  geom_point()+
  geom_line(data=s1.data)+
  geom_ribbon(data=s1.data,aes(ymin=s1_upper,ymax=s1_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s2_coef_plot <- ggplot(s2.data, aes(X1.25, s2, color=s2_sig))+
  geom_point()+
  geom_line(data=s2.data)+
  geom_ribbon(data=s2.data,aes(ymin=s2_upper,ymax=s2_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s3_coef_plot <- ggplot(s3.data, aes(X1.25, s3,  color=s3_sig))+
  geom_point()+
  geom_line(data=s3.data)+
  geom_ribbon(data=s3.data,aes(ymin=s3_upper,ymax=s3_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s4_coef_plot <- ggplot(s4.data, aes(X1.25, s4,  color=s4_sig))+
  geom_point()+
  geom_line(data=s4.data)+
  geom_ribbon(data=s4.data,aes(ymin=s4_upper,ymax=s4_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s5_coef_plot <- ggplot(s5.data, aes(X1.25, s5,  color=s5_sig))+
  geom_point()+
  geom_line(data=s5.data)+
  geom_ribbon(data=s5.data,aes(ymin=s5_upper,ymax=s5_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s6_coef_plot <- ggplot(s6.data, aes(X1.25, s6,  color=s6_sig))+
  geom_point()+
  geom_line(data=s6.data)+
  geom_ribbon(data=s6.data,aes(ymin=s6_upper,ymax=s6_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')


s7_coef_plot <- ggplot(s7.data, aes(X1.25, s7,  color=s7_sig))+
  geom_point()+
  geom_line(data=s7.data)+
  geom_ribbon(data=s7.data,aes(ymin=s7_upper,ymax=s7_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s8_coef_plot <- ggplot(s8.data, aes(X1.25, s8,  color=s8_sig))+
  geom_point()+
  geom_line(data=s8.data)+
  geom_ribbon(data=s8.data,aes(ymin=s8_upper,ymax=s8_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s9_coef_plot <- ggplot(s9.data, aes(X1.25, s9,  color=s9_sig))+
  geom_point()+
  geom_line(data=s9.data)+
  geom_ribbon(data=s9.data,aes(ymin=s9_upper,ymax=s9_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s10_coef_plot <- ggplot(s10.data, aes(X1.25, s10,  color=s10_sig))+
  geom_point()+
  geom_line(data=s10.data)+
  geom_ribbon(data=s10.data,aes(ymin=s10_upper,ymax=s10_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s11_coef_plot <- ggplot(s11.data, aes(X1.25, s11,  color=s11_sig))+
  geom_point()+
  geom_line(data=s11.data)+
  geom_ribbon(data=s11.data,aes(ymin=s11_upper,ymax=s11_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')


figure_fixed_effects_1 <- ggarrange(s1_coef_plot, s2_coef_plot, s3_coef_plot, s4_coef_plot, s5_coef_plot, s6_coef_plot,
                                    labels = c( "School Closing", "Work Closing", "Cancel Pub Event", "Restrict Gather", "Close Pub. Tran.", "Stay at Home Req."),
                                    ncol = 2, nrow = 3)

figure_fixed_effects_1 <- annotate_figure(figure_fixed_effects_1,
                                          top = text_grob("Coefficients for Fixed Effects Model", face = "bold", size = 18),
                                          bottom = text_grob("Data source: \n Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira. (2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government.", color = "blue",
                                                             hjust = 1, x = 1, face = "italic", size = 6),
                                          fig.lab = "Figure 1", fig.lab.face = "bold")

figure_fixed_effects_2 <- ggarrange(s7_coef_plot, s8_coef_plot, s9_coef_plot, s10_coef_plot, s11_coef_plot,
                                    labels = c("ROM", "Int. Trav. Cont.", "Pub Inf. Campaign", "Testing Policy" ,"Contact Tracing"),
                                    ncol = 2, nrow = 3)

figure_fixed_effects_2 <- annotate_figure(figure_fixed_effects_2,
                                          top = text_grob("Coefficients for Fixed Effects Model", face = "bold", size = 18),
                                          bottom = text_grob("Data source: \n Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira. (2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government.", color = "blue",
                                                             hjust = 1, x = 1, face = "italic", size = 6),
                                          fig.lab = "Figure 1", fig.lab.face = "bold")

## Redefine w/lag = 14 in order to compare across other regressions
reg4 <- plm(ma_percent_change_cases_per_million ~ lag(c1_school_closing, 14) + lag(c2_workplace_closing, 14) +
              lag(c3_cancel_public_events, 14) + lag(c4_restrictions_on_gatherings, 14) +
              lag(c5_close_public_transport, 14) + lag(c6_stay_at_home_requirements, 14) + 
              lag(c7_restrictions_on_internal_movement, 14) + lag(c8_international_travel_controls, 14) +
              lag(h1_public_information_campaigns, 14) + lag(h2_testing_policy, 14) + lag(h3_contact_tracing, 14) +
              spare_beds_per_million,
            data = known_covid19_cases, model = 'within', index=c('country_code', 'date'))


rm(s1_coef_plot, s1.data, s2_coef_plot, s2.data, s3_coef_plot, s3.data, s1_sig, s2_sig, s3_sig, s4_sig, s5_sig, s6_sig,s7_sig, s8_sig,s9_sig,s10_sig,s11_sig,
   s4_coef_plot, s4.data, s5_coef_plot, s5.data, s6_coef_plot, s6.data, s7_coef_plot, s7.data,  s8_coef_plot, s8.data, s9_coef_plot, s9.data, s10_coef_plot, s10.data, s11_coef_plot, s11.data,
   lag, s1, s1_lower, s1_upper,s2, s2_lower, s2_upper, s3, s3_lower, s3_upper, s4, s4_lower, s4_upper, 
   s5, s5_lower, s5_upper, s6, s6_lower, s6_upper, s7, s7_lower, s7_upper, s8, s8_lower, s8_upper, s9, s9_lower, s9_upper, s10, s10_lower, s10_upper, s11, s11_lower, s11_upper)
#####################RANDOM EFFECTS MODEL##########################################################
reg5 <- plm(ma_percent_change_cases_per_million ~ lag(stringency_index_for_display, 14) +
              
              people_per_sq_km + gdp_percap + population_millions + age_percent_15_to_64 + age_percent_65_UP +
              percent_smoking_prev + cases_per_million + spare_beds_per_million,
            data = known_covid19_cases, model = 'random', index=c('country_code', 'date'))



s1 <- c()
s1_upper <- c()
s1_lower <- c()
s1_sig <- c()
s2 <- c()
s2_upper <- c()
s2_lower <- c()
s2_sig <- c()
s3 <- c()
s3_upper <- c()
s3_lower <- c()
s3_sig <- c()
s4 <- c()
s4_upper <- c()
s4_lower <- c()
s4_sig <- c()
s5 <- c()
s5_upper <- c()
s5_lower <- c()
s5_sig <- c()
s6 <- c()
s6_upper <- c()
s6_lower <- c()
s6_sig <- c()
s7 <- c()
s7_upper <- c()
s7_lower <- c()
s7_sig <- c()
s8 <- c()
s8_upper <- c()
s8_lower <- c()
s8_sig <- c()
s9 <- c()
s9_upper <- c()
s9_lower <- c()
s9_sig <- c()
s10 <- c()
s10_upper <- c()
s10_lower <- c()
s10_sig <- c()
s11 <- c()
s11_upper <- c()
s11_lower <- c()
s11_sig <- c()

for(lag in 1:25){
  
  reg6 <- plm(ma_percent_change_cases_per_million ~   lag(c1_school_closing, lag) + lag(c2_workplace_closing, lag) +
                lag(c3_cancel_public_events, lag) + lag(c4_restrictions_on_gatherings, lag) +
                lag(c5_close_public_transport, lag) + lag(c6_stay_at_home_requirements, lag) + 
                lag(c7_restrictions_on_internal_movement, lag) + lag(c8_international_travel_controls, lag) +
                lag(h1_public_information_campaigns, lag) + lag(h2_testing_policy, lag) + lag(h3_contact_tracing, lag) +
                
                people_per_sq_km + gdp_percap + population_millions + age_percent_15_to_64 + age_percent_65_UP +
                percent_smoking_prev + spare_beds_per_million,
              data = known_covid19_cases, model = 'random', index=c('country_code', 'date'))
  
  s1 <- c(s1, summary(reg6)$coefficients[2,1])
  s1_upper <- c(s1_upper, summary(reg6)$coefficients[2,1] + summary(reg6)$coefficients[2,2])
  s1_lower <- c(s1_lower, summary(reg6)$coefficients[2,1] - summary(reg6)$coefficients[2,2])
  s1_sig <- c(s1_sig, summary(reg6)$coefficients[2,4] < 0.001)
  
  s2 <- c(s2, summary(reg6)$coefficients[3,1])
  s2_upper <- c(s2_upper, summary(reg6)$coefficients[3,1] + summary(reg6)$coefficients[3,2])
  s2_lower <- c(s2_lower, summary(reg6)$coefficients[3,1] - summary(reg6)$coefficients[3,2])
  s2_sig <- c(s2_sig, summary(reg6)$coefficients[3,4] < 0.001)
  
  s3 <- c(s3, summary(reg6)$coefficients[4,1])
  s3_upper <- c(s3_upper, summary(reg6)$coefficients[4,1] + summary(reg6)$coefficients[4,2])
  s3_lower <- c(s3_lower, summary(reg6)$coefficients[4,1] - summary(reg6)$coefficients[4,2])
  s3_sig <- c(s3_sig, summary(reg6)$coefficients[4,4] < 0.001)
  
  s4 <- c(s4, summary(reg6)$coefficients[5,1])
  s4_upper <- c(s4_upper, summary(reg6)$coefficients[5,1] + summary(reg6)$coefficients[5,2])
  s4_lower <- c(s4_lower, summary(reg6)$coefficients[5,1] - summary(reg6)$coefficients[5,2])
  s4_sig <- c(s4_sig, summary(reg6)$coefficients[5,4] < 0.001)
  
  s5 <- c(s5, summary(reg6)$coefficients[6,1])
  s5_upper <- c(s5_upper, summary(reg6)$coefficients[6,1] + summary(reg6)$coefficients[6,2])
  s5_lower <- c(s5_lower, summary(reg6)$coefficients[6,1] - summary(reg6)$coefficients[6,2])
  s5_sig <- c(s5_sig, summary(reg6)$coefficients[6,4] < 0.001)
  
  s6 <- c(s6, summary(reg6)$coefficients[7,1])
  s6_upper <- c(s6_upper, summary(reg6)$coefficients[7,1] + summary(reg6)$coefficients[7,2])
  s6_lower <- c(s6_lower, summary(reg6)$coefficients[7,1] - summary(reg6)$coefficients[7,2])
  s6_sig <- c(s6_sig, summary(reg6)$coefficients[7,4] < 0.001)
  
  s7 <- c(s7, summary(reg6)$coefficients[8,1])
  s7_upper <- c(s7_upper, summary(reg6)$coefficients[8,1] + summary(reg6)$coefficients[8,2])
  s7_lower <- c(s7_lower, summary(reg6)$coefficients[8,1] - summary(reg6)$coefficients[8,2])
  s7_sig <- c(s7_sig, summary(reg6)$coefficients[8,4] < 0.001)
  
  s8 <- c(s8, summary(reg6)$coefficients[9,1])
  s8_upper <- c(s8_upper, summary(reg6)$coefficients[9,1] + summary(reg6)$coefficients[9,2])
  s8_lower <- c(s8_lower, summary(reg6)$coefficients[9,1] - summary(reg6)$coefficients[9,2])
  s8_sig <- c(s8_sig, summary(reg6)$coefficients[9,4] < 0.001)
  
  s9 <- c(s9, summary(reg6)$coefficients[10,1])
  s9_upper <- c(s9_upper, summary(reg6)$coefficients[10,1] + summary(reg6)$coefficients[10,2])
  s9_lower <- c(s9_lower, summary(reg6)$coefficients[10,1] - summary(reg6)$coefficients[10,2])
  s9_sig <- c(s9_sig, summary(reg6)$coefficients[10,4] < 0.001)
  
  s10 <- c(s10, summary(reg6)$coefficients[11,1])
  s10_upper <- c(s10_upper, summary(reg6)$coefficients[11,1] + summary(reg6)$coefficients[11,2])
  s10_lower <- c(s10_lower, summary(reg6)$coefficients[11,1] - summary(reg6)$coefficients[11,2])
  s10_sig <- c(s10_sig, summary(reg6)$coefficients[11,4] < 0.001)
  
  s11 <- c(s11, summary(reg6)$coefficients[12,1])
  s11_upper <- c(s11_upper, summary(reg6)$coefficients[12,1] + summary(reg6)$coefficients[12,2])
  s11_lower <- c(s11_lower, summary(reg6)$coefficients[12,1] - summary(reg6)$coefficients[12,2])
  s11_sig <- c(s11_sig, summary(reg6)$coefficients[12,4] < 0.001)
  
  
  
}

s1.data <- data.frame(1:25, s1, s1_upper, s1_lower, s1_sig)
s2.data <- data.frame(1:25, s2, s2_upper, s2_lower, s2_sig)
s3.data <- data.frame(1:25, s3, s3_upper, s3_lower, s3_sig)
s4.data <- data.frame(1:25, s4, s4_upper, s4_lower, s4_sig)
s5.data <- data.frame(1:25, s5, s5_upper, s5_lower, s5_sig)
s6.data <- data.frame(1:25, s6, s6_upper, s6_lower, s6_sig)
s7.data <- data.frame(1:25, s7, s7_upper, s7_lower, s7_sig)
s8.data <- data.frame(1:25, s8, s7_upper, s8_lower, s8_sig)
s9.data <- data.frame(1:25, s9, s7_upper, s9_lower, s9_sig)
s10.data <- data.frame(1:25, s10, s7_upper, s10_lower, s10_sig)
s11.data <- data.frame(1:25, s11, s7_upper, s11_lower, s11_sig)

s1_coef_plot <- ggplot(s1.data, aes(X1.25, s1, color=s1_sig))+
  geom_point()+
  geom_line(data=s1.data)+
  geom_ribbon(data=s1.data,aes(ymin=s1_upper,ymax=s1_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s2_coef_plot <- ggplot(s2.data, aes(X1.25, s2, color=s2_sig))+
  geom_point()+
  geom_line(data=s2.data)+
  geom_ribbon(data=s2.data,aes(ymin=s2_upper,ymax=s2_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s3_coef_plot <- ggplot(s3.data, aes(X1.25, s3,  color=s3_sig))+
  geom_point()+
  geom_line(data=s3.data)+
  geom_ribbon(data=s3.data,aes(ymin=s3_upper,ymax=s3_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s4_coef_plot <- ggplot(s4.data, aes(X1.25, s4,  color=s4_sig))+
  geom_point()+
  geom_line(data=s4.data)+
  geom_ribbon(data=s4.data,aes(ymin=s4_upper,ymax=s4_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s5_coef_plot <- ggplot(s5.data, aes(X1.25, s5,  color=s5_sig))+
  geom_point()+
  geom_line(data=s5.data)+
  geom_ribbon(data=s5.data,aes(ymin=s5_upper,ymax=s5_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s6_coef_plot <- ggplot(s6.data, aes(X1.25, s6,  color=s6_sig))+
  geom_point()+
  geom_line(data=s6.data)+
  geom_ribbon(data=s6.data,aes(ymin=s6_upper,ymax=s6_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')


s7_coef_plot <- ggplot(s7.data, aes(X1.25, s7,  color=s7_sig))+
  geom_point()+
  geom_line(data=s7.data)+
  geom_ribbon(data=s7.data,aes(ymin=s7_upper,ymax=s7_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s8_coef_plot <- ggplot(s8.data, aes(X1.25, s8,  color=s8_sig))+
  geom_point()+
  geom_line(data=s8.data)+
  geom_ribbon(data=s8.data,aes(ymin=s8_upper,ymax=s8_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s9_coef_plot <- ggplot(s9.data, aes(X1.25, s9,  color=s9_sig))+
  geom_point()+
  geom_line(data=s9.data)+
  geom_ribbon(data=s9.data,aes(ymin=s9_upper,ymax=s9_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s10_coef_plot <- ggplot(s10.data, aes(X1.25, s10,  color=s10_sig))+
  geom_point()+
  geom_line(data=s10.data)+
  geom_ribbon(data=s10.data,aes(ymin=s10_upper,ymax=s10_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')

s11_coef_plot <- ggplot(s11.data, aes(X1.25, s11,  color=s11_sig))+
  geom_point()+
  geom_line(data=s11.data)+
  geom_ribbon(data=s11.data,aes(ymin=s11_upper,ymax=s11_lower),alpha=0.3)+
  labs(y='Coefficient', x='Lag Value (Days)', color ='p < 0.001')




figure_random_effects_1 <- ggarrange(s1_coef_plot, s2_coef_plot, s3_coef_plot, s4_coef_plot, s5_coef_plot, s6_coef_plot,
                                     labels = c( "School Closing", "Work Closing", "Cancel Pub Event", "Restrict Gather", "Close Pub. Tran.", "Stay at Home Req."),
                                     ncol = 2, nrow = 3)

figure_random_effects_1 <- annotate_figure(figure_random_effects_1,
                                           top = text_grob("Coefficients for Random Effects Model", face = "bold", size = 18),
                                           bottom = text_grob("Data source: \n Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira. (2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government.", color = "blue",
                                                              hjust = 1, x = 1, face = "italic", size = 6),
                                           fig.lab = "Figure 2", fig.lab.face = "bold")


figure_random_effects_2 <- ggarrange(s6_coef_plot, s7_coef_plot, s8_coef_plot, s9_coef_plot, s10_coef_plot,
                                     labels = c( "ROM", "Int. Trav. Cont.", "Pub Inf. Campaign", "Testing Policy", "Contact Tracing"),
                                     ncol = 2, nrow = 3)

figure_random_effects_2 <- annotate_figure(figure_random_effects_2,
                                           top = text_grob("Coefficients for Random Effects Model", face = "bold", size = 18),
                                           bottom = text_grob("Data source: \n Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira. (2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government.", color = "blue",
                                                              hjust = 1, x = 1, face = "italic", size = 6),
                                           fig.lab = "Figure 2", fig.lab.face = "bold")
## Redefine w/lag = 14 in order to compare across other regressions
reg6 <- plm(ma_percent_change_cases_per_million ~ lag(c1_school_closing, 14) + lag(c2_workplace_closing, 14 ) +
              lag(c3_cancel_public_events, 14) + lag(c4_restrictions_on_gatherings, 14) +
              lag(c5_close_public_transport, 14) + lag(c6_stay_at_home_requirements, 14) + 
              lag(c7_restrictions_on_internal_movement, 14) + lag(c8_international_travel_controls, 14) +
              lag(h1_public_information_campaigns, 14) + lag(h2_testing_policy, 14) + lag(h3_contact_tracing, 14) +
              
              people_per_sq_km + gdp_percap + population_millions + age_percent_15_to_64 + age_percent_65_UP +
              percent_smoking_prev + spare_beds_per_million,
            data = known_covid19_cases, model = 'random', index=c('country_code', 'date'))


rm(s1_coef_plot, s1.data, s2_coef_plot, s2.data, s3_coef_plot, s3.data, s1_sig, s2_sig, s3_sig, s4_sig, s5_sig, s6_sig, s7_sig, s8_sig, s9_sig, s10_sig, s11_sig, 
   s4_coef_plot, s4.data, s5_coef_plot, s5.data, s6_coef_plot, s6.data, s7_coef_plot, s7.data,  s8_coef_plot, s8.data, s9_coef_plot, s9.data, s10_coef_plot, s10.data, s11_coef_plot, s11.data,
   lag, s1, s1_lower, s1_upper,s2, s2_lower, s2_upper, s3, s3_lower, s3_upper, s4, s4_lower, s4_upper, 
   s5, s5_lower, s5_upper, s6, s6_lower, s6_upper, s7, s7_lower, s7_upper, s8, s8_lower, s8_upper, s9, s9_lower, s9_upper, s10, s10_lower, s10_upper, s11, s11_lower, s11_upper)


#######SAVE RESULTS#############

figure_fixed_effects_1 %>% ggexport(filename = "../figures/figure_fe_cases_1.pdf")
figure_fixed_effects_2 %>% ggexport(filename = "../figures/figure_fe_cases_2.pdf")
figure_random_effects_1 %>% ggexport(filename = "../figures/figure_re_cases_1.pdf")
figure_random_effects_2 %>% ggexport(filename = "../figures/figure_re_cases_2.pdf")
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, title="Results: COVID19 Confirmed Cases", align=TRUE, type = 'html', out="../figures/regression_results_cases.html")

