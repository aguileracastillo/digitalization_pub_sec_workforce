#### Installing and loading  needed packages ####
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(naniar)
library(visdat)
library(ggplot2)
library(plm)
library(stargazer)
library(caret)
library(gplots)


### Loading the imputed master df ####

imp_master_df <- read_csv(here("Data", 
                               "Processed", 
                               "imp_master_df.csv")) # load the data imputed data frame
imp_master_df <- imp_master_df[, -1] # deleting unnecessary column 
imp_master_df[, 1:3] <- lapply(imp_master_df[1:3], 
                               as.factor) # change data type to factor columns 1 to 3

imp_master_df[, 4:34] <- lapply(imp_master_df[4:34], 
                               as.numeric)

# Visualize the imputed data frame 
str(imp_master_df)
summary(imp_master_df)
vis_dat(imp_master_df)
vis_miss(imp_master_df)
prop_miss(imp_master_df)
gg_miss_var(imp_master_df, facet = year)
gg_miss_var(imp_master_df, facet = country)


#### Scatter plots with trend line to see variable relationships by country ####
ggplot(imp_master_df, 
       aes(x= fpu_em_professional, # Females as a share of public paid employees  (Professionals)
           y= egov_index)) + # e-government index
  geom_point() + # scatter plot
  geom_smooth(method=lm) + # trend line 
  facet_wrap(~country) # by country

ggplot(imp_master_df, 
       aes(x= online_ser_index , # online service index
           y=  fpu_em_clerks)) + # Females as a share of public paid employees  (Clerks)
  geom_point() + # scatter plot
  geom_smooth(method=lm)  + # trend line
  facet_wrap(~country) # by country

ggplot(imp_master_df, 
       aes(x= online_ser_index ,# online service index
           y=  fpu_em_technician)) + # Females as a share of public paid employees  (Technician)
  geom_point() + # scatter plot
  geom_smooth(method=lm)  + # trend line
  facet_wrap(~country) # by country

ggplot(imp_master_df, 
       aes(x= online_ser_index , # online service index
           y=  fpu_em_professional)) + # Females as a share of public paid employees  (Professional)
  geom_point() + # scatter plot
  geom_smooth(method=lm)  + # trend line
  facet_wrap(~country) # by country

ggplot(imp_master_df, 
       aes(x= egov_index , # e-government index
           y=  psec_stotal_em)) + # Public sector employment as share of total employment
  geom_point() + # scatter plot
  geom_smooth(method=lm)  + # trend line
  facet_wrap(~country) # by country

ggplot(imp_master_df, 
       aes(x= online_ser_index , # online service index
           y=  wbill_per_gdp)) + # Wage bill as a percent of GDP
  geom_point() + # scatter plot
  geom_smooth(method=lm)  + # trend line
  facet_wrap(~country) # by country

#### Data analysis after normalizing by mean####

panel_data <- pdata.frame(imp_master_df, # converting our data into panel data with plm function
                          index = c("country", # individuals 
                                    "year")) # time 

## Normalizing data using scale function (z means normalized)
zpanel_data <- panel_data 
zpanel_data <- zpanel_data[, -c(3,10)] # remove country_code and column 10 with NAs
zpanel_data[ ,c(3:26, 28:32)] = scale(zpanel_data[ ,c(3:26, 28:32)]) # normalizing all variables except per_growth_gdp
summary(zpanel_data)

fez1 <- plm(psec_sformal_em ~ hum_cap_index +  # Try with psec_sformal_em,  
             log(per_growth_gdp) + wbgi_gee + icrg_qog , 
           data = zpanel_data, 
           p.model= "within")

colnames(imp_master_df)
stargazer(fez1, type = "text")
summary(fe1)


fez2 <- plm(psec_spaid_em_urban ~ hum_cap_index + 
             log(per_growth_gdp) + wbgi_gee + icrg_qog , 
           data = zpanel_data, 
           p.model= "within")

colnames(imp_master_df)
stargazer(fez2, type = "text")
summary(fez2)


fez3 <- plm(fpu_em_senior_official ~ hum_cap_index + 
             log(per_growth_gdp) + wbgi_gee + icrg_qog , 
           data = zpanel_data, 
           p.model= "within")

colnames(imp_master_df)
stargazer(fez3, type = "text")
summary(fez3)

stargazer(fez1, fez2, fez3, type = "text")



write.csv(zpanel_data, file = "zpanel_data.csv")






