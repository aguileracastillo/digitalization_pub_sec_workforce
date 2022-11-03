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
imp_master_df <- imp_master_df[, -1]
imp_master_df[, 1:3] <- lapply(imp_master_df[1:3], 
                               as.factor) # change data type to factor columns 1 to 3

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
           y=  fpu_em_clerks)) + # Females as a share of public paid employees  (Clreks)
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

#### Panel data Analysis without normalizing####
# This is a test to see how the model behaves without normalizing the variables

panel_data <- pdata.frame(imp_master_df, # converting our data into panel data with plm function
                          index = c("country", # individuals 
                                    "year")) # time 



fe1 <- plm(psec_stotal_em ~ online_ser_index + 
             log(gdp_percap) + cofog_ggtot , 
           data = panel_data, 
           p.model= "within")

colnames(imp_master_df)
stargazer(fe1, type = "text")
summary(fe1)


fe2 <- plm(psec_stotal_em ~ log(egov_index) + log(gdp_percap) + lfpr + upop  + 
             wbill_per_gdp, data = panel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe2, type = "text")
summary(fe2)


fe3 <- plm(fpu_em_elem_ocupation~ egov_index + 
             gdp_percap + lfpr + upop + tud + 
             vdem_corr + icrg_qog + 
             wbgi_gee + frin , data = panel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe3, type = "text")
summary(fe3)

fe4 <- plm(fpu_em_professional~ egov_index + ####
             gdp_percap + lfpr + upop , data = panel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe4, type = "text")
summary(fe4)


fe5 <- plm(fpu_em_clerks~ egov_index + cofog_ggtot , data = panel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe5, type = "text")
summary(fe5)

fe6 <- plm(wbill_per_gdp ~ online_ser_index + ####
             gdp_percap + lfpr + upop + tud + 
             vdem_corr + icrg_qog + 
             wbgi_gee + frin , data = panel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe6, type = "text")
summary(fe6)

fe7 <- plm(wbill_per_gdp ~ online_ser_index, data = panel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe7, type = "text")
summary(fe7)

#### Data analysis after normalizing by mean####
zpanel_data <- panel_data
zpanel_data[ ,c(5:40)] = scale(zpanel_data[ ,c(5:40)]) 
summary(zpanel_data)

fe1 <- plm(psec_spaid_em~ egov_index + gdp_percap + ####paid employment
             wbgi_gee  , data = zpanel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe1, type = "text")
summary(fe1)

fe2 <- plm(ter_ed_sppaid_em~ egov_index + gdp_percap + ####paid employment
             wbgi_gee  , data = zpanel_data, p.model= "within")
colnames(imp_master_df)
stargazer(fe2, type = "text")
summary(fe2)







ggplot(zpanel_data, aes(x= fpu_em_professional, y= egov_index, color=region, shape=eu_member)) +
  geom_point() + 
  geom_smooth(method=lm) + facet_wrap(~country)

ggplot(zpanel_data, aes(x= egov_index , y=  upop)) + 
  geom_point() + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') +
  geom_smooth(method=lm)  + facet_wrap(~country)

write.csv(zpanel_data, file = "zpanel_data.csv")

fixeffects1 <- plm(psec_stotal_em~ egov_index, data = zpanel_data, p.model= "within")
stargazer(fixeffects1, type = "text")
summary(fixeffects1)

###MasterDF Redux ###
imp_master_df_redux <- imp_master_df %>% 
  filter(country_code !=  "BGR" & 
           country_code !=  "HRV" & 
           country_code !=  "CYP" & 
           country_code !=  "ROU" &
           country_code != "GBR" &
           country_code != "SVK" &
           country_code != "ISL" &
           country_code != "DEU" &
           country_code != "NOR"&
           country_code != "SVN" &
           country_code != "DNK" ) %>% droplevels() 
str(imp_master_df_redux)
levels(imp_master_df_redux$country)
summary(imp_master_df_redux)
gg_miss_var(imp_master_df_redux, facet = country)
str(imp_master_df_redux)
vis_dat(imp_master_df_redux)
vis_miss(imp_master_df_redux)
prop_miss(imp_master_df_redux)

### Plot Redux ###
ggplot(imp_master_df_redux, aes(x= egov_index , y=  psec_spaid_em)) + geom_point() + 
  geom_smooth(method=lm)  + facet_wrap(~country)

ggplot(imp_master_df_redux, aes(x= egov_index , y=  fpu_em_professional)) + geom_point() + 
  geom_smooth(method=lm)  + facet_wrap(~country)

ggplot(imp_master_df_redux, aes(x= egov_index , y=  fpu_em_technician)) + geom_point() + 
  geom_smooth(method=lm)  + facet_wrap(~country)


### Panel Redux ###
panel_data2 <- pdata.frame(imp_master_df_redux, index = c("country", "year"))

fe_red0 <- plm(psec_spaid_em ~ egov_index +  
                 log(per_growth_gdp) + wbgi_gee, data = panel_data2, p.model= "within")
colnames(imp_master_df_redux)
stargazer(fe_red0, type = "text")
summary(fe_red0)

### Stargazer Outreg ###
## stargazer(fe_red0, fe_red1, fe_red2, type = "html", out = "test.doc")

fe_red1 <- plm(fpu_em_professional ~ egov_index +  
               log(per_growth_gdp) + wbgi_gee, data = panel_data2, p.model= "within")
colnames(imp_master_df_redux)
stargazer(fe_red1, type = "text")
summary(fe_red1)

fe_red2 <- plm(fpu_em_technician ~ egov_index +  
                    log(per_growth_gdp) + wbgi_gee, data = panel_data2, p.model= "within")
colnames(imp_master_df_redux)
stargazer(fe_red2, type = "text")
summary(fe_red2)

fe_red3<- plm(wbill_per_gdp ~ online_ser_index +  
                 log(gdp_percap) + frin, data = panel_data2, p.model= "within")
colnames(imp_master_df_redux)
stargazer(fe_red3, type = "text")
summary(fe_red2)



## Random FX
re_red0 <- plm(psec_spaid_em ~ egov_index +  
                 log(per_growth_gdp) + wbgi_gee, data = panel_data2, p.model= "random")
colnames(imp_master_df_redux)
stargazer(re_red0, type = "text")
summary(re_red0)

re_red1 <- plm(fpu_em_professional ~ egov_index +  
                 log(per_growth_gdp) + wbgi_gee, data = panel_data2, p.model= "random")
colnames(imp_master_df_redux)
stargazer(re_red1, type = "text")
summary(re_red1)


## First Diff
fd_red1 <- plm(psec_stotal_em ~ online_ser_index, 
               data = panel_data2, p.model= "fd")
colnames(imp_master_df_redux)
stargazer(fd_red1, type = "text")
summary(fd_red1)

fd_red2 <- plm(psec_stotal_em ~ online_ser_index + 
                 tot_em_ter_ed_wpsec,  
               data = panel_data2, p.model= "fd")
colnames(imp_master_df_redux)
stargazer(fd_red2, type = "text")
summary(fd_red2)

