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
library(here)
library(lmtest)



load(here("script", "Environments", "")) 

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

stargazer(zpanel_data, type = "text", title = "Table 1: Summary Statistics", out = "Table1.html")


## Run model at the national level (formal employment & wage bill % gdp)
fez1 <- plm(psec_sformal_em ~ egov_index +  # Try with psec_sformal_em,  
             log(per_growth_gdp) + icrg_qog + upop, 
           data = zpanel_data, 
           model= "within")

rez1 <- plm(psec_sformal_em ~ egov_index +  # Try with psec_sformal_em,  
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez1, type = "text")
stargazer(rez1, type = "text")
summary(fez1)
summary(rez1)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value= 0.0003583


hausman_test1 <- phtest(rez1, fez1) #Hausman1 Test reject H0 with a=0.05 so fixed effects is recommended 
print(hausman_test1)


fez2 <- plm(wbill_per_gdp ~ egov_index +    
           log(per_growth_gdp) +icrg_qog + upop, 
         data = zpanel_data, 
         model= "within")

rez2 <- plm(wbill_per_gdp ~ egov_index +    
              log(per_growth_gdp) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez2, type = "text")
stargazer(rez2, type = "text")
summary(fez2)
summary(rez2)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.08145

hausman_test2 <- phtest(fez2, rez2) #Hausman1 Test reject H0 with a=0.1 so fixed effects is recommended 
print(hausman_test2)

stargazer(fez1, rez2, type = "text")
stargazer(fez1, rez2, type = "text", title = "Table 2: Aggregate Level: Public Sector Employment as Share of Formal Employment & Wage Bill as % of GDP", out = "Table2.html")

## Run model by occupational composition  (five levels)
fez3 <- plm(fpu_em_clerks ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez3 <- plm(fpu_em_clerks ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez3, type = "text")
stargazer(rez3, type = "text")
summary(fez3)
summary(rez3)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.1058


hausman_test3 <- phtest(fez3, rez3) #Hausman1 Test reject H0 with a=0.1 so fixed effects is recommended 
print(hausman_test3)


fez4 <- plm(fpu_em_elem_ocupation ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez4 <- plm(fpu_em_elem_ocupation ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez4, type = "text")
stargazer(rez4, type = "text")
summary(fez4)
summary(rez4)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.8102

hausman_test4 <- phtest(fez4, rez4) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test4)

fez5  <- plm(fpu_em_professional ~ egov_index +    
               log(per_growth_gdp) + icrg_qog + upop, 
             data = zpanel_data, 
             model= "within")

rez5 <- plm(fpu_em_professional ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez5, type = "text")
stargazer(rez5, type = "text")
summary(fez5)
summary(rez5)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.00000003494

hausman_test5 <- phtest(fez5, rez5) #Hausman1 Test reject H0 with a=0.1 so fixed effects is recommended 
print(hausman_test5)

fez6 <- plm(fpu_em_senior_official ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez6 <- plm(fpu_em_senior_official ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez6, type = "text")
stargazer(rez6, type = "text")
summary(fez6)
summary(rez6)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.1852

hausman_test6 <- phtest(fez6, rez6) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test6)

fez7 <- plm(fpu_em_technician ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez7 <- plm(fpu_em_technician ~ egov_index +    
              log(per_growth_gdp) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez7, type = "text")
stargazer(rez7, type = "text")
summary(fez7)
summary(rez7)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.954

hausman_test7 <- phtest(fez7, rez7) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test7)

stargazer(rez3, rez4, fez5, rez6, rez7, type = "text")
stargazer(rez3, rez4, fez5, rez6, rez7, type = "text", title = "Table 3: By Occupational Function: WWBI Database Five Levels", out = "Table3.html")


## Run model by educational tier (three levels)
fez8 <- plm(pri_ed_sppaid_em ~ egov_index +    
              log(per_growth_gdp) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez8 <- plm(pri_ed_sppaid_em ~ egov_index +    
              log(per_growth_gdp) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez8, type = "text")
stargazer(rez8, type = "text")
summary(fez8)
summary(rez8)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.7769

hausman_test8 <- phtest(fez8, rez8) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test8)

fez9 <- plm(sec_ed_sppaid_em ~ egov_index +    
              log(per_growth_gdp) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez9 <- plm(sec_ed_sppaid_em ~ egov_index +    
              log(per_growth_gdp) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

colnames(imp_master_df)
stargazer(fez9, type = "text")
stargazer(rez9, type = "text")
summary(fez9)
summary(rez9)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
## p-value = 0.2958

hausman_test9 <- phtest(fez9, rez9) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test9)

fez10 <- plm(ter_ed_sppaid_em ~ egov_index +    
               log(per_growth_gdp) +icrg_qog + upop, 
             data = zpanel_data, 
             model= "within")

rez10 <- plm(ter_ed_sppaid_em ~ egov_index +    
               log(per_growth_gdp) +icrg_qog + upop, 
             data = zpanel_data, 
             model= "random")

colnames(imp_master_df)
stargazer(fez10, type = "text")
stargazer(rez10, type = "text")
summary(fez10)
summary(rez10)

## H0= Null Hypothesis: Random effects is consistent  
## H1= Alternative Hypothesis: Random effect inconsistent 
## a= significance level 0.1 , 0.05 , 0.01
##  p-value = 0.41

hausman_test10 <- phtest(fez10, rez10) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test10)

stargazer(rez8, rez9, rez10, type = "text")
stargazer(rez8, rez9, rez10, type = "text", title = "Table 4: By Educational Tier: WWBI Database Three Levels", out = "Table4.html")



write.csv(zpanel_data, file = "zpanel_data.csv")






