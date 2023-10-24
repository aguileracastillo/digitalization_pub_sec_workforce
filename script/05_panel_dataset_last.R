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
install.packages("stargazer")
library(stargazer)
library(caret)
library(gplots)
library(here)
library(lmtest)



load(here("script", "Environments", "")) 

### Loading the imputed master df ####
attach(imp_master_df)

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
zpanel_data[ ,c(3:25, 28:32)] = scale(zpanel_data[ ,c(3:25, 28:32)]) # normalizing all variables except per_growth_gdp
summary(zpanel_data)

stargazer(zpanel_data, type = "text", title = "Table 1: Summary Statistics", out = "Table1.html")

################################################################################
#################################### FIRST MODEL  ##############################
################################################################################

## Run model at the national level (formal employment & wage bill % gdp)

# Fixed effects:
fez1 <- plm(psec_sformal_em ~ egov_index +  # Try with psec_sformal_em,  
             log(gdp_percap) + icrg_qog + upop, 
           data = zpanel_data, 
           model= "within")
summary(fez1)

# Random effects 
rez1 <- plm(psec_sformal_em ~ egov_index +  # Try with psec_sformal_em,  
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")
summary(rez1)

#Hausman1 Test reject H0 with a=0.05 so FIXED EFFECTS is recommended 
hausman_test1 <- phtest(rez1, fez1) 
print(hausman_test1)

stargazer(fez1, type = "text")
summary(fixef(fez1))###GABY: Efectos fijos del modelo 1

## Uses the first differences 
fd_model1 <- plm(psec_sformal_em ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
                data = zpanel_data, 
                model = "fd")  # fd for first differences

##GABY: Incluir la prueba de primera diferencia para saber si se debe o no incluir el modelo en primera diferencia:
pwfdtest(psec_sformal_em ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Como el estimador es significativo: no se utiliza el modelo en primera diferencia.


################################################################################
#################################### SECOND MODEL  ##############################
################################################################################

fez2 <- plm(wbill_per_gdp ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")
summary(fez2)

rez2 <- plm(wbill_per_gdp ~ egov_index +    
              log(gdp_percap) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")
summary(rez2)

hausman_test2 <- phtest(fez2, rez2) #Hausman1 Test reject H0 with a=0.1 so random effects is recommended 
print(hausman_test2)

pwfdtest(wbill_per_gdp ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #GABY: Por no ser significativo, se recomienda correr el modelo en primera diferencia. 

fd_model2 <- plm(wbill_per_gdp ~ egov_index +    
                   log(gdp_percap) +icrg_qog + upop, 
                 data = zpanel_data, 
                 model= "fd") # fd for first differences
summary(fd_model2)
stargazer(fd_model2, type = "text")

##GABY: Resumen primeros dos modelos
stargazer(fez1, fd_model2, type = "text")

################################################################################
#################################### THIRD MODEL  ##############################
################################################################################

## Run model by occupational composition  (five levels)
fez3 <- plm(fpu_em_clerks ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez3 <- plm(fpu_em_clerks ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

hausman_test3 <- phtest(fez3, rez3) #Hausman1 Test reject H0 with a=0.1 so fixed effects is recommended 
print(hausman_test3)

pwfdtest(fpu_em_clerks ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #GABY: Por ser significativo, se recomienda correr el modelo sin primera diferencia. 

summary(fez3)
summary(fixef(fez3))

################################################################################
#################################### FOURTH MODEL ##############################
################################################################################

fez4 <- plm(fpu_em_elem_ocupation ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez4 <- plm(fpu_em_elem_ocupation ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop,
            data = zpanel_data, 
            model= "random")

hausman_test4 <- phtest(fez4, rez4) #Hausman1 Test reject H1 with a=0.1 so FIXED effects is recommended
print(hausman_test4)

pwfdtest(fpu_em_elem_ocupation ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #GABY: Por ser significativo, se recomienda correr el modelo sin primera diferencia. 

summary(fez4)
summary(fixef(fez4))


################################################################################
#################################### FIFTH MODEL  ##############################
################################################################################

fez5  <- plm(fpu_em_professional ~ egov_index +    
               log(gdp_percap) + icrg_qog + upop, 
             data = zpanel_data, 
             model= "within")

rez5 <- plm(fpu_em_professional ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

hausman_test5 <- phtest(fez5, rez5) #Hausman1 Test reject H0 with a=0.1 so fixed effects is recommended 
print(hausman_test5)

pwfdtest(fpu_em_professional ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Por ser significativo, se recomienda correr el modelo sin primera diferencia. 

summary(fez5)
summary(fixef(fez5))

################################################################################
#################################### SIXTH MODEL  ##############################
################################################################################

fez6 <- plm(fpu_em_senior_official ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez6 <- plm(fpu_em_senior_official ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

hausman_test6 <- phtest(fez6, rez6) #Hausman1 Test accepts H1 with a=0.39 so random effects is recommended
print(hausman_test6)

pwfdtest(fpu_em_senior_official ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Por ser significativo, se recomienda correr el modelo sin primera diferencia. 

summary(rez6)

################################################################################
#################################### SEVENTH MODEL #############################
################################################################################

fez7 <- plm(fpu_em_technician ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez7 <- plm(fpu_em_technician ~ egov_index +    
              log(gdp_percap) + icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

hausman_test7 <- phtest(fez7, rez7) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test7)

pwfdtest(fpu_em_technician ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Por ser significativo, se recomienda correr el modelo sin primera diferencia. 

summary(fez7)
summary(fixef(fez7))


## Resumen modelos 3 a 7:
stargazer(fez3, fez4, fez5, rez6, fez7, type = "text")
stargazer(fez3, fez4, fez5, rez6, fez7, type = "text", title = "Table 3: By Occupational Function: WWBI Database Five Levels", out = "Table3.html")

################################################################################
#################################### EIGHTH MODEL ##############################
################################################################################


## Run model by educational tier (three levels)
fez8 <- plm(pri_ed_sppaid_em ~ egov_index +    
              log(gdp_percap) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez8 <- plm(pri_ed_sppaid_em ~ egov_index +    
              log(gdp_percap) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

hausman_test8 <- phtest(fez8, rez8) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test8)

pwfdtest(pri_ed_sppaid_em ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Por no ser significativo, se recomienda correr el modelo con primera diferencia. 

fd_model8 <- plm(pri_ed_sppaid_em ~ egov_index +    
                   log(gdp_percap) +icrg_qog + upop, 
                 data = zpanel_data, 
                 model= "fd") # fd for first differences
summary(fd_model8)


################################################################################
#################################### NINTH MODEL ###############################
################################################################################

fez9 <- plm(sec_ed_sppaid_em ~ egov_index +    
              log(gdp_percap) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "within")

rez9 <- plm(sec_ed_sppaid_em ~ egov_index +    
              log(gdp_percap) +icrg_qog + upop, 
            data = zpanel_data, 
            model= "random")

hausman_test9 <- phtest(fez9, rez9) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test9)

pwfdtest(sec_ed_sppaid_em ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Por no ser significativo, se recomienda correr el modelo con primera diferencia. 

fd_model9 <- plm(sec_ed_sppaid_em ~ egov_index +    
                   log(gdp_percap) +icrg_qog + upop, 
                 data = zpanel_data, 
                 model= "fd") # fd for first differences

summary(fd_model9)


################################################################################
#################################### TENTH MODEL ###############################
################################################################################

fez10 <- plm(ter_ed_sppaid_em ~ egov_index +    
               log(gdp_percap) +icrg_qog + upop, 
             data = zpanel_data, 
             model= "within")

rez10 <- plm(ter_ed_sppaid_em ~ egov_index +    
               log(gdp_percap) +icrg_qog + upop, 
             data = zpanel_data, 
             model= "random")

hausman_test10 <- phtest(fez10, rez10) #Hausman1 Test reject H1 with a=0.1 so random effects is recommended
print(hausman_test10)

pwfdtest(ter_ed_sppaid_em ~ egov_index + log(gdp_percap) + icrg_qog + upop, 
         data = zpanel_data, h0="fd") #Por ser significativo, se recomienda correr el modelo sin primera diferencia. 


stargazer(fd_model8, fd_model9, rez10, type = "text")
stargazer(fd_model8, fd_model9, rez10, type = "text", title = "Table 4: By Educational Tier: WWBI Database Three Levels", out = "Table4.html")

write.csv(zpanel_data, file = "zpanel_data.csv")
