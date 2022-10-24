#### Installing and loading  needed packages ####
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(naniar)
library(visdat)
library(ggplot2)
library(here)

#### CONTEXT OF THE DATA #####


#### Load WWBI imputed data frame ####
im_wwbi <- read_csv(here("Data", # load the WWBI imputed data frame
                      "Processed", 
                      "wwbi_impt_df.csv")) # from the script "WWBI-database.r"
im_wwbi <- im_wwbi[, -1] # deleting unnecessary column which contains the row number
im_wwbi[, 1:3] <- lapply(im_wwbi[1:3], 
                         as.factor ) # change var type to factor
droplevels(im_wwbi)
levels(im_wwbi$country) # check the list of countries in the df
nlevels(im_wwbi$country) # the "im_wwbi" df contains 24 countries
str(im_wwbi) # check the structure of the "im_wwbi"
vis_miss(im_wwbi) # visualize missing values


#### United Nations E-Government Index #####
un_egov_eu <- read_csv(here("Data", 
                            "Raw", 
                            "egov_08_18_eu.csv")) # Loading the UN EGOV index 5 variables
un_egov_eu <- un_egov_eu[, -c(1, 4: 7)] # deleting unnecessary columns
un_egov_eu[, 1:2] <- lapply(un_egov_eu[1:2], 
                            as.factor ) # change var type to factor
levels(un_egov_eu$country) # check the list of countries in the df
nlevels(un_egov_eu$country) # the "un_egov_eu" df contains 24 countries
str(un_egov_eu) # check the structure of the "un_egov_eu"
vis_miss(un_egov_eu) # visualize missing values


#### WDI World Bank GDP and Urban POP ####
wb_gdp_eu <- read_csv(here("Data", 
                           "Raw",
                           "gdp_upop_eu.csv")) # Loading the WB - GDP & POP 3 variables
wb_gdp_eu <- wb_gdp_eu[, -c(1, 4: 7)] # deleting unnecessary columns
wb_gdp_eu[, 1:3] <- lapply(wb_gdp_eu[1:3], 
                           as.factor ) # change var type to factor
levels(wb_gdp_eu$country) # check the list of countries in the df
nlevels(wb_gdp_eu$country) # the "wb_gdp_eu" df contains 24 countries
str(wb_gdp_eu) # check the structure of the "wb_gdp_eu"
vis_miss(wb_gdp_eu) # visualize missing values

#### OECD - Labor Force Participation Rate ####
oecd_lfpr_eu <- read_csv(here("Data", 
                              "Raw",
                              "oecd_lfpr_eu.csv")) # Loading the OECD - LFPR 1 varible
oecd_lfpr_eu <- oecd_lfpr_eu[, -c(1, 4: 7)] # deleting unnecessary columns
oecd_lfpr_eu[, 1:2] <- lapply(oecd_lfpr_eu[1:2], 
                              as.factor ) # change var type to factor
levels(oecd_lfpr_eu$country) # check the list of countries in the df
nlevels(oecd_lfpr_eu$country) # the "oecd_lfpr_eu" df contains 24 countries
str(oecd_lfpr_eu) # check the structure of the "oecd_lfpr_eu"
vis_miss(oecd_lfpr_eu) # visualize missing values
setdiff(im_wwbi$country, oecd_pol_qog$country)
setdiff(oecd_pol_qog$country, im_wwbi$country)

#### OECD - Quality of Government ####
oecd_pol_qog <- read_csv(here("Data", 
                              "Raw",
                              "oecd_pol_qog.csv")) # loading the OECD - Q-lity of Gov 3 variables
oecd_pol_qog <- oecd_pol_qog[, -c(1, 4: 7)] # deleting unnecessary columns
oecd_pol_qog[, 1:2] <- lapply(oecd_pol_qog[1:2], 
                              as.factor ) # change var type to factor
levels(oecd_pol_qog$country) # check the list of countries in the df
nlevels(oecd_pol_qog$country) # the "oecd_pol_qog" df contains 24 countries
str(oecd_pol_qog) # check the structure of the "oecd_pol_qog"
vis_miss(oecd_pol_qog) # visualize missing values

#### United Nations E-Government Index ####
cofog_ggtot <- read_csv(here("Data", 
                             "Raw",
                             "cofog_tot_df.csv")) # Loading the UN E-Gov Index 1 variable
cofog_ggtot <- cofog_ggtot[, -c(1, 4: 7)] # deleting unnecessary columns
cofog_ggtot[, 1:2] <- lapply(cofog_ggtot[1:2], 
                             as.factor ) # change var type to factor
levels(cofog_ggtot$country) # check the list of countries in the df
nlevels(cofog_ggtot$country) # the "cofog_ggtot" df contains 24 countries
str(cofog_ggtot) # check the structure of the "cofog_ggtot"
vis_miss(cofog_ggtot) # visualize missing values



### Joining all df to create the master df ####
imp_master_df <- inner_join(im_wwbi, # the "im_wwbi" has 18 variables 
                        oecd_lfpr_eu, # the "oecd_lfpr_eu" has 1 variable
                        by = c("country_code", 
                               "year")) %>% 
              left_join(un_egov_eu, # the "un_egov_eu" has 5 variables 
                        by = c("country_code",
                               "year")) %>% 
              left_join(wb_gdp_eu,  # the "wb_gdp_eu" has 3 variables 
                        by = c("country_code", 
                               "year")) %>% 
              left_join(oecd_pol_qog, # the "oecd_pol_qog" has 3 variables 
                        by = c("country_code", 
                               "year")) %>% 
              left_join(cofog_ggtot, # the "cofog_ggtot" has 1 variables 
                        by = c("country_code", 
                               "year")) %>% 
              droplevels() %>% 
              replace_with_na_all(condition = ~.x == 0.0000000000)
  


str(imp_master_df)
summary(imp_master_df)
vis_dat(imp_master_df)
vis_miss(imp_master_df)
prop_miss(master_df)
gg_miss_var(imp_master_df, facet = year)
gg_miss_var(imp_master_df, facet = country)

write.csv(imp_master_df, file = here("Data", "Processed", "imp_master_df.csv"))



levels(master_df$country_code)
master_df1 <- master_df %>% 
  filter(country_code %in% c("DEU", "NOR","DNK")) %>% droplevels() 
gg_miss_var(master_df1, facet = country)


master_df1 <- master_df %>% 
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
str(master_df1)
summary(master_df1)
gg_miss_var(master_df1, facet = country)
str(master_df1)
vis_dat(master_df1)
vis_miss(master_df1)
prop_miss(master_df1)


