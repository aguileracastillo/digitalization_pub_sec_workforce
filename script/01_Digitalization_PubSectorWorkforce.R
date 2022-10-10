
#### Master df - Digitalization and the Public Sector Workforce ####

# This script merges databases from different sources
# these databases filtered out countries that are not present in all the databases
# at the same time missing valued were imputed.

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

#### Upload the imputed df ####

wwbi <- read_csv(here("Data", #### 27 countries- Worldwide Bureaucracy Indicators v.18 ####
                      "Raw",
                      "wwbi_impt_df.csv")) 
wwbi <- wwbi[, -1] # delete unnecessary column that contain row indexes
wwbi[, 1:6] <- lapply(wwbi[1:6], 
                      as.factor ) # change the var type as factor from 1 to 6 column
levels(wwbi$country) # list of countries in wwbi
str(wwbi) # visualize the structure of wwbi

## visualize missing data 
vis_dat(wwbi)
vis_miss(wwbi)
prop_miss(wwbi)
gg_miss_var(wwbi, 
            facet = year)
gg_miss_var(wwbi, 
            facet = country)


un_egov_eu <- read_csv(here("Data", ### 25 countries- United Nations E-Government Index ####
                            "Raw", 
                            "egov_08_18_eu.csv")) 
un_egov_eu <- un_egov_eu[, -1] # delete unnecessary column that contain row indexes
un_egov_eu[, 1:6] <- lapply(un_egov_eu[1:6], # change the var type as factor from 1 to 6 column
                            as.factor )
levels(un_egov_eu$country) # list of countries in egov
str(un_egov_eu) # visualize the structure of egov

prop_miss(un_egov_eu) ## visualize missing data 
setdiff(wwbi$country, 
        un_egov_eu$country) ##"Slovak Republic" "United Kingdom" missing



wb_gdp_eu <- read_csv(here("Data", ### 27 countries - WDI World Bank GDP and Urban POP ####
                           "Raw",
                           "gdp_upop_eu.csv")) 
wb_gdp_eu <- wb_gdp_eu[, -1] # delete unnecessary column that contain row indexes
wb_gdp_eu[, 1:6] <- lapply(wb_gdp_eu[1:6], # change the var type as factor from 1 to 6 column
                           as.factor )
levels(wb_gdp_eu$country) # list of countries in gdp and pop
str(wb_gdp_eu) # visualize the structure of gdp and pop

prop_miss(wb_gdp_eu) ## visualize missing data 
setdiff(wwbi$country, 
        wb_gdp_eu$country) # see de difference between wwbi and wb_gdp


oecd_lfpr_eu <- read_csv(here("Data", ### 27 countries OECD - Labor Force Participation Rate ####
                              "Raw",
                              "oecd_lfpr_eu.csv")) 
oecd_lfpr_eu <- oecd_lfpr_eu[, -1] # delete unnecessary column that contain row indexes
oecd_lfpr_eu[, 1:6] <- lapply(oecd_lfpr_eu[1:6], 
                              as.factor ) # change the var type as factor from 1 to 6 column
levels(oecd_lfpr_eu$country) # list of countries in lfpr oecd database
str(oecd_lfpr_eu) # visualize the structure of lfpr oecd 


prop_miss(oecd_lfpr_eu) ## visualize missing data 
setdiff(wwbi$country, 
        oecd_lfpr_eu$country) # "Bulgaria" "Croatia"  "Cyprus"   "Romania" are missing



oecd_pol_qog <- read_csv(here("Data", #### 23 countries OECD - Quality of Government ####
                              "Raw",
                              "oecd_pol_qog.csv")) 
oecd_pol_qog <- oecd_pol_qog[, -1] # delete unnecessary column that contain row indexes
oecd_pol_qog[, 1:6] <- lapply(oecd_pol_qog[1:6], 
                              as.factor ) # change the var type as factor from 1 to 6 column
levels(oecd_pol_qog$country) # list of countries in oecd_pol_qog database
str(oecd_pol_qog) # visualize the structure of the df

prop_miss(oecd_pol_qog) ## visualize missing data 
setdiff(wwbi$country, 
        oecd_pol_qog$country) # "Bulgaria" "Croatia"  "Cyprus"   "Romania" are missing


cofog_ggtot <- read_csv(here("Data", #### 25 countries- United Nations E-Government Index ####
                             "Raw",
                             "cofog_tot_df.csv")) 
cofog_ggtot <- cofog_ggtot[, -1] # delete unnecessary column that contain row indexes
cofog_ggtot[, 1:6] <- lapply(cofog_ggtot[1:6], 
                             as.factor ) # change the var type as factor from 1 to 6 column
levels(cofog_ggtot$country) # list of countries in cofog
str(cofog_ggtot) # visualize the structure of cofog df
prop_miss(cofog_ggtot)




#### Deleting common columns to make the merge ####
oecd_lfpr_eu <- oecd_lfpr_eu[, -c(3:6)]
un_egov_eu <- un_egov_eu[, -c(3:6 )] 
wb_gdp_eu <- wb_gdp_eu[, -c(3:6)]
oecd_pol_qog <- oecd_pol_qog[, -c(3:6)]
cofog_ggtot <- cofog_ggtot[, -c(3:6)]


### Mergeing all the data frames ####
master_df <- merge(wwbi, oecd_lfpr_eu, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, un_egov_eu, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, wb_gdp_eu, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, oecd_pol_qog, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, cofog_ggtot, by = c("country_code", "year"), all = TRUE)


imp_master_df <- master_df[, c(2,1, 3:34)] # organize and create the imputed master df

write.csv(imp_master_df, 
          file = here("Data", 
                      "Processed",
                      "imp_master_df.csv")) # write as csv in the directory

str(master_df)
summary(master_df)
vis_dat(master_df)
vis_miss(master_df)
prop_miss(master_df)
gg_miss_var(master_df, facet = year)
gg_miss_var(master_df, facet = country)

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

