###############################
## Digitalization and the Public Sector Workforce
###############################

#### Installing and loading  needed packages ####
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(naniar)
library(visdat)
library(ggplot2)

### Master df ####

directory <- setwd("C:/Users/eagle/Dropbox/Shared_Lucho/Dissertation_Models/Data/RawSources/Clean DF - R/Imputed_master_df")
#directory <- setwd("C:/Users/eagle/Dropbox/Shared_Lucho/Dissertation_Models/Data/RawSources/Clean DF - R")
wwbi <- read_csv("wwbi_impt_df.csv") # 27 countries - Worldwide Bureaucracy Indicators version 18 indicators
wwbi <- wwbi[, -1]
wwbi[, 1:6] <- lapply(wwbi[1:6], as.factor )
levels(wwbi$country)
str(wwbi)
vis_dat(wwbi)
vis_miss(wwbi)
prop_miss(wwbi)
gg_miss_var(wwbi, facet = year)
gg_miss_var(wwbi, facet = country)


un_egov_eu <- read_csv("egov_08_18_eu.csv") # 25 countries- United Nations E-Government Index
un_egov_eu <- un_egov_eu[, -1]
un_egov_eu[, 1:6] <- lapply(un_egov_eu[1:6], as.factor )
str(un_egov_eu)
levels(un_egov_eu$country)
prop_miss(un_egov_eu)
setdiff(wwbi$country, un_egov_eu$country) ##"Slovak Republic" "United Kingdom" missing



wb_gdp_eu <- read_csv("gdp_upop_eu.csv") ##27 countries - WDI World Bank GDP and Urban POP
wb_gdp_eu <- wb_gdp_eu[, -1]
wb_gdp_eu[, 1:6] <- lapply(wb_gdp_eu[1:6], as.factor )
str(wb_gdp_eu)
prop_miss(wb_gdp_eu)
levels(wb_gdp_eu$country)
setdiff(wwbi$country, wb_gdp_eu$country) 


oecd_lfpr_eu <- read_csv("oecd_lfpr_eu.csv") #27 countries OECD - Labor Force Participation Rate
oecd_lfpr_eu <- oecd_lfpr_eu[, -1]
oecd_lfpr_eu[, 1:6] <- lapply(oecd_lfpr_eu[1:6], as.factor )
str(oecd_lfpr_eu)
levels(oecd_lfpr_eu$country)
prop_miss(oecd_lfpr_eu)
setdiff(wwbi$country, oecd_lfpr_eu$country) # "Bulgaria" "Croatia"  "Cyprus"   "Romania" are missing
setdiff(oecd_lfpr_eu$country, oecd_ggov_debt$country )



oecd_pol_qog <- read_csv("oecd_pol_qog.csv") #23 countries OECD - Quality of Government
oecd_pol_qog <- oecd_pol_qog[, -1]
oecd_pol_qog[, 1:6] <- lapply(oecd_pol_qog[1:6], as.factor )
str(oecd_pol_qog)
levels(oecd_pol_qog$country)
prop_miss(oecd_pol_qog)
setdiff(wwbi$country, oecd_pol_qog$country) # "Bulgaria" "Croatia"  "Cyprus"   "Romania" are missing
setdiff(oecd_pol_qog$country, oecd_ggov_debt$country )

cofog_ggtot <- read_csv("cofog_tot_df.csv") # 25 countries- United Nations E-Government Index
cofog_ggtot <- cofog_ggtot[, -1]
cofog_ggtot[, 1:6] <- lapply(cofog_ggtot[1:6], as.factor )
str(cofog_ggtot)
levels(cofog_ggtot$country)
prop_miss(cofog_ggtot)





oecd_lfpr_eu <- oecd_lfpr_eu[, -c(3:6)]
un_egov_eu <- un_egov_eu[, -c(3:6 )] 
wb_gdp_eu <- wb_gdp_eu[, -c(3:6)]
oecd_pol_qog <- oecd_pol_qog[, -c(3:6)]
cofog_ggtot <- cofog_ggtot[, -c(3:6)]





datalist <- list(oecd_ggov_debt,oecd_ggov_deficit,oecd_ggov_pro_cost,oecd_ggov_spending,oecd_lfpr_eu,oecd_tud_eu)
### mergeing all the dataframes
master_df <- merge(wwbi, oecd_lfpr_eu, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, un_egov_eu, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, wb_gdp_eu, by = c("country_code", "year"), all = TRUE)
#master_df <- merge(master_df, oecd_pol_qog, by = c("country_code", "year"), all = TRUE)
master_df <- merge(master_df, cofog_ggtot, by = c("country_code", "year"), all = TRUE)


imp_master_df <- master_df[, c(2,1, 3:34)]

write.csv(imp_master_df, file = "imp_master_df.csv")

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

