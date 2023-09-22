#### Installing and loading  needed packages ####
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(naniar)
library(visdat)
library(ggplot2)
library(countrycode)
library(here)
library(mice)
library(Amelia)
library(imputeTS)
library(data.table)

##load environment
load(here("script", "Environments", "WWBI_database.RData"))

#### CONTEXT OF THE DATA #####
# The data used in this script is the World Wide Bureaucracy Indicators that filters
# only the chosen variables. The WWBI has missing values 
# in which some countries needed imputation treatment. The imputation was made
# using the "imputeTS" package imputations for time series, after applying imputation
# the result is the "wwbi_impt_df.csv" which contains ONLY the wwbi indicators
# for EU countries. The "wwbi_impt_df.csv" file will be used in the script master_df
# to merge the wwbi with the other databases such as oecd. 


#### Loading the excel files and change var types ####

wwbi <- read_excel(here("Data", 
                        "Raw", 
                        "WWBIData.xlsx")) # loading the wwbi database 

wwbi <- wwbi %>% # renaming some columns of the wwbi df
  rename(country_code = "Country Code", 
         country = `Country Name`, 
         ind_name = `Indicator Name`, 
         ind_code= `Indicator Code`)

countries_info <- read_excel(here("Data", 
                                  "Processed",
                                  "WWBICountry.xlsx")) # loading country info (codes,  regions etc)

countries_info <- countries_info[, c(1,2,5,8)] %>%  # choosing the 4 columns of our interest
                  rename(country_code = "Country Code", # renaming the columns of country info
                         country = `Short Name`, 
                         code_2 = `2-alpha code`, 
                         region= Region)

countries_info[, 1:4] <- lapply(countries_info[1:4], # change columns 1:4 as factor
                                as.factor )

eu_regions <- read_excel(here("Data", 
                              "Processed", 
                              "WWBICountry_europe_subregions.xlsx")) # loading European region info

eu_member <- eu_regions[ , c(1 ,2, 6, 7)] %>% # selecting only needed columns 
             filter(eu_member != "NA") %>% # filtering out NA values leaving only EU members
             droplevels() %>% 
             rename(country_code = `Country Code`, # renaming the columns
                    country = `Short Name`, 
                    region = `SubRegion Biagi et al 2018`) 


wwbi[, 1:2] <- lapply(wwbi[1:2], as.factor ) # change column 1:2 as factor 
wwbi[, 5:23] <- lapply(wwbi[5:23], as.numeric) # change columns 5:23 as numeric 


glimpse(wwbi) # wwbi checking the variable type in the wwbi df
colnames(wwbi) # looking at colnames we need to change from wide to long format

### Cleaning and Reshaping data ####

## the df wwbi needs to be reshaped usually year 
## are in one column and variables in different columns
wwbi <- wwbi[, c(2,4, 5:23)] # selecting the necessary columns to format from wide to long
wwbi1<- gather(wwbi, year, value, c(-1:-2)) # converting into a long format 
wwbi2<- spread(wwbi1, "ind_code", "value") # placing every variable into columns

wwbi_eu <- inner_join(wwbi2, # doing a inner join with EU country members keep observations present 
                     eu_member, # in wwbi2 leaving out the countries that are not EU member,
                     by = "country_code") # in total the df has 532 rows and 97 columns

wwbi_eu<- wwbi_eu[, c(2, 1, 95:97, 41,43,53,57,
                      59,49,51,54,56,60,
                      13,3,8,15,6,7,93,94)] # this will select the variables we are interested in


# this long code renames the variables of our interest into a more readable description 

wwbi_eu <- wwbi_eu %>% 
  rename(fpu_em_clerks = BI.PWK.PUBS.CK.FE.ZS, # Females as a share of public paid employees
         fpu_em_elem_ocupation = BI.PWK.PUBS.EO.FE.ZS, 
         fpu_em_professional = BI.PWK.PUBS.PN.FE.ZS,
         fpu_em_senior_official = BI.PWK.PUBS.SN.FE.ZS,
         fpu_em_technician = BI.PWK.PUBS.TN.FE.ZS,
         fsppaid_em = BI.PWK.PUBS.FE.ZS, # Females, as a share of public paid employees
         no_ed_sppaid_em = BI.PWK.PUBS.NN.ZS, #Individuals with no education as a share of public paid employees
         pri_ed_sppaid_em = BI.PWK.PUBS.PR.ZS, # Individuals with primary education
         sec_ed_sppaid_em = BI.PWK.PUBS.SG.ZS, # Individuals with secondary education
         ter_ed_sppaid_em = BI.PWK.PUBS.TT.ZS, # Individuals with tertiary education
         tot_em_ter_ed_wpsec = BI.EMP.TOTL.PB.TT.ZS, # Proportion of total employees with tertiary education working in public sector
         psec_sformal_em = BI.EMP.FRML.PB.ZS, # Public sector employment as a share of total employment
         psec_spaid_em = BI.EMP.PWRK.PB.ZS, # Public sector employment as a share of paid employment
         psec_stotal_em = BI.EMP.TOTL.PB.ZS, # Public sector employment as a share of formal employment
         psec_spaid_em_rural = BI.EMP.PWRK.PB.RU.ZS, # Public sector employment as a share of paid employment by location (Rural)
         psec_spaid_em_urban = BI.EMP.PWRK.PB.UR.ZS, # Public sector employment as a share of paid employment by location (Urban)
         wbill_per_gdp = BI.WAG.TOTL.GD.ZS,# Wage bill as a percentage of GDP
         wbill_per_pub_expenditure = BI.WAG.TOTL.PB.ZS,) # Wage bill as a percentage of Public Expenditure

### Preparing data for imputation
# this paper https://journal.r-project.org/archive/2017/RJ-2017-009/index.html used imputeTS 
# with the spline https://en.wikipedia.org/wiki/Spline_interpolation 
# this method is suitable for multivariate time series imputation
# the process of imputation is the following: 
# 1 sub-setting for each country 
# 2 count how many observations there are
# 3 get the imputed df with the function complete
# 4 check the imputed df with vis_miss 

## Visualization before imputation
vis_miss(wwbi_eu) # explore missing values before imputation
gg_miss_var(wwbi_eu, facet = year) # explore missing values by year
gg_miss_var(wwbi_eu, facet = country) # explore missing values by country


#### Austria ####
Austria <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
           filter(country == "Austria") %>% # sub-setting for Austria 
           droplevels()
ggplot_na_distribution(Austria[3:20]) # plotting the data before imputing
prop_miss(Austria) # proportion of missing observations  
vis_miss(Austria) # visualize the na values before imputing
aut_impt <- na_interpolation(Austria, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Austria[9], # choose a column index to plot from country with NA
                      aut_impt[9]) # # choose a column index to plot from imputed country
prop_miss(aut_impt) # proportion of missing observations  
vis_miss(aut_impt) # compare the visualization before imputation

#### Belgium ####
Belgium <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Belgium") %>% # sub-setting for Belgium 
  droplevels()
ggplot_na_distribution(Belgium[3:20]) # plotting the data before imputing
prop_miss(Belgium) # proportion of missing observations  
vis_miss(Belgium) # visualize the na values before imputing
bel_impt <- na_interpolation(Belgium, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Belgium[9], # choose a column index to plot from country with NA
                      bel_impt[9]) # # choose a column index to plot from imputed country
prop_miss(bel_impt) # proportion of missing observations  
vis_miss(bel_impt) # compare the visualization before imputation

#### Bulgaria ####
Bulgaria <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Bulgaria") %>% # sub-setting for Bulgaria 
  droplevels()
ggplot_na_distribution(Bulgaria[3:20]) # plotting the data before imputing
prop_miss(Bulgaria) # proportion of missing observations  
vis_miss(Bulgaria) # visualize the na values before imputing
bul_impt <- na_interpolation(Bulgaria, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Bulgaria[9], # choose a column index to plot from country with NA
                      bul_impt[9]) # # choose a column index to plot from imputed country
prop_miss(bul_impt) # proportion of missing observations  
vis_miss(bul_impt) # compare the visualization before imputation

#### Croatia ####
Croatia <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Croatia") %>% # sub-setting for Croatia 
  droplevels()
ggplot_na_distribution(Croatia[3:20]) # plotting the data before imputing
prop_miss(Croatia) # proportion of missing observations  
vis_miss(Croatia) # visualize the na values before imputing
cro_impt <- na_interpolation(Croatia, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Croatia[9], # choose a column index to plot from country with NA
                      cro_impt[9]) # # choose a column index to plot from imputed country
prop_miss(cro_impt) # proportion of missing observations  
vis_miss(cro_impt) # compare the visualization before imputation

#### Cyprus ####
Cyprus <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Cyprus") %>% # sub-setting for Cyprus 
  droplevels()
ggplot_na_distribution(Cyprus[3:20]) # plotting the data before imputing
prop_miss(Cyprus) # proportion of missing observations  
vis_miss(Cyprus) # visualize the na values before imputing
cyp_impt <- na_interpolation(Cyprus, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Cyprus[9], # choose a column index to plot from country with NA
                      cyp_impt[9]) # # choose a column index to plot from imputed country
prop_miss(cyp_impt) # proportion of missing observations  
vis_miss(cyp_impt) # compare the visualization before imputation

#### Czech_R ####
Czech_R <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Czech Republic") %>% # sub-setting for Czech_R 
  droplevels()
ggplot_na_distribution(Czech_R[3:20]) # plotting the data before imputing
prop_miss(Czech_R) # proportion of missing observations  
vis_miss(Czech_R) # visualize the na values before imputing
czech_impt <- na_interpolation(Czech_R, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Czech_R[9], # choose a column index to plot from country with NA
                      czech_impt[9]) # # choose a column index to plot from imputed country
prop_miss(czech_impt) # proportion of missing observations  
vis_miss(czech_impt) # compare the visualization before imputation


#### Estonia ####
Estonia <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Estonia") %>% # sub-setting for Estonia 
  droplevels()
ggplot_na_distribution(Estonia[3:20]) # plotting the data before imputing
prop_miss(Estonia) # proportion of missing observations  
vis_miss(Estonia) # visualize the na values before imputing
est_impt <- na_interpolation(Estonia, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Estonia[9], # choose a column index to plot from country with NA
                      est_impt[9]) # # choose a column index to plot from imputed country
prop_miss(est_impt) # proportion of missing observations  
vis_miss(est_impt) # compare the visualization before imputation


#### Finland ####
Finland <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Finland") %>% # sub-setting for Finland 
  droplevels()
ggplot_na_distribution(Finland[3:20]) # plotting the data before imputing
prop_miss(Finland) # proportion of missing observations  
vis_miss(Finland) # visualize the na values before imputing
fin_impt <- na_interpolation(Finland, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Finland[9], # choose a column index to plot from country with NA
                      fin_impt[9]) # # choose a column index to plot from imputed country
prop_miss(fin_impt) # proportion of missing observations  
vis_miss(fin_impt) # compare the visualization before imputation


#### France ####
France <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "France") %>% # sub-setting for France 
  droplevels()
ggplot_na_distribution(France[3:20]) # plotting the data before imputing
prop_miss(France) # proportion of missing observations  
vis_miss(France) # visualize the na values before imputing
fra_impt <- na_interpolation(France, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(France[9], # choose a column index to plot from country with NA
                      fra_impt[9]) # # choose a column index to plot from imputed country
prop_miss(fra_impt) # proportion of missing observations  
vis_miss(fra_impt) # compare the visualization before imputation

#### Greece ####
Greece <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Greece") %>% # sub-setting for Greece 
  droplevels()
ggplot_na_distribution(Greece[3:20]) # plotting the data before imputing
prop_miss(Greece) # proportion of missing observations  
vis_miss(Greece) # visualize the na values before imputing
gre_impt <- na_interpolation(Greece, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Greece[9], # choose a column index to plot from country with NA
                      gre_impt[9]) # # choose a column index to plot from imputed country
prop_miss(gre_impt) # proportion of missing observations  
vis_miss(gre_impt) # compare the visualization before imputation

#### Hungary ####
Hungary <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Hungary") %>% # sub-setting for Hungary 
  droplevels()
ggplot_na_distribution(Hungary[3:20]) # plotting the data before imputing
prop_miss(Hungary) # proportion of missing observations  
vis_miss(Hungary) # visualize the na values before imputing
hun_impt <- na_interpolation(Hungary, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Hungary[9], # choose a column index to plot from country with NA
                      hun_impt[9]) # # choose a column index to plot from imputed country
prop_miss(hun_impt) # proportion of missing observations  
vis_miss(hun_impt) # compare the visualization before imputation

#### Iceland ####
Iceland <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Iceland") %>% # sub-setting for Iceland 
  droplevels()
ggplot_na_distribution(Iceland[3:20]) # plotting the data before imputing
prop_miss(Iceland) # proportion of missing observations  
vis_miss(Iceland) # visualize the na values before imputing
ice_impt <- na_interpolation(Iceland, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Iceland[9], # choose a column index to plot from country with NA
                      ice_impt[9]) # # choose a column index to plot from imputed country
prop_miss(ice_impt) # proportion of missing observations  
vis_miss(ice_impt) # compare the visualization before imputation

#### Ireland ####
Ireland <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Ireland") %>% # sub-setting for Ireland 
  droplevels()
ggplot_na_distribution(Ireland[3:20]) # plotting the data before imputing
prop_miss(Ireland) # proportion of missing observations  
vis_miss(Ireland) # visualize the na values before imputing
ire_impt <- na_interpolation(Ireland, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Ireland[9], # choose a column index to plot from country with NA
                      ire_impt[9]) # # choose a column index to plot from imputed country
prop_miss(ire_impt) # proportion of missing observations  
vis_miss(ire_impt) # compare the visualization before imputation

#### Lithuania ####
Lithuania <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Lithuania") %>% # sub-setting for Lithuania 
  droplevels()
ggplot_na_distribution(Lithuania[3:20]) # plotting the data before imputing
prop_miss(Lithuania) # proportion of missing observations  
vis_miss(Lithuania) # visualize the na values before imputing
lit_impt <- na_interpolation(Lithuania, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Lithuania[9], # choose a column index to plot from country with NA
                      lit_impt[9]) # # choose a column index to plot from imputed country
prop_miss(lit_impt) # proportion of missing observations  
vis_miss(lit_impt) # compare the visualization before imputation

#### Luxembourg ####
Luxembourg <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Luxembourg") %>% # sub-setting for Luxembourg 
  droplevels()
ggplot_na_distribution(Luxembourg[3:20]) # plotting the data before imputing
prop_miss(Luxembourg) # proportion of missing observations  
vis_miss(Luxembourg) # visualize the na values before imputing
lux_impt <- na_interpolation(Luxembourg, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Luxembourg[9], # choose a column index to plot from country with NA
                      lux_impt[9]) # # choose a column index to plot from imputed country
prop_miss(lux_impt) # proportion of missing observations  
vis_miss(lux_impt) # compare the visualization before imputation

#### Poland ####
Poland <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Poland") %>% # sub-setting for Poland 
  droplevels()
ggplot_na_distribution(Poland[3:20]) # plotting the data before imputing
prop_miss(Poland) # proportion of missing observations  
vis_miss(Poland) # visualize the na values before imputing
pol_impt <- na_interpolation(Poland, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Poland[9], # choose a column index to plot from country with NA
                      pol_impt[9]) # # choose a column index to plot from imputed country
prop_miss(pol_impt) # proportion of missing observations  
vis_miss(pol_impt) # compare the visualization before imputation

#### Portugal ####
Portugal <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Portugal") %>% # sub-setting for Portugal 
  droplevels()
ggplot_na_distribution(Portugal[3:20]) # plotting the data before imputing
prop_miss(Portugal) # proportion of missing observations  
vis_miss(Portugal) # visualize the na values before imputing
por_impt <- na_interpolation(Portugal, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Portugal[9], # choose a column index to plot from country with NA
                      por_impt[9]) # # choose a column index to plot from imputed country
prop_miss(por_impt) # proportion of missing observations  
vis_miss(por_impt) # compare the visualization before imputation

#### Romania ####
Romania <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Romania") %>% # sub-setting for Romania 
  droplevels()
ggplot_na_distribution(Romania[3:20]) # plotting the data before imputing
prop_miss(Romania) # proportion of missing observations  
vis_miss(Romania) # visualize the na values before imputing
rom_impt <- na_interpolation(Romania, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Romania[9], # choose a column index to plot from country with NA
                      rom_impt[9]) # # choose a column index to plot from imputed country
prop_miss(rom_impt) # proportion of missing observations  
vis_miss(rom_impt) # compare the visualization before imputation

#### Slovakia ####
Slovakia <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
            filter(country == "Slovak Republic") %>% # sub-setting for Slovakia 
  droplevels()
ggplot_na_distribution(Slovakia[3:20]) # plotting the data before imputing
prop_miss(Slovakia) # proportion of missing observations  
vis_miss(Slovakia) # visualize the na values before imputing
slk_impt <- na_interpolation(Slovakia, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Slovakia[9], # choose a column index to plot from country with NA
                      slk_impt[9]) # # choose a column index to plot from imputed country
prop_miss(slk_impt) # proportion of missing observations  
vis_miss(slk_impt) # compare the visualization before imputation

#### Spain ####
Spain <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "Spain") %>% # sub-setting for Spain 
  droplevels()
ggplot_na_distribution(Spain[3:20]) # plotting the data before imputing
prop_miss(Spain) # proportion of missing observations  
vis_miss(Spain) # visualize the na values before imputing
spn_impt <- na_interpolation(Spain, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(Spain[9], # choose a column index to plot from country with NA
                      spn_impt[9]) # # choose a column index to plot from imputed country
prop_miss(spn_impt) # proportion of missing observations  
vis_miss(spn_impt) # compare the visualization before imputation

#### UK ####
UK <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  # choosing the year country (1,3) and numeric vars (6:23)
  filter(country == "United Kingdom") %>% # sub-setting for UK 
  droplevels()
ggplot_na_distribution(UK[3:20]) # plotting the data before imputing
prop_miss(UK) # proportion of missing observations  
vis_miss(UK) # visualize the na values before imputing
uk_impt <- na_interpolation(UK, 
                             option = "spline") # impute function from imputeTS
ggplot_na_imputations(UK[9], # choose a column index to plot from country with NA
                      uk_impt[9]) # # choose a column index to plot from imputed country
prop_miss(uk_impt) # proportion of missing observations  
vis_miss(uk_impt) # compare the visualization before imputation

#### Not included countries ####

Italy <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  
  filter(country == "Italy") %>% droplevels()
ggplot_na_distribution(Italy[6:20]) # plotting the data before imputing
prop_miss(Italy) # proportion of missing observations  
vis_miss(Italy) # visualize the na values before imputing

Latvia <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  
  filter(country == "Latvia") %>% droplevels()
ggplot_na_distribution(Latvia[6:20]) # plotting the data before imputing
prop_miss(Latvia) # proportion of missing observations  
vis_miss(Latvia) # visualize the na values before imputing

Switzerland <- wwbi_eu[ ,c(1, 3,  6:23)] %>%  
  filter(country == "Switzerland") %>% droplevels()
ggplot_na_distribution(Switzerland[6:20]) # plotting the data before imputing
prop_miss(Switzerland) # proportion of missing observations  
vis_miss(Switzerland) # visualize the na values before imputing

#### Binding all countries to create the imputed df #####

df_list <- list(aut_impt, bel_impt, bul_impt, # here we create a data frame list 
                cro_impt, cyp_impt, czech_impt, # with the imputations results 
                est_impt, fin_impt, fra_impt, # this list will be used to create 
                gre_impt, hun_impt, ice_impt, # the imputed data frame
                ire_impt, lit_impt, lux_impt, 
                pol_impt, por_impt, rom_impt, 
                slk_impt,spn_impt, uk_impt,
                Italy, Latvia, Switzerland)


wwbi_impt_df <- rbindlist(df_list, # binding all the imputed countries 
                          use.names = TRUE) # creates the imputed df

vis_miss(wwbi_impt_df)

wwbi_impt_df <- wwbi_impt_df %>%  
                filter(year %in% # filtering by years of our interest
                         c(2008,2010,2012,
                           2014,2016,2018)) %>% 
                droplevels() # dropping levels 

vis_miss(wwbi_impt_df) # visualize imputed df
nlevels(wwbi_impt_df$country) # check the number of countries in the df
levels(wwbi_impt_df$country) # check the countries in the df

wwbi_impt_df <- inner_join(wwbi_impt_df, 
                           eu_member[, c(1,2)], 
                           by = "country") # adding the country code for joining wit other df
wwbi_impt_df <- wwbi_impt_df[, c(1,2 ,21, 3:20 )] # arranging the columns 

write.csv(wwbi_impt_df, # exporting the "wwbi_impt_df" 
          file = here("Data", 
                      "Processed", # to processed data
                      "wwbi_impt_df.csv")) # with this name


