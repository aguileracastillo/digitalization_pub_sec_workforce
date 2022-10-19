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

## the df wwbi needs to be reshape usually year 
## are in one column and variables in different columns
wwbi <- wwbi[, c(2,4, 5:23)] # selecting the necessary columns to format from wide to long
wwbi1<- gather(wwbi, year, value, c(-1:-2)) # converting into a long format 
wwbi2<- spread(wwbi1, "ind_code", "value") # placing every variable into columns

wwbi_eu <- inner_join(wwbi2, # doing a inner join with EU country members keep observation present 
                     eu_member, # in wwbi2 leaving out the counries that are not EU memeber,
                     by = "country_code") # in total the df has 532 rows and 97 columns

wwbi_eu<- wwbi_eu[, c(2, 1, 95:97, 41,43,53,57,
                      59,49,51,54,56,60,
                      13,3,8,15,6,7,93,94)] # this will select the variables we are interested in

wwbi_eu <- droplevels(wwbi_eu) # dropping countries that still appaer as non EU countries
wwbi_eu[, 2:5] <- lapply(wwbi_eu[2:5], as.factor ) # changing var type as factor
wwbi_eu[, c(1, 6:23)] <- lapply(wwbi_eu[c(1,6:23)], as.numeric) # changing var type as numeric

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
# this method  is suitable for multivariate time series imputation
# the process of imputation is the following: 
# 1 sub-setting for each country 
# 2 count how many observations are, the result will be used fill the argument m in the mice function
# 3 get the imputed df with the function complete
# 4 check the imputed df with vis_miss 


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

df_list <- list(aut_impt, bel_impt, bul_impt, cro_impt, cyp_impt, czech_impt, 
                est_impt, fin_impt, fra_impt, gre_impt, hun_impt, ice_impt,
                ire_impt, lit_impt, lux_impt, pol_impt, por_impt, rom_impt, slk_impt,
                spn_impt, uk_impt)

Italy <- wwbi_eu %>%  filter(country == "Italy") %>% droplevels()
Latvia <- wwbi_eu %>%  filter(country == "Latvia") %>% droplevels()
Switzerland <- wwbi_eu %>%  filter(country == "Switzerland") %>% droplevels()

dfs_list <- list( Switzerland , wwbi_impt_df)

wwbi_impt_df <- rbindlist(dfs_list, use.names =  TRUE)

library(data.table)
wwbi_impt_df <- rbindlist(df_list, use.names = TRUE)
wwbi_impt_df <- wwbi_impt_df %>%  filter(year %in% c(2008,2010,2012,2014,2016,2018)) %>% 
  droplevels()
vis_miss(wwbi_impt_df)
nlevels(wwbi_impt_df$country)
levels(wwbi_impt_df$country)
wwbi_impt_df <- read_csv("wwbi_impt_df.csv")
##wwbi_impt_df <- wwbi_impt_df[-c(49:54), -1]
wwbi_impt_df <- wwbi_impt_df[, -1]
wwbi_impt_df[, 1:6] <- lapply(wwbi_impt_df[1:6], as.factor )
gg_miss_var(wwbi_impt_df, facet = year)
gg_miss_var(wwbi_impt_df, facet = country)
str(wwbi_impt_df)
vis_dat(wwbi_impt_df)
vis_miss(wwbi_impt_df)

write.csv(wwbi_impt_df, file = "wwbi_impt_df.csv")


wwbi_eu <- wwbi_eu %>%  filter(year %in% c(2008,2010,2012,2014,2016,2018)) %>% droplevels()
str(wwbi_eu)
wwbi_eu <- full_join(wwbi_eu, eu_sample, by = "country_code")
wwbi_eu <- droplevels(wwbi_eu)
wwbi_eu <- wwbi_eu[ ,c(2, 1, 24, 21:23, 3:20)] 
wwbi_eu[, 1:2] <- lapply(wwbi_eu[1:2], as.factor ) 
wwbi_eu[, 7:24] <- lapply(wwbi_eu[7:24], as.numeric)

str(wwbi_eu)
vis_dat(wwbi_eu)
vis_miss(wwbi_eu)
prop_miss(wwbi_eu)
gg_miss_var(wwbi_eu, facet = year)
gg_miss_var(wwbi_eu, facet = country)
summary(wwbi_eu)
write.csv(wwbi_eu, file = "wwbi_eu.csv")

clerk <- wwbi1 %>% filter(ind_code == "BI.PWK.PUBS.CK.FE.ZS")

setdiff(wwbi_eu$country, ec_frin$country)




## Merging the data by country code (Region and Income Group)
## First create a subset of wwbicountry only keeping Country Code, Region, Income Group with the subindex
## of the columns we are keeping

countries_info<- countries_info[, c(2, 1,  3, 4)]

## Then we merge wwbi1 and wwbicountry1 by Country Code using full_join function

wwbi2<- full_join(wwbi1, countries_info, by = "country")



## Changing class of some variables because all the variables are as characters and we have to identify
## some categorical variables and convert it as factor 

wwbi2 <- wwbi2[ , c(1:5, 7, 8, 6)]
str(wwbi2)

wwbi2[, 1:7] <- lapply(wwbi2[1:7], as.factor )
wwbi2$value<- as.numeric(wwbi2$value)
#format(wwbi2$value, scientific = FALSE)


## Now we are gonna spread the database converting the column Indicator Code in variables 

## First, we need to create a subset with the indicators
indicators <- as.data.frame( colnames(wwbi2))
indicators<- unique(indicators)

## Second, I need to delete indicator name column from the dataframe we are going to keep indicator codes
## as variables 
wwbi3<- wwbi2[ ,-3]

## now we spread the dataframe

wwbi4<- spread(wwbi3, "ind_code", "value")

#### Variables of Interest ####
# By Occupational Level # Female is known then Male = (1 paid - Female)

# Females as a share of public paid employees by occupation (Clerks)
# BI.PWK.PUBS.CK.FE.ZS
# Females as a share of public paid employees by occupation (Elementary occupation)
# BI.PWK.PUBS.EO.FE.ZS
# Females as a share of public paid employees by occupation (Professionals)
# BI.PWK.PUBS.PN.FE.ZS
# Females as a share of public paid employees by occupation (Senior officials)
# BI.PWK.PUBS.SN.FE.ZS
# Females as a share of public paid employees by occupation (Technicians)
# BI.PWK.PUBS.TN.FE.ZS

#Females, as a share of public paid employees
# BI.PWK.PUBS.FE.ZS

#### By education level #### Has this changed over time??? 
# Individuals with no education as a share of public paid employees
# BI.PWK.PUBS.NN.ZS 
# Individuals with primary education as a share of public paid employees
# BI.PWK.PUBS.PR.ZS 
# Individuals with secondary education as a share of public paid employees
# BI.PWK.PUBS.SG.ZS 
# Individuals with tertiary education as a share of public paid employees
# BI.PWK.PUBS.TT.ZS 
# Proportion of total employees with tertiary education working in public sector
# BI.EMP.TOTL.PB.TT.ZS

#### Type of Employment ####
# Public sector employment as a share of formal employment
# BI.EMP.FRML.PB.ZS 
# Public sector employment as a share of paid employment
# BI.EMP.PWRK.PB.ZS 
# Public sector employment as a share of total employment
# BI.EMP.TOTL.PB.ZS 

#### URBAN / RURAL ####
# Public sector employment as a share of paid employment by location (Rural)
# BI.EMP.PWRK.PB.RU.ZS 
# Public sector employment as a share of paid employment by location (Urban)
# BI.EMP.PWRK.PB.UR.ZS 

#### WAGE BILL #### 
#Wage bill as a percentage of GDP
# BI.WAG.TOTL.GD.ZS
# Wage bill as a percentage of Public Expenditure
# BI.WAG.TOTL.PB.ZS

#### Shaping the df wwbi4 with the indicators of interest ####



### Write the column index for the chosen indicators 
wwbi5<- wwbi4[, c(1,2,3,4,5,6,9,10,11,16,18,44,46,52,54,56,57,59,60,62,63,96,97)]
wwbi5<- wwbi4[, c(1,2,3,4,5,44,46,56,60,62,52,54,57,59,63,16,6,11,18,9,10,96,97)]

wwbi5 <- wwbi5 %>%  filter(year %in% c(2008,2010,2012,2014,2016,2018)) %>% droplevels()
#no <- wwbi5 %>% filter(country_code == "GBR")

## missing values with naniar

# entire df
vis_dat(wwbi5)
vis_miss(wwbi5)
prop_miss(wwbi5)
miss_wwbi5<- miss_var_summary(wwbi5)

# for Europe

wwbi6 <- wwbi5[ , -c(2:4)]
wwbi_eu <- full_join(wwbi6, eu_sample, by = "country_code")
wwbi_eu <- wwbi_eu[ ,c(1, 21, 2, 24, 22, 23, 3:20)] 
wwbi_eu<-  wwbi_eu %>%  filter(eu_member != "NA") %>% droplevels()
str(wwbi_eu)
summary(wwbi_eu)
write.csv(wwbi_eu, file = "wwbi_eu.csv")





ggplot(europe, aes( x= year , y= BI.PWK.PUBS.TN.FE.ZS )) + geom_miss_point() + facet_wrap(~country_name)
ggplot(europe, aes( x= year , y= BI.PWK.PUBS.CK.FE.ZS )) + geom_miss_point() + facet_wrap(~country_name)

gg_miss_var(europe, facet = year)

Estonia<- europe %>% filter(`Country Name`== "Estonia")
Estonia<- Estonia[-c(1:4), ]
ggplot(Estonia, aes(x = year)) + geom_line (aes (y= BI.PWK.PUBS.TN.FE.ZS,  color = "darkred")) +
  geom_line(aes (y= BI.PWK.PUBS.CK.FE.ZS,  color="steelblue"))

ggplot() + 
  geom_point(data = Estonia, aes(x = year, y = BI.PWK.PUBS.TN.FE.ZS), color = "blue") +
  geom_point(data = Estonia, aes(x = year, y = BI.PWK.PUBS.CK.FE.ZS), color = "red") +
  geom_point(data = Estonia, aes(x = year, y = BI.PWK.PUBS.PN.FE.ZS), color = "black")

## Public sector employment as a share of paid employment
# BI.EMP.PWRK.PB.ZS 
# Public sector employment as a share of total employment
# BI.EMP.TOTL.PB.ZS 

ggplot(europe, aes( x= year , y= BI.EMP.PWRK.PB.ZS )) + geom_miss_point() + facet_wrap(~`Country Name`)
ggplot(europe, aes( x= year , y= BI.EMP.TOTL.PB.ZS )) + geom_miss_point() + facet_wrap(~`Country Name`)



## split by regions in europe




#### for latam ####
str(latam)
vis_dat(latam)
vis_miss(latam)
prop_miss(latam)
miss_latam<- miss_var_summary(latam)
ggplot(latam, aes( x= year , y= BI.PWK.PUBS.TN.FE.ZS )) + geom_miss_point() + facet_wrap(~`Country Name`)
ggplot(latam, aes( x= year , y= BI.PWK.PUBS.CK.FE.ZS )) + geom_miss_point() + facet_wrap(~`Country Name`)
gg_miss_var(latam, facet = year)


####  Panel Data exploration ####

wwbi_panel<- wwbi5[, -c(3, 5)]
write.csv(wwbi_panel, file = "wwbi_panel.csv")

















