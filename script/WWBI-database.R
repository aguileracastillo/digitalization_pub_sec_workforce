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

#### Loading the excel files ####
# directory<- setwd ("~/Dropbox/PhD Andrew/Dissertation_Models/Data/RawSources/WorldBureaucracyIndicators/WWBI_csv")
#directorya <- setwd("C:/Users/Andres/Dropbox/Shared_Lucho/Dissertation_Models/Data/RawSources/WorldBureaucracyIndicators/WWBI_csv")

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

countries_info <- countries_info[, c(1,2,5,8)] %>%  # renaming the columns of country info
                  rename(country_code = "Country Code", 
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

#### Exploring your data ####

glimpse(wwbi) # wwbi checking the variable type in the wwbi df
colnames(wwbi) # looking at colnames we need to change from wide to long format

### Cleaning data ####

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
wwbi_eu[, 1:5] <- lapply(wwbi_eu[1:5], as.factor ) # changing var type as factor
wwbi_eu[, 6:23] <- lapply(wwbi_eu[6:23], as.numeric) # changing var type as numeric

# this long code renames the variables of our interest into a more readable description 

var_cod_names <- wwbi %>% 
                 distinct(`Indicator Name`, 
                          `Indicator Code`) %>% 
                 mutate(index = row_number()) %>% var_cod_names[, c(3, 1, 2)]


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

### before we filter by year we check for imputation

library("mice") # package used for imputation 
md.pattern(wwbi_eu)



wwbi_eu <- wwbi_eu[ ,c(1, 3,  6:23)] # selecting necessary columsn for imputation

vis_miss(wwbi_eu)
gg_miss_var(wwbi_eu, facet = year)
gg_miss_var(wwbi_eu, facet = country)


mice:::find.collinear(Austria)

Austria <- Austria[ ,-19]

Austria <- wwbi_eu %>%  filter(country == "Austria") %>% droplevels()
sum(is.na(Austria))
vis_miss(Austria)
imp_aut <- mice(Austria, m= 76, method = "norm.boot")
aut_impt <- complete(imp_aut)
vis_miss(aut_impt)

Belgium <- wwbi_eu %>%  filter(country == "2l.lmer") %>% droplevels()
sum(is.na(Belgium))
vis_miss(Belgium)
imp_bel <- mice(Belgium, m= 75, method = "2l.lmer")
bel_impt <- complete(imp_bel)
vis_miss(bel_impt)

Bulgaria <- wwbi_eu %>%  filter(country == "Bulgaria") %>% droplevels()
sum(is.na(Bulgaria))
vis_miss(Bulgaria)
imp_bul <- mice(Bulgaria, m= 129, method = "2l.lmer")
colSums(is.na(Bulgaria))
bul_impt <- complete(imp_bul)
vis_miss(bul_impt)

Croatia <- wwbi_eu %>%  filter(country == "Croatia") %>% droplevels()
sum(is.na(Croatia))
vis_miss(Croatia)
imp_cro <- mice(Croatia, m= 183, method = "2l.lmer")
cro_impt <- complete(imp_cro, 7)
vis_miss(cro_impt)

Cyprus <- wwbi_eu %>%  filter(country == "Cyprus") %>% droplevels()
sum(is.na(Cyprus))
vis_miss(Cyprus)
imp_cyp <- mice(Cyprus, m= 90, method = "2l.lmer")
cyp_impt <- complete(imp_cyp, 7)
vis_miss(cyp_impt)

Czech_R <- wwbi_eu %>%  filter(country == "Czech Republic") %>% droplevels()
sum(is.na(Czech_R))
vis_miss(Czech_R)
imp_czech <- mice(Czech_R, m= 95, method = "2l.lmer")
czech_impt <- complete(imp_czech, 7)
vis_miss(czech_impt)

Estonia <- wwbi_eu %>%  filter(country == "Estonia") %>% droplevels()
sum(is.na(Estonia))
vis_miss(Estonia)
imp_esto <- mice(Estonia, m= 74, method = "2l.lmer")
esto_impt <- complete(imp_esto, 7)
vis_miss(esto_impt)

Finland <- wwbi_eu %>%  filter(country == "Finland") %>% droplevels()
vis_miss(Finland)
sum(is.na(Finland))
imp_finl <- mice(Finland, m= 198, method = "2l.lmer")
finl_impt <- complete(imp_finl, 7)
vis_miss(finl_impt)

France <- wwbi_eu %>%  filter(country == "France") %>% droplevels()
vis_miss(France)
sum(is.na(France))
imp_france <- mice(France, m= 80, method = "2l.lmer")
france_impt <- complete(imp_france, 7)
vis_miss(france_impt)

Greece <- wwbi_eu %>%  filter(country == "Greece") %>% droplevels()
vis_miss(Greece)
sum(is.na(Greece))
imp_greece <- mice(Greece, m= 126, method = "2l.lmer")
greece_impt <- complete(imp_greece, 7)
vis_miss(greece_impt)

Hungary <- wwbi_eu %>%  filter(country == "Hungary") %>% droplevels()
vis_miss(Hungary)
sum(is.na(Hungary))
imp_hung <- mice(Hungary, m= 88, method = "2l.lmer")
hung_impt <- complete(imp_hung, 7)
vis_miss(hung_impt)

Iceland <- wwbi_eu %>%  filter(country == "Iceland") %>% droplevels()
vis_miss(Iceland)
sum(is.na(Iceland))
imp_icel <- mice(Iceland, m= 164, method = "2l.lmer")
icel_impt <- complete(imp_icel)
vis_miss(icel_impt)


Ireland <- wwbi_eu %>%  filter(country == "Ireland") %>% droplevels()
vis_miss(Ireland)
sum(is.na(Ireland))
imp_irel <- mice(Ireland, m= 93, method = "2l.lmer")
irel_impt <- complete(imp_irel)
vis_miss(irel_impt)

Lithuania <- wwbi_eu %>%  filter(country == "Lithuania") %>% droplevels()
vis_miss(Lithuania)
sum(is.na(Lithuania))
imp_lith <- mice(Lithuania, m= 91, method = "2l.lmer")
lith_impt <- complete(imp_lith)
vis_miss(irel_lith)

Luxembourg <- wwbi_eu %>%  filter(country == "Luxembourg") %>% droplevels()
vis_miss(Luxembourg)
sum(is.na(Luxembourg))
imp_lux <- mice(Luxembourg, m= 80, method = "2l.lmer")
lux_impt <- complete(imp_lux)
vis_miss(lux_impt)

Poland <- wwbi_eu %>%  filter(country == "Poland") %>% droplevels()
vis_miss(Poland)
sum(is.na(Poland))
imp_pol <- mice(Poland, m= 70, method = "2l.lmer")
pol_impt <- complete(imp_pol)
vis_miss(pol_impt)

Portugal <- wwbi_eu %>%  filter(country == "Portugal") %>% droplevels()
vis_miss(Portugal)
sum(is.na(Portugal))
imp_port <- mice(Portugal, m= 126, method = "2l.lmer")
port_impt <- complete(imp_port)
vis_miss(port_impt)

Romania <- wwbi_eu %>%  filter(country == "Romania") %>% droplevels()
vis_miss(Romania)
sum(is.na(Romania))
imp_rom <- mice(Romania, m= 74, method = "2l.lmer")
rom_impt <- complete(imp_rom)
vis_miss(rom_impt)

Slovakia <- wwbi_eu %>%  filter(country == "Slovak Republic") %>% droplevels()
vis_miss(Slovakia)
sum(is.na(Slovakia))
imp_slov <- mice(Slovakia, m= 108, method = "2l.lmer")
slov_impt <- complete(imp_slov)
vis_miss(slov_impt)


Spain <- wwbi_eu %>%  filter(country == "Spain") %>% droplevels()
vis_miss(Spain)
sum(is.na(Spain))
imp_spain <- mice(Spain, m= 109, method = "2l.lmer")
spain_impt <- complete(imp_spain)
vis_miss(spain_impt)


UK <- wwbi_eu %>%  filter(country == "United Kingdom") %>% droplevels()
sum(is.na(UK))
imp_uk <- mice(UK, m= 119, method = "2l.lmer")
uk_impt <- complete(imp_uk)
vis_miss(uk_impt)

df_list <- list(aut_impt, bel_impt, bul_impt, cro_impt, cyp_impt, czech_impt, 
                esto_impt, finl_impt, france_impt, greece_impt, hung_impt, icel_impt,
                irel_impt, lith_impt, lux_impt, pol_impt, port_impt, rom_impt, slov_impt,
                spain_impt, uk_impt)
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

















