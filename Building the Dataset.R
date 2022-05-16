#### MASTER'S THESIS - CLIMATE CHANGE AND POSITIVE PEACE - A Study on the Effects of Rapid-Onset Climate Change on Positive Peace ####

## ____________________________________________________________________________________ ##
## Install & load following packages 
library(dplyr)
library(vtable)

## ____________________________________________________________________________________ ##
## Set working directory
setwd("C:/Users/....")


## ____________________________________________________________________________________ ##
## Load data Files

#### Dependent Variables ####
#VDEM - Egalitarian Democracy, Civil Liberties
VDEM <- readRDS("V-Dem-CY-Full+Others-v11.1.rds")

VDEM <- subset(VDEM, year >= 1990, 
               select = c(country_name, year, country_text_id,
                          v2x_egaldem, v2x_civlib))

VDEM$year <- as.numeric(VDEM$year)
VDEM$v2x_egaldem <- as.numeric(VDEM$v2x_egaldem)
VDEM$v2x_civlib <- as.numeric(VDEM$v2x_civlib)  

#World Bank - Under-5 mortality
WDI <- read.csv("f888f673-56c0-4f1c-b168-cccbaf90a60f_Data.csv")

WDI <- WDI %>% mutate(year = `?..Time`, shdynmort_wb =`Mortality.rate..under.5..per.1.000.live.births...SH.DYN.MORT.`)%>% 
  subset(year>=1990)%>%
  select(year, Country.Name, Country.Code, shdynmort_wb)%>% 
  mutate(across(everything(), ~na_if(., "..")))

#make variables into numeric ones, to enable merging and delete empty rows
WDI$year <- as.numeric(WDI$year)
WDI <- WDI[!is.na(WDI$year),]
WDI$shdynmort_wb <- as.numeric(WDI$shdynmort_wb)
WDI <- WDI[!is.na(WDI$shdynmort_wb),]

#arrange by year and country 
WDI <- WDI %>% arrange(Country.Name, year)

#To create the percentage increase/decrease: 
##divide child mortality rate of year by year before, subract 1 to have the change in decimals (for percentage)
##Make new variables with shdynmort_wb for each country individually 
WDI <- WDI%>%group_by(Country.Name)%>%mutate(shdynmort_perc=(((shdynmort_wb/lag(
  shdynmort_wb, default=first(shdynmort_wb))-1)*100)))
WDI$shdynmort_perc <- WDI$shdynmort_perc*(-1)


#UCDP
##UCDP-PRIO Dataset
UP <- read.csv("ucdp-prio-acd-211.csv")
UP <- subset(UP, year >= 1990, 
             select = c(location, year, intensity_level))

sum(is.na(UP))


#### Independent Variables ####
library(readxl)
EMDAT <- read_excel("emdat_public_2022_02_10_query_uid-1kYZHr.xlsx")

#Keep the original EM-DAT data for later checks
EMDAT1 <- EMDAT
EMDAT1 <- subset(EMDAT1, Year <= 2020,
                 select = c(`Dis No`, Year, `Disaster Group`, `Disaster Subgroup`, `Disaster Type`,
                            `Disaster Subtype`, Country, ISO))

EMDAT$Year <- as.numeric(EMDAT$Year)

#Subset EM-DAT for merging and hypothesis testing
EMDAT <- subset(EMDAT, Year >= 1990 & Year <= 2020)

##Check for missing values of the variables for the 9312 observations
sum(is.na(EMDAT$`Disaster Subgroup`)) #0
sum(is.na(EMDAT$`Disaster Type`)) #0
sum(is.na(EMDAT$Origin)) #5766 -> too many NAs
sum(is.na(EMDAT$`Disaster Subtype`)) #1605
sum(is.na(EMDAT$`Associated Dis`)) #6561 -> too many NAs
sum(is.na(EMDAT$`Dis Mag Value`)) #6138 -> too many NAs
sum(is.na(EMDAT$`Total Affected`)) #2320
sum(is.na(EMDAT$`Total Damages ('000 US$)`)) #5786 -> too many NAs

EMDAT <- subset(EMDAT, 
                select = c(`Dis No`, Year, `Disaster Group`, `Disaster Subgroup`, `Disaster Type`,
                           `Disaster Subtype`, Country, ISO))

EMDAT$Year <- as.numeric(EMDAT$Year)

#Remove values of fog, landslides, wave action (No disaster recorded from 1900-2022) and glacial lake outbursts from the dataset
EMDAT <- EMDAT %>% filter(`Disaster Type` != "Fog", `Disaster Type` != "Glacial lake outburst", `Disaster Type` != "Landslide")


#Create Dummy Variables of the different kinds of disasters to enable analysis 
#Dummy variables - Disaster Subgroup (Hydrological, Climatological, Meteorological)
EMDAT$Meteoro <- ifelse(EMDAT$`Disaster Subgroup`== 'Meteorological',1,0)
EMDAT$Climato <- ifelse(EMDAT$`Disaster Subgroup`== 'Climatological',1,0)
EMDAT$Hydro <- ifelse(EMDAT$`Disaster Subgroup`== 'Hydrological',1,0)

#Dummy variables - Disaster Type 
##Definitely Extreme Temperature, Storms, Floods, Drought, Fires
EMDAT$ExTemp <- ifelse(EMDAT$`Disaster Type`== 'Extreme temperature',1,0) 
EMDAT$Storm <- ifelse(EMDAT$`Disaster Type`== 'Storm',1,0)
EMDAT$Flood <- ifelse(EMDAT$`Disaster Type`== 'Flood',1,0)
EMDAT$Drought <- ifelse(EMDAT$`Disaster Type`== 'Drought',1,0)
EMDAT$Fire <- ifelse(EMDAT$`Disaster Type`== 'Wildfire',1,0)

#### Control Variables ####

#aid and income 
WDI2 <- read.csv("7dbfd909-a0cd-4277-8bf0-fc03d1c7d63f_Data.csv")

WDI2 <- WDI2 %>% mutate(year = `?..Time`, ODAus18 = `Net.official.development.assistance.received..constant.2018.US....DT.ODA.ODAT.KD.`,
                        InUSCur = `Adjusted.net.national.income.per.capita..current.US....NY.ADJ.NNTY.PC.CD.`,)%>% 
  select(year, Country.Name, Country.Code, ODAus18, InUSCur)%>% 
  mutate(across(everything(), ~na_if(., "..")))

#make variables into numeric ones, to enable merging and delete empty rows
WDI2$year <- as.numeric(WDI2$year)
WDI2 <- WDI2[!is.na(WDI2$year),]
WDI2$ODAus18 <- as.numeric(WDI2$ODAus18)
WDI2$InUSCur <- as.numeric(WDI2$InUSCur)

sum(is.na(WDI2$ODAus18))#2209
sum(is.na(WDI2$InUSCur))#1522

#arrange by year and country 
WDI2 <- WDI2 %>% arrange(Country.Name, year)

#To create the percentage increase/decrease: 
##divide income of year by year before, subract 1 to have the change in decimals (for percentage)
WDI2 <- WDI2%>%group_by(Country.Name)%>%mutate(InUSCur_perc=(((InUSCur/lag(
  InUSCur, default=first(InUSCur))-1)*100)))

#Making the ODAus18 variable into showing the millions 
WDI2 <- WDI2%>%group_by(Country.Name)%>%mutate(ODAus18_mil=(ODAus18/1000000))


#Creating the percentage change in received ODAus18 variable
#normal percentage change variable leads to wrong results, as there are negative values in the ODA variable
WDI2 <- WDI2%>%group_by(Country.Name)%>%mutate(ODAus18_perc=(((ODAus18/lag(
  ODAus18, default=first(ODAus18))-1)*100)))

#Try with leaving out the negative values by making them a 0 and creating a dummy variable with 1 for the 0s on the variable
WDI2 <- WDI2 %>% mutate(ODAus181 = replace(ODAus18, ODAus18<0, 0))
WDI2 <- WDI2%>%group_by(Country.Name)%>%mutate(ODAus18_perc_woNeg1=(((ODAus181/lag(
  ODAus181, default=first(ODAus181))-1)*100)))
WDI2$NoNegatives <- ifelse(WDI2$ODAus181== 0,1,0)

#End up with infinite values, thereby will make infinite values to 0 and add a new dummy for this 
WDI2 <- WDI2 %>% mutate(ODAus18_perc_woNeg = replace(ODAus18_perc_woNeg1, ODAus18_perc_woNeg1 == "Inf", 0)) %>%  
  mutate(ODAus18_perc_woNeg = replace(ODAus18_perc_woNeg, ODAus18_perc_woNeg == "NaN", 0))
WDI2$NoInfinitivesOrNegatives <- ifelse(WDI2$ODAus18_perc_woNeg== 0,1,0)

sum(is.na(WDI2$ODAus18_perc_woNeg))#2216 of 7182 
Missings <- subset(WDI2, WDI2$NoInfinitivesOrNegatives == 1)
#281 observations furhter missing from the 4966 left ones when ODA change variable is created
#Only around 65% of the observations left

#gross insurance claims payments 
OECD <- read.csv("PT7_11032022101923138.csv")

OECD <- OECD %>% subset(Ownership == "All undertakings (=1+3)" & DBRA == "TOT" & ITYP == "TOT") %>% 
  mutate(GIC_mil = `Value`, ISO = `?..COU`) %>% 
  select(Country, Year, GIC_mil, ISO)

#arrange by year and country 
OECD <- OECD %>% arrange(Country, Year)


#### Merging of all the DV and IV data ####
DVs <- left_join(VDEM, WDI, by = c("year", "country_name"="Country.Name", "country_text_id"="Country.Code"))
DVs <- left_join(DVs, UP, by = c("year", "country_name"="location"))
Control <- left_join(WDI2, OECD, by = c("year"="Year", "Country.Name"="Country", "Country.Code"="ISO"))
DV_C <- left_join(DVs, Control, by = c("year", "country_name"="Country.Name", "country_text_id"="Country.Code"))
Data <- left_join(DV_C, EMDAT, by = c("year"="Year", "country_name"="Country", "country_text_id"="ISO"))

#Create Dummy Variables of the different kinds of disasters to enable analysis 
Data <- mutate_at(Data, c("Meteoro", "Climato", "Hydro",
                          "ExTemp", "Storm", "Flood", 
                          "Drought", "Fire"), ~replace(., is.na(.), 0))


Data$Group <- ifelse(is.na(Data$`Disaster Group`), 0, 1)

#Create a conflict dummy - if conflict occured, then 1, otherwise 0 
#(all NAs = 0, due to variable not having any NAs in original datasaet)
Data$conflict <- ifelse(is.na(Data$intensity_level), 1, 0)

#Create a new dataset for the lagged test 
#(need 1994 to create the lag, therefore need to do this before delimiting my timeframe)
Data_lag <- Data %>% subset(year >= 1994)

##Due to Child mortality data starting in 1990, 
##the inclusion of 1990 with a 0 on the percentage of incerase/decrease would skew the results
##therefore, need to subset to only include past 30 years (1991-2020)

Data <- Data %>% subset(year >= 1995)

#arrange data by country and year
Data <- Data %>% arrange(country_name, year)


#Number of NAs
sum(is.na(Data))#54712 out of 318580
sum(is.na(Data$country_text_id)) #0

#DVs
sum(is.na(Data$v2x_egaldem)) #0
sum(is.na(Data$v2x_civlib))#0
sum(is.na(Data$shdynmort_perc)) #691
sum(is.na(Data$conflict)) #0


#IVs
sum(is.na(Data$`Disaster Group`)) #2576
sum(is.na(Data$`Disaster Subgroup`)) #2576
sum(is.na(Data$`Disaster Type`)) #2576

#Control Variables
#income growth
sum(is.na(Data$InUSCur_perc))#1423

#aid
sum(is.na(Data$ODAus18_mil))#2725
sum(is.na(Data$ODAus18_perc_woNeg))#2733
sum(is.na(Data$NoInfinitivesOrNegatives))#2733

#How many missings when aid is merged 
ODA <- subset(Data, Data$NoInfinitivesOrNegatives == 1) #287 missings, 
#thereby only 6637 observations left when dummying out the negatives, infinitives, and unknown

#Gross insurance claims payments 
sum(is.na(Data$GIC_mil)) #7465


#Infos about the variables 
typeof(Data$country_name)
typeof(Data$year)

## ____________________________________________________________________________________ ##
##Exporting this dataset
write.csv(Data,"C:/Users/..../thesis_dataset_final.csv", row.names = TRUE)

