#### CLIMATE CHANGE AND POSITIVE PEACE - A Study on the Effects of Rapid-Onset Climate Change on Positive Peace ####

### Replication code for the Master's Thesis at the Department of Peace and Conflict Research at Uppsala University
### Date: 05 19, 2022
### Author: Patt, Kristiane

## ____________________________________________________________________________________ ##
## Install & load following packages 
library(dplyr)
library(xtable)
library(car)
library(ggplot2)
library(ggpubr)
library(extrafont)
font_import
loadfonts(device = "win")
library(vtable)
library(rworldmap)
library(classInt)
library(RColorBrewer)
library(purrr)
library(broom)
library(sandwich)
library(lmtest)

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

WDI <- WDI %>% mutate(year = `Time`, shdynmort_wb =`Mortality.rate..under.5..per.1.000.live.births...SH.DYN.MORT.`)%>% 
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

WDI2 <- WDI2 %>% mutate(year = `Time`, ODAus18 = `Net.official.development.assistance.received..constant.2018.US....DT.ODA.ODAT.KD.`,
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
  mutate(GIC_mil = `Value`, ISO = `COU`) %>% 
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

  ##Exporting this dataset for later analysis
  write.csv(Data,"C:/Users/..../thesis_dataset_lagged.csv", row.names = TRUE)


##Due to Child mortality data starting in 1990, 
##the inclusion of 1990 with a 0 on the percentage of incerase/decrease would skew the results
##therefore, need to subset to only include past 30 years (1991-2020)

Data <- Data %>% subset(year >= 1995)

#arrange data by country and year
Data <- Data %>% arrange(country_name, year)


#Number of NAs
sum(is.na(Data))#54316 out of 318580
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
## ____________________________________________________________________________________ ##


#### Visualisations ####

##### Tables, Graphs, Plots, and Matrices ####
## ____________________________________________________________________________________ ##
#summary table for the dependent variables 
sum_DV <- subset(Data, select=c(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict))

st(sum_DV,
   summ = c('min(x)', 'median(x)','mean(x)','max(x)','notNA(x)'),
   summ.names = c('Min', 'Median','Mean','Max','N'), 
   title = "Table B 1: Summary statistics of the Dependent Variables",
   file = "Table_B_1.html",  digits=3
)

#summary table for the controls 
sum_control_mil <- subset(Data, select=c(InUSCur_perc, ODAus18_mil, GIC_mil)) 

st(sum_control_mil,
   summ = c('min(x)', 'median(x)','mean(x)','max(x)','notNA(x)'),
   summ.names = c('Min', 'Median','Mean','Max','N'), 
   title = "Summary statistics of the Control Variables",
   file = "Table_Controls_ODA-MIL.html",  digits=3
)

sum_control_perc <- subset(Data, select=c(InUSCur_perc, ODAus18_perc_woNeg, GIC_mil)) 

st(sum_control_perc,
   summ = c('min(x)', 'median(x)','mean(x)','max(x)','notNA(x)'),
   summ.names = c('Min', 'Median','Mean','Max','N'), 
   title = "Table B 2: Summary statistics of the Control Variables",
   file = "Table_B_2.html",  digits=3
)

## ____________________________________________________________________________________ ##
##Graphs
#Occurrence 1990-2020 in EM-DAT 
EMDAT2 <- subset(EMDAT, Year >= 1995)
EMDAT2$Group <- ifelse(is.na(EMDAT2$`Disaster Group`), 0, 1)
Occurrence <- EMDAT2 %>% group_by(Year, Group) %>% tally() %>% subset(Group == 1)
sum(Occurrence$n)
summary(Occurrence$n, digits = 4)

NatHa <- ggplot() + ggtitle("Natural Disaster Occurrence 1990-2020 (EM-DAT only)") +
  geom_line(data = Occurrence, aes(x=Year, y=n), 
            colour = "steelblue4")

png("NatHa_EM-DAT.png", width = 700, height = 500)
print(NatHa + labs(x="Year", y = "Number of Natural Disasters") +
        theme(text=element_text(size=20, family="serif")))
dev.off()

#Occurrence Natural Disasters 1900-2020
EMDAT1$Group <- ifelse(is.na(EMDAT1$`Disaster Group`), 0, 1)
Occurrence_b <- EMDAT1 %>% group_by(Year, Group)%>% tally() %>% subset(Group == 1)
Occurrence_b$Year <- as.numeric(Occurrence_b$Year)
sum(Occurrence_b$n)
summary(Occurrence_b$n, digits = 4)

NatHa_b <- ggplot() + ggtitle("Figure 4: Natural Disaster Occurrence 1900-2020") +
  geom_line(data = Occurrence_b, aes(x=Year, y=n), 
            colour = "cadetblue4") +
  scale_x_continuous(expand=expansion(mult=c(0,0.075)))  

png("Figure_4.png", width = 700, height = 500)
print(NatHa_b + labs(x="Year", y = "Occurrence") +
        theme(text=element_text(size=20, family="serif")), width=15, height=5)
dev.off()

#Occurrence Natural Disasters after merging with other data 
Occurrence_d <- Data %>% group_by(year, Group)%>% tally() %>% subset(Group == 1)
sum(Occurrence_d$n)
summary(Occurrence_d$n, digits = 4)

NatHa_d <- ggplot() + ggtitle("Figure 8: Natural Disaster Occurrence 1995-2020") +
  geom_line(data = Occurrence_d, aes(x=year, y=n), 
            colour = "cadetblue4")

png("Figure_8.png", width = 700, height = 500)
print(NatHa_d + labs(x="Year", y = "Number of Natural Disasters") +
        theme(text=element_text(size=20, family="serif")))
dev.off()

#Disasters by Subgroup - Figure 10 and Table B 3
Meteoro <- Data %>% group_by(year, Meteoro) %>% tally() %>% subset(Meteoro == 1)
print(summary(Meteoro$n), digits = 3)
Climato <- Data %>% group_by(year, Climato) %>% tally() %>% subset(Climato == 1)
print(summary(Climato$n), digits = 2)
Hydro <- Data %>% group_by(year, Hydro) %>% tally() %>% subset(Hydro == 1)
print(summary(Hydro$n), digits = 2)

Subgroup_Occ <- ggplot() + ggtitle("Figure 10: Natural Disaster Occurrence by Subgroup") +
  geom_line(data = Meteoro, aes(x=year, y=n, colour = "Meteorological")) + 
  geom_line(data = Climato, aes(x=year, y=n, colour = "Climatological")) +
  geom_line(data = Hydro, aes(x=year, y=n,  colour = "Hydrological")) +
  scale_color_manual(name = "Subgroup", values = c("Meteorological" = "steelblue4", 
                                                   "Climatological" = "aquamarine3",
                                                   "Hydrological" = "sienna4"))

png("Figure_10.png", width = 700, height = 500)
print(Subgroup_Occ + labs(x="Year", y = "Number of Natural Disasters")+
        theme(legend.position = "bottom", text=element_text(size=20, family="serif")))
dev.off()

#Disasters by Type - Figure 11 and Table B 4
ExTemp <- Data %>% group_by(year, ExTemp) %>% tally() %>% subset(ExTemp == 1)
print(summary(ExTemp$n), digits = 2)
Storm <- Data %>% group_by(year, Storm) %>% tally() %>% subset(Storm == 1)
print(summary(Storm$n), digits = 2)
Flood <- Data %>% group_by(year, Flood) %>% tally() %>% subset(Flood == 1)
print(summary(Flood$n), digits = 2)
Drought <- Data %>% group_by(year, Drought) %>% tally() %>% subset(Drought == 1)
print(summary(Drought$n), digits = 2)
Fire <- Data %>% group_by(year, Fire) %>% tally() %>% subset(Fire == 1)
print(summary(Fire$n), digits = 1)

Type_Occ <- ggplot() + ggtitle("Figure 11: Natural Disaster Occurrence by Type") +
  geom_line(data = ExTemp, aes(x=year, y=n, 
                               colour = "Extreme Temperature")) + 
  geom_line(data = Storm, aes(x=year, y=n, 
                              colour = "Storm")) +
  geom_line(data = Flood, aes(x=year, y=n, 
                              colour = "Flood")) + 
  geom_line(data = Drought, aes(x=year, y=n, 
                                colour = "Drought")) +
  geom_line(data = Fire, aes(x=year, y=n, 
                             colour = "Fire"))  +
  scale_color_manual(name = "Type", values = c("Extreme Temperature" = "steelblue4", 
                                               "Storm" = "aquamarine3", 
                                               "Flood" = "sienna4",
                                               "Drought" = "darkgoldenrod",
                                               "Fire" = "chartreuse4"))

png("Figure_11.png", width = 700, height = 500)
print(Type_Occ + labs(x="Year", y = "Number of Natural Disasters")+
        theme(legend.position = "bottom", text=element_text(size=20, family="serif")))
dev.off()

## ____________________________________________________________________________________ ##
#Scatter Plots 
SP_egaldem <- ggplot(Data, aes(x=year, y=v2x_egaldem)) + 
  geom_point()

SP_civlib <- ggplot(Data, aes(x=year, y=v2x_civlib)) + 
  geom_point()

SP_shdynmort_perc <- ggplot(Data, aes(x=year, y=shdynmort_perc)) + 
  geom_point()

SP_InUSCur_perc <- ggplot(Data, aes(x=year, y=InUSCur_perc)) + 
  geom_point() + labs(x="Year", y= "Income per Capita Change in Percent",
                      title="Income per Capita Change from 1995 to 2020") +
  theme_bw(base_size = 16) + theme(text = element_text(family = "serif"))

SP_ODAus18_mil <- ggplot(Data, aes(x=year, y=ODAus18_mil)) + 
  geom_point() + labs(x="Year", y= "ODA in Millions",
                      title="ODA in Millions from 1995 to 2020") +
  theme_bw(base_size = 16) + theme(text = element_text(family = "serif"))

SP_ODAus18_perc <- ggplot(Data, aes(x=year, y=ODAus18_perc_woNeg)) + 
  geom_point() + labs(x="Year", y= "ODA Change",
                      title="ODA Change without negatives and infinitive values from 1995 to 2020") +
  theme_bw(base_size = 16) + theme(text = element_text(family = "serif"))

#Output Scatter Plot Control - Figure 12
png("Figure_12.png", width = 800, height = 400) 

SP_InUSCur_perc

dev.off()

## ____________________________________________________________________________________ ##
#Boxplots
png("Figure_6.png", width = 800, height = 700) 

par(mfrow=c(2,2), family = "serif", cex.axis=1.5, cex.lab = 1.75)

ED_BP <- Boxplot(v2x_egaldem ~ Group, data = Data, ylab = "Egalitarian Democracy Index", xlab="Natural Disaster Group", id=FALSE)
CL_BP <- Boxplot(v2x_civlib ~ Group, data=Data, ylab = "Civil Liberties Index", xlab="Natural Disaster Group", id=FALSE)
CMPerc_BP <- Boxplot(shdynmort_perc ~ Group, data = Data, ylab = "Child Mortality Change", xlab="Natural Disaster Group", id=FALSE)
mtext("Figure 6: Boxplot of Dependent Variables and Natural Disaster Occurrence", side=3,line=-1.5,outer=TRUE, cex=1.75)

dev.off()

## ____________________________________________________________________________________ ##
#correlation matrix
Variables_y<- Data %>% select(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict)

Cor <- round(cor(Variables_y, use = "complete.obs"),3)
Cor[lower.tri(Cor)]<-""
Cor <- as.data.frame(Cor)


colnames(Cor) = c("1. Egalitarian democracy index", "2. Civil liberties index", 
                  "3. Child Mortality Change", "4. Conflict")
row.names(Cor) = c("1.", "2.", "3.", "4.")

print(xtable(Cor, digits = 3, caption = "Table 5: Correlations between dependent variables"),
      type="html", file = "Table_5.html", caption.placement = "top", html.table.attributes = getOption("border=0.5"))

#Correlation - IVs 
Variables_x1 <- Data %>% select(Meteoro, Climato, Hydro)
Cor1 <- round(cor(Variables_x1, use = "complete.obs"), 3)
Cor1[lower.tri(Cor1)]<-""
Cor1 <- as.data.frame(Cor1)

colnames(Cor1) = c("Meteorological", "Climatological", "Hydrological")
row.names(Cor1) = c("Meteorological", "Climatological", "Hydrological")

print(xtable(Cor1, digits = 3, caption = "Table 3: Correlations between independent variables (Subgroup)"),
      type="html", file = "Table_3.html", caption.placement = "top", html.table.attributes = getOption("border=0.5"))


Variables_x2 <- Data %>% select(ExTemp, Storm, Flood, Drought, Fire)
Cor2 <- round(cor(Variables_x2, use = "complete.obs"),3)
Cor2[lower.tri(Cor2)]<-""
Cor2 <- as.data.frame(Cor2)

colnames(Cor2) = c("Extreme Temperature", "Storm", "Flood", "Drought", "Fire")
row.names(Cor2) = c("Extreme Temperature", "Storm", "Flood", "Drought", "Fire")

print(xtable(Cor2, digits = 3, caption = "Table 4: Correlations between independent variables (Type)"),
      type="html", file = "Table_4.html", caption.placement = "top", html.table.attributes = getOption("border=0.5"))


##### Maps ##### 
colourPalette <- brewer.pal(7,'RdPu')

spdf <- joinCountryData2Map(VDEM, joinCode="ISO3", nameJoinColumn="country_text_id")

#Dependent Variables 
#Individual Maps 
png("EDI.png", width = 800, height = 800, pointsize = 20, family = "serif")
v2x_egaldem <- mapCountryData(spdf, 
                              nameColumnToPlot="v2x_egaldem", 
                              addLegend=FALSE,
                              catMethod="fixedWidth", 
                              colourPalette=colourPalette, 
                              mapTitle='Egalitarian Democracy Index')
do.call(addMapLegend, 
        c(v2x_egaldem, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()


png("CLI.png", width = 800, height = 800)
v2x_civlib <- mapCountryData(spdf, 
                             nameColumnToPlot="v2x_civlib", 
                             addLegend=FALSE,
                             catMethod="fixedWidth", 
                             colourPalette=colourPalette, 
                             mapTitle='Civil Liberties Index')
do.call(addMapLegend, 
        c(v2x_civlib, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

spdf1 <- joinCountryData2Map(WDI, joinCode="ISO3", nameJoinColumn="Country.Code")

png("CMP.png", width = 800, height = 800)
shdynmort_perc <- mapCountryData(spdf1, 
                                 nameColumnToPlot="shdynmort_perc", 
                                 addLegend=FALSE,
                                 catMethod=c(-10:10), 
                                 colourPalette=colourPalette, 
                                 mapTitle='Child Mortality Change')
do.call(addMapLegend, 
        c(shdynmort_perc, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()


Conflict_OT <- UP %>% group_by(location) %>% tally()
print(summary(Conflict_OT$n), digits = 3)


spdf2 <- joinCountryData2Map(Conflict_OT, joinCode="NAME", nameJoinColumn="location")

png("CO.png", width = 800, height = 800)
conflict <- mapCountryData(spdf2, 
                           nameColumnToPlot="n", 
                           addLegend=FALSE,
                           catMethod="fixedWidth", 
                           colourPalette=colourPalette, 
                           mapTitle='Conflict Occurrence')
do.call(addMapLegend, 
        c(conflict, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()


#One Output
png("Figure_7.png", width = 900, height = 800) 

par(mfrow=c(2,2), family = "serif", cex.axis=1.5, cex.lab = 1.75)

v2x_egaldem <- mapCountryData(spdf, 
                              nameColumnToPlot="v2x_egaldem", 
                              addLegend=FALSE,
                              catMethod="fixedWidth", 
                              colourPalette=colourPalette, 
                              mapTitle='Egalitarian Democracy Index')
do.call(addMapLegend, 
        c(v2x_egaldem, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))


v2x_civlib <- mapCountryData(spdf, 
                             nameColumnToPlot="v2x_civlib", 
                             addLegend=FALSE,
                             catMethod="fixedWidth", 
                             colourPalette=colourPalette, 
                             mapTitle='Civil Liberties Index')
do.call(addMapLegend, 
        c(v2x_civlib, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

shdynmort_perc <- mapCountryData(spdf1, 
                                 nameColumnToPlot="shdynmort_perc", 
                                 addLegend=FALSE,
                                 catMethod=c(-10:10), 
                                 colourPalette=colourPalette, 
                                 mapTitle='Child Mortality Change')
do.call(addMapLegend, 
        c(shdynmort_perc, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

conflict <- mapCountryData(spdf2, 
                           nameColumnToPlot="n", 
                           addLegend=FALSE,
                           catMethod="fixedWidth", 
                           colourPalette=colourPalette, 
                           mapTitle='Conflict Occurrence')
do.call(addMapLegend, 
        c(conflict, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))
dev.off()

## ____________________________________________________________________________________ ##
#Independent Variables 
colourPalette2 <- brewer.pal(7,'YlOrRd')

#Natural Disaster Group
Occ_Country <- Data %>% group_by(Group, country_name, country_text_id) %>% tally() %>% subset(Group==1)
spdf3 <- joinCountryData2Map(Occ_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Figure_9.png", width = 800, height = 800, pointsize = 20, family = "serif")
Occ_Country_OT <- mapCountryData(spdf3, 
                                 nameColumnToPlot="n", 
                                 addLegend=FALSE,
                                 catMethod="fixedWidth", 
                                 colourPalette=colourPalette2, 
                                 mapTitle='Occurrence of Natural Disasters')
do.call(addMapLegend, 
        c(Occ_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

## ____________________________________________________________________________________ ##
#Subgroup
Meteoro_Country <- Data %>% group_by(Meteoro, country_name, country_text_id) %>% tally() %>% subset(Meteoro==1)
spdf4 <- joinCountryData2Map(Meteoro_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Meteoro_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Meteoro_Country_OT <- mapCountryData(spdf4, 
                                     nameColumnToPlot="n", 
                                     addLegend=FALSE,
                                     catMethod="fixedWidth", 
                                     colourPalette=colourPalette2, 
                                     mapTitle='Meteorological Natural Disasters')
do.call(addMapLegend, 
        c(Meteoro_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

Climato_Country <- Data %>% group_by(Climato, country_name, country_text_id) %>% tally() %>% subset(Climato==1)
spdf5 <- joinCountryData2Map(Climato_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Meteoro_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Climato_Country_OT <- mapCountryData(spdf5, 
                                     nameColumnToPlot="n", 
                                     addLegend=FALSE,
                                     catMethod="fixedWidth", 
                                     colourPalette=colourPalette2, 
                                     mapTitle='Climatological Natural Disasters')
do.call(addMapLegend, 
        c(Climato_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

Hydro_Country <- Data %>% group_by(Hydro, country_name, country_text_id) %>% tally() %>% subset(Hydro==1)
spdf6 <- joinCountryData2Map(Hydro_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Hydro_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Hydro_Country_OT <- mapCountryData(spdf6, 
                                   nameColumnToPlot="n", 
                                   addLegend=FALSE,
                                   catMethod="fixedWidth", 
                                   colourPalette=colourPalette2, 
                                   mapTitle='Hydrological Natural Disasters')
do.call(addMapLegend, 
        c(Hydro_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

#One Output
png("Figure_C_1.png", width = 900, height = 800) 

par(mfrow=c(2,2), family = "serif", cex.axis=1.5, cex.lab = 1.75)

Meteoro_Country_OT <- mapCountryData(spdf4, 
                                     nameColumnToPlot="n", 
                                     addLegend=FALSE,
                                     catMethod="fixedWidth", 
                                     colourPalette=colourPalette2, 
                                     mapTitle='Meteorological Natural Disasters')
do.call(addMapLegend, 
        c(Meteoro_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 15.25))

Climato_Country_OT <- mapCountryData(spdf5, 
                                     nameColumnToPlot="n", 
                                     addLegend=FALSE,
                                     catMethod="fixedWidth", 
                                     colourPalette=colourPalette2, 
                                     mapTitle='Climatological Natural Disasters')
do.call(addMapLegend, 
        c(Climato_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 15.25))

Hydro_Country_OT <- mapCountryData(spdf6, 
                                   nameColumnToPlot="n", 
                                   addLegend=FALSE,
                                   catMethod="fixedWidth", 
                                   colourPalette=colourPalette2, 
                                   mapTitle='Hydrological Natural Disasters')
do.call(addMapLegend, 
        c(Hydro_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 15.25))

dev.off()

## ____________________________________________________________________________________ ##
#Type 
ExTemp_Country <- Data %>% group_by(ExTemp, country_name, country_text_id) %>% tally() %>% subset(ExTemp==1)
spdf7 <- joinCountryData2Map(ExTemp_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("ExTemp_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
ExTemp_Country_OT <- mapCountryData(spdf7, 
                                    nameColumnToPlot="n", 
                                    addLegend=FALSE,
                                    catMethod="fixedWidth", 
                                    colourPalette=colourPalette2, 
                                    mapTitle='Extreme Temperatures')
do.call(addMapLegend, 
        c(ExTemp_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

Storm_Country <- Data %>% group_by(Storm, country_name, country_text_id) %>% tally() %>% subset(Storm==1)
spdf8 <- joinCountryData2Map(Storm_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Storm_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Storm_Country_OT <- mapCountryData(spdf8, 
                                   nameColumnToPlot="n", 
                                   addLegend=FALSE,
                                   catMethod="fixedWidth", 
                                   colourPalette=colourPalette2, 
                                   mapTitle='Storms')
do.call(addMapLegend, 
        c(Storm_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

Flood_Country <- Data %>% group_by(Flood , country_name, country_text_id) %>% tally() %>% subset(Flood ==1)
spdf9 <- joinCountryData2Map(Flood_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Flood_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Flood_Country_OT <- mapCountryData(spdf9, 
                                   nameColumnToPlot="n", 
                                   addLegend=FALSE,
                                   catMethod="fixedWidth", 
                                   colourPalette=colourPalette2, 
                                   mapTitle='Floods')
do.call(addMapLegend, 
        c(Flood_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

Drought_Country <- Data %>% group_by(Drought, country_name, country_text_id) %>% tally() %>% subset(Drought==1)
spdf10 <- joinCountryData2Map(Drought_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Drought_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Drought_Country_OT <- mapCountryData(spdf10, 
                                     nameColumnToPlot="n", 
                                     addLegend=FALSE,
                                     catMethod="fixedWidth", 
                                     colourPalette=colourPalette2, 
                                     mapTitle='Droughts')
do.call(addMapLegend, 
        c(Drought_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

Fire_Country <- Data %>% group_by(Fire, country_name, country_text_id) %>% tally() %>% subset(Fire==1)
spdf11 <- joinCountryData2Map(Fire_Country, joinCode="ISO3", nameJoinColumn="country_text_id")

png("Fire_Country.png", width = 800, height = 800, pointsize = 20, family = "serif")
Fire_Country_OT <- mapCountryData(spdf11, 
                                  nameColumnToPlot="n", 
                                  addLegend=FALSE,
                                  catMethod="fixedWidth", 
                                  colourPalette=colourPalette2, 
                                  mapTitle='Fires')
do.call(addMapLegend, 
        c(Fire_Country_OT, legendLabels="all",
          legendWidth=0.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

#One Output
png("Figure_C_2.png", width = 900, height = 800) 

par(mfrow=c(3,2), family = "serif", cex.axis=1.5, cex.lab = 1.75)

ExTemp_Country_OT <- mapCountryData(spdf7, 
                                    nameColumnToPlot="n", 
                                    addLegend=FALSE,
                                    catMethod="fixedWidth", 
                                    colourPalette=colourPalette2, 
                                    mapTitle='Extreme Temperatures')
do.call(addMapLegend, 
        c(ExTemp_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

Storm_Country_OT <- mapCountryData(spdf8, 
                                   nameColumnToPlot="n", 
                                   addLegend=FALSE,
                                   catMethod="fixedWidth", 
                                   colourPalette=colourPalette2, 
                                   mapTitle='Storms')
do.call(addMapLegend, 
        c(Storm_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

Flood_Country_OT <- mapCountryData(spdf9, 
                                   nameColumnToPlot="n", 
                                   addLegend=FALSE,
                                   catMethod="fixedWidth", 
                                   colourPalette=colourPalette2, 
                                   mapTitle='Floods')
do.call(addMapLegend, 
        c(Flood_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

Drought_Country_OT <- mapCountryData(spdf10, 
                                     nameColumnToPlot="n", 
                                     addLegend=FALSE,
                                     catMethod="fixedWidth", 
                                     colourPalette=colourPalette2, 
                                     mapTitle='Droughts')
do.call(addMapLegend, 
        c(Drought_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

Fire_Country_OT <- mapCountryData(spdf11, 
                                  nameColumnToPlot="n", 
                                  addLegend=FALSE,
                                  catMethod="fixedWidth", 
                                  colourPalette=colourPalette2, 
                                  mapTitle='Fires')
do.call(addMapLegend, 
        c(Fire_Country_OT, legendLabels="all",
          legendWidth=1.5, 
          legendIntervals="data", 
          legendMar = 10.25))

dev.off()

## ____________________________________________________________________________________ ##
##Exporting this dataset
write.csv(Data,"C:/Users/..../thesis_dataset_final.csv", row.names = TRUE)

rm(list=ls()) 

#### Analysis #### 

## ____________________________________________________________________________________ ##
## Load Datasets

Data <- read.csv("thesis_dataset_final.csv")
Data_lag <- read.csv("thesis_dataset_lagged.csv")

## ____________________________________________________________________________________ ##

#Performing multivariate multiple regression (MMR) in R requires wrapping the multiple responses in the cbind() function. 
#cbind() takes two vectors, or columns, and "binds" them together into two columns of data

#Summary shows the results of two regressions -> exactly the same results we would get if modeled each separately.
#All results of the following MMRs are portrayed in tables in Appendix D and E.

#Natural Disaster Group
Group <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ Group, data = Data)
print(Group, digits = 2) 
print(summary(Group), digits=3)

coef_Group <- tidy(Group, conf.int = TRUE,  conf.level = 0.95)
coef_Group

png("Figure_D_1.png", width = 800, height = 600)
coef_Group %>% 
  mutate(response = factor(response, levels=c("v2x_egaldem", "v2x_civlib", "shdynmort_perc", "conflict"), 
                           labels=c("Egalitarian Democracy", "Civil Liberties", 
                                    "Child Mortality Change", "Conflict Occurrence")),
         term = factor(term, levels=c("(Intercept)", "Group"),
                       labels=c("(Intercept)", "Natural Disaster"))) %>% 
  ggplot(aes(x=estimate, y=term, colour=response)) + 
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  labs(x="Estimate", y="", colour="Model") + scale_color_hue(l=50, c=120) + 
  theme_bw() +
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

# Robust standard errors for Natural Disaster Group
Group_robust <- coeftest(Group, vcov =  vcovCL, cluster=~country_name)

coef_Group2 <- tidy(Group_robust, conf.int = TRUE,  conf.level = 0.95)
coef_Group2

png("Figure_E_1.png", width = 800, height = 600)
coef_Group2 %>% 
  mutate(term = factor(term, levels=c("v2x_egaldem:(Intercept)", "v2x_egaldem:Group",
                                      "v2x_civlib:(Intercept)", "v2x_civlib:Group",
                                      "shdynmort_perc:(Intercept)", "shdynmort_perc:Group", 
                                      "conflict:(Intercept)", "conflict:Group"), 
                       labels=c("Egalitarian Democracy, Intercept", "Egalitarian Democracy, Natural Disaster",
                                "Civil Liberties, Intercept", "Civil Liberties, Natural Disaster",
                                "Child Mortality Change, Intercept", "Child Mortality Change, Natural Disaster", 
                                "Conflict Occurrence, Intercept", "Conflict Occurrence, Natural Disaster"))) %>% 
  ggplot(aes(x=estimate, y=term)) + 
  labs(x="Estimate", y="") +
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

##Subgroup
Subgroup <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                 Meteoro + Climato + Hydro,  data = Data)
print(Subgroup, digits = 2)
print(summary(Subgroup), digits=3)


coef_Subgroup <- tidy(Subgroup, conf.int = TRUE,  conf.level = 0.95)
coef_Subgroup


png("Figure_D_2.png", width = 800, height = 600)
coef_Subgroup %>% 
  mutate(response = factor(response, levels=c("v2x_egaldem", "v2x_civlib", "shdynmort_perc", "conflict"), 
                           labels=c("Egalitarian Democracy", "Civil Liberties", 
                                    "Child Mortality Change", "Conflict Occurrence")),
         term = factor(term, levels=c("(Intercept)", "Climato", "Hydro", "Meteoro"),
                       labels=c("(Intercept)", "Climatlogical", "Hydrological", "Meteorological"))) %>% 
  ggplot(aes(x=estimate, y=term, colour=response)) + 
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  labs(x="Estimate", y="", colour="Model") + scale_color_hue(l=50, c=120) +
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

# Robust standard errors for Subgroup
Subgroup_robust <- coeftest(Subgroup, vcov =  vcovCL, cluster=~country_name)
coef_Subgroup2 <- tidy(Subgroup_robust, conf.int = TRUE,  conf.level = 0.95)
coef_Subgroup2

png("Figure_E_2.png", width = 800, height = 600)
coef_Subgroup2 %>% 
  mutate(term = factor(term, levels=c("v2x_egaldem:(Intercept)", "v2x_egaldem:Meteoro", "v2x_egaldem:Climato", "v2x_egaldem:Hydro",
                                      "v2x_civlib:(Intercept)", "v2x_civlib:Meteoro", "v2x_civlib:Climato", "v2x_civlib:Hydro",
                                      "shdynmort_perc:(Intercept)", "shdynmort_perc:Meteoro", "shdynmort_perc:Climato", "shdynmort_perc:Hydro", 
                                      "conflict:(Intercept)", "conflict:Meteoro", "conflict:Climato", "conflict:Hydro"), 
                       labels=c("Egalitarian Democracy, Intercept", "Egalitarian Democracy, Meteorological", "Egalitarian Democracy, Climatological", "Egalitarian Democracy, Hydrological",
                                "Civil Liberties, Intercept", "Civil Liberties, Meteorological", "Civil Liberties, Climatological", "Civil Liberties, Hydrological",
                                "Child Mortality Change, Intercept", "Child Mortality Change, Meteorological", "Child Mortality Change, Climatological", "Child Mortality Change, Hydrological", 
                                "Conflict Occurrence, Intercept", "Conflict Occurrence, Meteorological", "Conflict Occurrence, Climatological", "Conflict Occurrence, Hydrological"))) %>% 
  ggplot(aes(x=estimate, y=term)) + 
  labs(x="Estimate", y="") +
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

##Type
Type <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
             ExTemp + Storm + Flood +  Drought + Fire,  data = Data)
print(Type, digits=2)
print(summary(Type), digits=3)

coef_Type <- tidy(Type, conf.int = TRUE)
coef_Type

png("Figure_D_3.png", width = 900, height = 600)
coef_Type %>% 
  mutate(response = factor(response, levels=c("v2x_egaldem", "v2x_civlib", "shdynmort_perc", "conflict"), 
                           labels=c("Egalitarian Democracy", "Civil Liberties", 
                                    "Child Mortality Change", "Conflict Occurrence")),
         term = factor(term, levels=c("(Intercept)", "Drought", "ExTemp", "Fire", "Flood", "Storm"),
                       labels=c("(Intercept)", "Drought", "Extreme Temperature",  "Fire", "Flood", "Storm"))) %>% 
  ggplot(aes(x=estimate, y=term, colour=response)) + 
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  labs(x="Estimate", y="", colour="Model") + scale_color_hue(l=50, c=120) +
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()


# Robust standard errors for Subgroup
Type_robust <- coeftest(Type, vcov =  vcovCL, cluster=~year)

coef_Type2 <- tidy(Type_robust, conf.int = TRUE,  conf.level = 0.95)
coef_Type2

png("Figure_E_3.png", width = 800, height = 600)
coef_Type2 %>% 
  mutate(term = factor(term, levels=c("v2x_egaldem:(Intercept)", "v2x_egaldem:ExTemp", "v2x_egaldem:Storm", "v2x_egaldem:Flood", "v2x_egaldem:Drought", "v2x_egaldem:Fire",
                                      "v2x_civlib:(Intercept)", "v2x_civlib:ExTemp", "v2x_civlib:Storm", "v2x_civlib:Flood", "v2x_civlib:Drought", "v2x_civlib:Fire",
                                      "shdynmort_perc:(Intercept)", "shdynmort_perc:ExTemp", "shdynmort_perc:Storm", "shdynmort_perc:Flood", "shdynmort_perc:Drought", "shdynmort_perc:Fire", 
                                      "conflict:(Intercept)", "conflict:ExTemp", "conflict:Storm", "conflict:Flood", "conflict:Drought", "conflict:Fire"), 
                       labels=c("Egalitarian Democracy, Intercept", "Egalitarian Democracy, Extreme Temperature", "Egalitarian Democracy, Storm", "Egalitarian Democracy, Flood", "Egalitarian Democracy, Drought", "Egalitarian Democracy, Fire",
                                "Civil Liberties, Intercept", "Civil Liberties, Extreme Temperature", "Civil Liberties, Storm", "Civil Liberties, Flood", "Civil Liberties, Drought", "Civil Liberties, Fire",
                                "Child Mortality Change, Intercept", "Child Mortality Change, Extreme Temperature", "Child Mortality Change, Storm", "Child Mortality Change, Flood",  "Child Mortality Change, Drought", "Child Mortality Change, Fire", 
                                "Conflict Occurrence, Intercept", "Conflict Occurrence, Extreme Temperature", "Conflict Occurrence, Storm", "Conflict Occurrence, Flood", "Conflict Occurrence, Drought", "Conflict Occurrence, Fire"))) %>% 
  ggplot(aes(x=estimate, y=term)) + 
  labs(x="Estimate", y="") +
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

#MANOVA - Table 6 
#Determining whether or not to include predictors in a multivariate multiple regression requires the use of multivariate test statistics. 
#predictors are tested assuming all other predictors are already in the model
Manova(Group)

Manova(Subgroup)

Manova(Type)

## ____________________________________________________________________________________ ##
#Control for outliers with a dummy, to see if it significantly changes the effect 
#Haiti is an extreme outlier on child mortality in 2010 and 2011
Data$Haiti_Outlier_2010 <- ifelse(Data$country_name == 'Haiti' & Data$year == 2010, 1, 0)
Data$Haiti_Outlier_2011 <- ifelse(Data$country_name == 'Haiti' & Data$year == 2011, 1, 0)

#Natural Disaster Group 
#2010
Group_outlier_2010_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Haiti_Outlier_2010, data = Data)
Group_outlier_2010_CMC
summary(Group_outlier_2010_CMC)

#2011
Group_outlier_2011_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Haiti_Outlier_2011, data = Data)
Group_outlier_2011_CMC
summary(Group_outlier_2011_CMC)

##Subgroup
#2010
Subgroup_outlier_2010_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro +  Haiti_Outlier_2010,  data = Data)
Subgroup_outlier_2010_CMC
summary(Subgroup_outlier_2010_CMC)

#2011
Subgroup_outlier_2011_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Haiti_Outlier_2011,  data = Data)
Subgroup_outlier_2011_CMC
summary(Subgroup_outlier_2011_CMC)

##Type
#2010
Type_outlier_2010_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Haiti_Outlier_2010,  data = Data)
Type_outlier_2010_CMC
summary(Type_outlier_2010_CMC)

#2011
Type_outlier_2011_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Haiti_Outlier_2011,  data = Data)
Type_outlier_2011_CMC
summary(Type_outlier_2011_CMC)

#All outliers together 
#Natural Disaster Group
options(scipen=999)#make the output 'unscientific' to have decimal numbers

Group_outlier_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                          Group + Haiti_Outlier_2010 + Haiti_Outlier_2011, data = Data)
print(Group_outlier_CMC, digits = 2)
print(summary(Group_outlier_CMC), digits = 3)


##Subgroup
Subgroup_outlier_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                             Meteoro + Climato + Hydro +  Haiti_Outlier_2010 + Haiti_Outlier_2011,  data = Data)
print(Subgroup_outlier_CMC, digits = 2)
print(summary(Subgroup_outlier_CMC), digits = 4)

##Type
Type_outlier_CMC <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                         ExTemp + Storm + Flood +  Drought + Fire + Haiti_Outlier_2010 + Haiti_Outlier_2011,  data = Data)
print(Type_outlier_CMC, digits = 2)
print(summary(Type_outlier_CMC), digits = 3)

options(scipen=n)#return output back to normal

#Afghanistan and Eritrea outliers on civil liberties 
Data$Afghanistan_Outlier_1996 <- ifelse(Data$country_name == 'Afghanistan' & Data$year == 1996, 1, 0)
Data$Afghanistan_Outlier_1997 <- ifelse(Data$country_name == 'Afghanistan' & Data$year == 1997, 1, 0)
Data$Afghanistan_Outlier_1998 <- ifelse(Data$country_name == 'Afghanistan' & Data$year == 1998, 1, 0)
Data$Afghanistan_Outlier_1999 <- ifelse(Data$country_name == 'Afghanistan' & Data$year == 1999, 1, 0)
Data$Afghanistan_Outlier_2000 <- ifelse(Data$country_name == 'Afghanistan' & Data$year == 2000, 1, 0)


Data$Eritrea_Outlier_2003 <- ifelse(Data$country_name == 'Eritrea' & Data$year == 2003, 1, 0)
Data$Eritrea_Outlier_2004 <- ifelse(Data$country_name == 'Eritrea' & Data$year == 2004, 1, 0)
Data$Eritrea_Outlier_2008 <- ifelse(Data$country_name == 'Eritrea' & Data$year == 2008, 1, 0)

#Natural Disaster Group
#1996
Group_outlier_1996_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Afghanistan_Outlier_1996, data = Data)
Group_outlier_1996_CLI
summary(Group_outlier_1996_CLI)

#1997
Group_outlier_1997_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Afghanistan_Outlier_1997, data = Data)
Group_outlier_1997_CLI
summary(Group_outlier_1997_CLI)

#1998
Group_outlier_1998_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Afghanistan_Outlier_1998, data = Data)
Group_outlier_1998_CLI
summary(Group_outlier_1998_CLI)

#1999
Group_outlier_1999_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Afghanistan_Outlier_1999, data = Data)
Group_outlier_1999_CLI
summary(Group_outlier_1999_CLI)

#2000
Group_outlier_2000_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Afghanistan_Outlier_2000, data = Data)
Group_outlier_2000_CLI
summary(Group_outlier_2000_CLI)

#2003
Group_outlier_2003_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Eritrea_Outlier_2003, data = Data)
Group_outlier_2003_CLI
summary(Group_outlier_2003_CLI)

#2004
Group_outlier_2004_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Eritrea_Outlier_2004, data = Data)
Group_outlier_2004_CLI
summary(Group_outlier_2004_CLI)

#2008
Group_outlier_2008_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                               Group + Eritrea_Outlier_2008, data = Data)
Group_outlier_2008_CLI
summary(Group_outlier_2008_CLI)


##Subgroup
#1996
Subgroup_outlier_1996_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Afghanistan_Outlier_1996,  data = Data)
Subgroup_outlier_1996_CLI
summary(Subgroup_outlier_1996_CLI)

#1997
Subgroup_outlier_1997_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Afghanistan_Outlier_1997,  data = Data)
Subgroup_outlier_1997_CLI
summary(Subgroup_outlier_1997_CLI)

#1998
Subgroup_outlier_1998_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Afghanistan_Outlier_1998,  data = Data)
Subgroup_outlier_1998_CLI
summary(Subgroup_outlier_1998_CLI)

#1999
Subgroup_outlier_1999_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Afghanistan_Outlier_1999,  data = Data)
Subgroup_outlier_1999_CLI
summary(Subgroup_outlier_1999_CLI)

#2000
Subgroup_outlier_2000_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Afghanistan_Outlier_2000,  data = Data)
Subgroup_outlier_2000_CLI
summary(Subgroup_outlier_2000_CLI)

#2003
Subgroup_outlier_2003_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Eritrea_Outlier_2003,  data = Data)
Subgroup_outlier_2003_CLI
summary(Subgroup_outlier_2003_CLI)

#2004
Subgroup_outlier_2004_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Eritrea_Outlier_2004,  data = Data)
Subgroup_outlier_2004_CLI
summary(Subgroup_outlier_2004_CLI)

#2008
Subgroup_outlier_2008_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                                  Meteoro + Climato + Hydro + Eritrea_Outlier_2008,  data = Data)
Subgroup_outlier_2008_CLI
summary(Subgroup_outlier_2008_CLI)

##Type
#1996
Type_outlier_1996_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Afghanistan_Outlier_1996,  data = Data)
Type_outlier_1996_CLI
summary(Type_outlier_1996_CLI)

#1997
Type_outlier_1997_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Afghanistan_Outlier_1997,  data = Data)
Type_outlier_1997_CLI
summary(Type_outlier_1997_CLI)  

#1998
Type_outlier_1998_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Afghanistan_Outlier_1998,  data = Data)
Type_outlier_1997_CLI
summary(Type_outlier_1998_CLI)  

#1999
Type_outlier_1999_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Afghanistan_Outlier_1999,  data = Data)
Type_outlier_1999_CLI
summary(Type_outlier_1999_CLI)

#2000
Type_outlier_2000_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Afghanistan_Outlier_2000,  data = Data)
Type_outlier_2000_CLI
summary(Type_outlier_2000_CLI)  

#2003
Type_outlier_2003_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Eritrea_Outlier_2003,  data = Data)
Type_outlier_2003_CLI
summary(Type_outlier_2003_CLI)   

#2004
Type_outlier_2004_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Eritrea_Outlier_2004,  data = Data)
Type_outlier_2004_CLI
summary(Type_outlier_2004_CLI)   

#2008
Type_outlier_2008_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                              ExTemp + Storm + Flood +  Drought + Fire + Eritrea_Outlier_2008,  data = Data)
Type_outlier_2008_CLI
summary(Type_outlier_2008_CLI)

#All outliers together 
#Natural Disaster Group
Group_outlier_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ Group +
                          Afghanistan_Outlier_1996 + Afghanistan_Outlier_1997 + Afghanistan_Outlier_1998 + Afghanistan_Outlier_1999 + Afghanistan_Outlier_2000 +
                          Eritrea_Outlier_2003 + Eritrea_Outlier_2004 + Eritrea_Outlier_2008, 
                        data = Data)
print(Group_outlier_CLI, digits = 2)
print(summary(Group_outlier_CLI), digits = 3)

#Subgroup
Subgroup_outlier_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                             Meteoro + Climato + Hydro + 
                             Afghanistan_Outlier_1996 + Afghanistan_Outlier_1997 + Afghanistan_Outlier_1998 + Afghanistan_Outlier_1999 + Afghanistan_Outlier_2000 +
                             Eritrea_Outlier_2003 + Eritrea_Outlier_2004 + Eritrea_Outlier_2008, 
                           data = Data)
print(Subgroup_outlier_CLI, digits=2)
print(summary(Subgroup_outlier_CLI), digits = 3)

##Type
Type_outlier_CLI <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                         ExTemp + Storm + Flood +  Drought + Fire +
                         Afghanistan_Outlier_1996 + Afghanistan_Outlier_1997 + Afghanistan_Outlier_1998 + Afghanistan_Outlier_1999 + Afghanistan_Outlier_2000 +
                         Eritrea_Outlier_2003 + Eritrea_Outlier_2004 + Eritrea_Outlier_2008, 
                       data = Data)
print(Type_outlier_CLI, digits = 2)
print(summary(Type_outlier_CLI), digits = 3)


## ____________________________________________________________________________________ ##
#Control variable - Income Data and Aid

#Regressions Income 
##Natural Disaster Group 
Group_income <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                     Group + InUSCur_perc, data = Data)
print(Group_income, digits = 2)
print(summary(Group_income), digits = 3)

coef_Group_In <- tidy(Group_income, conf.int = TRUE)
coef_Group_In

png("Figure_13.png", width = 800, height = 600)
coef_Group_In %>% 
  mutate(response = factor(response, levels=c("v2x_egaldem", "v2x_civlib", "shdynmort_perc", "conflict"), 
                           labels=c("Egalitarian Democracy", "Civil Liberties", 
                                    "Child Mortality Change", "Conflict Occurrence")),
         term = factor(term, levels=c("(Intercept)", "Group", "InUSCur_perc"),
                       labels=c("(Intercept)", "Natural Disaster", "Income Change"))) %>% 
  ggplot(aes(x=estimate, y=term, colour=response)) + 
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  labs(x="Estimate", y="", colour="Model") + scale_color_hue(l=50, c=120) + 
  theme_bw() +
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

# Robust standard errors for Group
Group_In_robust <- coeftest(Group_income, vcov =  vcovCL, cluster=~country_name)

coef_Group_In2 <- tidy(Group_In_robust, conf.int = TRUE,  conf.level = 0.95)
coef_Group_In2

png("Figure_E_4.png", width = 800, height = 600)
coef_Group_In2 %>% 
  mutate(term = factor(term, levels=c("v2x_egaldem:(Intercept)", "v2x_egaldem:Group", "v2x_egaldem:InUSCur_perc",
                                      "v2x_civlib:(Intercept)", "v2x_civlib:Group", "v2x_civlib:InUSCur_perc",
                                      "shdynmort_perc:(Intercept)", "shdynmort_perc:Group", "shdynmort_perc:InUSCur_perc", 
                                      "conflict:(Intercept)", "conflict:Group", "conflict:InUSCur_perc"), 
                       labels=c("Egalitarian Democracy, Intercept", "Egalitarian Democracy, Natural Disaster", "Egalitarian Democracy, Income Change",
                                "Civil Liberties, Intercept", "Civil Liberties, Natural Disaster", "Civil Liberties, Income Change",
                                "Child Mortality Change, Intercept", "Child Mortality Change, Natural Disaster", "Child Mortality Change, Income Change", 
                                "Conflict Occurrence, Intercept", "Conflict Occurrence, Natural Disaster", "Conflict Occurrence, Income Change"))) %>% 
  ggplot(aes(x=estimate, y=term)) + 
  labs(x="Estimate", y="") +
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

##Subgroup
Subgroup_income <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                        Meteoro + Climato + Hydro + InUSCur_perc,  data = Data)
print(Subgroup_income, digits = 2)
print(summary(Subgroup_income), digits = 3)

coef_Subgroup_In <- tidy(Subgroup_income, conf.int = TRUE)
coef_Subgroup_In

png("Figure_14.png", width = 800, height = 600)
coef_Subgroup_In %>% 
  mutate(response = factor(response, levels=c("v2x_egaldem", "v2x_civlib", "shdynmort_perc", "conflict"), 
                           labels=c("Egalitarian Democracy", "Civil Liberties", 
                                    "Child Mortality Change", "Conflict Occurrence")),
         term = factor(term, levels=c("(Intercept)", "Climato", "Hydro", "Meteoro", "InUSCur_perc"),
                       labels=c("(Intercept)", "Climatlogical", "Hydrological", "Meteorological", "Income Change"))) %>% 
  ggplot(aes(x=estimate, y=term, colour=response)) + 
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  labs(x="Estimate", y="", colour="Model") + scale_color_hue(l=50, c=120) +
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

# Robust standard errors for Subgroup
Subgroup_In_robust <- coeftest(Subgroup_income, vcov =  vcovCL, cluster=~country_name)
coef_Subgroup_In2 <- tidy(Subgroup_In_robust, conf.int = TRUE,  conf.level = 0.95)
coef_Subgroup_In2

png("Figure_E_5.png", width = 800, height = 600)
coef_Subgroup_In2 %>% 
  mutate(term = factor(term, levels=c("v2x_egaldem:(Intercept)", "v2x_egaldem:Meteoro", "v2x_egaldem:Climato", "v2x_egaldem:Hydro", "v2x_egaldem:InUSCur_perc",
                                      "v2x_civlib:(Intercept)", "v2x_civlib:Meteoro", "v2x_civlib:Climato", "v2x_civlib:Hydro", "v2x_civlib:InUSCur_perc",
                                      "shdynmort_perc:(Intercept)", "shdynmort_perc:Meteoro", "shdynmort_perc:Climato", "shdynmort_perc:Hydro", "shdynmort_perc:InUSCur_perc",  
                                      "conflict:(Intercept)", "conflict:Meteoro", "conflict:Climato", "conflict:Hydro", "conflict:InUSCur_perc"), 
                       labels=c("Egalitarian Democracy, Intercept", "Egalitarian Democracy, Meteorological", "Egalitarian Democracy, Climatological", "Egalitarian Democracy, Hydrological", "Egalitarian Democracy, Income Change",
                                "Civil Liberties, Intercept", "Civil Liberties, Meteorological", "Civil Liberties, Climatological", "Civil Liberties, Hydrological", "Civil Liberties, Income Change",
                                "Child Mortality Change, Intercept", "Child Mortality Change, Meteorological", "Child Mortality Change, Climatological", "Child Mortality Change, Hydrological", "Child Mortality Change, Income Change", 
                                "Conflict Occurrence, Intercept", "Conflict Occurrence, Meteorological", "Conflict Occurrence, Climatological", "Conflict Occurrence, Hydrological", "Conflict Occurrence, Income Change"))) %>% 
  ggplot(aes(x=estimate, y=term)) + 
  labs(x="Estimate", y="") +
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

##Type
Type_income <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                    ExTemp + Storm + Flood + Drought + Fire + InUSCur_perc,  data = Data)
print(Type_income, digits = 2)
print(summary(Type_income), digits = 3)

coef_Type_In <- tidy(Type_income, conf.int = TRUE)
coef_Type_In

png("Figure_15.png", width = 900, height = 600)
coef_Type_In %>% 
  mutate(response = factor(response, levels=c("v2x_egaldem", "v2x_civlib", "shdynmort_perc", "conflict"), 
                           labels=c("Egalitarian Democracy", "Civil Liberties", 
                                    "Child Mortality Change", "Conflict Occurrence")),
         term = factor(term, levels=c("(Intercept)", "Drought", "ExTemp", "Fire", "Flood", "Storm", "InUSCur_perc"),
                       labels=c("(Intercept)", "Drought", "Extreme Temperature",  "Fire", "Flood", "Storm", "Income Change"))) %>% 
  ggplot(aes(x=estimate, y=term, colour=response)) + 
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  labs(x="Estimate", y="", colour="Model") + scale_color_hue(l=50, c=120) +
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

# Robust standard errors for Type
Type_In_robust <- coeftest(Type_income, vcov =  vcovCL, cluster=~country_name)

coef_Type_In2 <- tidy(Type_In_robust, conf.int = TRUE,  conf.level = 0.95)
coef_Type_In2

png("Figure_E_6.png", width = 800, height = 600)
coef_Type_In2 %>% 
  mutate(term = factor(term, levels=c("v2x_egaldem:(Intercept)", "v2x_egaldem:ExTemp", "v2x_egaldem:Storm", "v2x_egaldem:Flood", "v2x_egaldem:Drought", "v2x_egaldem:Fire", "v2x_egaldem:InUSCur_perc",
                                      "v2x_civlib:(Intercept)", "v2x_civlib:ExTemp", "v2x_civlib:Storm", "v2x_civlib:Flood", "v2x_civlib:Drought", "v2x_civlib:Fire", "v2x_civlib:InUSCur_perc",
                                      "shdynmort_perc:(Intercept)", "shdynmort_perc:ExTemp", "shdynmort_perc:Storm", "shdynmort_perc:Flood", "shdynmort_perc:Drought", "shdynmort_perc:Fire",  "shdynmort_perc:InUSCur_perc", 
                                      "conflict:(Intercept)", "conflict:ExTemp", "conflict:Storm", "conflict:Flood", "conflict:Drought", "conflict:Fire", "conflict:InUSCur_perc"), 
                       labels=c("Egalitarian Democracy, Intercept", "Egalitarian Democracy, Extreme Temperature", "Egalitarian Democracy, Storm", "Egalitarian Democracy, Flood", "Egalitarian Democracy, Drought", "Egalitarian Democracy, Fire", "Egalitarian Democracy, Income Change",
                                "Civil Liberties, Intercept", "Civil Liberties, Extreme Temperature", "Civil Liberties, Storm", "Civil Liberties, Flood", "Civil Liberties, Drought", "Civil Liberties, Fire", "Civil Liberties, Income Change",
                                "Child Mortality Change, Intercept", "Child Mortality Change, Extreme Temperature", "Child Mortality Change, Storm", "Child Mortality Change, Flood",  "Child Mortality Change, Drought", "Child Mortality Change, Fire", "Child Mortality Change, Income Change",  
                                "Conflict Occurrence, Intercept", "Conflict Occurrence, Extreme Temperature", "Conflict Occurrence, Storm", "Conflict Occurrence, Flood", "Conflict Occurrence, Drought", "Conflict Occurrence, Fire", "Conflict Occurrence, Income Change"))) %>% 
  ggplot(aes(x=estimate, y=term)) + 
  labs(x="Estimate", y="") +
  geom_point(position = position_dodge(width=.75)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=.75), height=0) + 
  theme_bw()+
  theme(legend.position = "right", text=element_text(size=20, family="serif"))
dev.off()

#Regressions Aid
options(scipen=999) #make the output 'unscientific' to have decimal numbers

#With ODA itself 
##Natural Disaster Group
Group_aid_mil <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                      Group + ODAus18_mil, data = Data)
Group_aid_mil
summary(Group_aid_mil)


#With ODA change (with negatives put to 0 and a dummy for those)
##Natural Disaster Group
Group_aid_noNeg <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                        Group + ODAus18_perc_woNeg + NoInfinitivesOrNegatives, data = Data)
Group_aid_noNeg
summary(Group_aid_noNeg)

#Regressions Insurance Payments 
##Natural Disaster Group
Group_insurance <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                        Group + GIC_mil, data = Data)
Group_insurance
summary(Group_insurance)


options(scipen=n) #return output back to normal

## ____________________________________________________________________________________ ##
#Control for lagged effects - 1 year and 5 year
Data_lag <- Data_lag %>% group_by(country_name) %>% mutate(Group_lag = lag(Group, n = 1, default = NA), 
                                                           Meteoro_lag = lag(Meteoro, n = 1, default = NA),
                                                           Climato_lag = lag(Climato, n = 1, default = NA),
                                                           Hydro_lag = lag(Hydro, n = 1, default = NA),
                                                           ExTemp_lag = lag(ExTemp, n = 1, default = NA),
                                                           Storm_lag = lag(Storm, n = 1, default = NA),
                                                           Flood_lag = lag(Flood, n = 1, default = NA),
                                                           Drought_lag = lag(Drought, n = 1, default = NA),
                                                           Fire_lag = lag(Fire, n = 1, default = NA),
                                                           Group_lag5 = lag(Group, n = 5, default = NA), 
                                                           Meteoro_lag5 = lag(Meteoro, n = 5, default = NA),
                                                           Climato_lag5 = lag(Climato, n = 5, default = NA),
                                                           Hydro_lag5 = lag(Hydro, n = 5, default = NA),
                                                           ExTemp_lag5 = lag(ExTemp, n = 5, default = NA),
                                                           Storm_lag5 = lag(Storm, n = 5, default = NA),
                                                           Flood_lag5 = lag(Flood, n = 5, default = NA),
                                                           Drought_lag5 = lag(Drought, n = 5, default = NA),
                                                           Fire_lag5 = lag(Fire, n = 5, default = NA)) 

Data_lag <- Data_lag %>% subset(year >= 1995)

##1 Year
#Natural Disaster Group
Group_lag <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                  Group_lag, data = Data_lag )
print(Group_lag, digits = 3)
print(summary(Group_lag), digits = 3)

##Subgroup
Subgroup_lag <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                     Meteoro_lag + Climato_lag + Hydro_lag,  data = Data_lag )
print(Subgroup_lag, digits = 3)
print(summary(Subgroup_lag), digits = 3)

##Type
Type_lag <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                 ExTemp_lag + Storm_lag + Flood_lag +  Drought_lag + Fire_lag,  data = Data_lag )
print(Type_lag, digits = 3)
print(summary(Type_lag), digits = 3)

##5 Years 
#Natural Disaster Group
Group_lag5 <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                   Group_lag5, data = Data_lag )
print(Group_lag5, digits = 2)
print(summary(Group_lag5), digits = 3)

##Subgroup
Subgroup_lag5 <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                      Meteoro_lag5 + Climato_lag5 + Hydro_lag5,  data = Data_lag )
print(Subgroup_lag5, digits = 2)
print(summary(Subgroup_lag5), digits = 3)

##Type
Type_lag5 <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                  ExTemp_lag5 + Storm_lag5 + Flood_lag5 +  Drought_lag5 + Fire_lag5,  data = Data_lag )
print(Type_lag5, digits = 2)
print(summary(Type_lag5), digits = 3)

## ____________________________________________________________________________________ ##
#Regression with 5-year time-lagged effect of climatological and droughts 
##Subgroup
Subgroup_clim_lag <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                          Meteoro + Climato_lag5 + Hydro,  data = Data_lag )
print(Subgroup_clim_lag, digits = 3)
print(summary(Subgroup_clim_lag), digits =3)

Subgroup_clim_lag2 <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                           Meteoro + Climato + Climato_lag5 + Hydro,  data = Data_lag )
print(Subgroup_clim_lag2, digits =2)
print(summary(Subgroup_clim_lag2), digits = 3)

##Type
Type_D_lag <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                   ExTemp + Storm + Flood + Drought_lag5 + Fire,  data = Data_lag )
print(Type_D_lag, digits = 3)
print(summary(Type_D_lag), digits = 3)

Type_D_lag2 <- lm(cbind(v2x_egaldem, v2x_civlib, shdynmort_perc, conflict) ~ 
                    ExTemp + Storm + Flood + Drought + Drought_lag5 + Fire,  data = Data_lag )
print(Type_D_lag2, digits = 2)
print(summary(Type_D_lag2), digits = 3)
