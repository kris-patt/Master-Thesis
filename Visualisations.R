#### MASTER'S THESIS - CLIMATE CHANGE AND POSITIVE PEACE - A Study on the Effects of Rapid-Onset Climate Change on Positive Peace ####

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
## Load Dataset

Data <- read.csv("thesis_dataset_final.csv")

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


#One Output - Figure 7
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
                                 mapTitle='Figure 9: Occurrence of Natural Disasters in the World')
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

#One Output - Figure C 1
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

#One Output - Figure C 2
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
