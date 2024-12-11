library(rio)
library(tidyverse)
library(stargazer)
library(fredr)
library(dplyr)
library(plm)
library(sandwich)

setwd()


#Fred Api key 
fredr_set_key()
####Importing####

#U.K. parliamentary records volume 58
british_import<- import("data/Brit_import.xlsx")
#moving to scale 
british_import$Yards <- british_import$Yards / 1000000


#Using fred api to pull Davis and Settler data
cotton<- fredr(
  series_id = "CPNETOTAL"
  
)

boston<- fredr(
  series_id = "CPNEBOSTON",
  observation_start = as.Date("1815-01-01"), 
  observation_end = as.Date("1860-01-01")
)

slater<- fredr(
  series_id = "CPNESLATER"
)

merrimack<- fredr(
  series_id = "CPNEMERRIMACK"
)

dwight<- fredr(
  series_id = "CPNEDWIGHTII"
)

suffolk<- fredr(
  series_id = "CPNESUFFOLK"
)

jackson<- fredr(
  series_id = "CPNEJACKSON"
)

slatertiff<- fredr(
  series_id= "CPNESLATERTIFFANY"
)

rampo<- fredr(
  series_id = "CPNERAMPO"
)
pepperill<- fredr(
  series_id = "CPNEPEPPERILL"
)
laconia<- fredr(
  series_id = "CPNELACONIA"
)
lymanII<- fredr(
  series_id = "CPNELYMANII"
)
hamilton<- fredr(
  series_id = "CPNEHAMILTON"
)
nashua<- fredr(
  series_id = "CPNENASHUA"
)
lawrence<- fredr(
  series_id = "CPNELAWRENCE"
)
metacomet<- fredr(
  series_id = "CPNEMETACOMET"
)
dwightIII<- fredr(
  series_id = "CPNEDWIGHTIII"
)
sutton<- fredr(
  series_id = "CPNESUTTON"
)
lancaster<- fredr(
  series_id = "CPNELANCASTER"
)

naumkeag<- fredr(
  series_id = "CPNENAUMKEAG"
)

amoskeagII<- fredr(
  series_id = "CPNEAMOSKEAGII"
)

lyman<- fredr(
  series_id = "CPNELYMANI"
)

tremont<- fredr(
  series_id = "CPNETREMONT"
)

amoskeagIII<- fredr(
  series_id = "CPNEAMOSKEAGIII"
)


amoskeag<- fredr(
  series_id = "CPNEAMOSKEAGI"
)

####cleaning Data####

jackson_sub<- jackson %>% select(date, value)
jackson2<- rename(jackson_sub, jacksonoutput= value)

slatertiff_sub<- slatertiff %>% select(date, value)
slatertiff2<- rename(slatertiff_sub, slatertiffoutput= value)

rampo_sub<- rampo %>% select(date, value)
rampo2<- rename(rampo_sub, rampooutput= value)

pepperill_sub<- pepperill %>% select(date, value)
pepperill2<- rename(pepperill_sub, pepperilloutput= value)

laconia_sub<- laconia %>% select(date, value)
laconia2<- rename(laconia_sub, laconiaoutput= value)

lymanII_sub<- lymanII %>% select(date, value)
lymanII2<- rename(lymanII_sub, lymanIIoutput= value)

hamilton_sub<- hamilton %>% select(date, value)
hamilton2<- rename(hamilton_sub, hamiltonoutput= value)

nashua_sub<- nashua %>% select(date, value)
nashua2<- rename(nashua_sub, nashuaoutput= value)

lawrence_sub<- lawrence %>% select(date, value)
lawrence2<- rename(lawrence_sub, lawrenceoutput= value)

metacomet_sub<- metacomet %>% select(date, value)
metacomet2<- rename(metacomet_sub, metacometoutput= value)

dwightIII_sub<- dwightIII %>% select(date, value)
dwightIII2<- rename(dwightIII_sub, dwightIIIoutput= value)

sutton_sub<- sutton %>% select(date, value)
sutton2<- rename(sutton_sub, suttonoutput= value)

lancaster_sub<- lancaster %>% select(date, value)
lancaster2<- rename(lancaster_sub, lancasteroutput= value)


naumkeag_sub<- naumkeag %>% select(date, value)
naumkeag2<- rename(naumkeag_sub, naumkeagoutput= value)


amoskeagII_sub<- amoskeagII %>% select(date, value)
amoskeagII2<- rename(amoskeagII_sub, amoskeagIIoutput= value)

lyman_sub<- lyman %>% select(date, value)
lyman2<- rename(lyman_sub, lymanoutput= value)

tremont_sub<- tremont %>% select(date, value)
tremont2<- rename(tremont_sub, tremontoutput= value)

amoskeagIII_sub<- amoskeagIII %>% select(date, value)
amoskeagIII2<- rename(amoskeagIII_sub, amoskeagIIIoutput= value)

amoskeag_sub<- amoskeag %>% select(date, value)
amoskeag2<- rename(amoskeag_sub, amoskeagoutput= value)

suffolk_sub<- suffolk %>% select(date, value)
suffolk2<- rename(suffolk_sub, suffolkoutput= value)


dwight_subset<- dwight %>% select(date, value)
dwight2<- rename(dwight_subset, dwightoutput= value)

merrimack_subset <- merrimack %>% select(date, value)
merrimack2<- rename(merrimack_subset, merrimackoutput= value)

boston_subset <- boston %>% select(date, value) 
boston2<- rename(boston_subset, bostonoutput= value)

slater_subset <- slater %>% select(date, value)
slater2<- rename(slater_subset, slateroutput= value)


cotton_subset <- cotton %>% select(date, value)
cotton2<- rename(cotton_subset, totaloutput= value)

#combining all mill-level data 

cottonall<- merge(cotton2, slater2, by= "date", all = TRUE) %>% merge(boston2, by= "date", all= TRUE) %>%
  merge(merrimack2, by= "date", all = TRUE) %>% 
  merge(amoskeag2,by= "date", all = TRUE) %>%
  merge(amoskeagIII2,by= "date", all = TRUE) %>%
  merge(tremont2,by= "date", all = TRUE) %>%
  merge(lyman2,by= "date", all = TRUE) %>%
  merge(amoskeagII2,by= "date", all = TRUE) %>%
  merge(lancaster2,by= "date", all = TRUE) %>%
  merge(sutton2,by= "date", all = TRUE) %>%
  merge(dwightIII2,by= "date", all = TRUE) %>%
  merge(metacomet2,by= "date", all = TRUE) %>%
  merge(nashua2,by= "date", all = TRUE) %>%
  merge(hamilton2,by= "date", all = TRUE) %>%
  merge(lymanII2,by= "date", all = TRUE) %>%
  merge(laconia2,by= "date", all = TRUE) %>%
  merge(pepperill2,by= "date", all = TRUE) %>%
  merge(rampo2,by= "date", all = TRUE) %>%
  merge(slatertiff2,by= "date", all = TRUE) %>%
  merge(jackson2,by= "date", all = TRUE) 

#converting string dates 
cottonall <- cottonall %>% mutate(year = as.integer(substring(date, 1, 4)))

british_import <- british_import %>% mutate(year = as.integer(date))

cottonall <- cottonall %>% merge(british_import, by = "year", all = TRUE)



####Summary stats####
cottonall2 <- select(data.frame(cottonall), -any_of(c("year","date.x","date.y")))
stargazer(cottonall2, 
          type="html",
          title = "Cotton Summary Stats",
          covariate.labels =  c("Total Cotton Produced in New England" ,"Slater" , "Boston" , "Merrimack",
                                "Amoskeag", "AmoskeagIII", "Tremont", "Lyman", "AmoskeagII", "Lancaster",
                                "Sutton", "DwightIII", "Metacomet", "Nashua", "Hamilton", "LymanII",
                                "Laconia", "Pepperill", "Rampo", "Slatertiff", "Laconia","U.K. Imports","Tariff"),
          notes= "Note: The table provides summary statistics for various variables related to cotton production and textile manufacturing in New England during a certain time period. It includes data on the total cotton produced in the region, as well as statistics on specific mills like Slater, Boston, Merrimack, Amoskeag, Tremont, Lyman, and others.",
          out = "tables/cotton_stats.doc")

#### plot ####


#Extra plot
ggplot(cottonall)+ 
  geom_line(aes(x=date.x, y=slateroutput),color="blue") +
  geom_line(aes(x=date.x, y=bostonoutput),color="blue") +
  geom_line(aes(x=date.x, y=merrimackoutput), color="red")+
  geom_line(aes(x=date.x, y=jacksonoutput),color="blue") +
  geom_line(aes(x=date.x, y=tremontoutput),color="blue") +
  geom_line(aes(x=date.x, y=amoskeagoutput),color="blue") +
  ggtitle("Cotton Production in New England") +
  xlab("Year")+
  ylab("Mill Output")+
  theme_classic()





#Figure 1 
  
cotton_long %>% filter(mill != "total") %>%
  ggplot( aes(x=date.y, y=output, group=mill, color=mill)) +
  geom_line()+
  ggtitle("Cotton Production in New England") +
  xlab("Year")+
  ylab("Mill Output")+
  theme_classic()
####Panel regression#### 

cotton_long<-cottonall %>% pivot_longer(names_to = "mill", values_to = "output",
                                        cols=ends_with("output"),
                                        names_pattern = "(.*)output")


#Basic reg Model
cotton_reg <- lm(output ~ Tariff + Yards,
                 data = cotton_long %>% filter(mill != "total"))
summary(cotton_reg)

#Fixed effect reg 

cotton_fe <- lm(output ~ Tariff + Yards + factor(mill) + factor(year),
                data = cotton_long %>% filter(mill != "total"))
summary(cotton_fe)

#Fixed effects only year
cotton_fey<- lm(output~ Tariff + Yards + factor(year),
                   data = cotton_long %>% filter(mill != "total"))
summary(cotton_feyear) 

#Fixed effects only mill
cotton_fem <- lm(output ~ Tariff + Yards + factor(mill), 
                    data= cotton_long %>% filter(mill != "total"))
summary(cotton_femill)

#Basic reg model with only aggregate production  

cotton_t<- lm(output~ Tariff + Yards, 
                     data = cotton_long %>% filter(mill == "total"))
summary(cotton_totalreg)




####Regression table####





stargazer(cotton_t, cotton_reg, cotton_fey, cotton_fem, cotton_fe,
          type="html",
          omit="factor",
          title="Antebellum Era Tariffs on Cotton Production in New England",
          out = "tables/cotton_regs.doc",
          add.lines = list(
            c("State Fixed Effects", "No", "No", "No", "Yes", "Yes"),
            c("Time Fixed Effects", "No","No","Yes","No","Yes")
          )
)





