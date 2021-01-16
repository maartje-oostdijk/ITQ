rm(list = ls())
require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(sjPlot)
require(patchwork)


#data
datadir = "~/Dropbox/ITQ_meta/Data/"

#read in classifications
mn_data = read.csv(paste0(datadir, "all_data_nov2020.csv"))
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
key = read.csv(paste0(datadir, "key_new_ram.csv"))

# read RAM legacy data
load("~/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
source("~/Documents/Development/ITQ/r_scripts/ramdata_clean.R")  # subset RAM data for analysis and make outcome variables


#make tidy data from management classes
classes = mn_data%>% select(fish_admin, name, year, share_admin, rec, ITQ, ILQ, IQ, SGQP, GQP, IRQP, RQP, RIQ, ITE, IE, TE, OA) %>%
  gather(key = "mngmt", value = percentage, -fish_admin, - name, -year, -share_admin) %>%
  distinct()
#filter(percentage!="TBD" & percentage!="NA")

#set missing data to zero
classes$percentage[classes$percentage==""] = 0

#multiply by fish admin share deselect fish admin and calculated "end" percentages of classifications
class = classes %>%
  mutate(share_admin = as.numeric(as.character(share_admin)), percentage = as.numeric(as.character(percentage)))%>%
  mutate(perc_share = share_admin*percentage/100)%>%
  group_by(name, year, mngmt)%>%
  summarise(perc= sum(perc_share))%>%
  left_join(key)%>%
  mutate(stocklong = as.character(ifelse(is.na(stocklong), name, as.character(stocklong))))


#prepare management data in combination with stock data for models
class_2 = class %>%
  ungroup()%>%
  mutate(year = as.numeric(as.character(year)))%>%
  filter(perc >=75)%>%
  left_join(series)%>%
  ungroup()%>%
  mutate(region = as.factor(region), year = as.factor(year),
         mngmt = as.factor(ifelse(mngmt == "TE","1TE",mngmt)),
         stocklong = as.factor(stocklong))%>%
  filter(mngmt!="rec")%>%
  mutate(ID= paste0(year, "_", stocklong))%>%
  distinct()%>%
  filter(mngmt!="ITE" & mngmt!="RQP" & mngmt!="RIQ" & mngmt!="SGQP")

#subset suitable observations for biomass
b_series = class_2%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(mngmt))%>%
  filter(!is.na(bbmsy))%>%
  filter(bbmsy>0)

b1_management <- glmmTMB(bbmsy_overfished ~ mngmt+ ar1(year + 0 | stocklong),
                         data= class_2,
                         family= binomial)
#looks quite alright..
simulationOutput <- simulateResiduals(fittedModel = b1_management, plot = T)

f_series = class_2%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(mngmt))%>%
  filter(!is.na(ffmsy))%>%
  filter(ffmsy>0)%>%
  distinct()

f1_management <- glmmTMB(overfishing ~ mngmt+ (1|region)+ar1(year + 0 | stocklong),
                         data= f_series,
                         family= binomial)
#looks quite alright..
simulationOutput <- simulateResiduals(fittedModel = f1_management, plot = T)

