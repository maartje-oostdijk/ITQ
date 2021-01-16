#script for classification attribute model (Fig 3C in main text, needs the "attributes.csv" which is the result of
#classification script)
rm(list = ls())

require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(sjPlot)


#data and source directories
datadir = "/Documents/Development/ITQ/data/"
sdir = "/Documents/Development/ITQ/r_scripts/"
#load data
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
try = read.csv(paste0(datadir, "attributes.csv"))%>%
  select(-X)
#cleaning script
source("~/Documents/Development/ITQ/r_scripts/ramdata_clean.R")  # subset RAM data for analysis and make outcome variables

#create data with classes and management data
series = series%>%
  left_join(try)%>%
  mutate(year = as.factor(as.character(year)),
         stocklong=as.factor(as.character(stocklong)),
         region = as.factor(as.character(region)), transferable = as.factor(transferable),
         individual = as.factor(individual), quota = as.factor(quota), effort = as.factor(effort),
         leasable = as.factor(leasable), pooled = as.factor(pooled),  rationed = as.factor(rationed),
         FisheryType=as.factor(FisheryType))%>%
  filter(!is.na(region) & !is.na(individual))%>%
  mutate(ID= paste0(year, "_", stocklong), region_ID = paste0(region, "_", stocklong))%>%
  distinct()


b_series = series%>%
  filter(!(stocklong %in% check$stocklong))%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(individual))%>%
  filter(!is.na(bbmsy))%>%
  filter(bbmsy>0)

b_m <- glmmTMB(bbmsy_overfished ~   leasable+ transferable+individual+quota+ rationed+ pooled+ar1(year+0  | stocklong) ,
               data= b_series,
               family= binomial(link="logit"))


#residuals look quite uniform (although deviate significantly from uniformity)
simulationOutput <- simulateResiduals(fittedModel = b_m, plot = T)
testDispersion(simulationOutput)#no significant overdispersion (value 1.03, so rather close to modeled)



f_series = series%>%
  filter(!(stocklong %in% check$stocklong))%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(individual))%>%
  filter(!is.na(ffmsy))%>%
  filter(ffmsy>0)

f_m <- glmmTMB(overfishing ~   rationed+pooled+leasable+transferable+individual+quota+(1|region)+ar1(year+0  |stocklong),
               data= f_series,
               family=binomial(link="logit"))
#some deviation from uniformity, but no strong pattern
simulationOutput <- simulateResiduals(fittedModel = f_m, plot = T)
testDispersion(simulationOutput)#no significant overdispersion

