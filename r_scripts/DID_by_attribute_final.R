#script for DiD per attribute (Fig 4A in main text, needs the "attributes.csv" which is the result of
#classification script)

#clear workspace
rm(list = ls())

#libraries
require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(sjPlot)
require(patchwork)


#data

datadir = "/Users/mtn1/Dropbox/ITQ_meta/Data/"
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")

class= read.csv(paste0(datadir, "attributes.csv"))%>%
  filter(!(quota==0 & effort==0))

#cleaning script
source("~/Documents/Development/ITQ/r_scripts/ramdata_clean.R")  # subset RAM data for analysis and make outcome variables


#filter time series that are too short
list = class%>%
  group_by(stocklong)%>%
  tally()%>%
  filter(n>9)

stocks = sort(unique(class$stocklong[class$stocklong%in%list$stocklong]))

#loop over data to find changes in management attribute
for(i in 1:length(stocks)){

  dat = class%>%
    filter(stocklong == stocks[i])

  years = sort(unique(dat$year[c(1:(length(dat$year)-4))]))


  out = purrr::map_df(years, function(t){



    gdat = dat%>%
      filter(year == t)
    gdat1 = dat%>%
      filter(year == t+1)
    gdat2 = dat%>%
      filter(year == t+2)
    gdat3 = dat%>%
      filter(year == t+3)
    gdat4 = dat%>%
      filter(year == t+4)

    #year quota, stock that is under effort management and shifts to any form of quota management
    #should record the year in which a system goes from effort to quota, any non-individual system to individual etc.

    if( length(gdat4$year)==1 & length(gdat3$year)==1 & length(gdat2$year)==1 & length(gdat1$year)==1 & length(gdat$year)==1){

      if(gdat1$quota == 1 & gdat2$quota == 1 & gdat3$quota == 1 & gdat4$quota == 1  & gdat$quota == 0){year_to_q = t+1}else{year_to_q = NA}
      if(gdat1$individual== 1 & gdat2$individual == 1 & gdat3$individual == 1 & gdat4$individual == 1  & gdat$individual == 0){year_to_i = t+1}else{year_to_i = NA}
      if(gdat1$rationed== 1 & gdat2$rationed == 1 & gdat3$rationed == 1 & gdat4$rationed == 1  & gdat$rationed == 0){year_to_r = t+1}else{year_to_r = NA}
      if(gdat1$transferable== 1 & gdat2$transferable == 1 & gdat3$transferable== 1 & gdat4$transferable == 1  & gdat$transferable == 0){year_to_t = t+1}else{year_to_t = NA}
      if(gdat1$leasable== 1 & gdat2$leasable == 1 & gdat3$leasable== 1 & gdat4$leasable == 1  & gdat$leasable == 0){year_to_l = t+1}else{year_to_l = NA}
      if(gdat1$pooled== 1 & gdat2$pooled == 1 & gdat3$pooled== 1 & gdat4$pooled == 1  & gdat$pooled == 0){year_to_p = t+1}else{year_to_p = NA}

      df = data.frame(year = t+1, stocklong =  stocks[i], year_to_q, year_to_i, year_to_r, year_to_t, year_to_l, year_to_p)
      return(df)

    }



  })

  if(i==1){results = out}else{results = rbind(results, out)}

}



controls <- purrr::map_df(stocks, function(t){


  dat = class%>%
    filter(stocklong == t)
  ############################
  #get control stocks that do not change management attribute
  if(length(dat$year[dat$quota==0])== length(dat$year)) {control_q = "quota_control_stock"} else{control_q= "no control"}
  if(length(dat$year[dat$individual==0])== length(dat$year)) {control_i = "individual_control_stock"} else{control_i= "no control"}
  if(length(dat$year[dat$rationed==0])== length(dat$year)) {control_r = "rationed_control_stock"} else{control_r= "no control"}
  if(length(dat$year[dat$leasable==0])== length(dat$year)) {control_l = "leasable_control_stock"} else{control_l= "no control"}
  if(length(dat$year[dat$pooled==0])== length(dat$year)) {control_p = "pooled_control_stock"} else{control_p= "no control"}
  if(length(dat$year[dat$transferable==0])== length(dat$year)) {control_t = "transferable_control_stock"} else{control_t= "no control"}


  df= data_frame(stocklong = t, control_q, control_i, control_r, control_l, control_p, control_t)
  return(df)



})

ch = results%>%
  filter(!(is.na(year_to_q) & is.na(year_to_i) & is.na(year_to_t)
         & is.na(year_to_l) & is.na(year_to_p)))%>%
  select(-year)%>%
  distinct()


classes = class%>%
  left_join(ch )%>%
  left_join(controls)
  ungroup()

series = series %>%
  left_join(classes)



Q_series = series%>%
    select(stocklong, year,  region, FisheryType, year_to_q, control_q, quota, ffmsy, bbmsy, bbmsy_overfished, overfishing, high_overfishing, bbmsy_overexploited) %>%
    filter(control_q=="quota_control_stock"| !is.na(year_to_q) & quota == 1 |!is.na(year_to_q) & quota == 0)%>%
    mutate(period = as.factor(year), stocklong = as.factor(stocklong), FisheryType= as.factor(FisheryType), region = as.factor(region),
           before_after_Q = ifelse(year>= year_to_q, 1, 0))%>%
    mutate(before_after_Q= ifelse(is.na(before_after_Q), 0, before_after_Q))%>%
    mutate(year = as.factor(year),
           region = as.factor(region), FisheryType = as.factor(FisheryType), before_after_Q = as.factor(before_after_Q),
           overfishing = as.factor(overfishing), high_overfishing= as.factor(high_overfishing), bbmsy_overfished = as.factor(bbmsy_overfished), bbmsy_overexploited=as.factor(bbmsy_overexploited))

quota_f_series = Q_series %>%
  filter(ffmsy>0)%>%
  mutate(ID=as.factor(paste0(year, "_", stocklong)))


model_Q_2 = glmmTMB(overfishing ~before_after_Q+(1|region)+ ar1(year + 0 | stocklong), family= binomial(link="logit"), data = quota_f_series)


#residuals look alright.. again slight deviation from uniformity but can't get any reasonable model specification which doesnt deviate slightly
simulationOutput <- simulateResiduals(fittedModel = model_Q_2, plot = T)
testDispersion(simulationOutput)#no significant overdispersion

quota_b_series = Q_series %>%
  filter(bbmsy>0)%>%
  mutate(ID=as.factor(paste0(year, "_", stocklong)))


model_Q_5 = glmmTMB(bbmsy_overfished ~before_after_Q +  ar1(year + 0 | stocklong)+(1|region)+(1|year), family= binomial, data = quota_b_series)

# a bit of a pattern in the residuals though...
simulationOutput <- simulateResiduals(fittedModel = model_Q_5, plot = T)
testDispersion(simulationOutput)# significant but rather small




I_series = series%>%
  select(stocklong, year,  region, FisheryType, year_to_i, control_i, individual, ffmsy, bbmsy, bbmsy_overfished, overfishing, high_overfishing, bbmsy_overexploited) %>%
  filter(control_i=="individual_control_stock"| !is.na(year_to_i) & individual == 1 |!is.na(year_to_i) & individual == 0)%>%
  mutate(period = as.factor(year), stocklong = as.factor(stocklong), FisheryType= as.factor(FisheryType), region = as.factor(region),
         before_after_I = ifelse(year>= year_to_i, 1, 0))%>%
  mutate(before_after_I= ifelse(is.na(before_after_I), 0, before_after_I))%>%
  mutate(year = as.factor(year),
         region = as.factor(region), FisheryType = as.factor(FisheryType), before_after_I = as.factor(before_after_I),
         overfishing = as.factor(overfishing), high_overfishing= as.factor(high_overfishing), bbmsy_overfished = as.factor(bbmsy_overfished), bbmsy_overexploited=as.factor(bbmsy_overexploited))

individual_f_series = I_series %>%
  filter(ffmsy>0)

model_I_2 = glmmTMB(overfishing ~before_after_I +  (1|region)+ar1(year + 0 | stocklong), family= binomial, data = individual_f_series)

#residuals look alright.. again slight deviation from uniformity
simulationOutput <- simulateResiduals(fittedModel = model_I_2, plot = T)
testDispersion(simulationOutput)#no significant overdispersion


individual_b_series = I_series %>%
  filter(bbmsy>0)

model_I_5 = glmmTMB(bbmsy_overfished ~before_after_I + (1|year)+ ar1(year + 0 | stocklong), family= binomial, data = individual_b_series)

simulationOutput <- simulateResiduals(fittedModel = model_I_5, plot = T)
testDispersion(simulationOutput)

#Transferable attribute
T_series = series%>%
  select(stocklong, year,  region, FisheryType, year_to_t, control_t, transferable, ffmsy, bbmsy, bbmsy_overfished, overfishing, high_overfishing, bbmsy_overexploited) %>%
  filter(control_t=="transferable_control_stock"| !is.na(year_to_t) & transferable == 1 |!is.na(year_to_t) & transferable == 0)%>%
  mutate(period = as.factor(year), stocklong = as.factor(stocklong), FisheryType= as.factor(FisheryType), region = as.factor(region),
         before_after_T = ifelse(year>= year_to_t, 1, 0))%>%
  mutate(before_after_T= ifelse(is.na(before_after_T), 0, before_after_T))%>%
  mutate(year = as.factor(year),
         region = as.factor(region), FisheryType = as.factor(FisheryType), before_after_T = as.factor(before_after_T),
         overfishing = as.factor(overfishing), high_overfishing= as.factor(high_overfishing), bbmsy_overfished = as.factor(bbmsy_overfished), bbmsy_overexploited=as.factor(bbmsy_overexploited))

transferable_f_series = T_series %>%
  filter(ffmsy>0)

model_T_2 = glmmTMB(overfishing ~before_after_T + (1|region)+ar1(year + 0 | stocklong), family= binomial, data = transferable_f_series)

#check residuals, bit worse these..
simulationOutput <- simulateResiduals(fittedModel = model_T_2, plot = T)
testDispersion(simulationOutput)#no significant overdispersion


transferable_b_series = T_series %>%
  filter(bbmsy>0)

model_T_5 = glmmTMB(bbmsy_overfished  ~before_after_T + (1|year)+ar1(year + 0 | stocklong), family= binomial, data = transferable_b_series)

#check residuals, slightly better..
simulationOutput <- simulateResiduals(fittedModel = model_T_5, plot = T)
testDispersion(simulationOutput)#no significant overdispersion


L_series = series%>%
  select(stocklong, year,  region, FisheryType, year_to_l, control_l, leasable, ffmsy, bbmsy, bbmsy_overfished, overfishing, high_overfishing, bbmsy_overexploited) %>%
  filter(control_l=="leasable_control_stock"| !is.na(year_to_l) & leasable == 1 |!is.na(year_to_l) & leasable == 0)%>%
  mutate(period = as.factor(year), stocklong = as.factor(stocklong), FisheryType= as.factor(FisheryType), region = as.factor(region),
         before_after_L = ifelse(year>= year_to_l, 1, 0))%>%
  mutate(before_after_L= ifelse(is.na(before_after_L), 0, before_after_L))%>%
  mutate(year = as.factor(year),
         region = as.factor(region), FisheryType = as.factor(FisheryType), before_after_L = as.factor(before_after_L),
         overfishing = as.factor(overfishing), high_overfishing= as.factor(high_overfishing), bbmsy_overfished = as.factor(bbmsy_overfished), bbmsy_overexploited=as.factor(bbmsy_overexploited))

leasable_f_series = L_series %>%
  filter(ffmsy>0)

model_L_2 = glmmTMB(overfishing  ~before_after_L +    (1|region)+ar1(year + 0 | stocklong), family= binomial, data = leasable_f_series)

#check residuals
simulationOutput <- simulateResiduals(fittedModel = model_L_2, plot = T)
testDispersion(simulationOutput)#no significant overdispersion

leasable_b_series = L_series %>%
  filter(bbmsy>0)

model_L_5 = glmmTMB(bbmsy_overfished ~ before_after_L +(1|year)+ar1(year + 0 | stocklong), family= binomial, data = leasable_b_series)

#check residuals, look quite alright
simulationOutput <- simulateResiduals(fittedModel = model_L_5, plot = T)
testDispersion(simulationOutput)#some significant overdispersion actually, but it's small (1.04 compared to modeled)

#Pooled attribute
P_series = series%>%
  select(stocklong, year,  region, FisheryType, year_to_p, control_p, pooled, ffmsy, bbmsy, bbmsy_overfished, overfishing, high_overfishing, bbmsy_overexploited) %>%
  filter(control_p=="pooled_control_stock"| !is.na(year_to_p) & pooled == 1 |!is.na(year_to_p) & pooled == 0)%>%
    mutate(period = as.factor(year), stocklong = as.factor(stocklong), FisheryType= as.factor(FisheryType), region = as.factor(region),
    before_after_P = ifelse(year>= year_to_p, 1, 0))%>%
        mutate(before_after_P= ifelse(is.na(before_after_P), 0, before_after_P))%>%
        mutate(year = as.factor(year),
               region = as.factor(region), FisheryType = as.factor(FisheryType), before_after_P = as.factor(before_after_P),
               overfishing = as.factor(overfishing), high_overfishing= as.factor(high_overfishing), bbmsy_overfished = as.factor(bbmsy_overfished), bbmsy_overexploited=as.factor(bbmsy_overexploited))

pooled_f_series = P_series %>%
        filter(ffmsy>0)

model_P_3 = glmmTMB(overfishing ~before_after_P +  (1|region)+ar1(year + 0 | stocklong), family= binomial, data = pooled_f_series)

  #check residuals, look less nice
  simulationOutput <- simulateResiduals(fittedModel = model_P_3, plot = T)
  testDispersion(simulationOutput)#no significant overdispersion



 pooled_b_series = P_series %>%
        filter(bbmsy>0)

model_P_5 = glmmTMB(bbmsy_overfished ~before_after_P +  ar1(year + 0 | stocklong), family= binomial, data = pooled_b_series)

 simulationOutput <- simulateResiduals(fittedModel = model_P_5, plot = T)
 testDispersion(simulationOutput)#no significant overdispersion

