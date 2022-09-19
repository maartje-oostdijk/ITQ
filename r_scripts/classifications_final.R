#this script classifies all attributes of management, needed for classification attribute model
#and DiD per attribute

rm(list = ls())
require(tidyverse)

#data
datadir = "/Users/mtn1/Dropbox/ITQ_meta/Data/"
#read in classifications
mn_data = read.csv(paste0(datadir, "all_data_nov2020.csv"))
key = read.csv(paste0(datadir, "key_new_ram.csv"))



#make tidy data
classes = mn_data%>% select(fish_admin, name, year, share_admin, rec, ITQ, ILQ, IQ, SGQP, GQP, IRQP, RQP, RIQ, ITE, IE, TE, OA) %>%
  gather(key = "mngmt", value = percentage, -fish_admin, - name, -year, -share_admin) %>%
  distinct()

#set missing data to zero
classes$percentage[classes$percentage==""] = 0
classes$percentage[classes$percentage=="TBD"] = 0

#multiply by fish admin share deselect fish admin and calculated "end" percentages of classifications
class = classes %>% mutate(share_admin = as.numeric(as.character(share_admin)), percentage = as.numeric(as.character(percentage)))%>%
  mutate(perc_share = share_admin*percentage/100)%>%
  group_by(name, year, mngmt)%>%
  summarise(perc_share= sum(perc_share))%>%
  rename(class= mngmt)

class$perc_share[is.na(class$perc_share)] = 0



#if any of the classifications is bigger than 75 the management should be classified as such...

stocks = sort(unique(classes$name))

for(i in 1:length(stocks)){

  gdata = class %>% filter(name == stocks[i])
  years = unique(gdata$year)


  out <- purrr::map_df(years, function(t){

    gdatat <- gdata[gdata$year==t, ]

    if(length(na.omit(gdatat$perc_share))<6){ transferable= NA} else
      if(gdatat$perc_share[gdatat$class == "ITQ"] + gdatat$perc_share[gdatat$class == "ITE"] >= 75){transferable = 1} else {transferable  = 0}

    if(length(na.omit(gdatat$perc_share))<6){ individual = NA} else
      if(gdatat$perc_share[gdatat$class == "ITQ"]+ gdatat$perc_share[gdatat$class == "ILQ"]+gdatat$perc_share[gdatat$class == "IQ"] +
         gdatat$perc_share[gdatat$class == "ITE"] + gdatat$perc_share[gdatat$class == "IE"] + gdatat$perc_share[gdatat$class == "RIQ"]>= 75 ){individual = 1} else {individual = 0}

    if(length(na.omit(gdatat$perc_share))<6){ quota = NA}else
      if(gdatat$perc_share[gdatat$class == "ITQ"]+gdatat$perc_share[gdatat$class == "IQ"] +gdatat$perc_share[gdatat$class == "ILQ"]+
         gdatat$perc_share[gdatat$class == "RQP"]+ gdatat$perc_share[gdatat$class == "RIQ"] + gdatat$perc_share[gdatat$class == "IRQP"] + gdatat$perc_share[gdatat$class == "SGQP"]+  gdatat$perc_share[gdatat$class == "GQP"]>= 75) {quota = 1} else {quota = 0}

    if(length(na.omit(gdatat$perc_share))<6){ effort = NA}else
      if(gdatat$perc_share[gdatat$class == "TE"] + gdatat$perc_share[gdatat$class == "ITE"] + gdatat$perc_share[gdatat$class == "IE"]>= 75) {effort = 1}else {effort = 0}

    if(length(na.omit(gdatat$perc_share))<6){ leasable = NA}else
      if(gdatat$perc_share[gdatat$class == "ILQ"] + gdatat$perc_share[gdatat$class == "ITQ"]>= 75) {leasable = 1}else {leasable = 0}

    if(length(na.omit(gdatat$perc_share))<6){ pooled = NA}else
      if(gdatat$perc_share[gdatat$class == "GQP"] + gdatat$perc_share[gdatat$class == "IRQP"]+ gdatat$perc_share[gdatat$class == "RQP"] + gdatat$perc_share[gdatat$class == "SGQP"] >= 75){pooled = 1}else {pooled = 0}

    if(length(na.omit(gdatat$perc_share))<6){ rationed = NA}else
      if(gdatat$perc_share[gdatat$class == "RIQ"] + gdatat$perc_share[gdatat$class == "IRQP"]+ gdatat$perc_share[gdatat$class == "RQP"] >= 75) {rationed = 1}else {rationed = 0}

    if(length(na.omit(gdatat$perc_share))<6){ SG = NA}else
      if(gdatat$perc_share[gdatat$class == "SGQP"]  >= 75) {SG = 1}else {SG = 0}


    df <- data.frame(name = stocks[i], year = t,leasable=leasable, transferable = transferable, individual= individual, quota= quota,
                     effort = effort, pooled = pooled, rationed= rationed, SG=SG)

    return(df)

  })
  if(i==1){results <- out}else{results <- rbind(results, out)}

}#warning for binding rows, ignore



try = results%>%
  left_join(key)%>%
  mutate(stocklong = ifelse(is.na(stocklong), as.character(name), as.character(stocklong)),
         year = as.numeric(as.character(year)))%>%
  select(stocklong, year, transferable, individual, quota, effort, leasable, SG, pooled, rationed)%>%
  filter(!is.na(transferable))%>%
  filter(!(transferable==0 & effort ==0 & quota ==0 & individual == 0))





write.csv(try, "/Users/mtn1/Dropbox/ITQ_meta/Data/attributes.csv")
