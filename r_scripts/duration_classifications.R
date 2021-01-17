rm(list = ls())
require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(gridExtra)
require(nlme)
require(sjPlot)
#data

datadir = "/~/Documents/development/data/"

#read in classifications
mn_data = read.csv(paste0(datadir, "all_data_nov2020.csv"))
key = read.csv(paste0(datadir, "key_new_ram.csv"))
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
try2 = read.csv("~/Dropbox/ITQ_meta/Data/attributes.csv")%>%
  select(-X)

# read RAM legacy data
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")

#cleaning script
source("~/Documents/development/ITQ/r_scripts/ramdata_clean.R")  # subset RAM data for analysis and make outcome variables


classes = mn_data%>% select(fish_admin, name,year, share_admin, idef, fixed_single, fixed_multiple, legal) %>%
  gather(key = "duration", value = percentage, -fish_admin, - name, -year, -share_admin) %>%
  distinct()

#set missing data to zero
classes$percentage[classes$percentage==""] = 0

classes2 = mn_data%>% select(fish_admin, name,year, share_admin, IQ, ITQ, ILQ, RIQ) %>%
  gather(key = "management", value = percentage, -fish_admin, - name, -year, -share_admin) %>%
  distinct()
#set missing data to zero
classes2$percentage[classes2$percentage==""] = 0


#multiply by fish admin share deselect fish admin and calculated "end" percentages of classifications
class = classes %>% mutate(share_admin = as.numeric(as.character(share_admin)), percentage = as.numeric(as.character(percentage)))%>%
  mutate(perc_share = share_admin*percentage/100, name_year = paste0(name,"_", year))%>%
  group_by(name, year, name_year,duration)%>%
  summarise(perc= sum(perc_share))


#add variable with variable type
class$class= class$duration

#this is needed to filter stock years that are 75% or more managed by IQ
class2 = classes2 %>% mutate(share_admin = as.numeric(as.character(share_admin)), percentage = as.numeric(as.character(percentage)))%>%
  mutate(perc_share = share_admin*percentage/100)%>%
  group_by(name, year, management)%>%
  summarise(perc= sum(perc_share))%>%
  mutate(IQ_share = perc, name_year = paste0(name,"_", year))%>%
  group_by(name_year,name, year)%>%
  summarise(perc_IQ = sum(IQ_share))%>%
  filter(perc_IQ>=75) #something strange again with the percentages for US westcoast

stock_years = unique(class2$name_year)

#filter out only IQ stocks
class = class %>%
  filter(name_year %in% stock_years)

classes = class%>%group_by(name, year, class) %>%
  summarise(perc_share = sum(perc))%>%
  filter(name!="")

classes$perc_share[is.na(classes$perc_share)] = 0

#if any of the classifications is bigger than 75 the management should be classified as such...
# so it needs to be the same loop, if the percentage is lower than 75 the management will be classified as
stocks = sort(unique(classes$name))

for(i in 1:length(stocks)){

gdata = classes %>% filter(name == stocks[i])

  years = unique(gdata$year)


  out <- purrr::map_df(years, function(t){

    gdatat <- gdata[gdata$year==t, ]

    if(length(na.omit(gdatat$perc_share))<4){legal= NA} else
      if(gdatat$perc_share[gdatat$class == "legal"] >= 75){legal = 1} else {legal = 0}

    if(length(na.omit(gdatat$perc_share))<4){fixed_multiple = NA} else
      if(gdatat$perc_share[gdatat$class == "fixed_multiple"]>= 75 ){fixed_multiple = 1} else {fixed_multiple = 0}

    if(length(na.omit(gdatat$perc_share))<4){ fixed_single = NA}else
      if(gdatat$perc_share[gdatat$class == "fixed_single"]>= 75) {fixed_single = 1} else {fixed_single = 0}


    if(length(na.omit(gdatat$perc_share))<4){ idef = NA}else
      if(gdatat$perc_share[gdatat$class == "idef"] >= 75) {idef = 1}else {idef = 0}





    df <- data.frame(name = stocks[i], year = t, legal= legal, fixed_multiple= fixed_multiple,fixed_single = fixed_single, idef= idef)

    return(df)

  })
  if(i==1){results <- out}else{results <- rbind(results, out)}

}#warning for binding rows, ignore


try = results%>%left_join(key)%>%
  mutate(stocklong = ifelse(is.na(stocklong), name, as.character(stocklong)))%>%
  select(stocklong, year, legal, idef, fixed_single, fixed_multiple)%>%
  filter(!is.na(idef))
#write duration data
write.csv(try, "~/Dropbox/ITQ_meta/Data/duration_classes.csv")



#format duration attribute for models
try = try %>%
  mutate(attribute = ifelse(fixed_multiple== 1, "fixed_multiple", NA))%>%
  mutate(attribute = ifelse(fixed_single== 1, "1fixed_single", attribute))%>%
  mutate(attribute = ifelse(idef== 1, "idef", attribute))%>%
  mutate(attribute = ifelse(legal== 1, "legal", attribute))




series = try %>%
  left_join(series)%>%
  left_join(try2)


series_b = series%>%
  select(leasable, transferable, attribute, region, stocklong, year, bbmsy_overexploited, bbmsy, bbmsy_overfished, FisheryType)%>%
  filter(!is.na(attribute), !is.na(bbmsy), !is.na(region))%>%
  mutate(leasable = as.factor(leasable), transferable = as.factor(transferable), bbmsy_overfished = as.factor(bbmsy_overfished))

#bbmsy model
b_m <- glmmTMB(bbmsy_overfished ~attribute+ ar1(year + 0 | stocklong),
               data= series_b,
               family= binomial(link="logit"))

summary(b_m)

sum = data.frame(summary(b_m)$coefficients$cond)
confidence_bm =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(b_m)[c(2:dim(sum)[1]), 3], upper = confint(b_m)[c(2:dim(sum)[1]), 2], lower = confint(b_m)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_bm) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_bm) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_bm$outcome = "overfished  (b/bmsy < 0.8)"



f_series = series%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(attribute))%>%
  filter(!is.na(ffmsy))%>%
  filter(ffmsy>0)%>%
  mutate(overfishing = as.factor(overfishing))


#ffmsy model
f_m <- glmmTMB(overfishing ~ attribute + (1|region)+   (1|year) + ar1(year + 0 | stocklong),
               data= f_series,
                family= binomial(link="logit"))
summary(f_m)

tab_model(f_m)
sum = data.frame(summary(f_m)$coefficients$cond)
confidence_fm =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(f_m)[c(2:dim(sum)[1]), 3], upper = confint(f_m)[c(2:dim(sum)[1]), 2], lower = confint(f_m)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_fm) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_fm) = c("predictors", "estimate", "upper", "lower", "probability")


confidence_fm$outcome = "overfishing (f/fmsy > 1.1)"

#model output table
tab_model(f_m, b_m)





#data for figure
confidence = bind_rows(confidence_bm, confidence_fm, confidence_fm2, confidence_bm2)
confidence$probability[is.na(confidence$probability)] = 0

confidence$effect = ifelse(confidence$estimate <0  & confidence$probability <0.05 , "negative", "non-significant")
confidence$effect = ifelse(confidence$estimate >0  & confidence$probability <0.05, "positive", confidence$effect)
confidence$effect = ifelse(confidence$lower == 0 & confidence$estimate ==0 &confidence$upper ==0, "", confidence$effect)



#confidence intervals
confidence$predictor = NA

confidence$predictor[confidence$predictors=="attributefixed_multiple"] = "fixed multiple seasons"
confidence$predictor[confidence$predictors=="attributeidef"] = "indefinitely"
confidence$predictor[confidence$predictors=="attributelegal"] = "legal ability"




#plot
ggplot(transform(confidence1, outcome=factor(outcome,levels=c("overfishing (f/fmsy > 1.1)","overfished  (bbmsy < 0.8)"))), aes( x= predictor, y = estimate, ymax = upper, ymin = lower, colour= effect)) +
  geom_pointrange(position=position_dodge(width=c(0.3)))+
  theme_bw()+ coord_flip() + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("grey", "red")) + xlab("predictor")+
  facet_wrap(~outcome) + theme(legend.position = "bottom", text = element_text(size=20))+ggtitle("Duration")



