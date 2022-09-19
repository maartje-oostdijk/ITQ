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


#check if stocks are transboundary
trans = mn_data%>%
select(fish_admin, name, year)%>%
  distinct()%>%
  group_by(name, year)%>%
  tally()%>%
  mutate(transboundary= as.factor(ifelse(n>1,1,0)))


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
  left_join(trans)%>%
  left_join(gdp_data)%>%
  left_join(key)%>%
  mutate(stocklong = as.character(ifelse(is.na(stocklong), as.character(name), as.character(stocklong))))
  
#write csv to use for attribute files on transboundary stocks
trans = class%>%ungroup()%>%select(stocklong, transboundary,year)%>%distinct()
write.csv(trans, "~/Dropbox/ITQ_meta/Data/transboundary.csv")

#prepare management data in combination with stock data for models
class_2 = class %>%
  ungroup()%>%
  mutate(year = as.numeric(as.character(year)))%>%
  filter(perc >=75)%>%
  left_join(series)%>%
  ungroup()%>%
  mutate(region = as.factor(region), year = as.factor(year),
         mngmt = as.factor(ifelse(mngmt == "TE","1TE",mngmt)),
         stocklong = as.factor(stocklong),
         taxGroup=as.factor(taxGroup))%>%
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

b1_management <- glmmTMB(bbmsy_overfished ~ mngmt+ transboundary+ ar1(year + 0 | stocklong),
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



f1_management <- glmmTMB(overfishing ~ mngmt+ transboundary+ (1|taxGroup)+ (1|region)+ar1(year + 0 | stocklong),
                         data= f_series,
                         family= binomial)
#looks quite alright..
simulationOutput <- simulateResiduals(fittedModel = f1_management, plot = T)


tab_model(f1_management, b1_management, string.est="Estimate", transform=NULL)



#train & test for model accuracy
## 75% of the sample size
smp_size <- floor(0.75 * nrow(f_series))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(f_series)), size = smp_size)

train <- f_series[train_ind, ]
test <- f_series[-train_ind, ]
f_m <- glmmTMB(overfishing ~  mngmt+ year +transboundary+ (1|year)+(1|taxGroup)+(1|region)+ar1(year+0  |stocklong),
               data= train,
               family=binomial(link="logit"))



test$pred<- predict(f_m , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred. = ifelse(pred>0.6, 1, 0))

#89% accuracy!
test = test %>%
  mutate(accurate = 1*(pred. == overfishing))
sum(test$accurate)/nrow(test)

t1 = ggplot(test, aes(x = overfishing, y = pred)) +
  geom_violin()+geom_boxplot(width=0.03)+ylab("model predicted overfishing")+
  theme_classic()+facet_wrap(~quota)


t1

## 75% of the sample size
smp_size <- floor(0.75 * nrow(b_series))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(b_series)), size = smp_size)

train <- b_series[train_ind, ]
test <- b_series[-train_ind, ]

b_m <- glmmTMB(bbmsy_overfished ~  mngmt+ year +transboundary+ ar1(year+0  | stocklong) ,
               data= train,
               family= binomial(link="logit"))

test$pred<- predict(b_m , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred. = ifelse(pred>0.6, 1, 0))

#94% accuracy!
test = test %>% 
  mutate(accurate = 1*(pred. == bbmsy_overfished))
sum(test$accurate)/nrow(test)

t2 = ggplot(test, aes(x = bbmsy_overfished, y = pred)) +
  geom_violin()+geom_boxplot(width=0.03)+ylab("model predicted overfished")+xlab("overfished")+
  theme_classic()

t2

require(patchwork)

t1+t2