rm(list = ls())
require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(sjPlot)
require(patchwork)


#data
datadir = "/Users/mtn1/Dropbox/ITQ_meta/Data/"

#read in classifications
mn_data = read.csv(paste0(datadir, "all_data_nov2020.csv"))
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
key = read.csv(paste0(datadir, "key_new_ram.csv"))

# read RAM legacy data
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
#load("/Users/mtn1/Dropbox/RLSADB v4.44/DB Files With Assessment Data/DBdata.RData")


#make tidy data https://r4ds.had.co.nz/tidy-data.html

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


stocks = sort(unique(class$stocklong))

for(i in 1:length(stocks)){
  
  sdat = class%>%
    filter(stocklong == stocks[i])
  
  
  
  years = sort(unique(sdat$year[c(1:(length(sdat$year)))]))
  years = years[years!=min(sdat$year)]
  
  out = purrr::map_df(years, function(t){
    
    
    gdat1 = sdat%>%
      filter(year == t-1)
    
    gdat = sdat%>%
      filter(year == t)
    
    num = gdat$year -gdat1$year
    
    if(!(length(num)>0)){df = data_frame(stocklong = stocks[i], brk = "break")
    return(df)}
    
    
    
  })
  
  if(i==1){check = out}else{check= rbind(check, out)}
  
}

#RAM timeseries
series1 =timeseries_values_views %>% left_join(metadata, by = c("stockid", "stocklong"))


series= subset(series1, !is.na(BdivBmsypref | UdivUmsypref | BdivBmgtpref | UdivUmgtpref | SSBdivSSBmsy | SSBdivSSBmgt | FdivFmgt |
                                 FdivFmsy))


#bbmsy & ffmsy
series$bbmsy = ifelse(!is.na(series$BdivBmsypref), series$BdivBmsypref, NA)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$BdivBmgtpref), series$BdivBmsypref, series$bbmsy)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$SSBdivSSBmsy), series$SSBdivSSBmsy, series$bbmsy)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$SSBdivSSBmgt), series$SSBdivSSBmgt, series$bbmsy)

series$ffmsy = ifelse(!is.na(series$UdivUmsypref), series$UdivUmsypref, NA)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$UdivUmgtpref), series$UdivUmsypref, series$ffmsy)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$FdivFmsy), series$FdivFmsy, series$ffmsy)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$FdivFmgt), series$FdivFmgt, series$ffmsy)

filter_out = unique(US_extra$stocklong)


series = series %>%
  filter(!(stocklong %in% filter_out))%>%
  select(year, stocklong, bbmsy, ffmsy, region, FisheryType)

series = bind_rows(series, US_extra) #add additional datapoints from catchshareindicators.org!




series = series %>%
  filter(!is.na(region))%>%
  mutate(bbmsy_overfished = ifelse(bbmsy<0.8, 1, 0),
         bbmsy_overfished = ifelse(is.na(bbmsy), NA, bbmsy_overfished),
         bbmsy_overfished = as.factor(bbmsy_overfished),
         bbmsy_overexploited = ifelse(bbmsy<0.5, 1, 0),
         bbmsy_overexploited = ifelse(is.na(bbmsy), NA, bbmsy_overexploited),
         bbmsy_overexploited = as.factor(bbmsy_overexploited),
         overfishing = ifelse(ffmsy>1.1, 1, 0),
         overfishing = ifelse(is.na(ffmsy), NA, overfishing),
         overfishing= as.factor(overfishing),
         high_overfishing = ifelse(ffmsy>1.5, 1, 0),
         high_overfishing = ifelse(is.na(ffmsy), NA, high_overfishing),
         high_overfishing= as.factor(high_overfishing))%>%
  mutate(region = ifelse(region == "US Alaska" | region == "US West Coast" | region == "US East Coast" |
                           region == "US Southeast and Gulf", "USA", as.character(region)))%>%
  mutate(region = ifelse(region == "Canada West Coast"| region == "Canada East Coast", "Canada",  as.character(region)))%>%
  mutate(region = ifelse(region == "European Union"| region == "Europe non EU", "Europe", as.character(region)))%>%
  mutate(FisheryType = ifelse(FisheryType == "Invertebrate"| FisheryType == "Forage Fish", FisheryType, "Marine fish and sharks/rays"))%>%
  distinct()

#filter observations from timeseries before first year f/fmsy >0.5 
min_years = series %>%
  group_by(stocklong)%>%filter(ffmsy > 0.5)%>%
  summarise(min_year_ffmsy0.5 = min(year)) %>%
  select(stocklong, min_year_ffmsy0.5)

non_ffmsy = series %>%
  group_by(stocklong)%>%filter(is.na(ffmsy))
non_ffmsy_stocks = unique(non_ffmsy$stocklong)

series = series %>%
  left_join(min_years) %>%
  filter(year >= min_year_ffmsy0.5|stocklong %in% non_ffmsy_stocks)

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

series$ID[duplicated(series$ID)]





b_series = class_2%>%
  filter(!(stocklong %in% check$stocklong))%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(mngmt))%>%
  filter(!is.na(bbmsy))%>%
  filter(bbmsy>0)

b1_management <- glmmTMB(bbmsy_overfished ~ mngmt+ ar1(year + 0 | stocklong),
                         data= class_2,
                         family= binomial)
#looks quite alright..
simulationOutput <- simulateResiduals(fittedModel = b1_management, plot = T)

#make table with model print



f_series = class_2%>%
  filter(!(stocklong %in% check$stocklong))%>%
  filter(!is.na(stocklong))%>%
  filter(!is.na(mngmt))%>%
  filter(!is.na(ffmsy))%>%
  filter(ffmsy>0)



f1_management <- glmmTMB(overfishing ~ mngmt+ (1|region)+ar1(year + 0 | stocklong),
                         data= f_series,
                         family= binomial)


c = f_series%>%group_by(year, overfishing)%>%tally()

#looks quite alright..
simulationOutput <- simulateResiduals(fittedModel = f1_management, plot = T)


#model outcome tables
tab_model(f1_management, b1_management)



#train & test for model accuracy

## 75% of the sample size
smp_size <- floor(0.75 * nrow(f_series))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(f_series)), size = smp_size)

train <- f_series[train_ind, ]
test <- f_series[-train_ind, ]

f1_management = glmmTMB(overfishing ~ mngmt+(1|region)+ar1(year + 0 | stocklong),
                        data= train,
                        family= binomial)



test$pred<- predict(f1_management , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred. = ifelse(pred>0.6, 1, 0))

#92% accuracy
test = test %>% 
  mutate(accurate = 1*(pred. == overfishing))
sum(test$accurate)/nrow(test)

t1 = ggplot(test, aes(x = overfishing, y = pred)) +
  geom_violin()+geom_boxplot(width=0.03)+ylab("model predicted overfishing")+
  theme_classic()


t1

## 75% of the sample size
smp_size <- floor(0.75 * nrow(b_series))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(b_series)), size = smp_size)

train <- b_series[train_ind, ]
test <- b_series[-train_ind, ]

b1_management = glmmTMB(bbmsy_overfished ~ mngmt+ ar1(year + 0 | stocklong),
                        data= train,
                        family= binomial)

test$pred<- predict(b1_management , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred. = ifelse(pred>0.6, 1, 0))

#95% accuracy
test = test %>% 
  mutate(accurate = 1*(pred. == bbmsy_overfished))
sum(test$accurate)/nrow(test)

t2 = ggplot(test, aes(x = bbmsy_overfished, y = pred)) +
  geom_violin()+geom_boxplot(width=0.03)+ylab("model predicted overfished")+xlab("overfished")+
  theme_classic()

t2


t1+t2