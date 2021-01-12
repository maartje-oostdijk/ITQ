rm(list = ls())

require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(sjPlot)


#data

datadir = "/Users/mtn1/Dropbox/ITQ_meta/Data/"
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")

try = read.csv(paste0(datadir, "attributes.csv"))




stocks = sort(unique(try$stocklong))

for(i in 1:length(stocks)){
  
  sdat = try%>%
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
  select(year, stocklong, bbmsy, ffmsy,  region, FisheryType)

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
         overfishing= as.factor(overfishing))%>%
  mutate(region = ifelse(region == "US Alaska" | region == "US West Coast" | region == "US East Coast" |
                           region == "US Southeast and Gulf", "USA", as.character(region)))%>%
  mutate(region = ifelse(region == "Canada West Coast"| region == "Canada East Coast", "Canada",  as.character(region)))%>%
mutate(region = ifelse(region == "European Union"| region == "Europe non EU", "Europe", as.character(region)))%>%
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

#model for each management regime
try = try%>%
  select(-X)

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

f_series$pred<- predict(f_m , newdata = f_series, type="response", allow.new.levels=TRUE)

p = ggplot(f_series, aes(x = pred)) +
  geom_histogram()+facet_wrap(~quota)+theme_classic()+xlab("model predicted overfishing")+
  ggtitle("quota")

p1= ggplot(f_series, aes(x = pred)) +
  geom_histogram()+facet_wrap(~individual)+theme_classic()+xlab("model predicted overfishing")+
  ggtitle("individual allocation")


p+p1
#some deviation from uniformity, but no strong pattern
simulationOutput <- simulateResiduals(fittedModel = f_m, plot = T)
testDispersion(simulationOutput)#no significant overdispersion

tab_model(f_m, b_m)

#train & test for model accuracy
## 75% of the sample size
smp_size <- floor(0.75 * nrow(f_series))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(f_series)), size = smp_size)

train <- f_series[train_ind, ]
test <- f_series[-train_ind, ]

f_m = glmmTMB(overfishing ~ rationed+pooled+leasable+transferable+individual+quota+(1|region)+ar1(year + 0 | stocklong),
                        data= train,
                        family= binomial)



test$pred<- predict(f_m , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred. = ifelse(pred>0.6, 1, 0))

#91% accuracy!
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

b_m = glmmTMB(bbmsy_overfished ~ rationed+pooled+leasable+transferable+individual+quota+ ar1(year + 0 | stocklong),
                        data= train,
                        family= binomial)

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

((t1+t2)/
    (p+p1))



