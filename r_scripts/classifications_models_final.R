#script for classification attribute model (Fig 3C in main text, needs the "attributes.csv" which is the result of
#classification script and transboundary.csv which is produced in the management models r script)
rm(list = ls())

require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(sjPlot)


#data and source directories
datadir = "/Users/mtn1/Dropbox/ITQ_meta/Data/"
sdir = "~/Documents/Development/ITQ/r_scripts/"
#load data
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
trans = read.csv(paste0(datadir, "transboundary.csv"))
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
try = read.csv(paste0(datadir, "attributes.csv"))%>%
  select(-X)


#cleaning script
source("~/Documents/development/ITQ/r_scripts/ramdata_clean.R")  # subset RAM data for analysis and make outcome variables

#for filtering stocks that have data gaps
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

#create data with classes and management data
series = series%>%
  left_join(try)%>%
  left_join(trans)%>%
  left_join(stock_gdp)%>%
  mutate(year = as.factor(as.character(year)),
         stocklong=as.factor(as.character(stocklong)),
         region = as.factor(as.character(region)), transferable = as.factor(transferable),
         individual = as.factor(individual), quota = as.factor(quota), effort = as.factor(effort),
         leasable = as.factor(leasable), pooled = as.factor(pooled),  rationed = as.factor(rationed),
         FisheryType=as.factor(FisheryType),
         taxGroup=as.factor(taxGroup),
         transboundary=as.factor(transboundary))%>%
  filter(!is.na(region) & !is.na(individual))%>%
  mutate(ID= paste0(year, "_", stocklong), region_ID = paste0(region, "_", stocklong))%>%
  distinct()


b_series = series%>%
  filter(!(stocklong %in% check$stocklong))%>%#filter stocks with breaks in attribute time series
  filter(!is.na(stocklong))%>%
  filter(!is.na(individual))%>%
  filter(!is.na(bbmsy))%>%
  filter(bbmsy>0)

b_m <- glmmTMB(bbmsy_overfished ~  (1|year) + transboundary+ leasable+ transferable+individual+quota+ rationed+ pooled+ ar1(year+0  | stocklong) ,
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

f_m <- glmmTMB(overfishing ~  transboundary+rationed+pooled+leasable+transferable+individual+quota+ (1|year)+(1|taxGroup)+(1|region)+ar1(year+0  |stocklong),
               data= f_series,
               family=binomial(link="logit"))



#some deviation from uniformity, but no strong pattern
simulationOutput <- simulateResiduals(fittedModel = f_m, plot = T)
testDispersion(simulationOutput)#no significant overdispersion

#model output table
tab_model(f_m, b_m, string.est="Estimate", transform=NULL)


# confidence intervals to dataframes

#function to get the confidence intervals
confidence_to_df = function(x){
  sum = data.frame(summary(x)$coefficients$cond)
  confidence_x =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(x)[c(2:dim(sum)[1]), 3], upper = confint(x)[c(2:dim(sum)[1]), 2], lower = confint(x)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
  rownames(confidence_x) <- rownames(sum[c(2:dim(sum)[1]),])
  colnames(confidence_x) = c("predictors", "estimate", "upper", "lower", "probability")
  
  print(confidence_x)
}

#f/fmsy confidence intervals
confidence_f_m = confidence_to_df(f_m)
confidence_f_m$outcome = "overfishing (f/fmsy > 1.1)"

#b/bmsy confidence intervals
confidence_b_m = confidence_to_df(b_m)
confidence_b_m$outcome = "overfished (b/bmsy < 0.8)"



confidence_attributes = bind_rows(confidence_f_m,confidence_b_m) 




write.csv(confidence_attributes, "~/Dropbox/ITQ_meta/model_outcomes/confidence_attributes.csv")


#train & test for model accuracy
## 75% of the sample size
smp_size <- floor(0.75 * nrow(f_series))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(f_series)), size = smp_size)

train <- f_series[train_ind, ]
test <- f_series[-train_ind, ]
f_m <- glmmTMB(overfishing ~  transboundary+rationed+pooled+leasable+transferable+individual+quota+ (1|year)+(1|taxGroup)+(1|region)+ar1(year+0  |stocklong),
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

b_m <- glmmTMB(bbmsy_overfished ~  (1|year) + transboundary+ leasable+ transferable+individual+quota+ rationed+ pooled+ ar1(year+0  | stocklong) ,
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