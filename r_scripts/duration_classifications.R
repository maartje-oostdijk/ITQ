rm(list = ls())
require(tidyverse)
require(glmmTMB)
require(DHARMa)
require(gridExtra)
require(nlme)
require(sjPlot)
#data

datadir = "/Users/mtn1/Dropbox/ITQ_meta/Data/"

#read in classifications
mn_data = read.csv(paste0(datadir, "all_data_nov2020.csv"))
key = read.csv(paste0(datadir, "key_new_ram.csv"))
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))

# read RAM legacy data
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")


#make tidy data https://r4ds.had.co.nz/tidy-data.html

classes = mn_data%>% select(fish_admin, name,year, share_admin, idef, fixed_single, fixed_multiple, legal) %>% 
  gather(key = "duration", value = percentage, -fish_admin, - name, -year, -share_admin) %>%
  distinct()
#filter(percentage!="TBD" & percentage!="NA")

#set missing data to zero
classes$percentage[classes$percentage==""] = 0

classes2 = mn_data%>% select(fish_admin, name,year, share_admin, IQ, ITQ, ILQ, RIQ) %>% 
  gather(key = "management", value = percentage, -fish_admin, - name, -year, -share_admin) %>%
  distinct()
#filter(percentage!="TBD" & percentage!="NA")
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
# something mixed so basically something we can't use



stocks = sort(unique(classes$name))

for(i in 1:length(stocks)){
  
gdata = classes %>% filter(name == stocks[i])
  #gdata = classes %>% filter(name == stocks[1])
  ##gdata = classes %>% filter(name == "Black Oreo Pukaki Rise")
  
  years = unique(gdata$year)
  
  
  out <- purrr::map_df(years, function(t){
    
    gdatat <- gdata[gdata$year==t, ]
    #gdatat <- gdata[gdata$year==1990, ]
    
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

write.csv(try, "~/Dropbox/ITQ_meta/Data/duration_classes.csv")




try = try %>%
  mutate(attribute = ifelse(fixed_multiple== 1, "fixed_multiple", NA))%>%
  mutate(attribute = ifelse(fixed_single== 1, "1fixed_single", attribute))%>%
  mutate(attribute = ifelse(idef== 1, "idef", attribute))%>%
  mutate(attribute = ifelse(legal== 1, "legal", attribute))


#timeseries values views contains the actual assessment data (ssb cpue etc) and the meta-data contains the stock data, these can be linked
#as an example :
series =timeseries_values_views %>% 
  left_join(metadata, by = c("stockid", "stocklong")) 

series= subset(series, !is.na(BdivBmsypref | UdivUmsypref | BdivBmgtpref | UdivUmgtpref | SSBdivSSBmsy | SSBdivSSBmgt | FdivFmgt |
                                FdivFmsy))

series = series %>%
  filter(!(stocklong %in% US_extra$stocklong))


series = bind_rows(series, US_extra) #add additional datapoints from catchshareindicators.org!


#bbmsy & ffmsy
series$bbmsy = ifelse(!is.na(series$BdivBmsypref), series$BdivBmsypref, NA)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$BdivBmgtpref), series$BdivBmsypref, series$bbmsy)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$SSBdivSSBmsy), series$SSBdivSSBmsy, series$bbmsy)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$SSBdivSSBmgt), series$SSBdivSSBmgt, series$bbmsy)

series$ffmsy = ifelse(!is.na(series$UdivUmsypref), series$UdivUmsypref, NA)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$UdivUmgtpref), series$UdivUmsypref, series$ffmsy)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$FdivFmsy), series$FdivFmsy, series$ffmsy)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$FdivFmgt), series$FdivFmgt, series$ffmsy)

try2 = read.csv("~/Dropbox/ITQ_meta/Data/attributes.csv")%>%
  select(-X)

series = try %>%
  mutate(year = as.numeric(as.character(year)))%>%
  left_join(series)%>%
  left_join(try2)

series = series %>% 
  filter(year>1989)%>%
  filter(!is.na(region))%>%
  mutate(bbmsy_overfished = ifelse(bbmsy<0.8, 1, 0),
         bbmsy_overfished = ifelse(is.na(bbmsy), NA, bbmsy_overfished),
         bbmsy_overexploited = ifelse(bbmsy<0.5, 1, 0),
         bbmsy_overexploited = ifelse(is.na(bbmsy), NA, bbmsy_overexploited),
         overfishing =ifelse(ffmsy>1.1, 1, 0),
         overfishing = ifelse(is.na(ffmsy), NA, overfishing),
         high_overfishing = ifelse(ffmsy>1.5, 1, 0),
         high_overfishing = ifelse(is.na(ffmsy), NA, high_overfishing))%>%
  mutate(region = ifelse(region == "US Alaska" | region == "US West Coast" | region == "US East Coast" |
                           region == "US Southeast and Gulf", "USA", as.character(region)))%>%
  mutate(region = ifelse(region == "Canada West Coast"| region == "Canada East Coast", "Canada",  as.character(region)))%>%
  mutate(region = ifelse(region == "European Union"| region == "Europe non EU", "Europe", as.character(region)))%>%
  mutate(FisheryType = ifelse(FisheryType == "Invertebrate"| FisheryType == "Forage Fish", FisheryType, "Marine fish and sharks/rays"))%>%
  mutate(year = as.factor(year), attribute = as.factor(attribute),
       region = as.factor(region), FisheryType = as.factor(FisheryType))

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

ch = series%>%
  select(attribute, stocklong)%>%
  distinct()%>%
  group_by(attribute)%>%
  tally()


series_b = series%>%
  select(leasable, transferable, attribute, region, stocklong, year, bbmsy_overexploited, bbmsy, bbmsy_overfished, FisheryType)%>%
  filter(!is.na(attribute), !is.na(bbmsy), !is.na(region))%>%
  mutate(leasable = as.factor(leasable), transferable = as.factor(transferable), bbmsy_overfished = as.factor(bbmsy_overfished))

#adding leasing to the model doesn't change the estimates
b_m <- glmmTMB(bbmsy_overfished ~attribute+leasable+  (1|FisheryType)+ (1|region)+ar1(year + 0 | stocklong),
               data= series_b,
               family= binomial(link="logit"))
#adding T to the model doesn't change the estimates
b_m <- glmmTMB(bbmsy_overfished ~attribute+leasable+ transferable+  (1|FisheryType)+ (1|region)+ar1(year + 0 | stocklong),
               data= series_b,
               family= binomial(link="logit"))
#or both without the fisherytype random effect (convergence issue) still no qualitative difference
b_m <- glmmTMB(bbmsy_overfished ~attribute+leasable+ transferable+   (1|region)+ar1(year + 0 | stocklong),
               data= series_b,
               family= binomial(link="logit"))

#stick with original model
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

#adding leasing and transferable still makes for no significant effects
f_m <- glmmTMB(overfishing ~ leasable+ transferable +attribute + (1|region)+   (1|year) + ar1(year + 0 | stocklong),
               data= f_series,
               family= binomial(link="logit"))
#stick with original model
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


tab_model(f_m, b_m)

#decreased overfished





confidence = bind_rows(confidence_bm, confidence_fm, confidence_fm2, confidence_bm2)
confidence$probability[is.na(confidence$probability)] = 0

confidence$effect = ifelse(confidence$estimate <0  & confidence$probability <0.05 , "negative", "non-significant")
confidence$effect = ifelse(confidence$estimate >0  & confidence$probability <0.05, "positive", confidence$effect)
confidence$effect = ifelse(confidence$lower == 0 & confidence$estimate ==0 &confidence$upper ==0, "", confidence$effect)



#exclude regional dummy
#confidence_rm = confidence[!grepl(paste('region'), confidence$predictors), ]
confidence$predictor = NA

confidence$predictor[confidence$predictors=="attributefixed_multiple"] = "fixed multiple seasons"
confidence$predictor[confidence$predictors=="attributeidef"] = "indefinitely"
confidence$predictor[confidence$predictors=="attributelegal"] = "legal ability"



#confidence1 = confidence%>%
 # filter(outcome =="overfishing (f/fmsy > 1.1)" | outcome == "overfished  (bbmsy < 0.8)")

write.csv(confidence, "~/Dropbox/ITQ_meta/model_outcomes/confidence_intervals_duration.csv")

#plot
ggplot(transform(confidence1, outcome=factor(outcome,levels=c("overfishing (f/fmsy > 1.1)","overfished  (bbmsy < 0.8)"))), aes( x= predictor, y = estimate, ymax = upper, ymin = lower, colour= effect)) +
  geom_pointrange(position=position_dodge(width=c(0.3)))+
  theme_bw()+ coord_flip() + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("grey", "red")) + xlab("predictor")+
  facet_wrap(~outcome) + theme(legend.position = "bottom", text = element_text(size=20))+ggtitle("Duration")


write.csv(confidence, "~/Dropbox/ITQ_meta/model_outcomes/confidence_duration.csv")




#duration test
## 75% of the sample size
smp_size <- floor(0.75 * nrow(f_series))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(f_series)), size = smp_size)

train <- f_series[train_ind, ]
test <- f_series[-train_ind, ]

model_fm= glmmTMB(overfishing ~attribute + (1|region)+   (1|year) + ar1(year + 0 | stocklong), family= binomial(link="logit"), data = train)


test$pre<- predict(model_fm , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred = ifelse(pre>0.6, 1, 0))

#90% accuracy
test = test %>% 
  mutate(accurate = 1*(pred == overfishing))
sum(test$accurate)/nrow(test)

test$overfishing=as.factor(test$overfishing)

t1 = ggplot(test, aes(x = overfishing, y = pre)) +
   geom_violin()+geom_boxplot(width=0.03)+ylab("model predicted overfishing")+xlab("overfishing")+
  theme_classic()


t1
series_b$bbmsy_overfished= as.factor(series_b$bbmsy_overfished)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(series_b))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(series_b)), size = smp_size)

train <- series_b[train_ind, ]
test <- series_b[-train_ind, ]


b_m <- glmmTMB(bbmsy_overfished ~attribute+ ar1(year + 0 | stocklong),
               data= train,
               family= binomial(link="logit"))

test$pre<- predict(b_m , newdata = test, type="response", allow.new.levels=TRUE)

test = test %>%
  mutate(pred = ifelse(pre>0.6, 1, 0))

#92% accuracy
test = test %>% 
  mutate(accurate = 1*(pred == bbmsy_overfished))
sum(test$accurate)/nrow(test)


t2 = ggplot(test, aes(x = bbmsy_overfished, y = pre)) +
  geom_violin()+geom_boxplot(width=0.03)+ylab("model predicted overfished")+xlab("overfished")+
  theme_classic()


t2



t1+t2

