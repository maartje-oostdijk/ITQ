#script for the final DiD analysis (Fig 4B) in which individual stocks are paired to one another

#clear workspace
rm(list = ls())

require(tidyverse)
require(glmmTMB)
require(nlme)
require(DHARMa)
require(patchwork)


datadir = "~/Dropbox/ITQ_meta/Data/"

did = read.csv(paste0(datadir, "Q_IQ_ITQ_strict2.csv"))
US_pacific = read.csv(paste0(datadir, "extra_stocks.csv"))

# read RAM legacy data
load("/Users/mtn1/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")

#these are only stocks that already have TAC in place and shift to individual or individual transferable quota


did = did %>%
  mutate(yof_iq = as.numeric(as.character(yof_iq),
                             beforeaftercontrol = as.character(beforeaftercontrol)),
                             yof_quota = as.numeric(as.character(yof_quota)))



ifelse(!is.na(did$yof_iq) & did$beforeaftercontrol == "control" , as.integer(did$yof_iq), 2017)


#create year that will be cut-off point, i.e. control will no longer be control
vars= did %>%
  group_by(baci_pair)%>%
  filter(beforeaftercontrol=="impact")%>%
  mutate(intervention_year = yof_iq)%>%
    select(baci_pair, intervention_year)

did = did %>% left_join(vars)

#create cut-off point so that both fisheries will be under quota management
minimum = did %>% group_by(baci_pair)%>%
  summarise(minimum_year = min(min_year))


did = did %>% left_join(minimum)

series =timeseries_values_views %>% left_join(metadata, by = c("stockid", "stocklong"))
series= subset(series, !is.na(BdivBmsypref | UdivUmsypref | BdivBmgtpref | UdivUmgtpref | SSBdivSSBmsy | SSBdivSSBmgt | FdivFmgt |
                                FdivFmsy))%>%
  filter(!(stocklong %in% US_pacific$stocklong))



#bbmsy & ffmsy
series$bbmsy = ifelse(!is.na(series$BdivBmsypref), series$BdivBmsypref, NA)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$BdivBmgtpref), series$BdivBmsypref, series$bbmsy)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$SSBdivSSBmsy), series$SSBdivSSBmsy, series$bbmsy)
series$bbmsy = ifelse(is.na(series$bbmsy) & !is.na(series$SSBdivSSBmgt), series$SSBdivSSBmgt, series$bbmsy)

series$ffmsy = ifelse(!is.na(series$UdivUmsypref), series$UdivUmsypref, NA)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$UdivUmgtpref), series$UdivUmsypref, series$ffmsy)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$FdivFmsy), series$FdivFmsy, series$ffmsy)
series$ffmsy = ifelse(is.na(series$ffmsy) & !is.na(series$FdivFmgt), series$FdivFmgt, series$ffmsy)

series = bind_rows(series, US_pacific) #add additional datapoints from catchshareindicators.org!



#cut-off for time-series
series_did = did %>% left_join(series) %>%
  filter(year >= minimum_year & year <= final_year)%>%
  mutate(ba = ifelse(year < intervention_year, "0. before", "1. after"))#go through baci-pairs and assign before and after to each RAM year

#now all is ready to set up baci model :)
series_did = series_did %>%
  filter(year > 1983)


# model for over or under bbmsy ffmsy
series_did$ffmsy_overfishing = as.factor(ifelse(series_did$ffmsy>1.1, 1, 0))
series_did$ffmsy_high_overfishing = as.factor(ifelse(series_did$ffmsy>1.5, 1, 0))
series_did$bbmsy_overfished = as.factor(ifelse(series_did$bbmsy<0.8, 1, 0))
series_did$bbmsy_overexploited = as.factor(ifelse(series_did$bbmsy<0.5, 1, 0))


#series_did$collapsed = as.factor(ifelse(series_did$bbmsy<0.4, 1, 0)
series_did$treatmentstock_iq_type = as.factor(series_did$treatmentstock_iq_type)

series_did$ba = as.factor(ifelse(series_did$ba=="1. after", 1, 0))
series_did$beforeaftercontrol = as.factor(ifelse(series_did$beforeaftercontrol=="impact", 1, 0))
series_did$baci_pair = as.factor(series_did$baci_pair)
series_did$year= as.factor(series_did$year)
series_did$stocklong= as.factor(series_did$stocklong)

series_did$did = as.factor(ifelse(series_did$beforeaftercontrol==1 & series_did$ba == 1, 1, 0))

f_series = series_did %>%
  filter(!is.na(ffmsy_overfishing))
#model overfishing
m_1 =  glmmTMB(ffmsy_overfishing ~ ba*beforeaftercontrol +  ar1(year + 0 | stocklong) ,  family=binomial(),  data = f_series)

sum = data.frame(summary(m_1)$coefficients$cond)
confidence_m_1 =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(m_1)[c(2:dim(sum)[1]), 3], upper = confint(m_1)[c(2:dim(sum)[1]), 2], lower = confint(m_1)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_m_1) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_m_1) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_m_1$outcome = "high overfishing (f/fmsy > 1.5)"
confidence_m_1$order = 1

series_b = series_did %>%
  filter(!is.na(bbmsy_overfished))
#model overfished
m_b1 =  glmmTMB(bbmsy_overfished ~ ba*beforeaftercontrol +    ar1(year + 0 | stocklong),  family=binomial(),  data = series_b)

sum = data.frame(summary(m_b1)$coefficients$cond)
confidence_m_b1 =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(m_b1)[c(2:dim(sum)[1]), 3], upper = confint(m_b1)[c(2:dim(sum)[1]), 2], lower = confint(m_b1)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_m_b1) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_m_b1) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_m_b1$outcome = "high overfished (b/bmsy < 0.5)"
confidence_m_b1$order = 3


confidence_interval = bind_rows(confidence_m_1, confidence_m_b1)

confidence_interval$predictor= NA
confidence_interval$predictor[confidence_interval$predictors=="ba1:beforeaftercontrol1"] = "difference in difference interaction"


confidence_i = confidence_interval %>%
  filter(!is.na(predictor))


confidence_i$effect = ifelse(confidence_i$estimate <0  & confidence_i$probability <0.05 , "negative", "non-significant")
confidence_i$effect = ifelse(confidence_i$estimate >0  & confidence_i$probability <0.05, "positive", confidence_i$effect)
confidence_i$effect = ifelse(confidence_i$lower == 0 & confidence_i$estimate ==0 &confidence_i$upper ==0, "", confidence_i$effect)


f_series = series_did %>%
  filter(!is.na(ffmsy_overfishing))%>%
  filter(ffmsy>0)



library(nlme)


f_series=f_series%>%
  mutate(ID=paste0(year,stocklong))


length(unique(f_series$ID))

x = lme(fixed = log(ffmsy) ~ ba*beforeaftercontrol,
  random = (~ year | stocklong), correlation = corARMA(value = rep(0.2, 2),
                                                    form = (~ year | stocklong), p = 1, q = 1),
  method = "REML", data = f_series, na.action = na.omit)


#plot confidence intervals
ggplot(confidence_i, aes( x= rev(order), y = estimate, ymax = upper, ymin = lower, colour= effect, shape= effect)) +
  geom_pointrange(position=position_dodge(width=c(0.3)))+scale_x_continuous(breaks = rev(confidence_i$order), labels = confidence_i$outcome)+
  theme_classic()+ coord_flip() + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("grey","black", "grey")) + xlab("")+
  theme(legend.position = "bottom", text = element_text(size=12))+
  scale_shape_manual(values = c(19,21,19))+ggtitle("paired approach Individual (n=19)")

#model coefficients
tab_model(m_1, m_b1)

