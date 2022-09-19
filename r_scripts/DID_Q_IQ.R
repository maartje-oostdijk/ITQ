#script for the final DiD analysis (Fig 4B) in which individual stocks are paired to one another
#these are only stocks that already have TAC in place and shift to individual or individual transferable quota

#clear workspace
rm(list = ls())

require(tidyverse)
require(glmmTMB)
require(nlme)
require(DHARMa)
require(patchwork)

#directories
datadir = "~/Documents/Development/ITQ/data/"
datadir2 = "/Users/mtn1/Dropbox/ITQ_meta/Data/"

#data
did = read.csv(paste0(datadir, "Q_IQ_ITQ_strict2.csv"))
US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
trans = read.csv(paste0(datadir2, "transboundary.csv"))%>%
  select(-X)

# read RAM legacy data
load("~/Dropbox/RAM v4.491 Files (1-16-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")

#cleaning script
source("~/Documents/Development/ITQ/r_scripts/ramdata_clean.R")  # subset RAM data for analysis and make outcome variables


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

did = did %>% 
  left_join(vars)%>%
  left_join(trans)

#create cut-off point so that both fisheries will be under quota management
minimum = did %>% group_by(baci_pair)%>%
  summarise(minimum_year = min(min_year))

did = did %>% left_join(minimum)

#cut-off for time-series
series_did = did %>% left_join(series) %>%
  filter(year >= minimum_year & year <= final_year)%>%
  mutate(ba = ifelse(year < intervention_year, "0. before", "1. after"))#go through baci-pairs and assign before and after to each RAM year

#now all is ready to set up baci model :)
series_did = series_did %>%
  filter(year > 1983)


#DiD fixed effects
series_did$ba = as.factor(ifelse(series_did$ba=="1. after", 1, 0))
series_did$beforeaftercontrol = as.factor(ifelse(series_did$beforeaftercontrol=="impact", 1, 0))
series_did$baci_pair = as.factor(series_did$baci_pair)
series_did$year= as.factor(series_did$year)
series_did$stocklong= as.factor(series_did$stocklong)
series_did$taxGroup= as.factor(series_did$taxGroup)
series_did$transboundary= as.factor(series_did$transboundary)

series_did$did = as.factor(ifelse(series_did$beforeaftercontrol==1 & series_did$ba == 1, 1, 0))

f_series = series_did %>%
  filter(!is.na(overfishing))
#model overfishing
m_1 =  glmmTMB(overfishing ~ ba*beforeaftercontrol +  transboundary+ ar1(year + 0 | stocklong) ,  family=binomial(),  data = f_series)

sum = data.frame(summary(m_1)$coefficients$cond)
confidence_m_1 =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(m_1)[c(2:dim(sum)[1]), 3], upper = confint(m_1)[c(2:dim(sum)[1]), 2], lower = confint(m_1)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_m_1) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_m_1) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_m_1$outcome = "overfishing (f/fmsy > 1.1)"
confidence_m_1$order = 1




series_b = series_did %>%
  filter(!is.na(bbmsy_overfished))
#model overfished
m_b1 =  glmmTMB(bbmsy_overfished ~ ba*beforeaftercontrol +  transboundary+  ar1(year + 0 | stocklong),  family=binomial(),  data = series_b)

sum = data.frame(summary(m_b1)$coefficients$cond)
confidence_m_b1 =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(m_b1)[c(2:dim(sum)[1]), 3], upper = confint(m_b1)[c(2:dim(sum)[1]), 2], lower = confint(m_b1)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_m_b1) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_m_b1) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_m_b1$outcome = "overfished (b/bmsy < 0.8)"
confidence_m_b1$order = 2


confidence_interval = bind_rows(confidence_m_1, 
                                confidence_m_b1)

confidence_interval$predictor= NA
confidence_interval$predictor[confidence_interval$predictors=="ba1:beforeaftercontrol1"] = "difference in difference interaction"


confidence_i = confidence_interval %>%
  filter(!is.na(predictor))


confidence_i$effect = ifelse(confidence_i$estimate <0  & confidence_i$probability <0.05 , "negative", "non-significant")
confidence_i$effect = ifelse(confidence_i$estimate >0  & confidence_i$probability <0.05, "positive", confidence_i$effect)
confidence_i$effect = ifelse(confidence_i$lower == 0 & confidence_i$estimate ==0 &confidence_i$upper ==0, "", confidence_i$effect)


write.csv(confidence_i, "/Users/mtn1/Dropbox/ITQ_meta/Data/paired_confidence.csv")

#plot confidence intervals
ggplot(confidence_i, aes( x= rev(order), y = estimate, ymax = upper, ymin = lower, colour= effect, shape= effect)) +
  geom_pointrange(position=position_dodge(width=c(0.3)))+scale_x_continuous(breaks = rev(confidence_i$order), labels = confidence_i$outcome)+
  theme_classic()+ coord_flip() + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("black", "grey")) + xlab("")+
  theme(legend.position = "bottom", text = element_text(size=12))+
  scale_shape_manual(values = c(21,19,19))+ggtitle("paired approach Individual (n=19)")


tab_model(m_1, m_b1, string.est="Estimate", transform=NULL)

