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

US_extra = read.csv(paste0(datadir, "extra_stocks.csv"))
trans = read.csv(paste0(datadir, "transboundary.csv"))%>%
  select(-X)

class= read.csv(paste0(datadir, "attributes.csv"))%>%
  filter(!(quota==0 & effort==0))%>%
  filter(quota==1)

#confidence intervals paired approach
confidence_i = read.csv(paste0(datadir, "paired_confidence.csv"))

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

      #if(gdat1$quota == 1 & gdat2$quota == 1 & gdat3$quota == 1 & gdat4$quota == 1  & gdat$quota == 0){year_to_q = t+1}else{year_to_q = NA}
      if(gdat1$individual== 1 & gdat2$individual == 1 & gdat3$individual == 1 & gdat4$individual == 1  & gdat$individual == 0){year_to_i = t+1}else{year_to_i = NA}
      if(gdat1$transferable== 1 & gdat2$transferable == 1 & gdat3$transferable== 1 & gdat4$transferable == 1  & gdat$transferable == 0){year_to_t = t+1}else{year_to_t = NA}
      if(gdat1$leasable== 1 & gdat2$leasable == 1 & gdat3$leasable== 1 & gdat4$leasable == 1  & gdat$leasable == 0){year_to_l = t+1}else{year_to_l = NA}
      if(gdat1$pooled== 1 & gdat2$pooled == 1 & gdat3$pooled== 1 & gdat4$pooled == 1  & gdat$pooled == 0){year_to_p = t+1}else{year_to_p = NA}

      df = data.frame(year = t+1, stocklong =  stocks[i], year_to_i,  year_to_t, year_to_l, year_to_p)
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
  #if(length(dat$year[dat$quota==0])== length(dat$year)) {control_q = "q_control_stock"} else{control_q= "no control"}
  if(length(dat$year[dat$individual==0])== length(dat$year)) {control_i = "i_control_stock"} else{control_i= "no control"}
  if(length(dat$year[dat$leasable==0])== length(dat$year)) {control_l = "l_control_stock"} else{control_l= "no control"}
  if(length(dat$year[dat$pooled==0])== length(dat$year)) {control_p = "p_control_stock"} else{control_p= "no control"}
  if(length(dat$year[dat$transferable==0])== length(dat$year)) {control_t = "t_control_stock"} else{control_t= "no control"}


  df= data_frame(stocklong = t,  control_i,  control_l, control_p, control_t)
  return(df)



})

classes = results%>%
  filter(!(is.na(year_to_i) & is.na(year_to_t)
         & is.na(year_to_l) & is.na(year_to_p)))%>%
  select(-year)%>%
  distinct()%>%
  left_join(class)%>%
  left_join(controls)
  ungroup()

series = series %>%
  left_join(classes)%>%
  left_join(trans)


attributes = c("i", "t", "l", "p")


confidence <- purrr::map_df(attributes, function(x){

  #test
#x="p"
d_series=series
d_series$attr=NA

#if(x=="q"){(d_series$attr=d_series$quota) & (d_series$year_to_x = d_series$year_to_q)}
if(x=="i"){(d_series$attr=d_series$individual)& (d_series$year_to_x = d_series$year_to_i)}
if(x=="t"){(d_series$attr=d_series$transferable) & (d_series$year_to_x = d_series$year_to_t)}
if(x=="l"){(d_series$attr=d_series$leasable) & (d_series$year_to_x = d_series$year_to_l)}
if(x=="p"){(d_series$attr=d_series$pooled) & (d_series$year_to_x = d_series$year_to_p)}


y = paste0(x, "_control_stock")

g_series = d_series%>%
  select(stocklong, year,  region, taxGroup,transboundary, FisheryType, paste0("year_to_", x), paste0("control_", x), ffmsy, bbmsy, bbmsy_overfished, overfishing, overfishing, bbmsy_overfished,
         attr, year_to_x)%>%
  rename(control_x = paste0("control_", x))%>%
  filter(control_x== y| !is.na(year_to_x) & attr == 1 |!is.na(year_to_x) & attr == 0)%>%
  mutate(period = as.factor(year), stocklong = as.factor(stocklong), FisheryType= as.factor(FisheryType), region = as.factor(region),
         did = ifelse(year>= year_to_x, 1, 0))%>%
  mutate(did= ifelse(is.na(did), 0, did))%>%
  mutate(year = as.factor(year),
         region = as.factor(region), taxGroup = as.factor(taxGroup), did = as.factor(did),
         overfishing = as.factor(overfishing), bbmsy_overfished = as.factor(bbmsy_overfished), bbmsy_overfished=as.factor(bbmsy_overfished),
         transboundary= as.factor(transboundary))


f_series = g_series %>%
  filter(ffmsy>0)%>%
  distinct()%>%
  filter(stocklong!="Pollock Faroe Plateau")#switches in two years

n_f = length(unique(f_series$stocklong[f_series$did==1]))

f_m =  glmmTMB(overfishing ~did+ transboundary+(1|taxGroup)+(1|region)+ (1|year)+(1|stocklong)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)

#test for non-significant trends control fishery & treatment fishery
#test = f_series %>%
  #filter(did==0)

#t_m =  glmmTMB(overfishing ~as.factor(control_x)+(1|region)+ (1|year)+(1|stocklong)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)

v = VarCorr(f_m)

#but if random variance approaches 0 model needs to be re-specified
if(v$cond$taxGroup[,1] <0.0001){f_m =  glmmTMB(overfishing ~did+transboundary+(1|region)+ (1|year)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}
if(v$cond$year[,1] <0.0001){f_m =  glmmTMB(overfishing ~did+transboundary+(1|taxGroup)+(1|region)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}
if(v$cond$region[,1] <0.0001){f_m =  glmmTMB(overfishing ~did+ transboundary+(1|taxGroup)+(1|year) ++ ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}

#year & taxGroup
if((v$cond$taxGroup[,1] <0.0001)&
   (v$cond$year[,1] <0.0001)){f_m =  glmmTMB(overfishing ~did+ transboundary+(1|region)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}
#year & region
if((v$cond$region[,1] <0.0001)&
   (v$cond$year[,1] <0.0001)){f_m =  glmmTMB(overfishing ~did+ transboundary+(1|taxGroup)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}
#region and stocklong
if((v$cond$region[,1] <0.0001)&
   (v$cond$taxGroup[,1] <0.0001)){f_m =  glmmTMB(overfishing ~did+transboundary+ (1|year)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}

#all aditional ran eff except AR1 model
if((v$cond$region[,1] <0.0001) & (v$cond$taxGroup[,1] <0.0001)&
   (v$cond$year[,1] <0.0001)){f_m =  glmmTMB(overfishing ~did+ transboundary+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}

sum = data.frame(summary(f_m)$coefficients$cond)
confidence_fm =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(f_m)[c(2:dim(sum)[1]), 3], upper = confint(f_m)[c(2:dim(sum)[1]), 2], lower = confint(f_m)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_fm) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_fm) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_fm$attribute = x
confidence_fm$n = n_f
confidence_fm$outcome = "overfishing (f/fmsy>1.1)"





b_series = g_series %>%
  filter(bbmsy>0)%>%
  distinct()%>%
  filter(stocklong!="Pollock Faroe Plateau")#switches in two years

n_b = length(unique(b_series$stocklong[b_series$did==1]))

#stocklong variance always near 0, removed due to a convergence issue
b_m =  glmmTMB(bbmsy_overfished ~did+(1|region)+ (1|year)+transboundary+(1|taxGroup)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = b_series)

v = VarCorr(b_m)

#but if random variance approaches 0 model needs to be re-specified
if(v$cond$taxGroup[,1] <0.0001){b_m =  glmmTMB(bbmsy_overfished ~did+transboundary+(1|region)+ (1|year)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}
if(v$cond$year[,1] <0.0001){b_m =  glmmTMB(bbmsy_overfished ~did+transboundary+(1|taxGroup)+(1|region)+(1|stocklong)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}
if(v$cond$region[,1] <0.0001){b_m =  glmmTMB(bbmsy_overfished~did+ transboundary+(1|taxGroup)+(1|year) +(1|stocklong) + ar1(year + 0 | stocklong), family= binomial(link="logit"), data = f_series)}

#year & stocklong
if((v$cond$taxGroup[,1] <0.0001)&
   (v$cond$year[,1] <0.0001)){b_m =  glmmTMB(bbmsy_overfished ~did+ transboundary+(1|region)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = b_series)}
#year & region
if((v$cond$region[,1] <0.0001)&
   (v$cond$year[,1] <0.0001)){b_m =  glmmTMB(bbmsy_overfished ~did+transboundary+(1|taxGroup)+ (1|stocklong)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = b_series)}
#region and stocklong
if((v$cond$region[,1] <0.0001)&
   (v$cond$taxGroup[,1] <0.0001)){b_m =  glmmTMB(bbmsy_overfished ~did+ transboundary+(1|year)+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = b_series)}

#all aditional ran eff except AR1 model
if((v$cond$region[,1] <0.0001) & (v$cond$taxGroup[,1] <0.0001)&
   (v$cond$year[,1] <0.0001)){b_m =  glmmTMB(bbmsy_overfished ~did+ transboundary+ar1(year + 0 | stocklong), family= binomial(link="logit"), data = b_series)}




sum = data.frame(summary(b_m)$coefficients$cond)
confidence_bm =   data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(b_m)[c(2:dim(sum)[1]), 3], upper = confint(b_m)[c(2:dim(sum)[1]), 2], lower = confint(b_m)[c(2:dim(sum)[1]), 1],  sum$Pr...z..[c(2:dim(sum)[1])])
rownames(confidence_bm) <- rownames(sum[c(2:dim(sum)[1]),])
colnames(confidence_bm) = c("predictors", "estimate", "upper", "lower", "probability")

confidence_bm$attribute = x
confidence_bm$outcome = "overfished (b/bmsy<0.8)"
confidence_bm$n = n_b

confidence = bind_rows(confidence_fm, confidence_bm)
return(confidence)

})

#make plot with confidence intervals
confidence$effect = ifelse(confidence$estimate <0  & confidence$probability <0.05 , "negative", "non-significant")
confidence$effect = ifelse(confidence$estimate >0  & confidence$probability <0.05, "positive", confidence$effect)
confidence$effect = ifelse(confidence$lower == 0 & confidence$estimate ==0 &confidence$upper ==0, "", confidence$effect)

confidence = confidence%>%
  filter(predictors =="did1")

confidence$predictor=NA
#confidence$predictor = ifelse(confidence$attribute=="q", paste0("Quota (n=",confidence$n,")"), confidence$predictor)
confidence$predictor = ifelse(confidence$attribute=="i", paste0("Individual (n=",confidence$n,")"), confidence$predictor)
confidence$predictor = ifelse(confidence$attribute=="t", paste0("Transferable (n=",confidence$n,")"), confidence$predictor)
confidence$predictor = ifelse(confidence$attribute=="p", paste0("Pooled (n=",confidence$n,")"), confidence$predictor)
confidence$predictor = ifelse(confidence$attribute=="l", paste0("Leasable (n=",confidence$n,")"), confidence$predictor)


confidence1 = confidence%>%
  filter(!(attribute=="i" & outcome=="overfished (b/bmsy<0.8)"))%>%
  filter(!(attribute=="p" & outcome=="overfished (b/bmsy<0.8)"))%>%
  filter(!(attribute=="p" & outcome=="overfishing (f/fmsy>1.1)"))%>%
  filter(!(attribute=="t" & outcome=="overfishing (f/fmsy>1.1)"))


#plot
p1 = ggplot(transform(confidence1, outcome=factor(outcome,levels=c("overfishing (f/fmsy>1.1)","overfished (b/bmsy<0.8)"))), aes( x=factor(predictor, 
        levels = rev(levels(factor(predictor)))),y = estimate, ymax = upper, ymin = lower, colour= effect, shape=effect)) +
  geom_pointrange(position=position_dodge(width=c(0.3)))+
  theme_classic()+ coord_flip() + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("black","light grey")) + xlab("predictor")+scale_shape_manual(values = c(21,19,19))+
  facet_wrap(~outcome, scales = "free_y") + theme(legend.position = "bottom", text = element_text(size=10))+ggtitle("DiD")

p1
ggsave("figure4_Q.pdf", width = 15, height = 15, units = "cm")

