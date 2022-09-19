

#RAM timeseries
series1 =timeseries_values_views %>% left_join(metadata, by = c("stockid", "stocklong"))

with(series1, {

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
    select(year, stocklong, bbmsy, ffmsy, region, FisheryType, taxGroup)

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
  
  #make a transition to overfishing and overfished variable
  stocks = sort(unique(series$stocklong))
  
  for(i in 1:length(stocks)){
    
    sdat = series%>%
      filter(stocklong == stocks[i])
    
    
    
    years = sort(unique(sdat$year[c(1:(length(sdat$year)))]))
    years = years[years!=min(sdat$year)]
    
    out = purrr::map_df(years, function(t){
      
      gdat1 = sdat%>%
        filter(year == t-1)
      
      gdat = sdat%>%
        filter(year == t)
      
      if(length(gdat1$year)>0){
        tr_overfished = NA
        tr_overfished = ifelse((gdat$bbmsy_overfished == 1 & gdat1$bbmsy_overfished == 0), 1,0)
        
        tr_overfish = NA
        tr_overfish = ifelse((gdat$overfishing == 1 & gdat1$overfishing == 0), 1,0)
        
        df = gdat %>%
          mutate(tr_overfished= tr_overfished, tr_overfish = tr_overfish)
      }
      # if(!(length(num)>0)){df = data_frame(stocklong = stocks[i], brk = "break")
      #return(df)}
      
      
      
    })
    
    if(i==1){s1 = out}else{s1= rbind(s1, out)}
    
  }
  
series = series%>%
  left_join(s1)


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

  assign("series", series, envir = .GlobalEnv)


})
