# CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(rgdal)
library(leaflet)
library(dplyr)
library(ggplot2)
library(scales)
library(htmltools)

calculate_similar_ccg_AGI = function(ccg, lsoa_data){
  similar_data = lsoa_data %>% filter(CCG16CDH %in% ccg[c("CCG16CDH",paste("similar_ccg_",1:10,sep=""))])
  model = lm(age_stdrate~imdscaled, data=similar_data, weights=population)
  
  return(c(coef(model),sqrt(vcov(model)[2,2])))  
}

calculate_ccg_data = function(db, cached){
  if(cached){
    ccg_data = collect(tbl(db,"ccg_data"))
  } else {
    lsoa_data = collect(tbl(db,"lsoa_data"))
    ccg_mappings = collect(tbl(db,"ccg_mappings"))
    CI_95 = qnorm((1+0.95)/2)
    ccg_data = lsoa_data %>%
      mutate(national_rate=100000*sum(admissions)/sum(population)) %>%
      group_by(CCG16CDH) %>%
      mutate(total_pop=sum(population),
             mean=(sum(admissions)/sum(expectedadmissions))*national_rate,
             IMD=sum(population*imdscaled)/sum(population)) %>%
      group_by(CCG16CDH,total_pop,mean,IMD) %>%
      do(model=lm(age_stdrate~imdscaled, data=., weights=population)) %>%
      mutate(AGI=coef(model)[2],
             AGI_intercept = coef(model)[1],
             SE=sqrt(vcov(model)[2,2]),
             AGI_LCI=AGI-CI_95*SE,
             AGI_UCI=AGI+CI_95*SE,
             RGI=AGI/(AGI_intercept+0.5*AGI)) %>%
      ungroup() %>%
      select(CCG16CDH,total_pop,mean,IMD,AGI,AGI_intercept,AGI_LCI,AGI_UCI,RGI) %>%
      left_join(ccg_mappings,by="CCG16CDH") %>%
      mutate(CCG16NM=trimws(gsub("CCG|NHS","",CCG16NM))) %>%
      group_by(CCG16CDH,CCG16CD,CCG16NM,total_pop,mean,IMD,AGI,AGI_intercept,AGI_LCI,AGI_UCI,RGI) %>%
      do(similar=calculate_similar_ccg_AGI(.,lsoa_data)) %>%
      mutate(similar_AGI=similar[2],
             similar_AGI_intercept = similar[1],
             similar_AGI_LCI=similar_AGI-CI_95*similar[3],
             similar_AGI_UCI=similar_AGI+CI_95*similar[3],
             similar_RGI=similar_AGI/(similar_AGI_intercept+0.5*similar_AGI)) %>%
      ungroup() %>%
      select(CCG16CDH,CCG16CD,CCG16NM,total_pop,mean,IMD,
             AGI,AGI_intercept,AGI_LCI,AGI_UCI,RGI,
             similar_AGI,similar_AGI_intercept,similar_AGI_LCI,similar_AGI_UCI,similar_RGI)
  }
  return(ccg_data)
}

get_plot_theme = function(){
  theme = theme_bw() + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  return(theme)
}

caterpillar_plot = function(ccg_data, ccg_code, benchmark_sii, benchmark_sii_lci, benchmark_sii_uci){
  cat_data = ccg_data %>% arrange(desc(AGI))
  cat_data[,"AGI_RANK"] = (1:nrow(cat_data))/nrow(cat_data)
  
  ccg = cat_data %>% filter(CCG16CDH==ccg_code)
  
  caterpillar = ggplot() +
    geom_point(data=cat_data, aes(x=AGI_RANK, y=AGI), size=1, colour="darkgrey") +
    geom_errorbar(data=cat_data, aes(x=AGI_RANK, ymin=AGI_LCI, ymax=AGI_UCI), colour="darkgrey") +
    geom_point(aes(x=0, y=benchmark_sii), size=1, colour="white") +
    geom_point(data=ccg, aes(x=AGI_RANK, y=AGI), size=1, colour="black") +
    geom_errorbar(data=ccg, aes(x=AGI_RANK, ymin=AGI_LCI, ymax=AGI_UCI), width=1/nrow(cat_data), colour="black") +
    xlab("CCG equity rank") + 
    ylab("AGI") +
    ggtitle(ccg$CCG16NM) +
    geom_hline(yintercept=benchmark_sii_lci, colour="red", linetype=3, alpha=0.1) +
    geom_hline(yintercept=benchmark_sii, colour="red", linetype=2) +
    geom_hline(yintercept=benchmark_sii_uci, colour="red", linetype=3, alpha=0.1) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least equitable","","","","","most equitable")) +
    get_plot_theme()
  
  return(caterpillar)
}

similar_caterpillar_plot = function(ccg_mappings, ccg_data, ccg_code){
  similar = ccg_mappings %>% filter(CCG16CDH==ccg_code) %>% select(similar_ccg_1:similar_ccg_10)
  similar_ccg_data = ccg_data %>% filter(CCG16CDH %in% c(ccg_code,similar))
  ccg = ccg_data %>% filter(CCG16CDH %in% c(ccg_code))
  return(caterpillar_plot(similar_ccg_data,ccg_code,ccg$similar_AGI,ccg$similar_AGI_LCI,ccg$similar_AGI_UCI))
}  

scatter_plot = function(lsoa_data, ccg_data, ccg_code, national_sii, trim){
  
  scatter_data = lsoa_data %>% filter(CCG16CDH==ccg_code & population>50)
  ccg = ccg_data %>% filter(CCG16CDH==ccg_code)
  
  # set up the AGI lines
  x = c(0,1)
  national_agi = c(national_sii[1],sum(national_sii))
  similar_agi = c(ccg[["similar_AGI_intercept"]],sum(ccg[c("similar_AGI","similar_AGI_intercept")]))
  ccg_agi =  c(ccg[["AGI_intercept"]],sum(ccg[c("AGI","AGI_intercept")]))  
  

  agi_lines = as.data.frame(rbind(
    cbind(x,national_agi,"National"),
    cbind(x,similar_agi,"Similar CCGs"),
    cbind(x,ccg_agi,"Selected CCG")), 
    row.names = FALSE,
    stringsAsFactors = FALSE)
  names(agi_lines) = c("imd","AGI","level")
  agi_lines$level = factor(agi_lines$level,levels=c("National","Similar CCGs","Selected CCG"))
  agi_lines$imd = as.double(agi_lines$imd)
  agi_lines$AGI = as.double(agi_lines$AGI)
  
  if(trim){
    scatter_data = subset(scatter_data,
                          (age_stdrate<(mean(age_stdrate)+qnorm((1+0.95)/2)*sd(age_stdrate))
                                        & age_stdrate>(mean(age_stdrate)-qnorm((1+0.95)/2)*sd(age_stdrate))))   
  }
  
  scatter = ggplot() +
    geom_point(data=scatter_data, 
               aes(x=imdscaled, y=age_stdrate, size=population), 
               alpha=0.3, colour="black") +
    xlab("small area deprivation rank") + 
    ylab("standardised rate") +
    ggtitle(ccg$CCG16NM) + 
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
    scale_color_manual(name="AGI Trend", values=c("firebrick4","dodgerblue4","forestgreen"), labels=c("National","Similar CCGs","Selected CCG")) +
    scale_linetype_manual(name="AGI Trend", values=c(2,3,1)) + 
    scale_size_continuous(name="Population") +
    geom_line(data=agi_lines, size=1.5, aes(x=imd, y=AGI, group=level, colour=level, linetype=level)) +	
    get_plot_theme()
 
  return(scatter) 
}

make_ccg_map = function(ccg_data){
  ccg_map = readOGR("data/ccg_map_2016.geojson", "OGRGeoJSON", verbose=FALSE, stringsAsFactors=FALSE)
  ccg_map@data$CCG16NM = trimws(gsub("CCG|NHS","",ccg_map@data$CCG16NM))
  ccg_map@data = left_join(ccg_map@data,ccg_data,by=c("CCG16CD","CCG16NM"))
  return(ccg_map)
}

make_popup_messages = function(ccg_map){
  ccg_map$AGI = round(ccg_map$AGI)
  popup_messages = paste0("<b>Name: </b>",ccg_map$CCG16NM,"<br>",
                         "<b>IMD: </b>",round(ccg_map$IMD,3),"<br>",
                         "<b>Population: </b>",round(ccg_map$total_pop),"<br>",
                         "<b>Standardised Rate: </b>", round(ccg_map$mean),"<br>",
                         "<b>AGI: </b>",round(ccg_map$AGI),
                         " (95% CI: ",round(ccg_map$AGI_LCI)," to ",round(ccg_map$AGI_UCI),")<br>",
                         "<b>RGI: </b>",round(ccg_map$RGI,2))
  return(data_frame(CCG16NM=ccg_map$CCG16NM,message=popup_messages))  
}

make_choropleth_map = function(ccg_data, cached){
  if(cached){
    load("data/ccg_map.RData")
  } else {
    ccg_map = ccg_data %>% make_ccg_map()
  }    

  ccg_map$AGI = round(ccg_map$AGI)
  popup_message = make_popup_messages(ccg_map)$message
  
  agi_pal = colorBin("Blues", ccg_map$AGI, 5, pretty = FALSE)
  rgi_pal = colorBin("Reds", ccg_map$RGI, 5, pretty = FALSE)
  imd_pal = colorQuantile("Greens", ccg_map$IMD, n=5)
  pop_pal = colorBin("Oranges", ccg_map$total_pop, 5, pretty = FALSE)
  rate_pal = colorBin("Purples", ccg_map$mean, 5, pretty = FALSE)
  
  choropleth_map = leaflet(ccg_map) %>% 
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = agi_pal(ccg_map$AGI), 
                color="black",
                layerId=ccg_map$CCG16NM,
                group="AGI") %>%
    # addLegend("topleft", 
    #           pal = agi_pal, 
    #           values = ccg_map$AGI, 
    #           title = "AGI", 
    #           opacity = 0.7) %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = rgi_pal(ccg_map$RGI), 
                color="black",
                group="RGI") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = imd_pal(ccg_map$IMD), 
                color="black",
                group="IMD") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = pop_pal(ccg_map$total_pop), 
                color="black",
                group="Population") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = rate_pal(ccg_map$mean), 
                color="black",
                group="Standardised Rate") %>%
    addLayersControl(
      baseGroups=c("AGI", "RGI", "IMD", "Population", "Standardised Rate"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    )

  return(choropleth_map)
}

all_ccg_table = function(ccg_data){
  ccg_table = ccg_data %>% 
    mutate(CCG16NM,Population=round(total_pop),IMD=round(IMD,2),Average=round(mean),AGI=round(AGI),`Similar CCG AGI`=round(similar_AGI),RGI=round(RGI,2)) %>%
    select(CCG16NM,Population,IMD,Average,AGI,`Similar CCG AGI`,RGI) %>% 
    arrange(CCG16NM)
    
  return(ccg_table)
}

similar_ccg_table = function(ccg_data, ccg_mappings, ccg_code){
  similar = ccg_mappings %>% filter(CCG16CDH==ccg_code) %>% select(similar_ccg_1:similar_ccg_10)
  similar_table = ccg_data %>% filter(CCG16CDH %in% c(ccg_code,similar)) %>%
    mutate(Population=round(total_pop),
           IMD=round(IMD,2),
           Average=round(mean),
           AGI=round(AGI),
           AGI_LCI=round(AGI_LCI),
           AGI_UCI=round(AGI_UCI),
           RGI=round(RGI,2)) %>%
    select(CCG16NM,Population,IMD,Average,AGI,AGI_LCI,AGI_UCI,RGI) %>%
      arrange(CCG16NM)
  ccg = ccg_data %>% filter(CCG16CDH %in% c(ccg_code))
  aggregate = similar_table %>% summarise(CCG16NM="Aggregate across group of similar CCGs",
                              IMD=round(weighted.mean(IMD, Population),2),
                              Average=round(weighted.mean(Average, Population)),
                              AGI=round(ccg$similar_AGI),
                              AGI_LCI=round(ccg$similar_AGI_LCI),
                              AGI_UCI=round(ccg$similar_AGI_UCI),
                              RGI=round(ccg$similar_RGI,2),
                              Population=sum(Population))
  results_table = bind_rows(aggregate,similar_table) %>% 
    select(CCG16NM,Population,IMD,Average,AGI,AGI_LCI,AGI_UCI,RGI)                            
    
  return(results_table)
}

# generate some results
db = src_sqlite("data/ccg_lsoa_data.sqlite3")
lsoa_data = collect(tbl(db,"lsoa_data"))
ccg_mappings = collect(tbl(db,"ccg_mappings"))
ccg_data = calculate_ccg_data(db, cached=TRUE)

national_lm = lm(age_stdrate~imdscaled, data=lsoa_data, weights=population)
national_sii = coef(national_lm)
national_sii_se = sqrt(vcov(national_lm)[2,2]) 
national_sii_uci = national_sii["imdscaled"] + qnorm((1+0.95)/2)*national_sii_se
national_sii_lci = national_sii["imdscaled"] - qnorm((1+0.95)/2)*national_sii_se

choropleth_map = make_choropleth_map(ccg_data, cached=TRUE)
