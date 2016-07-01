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

load_lsoa_data = function(){
  lsoa_data = read.csv("data/ccg_lsoa_data.csv", stringsAsFactors=FALSE)
  lsoa_data$rate = as.double(gsub(",","",lsoa_data$rate))
  lsoa_data$age_stdrate = as.double(gsub(",","",lsoa_data$age_stdrate))
  lsoa_data = lsoa_data %>% select(CCG16CDH=CCG,population,admissions,expectedadmissions,imdscaled,age_stdrate)
  return(lsoa_data)
}

load_ccg_mappings = function(){
  # NHS Newcastle Gateshead CCG (13T) was formed as of the 1st of April 2015 
  # by the merger of NHS Gateshead CCG (00F), NHS Newcastle North and East CCG (00G)
  # and NHS Newcastle West CCG (00H).
  ccg_mappings = read.csv("data/ccg_mappings.csv",stringsAsFactors = FALSE)

  return(ccg_mappings)
}

calculate_similar_ccg_AGI = function(ccg, lsoa_data){
  similar_data = lsoa_data %>% filter(CCG16CDH %in% ccg[paste("similar_ccg_",1:10,sep="")])
  model = lm(age_stdrate~imdscaled, data=similar_data, weights=population)
  
  return(c(coef(model),sqrt(vcov(model)[2,2])))  
}

calculate_ccg_data = function(lsoa_data, ccg_mappings){
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
           similar_AGI,similar_AGI_intercept,similar_AGI_LCI,similar_AGI_UCI,RGI)
  
  return(ccg_data)
}

caterpillar_plot = function(ccg_data, ccg_code, national_sii){
  cat_data = ccg_data %>% arrange(desc(AGI))
  cat_data[,"AGI_RANK"] = (1:nrow(cat_data))/nrow(cat_data)
  
  ccg = cat_data %>% filter(CCG16CDH==ccg_code)
  
  caterpillar = ggplot() +
    geom_point(data=cat_data, aes(x=AGI_RANK, y=AGI), size=1, colour="darkgrey") +
    geom_errorbar(data=cat_data, aes(x=AGI_RANK, ymin=AGI_LCI, ymax=AGI_UCI), colour="darkgrey") +
    geom_point(data=cat_data,aes(x=AGI_RANK, y=similar_AGI), size=1, colour="red") +
    geom_point(data=ccg, aes(x=AGI_RANK, y=AGI), size=1, colour="black") +
    geom_errorbar(data=ccg, aes(x=AGI_RANK, ymin=AGI_LCI, ymax=AGI_UCI), width=1/nrow(cat_data), colour="black") +
    xlab("CCG equity rank") + 
    ylab("AGI") +
    ggtitle(ccg$CCG16NM) +
    geom_hline(yintercept=national_sii[2], colour="red", linetype=2) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least equitable","","","","","most equitable")) +
    #scale_color_manual(values=c("red","darkgrey","black"),labels=c("Similar CCGs AGI","CCG AGI","Selected CCG AGI")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
          )
  
  return(caterpillar)
}

scatter_plot = function(lsoa_data, ccg_data, ccg_code, national_sii, trim){
  
  scatter_data = lsoa_data %>% filter(CCG16CDH==ccg_code)
  ccg = ccg_data %>% filter(CCG16CDH==ccg_code)
  
  # set up the AGI lines
  x = c(0,1)
  national_agi = c(national_sii[1],sum(national_sii))
  similar_agi = c(ccg[["similar_AGI_intercept"]],sum(ccg[c("similar_AGI","similar_AGI_intercept")]))
  ccg_agi =  c(ccg[["AGI_intercept"]],sum(ccg[c("AGI","AGI_intercept")]))  
  

  agi_lines = as.data.frame(rbind(
    cbind(x,national_agi,"national"),
    cbind(x,similar_agi,"similar"),
    cbind(x,ccg_agi,"ccg")), 
    row.names = FALSE,
    stringsAsFactors = FALSE)
  names(agi_lines) = c("imd","AGI","level")
  agi_lines$level = as.factor(agi_lines$level)
  agi_lines$imd = as.double(agi_lines$imd)
  agi_lines$AGI = as.double(agi_lines$AGI)
  
  if(trim){
    scatter_data = subset(scatter_data,
                          (age_stdrate<(mean(age_stdrate)+qnorm((1+0.90)/2)*sd(age_stdrate))
                                        & age_stdrate>(mean(age_stdrate)-qnorm((1+0.90)/2)*sd(age_stdrate))))   
  }
  
  scatter = ggplot() +
    geom_point(data=scatter_data, 
               aes(x=imdscaled, y=age_stdrate, size=population), 
               alpha=0.3, colour="black") +
    xlab("small area deprivation rank") + 
    ylab("standardised rate") +
    ggtitle(ccg$CCG16NM) + 
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
    geom_line(data=agi_lines, aes(x=imd, y=AGI, group=level, colour=level, linetype=level)) +	
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
 
  return(scatter) 
}

ccg_map = function(ccg_data){
  ccg_map = readOGR("data/ccg_map_2016.geojson", "OGRGeoJSON", verbose=FALSE, stringsAsFactors=FALSE)
  ccg_map@data$CCG16NM = trimws(gsub("CCG|NHS","",ccg_map@data$CCG16NM))
  ccg_map@data = left_join(ccg_map@data,ccg_data,by=c("CCG16CD","CCG16NM"))
  
  return(ccg_map)
}

colour_map = function(ccg_map, attribute){
  ccg_map$AGI = round(ccg_map$AGI)
  popup_message = paste0("<b>Name: </b>",ccg_map$CCG16NM,"<br>",
                         "<b>IMD: </b>",round(ccg_map$IMD,3),"<br>",
                         "<b>Population: </b>",round(ccg_map$total_pop),"<br>",
                         "<b>Standardised Rate: </b>", round(ccg_map$mean),"<br>",
                         "<b>AGI: </b>",round(ccg_map$AGI),
                         " (95% CI: ",round(ccg_map$AGI_LCI)," to ",round(ccg_map$AGI_UCI),")<br>",
                         "<b>RGI: </b>",round(ccg_map$RGI,2))
  
  ccg_pal = colorBin("Blues", ccg_map$AGI, 5, pretty = FALSE)#colorQuantile("Blues", ccg_map$AGI, n=5)
  
  leaflet(ccg_map) %>% 
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0.7, weight = 1, 
    popup = popup_message, fillColor = ccg_pal(ccg_map$AGI), color="black") %>%
    addLegend("topleft", pal = ccg_pal, values = ccg_map$AGI, title = "AGI", opacity = 0.7)
}

all_ccg_table = function(ccg_data){
  ccg_table = ccg_data %>% 
    mutate(CCG16NM,Population=round(total_pop),IMD=round(IMD,2),Average=round(mean),AGI=round(AGI),`Cluster AGI`=round(similar_AGI),RGI=round(RGI,2)) %>%
    select(CCG16NM,Population,IMD,Average,AGI,`Cluster AGI`,RGI) %>% 
    arrange(CCG16NM)
    
  return(ccg_table)
}

similar_ccg_table = function(ccg_data, ccg_mappings, ccg_code){
  similar = ccg_mappings %>% filter(CCG16CDH==ccg_code) %>% select(similar_ccg_1:similar_ccg_10)
  similar_table = ccg_data %>% filter(CCG16CDH %in% c(ccg_code,similar)) %>%
    mutate(Population=round(total_pop),IMD=round(IMD,2),Average=round(mean),AGI=round(AGI),AGI_LCI=round(AGI_LCI),AGI_UCI=round(AGI_UCI),RGI=round(RGI,2)) %>%
    select(CCG16NM,Population,IMD,Average,AGI,AGI_LCI,AGI_UCI,RGI) %>%
      arrange(CCG16NM)
  
  return(similar_table)
}

# generate some results
lsoa_data = load_lsoa_data()
ccg_mappings = load_ccg_mappings()
national_sii = coef(lm(age_stdrate~imdscaled, data=lsoa_data, weights=population))
ccg_data = calculate_ccg_data(lsoa_data, ccg_mappings)

map = ccg_map(ccg_data)
choropleth_map = colour_map(map,"AGI")