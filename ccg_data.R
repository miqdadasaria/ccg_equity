library(rgdal)
library(leaflet)
library(dplyr)
library(ggplot2)
library(scales)

load_lsoa_data = function(){
  lsoa_data = read.csv("data/ccg_lsoa_data.csv", stringsAsFactors=FALSE)
  lsoa_data$rate = as.double(gsub(",","",lsoa_data$rate))
  lsoa_data$age_stdrate = as.double(gsub(",","",lsoa_data$age_stdrate))
  
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
  similar_data = lsoa_data %>% filter(CCG %in% ccg[paste("similar_ccg_",1:10,sep="")])
  model = lm(age_stdrate~imdscaled, data=similar_data, weights=population)
  
  return(c(coef(model),sqrt(vcov(model)[2,2])))  
}

calculate_ccg_data = function(lsoa_data, ccg_mappings){
  CI_95 = qnorm((1+0.95)/2)
  ccg_data = lsoa_data %>%
    mutate(national_rate=100000*sum(admissions)/sum(population)) %>%
    group_by(CCG) %>%
    mutate(total_pop=sum(population),
           mean=(sum(admissions)/sum(expectedadmissions))*national_rate,
           IMD=sum(population*imdscaled)/sum(population)) %>%
    group_by(CCG,total_pop,mean,IMD) %>%
    do(model=lm(age_stdrate~imdscaled, data=., weights=population)) %>%
    mutate(AGI=coef(model)[2],
           AGI_intercept = coef(model)[1],
           SE=sqrt(vcov(model)[2,2]),
           AGI_LCI=AGI-CI_95*SE,
           AGI_UCI=AGI+CI_95*SE,
           RGI=AGI/(AGI_intercept+0.5*AGI)) %>%
    ungroup() %>%
    select(CCG,total_pop,mean,IMD,AGI,AGI_intercept,AGI_LCI,AGI_UCI,RGI) %>%
    left_join(ccg_mappings, by=c("CCG"="CCG13CDH")) %>%
    group_by(CCG,CCG13CD,CCG13NM,total_pop,mean,IMD,AGI,AGI_intercept,AGI_LCI,AGI_UCI,RGI) %>%
    do(similar=calculate_similar_ccg_AGI(.,lsoa_data)) %>%
    mutate(similar_AGI=similar[2],
           similar_AGI_intercept = similar[1],
           similar_AGI_LCI=similar_AGI-CI_95*similar[3],
           similar_AGI_UCI=similar_AGI+CI_95*similar[3],
           similar_RGI=similar_AGI/(similar_AGI_intercept+0.5*similar_AGI)) %>%
    ungroup() %>%
    select(CCG,CCG13CD,CCG13NM,total_pop,mean,IMD,
           AGI,AGI_intercept,AGI_LCI,AGI_UCI,RGI,
           similar_AGI,similar_AGI_intercept,similar_AGI_LCI,similar_AGI_UCI,RGI)
  
  return(ccg_data)
}

caterpillar_plot = function(ccg_data, national_sii){
  ccg_data = ccg_data %>% arrange(desc(AGI))
  ccg_data[,"AGI_RANK"] = (1:nrow(ccg_data))/nrow(ccg_data)

  caterpillar = ggplot(ccg_data, aes(x=AGI_RANK,y=AGI)) +
    geom_point(size=1) +
    geom_errorbar(aes(ymin=AGI_LCI, ymax=AGI_UCI)) +
    xlab("CCG equity rank") + 
    ylab("AGI") +
    ggtitle("Catepillar Plot") +
    geom_hline(yintercept=national_sii[2], colour="red", linetype=2) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least equitable","","","","","most equitable")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
          )
  
  return(caterpillar)
}

scatter_plot = function(lsoa_data, ccg_code, national_sii){
  
  ccg_data = lsoa_data %>% filter(CCG==ccg_code)
    
  ccg_reg = lm(age_stdrate~imdscaled, data=ccg_data, weights=population)
  ccg_sii = coef(ccg_reg)
  
  scatter = ggplot() +
    geom_point(data=subset(ccg_data,age_stdrate<sum(ccg_sii)/2+2*sd(age_stdrate)), 
               aes(x=imdscaled, y=age_stdrate, size=population), 
               alpha=0.3, colour="black") +
    xlab("small area deprivation rank") + 
    ylab("standardised rate") +
    ggtitle(ccg_code) + 
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
    geom_line(aes(x=c(0,1),y=c(ccg_sii[1],sum(ccg_sii))), colour="blue") +	
    geom_line(aes(x=c(0,1),y=c(national_sii[1],sum(national_sii))),linetype=2, colour="red") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
 
  return(scatter) 
}

ccg_map = function(ccg_data){
  ccg_map = readOGR("data/ccgs.json", "OGRGeoJSON")
  ccg_map@data = left_join(ccg_map@data,ccg_data)
  
  map = leaflet(ccg_map)
  
  return(map)
}

colour_map = function(map, attribute){
  map %>%
    addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.3)
}

# generate some results
lsoa_data = load_lsoa_data()
ccg_mappings = load_ccg_mappings()
national_sii = coef(lm(age_stdrate~imdscaled, data=lsoa_data, weights=population))
ccg_data = calculate_ccg_data(lsoa_data, ccg_mappings)