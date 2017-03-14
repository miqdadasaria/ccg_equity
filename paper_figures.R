# CCG equity indicators - figures for paper
# 
# Author: Miqdad Asaria
# Date: 04/07/2016
###############################################################################

source("ccg_data.R")
library(tidyr)
library(grid)

figure_1 = function(lsoa_data, ccg_data, ccg_code, national_sii, trim){
  
  scatter_data = lsoa_data %>% filter(CCG16CDH==ccg_code & population>50)
  ccg = ccg_data %>% filter(CCG16CDH==ccg_code)
  
  # set up the AGI lines
  x = c(0,1)
  national_agi = c(national_sii[1],sum(national_sii))
  similar_agi = c(ccg[["similar_AGI_intercept"]],sum(ccg[c("similar_AGI","similar_AGI_intercept")]))
  ccg_agi =  c(ccg[["AGI_intercept"]],sum(ccg[c("AGI","AGI_intercept")]))  
  
  
  agi_lines = as.data.frame(rbind(
    cbind(x,national_agi,"National"),
    cbind(x,similar_agi,"Similar populations"),
    cbind(x,ccg_agi,"Selected population")), 
    row.names = FALSE,
    stringsAsFactors = FALSE)
  names(agi_lines) = c("imd","AGI","level")
  agi_lines$level = factor(agi_lines$level,levels=c("National","Similar populations","Selected population"))
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
    xlab("Neighbourhood deprivation rank**") + 
    ylab("Standardized emergency admission rate*") +
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
    scale_color_manual(name="Gradient", values=c("firebrick4","dodgerblue4","forestgreen")) +
    scale_linetype_manual(name="Gradient", values=c(2,3,1)) + 
    scale_size_continuous(name="Population") +
    geom_line(data=agi_lines, size=1.5, aes(x=imd, y=AGI, group=level, colour=level, linetype=level)) +	
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
  
  return(scatter) 
}

figure_2 = function(ccg_data, ccg_code, benchmark_sii){
  cat_data = ccg_data %>% arrange(desc(AGI))
  cat_data[,"AGI_RANK"] = (1:nrow(cat_data))/nrow(cat_data)
  
  ccg = cat_data %>% filter(CCG16CDH==ccg_code)
  
  caterpillar = ggplot() +
    geom_point(data=cat_data, aes(x=AGI_RANK, y=AGI), size=1, colour="darkgrey") +
    geom_errorbar(data=cat_data, aes(x=AGI_RANK, ymin=AGI_LCI, ymax=AGI_UCI), colour="darkgrey") +
    geom_point(aes(x=0, y=benchmark_sii), size=1, colour="white") +
    geom_point(data=ccg, aes(x=AGI_RANK, y=AGI), size=1, colour="black") +
    geom_errorbar(data=ccg, aes(x=AGI_RANK, ymin=AGI_LCI, ymax=AGI_UCI), width=1/nrow(cat_data), colour="black") +
    xlab("Neighbourhood deprivation rank**") + 
    ylab("Absolute gradient index (AGI) of emergency admission rate*") +
    geom_hline(yintercept=benchmark_sii, colour="firebrick4", linetype=2) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least equitable","","","","","most equitable")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_text(size=16),
          axis.title=element_text(size=22), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
  
  return(caterpillar)
}

trim = function(scatter_data,CI=0.95){
    return(subset(scatter_data,
                          (age_stdrate<(mean(age_stdrate)+qnorm((1+CI)/2)*sd(age_stdrate))
                           & age_stdrate>(mean(age_stdrate)-qnorm((1+CI)/2)*sd(age_stdrate)))))   
}

figure_3 = function(lsoa_data, ccg_data, national_sii){
  
  scatter_data_HD_GP = lsoa_data %>% filter(CCG16CDH=="07P" & population>50) %>% mutate(performance="Better Equity", deprivation="High Deprivation") %>% trim()
  scatter_data_HD_BP = lsoa_data %>% filter(CCG16CDH=="99A" & population>50) %>% mutate(performance="Worse Equity", deprivation="High Deprivation") %>% trim()
  scatter_data_AD_GP = lsoa_data %>% filter(CCG16CDH=="09C" & population>50) %>% mutate(performance="Better Equity", deprivation="Average Deprivation") %>% trim()
  scatter_data_AD_BP = lsoa_data %>% filter(CCG16CDH=="03K" & population>50) %>% mutate(performance="Worse Equity", deprivation="Average Deprivation") %>% trim()
  scatter_data_LD_GP = lsoa_data %>% filter(CCG16CDH=="09X" & population>50) %>% mutate(performance="Better Equity", deprivation="Low Deprivation") %>% trim()
  scatter_data_LD_BP = lsoa_data %>% filter(CCG16CDH=="11C" & population>50) %>% mutate(performance="Worse Equity", deprivation="Low Deprivation") %>% trim()
  
  scatter_data = bind_rows(scatter_data_HD_GP,
                           scatter_data_HD_BP,
                           scatter_data_AD_GP,
                           scatter_data_AD_BP,
                           scatter_data_LD_GP,
                           scatter_data_LD_BP) %>% filter(age_stdrate<2500)
  
  scatter_data$performance = factor(scatter_data$performance, levels=c("Worse Equity", "Better Equity"))
  scatter_data$deprivation = factor(scatter_data$deprivation, levels=c("Low Deprivation", "Average Deprivation", "High Deprivation"))
  
  groups = scatter_data %>% select(CCG16CDH,performance,deprivation) %>% distinct()
  
  ccgs = groups %>% 
    left_join(ccg_data, by="CCG16CDH") %>% 
    mutate(selected_0=AGI_intercept, 
           selected_1=AGI_intercept+AGI, 
           similar_0=similar_AGI_intercept, 
           similar_1=similar_AGI_intercept+similar_AGI, 
           national_0=national_sii[1], 
           national_1=national_sii[1]+national_sii[2]) %>%
    select(performance, deprivation, selected_0, selected_1, similar_0, similar_1, national_0, national_1) %>%
    gather(key,value,-performance,-deprivation) %>%
    separate(key, into=c("trend", "x"), sep="_") %>%
    mutate(x=as.numeric(x))
  ccgs$trend = factor(ccgs$trend,levels=c("national","similar","selected"),labels=c("National","Similar populations","Selected population"))

  scatter = ggplot() +
    geom_point(data=scatter_data, 
               aes(x=imdscaled, y=age_stdrate, size=population), 
               alpha=0.3, colour="black") +
    xlab("Neighbourhood deprivation rank**") + 
    ylab("Standardized emergency admission rate*") +
    scale_x_continuous(breaks=seq(0,1,0.2), labels=c("least deprived","","","","","most deprived")) +
    scale_color_manual(name="Gradient", values=c("firebrick4","dodgerblue4","forestgreen")) +
    scale_linetype_manual(name="Gradient", values=c(2,3,1)) + 
    scale_size_continuous(name="Population") +
    geom_line(data=ccgs, size=1.5, aes(x=x, y=value, group=trend, colour=trend, linetype=trend)) +	
    facet_grid(deprivation~performance) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)),
          strip.text = element_text(size=22),
          axis.text = element_text(size=16),
          axis.title=element_text(size=22),
          legend.text=element_text(size=18),
          legend.title=element_text(size=20, face="bold"),
          panel.spacing = unit(4.5, "lines"),
          plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
  
  return(scatter) 
}

summary_stats_for_journal_article = function(national_lm, ccg_data){
  national_sii_se = sqrt(vcov(national_lm)[2,2]) 
  national_sii_uci = national_sii["imdscaled"] + qnorm((1+0.95)/2)*national_sii_se
  national_sii_lci = national_sii["imdscaled"] - qnorm((1+0.95)/2)*national_sii_se
  
  worse_than_average_ccgs = ccg_data %>% filter(AGI_LCI > national_sii_uci) %>% select(CCG16NM,AGI,AGI_LCI,AGI_UCI,IMD,CCG16CDH)
  better_than_average_ccgs = ccg_data %>% filter(AGI_UCI < national_sii_lci) %>% select(CCG16NM,AGI,AGI_LCI,AGI_UCI,IMD,CCG16CDH)
  
  worse_than_similar_ccgs = ccg_data %>% filter(AGI_LCI > similar_AGI_UCI) %>% select(CCG16NM,AGI,AGI_LCI,AGI_UCI,IMD,CCG16CDH)
  better_than_similar_ccgs = ccg_data %>% filter(AGI_UCI < similar_AGI_LCI) %>% select(CCG16NM,AGI,AGI_LCI,AGI_UCI,IMD,CCG16CDH)
  
  better_than_both_ccgs = intersect(better_than_average_ccgs, better_than_similar_ccgs)
  worse_than_both_ccgs = intersect(worse_than_average_ccgs, worse_than_similar_ccgs)
  
  
  sink(file="output/better_than_average_ccgs.txt")
  print(better_than_average_ccgs)
  sink()
  
  sink(file="output/worse_than_average_ccgs.txt")
  print(worse_than_average_ccgs)
  sink()
  
  sink(file="output/better_than_similar_ccgs.txt")
  print(better_than_similar_ccgs)
  sink()
  
  sink(file="output/worse_than_similar_ccgs.txt")
  print(worse_than_similar_ccgs)
  sink()
  
  sink(file="output/better_than_both_ccgs.txt")
  print(better_than_both_ccgs)
  sink()
  
  sink(file="output/worse_than_both_ccgs.txt")
  print(worse_than_both_ccgs)
  sink()
  
  correlation_bw_imd_similar_agi = cor(ccg_data$IMD,ccg_data$similar_AGI,method="pearson")
  
  results_summary = paste0("National average SII: ", round(national_sii["imdscaled"]), 
         " (95% CI: ", round(national_sii_lci), " to ", round(national_sii_uci),"), ",
         nrow(better_than_average_ccgs), " of ", nrow(ccg_data), " CCGs better than average (", 
         round(100*nrow(better_than_average_ccgs)/nrow(ccg_data)), "%), ",
         nrow(worse_than_average_ccgs), " of ", nrow(ccg_data), " CCGs worse than average (", 
         round(100*nrow(worse_than_average_ccgs)/nrow(ccg_data)), "%), ",
         nrow(better_than_similar_ccgs), " of ", nrow(ccg_data), " CCGs better than similar benchmark (", 
         round(100*nrow(better_than_similar_ccgs)/nrow(ccg_data)), "%), ",
         nrow(worse_than_similar_ccgs), " of ", nrow(ccg_data), " CCGs worse than similar benckmark (", 
         round(100*nrow(worse_than_similar_ccgs)/nrow(ccg_data)), "%), ",
         nrow(better_than_both_ccgs), " of ", nrow(ccg_data), " CCGs better than national and similar benchmark (", 
         round(100*nrow(better_than_both_ccgs)/nrow(ccg_data)), "%), ",
         nrow(worse_than_both_ccgs), " of ", nrow(ccg_data), " CCGs worse than national and similar benckmark (", 
         round(100*nrow(worse_than_both_ccgs)/nrow(ccg_data)), "%), ",
         "correlation (pearson) between deprivation and similar areas benchmark: ", round(correlation_bw_imd_similar_agi,2)
  )
  
  return(results_summary)
}

fig_1 = figure_1(lsoa_data, ccg_data, ccg_code="09C", national_sii, trim=TRUE)
ggsave(filename="output/figure_1.png",fig_1,width=10,height=9)

fig_2 = figure_2(ccg_data, ccg_code="09C", benchmark_sii=national_sii[2])
ggsave(filename="output/figure_2.png",fig_2,width=15,height=9)

fig_3 = figure_3(lsoa_data, ccg_data, national_sii)
ggsave(filename="output/figure_3.png",fig_3,width=18,height=20)

fig_4 = similar_caterpillar_plot(ccg_mappings, ccg_data, ccg_code="99A") + 
  coord_flip() + 
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())
ggsave(filename="output/figure_4.png",fig_4,width=9,height=12)

table_1 = similar_ccg_table(ccg_data, ccg_mappings, ccg_code="99A")
write.csv(table_1, file="output/table_1.csv", row.names=FALSE)

results_summary = summary_stats_for_journal_article(national_lm, ccg_data)

sink(file="output/summary_stats.txt")
print(results_summary)
sink()
