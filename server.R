# Shiny server for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
source("ccg_data.R")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
	
  output$title = renderText({
    title = ccg_data %>% filter(CCG16CDH==input$ccg_code) %>% select(CCG16NM)
    print(title[[1]])
  })
  
  output$scatter_plot = renderPlot({
    print(scatter_plot(lsoa_data, ccg_data, input$ccg_code, national_sii, input$trim))
  })
  
  output$caterpillar_plot = renderPlot({
    print(caterpillar_plot(ccg_data, input$ccg_code, national_sii))
  })
			
	output$similar_agi_data = renderDataTable({
	  print(similar_ccg_table(ccg_data, ccg_mappings, input$ccg_code))
	})
	
	output$ccg_agi_data = renderDataTable({
	  print(all_ccg_table(ccg_data))
	})
	
	output$ccg_map = renderLeaflet({
	  print(choropleth_map)
	})
	
	output$ccg = renderText({
	  if(is.null(input$ccg_map_shape_mouseover)){
	    highlight_ccg = ""
	  }else{
	    highlight_ccg = names(input$ccg_map_shape_mouseover)
	   }
	  
	  print(highlight_ccg)
	})
	
})
