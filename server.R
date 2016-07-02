# Shiny server for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
source("ccg_data.R")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  selected_ccg = reactiveValues(name="Vale of York")
  
  output$ccg_list = renderUI({
    ccgs = ccg_data %>% select(CCG16NM) %>% arrange(CCG16NM)
    default_ccg = ifelse(is.null(selected_ccg$name),"Vale of York",selected_ccg$name)
    selectInput(inputId="ccg_name", 
                label="Select CCG to show details:", 
                choices=as.list(t(ccgs$CCG16NM)), 
                selected=default_ccg)
  })
  
  observe({
    selected_ccg$name = input$ccg_name
  })

  output$title = renderText({
    print(selected_ccg$name)
  })
  
  output$scatter_plot = renderPlot({
    ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
    print(scatter_plot(lsoa_data, ccg_data, ccg_code, national_sii, input$trim))
  })
  
  output$caterpillar_plot = renderPlot({
    ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
    print(caterpillar_plot(ccg_data, ccg_code, national_sii))
  })
			
	output$similar_agi_data = renderDataTable({
	  ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
	  print(similar_ccg_table(ccg_data, ccg_mappings, ccg_code))
	})
	
	output$ccg_agi_data = renderDataTable({
	  print(all_ccg_table(ccg_data))
	})
	
	output$ccg_map = renderLeaflet({
	  print(choropleth_map)
	})
	
	observeEvent(input$ccg_map_shape_click$id, {
	  selected_ccg$name = input$ccg_map_shape_click$id
	})

	output$ccg = renderText({
	  highlight_ccg = ""
	    if(length(input$ccg_map_shape_mouseover$id)>0){
	      highlight_ccg = paste0("Click to see details for NHS ",input$ccg_map_shape_mouseover[["id"]]," CCG")
	    }
	  print(highlight_ccg)
	})

	# observeEvent(input$ccg_map_shape_mouseover$id, {
	#   pointId = input$ccg_map_shape_mouseover$id
	#   message = popup_messages %>% filter(CCG16NM==pointId) %>% select(message) %>% as.character()
	#   leafletProxy("ccg_map") %>% addPopups(lat = input$ccg_map_shape_mouseover$lat, lng = input$ccg_map_shape_mouseover$lng, message, layerId="popups_layer")
	# })
	
})
