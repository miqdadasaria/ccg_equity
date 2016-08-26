# Shiny server for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
source("ccg_data.R")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  selected_ccg = reactiveValues(name="Vale of York")
  
  output$ccg_list = renderUI({
    withProgress(message = 'Loading list off CCGs',{
    ccgs = ccg_data %>% select(CCG16NM) %>% arrange(CCG16NM)
    ccg16cdh = ccg_data %>% select(CCG16CDH)
    
    url = parseQueryString(session$clientData$url_search)
    url_ccg_code = NULL
    url_ccg_name = NULL
    if(!is.null(url)){
      url_ccg_code = url[["ccg16cdh"]]
    }
    
    current_ccg = "Vale of York"
    if(!is.null(selected_ccg$name)){
      current_ccg = selected_ccg$name
    } else if(!is.null(url_ccg_code)){
      if(url_ccg_code %in% ccg16cdh$CCG16CDH){
        url_ccg_name = ccg_data %>% filter(CCG16CDH == url_ccg_code) %>% select(CCG16NM)
        current_ccg = url_ccg_name
      }
    }
    
    selectInput(inputId="ccg_name", 
                label="Select CCG to show details:", 
                choices=as.list(t(ccgs$CCG16NM)), 
                selected=current_ccg)
    })
  })
  
  observe({
    selected_ccg$name = input$ccg_name
  })

  output$title = renderText({
    selected_ccg$name
  })
  
  output$scatter_plot = renderPlot({
    withProgress(message = paste0('Drawing scatter plot for ',selected_ccg$name),{
      ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
      scatter_plot(lsoa_data, ccg_data, ccg_code, national_sii, input$trim)
    })
  })
  
  output$caterpillar_plot = renderPlot({
    withProgress(message = paste0('Drawing caterpillar plot for ',selected_ccg$name),{
      ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
      caterpillar_plot(ccg_data, ccg_code, national_sii[2], national_sii_lci, national_sii_uci)
    })
  })
	
  output$similar_caterpillar_plot = renderPlot({
    withProgress(message = paste0('Drawing similar CCG caterpillar plot for ',selected_ccg$name),{
      ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
      print(similar_caterpillar_plot(ccg_mappings, ccg_data, ccg_code))
    })
  })
  
	output$similar_agi_data = renderDataTable({
	  withProgress(message = paste0('Loading similar CCG data table for ',selected_ccg$name),{
	    ccg_code = ccg_data %>% filter(CCG16NM==selected_ccg$name) %>% select(CCG16CDH) %>% as.character()
  	  table = similar_ccg_table(ccg_data, ccg_mappings, ccg_code)
  	  datatable(table, 
        style = 'bootstrap',
  	    rownames = FALSE,
  	    colnames = gsub("CCG16NM","CCG Name",gsub("_"," ",colnames(table))),
  	    options = list(pageLength = 12, autoWidth = TRUE, dom='ftri'))
	  })
	})
	
	output$ccg_agi_data = renderDataTable({
	  withProgress(message = 'Loading CCG data table',{
	    table = all_ccg_table(ccg_data)
	    datatable(table,
        style = 'bootstrap',
	      rownames = FALSE,
	      colnames = gsub("CCG16NM","CCG Name",colnames(table)),
	      options = list(pageLength = 25, autoWidth = TRUE, dom='ftrpi'))
	  })
	})
	
	output$ccg_map = renderLeaflet({
	  withProgress(message = 'Generating interactive CCG map...',
	    choropleth_map
	  )
	})
	
	observeEvent(input$ccg_map_shape_click$id, {
	  selected_ccg$name = input$ccg_map_shape_click$id
	})

	observeEvent(input$ccg_map_shape_mouseout$id, {
	  highlight_ccg = "Click on the map or select from the list to view another CCG"
	  output$ccg = renderText({ highlight_ccg })
	})
	
	observeEvent(input$ccg_map_shape_mouseover$id, {
	  highlight_ccg = ""
  	if(length(input$ccg_map_shape_mouseover$id)>0){
  	  highlight_ccg = paste0("Click to see details for NHS ",input$ccg_map_shape_mouseover[["id"]]," CCG")
  	}
	  output$ccg = renderText({ highlight_ccg })
	})

	
})
