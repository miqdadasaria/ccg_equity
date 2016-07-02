# Shiny ui for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
library(leaflet)

shinyUI(pageWithSidebar(
			
	# Application title
	headerPanel("CCG Equity Indicators"),
				
	sidebarPanel(
	  withTags({
	    div(class="header", checked=NA,
	        p(b("unplanned hospitalisation for chronic ambulatory care sensitive conditions")),
	        a(href="http://jech.bmj.com/content/early/2016/01/12/jech-2015-206742.full", "More details"),
	        p("")
	    )
	  }),
	    
			uiOutput("ccg_list"),
	  
	    selectInput("trim", "Trim outliers on scatter plots:",
	            list("True" = "TRUE",
	                 "False" = "FALSE"), selected="TRUE")
	),
	
	mainPanel(
	      h3(textOutput("title")),
	      textOutput("ccg"),
	      tabsetPanel(
	        tabPanel("Map", leafletOutput("ccg_map")),
	        tabPanel("Scatter", plotOutput("scatter_plot")),
				  tabPanel("Caterpillar", plotOutput("caterpillar_plot")),
				  tabPanel("Similar CCGs", dataTableOutput("similar_agi_data")),
				  tabPanel("All CCGs", dataTableOutput("ccg_agi_data"))
				)
					
		)
))