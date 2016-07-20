# Shiny ui for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
library(leaflet)

shinyUI(fluidPage(
			
  tags$head(includeScript("google-analytics.js")),
  
	titlePanel("CCG Equity Indicators"),
				
	sidebarPanel(
	  withTags({
	    div(class="header", checked=NA,
	        p(b("unplanned hospitalisation for chronic ambulatory care sensitive conditions")),
	        a(href="http://jech.bmj.com/content/early/2016/01/12/jech-2015-206742.full", "More details"),
	        p("")
	    )
	  }),
	    
			uiOutput("ccg_list"),
	  
	    selectInput("trim", "Trim outliers beyond 95% CI of mean on scatter plots:",
	            list("True" = "TRUE",
	                 "False" = "FALSE"), selected="TRUE"),
	  
	  tags$div(
	    HTML("<a href='http://www.york.ac.uk/about/legal-statements/'>University of York Disclaimer</a>")
	  )
	  
	),
	
	mainPanel(
	      h3(textOutput("title")),
	      textOutput("ccg"),
	      tabsetPanel(id="tabset",
	        tabPanel("Map", leafletOutput("ccg_map")),
	        tabPanel("Scatter", plotOutput("scatter_plot")),
				  tabPanel("Caterpillar", plotOutput("caterpillar_plot")),
				  tabPanel("Caterpillar Similar CCGs", plotOutput("similar_caterpillar_plot")),
				  tabPanel("Similar CCGs", dataTableOutput("similar_agi_data")),
				  tabPanel("All CCGs", dataTableOutput("ccg_agi_data"))
				),
				tags$div(
				  HTML("<p>&nbsp;<p>This site was produced by <a href='https://www.york.ac.uk/che/staff/research/miqdad-asaria/'>Miqdad Asaria</a> 
                as part of the <a href='https://www.york.ac.uk/che/research/equity/monitoring/'>Health Equity Indicators</a> project 
                at the <a href='https://www.york.ac.uk/che'>Centre of Health Economics</a> at the 
				        <a href='https://www.york.ac.uk/'>University of York</a>. 
				        Source code can be found <a href='https://github.com/miqdadasaria/ccg_equity'>here</a>.
				        <p>Contains National Statistics data © Crown copyright and database right 2016
				        <p>Contains OS data © Crown copyright and database right 2016
				        <p>Data licensed under the <a href='http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/'>Open Government Licence v3.0</a>")
				)
					
		)
))