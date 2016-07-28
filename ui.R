# Shiny ui for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
library(leaflet)

shinyUI(fluidPage(theme = "sandstone.css",
			
  tags$head(tags$meta(name="description", content="Compare NHS Clinical Commisioning Group performance on health inequality based on data from the 2015/16 NHS CCG IAF."),
            includeScript("google-analytics.js")),
  
	titlePanel("CCG Inequality Indicators"),
				
	sidebarPanel(
	  withTags({
	    div(class="header", checked=NA,
	        p(b("Unplanned hospitalisation for chronic ambulatory care sensitive conditions 2015/16")),
	        a(href="http://www.nhs.uk/Scorecard/Pages/IndicatorFacts.aspx?MetricId=463", "More details"),
	        p("")
	    )
	  }),
	    
			uiOutput("ccg_list"),
	  
	    selectInput("trim", "Trim outliers beyond 95% CI of mean on scatter plots:",
	            list("True" = "TRUE",
	                 "False" = "FALSE"), selected="TRUE"),
	  
	  tags$div(
	    HTML("<small><small><img src='UOY-Logo.svg' alt='University of York' width=90%/>
           <p>This site was produced by <a href='https://www.york.ac.uk/che/staff/research/miqdad-asaria/'>Miqdad Asaria</a> 
           as part of the <a href='https://www.york.ac.uk/che/research/equity/monitoring/'>Health Equity Indicators</a> project 
	         at the <a href='https://www.york.ac.uk/che'>Centre of Health Economics</a> at the 
	         <a href='https://www.york.ac.uk/'>University of York</a>. 
	         <p>Source code can be found <a href='https://github.com/miqdadasaria/ccg_equity'>here</a>.
	         <p><a href='http://www.york.ac.uk/about/legal-statements/'>University of York Disclaimer</a></small></small>")
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
				  HTML("<small><small><small><p>&nbsp;<p>
                <p>CCG is the <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>clinical commisioning group</a>
                these are the organisations that deliver health services in the NHS;
                AGI is the <a href='http://www.yhpho.org.uk/resource/item.aspx?RID=94925'>absolute gradient index</a> measuring the modelled absolute gap in
                standardised rates between the most and least deprived areas at the local level; 
                RGI is the <a href='https://en.wikipedia.org/wiki/Relative_index_of_inequality'>relative gradient index</a> measuring the modelled proportionate gap in 
                standardised rates between the most and least deprived areas at the local level; 
                IMD is the <a href='https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015'>index of multiple deprivation</a> 2015 normalised rank 
                and aggegated to CCG level with a higher value indicating greater deprivation;
                Population is the <a href='https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates'>ONS mid-year population</a> at LSOA level attributed to CCGs; 
                Standardised rate is the rate of <a href='https://data.england.nhs.uk/dataset/nhsof-2-6-unplanned-hospitalisation-for-chronic-ambulatory-care-sensitive-conditions'>unplanned hospitalisation for chronic ambulatory care sensitive conditions</a> over the year
                per 100,000 of population indirectly standardised for age and sex;
                <p>The scatter plot shows LSOAs within CCGs with the size of the point representing the population that the LSOA contributes to the CCG. 
	              The caterpillar plot show CCG AGI values and their 95% confidence intervals with the average across all CCGs plotted represented by the red dashed line
   			        <p>Contains National Statistics data © Crown copyright and database right 2016. 
                Contains OS data © Crown copyright and database right 2016.
				        Data licensed under the <a href='http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/'>Open Government Licence v3.0</a></small></small></small>")
				)
					
		)
))