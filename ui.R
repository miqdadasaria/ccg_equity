# Shiny ui for CCG equity indicators
# 
# Author: Miqdad Asaria
# Date: 30/06/2016
###############################################################################

library(shiny)
library(leaflet)
library(DT)

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
				  tabPanel("All CCGs", dataTableOutput("ccg_agi_data")),
				  tabPanel("Notes", tags$div(HTML("<p>&nbsp;<p>

                                           <dl class='dl-horizontal'>
				                                   <dt>CCG</dt> 
                                           <dd>CCG or <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>
                                           clinical commisioning groups</a> are the organisations that
                                           deliver health services in the NHS.</dd>
                                           <p>

                                           <dt>IMD</dt>
				                                   <dd>IMD is the <a href='https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015'>
                                           index of multiple deprivation</a> 2015 normalised rank. Rank 0 
                                           represents the least deprived neighbourhood (<a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>LSOA</a>)
                                           and rank 1 represents the most deprived neighbourhood in the
                                           country.</dd>
                                           <p>

				                                   <dt>Population</dt> 
                                           <dd>Population is the <a href='https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates'>
                                           ONS mid-year population</a> at <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>LSOA</a> level attributed to 
                                           <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCGs</a>.</dd>
				                                   <p>

                                           <dt>Standardised rate</dt>
                                           <dd>Standardised rate is the rate of <a href='https://data.england.nhs.uk/dataset/nhsof-2-6-unplanned-hospitalisation-for-chronic-ambulatory-care-sensitive-conditions'>
                                           unplanned hospitalisation for chronic ambulatory care sensitive conditions</a> 
                                           over the year per 100,000 of population <a href='https://en.wikipedia.org/wiki/Standardized_mortality_ratio'>indirectly standardised</a>
                                           for age and sex.</dd>
                                           <p>

                                           <dt>AGI</dt>
                                           <dd>The AGI (Absolute Gradient Index) is a tool that allows us to measure socioeconomic 
                                           inequalities within <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCGs</a>
                                           and compare socioeconomic inequalities
                                           between <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCGs</a>. 
                                           It describes the difference in a particular outcome
                                           (for example the rate of emergency hospital admissions) that would 
                                           be observed between the richest and poorest 
                                           <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> 
                                           in the country if the whole country were as unequal as that 
                                           <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCG</a>. 
                                           The greater the value of the AGI the higher the inequality within the 
                                           <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCG</a>.
                                           
                                           <p><br>To calculate the AGI we first calculate the outcome of interest 
				                                   for each <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhood</a> 
                                           in the country (for example the rate of 
				                                   emergency hospital admissions per 100,000 population). The calculated
				                                   outcome may need to be <a href='https://en.wikipedia.org/wiki/Standardized_mortality_ratio'>standardised</a> 
                                           to allow us to compare 
				                                   <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> 
                                           with different population age and sex structures. We 
				                                   then rank all the 
                                           <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> 
                                           on a zero to one scale based on the 
				                                   <a href='https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015'>index of multiple deprivation</a>
                                           rank with the richest (least deprived) 
				                                   <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhood</a>
                                           being ranked zero and the poorest (most deprived) 
				                                   <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhood</a>
                                           being ranked one. Next we select all the 
                                           <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> 
				                                   in the <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCG</a> 
                                           for which we want to calculate the AGI and plot the outcome 
				                                   for each <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhood</a> 
                                           against the 
                                           <a href='https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015'>index of multiple deprivation</a> rank. 
				                                   Finally we draw a line of best fit through the points we have plotted - 
				                                   when fitting our line we give those 
                                           <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> with larger 
				                                   populations more weight than those 
                                           <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> 
                                           with smaller populations. The AGI is the gradient of this line. The steeper 
				                                   the line the greater the difference between the richest and poorest 
				                                   <a href='http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/census/super-output-areas--soas-/index.html'>neighbourhoods</a> 
                                           and hence the greater the inequality and the larger the AGI value.</dd>
				                                   <p>
				                                   
                                           <dt>RGI</dt>
				                                   <dd>The RGI (Relative Gradient Index) is a relative inequality measure derived
                                           from the AGI. It is calculated by dividing the AGI by the rate of the 
                                           outcome for the area at the midpoint of the deprivation distribution.
                                           Hence it expresses the inequality as a proportion of the median outcome rate.</dd>
                                           <p>   

				                                   <dt>Similar CCGs</dt> 
                                           <dd>For each <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>
				                                   CCG</a> a group of 10 <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCGs</a> 
				                                   most similar to that <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCG</a> is identified. These form the
				                                   natural comparator group for the <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCG</a>. 
				                                  
				                                   <p><br>The similar <a href='https://en.wikipedia.org/wiki/Clinical_commissioning_group'>CCG</a>
				                                   group members are selected based on how closely matched they are on the following criteria:
				                                   <a href='https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015'>IMD</a> (deprivation) score, 
				                                   <a href='https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015'>IMD</a> health domain score, 
				                                   total registered population, 
				                                   % of population 0-4, 
				                                   % of population 5-14,
				                                   % of population 15-24, 
				                                   % of population 75+, 
				                                   ratio of registered population to ONS estimates (\"list inflation\"),
				                                   population density, 
				                                   slope variation in population density, 
				                                   % of population black ethnic groups,
				                                   % of population Asian ethnic groups. 
				                                   Further details can be found <a href='https://www.learnenv.england.nhs.uk/similar'>here</a>.
				                                   </dd>
 
				                                  </dl>"
				                                  )))
				),
				tags$div(
				  HTML("<small><small><small><p>&nbsp;<p>
                <p>The scatter plot shows LSOAs within CCGs with the size of the point representing the population that the LSOA contributes to the CCG. 
	              The caterpillar plot show CCG AGI values and their 95% confidence intervals with the average across all CCGs plotted represented by the red dashed line
   			        <p>Contains National Statistics data © Crown copyright and database right 2016. 
                Contains OS data © Crown copyright and database right 2016.
				        Data licensed under the <a href='http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/'>Open Government Licence v3.0</a></small></small></small>")
				)
					
		)
))