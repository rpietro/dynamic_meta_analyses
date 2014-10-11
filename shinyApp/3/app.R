## app.R

library(rrdf)
library(data.table)
require("meta")
library(googleVis)

model <- load.rdf("dynamicMetaAnalysis.ttl","TURTLE")

query <- "
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX dct: <http://purl.org/dc/terms/>
PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX eg: <http://example.org/dynamic-meta-analysis#>
SELECT  
?id (STR(YEAR(?date)) AS ?year) ?title ?author ?latitude ?longitude
?sampleSizeIntervention ?timeLearningAverageIntervention ?timeLearningStandardDeviationIntervention
?sampleSizeControl ?timeLearningAverageControl ?timeLearningStandardDeviationControl
WHERE 
{ 
  ?obs rdf:type qb:Observation .
  ?obs eg:pubid ?id .
  ?obs dct:date ?date .
  ?obs dct:title ?title .
  ?obs dct:creator ?author .
  ?obs dct:coverage ?spatialInfo .
  ?spatialInfo geo:lat ?latitude .
  ?spatialInfo geo:long ?longitude .
  ?obs eg:sample-size-intervention ?sampleSizeIntervention .
  ?obs eg:time-learning-avg-intervention ?timeLearningAverageIntervention .
  ?obs eg:time-learning-sd-intervention ?timeLearningStandardDeviationIntervention .
  ?obs eg:sample-size-control ?sampleSizeControl .
  ?obs eg:time-learning-avg-control ?timeLearningAverageControl .
  ?obs eg:time-learning-sd-control ?timeLearningStandardDeviationControl .
  FILTER (lang(?title) = 'en')
}
"

server <- function(input, output, session) {

	# Run SPARQL Query to get information about the studies for the meta analysis and the geographic plot
	result <- sparql.rdf(model, query)

	# Convert the matrix result into a data frame
	qualitativeAnalysis <- data.frame(result, stringsAsFactors=FALSE)

	# Convert the dimensions needed for metacont (meta analysis) to numeric
	class(qualitativeAnalysis$sampleSizeIntervention) <- "numeric"
	class(qualitativeAnalysis$timeLearningAverageIntervention) <- "numeric"
	class(qualitativeAnalysis$timeLearningStandardDeviationIntervention) <- "numeric"
	class(qualitativeAnalysis$sampleSizeControl) <- "numeric"
	class(qualitativeAnalysis$timeLearningAverageControl) <- "numeric"
	class(qualitativeAnalysis$timeLearningStandardDeviationControl) <- "numeric"

	# Update the data table headers for correct output of qualitative analysis
	qualitativeAnalysis["PUBMED ID"] <- qualitativeAnalysis$id
	qualitativeAnalysis["Year"] <- qualitativeAnalysis$year
	qualitativeAnalysis["Title"] <- qualitativeAnalysis$title
	qualitativeAnalysis["Author"] <- qualitativeAnalysis$author
	qualitativeAnalysis["Sample Size Intervention"] <- qualitativeAnalysis$sampleSizeIntervention
	qualitativeAnalysis["Time Learning Average Intervention"] <- qualitativeAnalysis$timeLearningAverageIntervention
	qualitativeAnalysis["Time Learning Standard Deviation Intervention"] <- qualitativeAnalysis$timeLearningStandardDeviationIntervention
	qualitativeAnalysis["Sample Size Control"] <- qualitativeAnalysis$sampleSizeControl
	qualitativeAnalysis["Time Learning Average Control"] <- qualitativeAnalysis$timeLearningAverageControl
	qualitativeAnalysis["Time Learning Standard Deviation Control"] <- qualitativeAnalysis$timeLearningStandardDeviationControl

	# Select the variables that are shown in the qualitative analysis
	variables <- c("PUBMED ID", "Title", "Author", "Year", 
		"Sample Size Intervention", "Time Learning Average Intervention", "Time Learning Standard Deviation Intervention",
		"Sample Size Control", "Time Learning Average Control", "Time Learning Standard Deviation Control")

	# Output the qualitative data table
	output$studiesTable <- renderDataTable( qualitativeAnalysis[, variables],
		options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
		callback = "
		function(table) {
			table.on('click.dt', 'tr', function() {
				$(this).toggleClass('selected');
				Shiny.onInputChange('rows', table.rows('.selected').indexes().toArray());
			});
		}"
	)

	# Break long titles into multiple lines needed for forest plot
	for (n in 1:length(qualitativeAnalysis$title) ) {
		qualitativeAnalysis$titleWrapped[[n]] <- paste( strwrap( qualitativeAnalysis$title[[n]] , width=40 ), collapse = "\n" )
	}
	
	# Create the location dimension needed for GoogleVis
	qualitativeAnalysis$location = paste(qualitativeAnalysis$latitude, qualitativeAnalysis$longitude, sep=":")

	# Create reactive values in order to update the meta analysis and the geographic plot when Run Meta Analysis is pressed
	values <- reactiveValues()

	# Observe any change in the Run Meta Analysis button
	observe({
		if ( length( input$rows ) == 0 ) { # If no rows are selected consider all studies
			values$quantitativeAnalysis <- qualitativeAnalysis[0,]
			values$quantitativeAnalysis <- rbind( values$quantitativeAnalysisquantitativeAnalysis, qualitativeAnalysis )
		} else if ( length( input$rows ) > 0 ) { # Consider only selected values in the data table if there are more than one selected
			values$quantitativeAnalysis <- qualitativeAnalysis[0,]
			for ( row in isolate(input$rows) ) {
				isolate(values$quantitativeAnalysis <- rbind(values$quantitativeAnalysis, qualitativeAnalysis[ ( row + 1 ) ,]))
			}
    	}
	})

	# Run and output the meta analysis forest plot into the user interface
	output$forestPlot <- renderPlot({

		# Create a Progress object
	    progress <- shiny::Progress$new()
	    # Make sure it closes when we exit this reactive, even if there's an error
	    on.exit(progress$close())
	    progress$set(message = "Plotting", value = 1)

		metaAnalysis <- metacont(sampleSizeIntervention, timeLearningAverageIntervention, timeLearningStandardDeviationIntervention, sampleSizeControl, timeLearningAverageControl, timeLearningStandardDeviationControl,
           	data = values$quantitativeAnalysis, sm="SMD")
		forest(metaAnalysis,
	 		studlab = values$quantitativeAnalysis$id,
	 		leftcols=c("studlab", "author", "year", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
	 		leftlabs = c("PUBMED ID", "Author", "Year", "Total", "Mean", "SD", "Total", "Mean", "SD"),
	 		ff.fixed="plain", ff.hetstat="plain"
		)
	 }, width = 1000)

	# Output the geographic plot with markers into the user interface
	output$geoPlot <- renderGvis({

		# Create a Progress object
	    progress <- shiny::Progress$new()
	    # Make sure it closes when we exit this reactive, even if there's an error
	    on.exit(progress$close())
	    progress$set(message = "Plotting", value = 1)

		gvisGeoChart(values$quantitativeAnalysis, "location", "PUBMED ID", "", "title", 
			options = list(displayMode="Markers",
	 			sizeAxis = "{minSize: 4, maxSize: 4}",
	 			colorAxis = "{ colors:['#DD4814', '#DD4814'] }",
	 			width = 1000, height = 400
	 		))
	})
}

ui <- shinyUI(fluidPage(theme = "united.css",
	titlePanel("Dynamic Meta Analysis"),
	fluidRow(
		wellPanel(
			dataTableOutput('studiesTable')
		)
	),
	fluidRow(
	   	plotOutput('forestPlot', width="100%"),
	   	align = "center"
	),
	fluidRow(
	   	htmlOutput('geoPlot'),
	   	align = "center"
   	)
))

shinyApp(ui = ui, server = server)