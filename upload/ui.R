# stop(getwd())
source("../uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Initiate genome analysis"),
	beginPage(),
	beginPanel('1/3'),
	fileInput("largeFile", "Upload genome data", multiple = FALSE, accept = NULL),
	textInput(inputId="email", label = "Email (optional)", value = ""),
	HTML("<u><a href='www/terms_of_use.html'>Terms of use.</a></u>"),
	checkboxInput("delete2weeks", "Delete data after two weeks", value = TRUE, width = NULL),
	actionButton("goButton","Send data to analysis"),
	
	
	

	
	endPanel(),
	beginPanel('2/3'),
	HTML("Upload a file containing the health data. It must excel xlsx format, the first column must be dates, and the first row must be headers. You can download an <u><a href='../www/template.xlsx'>example template here</a></u>."),
	
	
	textOutput("text2"),
	htmlOutput("text3"),
	endPanel(),
			
				
	endPage(),
	footer()
))












