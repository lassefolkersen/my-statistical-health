# stop(getwd())
setwd("/home/ubuntu/srv/my-statistical-health/imputeme")
source("../uifunctions.R")
# source("/home/ubuntu/srv/my-statistical-health/uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Initiate genome analysis"),
	beginPage(),
	beginPanel('1/3'),
	fileInput("largeFile", "Upload genome data", multiple = FALSE, accept = NULL),
	textInput(inputId="email", label = "Email", value = "nacho@24genetics.com"),
	# HTML("<u><a href='http://www.impute.me/www/terms_of_use.html'>Terms of use.</a></u>"),
	# checkboxInput("delete2weeks", "Non-fUnctional button", value = TRUE, width = NULL),
	actionButton("goButton","Start analysis data"),
	# actionButton("send_data","Summarize last-days data and send"),
	
	

	
	endPanel(),
	beginPanel('2/3'),
	HTML("Upload the file here. It is <i>very</i> important to wait until the process is finished. This may take several minutes and a receipt message will appear. If the browser window is closed or interupted before that, not all samples will process.<br>"),
	
	# textOutput("text1"),
	textOutput("text2"),
	htmlOutput("text3"),
	endPanel(),
			
				
	endPage(),
	footer()
))












