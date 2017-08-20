library("shiny")


options(shiny.maxRequestSize=100*1024^2) 

source("/home/ubuntu/srv/my-statistical-health/functions.R")





# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	
	
	output$text2 <- renderText({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return("")
		}else if(input$goButton == 1) {
			return("")
		}else{
			stop(safeError("Please don't try to submit the job more than once. Re-load the page, re-upload and then only press 'start imputation' once."))
		}
		
	})
	
	
	
	output$text3 <- renderText({ 
		# Take a dependency on input$goButton
		if(input$goButton == 1){
			path <- isolate(input$largeFile[["datapath"]])
			email <- isolate(input$email)
			protect_from_deletion <- TRUE
			filename <- isolate(input$largeFile[["name"]])
			if(is.null(path))return("No file selected")
			out<-prepare_input_file(path,email,filename, protect_from_deletion)
			return(out)
		}
	})
})


