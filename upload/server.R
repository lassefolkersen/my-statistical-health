library("shiny")

options(shiny.maxRequestSize=100*1024^2) 
source("/home/ubuntu/srv/my-statistical-health/functions.R")



# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$ui <- renderUI({
    uniqueID <- gsub(" ","",input$uniqueID)
    blank <- checkboxGroupInput("dynamic", "Dynamic",choices = "none")
    if(nchar(uniqueID)!=12 | length(grep("^id_",uniqueID))==0){
      return(blank)
      
    }else{
      f1 <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata")
      if(!file.exists(f1)){
        return(blank)
      }else{
        load(f1)  
        out <-checkboxGroupInput("dynamic", "Dynamic",choices = colnames(d))
        return(out)
        
      }

    }
  })
})


