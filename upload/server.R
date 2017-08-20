library("shiny")

options(shiny.maxRequestSize=100*1024^2) 
source("/home/ubuntu/srv/my-statistical-health/functions.R")



# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$ui <- renderUI({
    uniqueID<-isolate(gsub(" ","",input$uniqueID))
    if(nchar(uniqueID)!=12 | length(grep("^id_",uniqueID))==0){
      out <- checkboxGroupInput("dynamic", "Dynamic",choices = "none")
      stop(safeError("E1"))
    }else{
      load(paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata"))
      out <- checkboxGroupInput("dynamic", "Dynamic",choices = colnames(d))
      stop(safeError("E2"))
    }
    return(out)
  })
})


