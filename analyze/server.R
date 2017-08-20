library("shiny")


options(shiny.maxRequestSize=100*1024^2) 

# source("/home/ubuntu/srv/my-statistical-health/functions.R")





# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
  output$ui <- renderUI({
    uniqueID <- gsub(" ","",input$uniqueID)
    blank <- checkboxGroupInput("dynamic", "Variables to show",choices = NULL)
    if(nchar(uniqueID)!=12 | length(grep("^id_",uniqueID))==0){
      return(blank)
      
    }else{
      f1 <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata")
      if(!file.exists(f1)){
        return(blank)
      }else{
        load(f1)  
        c1 <- colnames(d)[2:ncol(d)]
        out <-checkboxGroupInput("dynamic", "Dynamic",choices = c1)
        return(out)
      }
    }
  })
  
  
  output$plot1 <- renderUI({
    uniqueID <- isolate(gsub(" ","",input$uniqueID))
    
    #take dependency
    if(input$goButton > 0){
      variables <- input$dynamic
      
      stop(safeError(paste(variables,collapse=", ")))
      # for(x in colnames(d)[2:ncol(d)]){
      #   if(length(grep("loess$",x))>0)next #don't calculate on an existing loess column
      #   d[,"var"]<-as.numeric(d[,x])
      #   #try to add some imputatin style stuff
      #   w<-which(is.na(d[,"var"]))
      #   d[w,"var"] <- d[w-1,"var"]
      #   if(any(is.na(d[,"var"]))){
      #     print(paste("skipping",x,"because of too many missing values")) 
      #     next
      #   }
      #   d1<-d
      #   
      #   
      #   d1[,"d"]<-as.numeric(d1[,"date"])
      #   f2<-as.formula(paste0("var ~ d"))
      #   l1<-try(loess(f2,d1,span=0.2))
      #   if(class(l1)!="try-error"){
      #     d[rownames(d1),paste0(x,"_loess")]<-l1$fitted
      #   }else{
      #     print(paste("Didn't perform loess calc for",x))
      #   }
      # }
      # 
      
    }
    
  })
  
})


