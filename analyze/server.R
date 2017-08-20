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
        out <-checkboxGroupInput("dynamic", "Variables to show",choices = c1)
        return(out)
      }
    }
  })
  
  
  output$plot1 <- renderPlot({
    uniqueID <- isolate(gsub(" ","",input$uniqueID))
    
    #take dependency
    if(input$goButton > 0){
      variables <- isolate(input$dynamic)
      
      f1 <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata")
      load(f1)  
      
      #remove any empty rows
      d<-d[apply(is.na(d),1,sum) < ncol(d)-1,]
      
      
      for(var in variables){
        if(class(d[,var])=="character"){
          d[,var] <- as.numeric(as.factor(d[,var])) #this won't work unless the variables are alphabetically sorted
        }
        
        if(class(d[,var])=="factor"){
          d[,var] <- as.numeric(d[,var]) #this won't work unless the variables are alphabetically sorted
        }
        
        d[,"var"]<-as.numeric(d[,var])
        
        #try to add some imputation style stuff
        w<-which(is.na(d[,"var"]))
        d[w,"var"] <- d[w-1,"var"]
        if(any(is.na(d[,"var"]))){
          print(paste("skipping",var,"because of too many missing values"))
          next
        }
        
        
        d[,"d"]<-as.numeric(d[,"date"])
        f2<-as.formula(paste0("var ~ d"))
        l1<-try(loess(f2,d,span=0.2))
        if(class(l1)!="try-error"){
          d[rownames(d),paste0(var,"_loess")]<-l1$fitted
        }else{
          print(paste("Didn't perform loess calc for",var))
        }
      }

      
      # n1<-0.4
      library(RColorBrewer)
      col<-rep(brewer.pal(12,"Set3"),ceiling(length(variables)/12))
          
      
      r <-data.frame(
        request=  variables,
        col = col[1:length(variables)],
        lwd = rep(1,length(variables)),
        stringsAsFactors = F
      )
      
      plot(x=d[,"date"],y=rep(0,nrow(d)),type="n",xlab="",ylab="",yaxt="n",ylim=c(0,1))
      for(i in 1:nrow(r)){
        data <- d[,paste0(r[i,"request"],"_loess")]
        data_norm <- (data-min(data)) / (max(data)-min(data))
        lines(d[,"date"], y=data_norm,col=r[i,"col"], lwd=r[i,"lwd"])
      }
      
      legend("topright",legend=sub(" \\(.+$","",gsub("\\."," ",r[,"request"])),lwd=r[,"lwd"],col=r[,"col"],bty="n")
      
      
            
    }
    
  })
  
})


