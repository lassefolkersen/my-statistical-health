library("shiny")


options(shiny.maxRequestSize=100*1024^2) 

# source("/home/ubuntu/srv/my-statistical-health/functions.R")




# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  output$ui_choices <- renderUI({
    input$uniqueID #update when uniqueID changes
    d<-get_data()
    
    if(is.null(d)){
      out = checkboxGroupInput("dynamic", "Variables to show",choices = NULL)  
    }else{
      c1 <- colnames(d)[2:ncol(d)]
      out = checkboxGroupInput("dynamic", "Variables to show",choices = c1)
    }
    return(out)
  })
    
  output$ui_slider <- renderUI({
    input$uniqueID #update when uniqueID changes
    d<-get_data()
    
    if(is.null(d)){
      out = sliderInput("time_window", "Time Window",min = 0, max = 10,value = c(1,9), step = 1)
    }else{
      d1<-d[,"date"]
      out = sliderInput("time_window", "Time Window",min = min(d1), max = max(d1),value = range(d1), step = 1)
    }
    return(out)
  })
   
  
  
  
  
  
  
  
  get_data <- reactive({
    uniqueID <- isolate(gsub(" ","",input$uniqueID))
    variables <- isolate(input$dynamic)
    
    
    if(nchar(uniqueID)!=12 | length(grep("^id_",uniqueID))==0){
     stop(safeError("uniqueID must be a 12 digit identifier starting with id_")) 
    }
      
    
    #get the data
    f1 <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata")
    if(!file.exists(f1)){return(NULL)}
    load(f1)  

    #remove any completely empty rows
    d<-d[apply(is.na(d),1,sum) < ncol(d)-1,]
    
    
    #iterate over variables, forcing all to become numeric, light-imputation, and loess for all
    for(var in variables){
      if(class(d[,var])=="character"){
        d[,var] <- as.numeric(as.factor(d[,var])) #this won't work unless the variables are alphabetically sorted
      }
      
      if(class(d[,var])=="factor"){
        d[,var] <- as.numeric(d[,var]) #this won't work unless the variables are alphabetically sorted
      }
      
      d[,"var"]<-as.numeric(d[,var])
      
      #try to add some light imputation style stuff (fill in same as preceding day, *if* there is only one day missing. Otherwise fail.)
      w<-which(is.na(d[,"var"]))
      d[w,"var"] <- d[w-1,"var"]
      if(any(is.na(d[,"var"]))){
        stop(safeError(paste("Couldn't compute for",var,"because of too many missing values")))
        next
      }
      
      
      d[,"d"]<-as.numeric(d[,"date"])
      f2<-as.formula(paste0("var ~ d"))
      l1<-try(loess(f2,d,span=0.2))
      if(class(l1)!="try-error"){
        d[rownames(d),paste0(var,"_loess")]<-l1$fitted
      }else{
        stop(safeError(paste("Didn't perform loess calculation for",var,"- the error message was:",l1)))
      }
    }
    return(d)
  })

  
  
    
  
  
  output$plot1 <- renderPlot({
    uniqueID <- isolate(gsub(" ","",input$uniqueID))
    variables <- isolate(input$dynamic)
    
    #take dependency
    if(input$goButton > 0){
      d<-get_data()
      
      library(RColorBrewer)
      col<-brewer.pal(12,"Set3")
      col<-col[c(1,3:12,2)]  #don't use yellow so early
      col<-rep(col,ceiling(length(variables)/12))
      
      lwd <- c(rep(3,4),rep(1,8))
      lwd<-rep(lwd,ceiling(length(variables)/12))
      
      r <-data.frame(
        request=  variables,
        col = col[1:length(variables)],
        lwd = lwd[1:length(variables)],
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
  
  output$table1 <- renderDataTable({
    uniqueID <- isolate(gsub(" ","",input$uniqueID))
    variables <- isolate(input$dynamic)
    time_lag <- isolate(input$time_lag)
    
    #take dependency
    if(input$predictionButton > 0){
      
      #get data
      d<-get_data()
      
      #get comparisons
      c2<-data.frame(t(combn(variables,2)),stringsAsFactors = F)
      colnames(c2)<-c("var1","var2")
      c2[,"name"]<-apply(c2,1,paste,collapse="/")
      if(nrow(c2)>30)stop(safeError("More than 30 combinations chosen. This is too much, don't input too many comparisons"))
      
      
      #prepare output table
      out<-data.frame(matrix(
        ncol=nrow(c2)+1,
        nrow=time_lag*2+1, 
        dimnames=list(seq(-time_lag,time_lag),c("time_lag",c2[,"name"]))),check.names=F)
      out[,"time_lag"] <- rownames(out)

      for(j in 1:nrow(c2)){
        var1<-c2[j,"var1"]
        var2<-c2[j,"var2"]  
        
        #removing rows that are NA for these two variables
        d1<-d[apply(is.na(d[,c(var1,var2)]),1,sum)==0,]

        #calculate correlation
        corr<-ccf(d1[,var1],d1[,var2],plot = F, lag.max=time_lag)
        out[,c2[j,"name"]] <- signif(corr[["acf"]][,,1],3)
      }
    return(out)
    }
  })
})


