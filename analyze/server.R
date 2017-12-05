library("shiny")


options(shiny.maxRequestSize=100*1024^2) 

# source("/home/ubuntu/srv/my-statistical-health/functions.R")



# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  output$ui_choices <- renderUI({
    input$uniqueID #update when uniqueID changes
    d<-get_data_for_ui()
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
    d<-get_data_for_ui()
    
    if(is.null(d)){
      out = sliderInput("time_window", "Time Window",min = 0, max = 10,value = c(1,9), step = 1)
    }else{
      d1<-d[,"date"]
      out = sliderInput("time_window", "Time Window",min = min(d1), max = max(d1),value = range(d1), step = 1)
    }
    return(out)
  })
  
  
  
  #just get the data - nothing more (for UI components)
  get_data_for_ui <- reactive({
    uniqueID <- gsub(" ","",input$uniqueID)
    
    #get the data
    f1 <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata")
    if(!file.exists(f1)){
      return(NULL)
    }
    load(f1)  
    #remove any completely empty rows
    d<-d[apply(is.na(d),1,sum) < ncol(d)-1,]
  })
  
  
  #get the data and clean it a bit - (for analytical components)
  get_all_data <- reactive({
    variables <- input$dynamic
    uniqueID <- gsub(" ","",input$uniqueID)
    time_window <- input$time_window
    
    if(nchar(uniqueID)!=12 | length(grep("^id_",uniqueID))==0){
      stop(safeError("uniqueID must be a 12 digit identifier starting with id_")) 
    }
    
    d<-get_data_for_ui()
    if(is.null(d)){
      Sys.sleep(2)
      stop(safeError(paste("This uniqueID does not exists")) )
    }
    
    #remove any completely empty rows
    d<-d[apply(is.na(d),1,sum) < ncol(d)-1,]
    
    
    #subset to time_window if requested
    if(class(time_window)=="Date"){
      d<-d[d[,"date"] >= time_window[1] & d[,"date"] <= time_window[2],]
      if(nrow(d)<1)stop(safeError("No data lines to plot - perhaps increase time window?"))
    }
    
    #iterate over variables, forcing all to become numeric, also "light"-imputation, and loess for all
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
    uniqueID <- gsub(" ","",input$uniqueID)
    variables <- input$dynamic
    
    
    #take dependency
    if(input$goButton > 0 & !is.null(variables)){
      d<-get_all_data()
      
      #set colours
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
        data_norm <- (data-min(data,na.rm=T)) / (max(data,na.rm=T)-min(data,na.rm=T))
        lines(d[,"date"], y=data_norm,col=r[i,"col"], lwd=r[i,"lwd"])
      }
      
      legend("topright",legend=sub(" \\(.+$","",gsub("\\."," ",r[,"request"])),lwd=r[,"lwd"],col=r[,"col"],bty="n")
    }
  })
  
  
  
  
  
  get_correlations <- reactive({
    variables <- input$dynamic
    time_lag <- input$time_lag
    
    if(input$goButton > 0 & input$do_correlation){
      #get data
      d<-get_all_data()
      
      #get comparisons
      c2<-data.frame(t(combn(variables,2)),stringsAsFactors = F)
      colnames(c2)<-c("var1","var2")
      c2[,"name"]<-apply(c2,1,paste,collapse="/")
      if(nrow(c2)>30)stop(safeError("More than 30 combinations chosen. This is too much, don't input too many comparisons"))
      
      
      #prepare output table
      out<-data.frame(matrix(NA,
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
        out[,c2[j,"name"]] <- corr[["acf"]][,,1]
      }
      return(out)
    }
  })
  
  
  output$text1 <- renderText({
    time_lag <- input$time_lag
    c1<-get_correlations()
    if(!is.null(c1)){
      
      #getting the best no lag positive correlation
      no_lag<-t(c1["0",2:ncol(c1)])
      no_lag<-no_lag[order((no_lag[,1]),decreasing = T),,drop=F]
      best_no_lag_label<-rownames(no_lag)[1]
      best_no_lag_magnitude<-signif(no_lag[1,1],2)
      
      #getting the best any-lag positive correlation
      if(time_lag>0){
        c2<-c1[order(apply((c1[,2:ncol(c1),drop=F]),1,max),decreasing=T),]
        best_lag_time <- rownames(c2)[1]
        lag<-t(c2[best_lag_time,2:ncol(c2),drop=F])
        lag<-lag[order((lag[,1]),decreasing = T),,drop=F]
        best_lag_label<-rownames(lag)[1]
        best_lag_magnitude<-signif(c2[best_lag_time,best_lag_label],2)
      }
      
      
      #getting the best no lag pos/neg correlation
      no_lag<-t(c1["0",2:ncol(c1)])
      no_lag<-no_lag[order((no_lag[,1]),decreasing = F),,drop=F]
      best_no_lag_neg_label<-rownames(no_lag)[1]
      best_no_lag_neg_magnitude<-signif(no_lag[1,1],2)
      
      
      #getting the best any-lag positive correlation
      if(time_lag>0){
        c2<-c1[order(apply(-(c1[,2:ncol(c1),drop=F]),1,max),decreasing=T),]
        best_lag_neg_time <- rownames(c2)[1]
        lag<-t(c2[best_lag_neg_time,2:ncol(c2),drop=F])
        lag<-lag[order((lag[,1]),decreasing = T),,drop=F]
        best_lag_neg_label<-rownames(lag)[1]
        best_lag_neg_magnitude<-signif(c2[best_lag_neg_time,best_lag_neg_label],2)
      }
      
      
      
      #composing message
      message <- paste0("The most positive same-day cross-correlation seems to be <i>",best_no_lag_label,"</i> at strength ",best_no_lag_magnitude,".")
      
      #if there's any <0 findings we also add a negative message
      if(best_no_lag_neg_magnitude < 0){
        message <- paste0(message," The most negative same-day cross-correlation seems to be <i>",best_no_lag_neg_label,"</i> at strength ",best_no_lag_neg_magnitude,".")
      }
      
      
      #if there was more time_lag we also add that search
      
      if(time_lag > 0){  
        intro <- " When searching for time-lagged correlations we found that"
        message <- paste0(message,intro) 

        if(best_lag_time != 0){  
          message <- paste0(message," the most positive cross-correlation seems to be <i>",best_lag_label,"</i> at time ",best_lag_time," having strength ",best_lag_magnitude,".")
        }        
        
        if(best_lag_neg_time != 0 & best_lag_neg_magnitude < 0){
          message <- paste0(message," the most negative time-lag cross-correlation seems to be <i>",best_lag_neg_label,"</i> at time ",best_lag_neg_time," having strength ",best_lag_neg_magnitude,".")
        }
        
        #don't include intro if nothing found
        message<-sub(paste0(intro,"$"),"",message)
      }
      
      
      #add some line spacing
      message <- paste0("<br>",message,"<br><br>")
      return(message)  
    }
    
  })
  
  
  
  output$table1 <- renderDataTable({
    c1<-get_correlations()
    if(!is.null(c1)){
      
      
      for(i in 2:ncol(c1)){
        c1[,i] <- signif(c1[,i],2)
      }
      c2 <- t(c1)
      
      
      c3 <- cbind(Comparison = rownames(c2), c2)
      
      #drop the time-lag row
      c4<-c3[2:nrow(c3),]
      
      
      #if time_lag zero we just do very plain headers
      if( input$time_lag == 0){
        if(ncol(c4)!=2)stop("Very odd - should only be one column in time_lag 0")
        colnames(c4) <- c("","Correlation")
        
      #otherwise we explain the time-lag in the headers  
      }else{
        colnames(c4)[1] <- c("Time-lag:")
        
      }
      
      return(c4)
    }
  })
  
  
  
  
})


