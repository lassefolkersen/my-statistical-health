library("shiny")
library("dplyr")
library("googlesheets")



source("/home/ubuntu/srv/my-statistical-health/functions.R")
gs_auth(token = gdoc)
# gs_ls()
sheet <- gs_title("The breath-test")
data<-gs_read(sheet)



# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
  
  output$plot1 <- renderPlot({
    dates<-data %>% collect %>% .[[1]]
    cons<-data %>% collect %>% .[[16]]
    figh<-data %>% collect %>% .[[18]]
     
    #define a real data frame
    rdf<-data.frame(
      date=as.Date(dates),
      figh=as.numeric(figh),
      cons=as.numeric(cons)
    )
    
    
    
    #subset to rows with data
    rdf<-rdf[!is.na(rdf[,"figh"]) & !is.na(rdf[,"cons"]),]
    
    #categorize
    rdf[,"figh_low"]<-rdf[,"figh"] >=1
    rdf[,"figh_high"]<-rdf[,"figh"] >=2
    rdf[,"cons_low"]<-rdf[,"cons"] >=1
    rdf[,"cons_high"]<-rdf[,"cons"] >=2
    
    rdf_year<-rdf[Sys.Date()-rdf[,"date"] < 365,]
    rdf_month<-rdf[Sys.Date()-rdf[,"date"] < 30,]
    rdf_week<-rdf[Sys.Date()-rdf[,"date"] < 7,]
    
    
    
    out<-matrix(NA,nrow=4,ncol=3,dimnames=list(c("figh_low","figh_high","cons_low","cons_high"),c("week","month","year")))
    
    for(c in colnames(out)){
    for(r in rownames(out)){
        d<-get(paste0("rdf_",c))[,r]
        out[r,c]<- 100* sum(d) / length(d)
      }
    }
    
    barplot(t(out),beside=T,horiz=T,xlab="% of days")
    legend("topright",legend=rev(c("last week","last month","last year")),pch=19,col=rev(c("grey40","grey70","grey90")))

  })
  
  
  
  output$text1 <- renderText({ 
    if(is.null(input$goButton) || input$goButton == 0){
      return("")
    }else if(input$goButton > 1) {
      return("Don't click the button more than once. Reload page to reset.")
    }else{

      #dplyr explorter
      out<-data.frame(matrix(ncol=ncol(data),nrow=nrow(data)-1,dimnames=list(NULL,t(data)[,1])))
      for(i in 1:ncol(data)){
        d<-data %>% collect %>% .[[i]]
        out[,i] <- d[2:length(d)]
      }
      colnames(out)[1]<-"date"
      out[,"date"] <- as.Date(out[,"date"])
      
      
      #save as 'input file'
      temp_file_name <- paste0(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"-temp_file.xlsx")
      library(openxlsx)
      write.xlsx(out,file=temp_file_name)
      out_text<-prepare_input_file(path=temp_file_name, email="lassefolkersen@gmail.com", filename="google-doc", protect_from_deletion=T)
      # uniqueID<-sub("</i>$","",sub("^.+<i>","",out_text))
      
      
      return(out_text)
    }
  })
    
  
  
})


