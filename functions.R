

# read configuration file
source("/home/ubuntu/misc_files/configuration.R")
if(!exists("maxImputations"))stop("Didn't find maxImputations")
if(!is.numeric(maxImputations))stop("maxImputations not numeric")
if(length(maxImputations)!=1)stop("maxImputations not length 1")
if(!exists("maxImputationsInQueue"))stop("Didn't find maxImputationsInQueue")
if(!is.numeric(maxImputationsInQueue))stop("maxImputationsInQueue not numeric")
if(length(maxImputationsInQueue)!=1)stop("maxImputationsInQueue not length 1")
if(!exists("serverRole"))stop("Didn't find serverRole")
if(!is.character(serverRole))stop("serverRole not character")
if(length(serverRole)!=1)stop("serverRole not length 1")
if(!serverRole%in%c("Hub","Node"))stop("serverRole not Hub or Node")
if(!exists("hubAddress"))stop("Didn't find hubAddress")
if(!is.character(hubAddress))stop("hubAddress not character")
if(length(hubAddress)!=1)stop("hubAddress not length 1")
if(!exists("email_password"))stop("Didn't find email_password ")
if(!is.character(email_password ))stop("email_password  not character")
if(length(email_password )!=1)stop("email_password  not length 1")
if(!exists("email_address"))stop("Didn't find email_address")
if(!is.character(email_address))stop("email_address not character")
if(length(email_address)!=1)stop("email_address not length 1")
if(!exists("routinely_delete_this"))stop("Didn't find routinely_delete_this")
if(!is.character(routinely_delete_this))stop("routinely_delete_this not character")
if(!exists("paypal"))stop("Didn't find paypal")
if(!is.character(paypal))stop("paypal not character")
if(length(paypal)!=1)stop("paypal not length 1")
if(!exists("gdoc"))stop("Didn't find gdoc")
if(!is.character(gdoc))stop("gdoc not character")
if(length(gdoc)!=1)stop("gdoc not length 1")




prepare_input_file<-function(path, email, filename, protect_from_deletion){
  library(tools)
  library(openxlsx)
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
  
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be lengh 1, not",length(protect_from_deletion)))
  
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
  if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
    stop(safeError(paste("a real email adress is needed:",email)))
  }
  

  # Create uniqueID 
  uniqueID <- paste("id_",sample(1000:9000,1),sample(10000:90000,1),sep="")
  numberOfLetters<-sample(c(1,1,2,3),1)
  if(numberOfLetters>0){
    positionsToInsertLetter<-sample(5:(nchar(uniqueID)-1),numberOfLetters)
    l<-c(LETTERS,letters)
    l<-l[!l%in%c("o","O")] #I hate it when O is in
    for(x in positionsToInsertLetter){
      substr(uniqueID,x,x)<-sample(l,1)
    }
  }
  
  #check that it doesn't exists already
  print(paste("create imputation folder and output data folder for",uniqueID))
  if(uniqueID%in%list.files("/home/ubuntu/data/")){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)
    stop(safeError("Problem with unique ID generation. Please re-load and try again."))
  }
  
  
  
  d<-try(read.xlsx(path, detectDates=T))
  if(class(d)=="try-error"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"no_xlsx",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)
    stop(safeError("The file didn't look like an xlsx file."))
    
  }
  
  
  if(tolower(colnames(d)[1])!="date"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"no_date_header",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)
    stop(safeError("The header of the first column must be 'date'."))
  }
  if(colnames(d)[1]!="date"){colnames(d)[1]<-"date"}
  
  
  
  if(class(d[,1])!="Date"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"no_date_class",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)
    stop(safeError("The first column must contain data that is seen as a date."))
    
  }
  
  
  
  #create data folder, copy input file and save output file
  data_folder<-paste("/home/ubuntu/data/",uniqueID,"/",sep="")
  dir.create(data_folder)
  data_path<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".rdata",sep="")
  save(d,file=data_path)
  
  
  #creating the pData file
  timeStamp<-format(Sys.time(),"%Y-%m-%d-%H-%M")
  md5sum <- md5sum(paste(uniqueID,"_raw_data.txt",sep=""))
  f<-file(paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep=""),"w")
  writeLines(paste(c("uniqueID","filename","email","first_timeStamp","md5sum","protect_from_deletion"),collapse="\t"),f)
  writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,protect_from_deletion),collapse="\t"),f)
  close(f)
  
  
  #clean up    
  unlink(path)
  return(paste0("Data file succesfully submitted. You can now go to the analysis interface and look at your data using the uniqueID <i>",uniqueID,"</i>"))
  
  
}





make_overview_of_samples<-function(verbose=T){
  uniqueIDs<-list.files("/home/ubuntu/data/")
  all_pData<-list()
  for(uniqueID in uniqueIDs){
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    if(file.exists(pDataFile)){
      all_pData[[uniqueID]]<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
    }else{
      if(verbose)print(paste("Didn't find a pData file for",uniqueID))	
    }
  }
  all_columns<-unique(unlist(lapply(all_pData,colnames)))
  pData<-as.data.frame(matrix(nrow=0,ncol=length(all_columns),dimnames=list(NULL,all_columns)))
  for(uniqueID in names(all_pData)){
    p<-all_pData[[uniqueID]]
    for(missing_col in all_columns[!all_columns%in%colnames(p)]){
      p[1,missing_col]<-NA
    }
    pData<-rbind(pData,p[,all_columns])
  }
  rownames(pData)<-pData[,"uniqueID"]
  return(pData)
  
  
}






remove_all_empty_data_folders<-function(uniqueIDs=NULL){
  #A function that will crawl all data directories and remove any that are empty. These can happen on submission errors. Best to just execute manually
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  for(uniqueID in uniqueIDs){
    dataFolder<-paste("/home/ubuntu/data/",uniqueID,sep="")
    filesInside<-list.files(dataFolder)
    if(length(filesInside) == 0){
      print(paste("Deleting",dataFolder,"because it was empty"))
      unlink(dataFolder,recursive=T)
    }
  }
}










generate_report<-function(uniqueIDs=NULL, filename=NULL){
  #A function that will crawl all data directories and generate report with various 
  stop("Not implemented")
  # 
  # if(is.null(uniqueIDs)){
  #   uniqueIDs<-list.files("/home/ubuntu/data/")
  # }else{
  #   if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
  #   if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  # }
  # 
  # if(is.null(filename)){
  #   filename <- paste0(sample(1000:9999,1),sample(1000:9999,1),"_report.pdf")	
  # }else{
  #   if(class(filename)!="character")stop("filename must be of class character")
  #   if(length(filename)!=1)stop("filename must be of length 1")
  #   
  # }
  # filepath <- paste0("/home/ubuntu/srv/my-statistical-health/www/",filename)
  # relative_webpath <- paste0("www/",filename)
  # pdf(filepath,width=5,height=8)
  # layout(matrix(1:6,nrow=3,byrow=T))
  # 
  # first_timeStamps<-vector()
  # user_log<-data.frame(uniqueIDs=vector(),modules=vector(),dates=vector(),stringsAsFactors=F)
  # for(uniqueID in uniqueIDs){
  #   pData_file<-paste("/home/ubuntu/data",uniqueID,"pData.txt",sep="/")
  #   if(!file.exists(pData_file))next
  #   pData<-read.table(pData_file,sep="\t",header=T,stringsAsFactors=F)
  #   first_timeStamps<-c(first_timeStamps,pData[1,"first_timeStamp"])
  #   
  #   user_log_file<-paste("/home/ubuntu/data",uniqueID,"user_log_file.txt",sep="/")
  #   if(file.exists(user_log_file)){
  #     user_log_here<-readLines(user_log_file)
  #     s<-strsplit(user_log_here,"\t")
  #     dates<-sapply(s,function(x){x[1]})
  #     modules<-sapply(s,function(x){x[2]})
  #     d<-data.frame(uniqueIDs=rep(uniqueID,length(user_log_here)),modules=modules,dates=dates,stringsAsFactors=F)
  #     user_log<-rbind(user_log,d)
  #   }else{
  #     # user_log<-c()
  #   }
  #   
  # }
  # 
  # sampleSize<-data.frame(dates=sort(as.Date(first_timeStamps)),count=1:length(first_timeStamps))
  # plot(type='l',x=sampleSize[,"dates"],sampleSize[,"count"],xlab="Date",ylab="Sample Count",lwd=2,main="Sample size")
  # user_log<-user_log[order(strptime(user_log[,"dates"],format="%Y-%m-%d-%H-%S")),]
  # 
  # special_ids<-c("id_613z86871","id_4K806Dh21","id_8E523a1t7")
  # 
  # for(module in sort(unique(user_log[,"modules"]))){
  #   #only grab for this module
  #   u1<-user_log[user_log[,"modules"]%in%module,]
  #   #omit the special-users (e.g. myself)
  #   u1<-u1[!u1[,"uniqueIDs"]%in%special_ids,]
  #   
  #   #get cum-count
  #   u1[,"count"]<-1:nrow(u1)
  #   plot(type='s',x=strptime(u1[,"dates"],format="%Y-%m-%d-%H-%S"),u1[,"count"],xlab="Date",ylab="",lwd=2,main=module,col=rgb(1,0,0,0.7))
  #   
  #   par(new = T)
  #   u2<-u1[!duplicated(u1[,"uniqueIDs"]),,drop=FALSE]
  #   u2[,"count"]<-1:nrow(u2)
  #   plot(type='s',x=strptime(u2[,"dates"],format="%Y-%m-%d-%H-%S"),u2[,"count"],lwd=2,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  #   axis(4)
  #   
  #   # plot(type='s',x=1:100,y=1:100*sample(100:200,100),lwd=2,col=rgb(0,0,1,0.7),xaxt="n",yaxt="n",xlab="",ylab="")
  #   legend("topleft",col=c(rgb(1,0,0,0.7),rgb(0,0,1,0.7)),lty=1,legend=c("Unique Requests (left)","Unique Users (right)"),cex=0.7,lwd=2)
  #   
  #   
  # }
  # 
  # 
  # 
  # #plot only special ids (myself)
  # u1<-user_log
  # u1<-u1[u1[,"uniqueIDs"]%in%special_ids,]
  # #get cum-count
  # u1[,"count"]<-1:nrow(u1)
  # plot(type='s',x=strptime(u1[,"dates"],format="%Y-%m-%d-%H-%S"),u1[,"count"],xlab="Date",ylab="",lwd=2,main="Special users",col=rgb(1,0,0,0.7))
  # par(new = T)
  # u2<-u1[!duplicated(u1[,"uniqueIDs"]),,drop=FALSE]
  # u2[,"count"]<-1:nrow(u2)
  # plot(type='s',x=strptime(u2[,"dates"],format="%Y-%m-%d-%H-%S"),u2[,"count"],lwd=2,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  # axis(4)
  # legend("topleft",col=c(rgb(1,0,0,0.7),rgb(0,0,1,0.7)),lty=1,legend=c("Unique Requests (left)","Unique Users (right)"),cex=0.7,lwd=2)
  # 
  # 
  # 
  # 
  # #generate list of waiting genomes
  # waiting_files<-vector()
  # for(w1 in list.files("/home/ubuntu/imputations",full.names=T)){
  #   load(paste0(w1,"/variables.rdata"))
  #   status<-sub("Job is ","",read.table(paste0(w1,"/job_status.txt"),sep="\t",stringsAsFactors = F)[1,1])
  #   waiting_files<-c(waiting_files,paste(uniqueID,email,status,sep=" - "))
  # }
  # plot(NULL,ylim=c(0,length(waiting_files)+1),xlim=c(0,1),frame=F,xaxt="n",yaxt="n",xlab="",ylab="")
  # for(w2 in 1:length(waiting_files)){
  #   text(x=0.02,y=length(waiting_files)-w2,label=waiting_files[w2],adj=0,cex=0.6)
  # }
  # 
  # 
  # 
  # dev.off()
  # 
  # 
  # 
  # 
  
  return(relative_webpath)
}








