#set the working directory to be the jdemetra-R-master folder
# eg setwd(".../.../jdemetra-R-master")

#load required packages
library(shiny)
library(ggplot2)

#source R files for JDemetra+ functions
source("./R files/jd_init.R")
source("./R files/jd_ts.R")
source("./R files/jd_calendars.R")
source("./R files/jd_regression.R")
source("./R files/jd_sa.R")
source("./R files/jd_rslts.R")
source("./R files/jd_spec.R")
source("./R files/jd_cholette.R")

#read in example data
nsa1<-ts(read.table("../SA-training-app/Data/exercise1.dat"),start=1996,frequency=12)
nsa2<-ts(read.table("../SA-training-app/Data/exercise2.dat"),start=1980,frequency=12)
nsa3<-ts(read.table("../SA-training-app/Data/exercise3.dat"),start=1997,frequency=4)
nsa4<-ts(read.table("../SA-training-app/Data/exercise4.dat"),start=1987,frequency=4)

#function to define inputs and outputs for app
shinyServer(function(input, output) {
  
#creates objects based on the user specified options in the interface
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Exercise 1" = nsa1,
           "Exercise 2" = nsa2,
           "Exercise 3" = nsa3,
           "Exercise 4" = nsa4)
  })

  trans1Input <- reactive({        
    switch(input$Transformation1,
           "Default" = "Auto",
           "None" = "None",
           "Log" = "Log")
  })
  
  trans2Input <- reactive({        
    switch(input$Transformation2,
           "Default" = "Auto",
           "None" = "None",
           "Log" = "Log")
  })
  
  easter1Input <- reactive({        
    switch(input$Easter1,
           "Default" = -1,
           "None" = 0,
           "Easter[1]" = 1,
           "Easter[8]" = 8,
           "Easter[15]" = 15)
  })
  
  easter2Input <- reactive({        
    switch(input$Easter2,
           "Default" = -1,
           "None" = 0,
           "Easter[1]" = 1,
           "Easter[8]" = 8,
           "Easter[15]" = 15)
  })
  
  SMA1Input <- reactive({        
    switch(input$SMA1,
           "S3X1" = "S3X1",
           "S3X3" = "S3X3",
           "S3X5" = "S3X5",
           "S3X9" = "S3X9",
           "S3X15" = "S3X15",
           "Msr" = "Msr",
           "Stable" = "Stable",
           "X11Default" = "X11Default")
  })
  
  SMA2Input <- reactive({        
    switch(input$SMA2,
           "S3X1" = "S3X1",
           "S3X3" = "S3X3",
           "S3X5" = "S3X5",
           "S3X9" = "S3X9",
           "S3X15" = "S3X15",
           "Msr" = "Msr",
           "Stable" = "Stable",
           "X11Default" = "X11Default")
  })
 

#perform seasonal adjustment with first set of parameters
  SA1 <- reactive({
    spec1<-spec_create()
    spec_str(spec1,"regarima.transform.function",trans1Input())
    if(input$TMA1D==FALSE)
    {spec_int(spec1,"x11.trendma",input$TMA1)}
    spec_strs(spec1,"x11.seasonalma",SMA1Input())
    if(easter1Input()==-1)
    {spec_str(spec1,"regarima.regression.mh1.type","Easter")
      spec_str(spec1,"regarima.regression.mh1.test","Add")}
    if(easter1Input()>0)
    {spec_str(spec1,"regarima.regression.mh1.type","Easter")
      spec_str(spec1,"regarima.regression.mh1.test","None")
      spec_int(spec1,"regarima.regression.mh1.param",easter1Input())}
    if(input$SB1!="")
    {year<-as.numeric(unlist(strsplit(input$SB1,"\\."))[1])
    period<-as.numeric(unlist(strsplit(input$SB1,"\\."))[2])
    start<-year+(period-1)/frequency(datasetInput())
    regs<-datasetInput()*0
    for(i in 1:(frequency(datasetInput())-1))
    {
      temp<-datasetInput()*0
      temp[cycle(temp)==i & time(temp)>=start]<-1
      temp[cycle(temp)==frequency(temp) & time(temp)>=start]<--1
      regs<-cbind(regs,temp)
    }  
    regs<-regs[,-1]
    colnames(regs)<-paste("S",1:(frequency(datasetInput())-1),sep="")
    for(i in 1:ncol(regs))
    {
      jd_registerVariable(regs[,i], paste(colnames(regs)[i]))
      spec_str(spec1,paste("regarima.regression.user",i,".name",sep=""),paste("vars.S",i,sep=""))
      spec_str(spec1,paste("regarima.regression.user",i,".effect",sep="") ,"Seasonal")
    }
    }
    x13_rslts1<-sa_x13(datasetInput(), "RSA3",spec1)
    if(input$SB1!="")
    {
      for(i in 1:(frequency(datasetInput())-1))
      {jd_unregisterVariable(paste("S",i,sep=""),"vars")}
      }
    return(x13_rslts1)
  })

#perform seasonal adjustment with second set of parameters  
  SA2 <- reactive({
    spec2<-spec_create()
    spec_str(spec2,"regarima.transform.function",trans2Input())
    if(input$TMA2D==FALSE)
    {spec_int(spec2,"x11.trendma",input$TMA2)}
    spec_strs(spec2,"x11.seasonalma",SMA2Input())
    if(easter2Input()==-1)
    {spec_str(spec2,"regarima.regression.mh1.type","Easter")
      spec_str(spec2,"regarima.regression.mh1.test","Add")}
    if(easter2Input()>0)
    {spec_str(spec2,"regarima.regression.mh1.type","Easter")
      spec_str(spec2,"regarima.regression.mh1.test","None")
      spec_int(spec2,"regarima.regression.mh1.param",easter2Input())}
    if(input$SB2!="")
    {year<-as.numeric(unlist(strsplit(input$SB2,"\\."))[1])
    period<-as.numeric(unlist(strsplit(input$SB2,"\\."))[2])
    start<-year+(period-1)/frequency(datasetInput())
    regs<-datasetInput()*0
    for(i in 1:(frequency(datasetInput())-1))
    {
      temp<-datasetInput()*0
      temp[cycle(temp)==i & time(temp)>=start]<-1
      temp[cycle(temp)==frequency(temp) & time(temp)>=start]<--1
      regs<-cbind(regs,temp)
    }  
    regs<-regs[,-1]
    colnames(regs)<-paste("S",1:(frequency(datasetInput())-1),sep="")
    for(i in 1:ncol(regs))
    {
      jd_registerVariable(regs[,i], paste(colnames(regs)[i]))
      spec_str(spec2,paste("regarima.regression.user",i,".name",sep=""),paste("vars.S",i,sep=""))
      spec_str(spec2,paste("regarima.regression.user",i,".effect",sep="") ,"Seasonal")
    }
    }
    x13_rslts2<-sa_x13(datasetInput(), "RSA3",spec2)
    
    if(input$SB2!="")
    {
      for(i in 1:(frequency(datasetInput())-1))
      {jd_unregisterVariable(paste("S",i,sep=""),"vars")}
      }
    return(x13_rslts2)
  })

#create output for overlay graph of original series and 2 seasonally adjusted series    
  output$tsPlot <- renderPlot({
    sa1<-proc_ts(SA1(), "sa")
    sa2<-proc_ts(SA2(), "sa")    

    data1<-data.frame(year=c(floor(time(datasetInput()))),month=c(cycle(datasetInput())),data=c(datasetInput()),Series=rep("NSA",times=length(datasetInput())))
    data2<-data.frame(year=c(floor(time(datasetInput()))),month=c(cycle(datasetInput())),data=c(sa1),Series=rep("SA1",times=length(datasetInput())))
    data3<-data.frame(year=c(floor(time(datasetInput()))),month=c(cycle(datasetInput())),data=c(sa2),Series=rep("SA2",times=length(datasetInput())))
    data<-rbind(data1,data2,data3)
    if(frequency(datasetInput())==12)
    {data$date<-as.Date(as.character(data$year*10000+data$month*100+1),"%Y%m%d")}
    if(frequency(datasetInput())==4)
    {data$date<-as.Date(as.character(data$year*10000+data$month*300+1),"%Y%m%d")}
    ggplot(data,aes(x=date,y=data,color=Series))+geom_line()+labs(x="",y="")+ 
      scale_color_manual(values=c("dodgerblue", "blue", "hotpink"))
  })

#create output for overlay graph of original series and 2 trends  
  output$trendPlot <- renderPlot({
    sa1<-proc_ts(SA1(), "t")
    sa2<-proc_ts(SA2(), "t")
    
    data1<-data.frame(year=c(floor(time(datasetInput()))),month=c(cycle(datasetInput())),data=c(datasetInput()),Series=rep("NSA",times=length(datasetInput())))
    data2<-data.frame(year=c(floor(time(datasetInput()))),month=c(cycle(datasetInput())),data=c(sa1),Series=rep("SA1",times=length(datasetInput())))
    data3<-data.frame(year=c(floor(time(datasetInput()))),month=c(cycle(datasetInput())),data=c(sa2),Series=rep("SA2",times=length(datasetInput())))
    data<-rbind(data1,data2,data3)
    if(frequency(datasetInput())==12)
    {data$date<-as.Date(as.character(data$year*10000+data$month*100+1),"%Y%m%d")}
    if(frequency(datasetInput())==4)
    {data$date<-as.Date(as.character(data$year*10000+data$month*300+1),"%Y%m%d")}
    ggplot(data,aes(x=date,y=data,color=Series))+geom_line()+labs(x="",y="")+ 
      scale_color_manual(values=c("dodgerblue", "blue", "hotpink"))
  })

#SI ratio chart of first adjustment  
  output$SIPlot1 <- renderPlot({
    d8.1<-proc_ts(SA1(), "decomposition.d-tables.d8")
    d9.1<-proc_ts(SA1(), "decomposition.d-tables.d9")
    d9.1[is.nan(d9.1)]<-NA
    d10.1<-proc_ts(SA1(), "decomposition.d-tables.d10")
    data1<-data.frame(year=floor(time(d8.1)),period=c(cycle(d8.1)),d8=d8.1,d9=d9.1,d10=d10.1)
    mean1<-aggregate(d10~period,data1,mean)
    colnames(mean1)[2]<-"mean"
    data1<-merge(data1,mean1,by="period")
    if(max(data1$period)==12)
      {data1$period<-factor(data1$period,levels=1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))}
    else if(max(data1$period)==4)
      {data1$period<-factor(data1$period,levels=1:4,labels=c("Q1","Q2","Q3","Q4"))}
    data1$unmod<-ifelse(!is.na(data1$d9),data1$d8,NA)
    data1$mod<-ifelse(!is.na(data1$d9),data1$d9,data1$d8)
    ggplot(data1, aes(x=year, y=mod))+geom_point(col="blue")+
      geom_point(aes(x=year, y=unmod),col="hotpink")+
      geom_line(aes(x=year, y=d10),color="red")+
      geom_line(aes(x=year,y=mean),color="darkgreen")+
      facet_wrap(~period,nrow=1)+scale_x_continuous()+
      labs(x="",y="Seasonal factor",title="Seasonal factors - adjustment 1")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })

#SI ratio chart of second adjustment   
  output$SIPlot2 <- renderPlot({
    d8.2<-proc_ts(SA2(), "decomposition.d-tables.d8")
    d9.2<-proc_ts(SA2(), "decomposition.d-tables.d9")
    d9.2[is.nan(d9.2)]<-NA
    d10.2<-proc_ts(SA2(), "decomposition.d-tables.d10")
    data2<-data.frame(year=floor(time(d8.2)),period=c(cycle(d8.2)),d8=d8.2,d9=d9.2,d10=d10.2)
    if(max(data2$period)==12)
      {data2$period<-factor(data2$period,levels=1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))}
    else if(max(data2$period)==4)
      {data2$period<-factor(data2$period,levels=1:4,labels=c("Q1","Q2","Q3","Q4"))}
    mean2<-aggregate(d10~period,data2,mean)
    colnames(mean2)[2]<-"mean"
    data2<-merge(data2,mean2,by="period")
    data2$unmod<-ifelse(!is.na(data2$d9),data2$d8,NA)
    data2$mod<-ifelse(!is.na(data2$d9),data2$d9,data2$d8)
    ggplot(data2, aes(x=year, y=mod))+geom_point(col="blue")+
      geom_point(aes(x=year, y=unmod),col="hotpink")+
      geom_line(aes(x=year, y=d10),color="red")+
      geom_line(aes(x=year,y=mean),color="darkgreen")+
      facet_wrap(~period,nrow=1)+
      labs(x="",y="Seasonal factor",title="Seasonal factors - adjustment 2")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

#create table of default parameters  
  output$defaults <- renderTable({
    
    x13_rslts<-sa_x13(datasetInput(), "RSA5c")
    trans<-proc_str(x13_rslts,"decomposition.mode")
    if(is.null(proc_str(x13_rslts,"regression.easter")))
    {easter<-"None"}
      else
      {easter<-unlist(strsplit(proc_str(x13_rslts,"regression.easter"),":"))[1]}
    tma<-proc_str(x13_rslts,"decomposition.trendfilter")
    sma<-proc_str(x13_rslts,"decomposition.seasfilter")
    Parameter<-c("Transformation","Easter","Trend MA","Seasonal MA")
    Default<-c(trans,easter,tma,sma)
    out<-data.frame(Parameter,Default)
    return(out)})
  
})