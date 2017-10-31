# stop(getwd())
source("../uifunctions.R")
initialize('hc',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("操作台"),
  beginPage(),	
  # beginPanel('1/3'),
  # textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
  # endPanel(),
  # beginPanel('2/3'),
  
  # h2("Current status:"),
  plotOutput("plot1",height="800px"),
  # htmlOutput("text1"),
  # plotOutput("plot2"),
  
  
  
  # endPanel(),
  endPage(),
  footer()
  
)
)







