# stop(getwd())
source("../uifunctions.R")
initialize('hc',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Control Panel"),
  beginPage(),	
  # beginPanel('1/3'),
  # textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
  # endPanel(),
  # beginPanel('2/3'),
  
  # h2("Current status:"),
  plotOutput("plot1"),
  # htmlOutput("text1"),
  # plotOutput("plot2"),
  
  
  
  # endPanel(),
  endPage(),
  footer()
  
)
)







