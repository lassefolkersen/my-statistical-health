# stop(getwd())
source("../uifunctions.R")
initialize('hc',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Health analyzer"),
  beginPage(),	
  beginPanel('1/3'),
  HTML("To run analysis input your user-id, or use the test-value of id_5909468R3:<br>"),
  textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
  
  
  wellPanel(
    # This outputs the dynamic UI component
    uiOutput("ui")
    
  ),
  actionButton("goButton","Run analysis"),
  endPanel(),
  beginPanel('2/3'),
  
  h2("Time series analysis:"),
  plotOutput("plot1"),
  
  
  
  endPanel(),
  endPage(),
  footer()
  
)
)







