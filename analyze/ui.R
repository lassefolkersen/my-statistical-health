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
  
  
  # This outputs the dynamic UI choice-component
  wellPanel(
    uiOutput("ui_choices")
  ),
  actionButton("goButton","Analyze data"),
  checkboxInput("do_correlation", label ="Analyze correlation", value = FALSE),
  checkboxInput("advanced", label ="Advanced options", value = FALSE),
  conditionalPanel(
    condition = "input.advanced",
    uiOutput("ui_slider")
  ),
  conditionalPanel(
    condition = "input.advanced & input.do_correlation",
    sliderInput("time_lag", "Max correlation lag",min = 1, max = 30,value = 3, step = 1)
  ),
  
  endPanel(),
  beginPanel('2/3'),
  
  h2("Time series analysis:"),
  plotOutput("plot1"),
  dataTableOutput("table1"),
  
  
  
  endPanel(),
  endPage(),
  footer()
  
)
)







