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
    sliderInput("time_lag", "Max correlation lag",min = 0, max = 30,value = 3, step = 1)
  ),
  
  endPanel(),
  beginPanel('2/3'),
  
  h2("Time series analysis:"),
  conditionalPanel(
    condition = "input.do_correlation",
    HTML("To avoid spurious correlations, it is your responsibility that data is <u><a href='https://en.wikipedia.org/wiki/Stationary_process'>stationary</a></u>. This just means that if there's a general trend or an un-related seasonality over time, then you may get inflated correlations. Also, when searching, remember the <u><a href='https://en.wikipedia.org/wiki/Multiple_comparisons_problem'>multiple comparisons problem</a></u>.")
  ),
  
  
  plotOutput("plot1"),
  htmlOutput("text1"),
  dataTableOutput("table1"),
  
  
  
  endPanel(),
  endPage(),
  footer()
  
)
)







