# stop(getwd())
source("../uifunctions.R")
initialize('hc',TRUE)


shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Health analyzer"),
  beginPage(),	
  beginPanel('1/3'),
  HTML("Run analysis of data here.<br><br>To run analysis input your user-id, or use the test-value of id_5856884C1:<br>"),
  textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
  
  
  wellPanel(
    # This outputs the dynamic UI component
    uiOutput("ui")
    
  ),
  # 
  # conditionalPanel(
  #   condition = "input.trait_group == 'all'",
  #   selectInput("trait_all", "Traits:", choices = selections_all)
  # ),
  # conditionalPanel(
  #   condition = "input.trait_group == 'disease'",
  #   selectInput("trait_disease", "Traits:", choices = selections_disease)
  # ),
  # conditionalPanel(
  #   condition = "input.trait_group == 'biometrics'",
  #   selectInput("trait_biometrics", "Traits:", choices = selections_biometrics)
  # ),
  # conditionalPanel(
  #   condition = "input.trait_group == 'biomarker'",
  #   selectInput("trait_biomarker", "Traits:", choices = selections_biomarker)
  # ),
  # conditionalPanel(
  #   condition = "input.trait_group == 'response'",
  #   selectInput("trait_response", "Traits:", choices = selections_response)
  # ),
  # conditionalPanel(
  #   condition = "input.trait_group == 'other'",
  #   selectInput("trait_other", "Traits:", choices = selections_other)
  # ),
  # 
  # checkboxInput("advanced", label ="Advanced options", value = FALSE),
  # conditionalPanel(
  #   condition = "input.advanced",
  #   radioButtons("trait_group", "Trait categories:", trait_groups, selected = "all"),
  #   radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "global", inline = FALSE,width = NULL),
  #   checkboxInput("real_dist", label ="Plot real distribution (experimental)", value = FALSE)
  #   
  # ),
  # 
  actionButton("goButton","Run analysis"),
  endPanel(),
  beginPanel('2/3'),
  
  # h2("Genetic risk score:"),
  # htmlOutput("text_1"),
  # plotOutput("plot_1"),
  # htmlOutput("text_2"),
  # dataTableOutput("table1"),
  # htmlOutput("text_3"),
  
  
  
  endPanel(),
  endPage(),
  footer()
  
)
)







