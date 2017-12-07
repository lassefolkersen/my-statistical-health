source("../uifunctions.R")
initialize('con',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Contact"),
	beginPage(),
	HTML(	"
Design and calculations: <u><a href='http://orcid.org/0000-0003-0708-9530'>Lasse Folkersen</a></u>.<br>"),
# 	endPage(),
	footer()
))













