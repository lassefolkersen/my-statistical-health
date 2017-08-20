source("../uifunctions.R")
initialize('con',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Contact"),
	beginPage(),
	HTML(	"
Design and calculations: <u><a href='http://www.dtu.dk/service/telefonbog/person?id=101696&tab=2&qt=dtupublicationquery'>Lasse Folkersen</a></u>.<br>"),
# 	endPage(),
	footer()
))













