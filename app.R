# ICAR v0.01 ####
# Dr Vincent Looten
# Date de création : 2020-02-17
# Date de dernière modification : 2020-03-18
# Text encoding : UTF-8

# Load packages  ####
listepackages<-c("shiny","shinydashboard","tableHTML","ggplot2","lubridate","spacyr","dplyr","tidyr")
for (pack in listepackages) {
    if (!is.element(pack, installed.packages()[,1])){
        install.packages(pack, dep = TRUE)
    }
    eval(parse(text=paste0("library(",pack,")")))
}
rm(pack)

# Load Shiny app ####
source('ICAR_UI_2.R', encoding = 'UTF-8')
source('ICAR_server_2.R', encoding = 'UTF-8')

# Run the application ####
shinyApp(ui = ui, server = server)
