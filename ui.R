
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
df_mush <- read.csv("./data/agaricus-lepiota.data.txt", header = FALSE)
df_nmmush <- read.csv("./data/agaricus-lepiota.nomes.csv", header = FALSE)
v_nmmush <- df_nmmush[['V1']]
get.names<- function(x,y) {
    names(x)<-y
    x
}
df_mush <- get.names(df_mush,v_nmmush)
df_mush <- get.names(df_mush,gsub("-",".",names(df_mush),))

shinyUI(pageWithSidebar(
    headerPanel('Análise de cluster de carteira de cartões'),
    sidebarPanel(
        selectInput('xcol', 'Variável X', names(df_mush),
                    selected=names(df_mush)[[2]]),
        selectInput('ycol', 'Variável Y', names(df_mush),
                    selected=names(df_mush)[[4]]),
        numericInput('clusters', 'Contagem de Agrupamentos', 3,
                     min = 1, max = 9)
    ),
    mainPanel(
        plotOutput('plot1')
    )
))