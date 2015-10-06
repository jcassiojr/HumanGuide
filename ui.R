
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
source("./R/f_cluster_df.R")
df_exp <- f_cluster_df()

shinyUI(pageWithSidebar(
    headerPanel('Análise de cluster'),
    sidebarPanel(
        selectInput('xcol', 'Variável X', names(df_exp),
                    selected=names(df_exp)[[2]]),
        selectInput('ycol', 'Variável Y', names(df_exp),
                    selected=names(df_exp)[[4]]),
        numericInput('clusters', 'Contagem de Agrupamentos', 3,
                     min = 1, max = 9)
    ),
    mainPanel(
        plotOutput('plot1')
    )
))