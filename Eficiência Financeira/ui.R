library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(lattice)

shinyUI(
    fluidPage(
        navbarPage(
            theme = shinytheme("sandstone"), "",
            tabPanel(icon = icon("star"), "Eficiencia Financeira",  
            )
        ),
        
        sidebarLayout(
            sidebarPanel(
                h3("Defina os valores abaixo:"),
		        br(),
                numericInput("peso_ent", "Peso", value = "400"),
                numericInput("valor_venda", "Valor de venda", value = "320"),
                numericInput("valor_compra", "Valor de compra", value = "350"),
                numericInput("ganho_dia", "Ganho por dia", value = "1.5"),
                
            ),
            
            mainPanel(
                h3("Equacao de lucro"),
                uiOutput("eqlucro"),
                h3("Grafico"),
                plotOutput("grafico"),
                
            ),
        ),
   )
)
