
library(shiny)
library(shinythemes)
library(mathjaxr)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggfortify)

shinyUI(
    fluidPage(
     navbarPage(
        theme = shinytheme("darkly"), "Zaft",
        tabPanel(icon = icon("star"), "Intervalo de Confianca",
                 sidebarPanel(
                     h1("Formulario"),
                     hr(),
                     withMathJax(helpText('$$prop (\\hat{p}) = \\frac{elementos}{amostra}$$')),
                     hr(),
                     h4("~> Pop infinita"),
                     withMathJax(helpText('$$IC = \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}}$$')
                    
                       ),
                    
                     
                     hr(),
                     h4("~> Pop finita"),
                     withMathJax(helpText('$$IC = \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}}\\sqrt{\\dfrac{(N-n)}{(N-1)}}$$')
                                 
                     ),
                     
                     hr(),
                     h4("~> Dif entre 2 pop"),
                     withMathJax(helpText('$$IC = (\\hat{p_1} - \\hat{p_2}) \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p_1}(1-\\hat{p_1})}{n_1} + \\dfrac{\\hat{p_2}(1-\\hat{p_2})}{n_2}}$$')
                    
                     ),            
                                           
            
         ),
   
        mainPanel(
            tabsetPanel(
                tabPanel(
                         icon = icon("tag"), "Pop infinita",
                         numericInput("elem", "Defina a quantidade de elementos " ,value = 20, step = 1 ),
                         numericInput("tam", "Defina o tamanho da amostra " ,value = 200, step = 1 ),
                         sliderInput("nivc", "Nivel de Confianca:", 0, 1, 0.95),
                         withMathJax(helpText('$$Prop (\\hat{p}) = $$')),
                         verbatimTextOutput("prop"),
                         withMathJax(helpText('$$IC = $$')),
                         verbatimTextOutput("propinf"),
                         
                         
                        
                ),
                tabPanel(
                        icon = icon("tag"),"Pop finita",
                        numericInput("elem2", "Defina a quantidade de elementos " ,value = 20, step = 1 ),
                        numericInput("tam2", "Defina o tamanho da amostra " ,value = 200, step = 1 ),
                        numericInput("tamp", "Defina o tamanho da pop" ,value = 200, step = 1 ),
                        sliderInput("nivc2", "Nivel de Confianca:", 0, 1, 0.95),
                        withMathJax(helpText('$$Prop (\\hat{p}) = $$')),
                        verbatimTextOutput("prop2"),
                        withMathJax(helpText('$$IC = $$')),
                        verbatimTextOutput("propfin"), 
                         
                         
                         
                ),
                tabPanel(icon = icon("tag"),"Dif entre 2 pop", 
                         numericInput("elem3", "Defina a quantidade de elementos 1 " ,value = 20, step = 1 ),
                         numericInput("tam3", "Defina o tamanho da amostra 1 " ,value = 200, step = 1 ),
                         numericInput("elem4", "Defina a quantidade de elementos 2 " ,value = 20, step = 1 ),
                         numericInput("tam4", "Defina o tamanho da amostra 2" ,value = 200, step = 1 ),
                         sliderInput("nivc3", "Nivel de Confianca:", 0, 1, 0.95),
                         withMathJax(helpText('$$Prop (\\hat{p_1}) = $$')),
                         verbatimTextOutput("prop3"),
                         withMathJax(helpText('$$Prop (\\hat{p_2}) = $$')),
                         verbatimTextOutput("prop4"),
                         withMathJax(helpText('$$IC = $$')),
                         verbatimTextOutput("prop2p"),
                         
                         
                         
                         )
            )
            
            
            
        )
        ),
        
        #dimensionamento de amostra
        
        tabPanel(
                 icon = icon("star"),"Dimensionamento de amostra", 
                 sidebarPanel(
                     h1("Formulario"),
                     hr(),
                     h4("~> Pop Infinita"),
                     withMathJax(helpText('$$n =  \\hat{p} \\hat{q} (\\dfrac{z_{\\alpha/2}}{e})^2 $$')),
                                 
                     
                     hr(),
                     h4("~> Pop Finita"),
                     withMathJax(helpText('$$n =  \\dfrac{N \\hat{p} \\hat{q} (z_{\\alpha/2})^2 }{\\hat{p}\\hat{q} (z_{\\alpha/2})^2 + (N-1) e^2}$$')
                                 
                     ),
                     
                 ),  
                 
             mainPanel(
                 
                 tabsetPanel(
                     tabPanel(
                         icon = icon("tag"), "Pop Infinita",
                         numericInput("err", "Defina o erro" , value = 0.03 ),
                         numericInput("phat", "Defina a prop sucesso " , value = 0.25, min = 0, max = 1, step = 0.01 ),
                         sliderInput("nivc4", "Nivel de Confianca:", 0, 1, 0.95),
                         withMathJax(helpText('$$Tamanho(n) = $$')),
                         verbatimTextOutput("propdesc"),
                  
                 ),
                     tabPanel(
                         icon = icon("tag"),"Pop Finita",
                         numericInput("phat2", "Defina a prop sucesso " , value = 0.25, min = 0, max = 1, step = 0.01 ),
                         numericInput("err2", "Defina o erro " ,value = 0.03, step = 1 ),
                         numericInput("tamp2", "Defina o tamanho da pop" ,value = 200, step = 1 ),
                         sliderInput("nivc5", "Nivel de Confianca:", 0, 1, 0.95),
                         withMathJax(helpText('$$Tamanho (n) = $$')),
                         verbatimTextOutput("propc"),
                          
                         
                         
                         
                     )
                     
                
                 )
                 
             )       
            
        ),
       
        #ajustes de modelos
        tabPanel(
          icon = icon("star"),"Regressao Linear Simples",
      
          mainPanel(
            tabsetPanel(
              tabPanel(  
                icon = icon("tag"), "Sem arquivo",
                textInput("x", "Variavel x", value = "426,550,493,165,276,350", placeholder = "Entre com valores separados por virgula e ponto para decimais, como por exemplo, 4.2, 4.4, 5, 5.03, etc."),
                textInput("y", "Variavel y", value = "536,920,680,230,420,490", placeholder = "Entre com valores separados por virgula e ponto para decimais, como por exemplo, 4.2, 4.4, 5, 5.03, etc."),
                textInput("xlab", label = "Nomes das variaveis:", value = "x", placeholder = "x label"),
                textInput("ylab", label = NULL, value = "y", placeholder = "y label"),
                
                uiOutput("verifcacao"),
                br(),
                h4("Equacao da reta"),
                uiOutput("eqreta"),
                br(),
                h4("Coeficiente de determinacao"),
                uiOutput("cd"),
                br(),
                h4("Regressao"),
                verbatimTextOutput("regressao"),
                br(),
                h4("Grafico de dispersao"),
                plotlyOutput("grafico"),
                
              ),
              tabPanel(  
                icon = icon("tag"), "Com arquivo",   
                sidebarLayout(
                  sidebarPanel(
                    fileInput('file', 'Escolha um arquivo',
                              accept=c('text/csv', 
                                       'text/comma-separated-values,text/plain', 
                                       '.csv')),
                    textInput("xlab2", label = "Nomes das variaveis:", value = "x", placeholder = "x label"),
                    textInput("ylab2", label = NULL, value = "y", placeholder = "y label"),
                    tags$hr(),
                    uiOutput("VarD"),
                    uiOutput("VarID"),
                    tags$hr(),
                    uiOutput('ui.action')
                  ),
                  mainPanel(
                    h4("Equacao da reta"),
                    uiOutput("eqreta2"),
                    br(),
                    h4("Coeficiente de determinacao"),
                    uiOutput("cd2"),
                    br(),
                    h4("Regressao"),
                    verbatimTextOutput('conteudo'),
                    br(),
                    h4("Grafico de dispersao"),
                    plotlyOutput("grafico2"),
                  )    
                )
              )
            )
          )
            
            )
          )
     )
          
        
     )
  
