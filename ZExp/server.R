library(shiny)

shinyServer(
  function(input, output) {
    
    #DIC
    #Basicamente a leitura dos dados
    filedata <- reactive({
      infile <- input$file
      if (is.null(infile)) {
        return(NULL)
      }
       read.csv(infile$datapath, sep = input$sep1, dec = input$dec1)
    })
    
    #Faz a leitura dos dados e assim obriga escolher as variaveis em questao
    output$VarTrat <- renderUI({
      df <- filedata()
      if (is.null(df))
        return(NULL)
      items = names(df)
      names(items) = items
      selectInput("VarTrat",
                           "Selecione uma variavel tratamento:",
                           items,
                           multiple = FALSE)
      
    })
    
    output$VarResp <- renderUI({
      df <- filedata()
      if (is.null(df))
        return(NULL)
      items = names(df)
      names(items) = items
      selectInput("VarResp",
                           "Selecione uma variavel resposta:",
                           items,
                           multiple = FALSE)
      
    })
    
    #Realizacao da anova
    output$anovatable <- renderPrint(
      {
        dataset <- filedata()
        respo <- dataset[input$VarResp]
        respos <- unlist(respo)
        trata <- dataset[input$VarTrat]
        tratas <- unlist(trata)
        
        if(input$q1 == TRUE)
          tratas = as.factor(tratas)
        else  
          tratas = as.numeric(tratas)
        
       if(input$t1 == "scheffe"){
          a = aov(respos~tratas)  
          try(ScheffeTest(a, conf.level = input$nc), silent = TRUE)
       }
      else
          try(dic(tratas, respos, quali = input$q1, mcomp = input$t1, sigT = input$nc, sigF = input$nc), silent = TRUE)
      })
    
    
    
    #DBC
    #Basicamente a leitura dos dados
    filedata2 <- reactive({
      infile <- input$file2
      if (is.null(infile)) {
        return(NULL)
      }
      read.csv(infile$datapath, sep = input$sep2, dec = input$dec2) 
    })
    
    output$VarTrat2 <- renderUI({
      df <- filedata2()
      if (is.null(df))
        return(NULL)
      items = names(df)
      names(items) = items
      selectInput("VarTrat2",
                  "Selecione uma variavel tratamento:",
                  items,
                  multiple = FALSE)
    })
    output$VarBloco <- renderUI({
      df <- filedata2()
      if (is.null(df))
        return(NULL)
      items = names(df)
      names(items) = items
      selectInput("VarBloco", "Selecione uma variavel bloco:", 
                  items,
                  multiple = FALSE)
    })
    output$VarResp2 <- renderUI({
      df <- filedata2()
      if (is.null(df))
        return(NULL)
      items = names(df)
      names(items) = items
      selectInput("VarResp2",
                  "Selecione uma variavel resposta:",
                  items,
                  multiple = FALSE)
    })
    
    #Realizacao da anova
    output$anovatable2 <- renderPrint(
      {
        dataset=filedata2()
        respo<-dataset[input$VarResp2]
        respos <- unlist(respo)
        trata<-dataset[input$VarTrat2]
        tratas <- unlist(trata)
        bloco<-dataset[input$VarBloco]
        blocos <- unlist(bloco)
        
        if(input$q2 == TRUE)
          tratas = as.factor(tratas)
        else  
          tratas = as.numeric(tratas)
        
        
       if(input$t2 == "scheffe"){
         a = aov(respos~tratas)  
         try(ScheffeTest(a, conf.level = input$nc2), silent = TRUE)
       }
      else
        try(dbc(tratas, blocos, respos, quali=input$q2, mcomp=input$t2, sigT=input$nc2, sigF=input$nc2), silent = TRUE)
        
      })
    
    #Sobre
    
    output$contato <- shiny::renderUI({
      h4(tags$strong("Email: pablofsalmeida.7@gmail.com"))
    })
    
    output$github <- renderUI({
      h4(tags$strong("GitHub: www.github.com/binalmeida/ShinyApp/tree/master/ZExp"))
    })
    
  })