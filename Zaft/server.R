

library(shiny)


shinyServer(function(input, output) {
  #---------------------------------------#
  #IC e Dimensionamento#

    output$txtout <- renderText({
        paste(input$txt, input$slider, sep = ";")
    })
    output$prop <- renderText({
       paste(round(input$elem/input$tam, 6))
       
    })
    output$prop2 <- renderText({
        paste(round(input$elem2/input$tam2, 6))
        
    })
    output$prop3 <- renderText({
        paste(round(input$elem3/input$tam3, 6))
        
    })
    output$prop4 <- renderText({
        paste(round(input$elem4/input$tam4, 6))
        
    })
    output$prop5 <- renderText({
        paste(round(input$elem5/input$tam5, 6))
        
    })
    prop.inf <- function(x, n, nivel = 0.95){
        p <- x/n 
        nc <- (nivel + 1)/2 
        
        LI <- p - qnorm(nc)*sqrt((p*(1-p))/n)  
        LS <- p + qnorm(nc)*sqrt((p*(1-p))/n)  
        
        return(list(x = x, n = n, nivel = nivel, LI = LI, LS = LS, p = p, nc = nc))
        
    }
 
    output$propinf <- renderText({
      
      ic <- prop.inf(input$elem, input$tam, input$nivc)
      
      paste0("[", round(ic$LI,4), ";" , round(ic$LS,4),"]")
      
    })  
    
    
    prop.fin <- function(x , n, N, nivel = 0.95){
      p <- x/n 
      nc <- (nivel + 1)/2 
      
      LI <- p - qnorm(nc)*sqrt((p*(1-p))/n)*sqrt(((N-n))/(N-1)) 
      LS <- p + qnorm(nc)*sqrt((p*(1-p))/n)*sqrt(((N-n))/(N-1))  
      
      return(list(x = x, n = n, N = N, nivel = nivel, LI = LI, LS = LS, p = p, nc = nc))
        
    }
    output$propfin <- renderText({
      
      ic <- prop.fin(input$elem2, input$tam2, input$tamp,input$nivc2)
      
      paste0("[", round(ic$LI,4), ";" , round(ic$LS,4),"]")
      
    })
    
    prop.2p <- function(x1 , n1, x2, n2, N, nivel = 0.95){
      p1 <- x1/n1
      p2 <- x2/n2
      nc <- (nivel + 1)/2 
      
      LI <- (p1-p2) - qnorm(nc)*sqrt((p1*(1-p1))/n1 + (p2*(1-p2))/n2) 
      LS <- (p1-p2) + qnorm(nc)*sqrt((p1*(1-p1))/n1 + (p2*(1-p2))/n2)  
      
      return(list(x1 = x1, n1 = n1, x2 = x2, n2 = 2, nivel = nivel, LI = LI, LS = LS, p1 = p1, p2 = p2, nc = nc))
      
    }
    output$prop2p <- renderText({
      
      ic <- prop.2p(input$elem3, input$tam3, input$elem4, input$tam4, input$nivc3)
      
      paste0("[", round(ic$LI,4), ";" , round(ic$LS,4),"]")
      
    })
    
    prop.desc <- function(e, p,  nivel = 0.95){
        nc <- (nivel + 1)/2
        
        n <- (p*(1-p))*((qnorm(nc)/e)^2)
        
        return(list(n = n, nivel = nivel, erro = e, p = p))
        
    }
    
     output$propdesc <- renderText({
         tn <- prop.desc(input$err, input$phat, input$nivc4)
         
         paste0(round(tn$n,3))
         
    })
     
     prop.c <- function(N, p, e , nivel = 0.95){
       nc <- (nivel + 1)/2
       q = 1 - p
       n <- ((N*p*q)*(nc)^2)/(((p*q)*(nc)^2) + ((N -1)* (e)^2))
       
       return(list(n = n, p = p , q = q, e = e , nivel = nivel))
       
     }
     
     output$propc <- renderText({
       tn <- prop.c(input$tamp2, input$phat2, input$err2, input$nivc5)
       
       paste0(round(tn$n,3))
       
     })
     
     
     #-------------------------------------#
     #Regressao#
     
     ext <- function(q) {
       text <- gsub(" ", "", q)
       split <- strsplit(text, ",", fixed = FALSE)[[1]]
       as.numeric(split)
     }
     
     output$verificacao <- renderUI({
       y <- ext(input$y)
       x <- ext(input$x)
       if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
         "Entrada invalida ou observacoes insuficientes"
       }
       else if(length(x) != length(y)) {
         "O numero de observacoes devem ser iguais para x e y"
       }
       
     })
     
     output$eqreta <- renderUI({
       y <- ext(input$y)
       x <- ext(input$x)
       q <- lm(y ~ x)
       withMathJax(
         paste0("\\( \\ y = \ a + \ b x = \\) ", round(q$coef[[1]], 3), " + ", round(q$coef[[2]], 3), "\\( x \\)")
       )
     })
     
     output$cd <- renderUI({
       y <- ext(input$y)
       x <- ext(input$x)
       q <- summary(lm(y ~ x))$r.squared
       withMathJax(
         paste0("\\( \\ R^2 = \\) ", round(q, 3))
       )
     })
     
     output$regressao <- renderPrint({
       y <- ext(input$y)
       x <- ext(input$x)
       m <- lm(y ~ x)
       
       summary(m)$coeff
     })
     
     output$grafico <- renderPlotly({
       y <- ext(input$y)
       x <- ext(input$x)
       q <- lm(y ~ x)
       df <- data.frame(x, y)
       gp <- ggplot(df, aes(x = x, y = y)) +
         geom_point() +
         geom_smooth(method = "lm", se = FALSE, color = "black") +
         ylab(input$ylab) +
         xlab(input$xlab) +
         theme_minimal()
       ggplotly(gp)
     })
     
     #------------------------#
     
     filedata <- reactive({
       infile <- input$file
       if (is.null(infile)){
         return(NULL)      
       }
       read.csv(infile$datapath)
     })
     
     output$VarD <- renderUI({
       df <- filedata()
       if (is.null(df)) return(NULL)
       items=names(df)
       names(items)=items
       selectInput("VarD","Selecione uma variavel dependente:",items)
     })
     
     
     output$VarID <- renderUI({
       df <- filedata()
       if (is.null(df)) return(NULL)
       items=names(df)
       names(items)=items
       selectInput("VarID","Selecione uma variavel independente:",items,multiple=FALSE)
     })
     
     
     output$conteudo <- renderPrint({
       input$action
       isolate({   
         df <- filedata()
         if (is.null(df)) return(NULL)
         fmla <- as.formula(paste(input$VarD," ~ ",paste(input$VarID,collapse="+")))
         summary(lm(fmla,data=df))$coeff
       })   
     })
     
     output$eqreta2 <- renderPrint({
       input$action
       isolate({   
         df <- filedata()
         if (is.null(df)) return(NULL)
         fmla <- as.formula(paste(input$VarD," ~ ",paste(input$VarID,collapse="+")))
         dmafe=summary(lm(fmla,data=df))$coeff
         withMathJax(
           paste0("\\( \\ y = \ a + \ b x = \\) ", round(dmafe[1,1], 3), " + ", round(dmafe[2,1], 3), "\\( x \\)")
         )
       })   
     })
     
     output$cd2 <- renderPrint({
       input$action
       isolate({   
         df <- filedata()
         if (is.null(df)) return(NULL)
         fmla <- as.formula(paste(input$VarD," ~ ",paste(input$VarID,collapse="+")))
         dmafe2=summary(lm(fmla,data=df))$r.squared
         
         withMathJax(
           paste0("\\( \\ R^2 = \\) ", round(dmafe2, 3))
         )
       })   
     })
     
     output$grafico2 <- renderPlotly({
       input$action
       isolate({
         df <- filedata()
         gp <- ggplot(df, aes(x = x, y = y)) +
           geom_point() +
           geom_smooth(method = "lm", se = FALSE, color = "black") +
           ylab(input$ylab2) +
           xlab(input$xlab2) +
           theme_minimal()
         ggplotly(gp)
       })
     })
     output$ui.action <- renderUI({
       if (is.null(input$file)) return()
       actionButton("action", "Run")
     })
     
    
  }
)
