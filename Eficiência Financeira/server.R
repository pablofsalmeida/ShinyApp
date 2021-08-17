
shinyServer(function(input, output) {
    
    eql <- function(peso, valorv, valorcom, ganhodia){
      
      a <- (peso/30)*(valorv-valorcom) +  1.6324939*valorv
      b <-  0.0348854*ganhodia*valorv
      c <- (-0.0348854)*ganhodia
      
      return(list(peso = peso, valorv = valorv, valorcom = valorcom, ganhodia = ganhodia, a=a, b=b,c=c))
    }
  
    
    
    output$eqlucro <- renderUI({
      lucro <- eql(input$peso_ent, input$valor_venda, input$valor_compra, input$ganho_dia)
        withMathJax(
            paste0("\\( Lucro = \\) ", "(", round(lucro$a, 4), ")", " + ", "(", round(lucro$b, 4), ")", "\\(*dia \ -1.6325*custo \ +\\)" ,"(",round(lucro$c,4), ")", "\\( *dia*custo \\)")
            
        )
    })
    
    output$grafico <- renderPlot({
      g <- expand.grid(dia = seq(50,400,length=200) , custo = seq(100,450,length=200))
      g$z <- (input$peso_ent/30)*(input$valor_venda-input$valor_compra) +   (1.6324939+  0.0348854*input$ganho_dia*g$dia)*(input$valor_venda -g$custo)
      
      
      cores <- colorRampPalette(c("brown1", "royalblue"))(20)
      levelplot(g$z ~ g$custo + g$dia, contour = TRUE, aspect = "iso",col.regions=cores, 
                xlab=list("Custo da arroba produzida", cex=.9), ylab=list("Dias em Confinamento", cex=.9)) 
      
    })
    
})
