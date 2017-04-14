library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(cowplot)

function(input, output, session) {
  
  # Data management on imported file
  ec = reactive({
    req(input$epidcurve)
    x = unlist(unname(read.csv(input$epidcurve$datapath)))
    data.frame(NCASES=x,NWEEK=1:length(x))
  })
  
  # Plot epidemic curve
  output$plotepidcurve = renderPlot(
    ggplot(ec()) + 
      geom_col(aes(x=NWEEK,y=NCASES),fill="grey",colour="black",alpha=.6,width=1) + 
      labs(title="Epidemic curve",x="Weeks",y="N") +
      theme_bw()
  )
  
  # Management of serial interval distribution
  sidistr = reactive({
    req(input$sidisttype,input$simean,input$sisd)
    ff = data.frame(x=seq(0,input$simean+5*input$sisd,by=0.001))
    if(input$sidisttype=="Normal") ff$prob = dnorm(ff$x,mean=input$simean,sd=input$sisd)
    if(input$sidisttype=="Gamma") {
      ff$prob = dgamma(ff$x,rate=input$simean/(input$sisd^2),shape=(input$simean^2)/(input$sisd^2))
    }
    return(ff)
  })
  
  output$siunif <- reactive({
    req(input$sidisttype)
    input$sidisttype=="Uniform"
  })
  outputOptions(output, "siunif", suspendWhenHidden = FALSE) 
  
  output$simeansd <- reactive({
    req(input$sidisttype)
    input$sidisttype %in% c("Normal","Log-normal","Gamma")
  })
  outputOptions(output, "simeansd", suspendWhenHidden = FALSE) 
  
  observe({
    if(input$exsi=="Zika virus (Ae. aegypti, 28°C)") {
      updateSelectInput(session,"sidisttype",selected="Gamma")
      updateNumericInput(session,"simean",value=2.5)
      updateNumericInput(session,"sisd",value=0.7)
    }
    if(input$exsi=="Chikungunya virus (Ae. aegypti, 28°C)") {
      updateSelectInput(session,"sidisttype",selected="Gamma")
      updateNumericInput(session,"simean",value=1.6)
      updateNumericInput(session,"sisd",value=0.6)
    }
  })

  # Plot serial interval distribution
  output$plotsi = renderPlot(
    ggplot(sidistr()) +
      geom_ribbon(aes(x=x,ymax=prob),ymin=0,fill="grey",alpha=0.6) +
      geom_line(aes(x=x,y=prob),size=.7) +
      labs(title="Distribution of the serial interval",x="Weeks",y="Density") +
      theme_bw()
  )
  
  # Managing priors
  output$r0unif <- reactive({
    req(input$r0disttype)
    input$r0disttype=="Uniform"
  })
  outputOptions(output, "r0unif", suspendWhenHidden = FALSE) 
  
  output$r0meansd <- reactive({
    req(input$r0disttype)
    input$r0disttype %in% c("Log-normal","Gamma")
  })
  outputOptions(output, "r0meansd", suspendWhenHidden = FALSE) 
  
  
}