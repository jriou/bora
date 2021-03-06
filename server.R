library(shiny)
library(dplyr)
library(ggplot2)
library(cowplot)
library(xtable)
library(knitr)

function(input, output, session) {
  
  # Imported file
  csvdata = reactive({
    req(input$epidcurve)
    x = unlist(unname(read.csv(input$epidcurve$datapath)))
    data.frame(NCASES=x,NWEEK=1:length(x))
  })
  
  
  # Data from library
  exdata = reactive({
    req(input$exdata)
    dd = NULL
    for(i in 1:length(data_lib)) {
      tmp = data_lib[[i]][[input$exdata]]
      if(length(tmp)>0) dd = data.frame(NCASES=tmp$data,NWEEK=1:length(tmp$data))
    }
    return(dd)
  })
  
  observe({
    req(input$exdata)
    for(i in 1:length(data_lib)) {
      tmp = data_lib[[i]][[input$exdata]]
      if(length(tmp)>0) {
        updateNumericInput(session,"popsize",value=tmp$popsize)
        updateNumericInput(session,"n.eoo",value=tmp$n.eoo)
      }
    }
  })
  
  # Data management
  ec = reactive({
    if(!is.null(exdata())) {
      exdata()
    } else {
      csvdata()
    }
  })
  
  popsize = reactive({
    req(input$popsize)
    input$popsize
  })
  
  # Plot epidemic curve
  output$plotepidcurve = renderPlot(
    ggplot(ec()) + 
      geom_col(aes(x=NWEEK,y=NCASES),fill="grey",colour="black",alpha=.6,width=1) + 
      labs(x="Weeks",y="N") +
      theme_bw()
  )
  
  # Controls on serial interval distribution
  output$siunif <- reactive({
    req(input$sidisttype)
    input$sidisttype=="Uniform"
  })
  outputOptions(output, "siunif", suspendWhenHidden = FALSE) 
  
  output$simeansd <- reactive({
    req(input$sidisttype)
    input$sidisttype %in% c("Normal","Gamma")
  })
  outputOptions(output, "simeansd", suspendWhenHidden = FALSE) 
  
  observe({
    req(input$exsi)
    for(i in 1:length(data_lib)) {
      tmp = si_lib[[i]][[input$exsi]]
      if(length(tmp)>0) {
        updateSelectInput(session,"sidisttype",selected=tmp$type)
        if(tmp$type=="Gamma") {
          updateNumericInput(session,"simean",value=tmp$mean)
          updateNumericInput(session,"sisd",value=tmp$sd)
        }
      }
    }
  })
  
  # Management of serial interval distribution
  sidistr = reactive({
    req(input$sidisttype)
    if(input$sidisttype=="Normal") {
      req(input$simean,input$sisd)
      ff = data.frame(x=seq(0,input$simean+5*input$sisd,by=0.001))
      ff$prob = dnorm(ff$x,mean=input$simean,sd=input$sisd)
      ff$prob = ff$prob/sum(ff$prob)
      return(list(type="Normal",mean=input$simean,sd=input$sisd,ff=ff))
    }
    if(input$sidisttype=="Gamma") {
      req(input$simean,input$sisd)
      ff = data.frame(x=seq(0,input$simean+5*input$sisd,by=0.001))
      ff$prob = dgamma(ff$x,rate=input$simean/(input$sisd^2),shape=(input$simean^2)/(input$sisd^2))
      return(list(type="Gamma",mean=input$simean,sd=input$sisd,rate=input$simean/(input$sisd^2),shape=(input$simean^2)/(input$sisd^2),ff=ff))
    }
    if(input$sidisttype=="Uniform") {
      req(input$simin,input$simax)
      ff = data.frame(x=seq(0,input$simax*1.1,by=0.001))
      ff$prob = dunif(ff$x,min=input$simin,max=input$simax)
      return(list(type="Uniform",min=input$simin,max=input$simax,ff=ff))
    }
  })
  
  # Plot serial interval distribution
  output$plotsi = renderPlot(
    if(input$sidisttype!="-")
      ggplot(sidistr()$ff) +
      geom_ribbon(aes(x=x,ymax=prob),ymin=0,fill="grey",alpha=0.6) +
      geom_line(aes(x=x,y=prob),size=0.7) +
      labs(x="Weeks",y="PDF") +
      theme_bw()
  )
  
  # Controls on prior on r0
  output$r0unif <- reactive({
    req(input$r0disttype)
    input$r0disttype=="Uniform"
  })
  outputOptions(output, "r0unif", suspendWhenHidden = FALSE) 
  
  output$r0meansd <- reactive({
    req(input$r0disttype)
    input$r0disttype %in% c("Normal","Gamma")
  })
  outputOptions(output, "r0meansd", suspendWhenHidden = FALSE) 
  
  output$r0exp <- reactive({
    req(input$r0disttype)
    input$r0disttype %in% c("Exponential")
  })
  outputOptions(output, "r0exp", suspendWhenHidden = FALSE) 
  
  observe({
    req(input$exr0)
    for(i in 1:length(data_lib)) {
      tmp = r0_lib[[i]][[input$exr0]]
      if(length(tmp)>0) {
        updateSelectInput(session,"r0disttype",selected=tmp$type)
        if(tmp$type=="Gamma") {
          updateNumericInput(session,"r0mean",value=tmp$mean)
          updateNumericInput(session,"r0sd",value=tmp$sd)
        }
      }
    }
  })
  
  
  # Management of prior on r0
  r0distr = reactive({
    req(input$r0disttype)
    if(input$r0disttype=="Uniform") {
      req(input$r0min,input$r0max)
      ff = data.frame(x=seq(0,max(3,input$r0max*1.1),by=0.001))
      ff$prob = dunif(ff$x,min=input$r0min,max=input$r0max)
      return(list(type="Uniform",min=input$r0min,max=input$r0max,ff=ff))
    }
    if(input$r0disttype=="Exponential") {
      req(input$r0rate)
      ff = data.frame(x=seq(0,max(3,2.5/input$r0rate),by=0.001))
      ff$prob = dexp(ff$x,rate=input$r0rate)
      return(list(type="Exponential",rate=input$r0rate,ff=ff))
    }
    if(input$r0disttype=="Normal") {
      req(input$r0mean,input$r0sd)
      ff = data.frame(x=seq(0,max(3,input$r0mean+5*input$r0sd),by=0.001))
      ff$prob = dnorm(ff$x,mean=input$r0mean,sd=input$r0sd)
      ff$prob = ff$prob/sum(ff$prob)
      return(list(type="Normal",mean=input$r0mean,sd=input$r0sd,ff=ff))
    }
    if(input$r0disttype=="Gamma") {
      req(input$r0mean,input$r0sd)
      ff = data.frame(x=seq(0,max(3,input$r0mean+5*input$r0sd),by=0.001))
      ff$prob = dgamma(ff$x,rate=input$r0mean/(input$r0sd^2),shape=(input$r0mean^2)/(input$r0sd^2))
      return(list(type="Gamma",mean=input$r0mean,sd=input$r0sd,rate=input$r0mean/(input$r0sd^2),shape=(input$r0mean^2)/(input$r0sd^2),ff=ff))
    }
  })
  
  # Plot prior distribution on r0
  output$plotpriorr0 = renderPlot(
    if(input$r0disttype!="-")
      ggplot(r0distr()$ff) +
      geom_ribbon(aes(x=x,ymax=prob),ymin=0,fill="grey",alpha=0.6) +
      geom_line(aes(x=x,y=prob),size=0.7) +
      labs(x=expression(R[0]),y="PDF") +
      theme_bw()
  )
  
  # Controls on prior on rho
  
  output$rhounif <- reactive({
    req(input$rhodisttype)
    input$rhodisttype=="Uniform"
  })
  outputOptions(output, "rhounif", suspendWhenHidden = FALSE) 
  
  output$rhobetad <- reactive({
    req(input$rhodisttype)
    input$rhodisttype %in% c("Beta")
  })
  outputOptions(output, "rhobetad", suspendWhenHidden = FALSE) 
  
  output$rhomeansd <- reactive({
    req(input$rhodisttype)
    input$rhodisttype %in% c("Normal","Gamma")
  })
  outputOptions(output, "rhomeansd", suspendWhenHidden = FALSE) 
  
  observe({
    req(input$exrho)
    for(i in 1:length(data_lib)) {
      tmp = rho_lib[[i]][[input$exrho]]
      if(length(tmp)>0) {
        updateSelectInput(session,"rhodisttype",selected=tmp$type)
        if(tmp$type=="Gamma") {
          updateNumericInput(session,"rhomean",value=tmp$mean)
          updateNumericInput(session,"rhosd",value=tmp$sd)
        }
        if(tmp$type=="Beta") {
          updateNumericInput(session,"rhoshape1",value=tmp$shape1)
          updateNumericInput(session,"rhoshape2",value=tmp$shape2)
        }
      }
    }
  })
  
  
  # Management of prior on rho
  rhodistr = reactive({
    req(input$rhodisttype)
    if(input$rhodisttype=="Uniform") {
      req(input$rhomin,input$rhomax)
      ff = data.frame(x=seq(0,1,by=0.001))
      ff$prob = dunif(ff$x,min=input$rhomin,max=input$rhomax)
      return(list(type="Uniform",min=input$rhomin,max=input$rhomax,ff=ff))
    }
    if(input$rhodisttype=="Beta") {
      req(input$rhoshape1,input$rhoshape2)
      ff = data.frame(x=seq(0,1,by=0.001))
      ff$prob = dbeta(ff$x,shape1=input$rhoshape1,shape2=input$rhoshape2)
      return(list(type="Beta",shape1=input$rhoshape1,shape2=input$rhoshape2,ff=ff))
    }
    if(input$rhodisttype=="Gamma") {
      req(input$rhomean,input$rhosd)
      ff = data.frame(x=seq(0,1,by=0.001))
      ff$prob = dgamma(ff$x,rate=input$rhomean/(input$rhosd^2),shape=(input$rhomean^2)/(input$rhosd^2))
      return(list(type="Gamma",mean=input$rhomean,sd=input$rhosd,rate=input$rhomean/(input$rhosd^2),shape=(input$rhomean^2)/(input$rhosd^2),ff=ff))
    }
  })
  
  # Plot prior distribution on rho
  output$plotpriorrho = renderPlot(
    if(input$rhodisttype!="-")
      ggplot(rhodistr()$ff) +
      geom_ribbon(aes(x=x,ymax=prob),ymin=0,fill="grey",alpha=0.6) +
      geom_line(aes(x=x,y=prob),size=.7) +
      labs(x=expression(rho),y="PDF") +
      theme_bw()
  )
  
  # Run simulations
  output$glimpse <- eventReactive(input$gosim, {
    source("runmodel.R")
    runmodel(data=exdata(),
             pop=popsize(),
             si=sidistr(),
             prior_r0=r0distr(),
             prior_rho=rhodistr(),
             nchains=input$nchains,nit=input$nit,nwarmup=input$nwarmup,nthin=input$nthin,
             n.eoo=input$n.eoo,w.eoo=input$w.eoo)
    HTML(markdown::markdownToHTML(knit('glimpse.Rmd', quiet = TRUE)))
  })
  
}