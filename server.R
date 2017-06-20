library(shiny)
library(dplyr)
library(ggplot2)
library(cowplot)
library(xtable)

function(input, output, session) {
  
  # Data management on imported file
  csvdata = reactive({
    req(input$epidcurve)
    x = unlist(unname(read.csv(input$epidcurve$datapath)))
    data.frame(NCASES=x,NWEEK=1:length(x))
  })
  
  exdata = reactive({
    req(input$exdata)
    if(input$exdata=="Zika virus in Martinique, 2015-2017 (W1-8)") data.frame(NCASES=c(57,119,145,456,619,1069,1629,1183),NWEEK=1:8)
  })
  
  popsize = reactive({
    req(input$popsize)
    input$popsize
  })
  
  datew1 <- reactive({
    req(input$datew1)
    input$datew1
  })
  
  observe({
    if(input$exdata=="Zika virus in Martinique, 2015-2017 (W1-8)") {
      updateNumericInput(session,"popsize",value=385000)
      updateDateInput(session,"datew1",value=as.Date("2015-12-20"))
    }
  })
  
  ec = reactive({
    if(!is.null(exdata())) {
      exdata()
    } else {
      csvdata()
    }
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
      labs(x="Weeks",y="Density") +
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
    if(input$exr0=="Non-informative prior distribution") {
      updateSelectInput(session,"r0disttype",selected="Exponential")
      updateNumericInput(session,"r0rate",value=0.5)
    }
    if(input$exr0=="CHIKV outbreak in Martinique (2013-2015) x difference between ZIKV and CHIKV outbreaks in French Polynesia (2013-2015)") {
      updateSelectInput(session,"r0disttype",selected="Gamma")
      updateNumericInput(session,"r0mean",value=1.3)
      updateNumericInput(session,"r0sd",value=0.1)
    }
  })
  
  
  # Management of prior on r0
  r0distr = reactive({
    req(input$r0disttype)
    if(input$r0disttype=="Uniform") {
      req(input$r0min,input$r0max)
      ff = data.frame(x=seq(0,input$r0max*1.1,by=0.001))
      ff$prob = dunif(ff$x,min=input$r0min,max=input$r0max)
      return(list(type="Uniform",min=input$r0min,max=input$r0max,ff=ff))
    }
    if(input$r0disttype=="Exponential") {
      req(input$r0rate)
      ff = data.frame(x=seq(0,2.5/input$r0rate,by=0.001))
      ff$prob = dexp(ff$x,rate=input$r0rate)
      return(list(type="Exponential",rate=input$r0rate,ff=ff))
    }
    if(input$r0disttype=="Normal") {
      req(input$r0mean,input$r0sd)
      ff = data.frame(x=seq(0,input$r0mean+5*input$r0sd,by=0.001))
      ff$prob = dnorm(ff$x,mean=input$r0mean,sd=input$r0sd)
      ff$prob = ff$prob/sum(ff$prob)
      return(list(type="Normal",mean=input$r0mean,sd=input$r0sd,ff=ff))
    }
    if(input$r0disttype=="Gamma") {
      req(input$r0mean,input$r0sd)
      ff = data.frame(x=seq(0,input$r0mean+5*input$r0sd,by=0.001))
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
      labs(x=expression(R[0]),y="Density") +
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
  
  observe({
    if(input$exrho=="Non-informative prior distribution") {
      updateSelectInput(session,"rhodisttype",selected="Beta")
      updateNumericInput(session,"rhoshape1",value=1)
      updateNumericInput(session,"rhoshape2",value=1)
    }
    if(input$exrho=="CHIKV outbreak in Martinique (2013-2015) x difference between ZIKV and CHIKV outbreaks in French Polynesia (2013-2015)") {
      updateSelectInput(session,"rhodisttype",selected="Beta")
      updateNumericInput(session,"rhoshape1",value=32)
      updateNumericInput(session,"rhoshape2",value=132)
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
  })
  
  # Plot prior distribution on rho
  output$plotpriorrho = renderPlot(
    if(input$rhodisttype!="-")
      ggplot(rhodistr()$ff) +
      geom_ribbon(aes(x=x,ymax=prob),ymin=0,fill="grey",alpha=0.6) +
      geom_line(aes(x=x,y=prob),size=.7) +
      labs(x=expression(rho),y="Density") +
      theme_bw()
  )
  
  # Run simulations
  R_ <- eventReactive(input$gosim, {
    source("runmodel.R")
    runmodel(data=exdata(),
             pop=popsize(),
             si=sidistr(),
             prior_r0=r0distr(),
             prior_rho=rhodistr(),
             nchains=input$nchains,nit=input$nit,nwarmup=input$nwarmup,nthin=input$nthin,
             n.eoo=input$n.eoo,w.eoo=input$w.eoo)
    
  })
  
  output$plotconv = renderPlot({
    req(R_())
    R_()$conv
  })
  
  output$plotfit = renderPlot({
    req(R_())
    ggplot() +
      geom_col(data=R_()$data,aes(x=NWEEK,y=NCASES),fill=NA,colour="black",width=1) +
      geom_ribbon(data=R_()$R_fit,aes(x=NWEEK,ymin=`25%`,ymax=`75%`),alpha=0.3) +
      geom_ribbon(data=R_()$R_fit,aes(x=NWEEK,ymin=`2.5%`,ymax=`97.5%`),alpha=0.3) +
      geom_line(data=R_()$R_fit,aes(x=NWEEK,y=mean),size=1) +
      labs(title="Model fit",x="Weeks",y="N") +
      theme_bw()
  })
  
  output$plotpost = renderPlot({
    require(cowplot)
    req(R_(),rhodistr(),r0distr())
    dd = rbind(cbind(as.data.frame(do.call("cbind",R_()$R_dens_r0[c("x","y")])),type="R0"),
               cbind(as.data.frame(do.call("cbind",R_()$R_dens_rho[c("x","y")])),type="rho"))
    ff = rbind(cbind(r0distr()$ff,type="R0"),cbind(rhodistr()$ff,type="rho"))
    ggplot() +
      geom_ribbon(data=dd,aes(x=x,ymax=y,ymin=0),alpha=0.7,fill="tomato") +
      geom_line(data=dd,aes(x=x,y=y),size=.7) +
      geom_line(data=ff,aes(x=x,y=prob),size=.7,linetype=2) +
      facet_wrap(~type,scales="free") +
      labs(title="Posterior distributions of the main parameters (compared to priors)",x="",y="Density") +
      theme_bw()
  })
  

  output$tablepost = renderUI({
    req(R_())
    ressum = function(x,digits) {
      c(mean=format(round(x[1],digits),big.mark=",",nsmall=digits),
        CI50=paste0("[",format(round(x[5],digits),big.mark=",",nsmall=digits),"; ",format(round(x[7],digits),big.mark=",",nsmall=digits),"]"),
        CI95=paste0("[",format(round(x[4],digits),big.mark=",",nsmall=digits),"; ",format(round(x[8],digits),big.mark=",",nsmall=digits),"]"),
        n_eff=format(round(x[9]),big.mark=",",nsmall=0),
        Rhat=format(round(x[10],2),big.mark=",",nsmall=2))
    }
    ss = cbind(c("R_0","\\rho","\\phi"),
               rbind(ressum(R_()$R_summarypars[1,],1),
                     ressum(R_()$R_summarypars[2,],2),
                     ressum(R_()$R_summarypars[3,],1)))
    dimnames(ss) = list(1:3,
                        c("Parameter","Mean","50\\%CI","95\\%CI","Eff. N","\\hat{R}"))
    ss = print(xtable(ss,
               align=rep("l", ncol(ss)+1),
               captio=paste("lol")),
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE,
               sanitize.text.function=function(x) x ,
               sanitize.colnames.function=function(x) x,
               include.rownames=FALSE,
               caption.placement = getOption("xtable.caption.placement", "top"), 
               caption.width = getOption("xtable.caption.width", NULL))
    list(withMathJax(HTML(ss)))
    })
  
  
  output$plotpred = renderPlot({
    req(R_())
    pathee = rbind(tail(R_()$R_fit,1),R_()$R_pred)
    linee = filter(pathee,NWEEK==min(NWEEK))
    ggplot() +
      geom_col(data=R_()$data,aes(x=NWEEK,y=NCASES),fill=NA,colour="black",width=1) +
      geom_ribbon(data=R_()$R_fit,aes(x=NWEEK,ymin=`25%`,ymax=`75%`),alpha=0.3) +
      geom_ribbon(data=R_()$R_fit,aes(x=NWEEK,ymin=`2.5%`,ymax=`97.5%`),alpha=0.3) +
      geom_line(data=R_()$R_fit,aes(x=NWEEK,y=mean),size=1) +
      
      geom_ribbon(data=pathee,aes(x=NWEEK,ymin=`25%`,ymax=`75%`),alpha=0.3,fill="tomato") +
      geom_ribbon(data=pathee,aes(x=NWEEK,ymin=`2.5%`,ymax=`97.5%`),alpha=0.3,fill="tomato") +
      geom_line(data=pathee,aes(x=NWEEK,y=mean),size=1,colour="tomato") +
      
      geom_vline(data=linee,aes(xintercept=NWEEK),size=0.3,linetype=2) +
      labs(title="Model fit and predicted course of the epidemic",x="Weeks",y="N") +
      theme_bw()
  })
  
  output$tablepred = renderUI({
    req(R_())
    ressum = function(x,digits) {
      c(mean=format(round(x[1],digits),big.mark=",",nsmall=digits),
        CI50=paste0("[",format(round(x[5],digits),big.mark=",",nsmall=digits),"; ",format(round(x[7],digits),big.mark=",",nsmall=digits),"]"),
        CI95=paste0("[",format(round(x[4],digits),big.mark=",",nsmall=digits),"; ",format(round(x[8],digits),big.mark=",",nsmall=digits),"]")
        )
    }

    ss = cbind(c("Total \\;observed \\;cases","Total overall cases","Final attack rate"),
               rbind(ressum(R_()$R_summarypred[1,],0),
                     ressum(R_()$R_summarypred[2,],0),
                     ressum(R_()$R_summarypred[3,],2),
                     ressum(R_()$R_start,0),
                     ressum(R_()$R_peak,0),
                     ressum(R_()$R_end,0)
                     )
               )
    dimnames(ss) = list(1:nrow(ss),
                        c("Parameter","Mean","50\\%CI","95\\%CI"))
    ss = print(xtable(ss,
                      align=rep("l", ncol(ss)+1)),
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE,
               #sanitize.text.function=function(x) x ,
               sanitize.colnames.function=function(x) x,
               include.rownames=FALSE)
    list(withMathJax(HTML(ss)))
  })
}