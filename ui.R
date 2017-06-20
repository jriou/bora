library(shiny)
library(ggplot2)
library(cowplot)
library(rmarkdown)

fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          tags$h2("Borat - Bayesian Outbreak Risk Assessment Tool"),
          p("Improving short- and middle-term forecasts about emerging epidemics by using historical data"),
          hr(),
          fluidRow(
            column(2, tags$h4("Import data")),
            column(2, tags$h4("Serial interval")),
            column(2, tags$h4("Prior distribution for the reproduction number")),
            column(2, tags$h4("Prior distribution for the reporting rate")),
            column(2, tags$h4("Miscellaneous"))
          ),
          hr(),
          fluidRow(
            # Input data
            column(2,
                   fileInput("epidcurve","Select CSV file*",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                   p("*single-column spreadsheet with the weekly numbers of observed cases")
            ),
            # Input serial interval
            column(2,
                   selectInput("sidisttype","Distribution type",choices=c("-","Uniform","Normal","Gamma")),
                   conditionalPanel(condition="output.simeansd==true",
                                    numericInput("simean","Mean (in weeks)",min=0,max=15,value=NA,step=0.1),
                                    numericInput("sisd","Standard deviation (in weeks)",min=0,max=15,value=NA,step=0.1)),
                   conditionalPanel(condition="output.siunif==true",
                                    numericInput("simin","Minimum (in weeks)",min=0,max=15,value=NA,step=0.1),
                                    numericInput("simax","Maximum (in weeks)",min=0,max=15,value=NA,step=0.1))
            ),
            # Input prior on R0
            column(2,
                   selectInput("r0disttype","Distribution type",choices=c("-","Uniform","Exponential","Normal","Gamma")),
                   conditionalPanel(condition="output.r0meansd==true",
                                    numericInput("r0mean","Mean",min=0,max=15,value=NA,step=0.1),
                                    numericInput("r0sd","Standard deviation",min=0,max=15,value=NA,step=0.1)),
                   conditionalPanel(condition="output.r0unif==true",
                                    numericInput("r0min","Minimum",min=0,max=15,value=NA,step=0.1),
                                    numericInput("r0max","Maximum",min=0,max=15,value=NA,step=0.1)),
                   conditionalPanel(condition="output.r0exp==true",
                                    numericInput("r0rate","Rate",min=0,max=15,value=NA,step=0.1))
            ),
            # Input prior on rho
            column(2,selectInput("rhodisttype","Distribution type",choices=c("-","Uniform","Beta")),
                   conditionalPanel(condition="output.rhounif==true",
                                    numericInput("rhomin","Minimum",min=0,max=1,value=NA,step=0.05),
                                    numericInput("rhomax","Maximum",min=0,max=1,value=NA,step=0.05)),
                   conditionalPanel(condition="output.rhobetad==true",
                                    numericInput("rhoshape1","Shape 1 (alpha)",min=0,max=10000,value=NA,step=1),
                                    numericInput("rhoshape2","Shape 2 (beta)",min=0,max=10000,value=NA,step=1))
            ),
            # Input misc
            column(2,
                   numericInput("popsize","Size of exposed population",min=0,value=NA),
                   numericInput("n.eoo","Threshold defining the end of the epidemic",value=200),
                   numericInput("w.eoo","Number of weeks above the threshold",value=3)
            )
          ),
          br(),
          # plot inputs
          fluidRow(
            column(2, 
                   plotOutput(outputId="plotepidcurve",height="150px",width="90%")
            ),
            column(2, 
                   plotOutput(outputId="plotsi",height="150px",width="90%")
            ),
            column(2, 
                   plotOutput(outputId="plotpriorr0",height="150px",width="90%")
            ),
            column(2, 
                   plotOutput(outputId="plotpriorrho",height="150px",width="90%")
            )
          ),
          br(),
          # provided examples
          fluidRow(
            column(2, 
                   selectInput("exdata","or use a prespecified example",choices=c("-","Zika virus in Martinique, 2015-2017 (W1-8)"))
            ),
            column(2, 
                   selectInput("exsi","or use a prespecified example",choices=c("-","Zika virus (Ae. aegypti, 28°C)","Chikungunya virus (Ae. aegypti, 28°C)"))
            ),
            column(2, 
                   selectInput("exr0","or use a prespecified example",choices=c("-","Non-informative prior distribution","CHIKV outbreak in Martinique (2013-2015) x difference between ZIKV and CHIKV outbreaks in French Polynesia (2013-2015)"))
            ),
            column(2, 
                   selectInput("exrho","or use a prespecified example",choices=c("-","Non-informative prior distribution","CHIKV outbreak in Martinique (2013-2015) x difference between ZIKV and CHIKV outbreaks in French Polynesia (2013-2015)"))
            )
          ),
          hr(),
          fluidRow(
            column(2, tags$h4("Procedure controls"))
          ),
          hr(),
          fluidRow(
            column(2, 
                   sliderInput("nchains","Number of chains",min=1,max=8,value=4,step=1)
            ),
            column(2, 
                   sliderInput("nit","Number of iterations by chain",min=0,max=10000,value=2000,step=100)
            ),
            column(2, 
                   sliderInput("nwarmup","Number of warmup iterations by chain",min=0,max=5000,value=500,step=50)
            ),
            column(2, 
                   sliderInput("nthin","Thinning ratio",min=1,max=10,value=1,step=1)
            ),
            column(2,align="center",
                   actionButton("gosim","Launch estimation",icon("random"))
            )
          ),
          hr(),
          fluidRow(
            column(10,tags$h4("Results"))
          ),
          hr(),
          fluidRow(
            column(10,includeHTML("report.html"))
          )
)