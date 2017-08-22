library(shiny)
library(ggplot2)
library(cowplot)
library(rmarkdown)

source("examples_library.R")

fluidPage(includeCSS("www/simplex.css"),
          titlePanel("Epicast"),
          h5("Forecasting current epidemics using historical data"),
          hr(),
          fluidRow(
            column(2, tags$h3("Settings"))
          ),
          hr(),
          fluidRow(
            column(2, tags$h4("Import data")),
            column(2, tags$h4("Serial interval")),
            column(2, tags$h4("Prior distribution for the reproduction number")),
            column(2, tags$h4("Prior distribution for the reporting rate")),
            column(2, tags$h4("Other"))
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
            column(2,selectInput("rhodisttype","Distribution type",choices=c("-","Uniform","Beta","Gamma")),
                   conditionalPanel(condition="output.rhomeansd==true",
                                    numericInput("rhomean","Mean",min=0,max=1,value=NA,step=0.1),
                                    numericInput("rhosd","Standard deviation",min=0,max=1,value=NA,step=0.1)),
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
                   numericInput("n.eoo","Threshold defining high autochtonous transmission",value=NA)
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
                   selectInput("exdata","or use an example from the library",choices=c("-",lapply(data_lib,function(x) names(x))))
            ),
            column(2, 
                   selectInput("exsi","or use an example from the library",choices=c("-",lapply(si_lib,function(x) names(x))))
            ),
            column(2, 
                   selectInput("exr0","or use an example from the library",choices=c("-","Non-informative",lapply(r0_lib,function(x) names(x))))            
            ),
            column(2, 
                   selectInput("exrho","or use an example from the library",choices=c("-","Non-informative",lapply(rho_lib,function(x) names(x))))            
            )
          ),
          hr(),
          fluidRow(
            column(2, tags$h4("Advanced settings"))
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
            column(3,align="center",br(),
                   actionButton("gosim"," \tEstimation",icon("random")),
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    h6("Computation in progress..."))
            )
          ),
          hr(),
          tags$head(tags$style(HTML("
                               body {
                                    width: 100% !important;
                                    max-width: 100% !important;
                                    }
                                    
                                    "))),
          fluidRow(
            column(10,uiOutput(outputId="glimpse"))
          )
)