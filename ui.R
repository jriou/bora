library(shiny)
library(ggplot2)
library(cowplot)

fluidPage(
  
  titlePanel("Bora - Bayesian Outbreak Risk Assessment"),
  hr(),
  
  # 1 - Import epidemic curve
  sidebarLayout(position="left",
                sidebarPanel(h4("Epidemic data"),br(),
                             fileInput("epidcurve","Select CSV file",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                             selectInput("exdata","or use a provided example",choices=c("-","Zika virus in Martinique, 2015-2017 (W1-8)"))),
                mainPanel(column(10,plotOutput(outputId="plotepidcurve",width="400px",heigh="200px")))
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Distribution of the serial interval"),br(),
                             selectInput("sidisttype","Distribution type",choices=c("-","Uniform","Normal","Gamma")),
                             conditionalPanel(condition="output.simeansd==true",
                                              numericInput("simean","Mean (in weeks)",min=0,max=15,value=NA,step=0.1),
                                              numericInput("sisd","Standard deviation (in weeks)",min=0,max=15,value=NA,step=0.1)),
                             conditionalPanel(condition="output.siunif==true",
                                              numericInput("simin","Minimum (in weeks)",min=0,max=15,value=NA,step=0.1),
                                              numericInput("simax","Maximum (in weeks)",min=0,max=15,value=NA,step=0.1)),
                             selectInput("exsi","or use a provided example",choices=c("-","Zika virus (Ae. aegypti, 28°C)","Chikungunya virus (Ae. aegypti, 28°C)"))
                ),
                mainPanel(column(10,plotOutput(outputId="plotsi",width="400px",heigh="200px"))
                )
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Prior distribution on the reproduction number"),
                             selectInput("r0disttype","Distribution type",choices=c("-","Uniform","Exponential","Normal","Gamma")),
                             conditionalPanel(condition="output.r0meansd==true",
                                              numericInput("r0mean","Mean",min=0,max=15,value=NA,step=0.1),
                                              numericInput("r0sd","Standard deviation",min=0,max=15,value=NA,step=0.1)),
                             conditionalPanel(condition="output.r0unif==true",
                                              numericInput("r0min","Minimum",min=0,max=15,value=NA,step=0.1),
                                              numericInput("r0max","Maximum",min=0,max=15,value=NA,step=0.1)),
                             conditionalPanel(condition="output.r0exp==true",
                                              numericInput("r0rate","Rate",min=0,max=15,value=NA,step=0.1)),
                             selectInput("exr0","or use a provided example",choices=c("-","Non-informative prior distribution","Chikungunya virus in Martinique, 2013-2015"))
                ),
                mainPanel(column(10,plotOutput(outputId="plotpriorr0",width="400px",heigh="200px"))
                )
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Prior distribution on the reporting rate"),
                             selectInput("rhodisttype","Distribution type",choices=c("-","Uniform","Beta")),
                             conditionalPanel(condition="output.rhounif==true",
                                              numericInput("rhomin","Minimum",min=0,max=1,value=NA,step=0.05),
                                              numericInput("rhomax","Maximum",min=0,max=1,value=NA,step=0.05)),
                             conditionalPanel(condition="output.rhobetad==true",
                                              numericInput("rhoshape1","Shape 1 (alpha)",min=0,max=10000,value=NA,step=1),
                                              numericInput("rhoshape2","Shape 2 (beta)",min=0,max=10000,value=NA,step=1)),
                             selectInput("exrho","or use a provided example",choices=c("-","Non-informative prior distribution","Chikungunya virus in Martinique, 2013-2015"))
                ),
                mainPanel(column(10,plotOutput(outputId="plotpriorrho",width="400px",heigh="200px"))
                )
  ),
  
  
  
  br(),br(),br()
)