library(shiny)
library(ggplot2)
library(cowplot)

fluidPage(
  
  titlePanel("Bora - Bayesian Outbreak Risk Assessment"),
  hr(),
  
  # 1 - Import epidemic curve
  sidebarLayout(position="left",
                sidebarPanel(h4("Epidemic data"),br(),
                             fileInput("epidcurve","Select CSV file*",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                             selectInput("exdata","or use a provided example",choices=c("-","Zika virus in Martinique, 2015-2017 (W1-8)")),
                             numericInput("popsize","Size of exposed population",min=0,value=NA),
                             p("*single-column spreadsheet with the weekly numbers of observed cases")),
                
                mainPanel(column(10,plotOutput(outputId="plotepidcurve",width="700px",heigh="350px")))
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
                mainPanel(column(10,plotOutput(outputId="plotsi",width="700px",heigh="350px"))
                )
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Prior distribution on the reproduction number"),br(),
                             selectInput("r0disttype","Distribution type",choices=c("-","Uniform","Exponential","Normal","Gamma")),
                             conditionalPanel(condition="output.r0meansd==true",
                                              numericInput("r0mean","Mean",min=0,max=15,value=NA,step=0.1),
                                              numericInput("r0sd","Standard deviation",min=0,max=15,value=NA,step=0.1)),
                             conditionalPanel(condition="output.r0unif==true",
                                              numericInput("r0min","Minimum",min=0,max=15,value=NA,step=0.1),
                                              numericInput("r0max","Maximum",min=0,max=15,value=NA,step=0.1)),
                             conditionalPanel(condition="output.r0exp==true",
                                              numericInput("r0rate","Rate",min=0,max=15,value=NA,step=0.1)),
                             selectInput("exr0","or use a provided example",choices=c("-","Non-informative prior distribution","Prior computed from the Chikungunya virus outbreak in Martinique (2013-2015) and both Chikungunya and Zika virus outbreaks in French Polynesia (2013-2015)"))
                ),
                mainPanel(column(10,plotOutput(outputId="plotpriorr0",width="700px",heigh="350px"))
                )
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Prior distribution on the reporting rate"),br(),
                             selectInput("rhodisttype","Distribution type",choices=c("-","Uniform","Beta")),
                             conditionalPanel(condition="output.rhounif==true",
                                              numericInput("rhomin","Minimum",min=0,max=1,value=NA,step=0.05),
                                              numericInput("rhomax","Maximum",min=0,max=1,value=NA,step=0.05)),
                             conditionalPanel(condition="output.rhobetad==true",
                                              numericInput("rhoshape1","Shape 1 (alpha)",min=0,max=10000,value=NA,step=1),
                                              numericInput("rhoshape2","Shape 2 (beta)",min=0,max=10000,value=NA,step=1)),
                             selectInput("exrho","or use a provided example",choices=c("-","Non-informative prior distribution","Prior computed from the Chikungunya virus outbreak in Martinique (2013-2015) and both Chikungunya and Zika virus outbreaks in French Polynesia (2013-2015)"))
                ),
                mainPanel(column(10,plotOutput(outputId="plotpriorrho",width="700px",heigh="350px"))
                )
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Estimation procedure"),br(),
                             checkboxGroupInput("parofint","Parameters of interest",
                                                choices=list("reproduction number"=1,
                                                             "reporting rate"=2,
                                                             "total number of observed cases"=3,
                                                             "attack rate"=4,
                                                             "date of epidemic start"=5,
                                                             "date of peak incidence"=6,
                                                             "date of outbreak end"=7),
                                                selected=1:7),
                             dateInput("datew1",h5("Date corresponding to week 1"),value=NA),
                             br(),
                             strong("Threshold for epidemic start and end"),
                             numericInput("n.eoo",h5("Number of weekly observed cases"),value=200),
                             numericInput("w.eoo",h5("Number of consecutive weeks above or below the threshold"),value=3),
                             br(),
                             strong("Procedure controls"),
                             numericInput("nchains",h5("Number of chains (default 4)"),min=1,value=4,step=1),
                             numericInput("nit",h5("Number of iterations by chain (default 2000)"),min=1,value=2000,step=1),
                             numericInput("nwarmup",h5("Number of warmup iterations by chain (default 500)"),min=1,value=500,step=1),
                             numericInput("nthin",h5("Thinning (default 1)"),min=1,value=1,step=1),
                             br(),br(),
                             actionButton("gosim","Launch estimation"),
                             p("This might take a while, please consider downloading the application and running it locally."),
                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                              p("Computation in progress...")
                             )
                             
                ),
                mainPanel(plotOutput(outputId="plotconv",width="700px",heigh="350px"),
                          plotOutput(outputId="plotpost",width="700px",heigh="350px"),
                          tableOutput('tablepost'),
                          plotOutput(outputId="plotfit",width="700px",heigh="350px"),
                          plotOutput(outputId="plotpred",width="700px",heigh="350px")
                          
                )
  ),
  br(),br(),br()
)