library(shiny)
library(ggplot2)
library(cowplot)

fluidPage(
  
  titlePanel("Bora - Bayesian Outbreak Risk Assessment"),
  hr(),
  
  # 1 - Import epidemic curve
  sidebarLayout(position="left",
                sidebarPanel(h4("Epidemic curve"),br(),
                             fileInput("epidcurve","Select CSV file",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                             selectInput("exdata","or use a provided example",choices=c("-","ZIKV Martinique 2016","ZIKV Guadeloupe 2016"))),
                mainPanel(column(10,plotOutput(outputId="plotepidcurve",width="400px",heigh="200px")))
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Distribution of the serial interval"),br(),
                             selectInput("sidisttype","Select a distribution",choices=c("-","Normal","Log-normal","Gamma","Uniform")),
                             conditionalPanel(condition="output.siunif==true",
                                              numericInput("simin","Minimum (in weeks)",min=0,max=15,value=NA,step=0.1),
                                              numericInput("simax","Maximum (in weeks)",min=0,max=15,value=NA,step=0.1)),
                             conditionalPanel(condition="output.simeansd==true",
                                              numericInput("simean","Mean (in weeks)",min=0,max=15,value=NA,step=0.1),
                                              numericInput("sisd","Standard deviation (in weeks)",min=0,max=15,value=NA,step=0.1)),
                             selectInput("exsi","or use a provided example",choices=c("-","Zika virus (Ae. aegypti, 28°C)","Chikungunya virus (Ae. aegypti, 28°C)"))
                ),
                mainPanel(column(10,plotOutput(outputId="plotsi",width="400px",heigh="200px"))
                )
  ),
  sidebarLayout(position="left",
                sidebarPanel(h4("Prior information"),
                             h4("- on the reproduction ratio"),
                             selectInput("r0disttype","Distribution type",choices=c("","Non-informative","Log-normal","Gamma","Uniform")),
                             conditionalPanel(condition="output.r0unif==true",
                                              numericInput("r0min","Minimum",min=0,max=15,value=NA,step=.1),
                                              numericInput("r0max","Maximum",min=0,max=15,value=NA,step=.1)),
                             conditionalPanel(condition="output.r0meansd==true",
                                              numericInput("r0mean","Mean",min=0,max=15,value=NA,step=.1),
                                              numericInput("r0sd","Standard deviation",min=0,max=15,value=NA,step=.1)),
                             br(),
                             h4("- on the reporting rate"),
                             selectInput("rhodisttype","Distribution type",choices=c("","Non-informative","Beta","Uniform")),
                             numericInput("rhomean","Mean (in weeks)",min=0,max=1,value=NA,step=0.05),
                             numericInput("rhosd","Standard deviation (in weeks)      ",min=0,max=1,value=NA,step=0.05)
                ),
                mainPanel(column(10,plotOutput(outputId="plotprior",width="400px",heigh="200px"))
                )
  )
)