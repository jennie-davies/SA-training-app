#define the layout and content of the user interface
shinyUI(fluidPage(
  
  titlePanel("Seasonal adjustment training"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose an exercise:", 
                  choices = c("Exercise 1", "Exercise 2", "Exercise 3", "Exercise 4")),
      tags$hr(),
      tags$div(class="header",checked=NA,tags$b("Seasonal adjustment 1")),
      selectInput("Transformation1", "Transformation:", 
                  choices = c("Default", "None", "Log")),
      selectInput("Easter1", "Easter:", 
                  choices = c("Default", "None", "Easter[1]", "Easter[8]", "Easter[15]")),
      checkboxInput("TMA1D",tags$b("Default trend moving average"),value=TRUE),
      conditionalPanel(
        condition="input.TMA1D==false",
        numericInput("TMA1", "Trend moving average:", 
                     9,min=1,max=101,step=2)),
      selectInput("SMA1", "Seasonal moving average:",
                  choices = c("S3X1","S3X3","S3X5","S3X9","S3X15","Msr","Stable","X11Default"),selected="Msr"),
      textInput("SB1","Seasonal break:",value="", placeholder="year.period"),
      tags$hr(),
      tags$div(class="header",checked=NA,tags$b("Seasonal adjustment 2")),
      selectInput("Transformation2", "Transformation:", 
                  choices = c("Default", "None", "Log")),
      selectInput("Easter2", "Easter:", 
                  choices = c("Default", "None", "Easter[1]", "Easter[8]", "Easter[15]")),
      checkboxInput("TMA2D",tags$b("Default trend moving average"),value=TRUE),
      conditionalPanel(
        condition="input.TMA2D==false",
      numericInput("TMA2", "Trend moving average:", 
                   9,min=1,max=101,step=2)),
      selectInput("SMA2", "Seasonal moving average:",
                  choices = c("S3X1","S3X3","S3X5","S3X9","S3X15","Msr","Stable","X11Default"),selected="Msr"),
      textInput("SB2","Seasonal break:",value="", placeholder="year.period")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overlay graph", plotOutput("tsPlot")), 
        tabPanel("Trend", plotOutput("trendPlot")), 
        tabPanel("SI ratios", plotOutput("SIPlot1"), plotOutput("SIPlot2")),
        tabPanel("Default selections", tableOutput("defaults"))
      ) 
    )
  )
))
