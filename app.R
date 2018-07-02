
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
read.csv("GLB.Ts.csv")
# Read Data
GlobalData <- read.csv("GLB.Ts.csv",stringsAsFactors = F)
NorthPoleData <- read.csv("NH.Ts.csv",stringsAsFactors = F)
SouthPoleData <- read.csv("SH.Ts.csv",stringsAsFactors = F)

#print(str(TemperatureData))

# Define UI for application that draws a plot
ui <- fluidPage(
   
   # Application title
   titlePanel(title = h3("GISS Surface Temperature Analysis", align="center")),
   br(),   br(),
   sidebarLayout(
           sidebarPanel(
                   #------------------------------------------------------------------
                   # Add radio button to choice for north pole or south pole data
                   radioButtons("poleInput", 
                                label = "Select Data: ",
                                choices = list("North Pole data" = 'NHem', "South Pole Data" = 'SHem', "Global Data" = 'Glob'),
                                selected = 'Glob'),
                   br(),   br(),
                   #------------------------------------------------------------------
                   # Add Variable for Year Selection
                   sliderInput("YearRange", "Select Year Range : ", min=1880, max=2016, value=c(1880, 1900), step=1
                   
                   ),
                   
                   br(),   br(),
                   #------------------------------------------------------------------
                   # Add Variables selection option from January to December : 
                   selectInput("var", "Select Variable from Dataset", 
                               choices=c("Jan"=2, "Feb"=3, "Mar"=4, "Apr"=5, 
                                         "May"=6, "Jun"=7, "Jul"=8, "Aug"=9, 
                                         "Sep"=10, "Oct"=11, "Nov"=12, "Dec"=13),
                               multiple=TRUE, selected = "Jan"
                               ),
                   
                   br(),   br()
                   #------------------------------------------------------------------
                   # Change background color for body
                   #tags$style("body{background-color:lightyellow; color:brown}")
           ),
           
           mainPanel(
                   # Create tab panes
             
                   tabsetPanel(type="tab",
                               tabPanel("Summary",verbatimTextOutput("sumry")),
                               tabPanel("Structure", verbatimTextOutput("struct")),
                               tabPanel("Data", tableOutput("displayData")),
                               tabPanel("Plot", plotOutput("mygraph"))
                   ),
                   h2("INSTRUCTIONS"),
                   h4(" - Select data options using radio buttons from North Pole, South Pole or Global Data."),
                   h4(" - Select Year Range using Slider Range. Data Range contains minimum year as 1880 and maximum year as 2016."),
                   h4(" - Select Variable from Dataset. This is a multi select text field which contains months from January to December.")
           )
   )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
                cols <- reactive({
                        as.numeric(c(input$var))
                        
                })
                mylabel <- reactive({
                        if(input$poleInput=='Glob'){
                                lable <- "Plot for Global Data"
                        }
                        if(input$poleInput=='NHem'){
                                lable <- "Plot for North Pole Data"
                        }
                        if(input$poleInput=='SHem'){
                                lable <- "Plot for South Pole Data"
                        }
                        lable
                })
        
                
                myFinalData <- reactive({
                        #------------------------------------------------------------------
                        # Select data according to selection of ratdio button
                        if(input$poleInput=='Glob'){
                                mydata <- GlobalData
                                
                        }
                        
                        if(input$poleInput=='NHem'){
                                mydata <- NorthPoleData
                        }
                        
                        if(input$poleInput=='SHem'){
                                mydata <- SouthPoleData
                        }
                        #------------------------------------------------------------------
                        # Get data rows for selected year
                        mydata1 <- mydata[mydata$Year >= input$YearRange[1], ] # From Year
                        mydata1 <- mydata1[mydata1$Year <= input$YearRange[2], ] # To Year
                        #------------------------------------------------------------------
                        # Get Data for selected months as variable
                        mydata2<- mydata1[, c(1, sort(cols()))]
                        #------------------------------------------------------------------
                        # Get data rows for selected year
                        data.frame(mydata2)
                        #------------------------------------------------------------------
                        
                })
                        
                # Prepare "Data tab"
                output$displayData <- renderTable({
                        myFinalData()
                })
                
                # Prepare Structure Tab
                renderstr <- reactive({ str(myFinalData())})
                
                output$struct <- renderPrint({
                        renderstr()
                })
                
                # Prepare Summary Tab
                rendersumry <- reactive({ summary(myFinalData())})
                
                output$sumry <- renderPrint({
                        rendersumry()
                })
                
                # Prepare Plot Tab
                output$mygraph <- renderPlot({
                        plotdata <- myFinalData()
                        plot(plotdata, col=c(1,2,3,4,5,6,7,8,9), main=mylabel())
                })
                
                  
}

        

# Run the application 
shinyApp(ui = ui, server = server)

