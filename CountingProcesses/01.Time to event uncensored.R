#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel( title = div( "Old Faithful Geyser Data (n=272)", 
                             img( src="maps.jpg", align="right", height = "120"),
                             img( src="Geyser.jpg", align="right", height = "120") ) ),
    
    
    h3("Description"),

    p("Waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park, Wyoming, USA."),

    fluidRow( 
      column( 12, p(" ") ),
      column( 12, p(" ") ) 
    ),

    fluidRow(

    column( 4,
        wellPanel(
          
          p( "Summary of waiting times (minutes)"),
          
          verbatimTextOutput("summary"),
          
          sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 96-43,
                        value = 96-43 )
        ) ),

      # Show a plot of the generated distribution
      column( 8,
          
          tabPanel( "Plots", plotOutput("distPlot")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$summary <- renderPrint( {
      summary( faithful$waiting )
    } )

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of time to next eruption')
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
