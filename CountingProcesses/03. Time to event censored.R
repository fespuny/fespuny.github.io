#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
x    <- faithful[, 2]
set.seed( 2023 )
Xperm <- sample( x )
Xcens <- sapply( Xperm, function(xx) { floor(runif(1, min=min(x),max=xx ) ) } ) #amount of time we observe

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Censored Old Faithful Geyser Data (n=272)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pcens",
                        "Percentage of censoring:",
                        min = 1,
                        max = 100,
                        value = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabPanel( "Plots",
           plotOutput("distPlot"),
           fluidRow( 
             column( 12, p(" ") ) 
           ),
           plotOutput("distPlot2"),
           fluidRow( 
             column( 12, p(" ") )
           )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      # generate observation times using input$pcens
      ncens = floor( length(Xperm) * input$pcens / 100 )
      
      # generate 1 minute bins from ui.R
      bins <- seq(min(Xperm), max(Xperm), length.out = max(Xperm)-min(Xperm)+ 1 )
      x    <- Xperm[ -(1:ncens) ]
      hist_uncens <- hist( Xperm, bins, plot=FALSE )
      hist_data <- hist( x, bins, plot=FALSE )
      hist_cens <- hist( Xcens[1:ncens], bins, plot=FALSE )
      
      op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
      
      # draw the histogram with the specified number of bins
      hist(Xperm, breaks = bins, col = 'gray97', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)', main="Censored count data")
      points( hist_data$breaks[-1], hist_data$counts, col="gray10", pch=10 )
      points( hist_cens$breaks[-1][ hist_cens$counts > 0 ], hist_cens$counts[ hist_cens$counts > 0 ], col="red", pch=10 )
      yy = - max( hist_uncens$counts ) / 1.9
      plotrix::addtable2plot( 40, yy, table = 
                                matrix( c("No eruption", "Eruption", "Censored"), nrow=3, dimnames=list(c("","",""),c("Numbers at risk")) ), xjust=0.5 )
      for( xx in c(43, seq(50,90,10), 96) ){
        plotrix::addtable2plot( xx, yy, table = 
                                  matrix( c(sum(x>xx)+sum(Xcens[1:ncens]>xx), sum(x<=xx), sum(Xcens[1:ncens]<=xx) ), nrow=3, dimnames=list(c("","",""),c("")) ), xjust=0.5 )
      }
      
      par(op)
    })
    
    output$distPlot2 <- renderPlot({
      # generate observation times using input$pcens
      ncens = floor( length(Xperm) * input$pcens / 100 )
      
      # generate 1 minute bins from ui.R
      bins <- seq(min(Xperm), max(Xperm), length.out = max(Xperm)-min(Xperm)+ 1 )
      x    <- Xperm[ -(1:ncens) ]
      hist_uncens <- hist( Xperm, bins, plot=FALSE )
      hist_data <- hist( x, bins, plot=FALSE )
      hist_cens <- hist( Xcens[1:ncens], bins, plot=FALSE )

      hist_uncens$counts = cumsum( hist_uncens$counts)
      hist_data$counts = cumsum( hist_data$counts)
      hist_cens$counts = cumsum( hist_cens$counts)

      op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
      
      # draw the histogram with the specified number of bins
      plot( hist_uncens$breaks[-1], hist_uncens$counts, col="gray10", type="s",
            xlim = c(min(Xperm),max(Xperm)), 
            xlab = 'Waiting time to next eruption (in mins)',
            ylab = 'Cumulative incidence',
            main = "" ) #'Cumulative counts (Actual versus observed)')
      title( expression( bold( "Cumulative counts (Actual versus " * phantom( "observed" ) * ")" ) ) )
      title( expression( bold( phantom( "Cumulative counts (actual versus " ) * "observed" * phantom(")") ) ), col.main = "red")
      lines( hist_uncens$breaks[-1], hist_uncens$counts, col="gray10", type="s" )
      lines( hist_data$breaks[-1], hist_data$counts, col="red", type="s" )
      plotrix::addtable2plot( 40, -145, table = 
                                matrix( c("No eruption", "Eruption", "Censored"), nrow=3, dimnames=list(c("","",""),c("Numbers at risk")) ), xjust=0.5 )
      for( xx in c(43, seq(50,90,10), 96) ){
        plotrix::addtable2plot( xx, -145, table = 
                                  matrix( c(sum(x>xx)+sum(Xcens[1:ncens]>xx), sum(x<=xx), sum(Xcens[1:ncens]<=xx) ), nrow=3, dimnames=list(c("","",""),c("")) ), xjust=0.5 )
      }

      par(op)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
