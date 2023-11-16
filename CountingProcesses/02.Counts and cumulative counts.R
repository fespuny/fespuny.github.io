#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 
library( splines )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data (n=272)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxInput("linear", "Linear Regression", FALSE ),
          checkboxInput("spline", "Spline Regression", FALSE ),
          checkboxInput("sspline", "Smooth spline Regression", FALSE ),
          sliderInput("nknots",
                        "Number of spline knots:",
                        min = 4,
                        max = (96-43)-1,
                        value = 5 ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabPanel( "Plots",
           plotOutput("dataPlot"),
           fluidRow( 
             column( 12, p(" ") ) 
           ),
           plotOutput("distPlot2")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$dataPlot <- renderPlot({
      # generate 1 minute length bins 
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = (max(x)-min(x)+1) )
      hist_data <- hist( x, bins, plot=FALSE )
      
      op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'gray97', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)', main="Count data")
      points( hist_data$breaks[-1], hist_data$counts, col="gray10", pch=10 )
      yy = - max( hist_data$counts ) / 2.2
      plotrix::addtable2plot( 40, yy, table = 
                                matrix( c("No eruption", "Eruption"), nrow=2, dimnames=list(c("",""),c("Numbers at risk")) ), xjust=0.5 )
      for( xx in c(43, seq(50,90,10), 96) ){
        plotrix::addtable2plot( xx, yy, table = 
                                  matrix( c(sum(x>xx), sum(x<=xx)), nrow=2, dimnames=list(c("",""),c("")) ), xjust=0.5 )
      }
      
      # linear approximation of count data
      if( input$linear ) abline( lm( hist_data$counts ~ hist_data$breaks[-1] ), col="red" )
        
      # spline regression of count data using input$nknots
      if( input$spline ){ 
        spline_fit <- lm( hist_data$counts ~ splines::bs(hist_data$breaks[-1],df=3+input$nknots) )
        xout = seq(min(hist_data$breaks[-1]),max(hist_data$breaks[-1]))
        preds = predict(spline_fit,newdata=list(x=xout) )
        lines( xout, preds, lty=2, col="blue" )
      }
      
      # smooth spline
      if( input$sspline ) lines( stats::smooth.spline( hist_data$breaks[-1], hist_data$counts, nknots = input$nknots ), col="blue" )
      
      par(op)
    })
    
    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = (max(x)-min(x)+1))
      hist_data <- hist( x, bins, plot=FALSE )
      hist_data$counts = cumsum( hist_data$counts)
      
      op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
      
      # draw the histogram with the specified number of bins
      plot( hist_data$breaks[-1], hist_data$counts, col="gray10", type="s",
            xlim = c(min(x),max(x)), 
            xlab = 'Waiting time to next eruption (in mins)',
            ylab = 'Cumulative frequency',
            main = 'Cumulative counts')
      plotrix::addtable2plot( 40, -100, table = 
                                matrix( c("No eruption", "Eruption"), nrow=2, dimnames=list(c("",""),c("Numbers at risk")) ), xjust=0.5 )
      for( xx in c(43, seq(50,90,10), 96) ){
        plotrix::addtable2plot( xx, -100, table = 
                                matrix( c(sum(x>xx), sum(x<=xx)), nrow=2, dimnames=list(c("",""),c("")) ), xjust=0.5 )
      }
      par(op)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
