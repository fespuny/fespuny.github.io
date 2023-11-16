#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
if( system.file( package="SplineHazardRegression") == "" ){
  library( devtools )
  install_github( "fespuny/SplineHazardRegression" )
  detach("package:devtools", unload = TRUE)
}
library( SplineHazardRegression )

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
                        value = 10),
            
            div( checkboxInput("spline", "Spline hazard estimator of incidence", FALSE ),
                 style="color:blue" ),
            sliderInput("nknots",
                        "Number of spline knots:",
                        min = 0,
                        max = 15,
                        value = 0 ),
            checkboxInput("automatic", "Automatic selection of spline knots", FALSE ),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabPanel( "Plots",
           plotOutput("distPlot"),
           fluidRow( 
             column( 12, p(" ") ) 
           ),
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
      nevents = length(Xperm)-ncens
      
      # generate 1 minute bins from ui.R
      bins <- seq(min(Xperm), max(Xperm), length.out = max(Xperm)-min(Xperm)+ 1 )
      x    <- Xperm[ -(1:ncens) ]
      hist_uncens <- hist( Xperm, bins, plot=FALSE )
      hist_data <- hist( x, bins, plot=FALSE )
      hist_cens <- hist( Xcens[1:ncens], bins, plot=FALSE )
      hist_cens$counts = hist_cens$counts / ncens
      hist_uncens$counts = hist_uncens$counts / nevents
      hist_data$counts = hist_data$counts / length(Xperm)
      
      op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
      
      # draw the histogram with the specified number of bins
      hist(Xperm, breaks = bins, col = 'gray90', border = 'white', probability=T,
           xlab = 'Waiting time to next eruption (in mins)', main="Censored count data")
      points( hist_data$breaks[-1], hist_data$counts, col="gray10", pch=10 )
#      points( hist_cens$breaks[-1][ hist_cens$counts > 0 ], hist_cens$counts[ hist_cens$counts > 0 ], col="red", pch=10 )
      yy = - max( hist_uncens$counts ) / 4
      plotrix::addtable2plot( 40, yy, table = 
                                matrix( c("No eruption", "Eruption", "Censored"), nrow=3, dimnames=list(c("","",""),c("Numbers at risk")) ), xjust=0.5 )
      for( xx in c(43, seq(50,90,10), 96) ){
        plotrix::addtable2plot( xx, yy, table = 
                                  matrix( c(sum(x>xx)+sum(Xcens[1:ncens]>xx), sum(x<=xx), sum(Xcens[1:ncens]<=xx) ), nrow=3, dimnames=list(c("","",""),c("")) ), xjust=0.5 )
      }
      
      # spline regression of censored time-to-event data using input$nknots
      if( input$spline ){
        eventtimes = sort( x )
        tte_data <- data.frame( time = c( x , Xcens[1:ncens] ),
                            status = c( rep(1,nevents), rep(0,ncens) ) )
        
        Exterior.knots = c( min(Xperm), max(Xperm) )
        K = input$nknots - 2 #number of interior knots
        if( K >0 & !input$automatic ){
          Interior.knots = approx( 1:nevents, eventtimes, 
                                 seq( nevents/(K+1), nevents/(K+1) * K , nevents/(K+1) ), method="linear" , na.rm=FALSE )$y
        } else {
          Interior.knots = NULL  
        }
        xout = seq(Exterior.knots[1],Exterior.knots[2],length.out=201)
        
        Result = hspcore( yd=tte_data, Exterior.knots=Exterior.knots, Interior.knots=Interior.knots, 
                          SelectBestKnots = input$automatic, time=xout, Bootstrap = 0, verbose=FALSE )
        
        abline(h= c(0.0,0.01,0.02,0.03,0.04,0.05), v=seq(50,90,10), lty=1,col="gray")
        lines( Result$t, Result$h[,1] * Result$S[,1], col="blue")
      }
      
      par(op)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
