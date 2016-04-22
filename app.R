#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(boot)
library(ggplot2)

bootFun = function(data) {
  s = sum(data)
  n = length(data)
  tt = s/n
  return(tt)
}

bootRan = function(data, n) {
  s = sample(data, n, replace = TRUE)
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

   # Application title
   h1("Agile Forecasting"),
   h4("Simulation of working days Needed to complete a number of stories basing on historical data."),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        sliderInput("legsNumber", "Number of Legs", min=1, max=5, value=1, ticks = FALSE),
        helpText("Select the number of legs."),
        helpText("Then insert a sample of cycle times (working days), separated by commas:, e.g. 1,2,3,4,5,6,7,8,9,10."),
        helpText("Then insert the N° of stories to simulate."),
        uiOutput("leg_ui"),
        actionButton("go", "Run")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        h3("Simulations Length Histogram", align = "center"),
        plotOutput("hist", width = "auto", height = "600"),
        div(
          h5("Options"),
          fluidRow(
            column(4,
              textInput("percentiles", label = "Percentiles", value = ".25, .5, .75", ),
              helpText("Insert a list of percentile to show in the graph separated by commas: e.g. .25, .5, .75. They will be printed on the histogram.")
            ),
            column(4,
              numericInput("predictions", label = "N° of Bootstrap Predictions", value = "500", step = "100", max = "1000", min = "100")
            ),
            column(4,
              numericInput("runs", label = "N° of Montecarlo Runs", value = "25000", step = "1000", min = "10000", max = "50000")
            )
          )
        )
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  legCounter = reactiveValues(n = 1)

  observeEvent(input$legsNumber, {legCounter$n = input$legsNumber})

  legSamples = c()
  legN = c()
  legBoots = c()
  legMontecarlos = c()

  legMontecarlosSum = eventReactive(input$go, {
    if (legCounter$n >= 1) {
      for (i in seq_len(legCounter$n)) {
        legSamples[[as.character(i)]] = as.numeric(unlist(strsplit(input[[paste0("legSample", i)]], ",")))
        legN[[as.character(i)]] = as.integer(input[[paste0("legN", i)]])
        legBoots[[as.character(i)]] = boot(legSamples[[as.character(i)]], bootFun, ran.gen = bootRan, mle = 10, sim = "parametric", R = isolate(input$predictions))
        legMontecarlos[[as.character(i)]] = sample(legBoots[[as.character(i)]]$t*legN[[as.character(i)]], size = isolate(input$runs), replace = TRUE)
      }
    }

    for (i in seq_len(legCounter$n)) {
      if (i == 1) {
        legMontecarlosSum = legMontecarlos[[as.character(i)]]
      } else {
        legMontecarlosSum = legMontecarlosSum + legMontecarlos[[as.character(i)]]
      }
    }

    return(legMontecarlosSum)
  })

  legBoxes = eventReactive(input$legsNumber, {
    n = legCounter$n

    if (n >= 0) {

        lapply(seq_len(n), function(i) {
          div(
            h4(paste0("Leg ", i)),
            fluidRow(
              column(9,
              textInput(
                inputId = paste0("legSample", i),
                label = paste0("Cycle Times:"),
                value = input[[paste0("legSample", i)]]
              )),
              column(3,
              numericInput(
                inputId = paste0("legN", i),
                label = paste0("N°:"),
                value = if (is.null(input[[paste0("legN", i)]])) 10 else input[[paste0("legN", i)]]
              )
              )
            )
          )
        })
    }
  })

  output$leg_ui <- renderUI({ legBoxes() })

  output$hist = renderPlot({
    histData = legMontecarlosSum()
    probs = as.numeric(unlist(strsplit(isolate(input[["percentiles"]]), ",")))
    quantiles = quantile(histData, prob=probs)

    ggplot() +
      aes(histData) +
      geom_histogram(aes(y =..density..),
                     binwidth = 1,
                     col="black",
                     fill="white") +
      geom_line(aes(quantiles), x = quantiles, color="red") +
      scale_x_continuous(breaks=quantiles, labels=quantiles) +
      labs(x="Working Days", y="Count")
  })
})

# Run the application
shinyApp(ui = ui, server = server)
