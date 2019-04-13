library(shiny)

ui <- fluidPage(
  
  #Title
  titlePanel("Mo Money, Mo Problems"),

  flowLayout(
  #Sliders
      sliderInput(inputId = "num",
              label="Initial Amount",
              value=100,min=1,max=100000, pre = "$"),
      sliderInput(inputId = "num",
              label="Annual Contribution",
              value=2000,min=0,max=50000, pre = "$"),
      sliderInput(inputId = "num",
              label="Return Rate (in %)",
              value=5,min=0,max=20, pre = "$"),
      sliderInput(inputId = "num",
              label="Growth Rate (in %)",
              value=2,min=0,max=20, pre = "$"),
      sliderInput(inputId = "num",
              label="Years",
              value=10,min=0,max=50, pre = "$"),
      selectInput("Facet?", "Facet?",
              c("Yes" = "yes",
                "No" = "no"))))

server <- function(input,output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}

shinyApp(server = server, ui = ui)
