# Daniel del Carpio 
# Workout02 Shiny App

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Mo Money Mo Problems (A Savings Account Timeline)"),
  
  fluidRow(column(3, sliderInput("intial",
                                 "Initial Amount",
                                 min = 1,
                                 max = 100000,
                                 value = 1000,
                                 step = 500,
                                 pre = '$'),
                  sliderInput("annual",
                              "Annual Contribution",
                              min = 0,
                              max = 50000,
                              value = 2000,
                              step = 500,
                              pre='$'), offset = 2),
                  column(3, sliderInput("return",
                                 "Return Rate (in %)",
                                 min = 0,
                                 max = 20,
                                 value = 5,
                                 step = 0.1),
                  sliderInput("growth",
                              "Growth Rate (in %)",
                              min = 0,
                              max = 20,
                              value = 2,
                              step = 0.1)),
                  column(3, sliderInput("years",
                                 "Years",
                                 min = 0,
                                 max = 50,
                                 value = 10,
                                 step = 1),
                  selectInput("facet",
                              "Facet?",
                              c("No", "Yes")))),
  fluidRow(column(6, h4("Timelines"), offset = 2)),
  fluidRow(column(6, plotOutput("plot1"), offset = 2)),
  fluidRow(column(6, h4("Balances"), offset = 2)),
  fluidRow(column(6, verbatimTextOutput("balances_table"), offset = 2))
)


server <- function(input, output) {
  modalities <- reactive({
    future_value <-  function(amount, rate, years){
      return(amount*(1+rate)^years)
    }
    annuity <- function(contrib, rate, years){
      return(contrib *(((1+rate)^(years)-1)/(rate)))
    }
    growing_annuity <- function(contrib, rate, growth, years){
      return(contrib * (((1+rate)^(years)-(1+growth)^(years))/(rate-growth)))
    }
    years <- 0
    no_contrib <- 0
    fixed_contrib <- 0
    growing_contrib <- 0
    return <- input$return * .01
    growth <- input$growth * .01
    k <- input$years
    for(i in 0:k){
      years[i+1] <- i
      no_contrib[i+1] <- future_value(amount = input$intial, rate = return, years = i) 
      fixed_contrib[i+1] <- no_contrib[i+1] + annuity(contrib = input$annual, rate = return, years = i)
      growing_contrib[i+1] <- no_contrib[i+1] + growing_annuity(contrib = input$annual, rate = return, growth = growth, years = i)
    }
    
    data.frame("year" = years, "no_contrib" = no_contrib, "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
  })
  
  output$plot1 <- renderPlot({
    mod <- modalities()
    df_contrib <- data.frame("type" = rep("no_contrib", length(mod$year)), "year" = mod[,1], "value" = mod[,2])
    df_fixed <- data.frame("type" = rep("fixed_contrib", length(mod$year)), "year" = mod[,1], "value" = mod[,3])
    df_growing <- data.frame("type" = rep("growing_contrib", length(mod$year)), "year" = mod[,1], "value" = mod[,4])
    df_final <- rbind(df_contrib, df_fixed, df_growing)
    
    facet <- function(number){
      if(input$facet == "No"){
        
        ggplot(mod, aes(x= year)) +
          geom_line(aes(y = no_contrib, color = "no_contrib"), size = 1) + 
          geom_line(aes(y = fixed_contrib, color = "fixed_contrib"), size = 1) +
          geom_line(aes(y = growing_contrib, color = "growing_contrib"), size = 1) + 
          geom_point(aes(y = no_contrib, color = "no_contrib")) +
          geom_point(aes(y = fixed_contrib, color = "fixed_contrib")) +
          geom_point(aes(y = growing_contrib, color = "growing_contrib")) +
          labs(title = "Three modes of investing", x = "Year", y = "value", color = "variable") + 
          scale_color_manual(values = c("no_contrib" = "red", "fixed_contrib" = "green", "growing_contrib" = "blue"), 
                             breaks = c("no_contrib", "fixed_contrib", "growing_contrib"), guide = "legend") 
      }
      else{
        ggplot(df_final, aes(year, value)) +
          geom_line(aes(color = type), size = 1) +
          geom_point(aes(color = type)) +
          geom_area(mapping = aes(fill = type), alpha = 0.5) +
          facet_wrap(~ type) +
          scale_alpha() +
          labs(title = "Three modes of investing", color = "variable", fill = "variable") +
          theme_bw()
        
      }
    }
    
    facet(number = return)
    
  })
  
  output$balances_table <- renderPrint({
    modalities()
    
  })
  
}

shinyApp(ui = ui, server = server)
