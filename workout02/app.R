#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Different Saving scenarios"),
  
  fluidRow(
    column(4,
           sliderInput(inputId = "amount",
                       label = "Initial Amount",
                       min = 0, 
                       max = 100000,
                       step = 500,
                       value = 1000,
                       pre = '$')
    ),
    column(4,
           sliderInput(inputId = "rate",
                       label = "Return Rate (in %)",
                       min = 0, 
                       max = 20,
                       step = 0.1,
                       value = 5)
    ),
    column(4,
           sliderInput(inputId = "years",
                       label = "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20)
    )
  ),
  fluidRow(
    column(4,
           sliderInput(inputId = "contrib",
                       label = "Annual Contribution",
                       min = 0, 
                       max = 50000,
                       step = 500,
                       value = 2000,
                       pre = '$')
    ),
    column(4,
           sliderInput(inputId = "growth",
                       label = "Growth Rate (in %)",
                       min = 0, 
                       max = 20,
                       step = 0.1,
                       value = 2)
    ),
    column(4,
           selectInput(inputId = "facet",
                       label = "Facet?",
                       choices = c("Yes", "No"),
                       selected = "No")
    )
  ),
  
  h4("Timelines"),
  fluidRow(plotOutput("distPlot")),
  h4("Balances"),
  verbatimTextOutput("balance")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  dat <- reactive({
    # future value function
    future_value <- function(amount, rate, years){
      future_value <- amount * (1 + rate)^years
      return(future_value)
    }
    
    # future value of annuity function
    annuity <- function(contrib, rate, years){
      annuity <- contrib * (((1 + rate)^years - 1) / rate)
      return(annuity)
    }
    
    # future value of growing annuity function
    growing_annuity <- function(contrib, rate, growth, years){
      growing_annuity <- contrib * (((1 + rate)^years - (1 + growth)^years)/(rate - growth))
      return(growing_annuity)
    }
    
    year <- seq(0, input$years, by = 1)
    amount <- input$amount
    contrib <- input$contrib
    rate <- input$rate/100
    growth <- input$growth/100
    
    # get balance depends on different saving modes
    balance_mode1 <- rep(0, input$years + 1)
    for(i in 1:length(balance_mode1)){
      balance_mode1[i] <- future_value(amount, rate, years = (i-1)) 
    }
    
    balance_mode2 <- rep(0, input$years + 1)
    for(i in 1:length(balance_mode2)){
      balance_mode2[i] <- future_value(amount, rate, years = (i-1)) + annuity(contrib, rate, years = (i-1))
    }
    
    balance_mode3 <- rep(0, input$years+1)
    for(i in 1:length(balance_mode3)){
      balance_mode3[i] <- future_value(amount, rate, years = (i-1)) + growing_annuity(contrib, rate, growth, years = (i-1))
    }
    
    balance <- c(balance_mode1, balance_mode2, balance_mode3)
    modality <- rep(c("no contribution", "fixed contribution", "growing contribution"), c(input$years+1,input$years+1,input$years+1))
    
    # create a data frame
    dat <- data.frame(year = rep(year,3), balance = balance, modality = modality)
    return(dat)
  })
  
  output$distPlot <- renderPlot({
    timeline <- ggplot(dat = dat(), aes(x = year, y = balance)) + geom_point(aes(color = modality), alpha = 0.7) + geom_line(aes(color = modality)) + labs(title = "Three modes of investing", x = "year", y = "value")
    if (input$facet == "Yes"){
      timeline <- timeline + facet_grid(~ modality) + geom_area(aes(x = year, y = balance, fill = modality), alpha = 0.2) + theme_bw()
    }
    timeline
  })
  
  output$balance <- renderPrint({
    data.frame(year = seq(0, input$years), no_contrib = dat()$balance[dat()$modality == "no contribution"], fixed_contrib = dat()$balance[dat()$modality == "fixed contribution"], growing_contrib = dat()$balance[dat()$modality == "growing contribution"])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



