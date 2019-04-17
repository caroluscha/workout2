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
library(DT)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Simulator"),
   
   #input sliders  
   wellPanel(
     fluidRow(
       column(4,
        sliderInput("int_amount", "Initial Amount", min = 0, max = 100000, value = 1000, step = 500, pre = "$"),
        sliderInput("anu_con", "Annual Contribution", min = 0, max = 50000, value = 2000, step = 500, pre = "$")
       ),
      column(4,
        sliderInput("ret_rate", "Return Rate (in %)", min = 0, max = 20, value = 5, step = 0.1, post ="%"),
        sliderInput("grow_rate", "Growth Rate (in %)", min = 0, max = 20, value = 2, step = 0.1, post = "%")
      ),
      column(4,
        sliderInput("years", "Years", min = 0, max = 50, value = 20, step = 1),
        selectInput("facet", "Facet", choices = c("Yes", "No"), selected = "Yes")
              
      )
    )
    ),
       #Show a plot of the generated distribution
   wellPanel(
         plotOutput("savings_plot")
      ),
  wellPanel(
    dataTableOutput("saving_table")
  )
)

# Define server logic required to generate plot and data table
server <- function(input, output) {

  output$savings_plot <- renderPlot({
      
    #code to generate data frame 
    
    #Future Value Function
    
    #' @title future value function
    #' @param amount initial invested amount (real)
    #' @param rate annual rate of return (real)
    #' @param years number of years (real)
    future_value <- function(amount, rate, years) {
      fv <- amount * (1 + rate) ^ years
      return(fv)
    }
    
    #Future Value of Annuity
    
    #' @title Future Value of Annuity
    #' @param contrib first contribution 
    #' @param rate annual rate of return
    #' @param years number of years 
    annuity <- function(contrib, rate, years) {
      fva <- contrib * (((1 + rate) ^ years - 1) / rate)
      return(fva)
    }
    
    #Future Value of Growing Annuity
    
    #' @title Future Value of Growing Annuity
    #' @param contrib first contribution 
    #' @param rate annual rate of return
    #' @param growth annual growth rate
    #' @param years number of years 
    growing_annuity <- function(contrib, rate, growth, years) {
      fvga <- contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth)
      return(fvga)
    }
    
    no_con_series <- vector("double")
    no_con_series[1] <- input$int_amount
    #loop for no contribution annuity
    for(i in 1:input$years) {
      no_con <- future_value(input$int_amount, input$ret_rate/100, i)
      no_con_series[i+1] <- no_con
      
    }
    
    #loop for fixed contribution annuity
    fixed_con_series <- vector("double")
    fixed_con_series[1] <- input$int_amount
    for(i in 1:input$years) {
      fixed_con <- future_value(input$int_amount, input$ret_rate/100, i) + annuity(input$anu_con, input$ret_rate/100, i)
      fixed_con_series[i + 1] <- fixed_con
      
    }
    
    #loop for growing contribution annuity
    grow_con_series <- vector("double")
    grow_con_series[1] <- input$int_amount
    for (i in 1:input$years) {
      grow_con <- future_value(input$int_amount, input$ret_rate/100, i) + growing_annuity(input$anu_con, input$ret_rate/100, input$grow_rate/100, i)
      grow_con_series[i + 1] <- grow_con
      
    }
  #vector for the amounts
  amounts_total <- c(no_con_series, fixed_con_series, grow_con_series)
  # vector for the mode of annuity
  modes <- c(rep("no_contrib", input$years + 1), rep("fixed_contrib", input$years + 1), rep("growing_contrib", input$years + 1))
  
  #vector for the years of investment
  year <- rep(c(0:input$years),3)
  #data frame to be plotted
  save_sim <- data.frame(year, amounts_total, modes)
  
  if (input$facet == "Yes") {
  ggplot(data = save_sim, aes(x = year, y = amounts_total)) +
      geom_line(aes(color= modes)) +
      scale_color_discrete(name = "Annuity", 
                           labels = c(
                             "Fixed Contribution", 
                             "Growing Contribution", 
                             "No Contribution")) +
      labs(title = "Future Return on Investments") + 
      xlab("Year") +
      ylab("Dollars (USD)") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) + 
      facet_wrap(modes ~., labeller = as_labeller(c(
        `fixed_contrib` = "Fixed Contribution", 
        `growing_contrib` = "Growing Contribution", 
        `no_contrib` = "No Contribution")))
      
  }
  else (
    ggplot(data = save_sim, aes(x = year, y = amounts_total)) +
      geom_line(aes(color= modes)) +
      scale_color_discrete(name = "Annuity", 
                           labels = c(
                             "Fixed Contribution", 
                             "Growing Contribution", 
                             "No Contribution")) +
      labs(title = "Future Return on Investments") + 
      xlab("Year") +
      ylab("Dollars (USD)") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
  )
  })
  
  output$saving_table <- renderDataTable({
    #code to generate data frame 
    
    #Future Value Function
    
    #' @title future value function
    #' @param amount initial invested amount (real)
    #' @param rate annual rate of return (real)
    #' @param years number of years (real)
    future_value <- function(amount, rate, years) {
      fv <- amount * (1 + rate) ^ years
      return(fv)
    }
    
    #Future Value of Annuity
    
    #' @title Future Value of Annuity
    #' @param contrib first contribution 
    #' @param rate annual rate of return
    #' @param years number of years 
    annuity <- function(contrib, rate, years) {
      fva <- contrib * (((1 + rate) ^ years - 1) / rate)
      return(fva)
    }
    
    #Future Value of Growing Annuity
    
    #' @title Future Value of Growing Annuity
    #' @param contrib first contribution 
    #' @param rate annual rate of return
    #' @param growth annual growth rate
    #' @param years number of years 
    growing_annuity <- function(contrib, rate, growth, years) {
      fvga <- contrib * ((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth)
      return(fvga)
    }
    
    no_con_series <- vector("double")
    no_con_series[1] <- input$int_amount
    #loop for no contribution annuity
    for(i in 1:input$years) {
      no_con <- future_value(input$int_amount, input$ret_rate/100, i)
      no_con_series[i+1] <- no_con
      
    }
    
    #loop for fixed contribution annuity
    fixed_con_series <- vector("double")
    fixed_con_series[1] <- input$int_amount
    for(i in 1:input$years) {
      fixed_con <- future_value(input$int_amount, input$ret_rate/100, i) + annuity(input$anu_con, input$ret_rate/100, i)
      fixed_con_series[i + 1] <- fixed_con
      
    }
    
    #loop for growing contribution annuity
    grow_con_series <- vector("double")
    grow_con_series[1] <- input$int_amount
    for (i in 1:input$years) {
      grow_con <- future_value(input$int_amount, input$ret_rate/100, i) + growing_annuity(input$anu_con, input$ret_rate/100, input$grow_rate/100, i)
      grow_con_series[i + 1] <- grow_con
      
    }
    
    #vector for the years of investment
    year <- c(0:input$years)
    #data frame to be plotted
    save_sim2 <- data.frame("Year" = year,"No Contribution" = no_con_series,"Fixed Contribution" = fixed_con_series,"Growing Contribution" = grow_con_series)
    save_sim2
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

