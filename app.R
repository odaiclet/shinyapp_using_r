# ===============================================
# Fill in the following fields
# ===============================================
# Title: Index Fund Investment Portafolio Calculator
# Description: This app retunrs a graph and the maximum investment return given an initial lump sum and annual/montly contributions 
# Author: Odaiclet Piccinini
# Date: 11/09/2020


# ===============================================
# Required packages
# ===============================================
library(dplyr)
library(ggplot2)
library(shiny)
library(reshape2)
library(tidyverse)
library(DT)

# ===============================================
# UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Index Fund Investment Portafolio Calculator"),
  fluidRow(
    # Inputs for initial amount, and number of years
    column(3,
           # initial investment amount
           sliderInput(inputId = "initial_investment", 
                       label = "Initial Investment Amount ($)", 
                       min = 0, max = 10000, 
                       value = 1000
                       ),
           # years of investment
           sliderInput(inputId = "years", 
                       label = "Number of Years", 
                       min = 0, max = 100, 
                       value = 10
                       )
          ),
    
    # Inputs for periodic contributions
    column(3,
           # Periodic contribution
           numericInput(inputId = "contrib_amount", 
                       label = "Periodic Contribution Amount ($)",
                       value = 360
                       ),
           # Contribution at the end of each month or of each year
           radioButtons(inputId = "contrib_type", 
                        label = "Type of Contribution (Annual/ Monthly)", 
                        choices = list("at the End of Month" = "at the End of Month",
                                       "at the End of Year" = "at the End of Year"), 
                        selected = "at the End of Year")
           ),
    
    # Inputs for Avg annual return, and avg annual volatility
    column(3,
           # Average annual rate of return is the mean value
           sliderInput(inputId = "avg_annual_rate_r",
                       label = "Average Annual Rate of Return (%)",
                       min = 0, max = 50, 
                       value = 10),
           # Average annual volatility is the standard deviation
           sliderInput(inputId = "avg_annual_volatility",
                       label = "Average Annual Volatility (%)",
                       min = 0, max = 50,
                       value = 18)
           ),
    # Inputs for number of simulations, and random seed
    column(2,
           # Number of simulations
           numericInput(inputId = "simulations",
                        label = "Number of Simulations", 
                        value = 50),
           # random seed
           numericInput(inputId = "seed", 
                        label = "Random Seed", 
                        value = 12345
                        )
           ),
    mainPanel( 
      h3('Portfolio Performance'),
      plotOutput('plot'),
      h5("The plot above displays the projected investment balance of an Index Fund. 
      On the Y-axis we have the Balance in $ (USD), and on the X-axis we have the numbers of years of 
      investment. The gray lines represent one simulation per year. The black line is the average 
      balances for simulations and the 10-th and 90-th percentiles are displayed in light red"),
      
      # Quantiles table #
      hr(),
      h3('Balance Quantiles'),
      h5("Below see the quantiles of the balance amounts at the end of the investing period in $ (USD)."),
      tableOutput('table1'),
      
      # Summary statistics table #
      hr(),
      h3('Summary Statistics Table'),
      h5("This table provides you with the average and the median balance per year."),
      h5("The Max column is the 90-th percentile values."),
      h5("The Min column is the 10-th percentile values."),
      tableOutput('table2'),
      
      # List of Balances per year per simulation #
      h2("Balances Table"),
      h5("This is an exhaustive table with balance amounts for all terms and all simulations."),
      DT::dataTableOutput("balance_table")
      
      ) # close main panel
  ) # Close fluid row parenthesis
) # Close fluid page
 

# ===============================================
# Server "server" logic
# ===============================================

server <- function(input, output) {

  # ============= Balance Table ================= #
  balance <- reactive({
    # define the seed
    set.seed(input$seed)
    
    # convert inputted mean and std. deviation into proportions
    avg_rate_r <- input$avg_annual_rate_r / 100  # covert annual rate percentages to proportions
    avg_volatility <- input$avg_annual_volatility / 100 # covert volatility rate percentages to proportions
    
    # create a balances matrix of size years, simulations to populate the values of return
    balances <- matrix(0, nrow = (input$years + 1), ncol = input$simulations)
    balances[1, ] <- input$initial_investment
    
    
    # start of condition 1: the contribution is at end of year
    if (input$contrib_type == "at the End of Year"){
      # c are the columns of the simulation number
      for (c in 1:input$simulations){
        # r are the rows with year number starting at 0
        for (r in 2:(input$years + 1)){
          # at index r, c calculate the annual return for year r - 1
          balances[r, c] <- balances[r - 1, c] * (1 + rnorm(1, avg_rate_r, avg_volatility)) + input$contrib_amount
        } # close rows for loop
      } # close columns for loop

    # end of condition 1
    # start condition 2: the periodic contribution is at end of each month
    } else {
      for (c in 1:input$simulations){
        sum_value <- input$initial_investment
        # r are the rows with year number starting at 0
        for (r in 2:(input$years + 1)){
          # at index r, c calculate the annual return for year r - 1
          for (month in 1:12){
            # calculate the annual return for year with a monthly contribution
            sum_value <- sum_value * (1 + (rnorm(1, avg_rate_r, avg_volatility) / 12)) + input$contrib_amount
          } # close for loop
          balances[r, c] <- sum_value
        } # close rows for loop
      } # close columns foor loop
    } # end of condition 2 or else statement
    
    # use  melt to oncatenate values into 1 table of 2 columns year and simulation
    balance <- melt(balances, varnames = c("year", "simulation"))
    # calculate the average values of balances matrix
    means <- rowMeans(balances)
    # update the year column and mean columns 
    balance$year <- seq(0,input$years)
    # add the column means to the data frame
    balance$means <- means
    # print the balance data frame
    balance
    
    }) # close reactive line for the BALANCE function

  
  
  # =========== 10 and 90th Quantiles Table =============== #
  percentile_90_10 <- reactive({
    # Summarize balance data frame to find 90th and 10th quantiles
    percentile_90_10 <- balance() %>%
      # group by core variable year
      group_by(year) %>%
      # summarize to extract the 90th and 10th quantiles plus the median
      summarize(.groups = "drop",
                # calculate the mean per year
                average = mean(value),
                # calculate the median per year
                median = median(value),
                # calculate the 90-th quantile
                max = quantile(value, probs =  0.90),
                # calculate the 10-th quantile
                min = quantile(value, probs = 0.10))
    # print table
    percentile_90_10
  }) # close reactive line for the 10 and 90th Quantiles table

  
  
  # ================== Percentiles Table ================== #
  percentiles_table <- reactive({
    # vector of proportions of percentages
    p <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.0)
    # Summarize table by core values
    percentiles_table <- balance() %>% 
      # group by simulations
      group_by(year) %>%  
      # summarize and calculate the quantiles of the balance
      summarize(.groups = "drop",
                # calculate quantiles of values column
                enframe(quantile(value, p), "quantile", "end_amount")) %>%
      # slice rows for end of investment period
      filter(year == input$years) %>%
      # select relevant variables and amount
      select(quantile, end_amount)
    # print table
    percentiles_table 
  })

  
  
#### ========= outputs ========== ####

     # ==== Plot Details ===== #
    output$plot <- renderPlot({
      ggplot(data = balance(), 
             aes(x = year, y = value)) + 
        # display the simulations in gray
        geom_line(aes(group = simulation), 
                  colour = "gray") +
        # display the average in black 
        geom_line(aes(y = means, colour = "mean"),
                  size = 1.2,
                  alpha = 0.8) +
        # set 90-th percentile line
        geom_line(data = percentile_90_10(), 
                  aes(y = max, colour = "90-th percentile"), 
                  size = 1.2,
                  alpha = 0.8) + 
        # display 10-th percentile line
        geom_line(data = percentile_90_10(), 
                  aes(y = min, colour = "10-th percentile"),
                  size = 1.2,
                  alpha = 0.8) +
        # set legend name
        scale_color_discrete(name = "Legend") +
        # set names for legend
        scale_color_manual(values = c(
                  "mean" = "black",
                  "90-th percentile" = "coral3",
                  "10-th percentile" = "coral3")) +
        # set plot x-axis label
        xlab("Number of Years") +
        # set plot y-axis label
        ylab("Portfolio Balance") + 
        # set plot title
        ggtitle("Total Portfolio Balance of Pojected Simulations per Year") + 
        # use theme classic to remove gray background
        theme_minimal() + 
        # set axis font sizes and colors
        theme(plot.title = element_text(lineheight = 3, 
                                        face = "bold", 
                                        color = "coral4", 
                                        size = 18,
                                        hjust = 0.5),
              axis.title.x = element_text(lineheight = 3, 
                                        face = "bold", 
                                        color = "coral4", 
                                        size = 16),
              axis.title.y = element_text(lineheight = 3, 
                                          face = "bold", 
                                          color = "coral4", 
                                          size = 16)) +
        labs(colour = "Legend") + 
        # dollar symbol
        scale_y_continuous(labels = scales::dollar) +
        # letter size
        theme(text = element_text(size = 14)) + 
        # expand limits
        expand_limits(x = 0, y = 0) 
    }) # output of plot end

# =========== Percentiles Statistics ======== #
    output$table1 <- renderTable({
      # round digits to nearest hundred and add dollar sign
      percent_list <- percentiles_table() %>%
        mutate_if(is.numeric, round, digits = 3)
      percent_list$end_amount <- paste0('$', percent_list$end_amount)
      # print table for display
      percent_list
    }) # close renderTable
    
    # static data of Summary Statistics
    output$table2 <- renderTable({
      # round digits to nearest hundred and add dollar sign
      summary_table <- percentile_90_10() %>%
        mutate_if(is.numeric, round, digits = 3)
      summary_table$average <- paste0('$', summary_table$average)
      summary_table$median <- paste0('$', summary_table$median)
      summary_table$max <- paste0('$', summary_table$max)
      summary_table$min <- paste0('$', summary_table$min)
      # print table for display
      summary_table
    }) # close renderTable
     
    
    
# =========== List of Balance per Year Table =============== #
    output$balance_table <- DT::renderDataTable({
      
      # insert balance table rounded to 3 digits
      table <- balance()
      # round digits to nearest hundred and add dollar sign
      table <- table %>%
        mutate_if(is.numeric, round, digits = 3)
      table$value <- paste0('$', table$value)
      table$means <- paste0('$', table$means)
      # print table for display
      table
      
    }) # close renderDataTable
    

}# close server function
# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

