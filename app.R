#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#theme
theme_set(theme_bw())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regression towards the mean"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          #heritability
          sliderInput("additive_h2",
                      "Additive heritability:",
                      min = 0,
                      max = 100,
                      value = 60),
          #grandparents
          numericInput("momo_value",
                      "Mother's mother's value",
                      value = 100),
          numericInput("mofa_value",
                       "Mother's father's value",
                       value = 100),
          numericInput("famo_value",
                       "Father's mother's value",
                       value = 100),
          numericInput("fafa_value",
                       "Father's father's value",
                       value = 100),
          #parents
          numericInput("mo_value",
                       "Mother's value",
                       value = 120),
          numericInput("fa_value",
                       "Father's value",
                       value = 120),
          #data range
          numericInput("lower_limit",
                       "Plot lower limit",
                       value = 50),
          numericInput("upper_limit",
                       "Plot upper limit",
                       value = 150)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("reg_line")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$reg_line <- renderPlot({
      # browser()
      #breeder's equation
      parent_mean = mean(c(input$mo_value, input$fa_value))
      grandparent_mean = mean(c(input$momo_value, input$mofa_value, input$famo_value, input$fafa_value))
      offspring_mean = grandparent_mean + (input$additive_h2/100) * (parent_mean - grandparent_mean)

      #prep data
      d = tibble(
        x = seq(input$lower_limit, input$upper_limit),
        y = x,
        y_reg = grandparent_mean + (input$additive_h2/100) * (y - grandparent_mean)
      )

      #offspring data
      d_offspring = tibble(
        x = parent_mean,
        y = parent_mean,
        xend = parent_mean,
        yend = offspring_mean,
        text = round(offspring_mean, 0) %>% as.character()
        )

      #regression plot
      ggplot(d, aes(x, y)) +
        geom_line() +
        geom_line(mapping = aes(y = y_reg), linetype = "dotted") +
        geom_segment(data = d_offspring, mapping = aes(x, y, xend = xend, yend = yend), arrow = arrow()) +
        geom_label(data = d_offspring, mapping = aes(x = x, y = y, label = text)) +
        scale_x_continuous("Phenotypic value") +
        scale_y_continuous("Phenotypic value")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
