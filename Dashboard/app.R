#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)
library(shinythemes)

data <- QUan
dfasxts <- as.xts(x = data[, -1], order.by = data$Date)
data_2022 <- as.xts(x = data_2022[, -1], order.by = data_2022$Date)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    body {
      background-color: black;
      color: white;
    }"
    ))
  ),
  titlePanel("Modeling the Volatility of US Bond Yields"),
  dygraphOutput("dygraph")
)




server <- function(input, output) {
  output$dygraph <- renderDygraph({
    dygraph(data_2022, main = "All Zero Coupon Yields (All Time Horizons) 2022", 
            ylab = "Value") %>%
      dyAxis('x', axisLabelFontSize = 12) %>%
      dyRangeSelector()
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
