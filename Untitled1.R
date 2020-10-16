library(shiny)
library(ggplot2)
library(Cairo)

ui <- fluidPage(
  shinythemes::themeSelector(),
  fluidRow(
    column(width = 6,
           plotOutput("plot2", height = 350,
                      click = "plot2_click",
                      brush = brushOpts(
                        id = "plot2_brush"
                      )
           ),
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    )
  )
)
server <- function(input, output) {
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(d15))
  )
  
  output$plot2 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- d15[ vals$keeprows, , drop = FALSE]
    exclude <- d15[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(x= StudentWorkExpMonths, y=StudentAge )) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "blue") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(0,50), ylim = c(15,40))
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot2_click, {
    res <- nearPoints(d15, input$plot2_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(d15, input$plot2_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(d15))
  })
  
}

shinyApp(ui, server)