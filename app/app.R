library(shiny)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

ui <- fluidPage(
  titlePanel("Mondrian Art"),
  sidebarLayout(
    sidebarPanel(
      selectInput("artPeriod", "Select Art Period:",
                  choices = c("Paris", "New York")),
      sliderInput("colorScheme", "Color Scheme:",
                  min = 1, max = 4, value = 1,
                  step = 1, ticks = FALSE),  # 'labels' argument removed
      tags$head(tags$style(HTML('
        #colorScheme .irs-grid-text {font-size: 10px; padding-top: 25px;}
        #colorScheme .irs-grid-pol.small {top: 20px;}
        #colorScheme .irs {margin-top: 0px;}
        #colorScheme .irs-with-grid {bottom: 0px;}
      '))),
      div(style = "display: flex; justify-content: space-between; padding-right: 20px; padding-left: 20px;",
          span("RYB"), span("CMYK"), span("Grayscale"), span("Modern")  # Labels for the slider positions
      )
    ),
    mainPanel(
      plotOutput("artDisplay", width = "100%", height = "800px")
    )
  )
)
server <- function(input, output, session) {
  

  color_palettes <- list(
    ryb = c("#CC0000", "#FFFF00", "#0000FF"),
    cmyk = c("#00FFFF", "#FF00FF", "#FFFF00", "#000000"),
    grayscale = c("#000000", "#555555", "#AAAAAA", "#FFFFFF"),
    modern = c("#800080", "#00FF00", "#FFA500", "#FFC0CB")
  )
  
  
  selected_palette <- reactive({
    switch(input$colorScheme,
           "1" = color_palettes$ryb,
           "2" = color_palettes$cmyk,
           "3" = color_palettes$grayscale,
           color_palettes$modern) 
  })
  
  my_theme <- function() {
    theme_minimal() +
      theme(axis.title = element_blank(),
            plot.title = element_text(face = "bold", size = 16),
            axis.text = element_blank(),
            plot.background = element_rect(fill = 'ghostwhite', color = 'white'),
            panel.grid = element_blank(),
            legend.position = 'none', legend.title = element_blank())
  }
  
  output$artDisplay <- renderPlot({
    if (input$artPeriod == "Paris") {
      selected_palette <- color_palettes[[input$colorScheme]]
      
      set.seed(1)
      rectangles <- data.frame(
        xmin = c(2.5, -0.5, 9),
        xmax = c(10.5, 2.5, 10.5),
        ymin = c(3, -0.5, -0.5),
        ymax = c(10.5, 3, 1.25),
        color = c("#CC0000", "blue", "yellow")
      )
      x_values <- data.frame(x = c(2.5))
      y_values <- data.frame(y = c(3))
      ggplot() +
        geom_rect(data = rectangles, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                         ymax = ymax, fill = as.factor(color))) +
        geom_vline(xintercept = x_values$x, linewidth = 4) +
        geom_hline(yintercept = y_values$y, linewidth = 4) +
        scale_fill_manual(values = selected_palette) +
        my_theme() 
      
    } else {
      set.seed(1)
      rectangles <- data.frame(
        xmin = c(-0.5, 2.5, 4, 6, 7.5),
        xmax = c(1, 3.5, 6, 7.5, 9.7),
        ymin = c(7.5, 0, 7.5, .5, 4),
        ymax = c(10, 4, 7.5, 2, 10),
        color = c("#E8B600", "white", "white", "#CC0000", "white")
      )
      vertical_lines <- data.frame(
        x = c(1, 2.5, 3.5, 4, 6, 7.5, 8, 9.7),
        y = rep(10, 8)
      )
      horizontal_lines <- data.frame(
        x = c(10, 10, 10, 10),
        y = c(2, 4, 6, 7.5)
      )
      ggplot() +
        geom_rect(data = rectangles, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color)) +
        geom_segment(data = vertical_lines, aes(x = x, y = -0.5, xend = x, yend = y), color = "black", size = 2) +
        geom_segment(data = horizontal_lines, aes(x = -0.5, y = y, xend = x, yend = y), color = "black", size = 4) +
        scale_fill_manual(values = c("#E8B600", "white", "white", "#CC0000", "white")) +
        my_theme()
    }
  })
}


shinyApp(ui = ui, server = server)