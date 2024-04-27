library(shiny)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(thematic)
library(shinythemes)
library(shinyvalidate)

ui <- fluidPage(
  titlePanel("Mondrian Art"),
  
  theme = shinytheme("cyborg"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("artPeriod", "Select Art Period:",
                  choices = c("Paris", "New York")),
      sliderInput("colorScheme", "Color Scheme:",
                  min = 1, max = 4, value = 1,
                  step = 1, ticks = FALSE),  
      numericInput("numVerticalLines", "Number of Vertical Lines:",
                   min = 0, max = 10, value = 0),
      sliderInput("numHorizontalLines", "Number of Horizontal Lines:",
                  min = 0, max = 10, value = 2),
      sliderInput("moveLines", "Move Lines (Left/Right or Up/Down):",
                  min = -5, max = 5, value = 0),
      submitButton("Create", icon("refresh")),
      tags$head(
        tags$style(HTML('
          #colorScheme .irs-grid-text {font-size: 10px; padding-top: 25px;}
          #colorScheme .irs-grid-pol.small {top: 20px;}
          #colorScheme .irs {margin-top: 0px;}
          #colorScheme .irs-with-grid {bottom: 0px;}
        '))
      ),
      div(
        style = "display: flex; justify-content: space-between; padding-right: 20px; padding-left: 20px;",
        span("RYB"), span("CMYK"), span("Grayscale"), span("Modern")  # Labels for the slider positions
      )
    ),
    mainPanel(
      plotOutput("artDisplay", width = "800px", height = "800px")
    )
  )
)




server <- function(input, output, session) {
  
  
  
  iv <- InputValidator$new()
  iv$add_rule("numVerticalLines", sv_between(0, 10))
  iv$enable()
  
  
  
  color_palettes <- list(
    ryb = c("#CC0000", "#E8B600", "#0000FF"),
    cmyk = c("#00FFFF", "#FF00FF", "#FFFF00", "#000000"),
    grayscale = c("#000000", "#555555", "#AAAAAA", "#FFFFFF"),
    modern = c("#800080", "#00FF00", "#FFA500", "#FFC0CB")
  )
  
  my_theme <- function() {
    theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        axis.text = element_blank(),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.grid = element_blank(),
        legend.position = 'none', legend.title = element_blank()
      )
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
        color = c("#CC0000", "blue", "#E8B600")
      )
      
      num_vertical <- input$numVerticalLines
      if (!iv$is_valid()) {
        num_vertical = 0
      }
      
      vertical_lines <- data.frame(
        x = runif(num_vertical, min = 0, max = 10),
        y = rep(c(0, 10), length.out = num_vertical)
      )
      
      num_horizontal <- input$numHorizontalLines
      horizontal_lines <- data.frame(
        x = rep(c(0, 10), length.out = num_horizontal),
        y = runif(num_horizontal, min = 0, max = 10)
      )
      
      vertical_lines$x <- vertical_lines$x + input$moveLines
      horizontal_lines$y <- horizontal_lines$y + input$moveLines
      
      ggplot() +
        geom_rect(
          data = rectangles,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(color))
        ) +
        geom_vline(
          data = vertical_lines,
          aes(xintercept = x),
          color = "black",
          size = 2
        ) +
        geom_hline(
          data = horizontal_lines,
          aes(yintercept = y),
          color = "black",
          size = 4
        ) +
        geom_vline(xintercept = x_values$x, linewidth = 4) +
        geom_hline(yintercept = y_values$y, linewidth = 4) + 
        coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
        geom_segment(data = half_lines_horizontal, aes(x = x_start, xend = x_end, y = y),
                     linewidth = 4) +
        geom_segment(data = half_lines_vertical, aes(x = x, y = y_start, xend = x, 
                                                     yend = y_end), linewidth = 4) +
        scale_fill_manual(values = selected_palette) +
        coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
        my_theme()
      
    } else if (input$artPeriod == "New York") {
      
      selected_palette <- color_palettes[[input$colorScheme]]
      
      set.seed(1)
      
      rectangles <- data.frame(
        xmin = c(-0.5, 2.5, 4, 6, 7.5),
        xmax = c(1, 3.5, 6, 7.5, 9.7),
        ymin = c(7.5, 0, 7.5, .5, 4),
        ymax = c(10, 4, 7.5, 2, 10),
        color = c("#E8B600", "white", "white", "#CC0000", "white")
      )
      
      
      vertical_lines_NYbase <- data.frame(
        x = c(1, 2.5, 3.5, 4, 6, 7.5, 8, 9.7),
        y = rep(10, 8)
      )
      
      
      horizontal_lines_NYbase <- data.frame(
        x = c(10, 10, 10, 10),
        y = c(2, 4, 6, 7.5)
      )
      
      
      num_vertical <- input$numVerticalLines
      if (!iv$is_valid()) {
        num_vertical = 0
      }
      
      vertical_lines <- data.frame(
        x = runif(num_vertical, min = 0, max = 10),
        y = rep(c(0, 10), length.out = num_vertical)
      )
      
      num_horizontal <- input$numHorizontalLines
      horizontal_lines <- data.frame(
        x = rep(c(0, 10), length.out = num_horizontal),
        y = runif(num_horizontal, min = 0, max = 10)
      )
      
      vertical_lines_NYfinal <- rbind(vertical_lines_NYbase, vertical_lines)
      horizontal_lines_NYfinal <- rbind(horizontal_lines_NYbase, horizontal_lines)
      
      ggplot() +
        geom_rect(data = rectangles, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(color))) +
        geom_segment(data = vertical_lines_NYfinal, aes(x = x, y = -0.5, xend = x, yend = y), color = "black", size = 2) +
        geom_segment(data = horizontal_lines_NYfinal, aes(x = -0.5, y = y, xend = x, yend = y), color = "black", size = 4) +
        geom_segment(data= horizontal_lines_NYfinal, aes(x=1.1, y=0.25, xend=3.9, yend=0.25), color = "#1B6FAA", size = 3) +
        geom_segment(data= horizontal_lines_NYfinal, aes(x=2.6, y=0, xend=3.39, yend=0), color = "white", size = 3) +
        geom_segment(data= horizontal_lines_NYfinal, aes(x=6, y=0.4, xend=8, yend=0.4), color = "black", size = 3) +
        geom_segment(data= horizontal_lines_NYfinal, aes(x=6, y=-0.25, xend=8, yend=-.25), color = "black", size = 3) +
        geom_segment(data= horizontal_lines_NYfinal, aes(x=9.8, y=0.3, xend=10, yend=0.3), color="#CC0000", size = 3) +
        coord_cartesian(xlim = c(0, 10), ylim = c(0, 9.7)) +
        geom_vline(
          data = vertical_lines,
          aes(xintercept = x),
          color = "black",
          size = 2
        ) +
        geom_hline(
          data = horizontal_lines,
          aes(yintercept = y),
          color = "black",
          size = 4
        ) +
        scale_fill_manual(values = selected_palette) +
        my_theme()
    } 
  })
}

shinyApp(ui = ui, server = server)