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
  tags$head(
    tags$style(HTML('
      body {
        font-family: "Lato", sans-serif;
      }
    '))
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("artPeriod", "Select Art Period:",
                  choices = c("Paris", "New York")),
      selectInput("colorScheme", "Color Scheme:",
                  choices = c("Original", "CMYK", "Grayscale", "Modern")),  
      numericInput("numVerticalLines", "Number of Vertical Lines:",
                   min = 0, max = 10, value = 0),
      numericInput("numHorizontalLines", "Number of Horizontal Lines:",
                  min = 0, max = 10, value = 0),
      submitButton("Create", icon("refresh")),
      tags$head(
        tags$style(HTML('
          #colorScheme .irs-grid-text {font-size: 10px; padding-top: 25px;}
          #colorScheme .irs-grid-pol.small {top: 20px;}
          #colorScheme .irs {margin-top: 0px;}
          #colorScheme .irs-with-grid {bottom: 0px;}
        '))
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
  
  iv <- InputValidator$new()
  iv$add_rule("numHorizontalLines", sv_between(0, 10))
  iv$enable()
  
  

  
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
      
      
      color_palettes_paris <- list(
        Original = c("#CC0000", "#E8B600", "#0000FF"),
        CMYK = c("#00FFFF", "#FF00FF", "#FFFF00", "#000000"),
        Grayscale = c("#000000", "#555555", "#AAAAAA", "#FFFFFF"),
        Modern = c("#800080", "#00FF00", "#FFA500", "#FFC0CB")
      )
      
      
      selected_palette <- color_palettes_paris[[input$colorScheme]]
      
    
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
      
      num_horizontal <- input$numHorizontalLines
      if (!iv$is_valid()) {
        num_horizontal = 0
      }
      
      vertical_lines <- data.frame(
        x = runif(num_vertical, min = 0, max = 10),
        y = rep(c(0, 10), length.out = num_vertical)
      )
      

      horizontal_lines <- data.frame(
        x = rep(c(0, 10), length.out = num_horizontal),
        y = runif(num_horizontal, min = 0, max = 10)
      )
      
      vertical_lines$x <- vertical_lines$x + input$moveLines
      horizontal_lines$y <- horizontal_lines$y + input$moveLines
      
      x_values = data.frame(x = c(2.5))
      y_values = data.frame(y = c(3))
      
      half_lines_horizontal = data.frame(
        x_start = c(-0.5, 9), 
        x_end = c(2.5, 10.5), 
        y = c(7, 1.25)
      )
      
      half_lines_vertical = data.frame(
        x = 9,             
        y_start = -0.5,       
        y_end = 3          
      )
      
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
      
      
      color_palettes_ny <- list(
        Original = c("#CC0000", "#E8B600", "#1B6FAA"),
        CMYK = c("#00FFFF", "#FF00FF", "#FFFF00", "#000000"),
        Grayscale = c("#000000", "#555555", "#AAAAAA", "#FFFFFF"),
        Modern = c("#800080", "#00FF00", "#FFA500", "#FFC0CB")
      )
      
      selected_palette <- color_palettes_ny[[input$colorScheme]]
      
      
      rectangles <- data.frame(
        xmin = c(-0.5, 6),
        xmax = c(1, 7.5),
        ymin = c(7.5, .4),
        ymax = c(13, 2),
        color = c("#E8B600", "#CC0000")
      )
      
      
      vertical_lines_NYbase <- data.frame(
        x = c(1, 2.5, 3.5, 4, 6, 7.5, 8, 9.7),
        y = rep(10, 8)
      )
      
      
      horizontal_lines_NYbase <- data.frame(
        x = c(10, 10, 10, 10),
        y = c(2, 4, 6, 7.5)
      )
      
      
      additional_segments <- data.frame(
        x_start = c(1.03, 2.6, 6, 6, 9.73),
        x_end = c(3.97, 3.39, 8, 8, 11),
        y = c(0.25, 0, 0.4, -0.25, 0.3),
        color = c("#1B6FAA", "white", "black", "black", "#CC0000")
      )
      
      num_vertical <- input$numVerticalLines
      if (!iv$is_valid()) {
        num_vertical = 0
      }
      
      num_horizontal <- input$numHorizontalLines
      if (!iv$is_valid()) {
        num_horizontal = 0
      }
      
      vertical_lines <- data.frame(
        x = runif(num_vertical, min = 0, max = 10),
        y = rep(c(0, 10), length.out = num_vertical)
      )
      

      horizontal_lines <- data.frame(
        x = rep(c(0, 10), length.out = num_horizontal),
        y = runif(num_horizontal, min = 0, max = 10)
      )
      
      vertical_lines_NYfinal <- rbind(vertical_lines_NYbase, vertical_lines)
      horizontal_lines_NYfinal <- rbind(horizontal_lines_NYbase, horizontal_lines)
      
      
      ggplot() +
        geom_rect(data = rectangles, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color)) +
        scale_fill_identity() +
        geom_vline(data = vertical_lines_NYfinal, aes(xintercept = x), color = "black", size = 2) +
        geom_hline(data = horizontal_lines_NYfinal, aes(yintercept = y), color = "black", size = 4) +
        geom_segment(data = additional_segments, aes(x = x_start, xend = x_end, y = y, yend = y, color = color), size = 3) +
        scale_color_identity() +
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
        coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
        my_theme()
    } 
  })
}

shinyApp(ui = ui, server = server)