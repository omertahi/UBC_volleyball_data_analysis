library(shiny)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggspectra)

#################################################################
#################################################################


# See above for the definitions of ui and server
ui <- fluidPage(
  
  # App title ----
  titlePanel("UBC Volleyball Data Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for serve_type to filter for ----
      selectInput(inputId = "serve_type",
                  label = "Type of Serve:",
                  c("Float" = "float",
                    "Spin" = "spin",
                    "Hybrid" = "hybrid")),
      
      # Input: Slider for server number  ----
      sliderInput("server", "Server Number:",
                  min = 1, max = 20,
                  value = 1, step = 1),
      
      # Input: Checkbox for serve to:  ----
      checkboxGroupInput("serve_to", "Serve To:", 
                         choices = list("1" = 1, 
                                        "2" = 2, 
                                        "3" = 3,
                                        "4" = 4,
                                        "5" = 5,
                                        "6" = 6),
                         selected = 1),
      
      # Input: Checkbox for server from:  ----
      checkboxGroupInput("serve_from", "Serve from:",
                         choices = list("1" = 1,
                                        "5" = 5,
                                        "6" = 6),
                         selected = 1),
      
      # Input: Slider for k-nn bandwidth
      numericInput("k", "K-NN Bandwidth", 
                   value = 5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: ShinyPlot ----
      plotOutput(outputId = "shinyPlot"),
      
      # Output: SampleSize barPlot
      plotOutput(outputId = "sample_size_bar_plot")
      
    )
  )
)

############################################################
############################################################

server <- function(input, output) {
  
  # Data pre-processing ----
  # read data
  volleyball_data <- read_excel(paste0(getwd(), "/SAMPLE DATA- TWU FRI.xlsx"))
  
  # combine cut-spin and spin as spin in the serve_type column
  `%notin%` <- Negate(`%in%`)
  
  data <- 
    volleyball_data %>% 
    mutate(serve_type = replace(volleyball_data$serve_type,
                                volleyball_data$serve_type == "cut_spin",
                                "spin"),
           server = as.numeric(server)) %>% 
    filter(!is.na(server),
           server %notin% c(0, 99))
  
  ## add point_probability column
  pp_data <- 
    data %>% 
    mutate(point_probability = case_when(serve_outcome == 0 ~ 1,
                                         serve_outcome == 1 ~ 0.614,
                                         serve_outcome == 2 ~ 0.473,
                                         serve_outcome == 3 ~ 0.329,
                                         serve_outcome == 4 ~ 0.366,
                                         TRUE ~ 0)
    )

  
  # calculate average point scoring probability and 
  # average error percentage for each serve speed per player
  avg_pp_data_reactive <- reactive({
    req(input$server)
    req(input$serve_type)
    req(input$serve_to)
    req(input$serve_from)
    avg_pp_data <- 
      pp_data %>% 
      filter(serve_type == input$serve_type,
             server == input$server,
             server_to %in% input$serve_to,
             serve_from %in% input$serve_from) %>% 
      group_by(serve_speed) %>% 
      summarize(avg_prob = mean(point_probability),
                avg_err_perc = sum(point_probability == 0)/n(),
                avg_ace_perc = sum(point_probability == 1)/n())
  })
  
  
  
  ## perform k-smoothing
  ksmoothed_data_reactive <- reactive({
    req(input$k)
    ksmoothed_data <- 
      avg_pp_data_reactive() %>%
      summarize(ksmooth_point_prob = list(ksmooth(x = serve_speed,
                                                  y = avg_prob,
                                                  kernel = "normal",
                                                  bandwidth = input$k,
                                                  n.points = n())), 
                ksmooth_error_perc = list(ksmooth(x = serve_speed,
                                                  y = avg_err_perc,
                                                  kernel = "normal",
                                                  bandwidth = input$k,
                                                  n.points = n())),
                ksmooth_ace_perc = list(ksmooth(x = serve_speed,
                                                y = avg_ace_perc,
                                                kernel = "normal",
                                                bandwidth = input$k,
                                                n.points = n()))
      ) %>% 
      unnest_wider(col = c(ksmooth_point_prob,
                           ksmooth_error_perc,
                           ksmooth_ace_perc),
                   names_sep = "") %>% 
      unnest() %>% 
      select(-c("ksmooth_error_percx", "ksmooth_ace_percx")) %>% 
      setNames(., c("serve_velocity",
                    "point_prob",
                    "error_perc",
                    "ace_perc")) %>% 
      pivot_longer(cols = c(point_prob, error_perc, ace_perc),
                   names_to = "perc_type",
                   values_to = "percentage")
  })
  
  

  ############################################################
  ############################################################
  # shiny plot
  output$shinyPlot <- renderPlot({
    
    ksmoothed_data_reactive() %>%
      ggplot(aes(x = serve_velocity, y = percentage)) +
      geom_line(aes(color = perc_type)) +
      labs(x = "Serve Speed (km/h)", y = "Percentage (%)", color = "") +
      scale_colour_manual(values=c("#00FF00", "#FF0000", "#0000FF"),
                          labels = c("Ace Percentage",
                                     "Error Percentage",
                                     "Earn Percentage")) +
      theme_bw() + 
      theme(legend.position = c(0.255, 0.970),
            legend.key.size = unit(0.25, "cm"),
            legend.direction="horizontal",
            legend.background = element_rect(colour = 'black', fill = 'white', linetype = 'solid')) +
      stat_peaks(data = ksmoothed_data_reactive() %>% filter(perc_type == "point_prob"),
                 aes(x = serve_velocity, y = percentage),
                 span = 5,
                 color = "black") +
      stat_peaks(data = ksmoothed_data_reactive() %>% filter(perc_type == "point_prob"),
                 aes(x = serve_velocity,
                     y = percentage,
                     label = paste0(as.numeric(after_stat(y.label))*100,
                                    "% at ",
                                    after_stat(x.label),
                                    " km/h")),
                 span = 5,
                 geom = "text",
                 vjust = -0.5,
                 size = 3)

  })

  sample_size_table_reactive <- reactive({
    req(input$server)
    req(input$serve_type)
    req(input$serve_to)
    req(input$serve_from)
    sample_size_table <- 
      pp_data %>% 
      filter(serve_type == input$serve_type,
             server == input$server,
             server_to %in% input$serve_to,
             serve_from %in% input$serve_from) %>% 
      group_by(serve_speed) %>% 
      summarize(sample_size = n())
  })
  
  
  output$sample_size_bar_plot <- renderPlot({
    
    sample_size_table_reactive() %>% 
      ggplot(aes(x = serve_speed, y = sample_size)) +
      geom_bar(stat = "identity", color = "#F2A900", fill = "#002145", width = 1) +
      labs(x = "Serve Speed (km/h)", y = "Sample Size") +
      geom_text(aes(label = sample_size), vjust = -0.5, size = 2.5) +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      theme_bw()
  })
  
  
}

shinyApp(ui = ui, server = server)