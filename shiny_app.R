library(shiny)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggspectra)

# Next to do:
# add slider for k for knn
# add local maxima 
# add bar graph of number of serves at each speed

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
avg_pp_data <- 
  pp_data %>% 
  group_by(server, serve_type, serve_speed, server_to) %>% 
  summarize(avg_prob = mean(point_probability),
            avg_err_perc = sum(point_probability == 0)/n(),
            avg_ace_perc = sum(point_probability == 1)/n()) %>% 
  arrange(server)


ksmoothed_data <- 
  avg_pp_data %>%
  group_by(server, serve_type, server_to) %>% 
  summarize(ksmooth_point_prob = list(ksmooth(x = serve_speed,
                                              y = avg_prob,
                                              kernel = "normal",
                                              bandwidth = 5,
                                              n.points = n())), 
            ksmooth_error_perc = list(ksmooth(x = serve_speed,
                                              y = avg_err_perc,
                                              kernel = "normal",
                                              bandwidth = 5,
                                              n.points = n())),
            ksmooth_ace_perc = list(ksmooth(x = serve_speed,
                                            y = avg_ace_perc,
                                            kernel = "normal",
                                            bandwidth = 5,
                                            n.points = n()))
  ) %>% 
  unnest_wider(col = c(ksmooth_point_prob,
                       ksmooth_error_perc,
                       ksmooth_ace_perc),
               names_sep = "") %>% 
  unnest(col = -server) %>% 
  select(-c("ksmooth_error_percx", "ksmooth_ace_percx")) %>% 
  setNames(., c("server",
                "serve_type",
                "position",
                "serve_velocity",
                "point_prob",
                "error_perc",
                "ace_perc"))

## create a plot of ksmooth values for point probability where:
## - error percentage and ace percentage on the y-axis and 
## - serve speed on the x-axis 
shiny_plot_data <- 
  ksmoothed_data %>% 
  pivot_longer(cols = c(point_prob, error_perc, ace_perc),
               names_to = "perc_type",
               values_to = "percentage")
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
      
      # Input: Slider for server position  ----
      selectInput("position", "Serve Position:",
                  c("1" = 1,
                    "5" = 5,
                    "6" = 6))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: ShinyPlot ----
      plotOutput(outputId = "shinyPlot")
      
    )
  )
)

############################################################
############################################################

server <- function(input, output) {
  
  # shiny plot
  output$shinyPlot <- renderPlot({
    
    shiny_plot_data %>% 
    filter(server == input$server,
           serve_type == input$serve_type,
           position == input$position) %>%
    ggplot(aes(x = serve_velocity, y = percentage)) +
    geom_line(aes(color = perc_type)) +
    labs(x = "Serve Speed (km/h)", y = "Percentage (%)", color = "")+
    scale_colour_manual(values=c("#00FF00", "#FF0000", "#0000FF"),
                        labels = c("Ace Percentage",
                                   "Error Percentage",
                                   "Earn Percentage")) +
    theme_bw()
  })

}

shinyApp(ui = ui, server = server)