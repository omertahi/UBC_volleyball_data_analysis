library(shiny)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggspectra)


volleyball_data <-
  read_excel("UBC volleyball data input.xlsx",
             sheet = "Consolidated Data")[-1, ] %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         serve_type = gsub("_", " ", volleyball_data$serve_type))

# combine cut-spin and spin as spin in the serve_type column
`%notin%` <- Negate(`%in%`)

data <-
  volleyball_data %>%
  mutate(serve_type = replace(volleyball_data$serve_type,
                              volleyball_data$serve_type == "Cut Spin",
                              "Spin"),
         across(.cols = c(server, server_position, serve_speed), as.numeric)) %>%
  filter(!is.na(server),
         server %notin% c(0, 99))

## add point_probability column
pp_data <-
  data %>%
  mutate(point_probability = case_when(pass_outcome == 0 ~ 1,
                                       pass_outcome == 1 ~ 0.614,
                                       pass_outcome == 2 ~ 0.473,
                                       pass_outcome == 3 ~ 0.329,
                                       pass_outcome == 4 ~ 0.366,
                                       TRUE ~ 0))


# calculate average point scoring probability and
# average error percentage for each serve speed per player
avg_pp_data <-
  pp_data %>%
  filter(serve_type == "Spin",
         server == 2,
         reciever_position %in% c(1:6),
         server_position %in% c(1, 5, 6)) %>%
  group_by(serve_speed) %>%
  summarize(
    avg_prob = mean(point_probability),
    avg_err_perc = sum(point_probability == 0) / n(),
    avg_ace_perc = sum(point_probability == 1) / n()
  )



## perform k-smoothing
ksmoothed_data <-
  avg_pp_data %>%
  summarize(
    ksmooth_point_prob = list(ksmooth(x = serve_speed,
                                      y = avg_prob,
                                      kernel = "normal",
                                      bandwidth = 3,
                                      n.points = n())),
    ksmooth_error_perc = list(ksmooth(x = serve_speed,
                                      y = avg_err_perc,
                                      kernel = "normal",
                                      bandwidth = 3,
                                      n.points = n())),
    ksmooth_ace_perc = list(ksmooth(x = serve_speed,
                                    y = avg_ace_perc,
                                    kernel = "normal",bandwidth = 3,
                                    n.points = n()))) %>%
  unnest_wider(col = c(ksmooth_point_prob,
                       ksmooth_error_perc,
                       ksmooth_ace_perc),
               names_sep = "") %>%
  unnest(cols = everything()) %>%
  select(-c("ksmooth_error_percx", "ksmooth_ace_percx")) %>%
  setNames(., c("serve_velocity", "point_prob", "error_perc", "ace_perc")) %>%
  pivot_longer(
    cols = c(point_prob, error_perc, ace_perc),
    names_to = "perc_type",
    values_to = "percentage")



############################################################
############################################################
# shiny plot
ksmoothed_data %>%
  ggplot(aes(x = serve_velocity, y = percentage)) +
  geom_line(aes(color = perc_type)) +
  labs(x = "Serve Speed (km/h)", y = "Percentage (%)", color = "") +
  ggtitle("Optimal Serve Velocity") +
  scale_colour_manual(
    values = c("#80D77B", "#F50330", "#002145"),
    labels = c("Ace Percentage",
               "Error Percentage",
               "Earn Percentage")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.255, 0.970),
    legend.key.size = unit(0.25, "cm"),
    legend.direction = "horizontal",
    legend.background = element_rect(
      colour = '#002145',
      fill = 'white',
      linetype = 'solid'
    ),
    axis.title.x = element_text(colour = "#002145"),
    axis.title.y = element_text(colour = "#002145"),
    plot.title = element_text(colour = "#F2A900")
  ) +
  stat_peaks(
    data = ksmoothed_data %>% filter(perc_type == "point_prob"),
    aes(x = serve_velocity, y = percentage),
    span = 5,
    color = "black"
  ) +
  stat_peaks(
    data = ksmoothed_data %>% filter(perc_type == "point_prob"),
    aes(
      x = serve_velocity,
      y = percentage,
      label = paste0(
        as.numeric(after_stat(y.label)) * 100,
        "% at ",
        after_stat(x.label),
        " km/h"
      )
    ),
    span = 5,
    geom = "text",
    vjust = -0.5,
    size = 3
  )


sample_size_table <-
  pp_data %>%
  filter(
    serve_type %in% input$serve_type,
    server == input$server,
    reciever_position %in% input$reciever_position,
    passer_position %in% input$passer_position
  ) %>%
  group_by(serve_speed) %>%
  summarize(sample_size = n())


sample_size_table %>%
  ggplot(aes(x = serve_speed, y = sample_size)) +
  geom_bar(
    stat = "identity",
    color = "#002145",
    fill = "#F2A900",
    width = 1
  ) +
  labs(x = "Serve Speed (km/h)", y = "Sample Size") +
  geom_text(aes(label = sample_size), vjust = -0.5, size = 2.5) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme_bw() +
  theme(
    axis.title.x = element_text(colour = "#002145"),
    axis.title.y = element_text(colour = "#002145"),
    plot.title = element_text(colour = "#F2A900")
  )
