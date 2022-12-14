# load relevant packages -----------------------------------------------------#
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggspectra)
#-----------------------------------------------------------------------------#

# read data ------------------------------------------------------------------#
volleyball_data <- read_excel("SAMPLE DATA BAD.xlsx")
#-----------------------------------------------------------------------------#

# combine cut-spin and spin as spin in the serve_type column -----------------#
`%notin%` <- Negate(`%in%`)

data <- 
  volleyball_data %>% 
  mutate(serve_type = replace(volleyball_data$serve_type,
                              volleyball_data$serve_type == "cut_spin",
                              "spin"),
         server = as.numeric(server)) %>% 
  filter(!is.na(server),
         server %notin% c(0, 4, 99, 19))
#-----------------------------------------------------------------------------#

# check counts of each serve_type --------------------------------------------#
count_serve_type <- 
  data %>% 
  group_by(serve_type) %>% 
  summarize(counts = n())
#-----------------------------------------------------------------------------#

# check main serve type for each server ----------------#
main_serve_type <- 
  data %>%
  group_by(server, serve_type) %>% 
  summarise(n = n(),
            error_perc = sum(is.na(serve_outcome))/n) %>% 
  group_by(server) %>% 
  slice_max(order_by = n)
#-----------------------------------------------------------------------------#

# semi-join data with main_serve_type_for_player to get corresponding complete data rows
main_serve_type_data <- 
  data %>% 
  semi_join(main_serve_type,
            by = c("server", "serve_type"))
#-----------------------------------------------------------------------------#

# create new column to assign probabilities for each serve outcome -----------#
point_prob_data <- 
  main_serve_type_data %>%
  mutate(point_probability = case_when(serve_outcome == 0 ~ 1,
                                       serve_outcome == 1 ~ 0.614,
                                       serve_outcome == 2 ~ 0.473,
                                       serve_outcome == 3 ~ 0.329,
                                       serve_outcome == 4 ~ 0.366,
                                       TRUE ~ 0)
  )
#-----------------------------------------------------------------------------#

# calculate average point scoring probability and ----------------------------#
# average error percentage for each serve speed per player -------------------#
avg_point_prob_data <- 
  point_prob_data %>%
  group_by(server, serve_speed) %>% 
  summarize(avg_prob = mean(point_probability),
            avg_err_perc = sum(point_probability == 0)/n(),
            avg_ace_perc = sum(point_probability == 1)/n()) %>% 
  arrange(server)
#-----------------------------------------------------------------------------#

# perform k-smoothing on serve_speed vs avg_prob & error_perc ----------------#
ksmooth_values <- 
  avg_point_prob_data %>%
  group_by(server) %>% 
  summarize(ksmooth_point_prob = list(ksmooth(x = serve_speed,
                                              y = avg_prob,
                                              kernel = "normal",
                                              bandwidth = 7,
                                              n.points = n())), 
            ksmooth_error_perc = list(ksmooth(x = serve_speed,
                                              y = avg_err_perc,
                                              kernel = "normal",
                                              bandwidth = 7,
                                              n.points = n())),
            ksmooth_ace_perc = list(ksmooth(x = serve_speed,
                                            y = avg_ace_perc,
                                            kernel = "normal",
                                            bandwidth = 7,
                                            n.points = n()))
  ) %>% 
  unnest_wider(col = c(ksmooth_point_prob,
                       ksmooth_error_perc,
                       ksmooth_ace_perc),
               names_sep = "") %>% 
  unnest(col = -server) %>% 
  select(-4, -6) %>% 
  setNames(., c("server",
                "serve_velocity",
                "point_prob",
                "error_perc",
                "ace_perc")) 
#-----------------------------------------------------------------------------#

# wrangle ksmooth data to obtain most optimal serve speeds for each server ---#
top2_serve_velocity <- 
  ksmooth_values %>% 
  select(server, serve_velocity, point_prob) %>% 
  group_by(server) %>% 
  slice_max(order_by = point_prob, n = 2)
#-----------------------------------------------------------------------------#

# obtain most optimal serve speed per server ---------------------------------#  
optimal_serve_velocity <- 
  top2_serve_velocity %>% 
  slice(seq(1, nrow(.), 2))
#-----------------------------------------------------------------------------#

# obtain second most optimal serve speed per server --------------------------#
second_serve_velocity <- 
  top2_serve_velocity %>% 
  slice(seq(2, nrow(.), 2))
#-----------------------------------------------------------------------------#

# create top 2 most optimal serve speeds per server table --------------------#
plus_minus_2 <- function(x) {
  ## this function adds and subtracts 2 from the optimum serve speed to
  ## obtain the optimal serve speed range
  return(paste0("[", x-2, ", ", x+2, "]"))
}

optimal_serve_velocity_table <- 
  left_join(optimal_serve_velocity,
            second_serve_velocity,
            by = "server") %>% 
  mutate(optimal_serve_velocity = round(serve_velocity.x, 0),
         second_optimal_serve_velocity = round(serve_velocity.y, 0),
         optimal_point_prob = round(point_prob.x, 3),
         second_optimal_point_prob = round(point_prob.y, 3),
         optimal_velocity_range = sapply(optimal_serve_velocity, plus_minus_2),
         second_optimal_velocity_range = sapply(second_optimal_serve_velocity, plus_minus_2)) %>% 
  select(1, 6, 8, 10, 7, 9, 11)
#-----------------------------------------------------------------------------#  

# save table as .png image ---------------------------------------------------#
# png("volleyball_data.png",
#     height = 50*nrow(optimal_serve_velocity_table),
#     width = 200*ncol(optimal_serve_velocity_table))
# grid.table(optimal_serve_velocity_table)
# dev.off()
#-----------------------------------------------------------------------------#
