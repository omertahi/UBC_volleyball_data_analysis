# load relevant packages -----------------------------------------------------#
library(tidyverse)
library(readxl)
library(gridExtra)
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

# check main serve type for each server --------------------------------------#
main_serve_type <- 
  data %>%
  group_by(server, serve_type) %>% 
  summarise(n = n()) %>% 
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

# calculate average point scoring probability for each serve speed per player #
avg_point_prob_data <- 
  point_prob_data %>%
  group_by(server, serve_speed) %>% 
  summarize(avg_prob = mean(point_probability)) %>% 
  arrange(server)
#-----------------------------------------------------------------------------#

# perform k-smoothing on serve_speed vs avg_prob -----------------------------#
gaussian <- 
  avg_point_prob_data %>%
  group_by(server) %>% 
  summarize(gaussian = ksmooth(x = serve_speed,
                               y = avg_prob,
                               kernel = "normal",
                               bandwidth = 10,
                               n.points = n())) %>% 
  pivot_wider(names_from = server,
              values_from = gaussian) %>% 
  unnest()
#-----------------------------------------------------------------------------#

# wrangle ksmooth data to obtain most optimal serve speeds for each server ---#
top2_serve_velocity <- 
  gaussian %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "server") %>% 
  unnest(cols = c("V1", "V2")) %>% 
  mutate(server = as.factor(server),
         serve_velocity = V1,
         point_prob = V2) %>% 
  select(-V1, -V2) %>% 
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



# Create a plots of avg point prob vs serve_speed for each player ------------#
## SERVER 0
server_0_analysis <- 
  avg_point_prob_data %>%
  filter(server == 0) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`0`$x, y = gaussian$`0`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_0_analysis
#-----------------------------------------------------------------------------#

## SERVER 1
server_1_analysis <- 
  avg_point_prob_data %>%
  filter(server == 1) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`1`$x, y = gaussian$`1`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_1_analysis
#-----------------------------------------------------------------------------#

## SERVER 2
server_2_analysis <- 
  avg_point_prob_data %>%
  filter(server == 2) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`2`$x, y = gaussian$`2`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_2_analysis
#-----------------------------------------------------------------------------#

## SERVER 3
server_3_analysis <- 
  avg_point_prob_data %>%
  filter(server == 3) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`3`$x, y = gaussian$`3`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_3_analysis
#-----------------------------------------------------------------------------#

## SERVER 4
server_4_analysis <- 
  avg_point_prob_data %>%
  filter(server == 4) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`4`$x, y = gaussian$`4`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_4_analysis
#-----------------------------------------------------------------------------#

## SERVER 5
server_5_analysis <- 
  avg_point_prob_data %>%
  filter(server == 5) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`5`$x, y = gaussian$`5`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_5_analysis
#-----------------------------------------------------------------------------#

## SERVER 7
server_7_analysis <- 
  avg_point_prob_data %>%
  filter(server == 7) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`7`$x, y = gaussian$`7`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_7_analysis
#-----------------------------------------------------------------------------#

## SERVER 8
server_8_analysis <- 
  avg_point_prob_data %>%
  filter(server == 8) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`8`$x, y = gaussian$`8`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_8_analysis
#-----------------------------------------------------------------------------#

## SERVER 9
server_9_analysis <- 
  avg_point_prob_data %>%
  filter(server == 9) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`9`$x, y = gaussian$`9`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_9_analysis
#-----------------------------------------------------------------------------#

## SERVER 10
server_10_analysis <- 
  avg_point_prob_data %>%
  filter(server == 10) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`10`$x, y = gaussian$`10`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_10_analysis
#-----------------------------------------------------------------------------#

## SERVER 11
server_11_analysis <- 
  avg_point_prob_data %>%
  filter(server == 11) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`11`$x, y = gaussian$`11`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_11_analysis
#-----------------------------------------------------------------------------#

## SERVER 12
server_12_analysis <- 
  avg_point_prob_data %>%
  filter(server == 12) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`12`$x, y = gaussian$`12`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_12_analysis
#-----------------------------------------------------------------------------#

## SERVER 13
server_13_analysis <- 
  avg_point_prob_data %>%
  filter(server == 13) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`13`$x, y = gaussian$`13`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_13_analysis
#-----------------------------------------------------------------------------#

## SERVER 14
server_14_analysis <- 
  avg_point_prob_data %>%
  filter(server == 14) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`14`$x, y = gaussian$`14`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_14_analysis
#-----------------------------------------------------------------------------#

## SERVER 15
server_15_analysis <- 
  avg_point_prob_data %>%
  filter(server == 15) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`15`$x, y = gaussian$`15`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_15_analysis
#-----------------------------------------------------------------------------#

## SERVER 16
server_16_analysis <- 
  avg_point_prob_data %>%
  filter(server == 16) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`16`$x, y = gaussian$`16`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_16_analysis
#-----------------------------------------------------------------------------#

## SERVER 18
server_18_analysis <- 
  avg_point_prob_data %>%
  filter(server == 18) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`18`$x, y = gaussian$`18`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_18_analysis
#-----------------------------------------------------------------------------#

## SERVER 20
server_20_analysis <- 
  avg_point_prob_data %>%
  filter(server == 20) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`20`$x, y = gaussian$`20`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_20_analysis
#-----------------------------------------------------------------------------#

