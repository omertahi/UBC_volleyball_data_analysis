library(tidyverse)
library(readxl)

## Read data
volleyball_data <- read_excel("SAMPLE DATA BAD.xlsx")

## Combine cut-spin and spin as spin in the serve_type column
data <- 
  volleyball_data %>% 
  mutate(serve_type = replace(volleyball_data$serve_type,
                              volleyball_data$serve_type == "cut_spin",
                              "spin"),
         server = as.numeric(server)) %>% 
  filter(!is.na(server))

## Check counts of each serve_type
count_serve_type <- 
  data %>% 
  group_by(serve_type) %>% 
  summarize(counts = n())

## Check main serve type for each server
main_serve_type <- 
  data %>%
  group_by(server, serve_type) %>% 
  summarise(n = n()) %>% 
  group_by(server) %>% 
  slice_max(order_by = n)


## Semi-join data with main_serve_type_for_player to get corresponding complete data rows
main_serve_type_data <- 
  data %>% 
  semi_join(main_serve_type,
            by = c("server", "serve_type"))

## Create new column to assign probabilities for each serve outcome
point_prob_data <- 
  main_serve_type_data %>%
  mutate(point_probability = case_when(serve_outcome == 0 ~ 1,
                                       serve_outcome == 1 ~ 0.614,
                                       serve_outcome == 2 ~ 0.473,
                                       serve_outcome == 3 ~ 0.329,
                                       serve_outcome == 4 ~ 0.366,
                                       TRUE ~ 0)
  )

avg_point_prob_data <- 
  point_prob_data %>%
  group_by(server, serve_speed) %>% 
  summarize(avg_prob = mean(point_probability)) %>% 
  arrange(server)

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
  

## Create a plot of avg_prob of scoring a point against serve_speed for a player
### SERVER 0
server_0_analysis <- 
  avg_point_prob_data %>%
  filter(server == 0) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`0`$x, y = gaussian$`0`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_0_analysis
##################

### SERVER 1
server_1_analysis <- 
  avg_point_prob_data %>%
  filter(server == 1) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`1`$x, y = gaussian$`1`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_1_analysis
##################

### SERVER 2
server_2_analysis <- 
  avg_point_prob_data %>%
  filter(server == 2) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`2`$x, y = gaussian$`2`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_2_analysis
##################

### SERVER 3
server_3_analysis <- 
  avg_point_prob_data %>%
  filter(server == 3) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`3`$x, y = gaussian$`3`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_3_analysis
##################

### SERVER 4
server_4_analysis <- 
  avg_point_prob_data %>%
  filter(server == 4) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`4`$x, y = gaussian$`4`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_4_analysis
##################

### SERVER 5
server_5_analysis <- 
  avg_point_prob_data %>%
  filter(server == 5) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`5`$x, y = gaussian$`5`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_5_analysis
##################

### SERVER 7
server_7_analysis <- 
  avg_point_prob_data %>%
  filter(server == 7) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`7`$x, y = gaussian$`7`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_7_analysis
##################

### SERVER 8
server_8_analysis <- 
  avg_point_prob_data %>%
  filter(server == 8) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`8`$x, y = gaussian$`8`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_8_analysis
##################

### SERVER 9
server_9_analysis <- 
  avg_point_prob_data %>%
  filter(server == 9) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`9`$x, y = gaussian$`9`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_9_analysis
##################

### SERVER 10
server_10_analysis <- 
  avg_point_prob_data %>%
  filter(server == 10) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`10`$x, y = gaussian$`10`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_10_analysis
##################

### SERVER 11
server_11_analysis <- 
  avg_point_prob_data %>%
  filter(server == 11) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`11`$x, y = gaussian$`11`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_11_analysis
##################

### SERVER 12
server_12_analysis <- 
  avg_point_prob_data %>%
  filter(server == 12) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`12`$x, y = gaussian$`12`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_12_analysis
##################

### SERVER 13
server_13_analysis <- 
  avg_point_prob_data %>%
  filter(server == 13) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`13`$x, y = gaussian$`13`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_13_analysis
##################

### SERVER 14
server_14_analysis <- 
  avg_point_prob_data %>%
  filter(server == 14) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`14`$x, y = gaussian$`14`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_14_analysis
##################

### SERVER 15
server_15_analysis <- 
  avg_point_prob_data %>%
  filter(server == 15) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`15`$x, y = gaussian$`15`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_15_analysis
##################

### SERVER 16
server_16_analysis <- 
  avg_point_prob_data %>%
  filter(server == 16) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`16`$x, y = gaussian$`16`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_16_analysis
##################

### SERVER 18
server_18_analysis <- 
  avg_point_prob_data %>%
  filter(server == 18) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`18`$x, y = gaussian$`18`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_18_analysis
##################

### SERVER 20
server_20_analysis <- 
  avg_point_prob_data %>%
  filter(server == 20) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  geom_line(aes(x = gaussian$`20`$x, y = gaussian$`20`$y)) +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

server_20_analysis
##################

# pass.score.med <- mean.all %>%
#   ggplot(aes(x = velocity, y = prob, color=type)) +
#   geom_line(aes(x = c$x, y= c$y, col = "Float"),size=3) +
#   geom_line(aes(x = d$x, y= d$y, col = "Hybrid"),size=3) +
#   geom_line(aes(x = e$x, y= e$y, col = "Spin"),size=3) +
#   labs(x = "Velocity (km/h)",
#        y = "Probability of Winning the Point",
#        title = "Probability of Winning the Point as a Function of the Serve",
#        type = "Legend") +
#   #scale_color_manual(values = c("Float", "Hybrid", "Spin"))+
#   #scale_colour_discrete(name = "Type:", labels = c("Float", "Hybrid", "Spin")) +
#   theme(legend.position = "right")+
#   theme(legend.position = "bottom",
#         axis.text=element_text(size=16),
#         axis.title=element_text(size=18),
#         plot.title = element_text(size=25),
#         legend.title = element_text(size=15),
#         legend.text = element_text(size=15))
# 
# labels = c(f = "Float", h = "Hybrid", s = "Spin")
# pass.score.med
