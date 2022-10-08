library(tidyverse)
library(readxl)

## Read data
volleyball_data <- read_excel("SAMPLE DATA BAD.xlsx")

## Combine cut-spin and spin as spin in the serve_type column
data <- 
  volleyball_data %>% 
  mutate(serve_type = replace(volleyball_data$serve_type,
                              volleyball_data$serve_type == "cut_spin",
                              "spin"))

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
final_data <- 
  main_serve_type_data %>%
  mutate(point_probability = case_when(serve_outcome == 0 ~ 1,
                                       serve_outcome == 1 ~ 0.638,
                                       serve_outcome == 2 ~ 0.549,
                                       serve_outcome == 3 ~ 0.364,
                                       serve_outcome == 4 ~ 0.364,
                                       TRUE ~ 0)
  )

## Create a plot of avg_prob of scoring a point against serve_speed for a player
serve_point_score_analysis <- 
  final_data %>%
  filter(server == 2) %>% 
  group_by(serve_speed) %>% 
  summarize(avg_prob = mean(point_probability)) %>% 
  ggplot(aes(x = serve_speed, y = avg_prob)) +
  geom_point() +
  labs(x = "Serve Speed (km/h)", y = "Probability of scoring a point")

serve_point_score_analysis


# Pass-Score Analysis:

## Medium Smoothing (k=10):

mean.all <- prob.data %>%
  group_by(velocity, type) %>%
  summarise(prob = mean(outcome))

c <- ksmooth(x = mean.all[(mean.all$type == "f"),]$velocity,
             y = mean.all[(mean.all$type == "f"),]$prob,
             kernel = "normal",
             bandwidth = 10,
             n.points = nrow(mean.all))

d <- ksmooth(x = mean.all[(mean.all$type == "h"),]$velocity,
             y = mean.all[(mean.all$type == "h"),]$prob,
             kernel = "normal",
             bandwidth = 10,
             n.points = nrow(mean.all))

e <- ksmooth(x = mean.all[(mean.all$type == "s"),]$velocity,
             y = mean.all[(mean.all$type == "s"),]$prob,
             kernel = "normal",
             bandwidth = 10,
             n.points = nrow(mean.all))

pass.score.med <- mean.all %>%
  ggplot(aes(x = velocity, y = prob, color=type)) +
  geom_line(aes(x = c$x, y= c$y, col = "Float"),size=3) +
  geom_line(aes(x = d$x, y= d$y, col = "Hybrid"),size=3) +
  geom_line(aes(x = e$x, y= e$y, col = "Spin"),size=3) +
  labs(x = "Velocity (km/h)",
       y = "Probability of Winning the Point",
       title = "Probability of Winning the Point as a Function of the Serve",
       type = "Legend") +
  #scale_color_manual(values = c("Float", "Hybrid", "Spin"))+
  #scale_colour_discrete(name = "Type:", labels = c("Float", "Hybrid", "Spin")) +
  theme(legend.position = "right")+
  theme(legend.position = "bottom",
        axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=25),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))

labels = c(f = "Float", h = "Hybrid", s = "Spin")
pass.score.med
