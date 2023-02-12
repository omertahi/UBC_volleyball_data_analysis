# load relevant packages -----------------------------------------------------#
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggspectra)
#-----------------------------------------------------------------------------#

# read data ------------------------------------------------------------------#
volleyball_data <- 
  read_excel("UBC volleyball data input.xlsx",
             sheet = "Consolidated Data") %>% 
  mutate(serve_type = gsub("_", " ", .$serve_type))

#-----------------------------------------------------------------------------#

# combine cut-spin and spin as spin in the serve_type column -----------------#
`%notin%` <- Negate(`%in%`)

data <- 
  volleyball_data %>% 
  mutate(serve_type = replace(volleyball_data$serve_type,
                              volleyball_data$serve_type == "Cut Spin",
                              "Spin"),
         across(.cols = c(server, server_position, serve_speed), as.numeric)) %>% 
  filter(!is.na(server),
         !is.na(serve_speed),
         server %notin% c(0, 99))
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
            error_perc = sum(pass_outcome == "E")/n) %>% 
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
pp_data <- 
  data %>% 
  mutate(point_probability = case_when(pass_outcome == 0 ~ 1,
                                       pass_outcome == 1 ~ 0.640,
                                       pass_outcome == 2 ~ 0.416,
                                       pass_outcome == 3 ~ 0.382,
                                       pass_outcome == 4 ~ 0.324,
                                       TRUE ~ 0)
  )
#-----------------------------------------------------------------------------#

# calculate average point scoring probability and ----------------------------#
# average error percentage for each serve speed per player -------------------#
# calculate average point scoring probability and
# average error percentage for each serve speed per player
avg_pp_data <-
  pp_data %>%
  group_by(server, serve_speed) %>%
  summarize(
    avg_prob = mean(point_probability),
    avg_err_perc = sum(point_probability == 0) / n(),
    avg_ace_perc = sum(point_probability == 1) / n()
  )
#-----------------------------------------------------------------------------#

# perform k-smoothing on serve_speed vs avg_prob & error_perc ----------------#
## perform k-smoothing
ksmoothed_data <-
  avg_pp_data %>%
  group_by(server) %>% 
  summarize(
    ksmooth_point_prob = list(ksmooth(x = serve_speed,
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
                                    n.points = n()))) %>%
  unnest_wider(col = c(ksmooth_point_prob,
                       ksmooth_error_perc,
                       ksmooth_ace_perc),
               names_sep = "") %>%
  unnest(cols = everything()) %>%
  select(-c("ksmooth_error_percx", "ksmooth_ace_percx")) %>%
  setNames(., c("server", "serve_velocity", "point_prob", "error_perc", "ace_perc")) %>% 
  pivot_longer(
    cols = c(point_prob, error_perc, ace_perc),
    names_to = "perc_type",
    values_to = "percentage")
#-----------------------------------------------------------------------------#

# wrangle ksmooth data to obtain most optimal serve speeds for each server ---#
top2_serve_velocity <- 
  ksmoothed_data %>% 
  pivot_wider(names_from = perc_type,
              values_from = percentage) %>% 
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


# earn_percentage of server #7 based on where he serves to  ------------------#
prop_points_won <- 
  data %>% 
  filter(server == 7,
         !is.na(passer_position)) %>% 
  group_by(passer_position) %>% 
  summarise(earn_percentage = sum(point_outcome)/n())

view(prop_points_won)


## check counts of each position served to
t <- data %>% 
  filter(server == 7) %>% 
  group_by(passer_position) %>% 
  summarise(count = n())

view(t)

## Notes: 
## - Too many NA values in point_outcome column preventing proper calculations
## - passer_position has null and NA values. Should they be merged?

#-----------------------------------------------------------------------------#
library(MASS)
roster_number = 7
point_data <- volleyball_data %>% drop_na(point_outcome)
point_data$server_to<- as.factor(point_data$server_position)
# 
# 
full_logistic <- point_data  %>%
  mutate(pass_outcome = coalesce(pass_outcome, "err")) 
# 
# 
all_player_logistic <- glm(point_outcome ~ server_position + reciever_position + serve_speed + passer_position+ passer_hands_arms , data=  full_logistic, family= binomial)
summary(all_player_logistic)
full_backwards <- all_player_logistic %>% stepAIC(trace= FALSE)
coef(full_backwards)
