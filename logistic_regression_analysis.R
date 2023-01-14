# load relevant packages -----------------------------------------------------#
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggspectra)
library(glmnet)
#-----------------------------------------------------------------------------#

# read data ------------------------------------------------------------------#
volleyball_data <- read_excel("SAMPLE DATA- TWU FRI.xlsx")
#-----------------------------------------------------------------------------#

# combine cut-spin and spin as spin in the serve_type column -----------------#
`%notin%` <- Negate(`%in%`)

data_clean <- 
  volleyball_data %>% 
  mutate(serve_type = replace(volleyball_data$serve_type,
                              volleyball_data$serve_type == "cut_spin",
                              "spin"),
         server = as.numeric(server),
         point_outcome = case_when(point_outcome == "TRUE" ~ 1,
                                   point_outcome == "FALSE" ~ 0)) %>% 
  filter(!is.na(server),
         !is.na(point_outcome),
         server %notin% c(0, 99)) %>% 
  select(-c(point, serve_outcome_error, passer)) %>% 
  mutate(ID = 1:nrow(.),
         across(.cols = c(server:serve_tape, serve_outcome:seam_positioning), as.factor))

# model selection -------------------------------------------------------------#

set.seed(1234)

training_data <- sample_n(data_clean,
                          size = nrow(data_clean) * 0.70,
                          replace = FALSE)

testing_data <- anti_join(data_clean,
                          training_data,
                          by = "ID")

training_vb <- training_data |> select(-ID)
testing_vb <- testing_data |> select(-ID)

## Create training matrix for input variables
X_train <- model.matrix(object = point_outcome ~ . - 1,
                        data = training_vb)

## Create training matrix for response variable
Y_train <- 
  training_vb %>%
  select(point_outcome) %>%
  as.matrix(ncol = 1)

## Create testing matrix for input variables
X_test <- model.matrix(object = point_outcome ~ . - 1,
                       data = testing_vb)

## Create testing matrix for response variable
Y_test <- 
  testing_vb %>%
  select(point_outcome) %>%
  as.matrix(ncol = 1)

## Perform Lasso regression to select input variables
cv_lambda_LASSO <- cv.glmnet(
  x = X_train,
  y = Y_train,
  alpha = 1,
  lambda = NULL,
  nfolds = 10
)

## Obtain the plot
plot(cv_lambda_LASSO, main = "Lambda selection by CV with LASSO\n\n")