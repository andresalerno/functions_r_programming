
# 1) Function Fundamentals ----
coin_sides <- c("head", "tail")

sample(coin_sides, 1)

toss_coin <- function(){
  coin_sides <- c("head", "tail")
  sample(coin_sides, 1)
}

toss_coin()

# 2) Creating function arguments ----

toss_coin <- function(n_flips){
  coin_sides <- c("head", "tail")
  sample(coin_sides, size = 10, replace = TRUE)
}

toss_coin(10)

# 3) Creating more arguments in a function (weights) ----

# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1-p_head)
  # Modify the sampling to be weighted
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(10, 0.8)


# 4) Renaming GLM ----

if(!require('COUNT')) {
  install.packages('COUNT')
  library('COUNT')
}


data(loomis)

# 5) Fetching the dataset ----

setwd("~/Private/Salerno/Profissional/1) AS PARTNERS/Data Fintech Solutions/projetos/functions/functions_r_programming")

snake_river_visits <- readxl::read_xlsx('snake_river_visits.xlsx')

snake_river_explanatory <- readxl::read_xlsx('snake_river_explanatory.xlsx')

# 6) Run a generalized linear regression ----
glm(
  # Model no. of visits vs. gender, income, travel
  n_visits ~ gender + income + travel, 
  # Use the snake_river_visits dataset
  data = snake_river_visits, 
  # Make it a Poisson regression
  family = "poisson"
)

# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# 7) Re-run the Poisson regression, using your function ----
library(magrittr)
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_explanatory %>%
  dplyr::mutate(predicted_n_visits = predict(model, ., type = "response"))%>%
  dplyr::arrange(desc(predicted_n_visits))