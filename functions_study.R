
coin_sides <- c("head", "tail")

sample(coin_sides, 1)

toss_coin <- function(){
  coin_sides <- c("head", "tail")
  sample(coin_sides, 1)
}

toss_coin()

toss_coin <- function(n_flips){
  coin_sides <- c("head", "tail")
  sample(coin_sides, size = 10, replace = TRUE)
}

toss_coin(10)

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
