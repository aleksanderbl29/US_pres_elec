# @knitr read_data_create_functions

# originally by Pierre Kremp - updated by Derek Beach

# SYNTAX
# harris_state: a character vector of states already called for Harris;

# trump_state: a character vector of states already called for Trump;

# harris_scores_list: a list of elements named with 2-letter state abbreviations; each element should be a numeric vector of length 2 containing the lower and upper bound of the interval in which Harris share of the Harris + Trump score is expected to fall.

# target_nsim: an integer indicating the minimum number of samples that should be drawn from the conditional distribution (set to 1000 by default).

# show_all_states: a logical value indicating whether to output the state by state expected win probabilities (set to FALSE by default).


library(mvtnorm)
# library(knitr)
out <- read_rds("runs/new_output.rds")

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) 1/(1 + exp(-x))
## Extract predicted score
predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]

# this is an 'array' with dimensions: sims x days x states
## Get all electoral votes
ev <- states2020$ev
names(ev) <- states2020$state
ev["ME"] <- 1
ev["NE"] <- 2
ev <- c(ev, "ME1" = 1, "ME2" = 1, "NE1" = 1, "NE2" = 1, "NE3" = 1)
# state-level latent harris support by simulation x day x state
state_mat <- pblapply(1:dim(predicted_score)[3],
                      function(x){
                        p_harris <- predicted_score[,,x]
                        p_harris <- p_harris %>%
                          as.data.frame() %>%
                          select(ncol(.))
                      }) %>% do.call('bind_cols',.)
# 'draws' is data set with ~13 million rows
# it contains simulation draws of each day's & state's
# latent support for harris.
# Basically, it just unwraps the 'predicted_score' object
# and stretches it into one long data frame
# name state sim results
colnames(state_mat) <- state_abb_list
# add special ME/NE districts (2020 differences)
state_mat <- state_mat %>%
  mutate(
    ME1 = ME + rnorm(nrow(.), .06, .0075),
    ME2 = ME + rnorm(nrow(.), -.07, .0075),
    NE1 = NE + rnorm(nrow(.), .03, .0075),
    NE2 = NE + rnorm(nrow(.),  .12, .0075),
    NE3 = NE + rnorm(nrow(.), -.14, .0075))
Sigma <- cov(logit(state_mat))
mu <- colMeans(logit(state_mat))
names(mu) <- colnames(state_mat)
draw_samples <- function(harris_state = NULL, trump_state = NULL, states = NULL,
                         upper_harris = NULL, lower_harris = NULL, print_acceptance = FALSE, target_nsim = 1000){
  sim <- matrix(NA, nr = 1, nc = length(mu))
  n <- 0
  while(nrow(sim) < target_nsim){
    # randomly sample from the posterior distribution and reject when constraints are not met
    n <- n + 1
    proposals <- inv_logit(rmvnorm(1e5, mu, Sigma, method = "svd")) # "DC" is pretty much uncorrelated
    colnames(proposals) <- names(mu)
    if (!is.null(harris_state)) proposals[which(proposals[,harris_state] < .5)] <- NA
    if (!is.null(  trump_state)) proposals[which(proposals[,  trump_state] > .5)] <- NA
    if (!is.null(        states)){
      for (s in states){
        proposals[which(proposals[, s] > upper_harris[s] |
                          proposals[, s] < lower_harris[s])] <- NA
      }
    }
    reject <- apply(proposals, 1, function(x) any(is.na(x)))
    sim <- rbind(sim, proposals[!reject,])
    if (nrow(sim) < target_nsim & nrow(sim)/(nrow(proposals)*n) < 1-99.99/100){
      stop(paste("rmvnorm() is working hard... but more than 99.99% of the samples are rejected; you should relax some contraints.", sep = ""))
    }
  }
  return(list("matrix" = sim[-1,], "acceptance_rate" = nrow(sim)/(nrow(proposals)*n)))
}
update_prob <- function(harris_state = NULL,
                        trump_state = NULL,
                        harris_scores_list = NULL,
                        target_nsim = 1000,
                        show_all_states = FALSE){
  states <- names(harris_scores_list)
  lower_harris <- sapply(harris_scores_list, function(x) x[1]/100)
  upper_harris <- sapply(harris_scores_list, function(x) x[2]/100)
  sim <- draw_samples(harris_state = harris_state, trump_state = trump_state, states = states,
                      upper_harris = upper_harris, lower_harris = lower_harris,
                      target_nsim = target_nsim)
  ev_dist <- (sim[["matrix"]] > .5) %*% ev
  state_win <- colMeans(sim[["matrix"]] > .5)
  p <- mean(ev_dist >= 270)
  sd <- sqrt(p*(1-p)/length(ev_dist))
  if (show_all_states){
    cat("Pr(Harris wins) by state, in %:\n")
    print(t(round(100*state_win)))
    cat("--------\n")
  }
  cat(paste("Pr(Harris wins the electoral college) = ", round(100*p), "%\n[nsim = ", length(ev_dist), "; se = ", round(sd*100,1), "%]", sep = ""))
  if (show_all_states) cat("\n--------\n")
}

## to use update_prob command: update_prob(harris_state = c("[state abbr]"))
## example : update_prob(trump_state = c("MO", "UT", "AR", "NC"), harris_state = c("PA"))