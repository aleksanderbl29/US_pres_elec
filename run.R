# First ensure that all packages are installed.
# renv::restore()

# Source all custom functions.
targets::tar_source()

# Load all the needed libraries
source("_targets_packages.R")

# Run the first part of the pipeline.
# The targets to run are defined in `R/helper.R` which is sourced above.
targets::tar_make(names = all_of(first_targets))

# Load the ouput
targets::tar_load_everything()

# Run the STAN model
out <- stan(
  file = model_file,
  data = stan_data,
  chains = n_chains,
  warmup = n_warmup,
  iter = n_iter,
  cores = n_cores,
  refresh = n_refresh
)

# Post process the stan output
predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]

# Extracting Harris support for each day in each state. This means that each row
# is one sample of predicted Harris-support in a given state on a given date.
draws <- pblapply(1:dim(predicted_score)[3],
                  function(x){
                    p_harris <- predicted_score[,,x]
                    p_harris <- p_harris %>%
                      as.data.frame() %>%
                      mutate(draw = row_number()) %>%
                      gather(t, p_harris, 1:(ncol(.)-1)) %>%
                      mutate(t = as.numeric(gsub('V','',t)) + min(df$begin),
                             state = state_abb_list[x])
                  }) %>% do.call('bind_rows',.)


# Also adding EVs to draws
draws <- draws %>%
  left_join(states2020 %>% select(state, ev), by='state')

forecast <- pblapply(1:dim(predicted_score)[3], function(x){
  # pred is mu_a + mu_b for the past, just mu_b for the future
  temp <- predicted_score[,,x]

  # put 90% credible intervals in tibble
  tibble(low_dem = apply(temp,2,function(x){(quantile(x,0.05))}),
         high_dem = apply(temp,2,function(x){(quantile(x,0.95))}),
         mean_dem = apply(temp,2,function(x){(mean(x))}),
         prob_dem = apply(temp,2,function(x){(mean(x>0.5))}),
         low_rep = apply(1-temp,2,function(x){(quantile(x,0.05))}),
         high_rep = apply(1-temp,2,function(x){(quantile(x,0.95))}),
         mean_rep = apply(1-temp,2,function(x){(mean(x))}),
         prob_rep = apply(1-temp,2,function(x){(mean(x>0.5))}),
         state = x)
}) %>% do.call('bind_rows',.)

# Adding state names and dates
forecast$state = state_abb_list[forecast$state]
forecast <- forecast %>%
  group_by(state) %>%
  mutate(t = row_number() + min(df$begin)) %>%
  ungroup()

# Adding national-level values to the forecast-object - for the national we also
# have predicted EVs - this does not make sense to include at the state level.
# IMPORTANT!: prob is here the probability of an electoral college victory, while
# at the state level it is the probability of winning the state vote !!!!!!
forecast <- forecast %>%
  bind_rows(
    pblapply(1:dim(predicted_score)[1], function(x){

      # each row is a day for a particular draw
      temp <- predicted_score[x,,] %>% as.data.frame()
      names(temp) <- state_abb_list

      # for each row, get weigted natl vote
      tibble(natl_dem = apply(temp,MARGIN = 1,function(y){weighted.mean(y,state_weights)}),
             natl_rep = apply(1-temp,MARGIN = 1,function(y){weighted.mean(y,state_weights)})) %>%
        mutate(t = row_number() + min(df$begin)) %>%
        mutate(draw = x)
    }) %>% do.call('bind_rows',.) %>%
      group_by(t) %>%
      summarise(low_dem = quantile(natl_dem,0.05),
                high_dem = quantile(natl_dem,0.95),
                mean_dem = mean(natl_dem),
                low_rep = quantile(natl_rep,0.05),
                high_rep = quantile(natl_rep,0.95),
                mean_rep = mean(natl_rep)) %>%
      mutate(state = '--') %>%

      # Adding electoral votes
      left_join(
        draws %>%
          group_by(t,draw) %>%
          summarise(dem_ev = sum(ev * (p_harris > 0.5)),
                    rep_ev = sum(ev * (p_harris < 0.5))) %>%
          group_by(t) %>%
          summarise(mean_dem_ev = mean(dem_ev),
                    median_dem_ev = median(dem_ev),
                    high_dem_ev = quantile(dem_ev,0.975),
                    low_dem_ev = quantile(dem_ev,0.025),
                    prob_dem = mean(dem_ev >= 270),
                    mean_rep_ev = mean(rep_ev),
                    median_rep_ev = median(rep_ev),
                    high_rep_ev = quantile(rep_ev,0.975),
                    low_rep_ev = quantile(rep_ev,0.025),
                    prob_rep = mean(rep_ev >= 270),
                    prob_draw = mean(rep_ev == dem_ev)),
        by = "t"))


# Saving results ----------------------------------------------------------

saveRDS(draws, "forecast/draws.RDS")
saveRDS(forecast, "forecast/forecast.RDS")
