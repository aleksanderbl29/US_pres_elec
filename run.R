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
# targets::tar_load_everything()

if (!file.exists("data/forecast/forecast.RDS") || !file.exists("data/forecast/draws.RDS")) {

  # Run the stan model
  source("model.R")

  # Calculating credibility intervals
  mean_low_high <- function(draws, states, id){
    tmp <- draws
    draws_df <- data.frame(mean = inv.logit(apply(tmp, MARGIN = 2, mean)),
                           high = inv.logit(apply(tmp, MARGIN = 2, mean) + 1.96 * apply(tmp, MARGIN = 2, sd)),
                           low  = inv.logit(apply(tmp, MARGIN = 2, mean) - 1.96 * apply(tmp, MARGIN = 2, sd)),
                           state = states,
                           type = id)
    return(draws_df)
  }

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

  draws <- draws %>%
    left_join(states2020 %>% select(state, ev), by='state')

  forecast <- pblapply(1:dim(predicted_score)[3], function(x){
    temp <- predicted_score[,,x]

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

  forecast$state = state_abb_list[forecast$state]
  forecast <- forecast %>%
    group_by(state) %>%
    mutate(t = row_number() + min(df$begin)) %>%
    ungroup()

  forecast <- forecast %>%
    bind_rows(
      pblapply(1:dim(predicted_score)[1], function(x){

        temp <- predicted_score[x,,] %>% as.data.frame()
        names(temp) <- state_abb_list

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

  saveRDS(draws, "data/forecast/draws.RDS")
  saveRDS(forecast, "data/forecast/forecast.RDS")

  rm(draws, forecast)
}

# Run the rest of the pipeline
targets::tar_make(names = !all_of(first_targets))

# This works only on mac
# system("open synopsis/synopsis.pdf")
