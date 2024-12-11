run_model <- function(stan_data,
                      model_file,
                      n_chains,
                      n_warmup,
                      n_iter,
                      n_cores,
                      n_refresh) {

  out <- stan(
    file = model_file,
    data = stan_data,
    chains = n_chains,
    warmup = n_warmup,
    iter = n_iter,
    cores = n_cores,
    refresh = n_refresh
  )

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

  saveRDS(draws, "_output/draws.rds")
}




