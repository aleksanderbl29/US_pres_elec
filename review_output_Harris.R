## econ_2024_review_output.R

#######################################################################
## Libraries
#######################################################################
{
  library(tidyverse, quietly = TRUE)
  library(rstan, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(gridExtra, quietly = TRUE)
  library(pbapply, quietly = TRUE)
  library(parallel, quietly = TRUE)
  library(boot, quietly = TRUE)
  library(lqmm, quietly = TRUE) # make.positive.definite
  library(gridExtra, quietly = TRUE)
  library(ggrepel, quietly = TRUE)
  library(ggplot2, quietly = TRUE)

}
#######################################################################


#######################################################################
## Read in a model
# plug in here the name of the model you plan to evaluate
# You can just delete the 'sprintf' part and write in, e.g.:
# read_rds("name_of_my_model.rds")
# out <- read_rds("_output/Model output.rds")
# out <- targets::tar_read(stan_run)
#######################################################################


#---------------------------------------------------------------------#
## NB: be sure you've initialized the data objects that go INTO the
## model estimation (like, 'states2016' etc.)
## we'll use those as we plot and interpret the model output
## to be sure you have them all, just run the entire code in
## 'econ_2020_model_run_adj.R' EXCEPT for actually running the
## model itself.
#---------------------------------------------------------------------#


#######################################################################
## helper function for calculating credible intervals
mean_low_high <- function(draws, states, id){
  tmp <- draws
  draws_df <- data.frame(mean = inv.logit(apply(tmp, MARGIN = 2, mean)),
                         high = inv.logit(apply(tmp, MARGIN = 2, mean) + 1.96 * apply(tmp, MARGIN = 2, sd)),
                         low  = inv.logit(apply(tmp, MARGIN = 2, mean) - 1.96 * apply(tmp, MARGIN = 2, sd)),
                         state = states,
                         type = id)
  return(draws_df)
}
#######################################################################



#######################################################################
## Get names of all estimated parameters
out@par_dims %>% names
#######################################################################



#######################################################################
# View single parameters (i.e. not the random effects)
# this can be edited by changing parameter names or
# adding lines that extract futher single parameters
tibble(
  logit_nat = rstan::extract(out, pars = "logit_pi_democrat_national")[[1]],
  logit_state = rstan::extract(out, pars = "logit_pi_democrat_state")[[1]],
  state_cov = rstan::extract(out, pars = "sigma_rho")[[1]]
) %>%
  gather(parameter, value) %>%
  ggplot(., aes(x = value)) +
  geom_histogram(binwidth = 0.001) +
  facet_grid(rows = vars(parameter),
             scales = "free")
#######################################################################



#######################################################################
## GET ELECTORAL VOTE PREDICTION
#######################################################################

## Extract predicted score
predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]
# this is an 'array' with dimensions: sims x days x states


# state-level latent harris support by simulation x day x state
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
# 'draws' is data set with ~13 million rows
# it contains simulation draws of each day's & state's
# latent support for harris.
# Basically, it just unwraps the 'predicted_score' object
# and stretches it into one long data frame


# final EV distribution
final_evs <- draws %>% as_tibble %>%
  left_join(states2020 %>% select(state, ev), by='state') %>%
  filter(t == max(t)) %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev * (p_harris > 0.5)))


#######################################################################
# P(harris win) x state -- PLUS credible intervals
#######################################################################
p_harris <- pblapply(1:dim(predicted_score)[3],
                     function(x){
                       # pred is mu_a + mu_b for the past, just mu_b for the future
                       temp <- predicted_score[,,x]

                       # put 90% credible intervals in tibble
                       tibble(low = apply(temp,2,function(x){(quantile(x,0.05))}),
                              high = apply(temp,2,function(x){(quantile(x,0.95))}),
                              mean = apply(temp,2,function(x){(mean(x))}),
                              prob = apply(temp,2,function(x){(mean(x>0.5))}),
                              state = x)

                     }) %>% do.call('bind_rows',.)

## add state names to p_harris
p_harris$state = state_abb_list[p_harris$state]

## add proper dates to p_harris
p_harris <- p_harris %>%
  group_by(state) %>%
  mutate(t = row_number() + min(df$begin)) %>%
  ungroup()

# get national-level p_harris
p_harris_natl <- pblapply(1:dim(predicted_score)[1],
                          function(x){
                            # each row is a day for a particular draw
                            temp <- predicted_score[x,,] %>% as.data.frame()
                            names(temp) <- state_abb_list

                            # for each row, get weigted natl vote
                            tibble(natl_vote = apply(temp,MARGIN = 1,function(y){weighted.mean(y,state_weights)})) %>%
                              mutate(t = row_number() + min(df$begin)) %>%
                              mutate(draw = x)
                          }) %>% do.call('bind_rows',.)

# insert 90% credible intervals
p_harris_natl <- p_harris_natl %>%
  group_by(t) %>%
  summarise(low = quantile(natl_vote,0.05),
            high = quantile(natl_vote,0.95),
            mean = mean(natl_vote),
            prob = mean(natl_vote > 0.5)) %>%
  mutate(state = '--')

# bind state and national vote
p_harris <- p_harris %>%
  bind_rows(p_harris_natl) %>%
  arrange(desc(mean))


#################MODELS ###########


#######################################################################

#######################################################################
# look at some important example states
ex_states <- c('IA','FL','OH','WI','MI','PA','AZ','NC','NH','TX','GA','MN')

p_harris %>%
  filter(t == RUN_DATE,state %in% c(ex_states,'--')) %>%
  mutate(se = (high - mean) / 1.68) %>%
  dplyr::select(-t)
#######################################################################

## sims of total EVs + credible interval x time
sim_evs <- draws %>%
  left_join(states2020 %>% select(state,ev),by='state') %>%
  group_by(t,draw) %>%
  summarise(dem_ev = sum(ev * (p_harris > 0.5))) %>%
  group_by(t) %>%
  summarise(mean_dem_ev = mean(dem_ev),
            high_dem_ev = quantile(dem_ev,0.975),
            low_dem_ev = quantile(dem_ev,0.025),
            prob = mean(dem_ev >= 270))

## name this output: model date + hash for model name
identifier <- paste0(Sys.Date()," || " , out@model_name)



#######################################################################
## Plots of nat'l + ex_state results + credible intervals
#######################################################################

## national p(harris) x time
natl_polls.gg <- p_harris %>%
  filter(state == '--') %>%
  left_join(df %>% select(state, t, p_harris, mode)) %>% # plot over time
  # plot
  ggplot(., aes(x = t)) +
  geom_ribbon(aes(ymin = low,
                  ymax = high),
              col = NA, alpha = 0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = national_mu_prior,
             linetype = 2) +
  geom_point(aes(y = p_harris, shape = mode),
             alpha = 0.3) +
  geom_line(aes(y = mean)) +
  facet_wrap(. ~ state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits = c(start_date, Sys.Date()),
               date_breaks = '1 month',
               date_labels = '%b') +
  labs(subtitle='p_harris national')

## ev predictions x time
natl_evs.gg <-  ggplot(sim_evs, aes(x=t)) +
  geom_hline(yintercept = 270) +
  geom_line(aes(y=mean_dem_ev)) +
  geom_ribbon(aes(ymin=low_dem_ev,ymax=high_dem_ev),alpha=0.2) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(start_date, Sys.Date()),
               date_breaks='1 month',
               date_labels='%b') +
  labs(subtitletitle='harris evs')

## state p(harris) x time
state_polls.gg <- p_harris %>%
  filter(state %in% ex_states) %>%
  left_join(df %>% select(state,t,p_harris,mode)) %>% # plot over time
  ggplot(.,aes(x=t,col=state)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_point(aes(y=p_harris,shape=mode),alpha=0.3) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'top') +
  guides(color='none') +
  scale_x_date(limits=c(start_date, Sys.Date()),
               date_breaks='1 month',
               date_labels='%b') +
  labs(subtitle='p_harris state')

## Display the full, aggregated plot
grid.arrange(natl_polls.gg, natl_evs.gg, state_polls.gg,
             layout_matrix = rbind(c(1,1,3,3,3),
                                   c(2,2,3,3,3)),
             top = identifier
)
#######################################################################


#######################################################################
# what's the tipping point state?
#######################################################################
tipping_point <- draws %>%
  filter(t == ymd("2024-07-18")) %>%
  left_join(states2020 %>% dplyr::select(state,ev),by='state') %>%
  left_join(enframe(state_weights,'state','weight')) %>%
  group_by(draw) %>%
  mutate(dem_nat_pop_vote = weighted.mean(p_harris, weight))

tipping_point <- pblapply(max(tipping_point$draw, na.rm = T),
                          cl = parallel::detectCores() - 1,
                          function(x){
                            temp <- tipping_point[tipping_point$draw==x,]

                            ifelse(temp$dem_nat_pop_vote > 0.5, temp <- temp %>% arrange(desc(p_harris)), temp <- temp %>% arrange(p_harris))

                            return(temp)
                          }) %>%
  do.call('bind_rows',.)

# show TABLE of state tipping point probs
tipping_point %>%
  mutate(cumulative_ev = cumsum(ev)) %>%
  filter(cumulative_ev >= 270) %>%
  filter(row_number() == 1) %>%
  group_by(state) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop / sum(prop)) %>%
  arrange(desc(prop))
#######################################################################



#######################################################################
# now-cast probability over time all states
#######################################################################
# point predictions of state p(harris) x time
p_harris %>%
  ggplot(.,
         aes(x = t,
             y = prob,
             col = state)) +
  geom_hline(yintercept = 0.5) +
  geom_line() +
  geom_label_repel(data = p_harris %>%
                     filter(t == max(t),
                            prob > 0.1 & prob < 0.9),
                   aes(label = state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(start_date, Sys.Date()),
               date_breaks='1 month',
               date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(subtitle = identifier)

#######################################################################
# point predictions of state p(harris) diff from national over time
#######################################################################
p_harris[p_harris$state != '--',] %>%
  left_join(p_harris[p_harris$state=='--',] %>%
              select(t,p_harris_national=mean), by='t') %>%
  mutate(diff=mean-p_harris_national) %>%
  group_by(state) %>%
  mutate(last_prob = last(prob)) %>%
  filter(state %in% ex_states) %>%
  ggplot(.,aes(x=t,y=diff,col=state)) +
  geom_hline(yintercept=0.0) +
  geom_line() +
  geom_label_repel(data = . %>%
                     filter(t==max(t),
                            prob > 0.1 & prob < 0.9),
                   aes(label=state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(start_date, Sys.Date()),
               date_breaks='1 month',
               date_labels='%b') +
  scale_y_continuous(breaks=seq(-1,1,0.01)) +
  labs(subtitle = identifier)



###############################################################
## Examine evolution of priors --> posterior estimates
###############################################################
## mu_b_T -- Final state result -- prior vs. posterior
y <- MASS::mvrnorm(1000, mu_b_prior, Sigma = ss_cov_mu_b_T)
mu_b_T_posterior_draw <- rstan::extract(out, pars = "mu_b")[[1]][,,1]
mu_b_T_prior_draws     <- mean_low_high(y, states = colnames(y), id = "prior")
mu_b_T_posterior_draws <- mean_low_high(mu_b_T_posterior_draw, states = colnames(y), id = "posterior")
mu_b_T <- rbind(mu_b_T_prior_draws, mu_b_T_posterior_draws)

# plot
mu_b_T %>% arrange(mean) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(state, mean), color = type), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = state, color = type), width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()


## mu_c -- house effects -- prior vs. posterior
mu_c_posterior_draws <- rstan::extract(out, pars = "mu_c")[[1]]
mu_c_posterior_draws <- data.frame(draws = as.vector(mu_c_posterior_draws),
                                   index_p = sort(rep(seq(1, P), dim(mu_c_posterior_draws)[1])),
                                   type = "posterior")
mu_c_prior_draws <- data.frame(draws = rnorm(P * 1000, 0, sigma_c),
                               index_p = sort(rep(seq(1, P), 1000)),
                               type = "prior")
mu_c_draws <- rbind(mu_c_posterior_draws, mu_c_prior_draws)
pollster <- df %>% select(pollster, index_p) %>% distinct()
mu_c_draws <- merge(mu_c_draws, pollster, by = "index_p", all.x = TRUE)
mu_c_draws <- mu_c_draws %>%
  group_by(pollster, type) %>%
  summarize(mean = mean(draws),
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

# plot
mu_c_draws %>%
  arrange(mean) %>%
  filter(pollster %in% (df %>% group_by(pollster) %>%
                          summarise(n=n()) %>% filter(n>=5) %>% pull(pollster))) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(pollster, mean), color = type),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = pollster, color = type),
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()


## mu_m -- poll mode -- prior vs. posterior
mu_m_posterior_draws <- rstan::extract(out, pars = "mu_m")[[1]]
mu_m_posterior_draws <- data.frame(draws = as.vector(mu_m_posterior_draws),
                                   index_m = sort(rep(seq(1, M), dim(mu_m_posterior_draws)[1])),
                                   type = "posterior")
mu_m_prior_draws <- data.frame(draws = rnorm(M * 1000, 0, sigma_m),
                               index_m = sort(rep(seq(1, M), 1000)),
                               type = "prior")
mu_m_draws <- rbind(mu_m_posterior_draws, mu_m_prior_draws)
mode <- df %>% select(mode, index_m) %>% distinct()
mu_m_draws <- merge(mu_m_draws, mode, by = "index_m", all.x = TRUE)
mu_m_draws <- mu_m_draws %>%
  group_by(mode, type) %>%
  summarize(mean = mean(draws),
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

# plot
mu_m_draws %>% arrange(mean) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(mode, mean), color = type),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = mode, color = type),
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()


## mu_pop -- poll population -- prior vs. posterior
mu_pop_posterior_draws <- rstan::extract(out, pars = "mu_pop")[[1]]
mu_pop_posterior_draws <- data.frame(draws = as.vector(mu_pop_posterior_draws),
                                     index_pop = sort(rep(seq(1, Pop), dim(mu_pop_posterior_draws)[1])),
                                     type = "posterior")
mu_pop_prior_draws <- data.frame(draws = rnorm(Pop * 1000, 0, sigma_pop),
                                 index_pop = sort(rep(seq(1, Pop), 1000)),
                                 type = "prior")
mu_pop_draws <- rbind(mu_pop_posterior_draws, mu_pop_prior_draws)
mode <- df %>% select(polltype, index_pop) %>% distinct()
mu_pop_draws <- merge(mu_pop_draws, mode, by = "index_pop", all.x = TRUE)
mu_pop_draws <- mu_pop_draws %>%
  group_by(polltype, type) %>%
  summarize(mean = mean(draws),
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

# plot
mu_pop_draws %>% arrange(mean) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(polltype, mean), color = type),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = polltype, color = type),
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()


#######################################################################
## state error terms
polling_bias_posterior <- rstan::extract(out, pars = "polling_bias")[[1]]
polling_bias_posterior %>% apply(.,2,sd) / 4
polling_bias_posterior_draws <- data.frame(draws = as.vector(polling_bias_posterior),
                                           index_s = sort(rep(seq(1, S), dim(polling_bias_posterior)[1])),
                                           type = "posterior")
y <- MASS::mvrnorm(1000, rep(0, S), Sigma = state_covariance_polling_bias)
polling_bias_prior_draws <- data.frame(draws = as.vector(y),
                                       index_s = sort(rep(seq(1, S), dim(y)[1])),
                                       type = "prior")
polling_bias_draws <- rbind(polling_bias_posterior_draws, polling_bias_prior_draws)
states <- data.frame(index_s = 1:51, states = rownames(state_correlation_polling))
polling_bias_draws <- merge(polling_bias_draws, states, by = "index_s", all.x = TRUE)
polling_bias_draws <- polling_bias_draws %>%
  group_by(states, type) %>%
  summarize(mean = mean(draws),
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))

# plot
polling_bias_draws %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(states, mean), color = type),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = states, color = type),
                width = 0, position = position_dodge(width = 0.5)) +
  ggtitle("State polling bias") +
  coord_flip() +
  theme_bw()
#######################################################################



#######################################################################
## poll bias x time x adjusted/unadjusted

# poll terms
poll_terms <- rstan::extract(out, pars = "mu_c")[[1]]
non_adjusters <- df %>%
  mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>%
  select(unadjusted, index_p) %>%
  distinct() %>%
  arrange(index_p)

# estimates for poll bias
e_bias <- rstan::extract(out, pars = "e_bias")[[1]]

# plot bias x time (adjusted polls)
plt_adjusted <- lapply(1:100,
                       function(x){
                         tibble(e_bias_draw = e_bias[x,]
                                - mean(poll_terms[x, non_adjusters[non_adjusters$unadjusted == 0, 2]$index_p])
                                + mean(poll_terms[x, non_adjusters[non_adjusters$unadjusted == 1, 2]$index_p]),
                                trial = x) %>%
                           mutate(date = min(df$end) + row_number())
                       }) %>%
  do.call('bind_rows',.) %>%
  ggplot(.,aes(x=date,y=e_bias_draw,group=trial)) +
  geom_line(alpha=0.2)

# plot bias x time (unadjusted polls)
plt_unadjusted <- lapply(1:100,
                         function(x){
                           tibble(e_bias_draw = e_bias[x,],
                                  trial = x) %>%
                             mutate(date = min(df$end) + row_number())
                         }) %>%
  do.call('bind_rows',.) %>%
  ggplot(.,aes(x=date,y=e_bias_draw,group=trial)) +
  geom_line(alpha=0.2)

# combine plots
grid.arrange(plt_adjusted, plt_unadjusted)

# average aggregated polling errors at each time point
rstan::extract(out, pars = "e_bias")[[1]] %>% apply(.,2,mean) %>% plot



# plot final EV distribution
ev.gg <- ggplot(final_evs,
                aes(x = dem_ev,
                    fill = ifelse(dem_ev >= 270,
                                  'Democratic',
                                  'Republican'))) +
  geom_vline(xintercept = 270) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank()) +
  scale_fill_manual(name = 'Winner in the EC',
                    values = c('Democratic' = '#3A4EB1',
                               'Republican' = '#E40A04')) +
  labs(x='harris EC votes',
       subtitle=sprintf("p(harris victory) = %s",
                        round(mean(final_evs$dem_ev >= 270), 2)) )

print(ev.gg)

sum_states <- draws %>%
  as_tibble %>%
  filter(t == max(t)) %>%
  group_by(state) %>%
  summarize(
    pred = mean(p_harris),
    hi = quantile(p_harris, .975, na.rm = T),
    lo = quantile(p_harris, .025, na.rm = T)
  ) %>%
  mutate(label = factor(state))
ggplot(sum_states,
       aes(x = label,
           y = pred,
           ymax = hi,
           ymin = lo,
           color = ifelse(pred >= .5,
                          'Democratic',
                          'Republican'))) +
  geom_point(size = 2)+
  geom_linerange(size = 2.5,
                 alpha = .4) +
  geom_hline(yintercept = .5,
             linetype = "dashed") +
  scale_x_discrete("",
                   limits = sum_states$label[order(sum_states$pred,
                                                   decreasing = T)]) +
  scale_y_continuous("Probability of a harris win") +
  scale_color_manual(name = 'State winner',
                     values = c('Democratic' = '#3A4EB1',
                                'Republican' = '#E40A04')) +
  geom_text(aes(label = round(pred, digits = 2)),
            nudge_y = .08,
            size = 3) +
  coord_flip() +
  theme_bw()
ggsave(filename = "state_results.jpg",
       height = 5, width = 7, dpi = 300)



## ERRORS ##
pred_state <- draws %>%
  as_tibble %>%
  filter(t == max(t)) %>%
  group_by(state) %>%
  summarize(
    pred = mean(p_harris),
    hi = quantile(p_harris, .975, na.rm = T),
    lo = quantile(p_harris, .025, na.rm = T))

result <- read_csv('data/2020.csv') %>%
  select(state, biden) %>%
  arrange(state) %>%
  mutate(biden_score = (biden/100))

pred_state <- pred_state %>%
  left_join(result) %>%
  mutate(error = pred - biden_score) %>%
  mutate(average = mean(error)) %>%
  mutate(label = factor(state))

ggplot(pred_state) +
  aes(x = fct_reorder(label,error), y = error)     +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.0282,
             linetype = "dashed") +
  geom_text(aes(label = 100 * round(error, digits=3)),
            nudge_y = .003,
            size = 3) +
  coord_flip() +
  theme_bw()

ggsave(filename = "state_results.jpg",
       height = 5, width = 7, dpi = 300)


