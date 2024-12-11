extract_polls <- function(file) {
  filename <- "data/poll_list.csv"

  orig_path <- "538_2024_election_forecast_data"

  unzip(file, overwrite = TRUE, exdir = "data/")

  file.copy(from = paste0(orig_path, "/", basename(filename)),
            to = filename,
            overwrite = TRUE)

  df <- read_csv("data/poll_list.csv") %>%
    initial_poll_wrangle() %>%
    rename_variables() %>%
    clean_mode_variable() %>%
    clean_pollster_variable() %>%
    calc_vote_share() %>%
    remove_congressional_districts()

  df <- subset(df, !is.na(df$mode))
  df$state[is.na(df$state)] <- "--"

  return(df)
}

initial_poll_wrangle <- function(polls) {
  polls %>%
    mutate(
      grp = paste(poll_id, pollster_rating_id),
      pollpollsterID = match(grp, unique(grp))
    ) %>%
    filter(
      candidate_name == "Kamala Harris" | candidate_name == "Donald Trump"
    ) %>%
    group_by(pollpollsterID) %>%
    mutate(
      trump = case_when(candidate_name == "Donald Trump" ~ pct),
      harris = case_when(candidate_name == "Kamala Harris" ~ pct)
    ) %>%
    fill(trump, .direction = "up") %>%
    fill(harris) %>%
    filter(row_number(pollpollsterID) == 1) %>%
    ungroup() %>%
    dplyr::select(
      state_abb,
      pollster_rating_name,
      sample_size,
      population,
      methodology,
      start_date,
      end_date,
      region,
      region_computed,
      pollster_wt,
      harris,
      trump
    ) %>%
    mutate(
      begin = ymd(start_date),
      end = ymd(end_date),
      t = begin + (as.numeric(end - begin) / 2)
    )  # Assign poll date to middle of field period
}

rename_variables <- function(df) {
  df %>%
    rename(
      mode = methodology,
      n = sample_size,
      pollster = pollster_rating_name,
      state = state_abb
    )
}

clean_mode_variable <- function(df) {
  df$mode <- case_when(
    df$mode == "IVR/Online Panel" ~ "IVR",
    df$mode == "IVR/Text-to-Web" ~ "IVR",
    df$mode == "IVR/Online Panel/Text-to-Web" ~ "IVR",
    df$mode == "IVR/Live Phone/Online Panel/Text-to-Web" ~ "IVR",
    df$mode == "IVR/Text" ~ "IVR",
    df$mode == "Live Phone/Text-to-Web" ~ "Live Phone",
    df$mode == "Live Phone/Text-to-Web/Email/Mail-to-Web/Mail-to-Phone" ~ "Live Phone",
    df$mode == "Live Phone/Text" ~ "Live Phone",
    df$mode == "Live Phone/Email" ~ "Live Phone",
    df$mode == "Live Phone/Probability Panel" ~ "Live Phone",
    df$mode == "Live Phone/Online Panel/Text-to-Web" ~ "Live Phone",
    df$mode == "Live Phone/Online Panel/App Panel" ~ "Live Phone",
    df$mode == "IVR/Live Phone/Text/Online Panel/Email" ~ "IVR",
    df$mode == "Text-to-Web/Online Ad" ~ "Text-to-Web",
    df$mode == "Text-to-Web/Email" ~ "Text-to-Web",
    df$mode == "Online Panel/Text-to-Web" ~ "Online Panel",
    df$mode == "Mail-to-Web/Mail-to-Phone" ~ "Mail-to-Web",
    df$mode == "Live Phone/Online Panel" ~ "Live Phone",
    df$mode == "Live Phone/Online Panel/Text" ~ "Live Phone",
    TRUE ~ "Other"
  )
  return(df)
}

clean_pollster_variable <- function(df) {
  df$pollster <- case_when(
    df$pollster == "The New York Times/Siena College" ~ "NYT",
    df$pollster == "Harris Insights & Analytics" ~ "Harris",
    df$pollster == "Fabrizio, Lee & Associates/GBAO" ~ "Fabrizio",
    df$pollster == "Marquette University Law School" ~ "Marquette",
    df$pollster == "YouGov/Johns Hopkins University SNF Agora Institute" ~ "YouGov",
    df$pollster == "Florida Atlantic University PolCom Lab/Mainstreet Research" ~ "Florida Atlantic",
    df$pollster == "University of Massachusetts Department of Political Science/YouGov" ~ "Uni Mass",
    df$pollster == "Hart Research Associates/Public Opinion Strategies" ~ "Hart",
    df$pollster == "Beacon Research/Shaw & Co. Research" ~ "Beacon",
    TRUE ~ as.character(df$pollster)
  )
  return(df)
}

calc_vote_share <- function(df) {
  df %>%
    mutate(
      two_party_sum = harris + trump, # two-party sum
      polltype = population, # poll population
      n_respondents = round(n), # poll sample size (rounding should not be needed, but in case)

      # harris
      n_harris = round(n * harris / 100), # approximate Harris supporters
      p_harris = harris / two_party_sum, # % of Harris voters

      # trump
      n_trump = round(n * trump / 100), # approximate Trump supporters
      p_trump = trump / two_party_sum # % of Trump voters
    )
}

remove_congressional_districts <- function(df) {
  ## Removing polls for congressional districts (NE-CD2 and ME-CD1 and CD2)
  df <- df[!grepl('ME-02', df$state),]
  df <- df[!grepl('ME-01', df$state),]
  df <- df[!grepl('NE-02', df$state),]
}
