# Define data subsets to be used for model fitting
define_subsets <- function(df, cohort = c("star", 'khandle','LA90','LA90_FEMALE','LA90_MALE')){

  if(cohort == "LA90"|cohort == "LA90_FEMALE"|cohort == "LA90_MALE"){
    # dropping wave 4 and people w/ phone assessment
    df <- df %>%
      filter(
        phone_ind2 != 1,
        !(wave %in% c("W4", "W5", "W6"))
        )
    # balanced data
    df_balanced <- df %>%
      drop_na(score_z) %>%
      group_by(STUDYID) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n == 3)

    return(list(df, df_balanced))

  } else{
    # dropping wave 3 and people w/ phone assessment
    df <- df %>%
      filter(phone_ind2 != 1 & wave != "W3")

    # balanced data
    df_balanced <- df %>%
      drop_na(score) %>%
      group_by(STUDYID) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n != 1)

    old_age_threshold <- ifelse(cohort == "star", 65, 85)

    # unbalanced data 85+
    df_old <- df %>%
      filter(base_age >= old_age_threshold)

    # balanced data 85+
    df_old_balanced <- df_balanced %>%
      filter(base_age >= old_age_threshold)

    return(list(df, df_balanced, df_old, df_old_balanced))
  }
}


# function for making a tidy dataframe from a named list of model objects
make_tidy_df <- function(model_list){

  for(i in seq_along(names(model_list))){
    sample_type <- names(model_list)[[i]]
    mod_names <- names(model_list[[sample_type]])

    tidy_dfs <- model_list[[sample_type]] %>%
      map(~tidy(.x, conf.int = TRUE))

    tidy_df <- map2(tidy_dfs, mod_names, ~ cbind(.x, "model" = .y)) %>%
      reduce(rbind) %>%
      mutate(sample_type = sample_type) %>%
      select(sample_type, model, everything())

    if(i == 1){
      d_tidy <- tidy_df
    } else{
      d_tidy <- d_tidy %>%
        rbind(tidy_df)
    }
  }
  return(d_tidy)
}

# calls make tidy df on a list of models lists and returns one tidy dataframe
# to rule them all




tidy_all <- function(model_lists, cohort_names = c("KHANDLE", "STAR", "LA90", "LA90_FEMALE", "LA90_MALE")){
  tidy_dfs <- map(model_lists, ~ make_tidy_df(.x))

  d_all_res <- map2(tidy_dfs, cohort_names, ~ cbind(.x, "cohort" = .y)) %>%
    reduce(rbind) %>%
    select(cohort, everything())

  return(d_all_res)
}




tidy_all2 <- function(model_lists, cohort_names = c("KHANDLE", "STAR", "LA90", "LA90_FEMALE", "LA90_MALE", "LA90_2")){
  tidy_dfs <- map(model_lists, ~ make_tidy_df(.x))

  d_all_res <- map2(tidy_dfs, cohort_names, ~ cbind(.x, "cohort" = .y)) %>%
    reduce(rbind) %>%
    select(cohort, everything())

  return(d_all_res)
}















# function for adding offset columns to data
add_offsets <- function(
  df,
  df_PE_estimates,
  offset_wave = "after_wave1",
  cohort = c("star", "khandle","LA90", "LA90_FEMALE", "LA90_MALE"),task){

  cohort_name <- str_to_upper(cohort)
  sample_type <- "full_b"

  if(cohort == "LA90"& task == "executive_function"){
    model <- "m3"
    terms <- c(
      "waveW2", "waveW3", "waveW2:genderFemale", "waveW3:genderFemale",
      "W2:Female", "W3:Female", "W2:Male", "W3:Male"
      )

    d_cis <- df_PE_estimates %>%
      filter(
        cohort == {{ cohort_name }},
        model  == {{ model }},
        sample_type == {{ sample_type }},
        term %in% terms
      )

    df <- df %>%
      mutate(
        off_pt = case_when(
          wave == "W1" ~ 0,
          wave == "W2" & gender == "Male" ~
            d_cis %>% filter(term == "waveW2") %>% pull(estimate),
          wave == "W2" & gender == "Female" ~
            d_cis %>% filter(term == "waveW2") %>% pull(estimate) +
            d_cis %>% filter(term == "waveW2:genderFemale") %>% pull(estimate),
         !(wave %in% c("W1", "W2")) & gender == "Male" ~
            d_cis %>% filter(term == "waveW3") %>% pull(estimate),
         !(wave %in% c("W1", "W2")) & gender == "Female" ~
            d_cis %>% filter(term == "waveW3") %>% pull(estimate) +
            d_cis %>% filter(term == "waveW3:genderFemale") %>% pull(estimate)
        ),
        off_low = case_when(
          wave == "W1" ~ 0,
          wave == "W2" & gender == "Male" ~
            d_cis %>% filter(term == "W2:Male") %>% pull(off_25),
          wave == "W2" & gender == "Female" ~
            d_cis %>% filter(term == "W2:Female") %>% pull(off_25),
          !(wave %in% c("W1", "W2")) & gender == "Male" ~
            d_cis %>% filter(term == "W3:Male") %>% pull(off_25),
          !(wave %in% c("W1", "W2")) & gender == "Female" ~
            d_cis %>% filter(term == "W3:Female") %>% pull(off_25)
        ),
        off_high = case_when(
          wave == "W1" ~ 0,
          wave == "W2" & gender == "Male" ~
            d_cis %>% filter(term == "W2:Male") %>% pull(off_75),
          wave == "W2" & gender == "Female" ~
            d_cis %>% filter(term == "W2:Female") %>% pull(off_75),
          !(wave %in% c("W1", "W2")) & gender == "Male" ~
            d_cis %>% filter(term == "W3:Male") %>% pull(off_75),
          !(wave %in% c("W1", "W2")) & gender == "Female" ~
            d_cis %>% filter(term == "W3:Female") %>% pull(off_75)
        )
    )

  }  else if ((cohort =="LA90_FEMALE" | cohort =="LA90_MALE" ) & task == "executive function"){
    model <- "m2"
    terms <- c(
      "waveW2", "waveW3"
    )

    d_cis <- df_PE_estimates %>%
      filter(
        cohort == {{ cohort_name }},
        model  == {{ model }},
        sample_type == {{ sample_type }},
        term %in% terms
      )

    df <- df %>%
      mutate(
        off_pt = case_when(
          wave == "W1" ~ 0,
          wave == "W2" ~
            d_cis %>% filter(term == "waveW2") %>% pull(estimate),
          ! wave %in% c("W1", "W2")  ~
            d_cis %>% filter(term == "waveW3") %>% pull(estimate)
        ),
        off_low = case_when(
          wave == "W1" ~ 0,
          wave == "W2"  ~
            d_cis  %>%filter(term == "waveW2") %>%  pull(conf.low),
          ! wave %in% c("W1", "W2")   ~
            d_cis %>% filter(term == "waveW3")  %>% pull(conf.low)
        ),
        off_high= case_when(
          wave == "W1" ~ 0,
          wave == "W2"  ~
            d_cis %>% filter(term == "waveW2") %>% pull(conf.high),
          ! wave %in% c("W1", "W2")    ~
            d_cis %>% filter(term == "waveW3")   %>% pull(conf.high)
        )
      )
  }else if (cohort =="LA90" & task == "verbal_memory"){
    model <- "m2"
    terms <- c(
      "waveW2", "waveW3"
    )

    d_cis <- df_PE_estimates %>%
      filter(
        cohort == {{ cohort_name }},
        model  == {{ model }},
        sample_type == {{ sample_type }},
        term %in% terms
      )

    df <- df %>%
      mutate(
        off_pt = case_when(
          wave == "W1" ~ 0,
          wave == "W2" ~
            d_cis %>% filter(term == "waveW2") %>% pull(estimate),
          ! wave %in% c("W1", "W2")  ~
            d_cis %>% filter(term == "waveW3") %>% pull(estimate)
        ),
        off_low = case_when(
          wave == "W1" ~ 0,
          wave == "W2"  ~
            d_cis  %>%filter(term == "waveW2") %>%  pull(conf.low),
          ! wave %in% c("W1", "W2")   ~
            d_cis %>% filter(term == "waveW3")  %>% pull(conf.low)
        ),
        off_high= case_when(
          wave == "W1" ~ 0,
          wave == "W2"  ~
            d_cis %>% filter(term == "waveW2") %>% pull(conf.high),
          ! wave %in% c("W1", "W2")    ~
            d_cis %>% filter(term == "waveW3")   %>% pull(conf.high)
        )
      )
  }

  else {
    model <- "m2"
    term  <- "waveW2"

    d_cis <- df_PE_estimates %>%
      filter(
        cohort == {{ cohort_name }},
        model  == {{ model }},
        sample_type == {{ sample_type }},
        term == {{ term }}
      )

    if(offset_wave == "wave1"){
      df <- df %>%
        mutate(
          # here's how it was coded originally
          off_pt   = ifelse(first_assess == "1", -d_cis$estimate, 0),
          off_low  = ifelse(first_assess == "1", -d_cis$conf.low, 0),
          off_high = ifelse(first_assess == "1", -d_cis$conf.high, 0)
        )
    } else if(offset_wave == "after_wave1"){
      df <- df %>%
        mutate(
          # fixing the coding of the offsets
          off_pt   = ifelse(first_assess == "1", 0, d_cis$estimate),
          off_low  = ifelse(first_assess == "1", 0, d_cis$conf.low),
          off_high = ifelse(first_assess == "1", 0, d_cis$conf.high)
        )
    }
  }
  return(df)
}


#model 1 using current age as the time scale
run_analysis_tab3_c <- function(dat, off_pt, cohort = c("star", 'khandle', 'LA90','LA90_FEMALE','LA90_MALE','LA90_2')){
  if(cohort == "star"){
    # grip strength
    grip.omit_c   <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 + gender + edu + age_c:gripstrength_w1 + (1|STUDYID)"
    grip.direct_c <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 + gender + edu + age_c:gripstrength_w1 + first_assess + (1|STUDYID)"

    ### Kaitlin: code for khandle and LA90 is identical - don't need separate if conditions
  } else if(cohort == "khandle") {
    # grip strength
    grip.omit_c   <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 + gender + race + edu + age_c:gripstrength_w1 + (1|STUDYID)"
    grip.direct_c <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 + gender + race + edu + age_c:gripstrength_w1 + first_assess + (1|STUDYID)"

  } else if(cohort == "LA90"|cohort == "LA90_2") {
    # grip strength
    grip.omit_c   <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 + gender + race + edu + age_c:gripstrength_w1 + (1|STUDYID)"
    grip.direct_c <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 + gender + race + edu + age_c:gripstrength_w1 + first_assess + (1|STUDYID)"
  }else if(cohort == "LA90_FEMALE"|cohort == "LA90_MALE") {
    # grip strength
    grip.omit_c   <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 +  race + edu + age_c:gripstrength_w1 + (1|STUDYID)"
    grip.direct_c <- "score_z ~ age_c + gripstrength_w1 + phone_ind2 +  race + edu + age_c:gripstrength_w1 + first_assess + (1|STUDYID)"
  }

  # main models
  # # grip (current age as the time scale)
  m1.grip_c <- lmer(grip.omit_c, dat = dat, REML = F)
  m2.grip_c <- lmer(grip.direct_c, dat = dat, REML = F)
  m3.grip_c <- lmer(grip.omit_c, offset = off_pt, dat = dat, REML = F)

  res = list(
    grip_c = list(m1 = m1.grip_c,
                  m2 = m2.grip_c,
                  m3 = m3.grip_c))

  return(res)
}

# Make a dataframe with model predictions
make_pred_df <- function(
  model_list,
  timescale = c("years_since_base", "age_c")
  ){
  m1 <- model_list$main$m_omit
  m2 <- model_list$main$m_direct
  m3 <- model_list$main$m_force_pt
  m4 <- model_list$main$m_force_low
  m5 <- model_list$main$m_force_high

  p1 <- ggpredict(m1, {{ timescale }}, interval="confidence")
  p2 <- ggpredict(m2, {{ timescale }}, interval="confidence")
  p3 <- ggpredict(m3, {{ timescale }}, interval="confidence")
  p4 <- ggpredict(m4, {{ timescale }}, interval="confidence")
  p5 <- ggpredict(m5, {{ timescale }}, interval="confidence")

  dfc <- rbind(as.data.frame(p1),
               as.data.frame(p2),
               as.data.frame(p3),
               as.data.frame(p4),
               as.data.frame(p5))

  n_repeats <- nrow(p1)
  dfc$model <- rep(c("A", "B", "C", "D", "E"), each = n_repeats)

  dfc$size_dot <- rep(c(1, 1, 3, 1, 1), each = n_repeats)
  dfc$size_line <- rep(c(0.5, 0.5, 1.3, 0.5, 0.5), each = n_repeats)

  return(dfc)
}


# fit quantile regression models and return model summary
fit_qr <- function(df, cohort = c("khandle", "star","LAFull", "LA90")){

  formulas <- list(
    "khandle" = "score_z ~ age_c + wave + edu + race + gender",
    "star"    = "score_z ~ age_c + wave + edu + gender ",
    "LA90"    = "score_z ~ age_c + wave + edu + race",
    "LAFull" = "score_z ~ age_c + wave + edu + race + gender"

  )
  c <- str_remove(cohort, "_.+")
  f <- formulas[[c]]

  qr <- rq(
    f,
    data = df,
    tau = c(.1,.25,.5,.75,.9)
    )
  # return(qr)

  # add code to log the anova?
  # anova(qr_k, joint=FALSE)
  log_step(
    glue(
    "
        Running anova for {cohort}:
    "
    ),
    log_file
    )

  capture_a <- (anova(qr, joint=FALSE, se = "ker"))
  capture.output(capture_a, file = log_file, append = TRUE)

  summ_qr <- summary(qr, se = "boot") #summary(qr, se = 'rank')

  results_df <- map(
    summ_qr,
    ~ cbind(
        cohort = toupper(cohort),
        tau = .x[['tau']],    # from each list element, cbind the tau...
        coef(.x) %>%    # ...and the coefficient matrix,
          data.frame(check.names = TRUE) %>%    # cleaned a little
          rownames_to_column('term')
        )
    ) %>%
    reduce(rbind)

    return(results_df)

}

# function for logging steps to a text file
log_step <- function(
  step_string,
  log_file = glue("results/02_output/log.txt")
  ){
  write(
    step_string,
    file = log_file,
    append=TRUE
  )
}


fit_la_subsets <- function(dla){
  ###subset the data to male
  dla_m<-subset(dla, gender=="Male")
  la_force_m <-lmer(
    score_z ~ years_since_base + base_age_c + gripstrength_w1 + phone_ind2 +
      race + edu + years_since_base:gripstrength_w1 + (1|STUDYID),
    offset = off_pt,
    dat = dla_m,
    REML = F
  )
  la_nope_m <- lmer(
    score_z ~ years_since_base + base_age_c + gripstrength_w1 + phone_ind2 +
      race + edu + years_since_base:gripstrength_w1 + (1|STUDYID),
    dat = dla_m,
    REML = F
  )
  la_direct_m <- lmer(
    score_z ~ years_since_base + base_age_c + gripstrength_w1 + phone_ind2 +
      race + edu + first_assess + years_since_base:gripstrength_w1 + (1|STUDYID),
    dat = dla_m,
    REML = F
  )

  # repeat above with female subset
  dla_f <- subset(dla, gender=="Female")
  la_force_f <- lmer(
    score_z ~ years_since_base + base_age_c + gripstrength_w1 + phone_ind2 +
      race + edu + years_since_base:gripstrength_w1 + (1|STUDYID),
    offset = off_pt,
    dat = dla_f,
    REML = F
  )
  la_nope_f <- lmer(
    score_z ~ years_since_base + base_age_c + gripstrength_w1 + phone_ind2 +
      race + edu + years_since_base:gripstrength_w1 + (1|STUDYID),
    dat = dla_f,
    REML = F
  )
  la_direct_f <- lmer(
    score_z ~ years_since_base + base_age_c + gripstrength_w1 + phone_ind2 +
      race + edu + first_assess + years_since_base:gripstrength_w1 + (1|STUDYID),
    dat = dla_f,
    REML = F
  )

  # make dataframe with all results
  model_list <- list(
    la_force_m, la_nope_m, la_direct_m,
    la_force_f, la_nope_f, la_direct_f
  )

  subsets <- rep(c("Male", "Female"), each = 3)
  model_types <- rep(c("m_force_pt", "m_omit", "m_direct"), 2)

  df_results <- map(model_list, ~ tidy(.x, conf.int = TRUE)) %>%
    map2(subsets, ~ cbind(.x, "subset" = .y)) %>%
    map2(model_types, ~ cbind(.x,  "model" = .y)) %>%
    reduce(rbind) %>%
    mutate(cohort = "LA90") %>%
    select(cohort, subset, model, everything())

  return(df_results)
}

