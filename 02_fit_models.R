#*******************************************************
#* Fits models
#* Using pre-pandemic data, run models to estimate PE
#* Then fits linear mixed models with and without forcing PE
#******************************************************
# load packages
library(pacman)
p_load(
  "here", "dplyr", "stargazer", "tidyverse", "lmerTest", "broom", "broom.mixed",
  "sjPlot", "ggtext", "ggpubr", "glue", "quantreg", "patchwork", "ggeffects", "purr"
)

options(scipen = 999)

source(glue("{path_to_code}/run_analysis_functions.R"))
source(glue("{path_to_code}/kaitlin_functions.R"))

# function for fitting all models for each task
fit_all <- function(task){

  if(task == "executive_function"){
    test <-  "exec"
    khandle_data_path <- "results/01_data/d_e_k.RDS"
    star_data_path    <- "results/01_data/d_e_s.RDS"
    la90_data_path    <- "results/01_data/d_e_la.RDS"
  } else if(task == "verbal_memory"){
    test <-  "vm"
    khandle_data_path <- "results/01_data/d_vm_k.RDS"
    star_data_path    <- "results/01_data/d_vm_s.RDS"
    la90_data_path    <- "results/01_data/d_vm_la.RDS"
  }

  offset_type <- "after_wave1"
  output_path <- glue("results/02_output/{task}")

  if(!dir.exists(output_path)){
    dir.create(output_path)
  }

  #-----------------------------------------------------------------------------
  # read in all data
  #-----------------------------------------------------------------------------

  # khandle, star and LA90 executive function data
  d_khandle <- readRDS(khandle_data_path)
  d_star    <- readRDS(star_data_path)
  d_la90    <- readRDS(la90_data_path)
  ###restrict to females
  d_la90_Female <- d_la90 %>% filter(gender=="Female")
  d_la90_Male <- d_la90 %>% filter(gender=="Male")

  subset_names    <- c('full', 'full_b', 'old', 'old_b')
  khandle_subsets <- define_subsets(d_khandle, cohort = "khandle")
  star_subsets    <- define_subsets(d_star, cohort = "star")
  la90_subsets    <- define_subsets(d_la90, cohort = "LA90")
  la90_Female_subsets    <- define_subsets(d_la90_Female, cohort = "LA90_FEMALE")
  la90_Male_subsets    <- define_subsets(d_la90_Male, cohort = "LA90_MALE")

  #-------------------------------------------------------------------------------
  # Estimate practice effects
  #-------------------------------------------------------------------------------
  log_step("\n\t Estimating practice effects \n\n", log_file)

  # fit models on subsets for each cohort
  exec_k <- lapply(khandle_subsets, run_analysis_tab2, cohort='khandle')
  names(exec_k) <- subset_names

  exec_s <- lapply(star_subsets, run_analysis_tab2, cohort='star')
  names(exec_s) <- subset_names

  exec_la90 <- lapply(la90_subsets, run_analysis_tab2, cohort='LA90')
  names(exec_la90) <- subset_names[1:2]

  exec_la90_Female <- lapply(la90_Female_subsets, run_analysis_tab2, cohort='LA90_FEMALE')
  names(exec_la90_Female) <- subset_names[1:2]

  exec_la90_Male <- lapply(la90_Male_subsets, run_analysis_tab2, cohort='LA90_MALE')
  names(exec_la90_Male) <- subset_names[1:2]

  # condense all results into one tidy dataframe
  model_lists <- list(exec_k, exec_s, exec_la90,exec_la90_Female,exec_la90_Male)
  d_PE_estimates <- tidy_all(model_lists)

  # calculate low and high offsets for LA90--------------------------
  # code from 05_01_offselt_la90.R
  # sections of this code are named "exec", but it'll work for vrmem too
  dat <- la90_subsets[[which(subset_names == "full_b")]] %>%
    select(STUDYID, score_z, age_c, edu, gender, wave, race) %>%
    mutate(wave = as.factor(wave),
           gender = as.factor(gender),
           gender = relevel(gender, ref = "Male"),
           w2 = ifelse(wave == "W2", 1, 0),
           w3 = ifelse(wave == "W3", 1, 0),
           gender_num = ifelse(gender == "Male", 0, 1))

  exec <- lmer(
    score_z ~ age_c + w2 + w3 + race + gender + edu + w2:gender + w3:gender +
      (1|STUDYID),
    data = dat, REML = FALSE
    )
  CIs<-confint(exec)
  mod_summ<-summary(exec)
  coefs <- mod_summ$coefficients[,1]
  se<-mod_summ$coefficients[,2]
  X <- model.matrix(exec)
  dof <- nrow(X) - ncol(X)
  coefs_var <- vcov(exec)

  #chose 25% of the CI based on step 1
  male_25_w2 <-coefs["w2"]- qt(0.625, dof) * sqrt(coefs_var["w2","w2"])
  male_25_w3 <-coefs["w3"]- qt(0.625, dof) * sqrt(coefs_var["w3","w3"])

  ##to chose the value for group 2, re-estimate the step 1 model forcing the
  # group 1 PE to be the 25th percentitle value you chose in step 2. Then use
  # the estimated PE for group 2
  exec_25 <- lmer(
    score_z ~ age_c + race + edu + gender_num +
      offset(male_25_w2*w2 + male_25_w3*w3) + w2:gender_num + w3:gender_num +
      (1|STUDYID),data = dat, REML = F
    )

  mod_exec_25<-summary(exec_25)
  mod_exec_25_coef <- mod_exec_25$coefficients[,"Estimate"]

  female_25_w2<-male_25_w2+mod_exec_25_coef["gender_num:w2"]
  female_25_w3<-male_25_w3+mod_exec_25_coef["gender_num:w3"]

  ###use 75 percentile CI instead
  male_75_w2 <-coefs[3]- qt(0.875, dof) * sqrt(coefs_var[3,3])
  male_75_w3 <-coefs[4]- qt(0.875, dof) * sqrt(coefs_var[4,4])

  ##to chose the value for group 2, re-estimate the step 1 model forcing  the
  # group 1 PE to be the 25th percentitle value you chose in step 2. Then use
  # the estimated PE for group 2
  exec_75 <- lmer(
    score_z ~ age_c + race + edu + gender_num +
      offset(male_75_w2*w2 + male_75_w3*w3) + w2:gender_num + w3:gender_num +
      (1|STUDYID),data = dat, REML = F
    )

  mod_exec_75<-summary(exec_75)
  mod_exec_75_coef <- mod_exec_75$coefficients[,1]

  female_75_w2<-male_75_w2+mod_exec_75_coef[10]
  female_75_w3<-male_75_w3+mod_exec_75_coef[11]

  # make offsets into dataframe
  la90_offsets <- tibble(
    cohort = "LA90",
    sample_type = "full_b",
    model = "m3",
    # gender = c("Female", "Female", "Male", "Male"),
    term = c("W2:Female", "W3:Female", "W2:Male", "W3:Male"),
    off_25 = c(female_25_w2, female_25_w3, male_25_w2, male_25_w3),
    off_75 = c(female_75_w2, female_75_w3, male_75_w2, male_75_w3)
  )


  # save results
  d_PE_estimates <- d_PE_estimates %>%
    select(
      cohort, sample_type, model,
      term, estimate, conf.low, conf.high,
      std.error, statistic, df, p.value
      ) %>%
    full_join(la90_offsets)

  d_PE_estimates %>%
    saveRDS(glue('{output_path}/PE_estimating_models.RDS'))



  #-----------------------------------------------------------------------------
  # Fit models with PE constraints
  #-----------------------------------------------------------------------------
  log_step("\t Fitting models with PE constraints \n", log_file)
  # These models are run on d_khandle, d_star, and d_la90 imported above
  # for each cohort:
  # extract desired PE coefficients and confidence intervals
  # add columns for offset based on each of those
  # fit models

  d_khandle <- add_offsets(d_khandle, d_PE_estimates, cohort = "khandle", task=task)
  d_star    <- add_offsets(d_star, d_PE_estimates, cohort = "star", task=task)
  d_la90    <- add_offsets(d_la90, d_PE_estimates, cohort = "LA90", task=task)
  d_la90_Female    <- add_offsets(d_la90_Female, d_PE_estimates, cohort = "LA90_FEMALE", task=task)
  d_la90_Male    <- add_offsets(d_la90_Male, d_PE_estimates, cohort = "LA90_MALE", task=task)


  ###restrict to females
  d_la90_2<-rbind(d_la90_Female, d_la90_Male)

  log_step(glue("\t \t added offsets \n"), log_file)
  # run main analyses with years on study as time scale
  tab3_k <- run_analysis_tab3(
    d_khandle,
    off_pt = off_pt,
    off_low = off_low,
    off_high = off_high,
    cohort = 'khandle'
    )

  tab3_s <- run_analysis_tab3(
    d_star,
    off_pt = off_pt,
    off_low = off_low,
    off_high = off_high,
    cohort = 'star'
  )

  tab3_la <- run_analysis_tab3_LA90_e(
    d_la90,
    off_pt = off_pt,
    off_low = off_low,
    off_high = off_high
    )

  tab3_la_male <- run_analysis_tab3_LA90_gender(
    d_la90_Male,
    off_pt = off_pt,
    off_low = off_low,
    off_high = off_high
  )

  tab3_la_female <- run_analysis_tab3_LA90_gender(
    d_la90_Female,
    off_pt = off_pt,
    off_low = off_low,
    off_high = off_high
  )

  tab3_la_2<- run_analysis_tab3_LA90_e(
    d_la90_2,
    off_pt = off_pt,
    off_low = off_low,
    off_high = off_high
  )



  log_step("\t Finished models with years as timescale \n", log_file)

  # condense results into tidy dataframe
  model_lists <- list(tab3_k, tab3_s, tab3_la, tab3_la_female, tab3_la_male,tab3_la_2)
  d_results <- tidy_all2(model_lists) %>%
    mutate(timescale = "years_since_baseline")
  log_step("\t Made results dataframe \n", log_file)

  # redo above with age as timescale for sensitivity analyses
  tab3_k_age <- run_analysis_tab3_c(
    d_khandle,
    off_pt = off_pt,
    cohort = 'khandle'
  )

  tab3_s_age <- run_analysis_tab3_c(
    d_star,
    off_pt = off_pt,
    cohort = 'star'
  )

  tab3_la_age <- run_analysis_tab3_c(d_la90,   off_pt = off_pt, cohort = "LA90")
  tab3_la_2_age <- run_analysis_tab3_c(d_la90_2,   off_pt = off_pt, cohort = "LA90_2")
  tab3_la_age_female <- run_analysis_tab3_c(d_la90_Female,   off_pt = off_pt, cohort = "LA90_FEMALE")
  tab3_la_age_male <- run_analysis_tab3_c(d_la90_Male,  off_pt = off_pt, cohort = "LA90_MALE")

  log_step("\t Finished models with age as timescale \n", log_file)

  # condense results into tidy dataframe
  model_lists_age <- list(tab3_k_age, tab3_s_age, tab3_la_age,tab3_la_age_female,tab3_la_age_male,  tab3_la_2_age)
  d_results <- d_results %>%
    rbind(
      tidy_all2(model_lists_age) %>%
        mutate(timescale = "age")
      ) %>%
    select(cohort, timescale, everything())
  log_step("\t Made results dataframe \n", log_file)

  # save results
  d_results %>%
    select(
      cohort, timescale, sample_type, model,
      term, estimate, conf.low, conf.high,
      std.error, statistic, df, p.value
    ) %>%
    saveRDS(glue('{output_path}/applying_PE_models.RDS'))

  # get LA90 results by sex
  d_results_stratified <- fit_la_subsets(d_la90) %>%
    saveRDS(
      glue('{output_path}/applying_PE_models_LA90_gender_speficic.RDS')
    )

  #-----------------------------------------------------------------------------
  # Get predicted values for plots
  #-----------------------------------------------------------------------------
  log_step(glue("Get predicted values for plots \n"), log_file)

  tab3_k %>%
    make_pred_df(timescale = "years_since_base") %>%
    mutate(
      cohort = "KHANDLE",
      timescale = "years_since_baseline"
    ) %>%
    rbind(
      tab3_s %>%
        make_pred_df(timescale = "years_since_base") %>%
        mutate(
          cohort = "STAR",
          timescale = "years_since_baseline"
        )
    ) %>%
    rbind(
      tab3_la %>%
        make_pred_df(timescale = "years_since_base") %>%
        mutate(
          cohort = "LA90",
          timescale = "years_since_baseline"
        )
      ) %>%
    rbind(
      tab3_la_female %>%
        make_pred_df(timescale = "years_since_base") %>%
        mutate(
          cohort = "LA90_FEMALE",
          timescale = "years_since_baseline"
        )
    ) %>%  rbind(
      tab3_la_male %>%
        make_pred_df(timescale = "years_since_base") %>%
        mutate(
          cohort = "LA90_MALE",
          timescale = "years_since_baseline"
        )
    ) %>%
    rbind(
      tab3_la_2 %>%
        make_pred_df(timescale = "years_since_base") %>%
        mutate(
          cohort = "LA90_2",
          timescale = "years_since_baseline"
        )
    ) %>%
    saveRDS(glue('{output_path}/fig3_predictions.RDS'))
  #-----------------------------------------------------------------------------
  # Quantile Regression
  #-----------------------------------------------------------------------------
  log_step(glue("Quantile regression \n"), log_file)

  # fit on balanced data
  # get dfs
  d_k_balance  <- khandle_subsets[[which(subset_names == "full_b")]]
  d_s_balance  <- star_subsets[[which(subset_names == "full_b")]]
  d_la_balance <- la90_subsets[[which(subset_names == "full_b")]]

  d_la_balance_f <- d_la_balance %>% subset(gender=="Female")
  d_la_balance_m <- d_la_balance %>% subset(gender=="Male")






  balanced_dfs <- list(d_k_balance, d_s_balance,d_la_balance, d_la_balance_f, d_la_balance_m)
  cohorts <- c("khandle", "star","LAFull", "LA90_f", "LA90_m")

  # map QR fitting function to data
  d_QR <- map2(balanced_dfs, cohorts, fit_qr) %>%
    reduce(rbind)

  d_QR %>% saveRDS(glue('{output_path}/qr_results.RDS'))

}

#-------------------------------------------------------------------------------

tasks <- c("executive_function", "verbal_memory")
for(task in tasks){
  log_step(glue("Fitting models for {task} \n"), log_file)
  fit_all(task)
}

