# models for table 2
run_analysis_tab2 <- function(dat, cohort = c("star", 'khandle','LA90', 'LA90_FEMALE','LA90_MALE')){
  if(cohort == "star"){
    f1 <- "score_z ~ age_c + wave + (1|STUDYID)"
    f2 <- "score_z ~ age_c + wave + gender + edu + (1|STUDYID)"
    f3 <- "score_z ~ age_c + wave + gender + edu + edu:wave + gender:wave + (1|STUDYID)"
  }
  else if(cohort == "khandle"|cohort == "LA90"){
    f1 <- "score_z ~ age_c + wave + (1|STUDYID)"
    f2 <- "score_z ~ age_c + wave + gender + edu + race + (1|STUDYID)"
    f3 <- "score_z ~ age_c + wave + gender + race + edu  + gender:wave + (1|STUDYID)"
  }
  else if(cohort == "LA90_FEMALE"|cohort == "LA90_MALE"){
    f1 <- "score_z ~ age_c + wave + (1|STUDYID)"
    f2 <- "score_z ~ age_c + wave + edu + race + (1|STUDYID)"
    f3 <- "score_z ~ age_c + wave + race + edu + (1|STUDYID)"
  }
  # practice effects models
  m1 <- lmer(f1, dat = dat, REML = F)
  m2 <- lmer(f2, dat = dat, REML = F)
  m3 <- lmer(f3, dat = dat, REML = F)

  res = list(m1 = m1,
             m2 = m2,
             m3 = m3)

  return(res)
}

# models for table 3
# 1. omit PE / omit PE (include age as a quadratic term)
# 2. directly adjust for PE (first_assess)/directly adjust for PE (first_assess)(include age as a quadratic term)
# 3. force PE (pt estimate from tab2, upper bound and lower bound from CI)/force PE (pt estimate from tab2, upper bound and lower bound from CI)(include age as a quadratic term)
run_analysis_tab3 <- function(dat, off_pt, off_low, off_high,  cohort = c("star", 'khandle','LA90')){
  if(cohort == "star"){
    # formulas (no adjustment for race)
    f_omit      <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + (1|STUDYID)"
    f_direct    <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + first_assess + (1|STUDYID)"

    # grip strength
    grip.omit   <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + gripstrength_w1 + years_since_base:gripstrength_w1 + (1|STUDYID)"
    grip.direct <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + gripstrength_w1 + years_since_base:gripstrength_w1 + first_assess + (1|STUDYID)"

  } else if(cohort == "khandle"|cohort == "LA90"){
    # formulas (same as star, but with race adjustment)
    f_omit      <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + race + (1|STUDYID)"
    f_direct    <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + race + first_assess + (1|STUDYID)"

    # grip strength
    grip.omit   <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + race + gripstrength_w1 + years_since_base:gripstrength_w1 + (1|STUDYID)"
    grip.direct <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + edu + race + gripstrength_w1 + years_since_base:gripstrength_w1 + first_assess + (1|STUDYID)"

  }

  # tab 3 models
  # omit PE
  m_omit       <- lmer(f_omit, dat = dat, REML = F)

  # directly adjust
  m_direct     <- lmer(f_direct, dat = dat, REML = F)

  # force pt estimate
  m_force_pt   <- lmer(f_omit, offset = off_pt, dat = dat, REML = F)

  # force lower bound
  m_force_low  <- lmer(f_omit, offset = off_low, dat = dat, REML = F)

  # force upper bound
  m_force_high <- lmer(f_omit, offset = off_high, dat = dat, REML = F)

  # grip (time since baseline as the time scale)
  m_omit_grip        <- lmer(grip.omit,   dat = dat, REML = F)
  m_direct_grip      <- lmer(grip.direct, dat = dat, REML = F)
  m_force_pt_grip    <- lmer(grip.omit, offset = off_pt,   dat = dat, REML = F)
  m_force_low_grip   <- lmer(grip.omit, offset = off_low,  dat = dat, REML = F)
  m5_force_high_grip <- lmer(grip.omit, offset = off_high, dat = dat, REML = F)

  res = list(main = list(m_omit = m_omit,
                         m_direct = m_direct,
                         m_force_pt = m_force_pt,
                         m_force_low = m_force_low,
                         m_force_high = m_force_high
                         ),
             grip = list(m_omit = m_omit_grip,
                         m_direct = m_direct_grip,
                         m_force_pt = m_force_pt_grip,
                         m_force_low = m_force_low_grip,
                         m_force_high = m5_force_high_grip)
             )

  return(res)
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



# create separate function to estimate executive function scores in LA90:
# this is because in LA90, we need to estimate the 95% CI differently

run_analysis_tab3_LA90_e <- function(dat,  off_pt, off_low, off_high){

  # f0.main  <- "score_z ~ years_since_base + base_age_c + (1|STUDYID)"

  # directly adjust for PE
  f_direct <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + race + edu + first_assess + (1|STUDYID)"
  # omit PE
  f_omit   <- "score_z ~ years_since_base + base_age_c + phone_ind2 + gender + race + edu + (1|STUDYID)"

  # grip strength
  grip.omit        <- "score_z ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"
  # grip.omit.gender <- "score_z ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+years_since_base:gripstrength_w1:gender+ (1|STUDYID)"
  grip.direct      <- "score_z ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + first_assess + gender + race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"

  # # tab 3 models
  # # without forcing
  # m_omit <- lmer(f_omit, dat = dat, REML = F)
  # m_direct <- lmer(f_direct, dat = dat, REML = F)
  # # forcing coef
  # m_force <- lmer(f_omit, offset = off, dat = dat, REML = F)
  #
  #
  # # # grip (time since baseline as the time scale)
  # m_omit_grip         <- lmer(grip.omit, dat = dat, REML = F)
  # m_direct_grip       <- lmer(grip.direct, dat = dat, REML = F)
  # m_force_grip        <- lmer(grip.omit, offset = off, dat = dat, REML = F)
  # m_force_gender_grip <- lmer(grip.omit.gender, offset = off, dat = dat, REML = F)

  # res = list(
  #   main = list(
  #     m_omit = m_omit,
  #     m_direct = m_direct,
  #     m_force = m_force
  #     ),
  #   grip = list(
  #     m_omit = m_omit_grip,
  #     m_direct = m_direct_grip,
  #     m_force = m_force_grip
  #     )
  #   )


  # from above
  # tab 3 models
  m_omit       <- lmer(f_omit, dat = dat, REML = F)
  m_direct     <- lmer(f_direct, dat = dat, REML = F)
  m_force_pt   <- lmer(f_omit, offset =  off_pt, dat = dat, REML = F)
  m_force_low  <- lmer(f_omit, offset = off_low, dat = dat, REML = F)
  m_force_high <- lmer(f_omit, offset = off_high, dat = dat, REML = F)
  # grip (time since baseline as the time scale)
  m_omit_grip        <- lmer(grip.omit,   dat = dat, REML = F)
  m_direct_grip      <- lmer(grip.direct, dat = dat, REML = F)
  m_force_pt_grip    <- lmer(grip.omit, offset =  off_pt,   dat = dat, REML = F)
  m_force_low_grip   <- lmer(grip.omit, offset = off_low,  dat = dat, REML = F)
  m5_force_high_grip <- lmer(grip.omit, offset = off_high, dat = dat, REML = F)

  res = list(main = list(m_omit = m_omit,
                         m_direct = m_direct,
                         m_force_pt = m_force_pt,
                         m_force_low = m_force_low,
                         m_force_high = m_force_high
  ),
  grip = list(m_omit = m_omit_grip,
              m_direct = m_direct_grip,
              m_force_pt = m_force_pt_grip,
              m_force_low = m_force_low_grip,
              m_force_high = m5_force_high_grip)
  )

  return(res)
}




run_analysis_tab3_LA90_gender <- function(dat,off_pt, off_low, off_high){

  # f0.main  <- "score_z ~ years_since_base + base_age_c + (1|STUDYID)"

  # directly adjust for PE
  f_direct <- "score_z ~ years_since_base + base_age_c + phone_ind2 + race + edu + first_assess + (1|STUDYID)"
  # omit PE
  f_omit   <- "score_z ~ years_since_base + base_age_c + phone_ind2 +  race + edu + (1|STUDYID)"

  # grip strength
  grip.omit        <- "score_z ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 +  race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"
  # grip.omit.gender <- "score_z ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+years_since_base:gripstrength_w1:gender+ (1|STUDYID)"
  grip.direct      <- "score_z ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + first_assess +  race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"

  # # tab 3 models
  # # without forcing
  # m_omit <- lmer(f_omit, dat = dat, REML = F)
  # m_direct <- lmer(f_direct, dat = dat, REML = F)
  # # forcing coef
  # m_force <- lmer(f_omit, offset = off, dat = dat, REML = F)
  #
  #
  # # # grip (time since baseline as the time scale)
  # m_omit_grip         <- lmer(grip.omit, dat = dat, REML = F)
  # m_direct_grip       <- lmer(grip.direct, dat = dat, REML = F)
  # m_force_grip        <- lmer(grip.omit, offset = off, dat = dat, REML = F)
  # m_force_gender_grip <- lmer(grip.omit.gender, offset = off, dat = dat, REML = F)

  # res = list(
  #   main = list(
  #     m_omit = m_omit,
  #     m_direct = m_direct,
  #     m_force = m_force
  #     ),
  #   grip = list(
  #     m_omit = m_omit_grip,
  #     m_direct = m_direct_grip,
  #     m_force = m_force_grip
  #     )
  #   )


  # from above
  # tab 3 models
  m_omit       <- lmer(f_omit, dat = dat, REML = F)
  m_direct     <- lmer(f_direct, dat = dat, REML = F)
  m_force_pt   <- lmer(f_omit, offset =  off_pt, dat = dat, REML = F)
  m_force_low  <- lmer(f_omit, offset = off_low, dat = dat, REML = F)
  m_force_high <- lmer(f_omit, offset = off_high, dat = dat, REML = F)
  # grip (time since baseline as the time scale)
  m_omit_grip        <- lmer(grip.omit,   dat = dat, REML = F)
  m_direct_grip      <- lmer(grip.direct, dat = dat, REML = F)
  m_force_pt_grip    <- lmer(grip.omit, offset =  off_pt,   dat = dat, REML = F)
  m_force_low_grip   <- lmer(grip.omit, offset = off_low,  dat = dat, REML = F)
  m5_force_high_grip <- lmer(grip.omit, offset = off_high, dat = dat, REML = F)

  res = list(main = list(m_omit = m_omit,
                         m_direct = m_direct,
                         m_force_pt = m_force_pt,
                         m_force_low = m_force_low,
                         m_force_high = m_force_high
  ),
  grip = list(m_omit = m_omit_grip,
              m_direct = m_direct_grip,
              m_force_pt = m_force_pt_grip,
              m_force_low = m_force_low_grip,
              m_force_high = m5_force_high_grip)
  )

  return(res)
}
