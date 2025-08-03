run_analysis_tab2 <- function(dat){{
    f1 <- "score ~ age_c + wave + (1|STUDYID)"
    f2 <- "score ~ age_c + wave + gender + edu + race + (1|STUDYID)"
    f2_q <- "score ~ age_c +I(age_c^2)+ wave + gender + edu + race + (1|STUDYID)"
    f3 <- "score ~ age_c + wave + gender + race + edu +  gender:wave + (1|STUDYID)"
    f3_q <- "score ~ age_c +I(age_c^2)+ wave + gender + race + edu +  gender:wave + (1|STUDYID)"

  }

  # practice effects models
  m1 <- lmer(f1, dat = dat, REML = F)
  m2 <- lmer(f2, dat = dat, REML = F)
  m2_q <- lmer(f2_q, dat = dat, REML = F)
  m3 <- lmer(f3, dat = dat, REML = F)
  m3_q <- lmer(f3_q, dat = dat, REML = F)

  res = list(m1 = m1,
             m2 = m2,
             m2_q = m2_q,
             m3 = m3,
             m3_q = m3_q)

  return(res)
}

run_analysis_tab3 <- function(dat, off){

    f0.main <- "score ~ years_since_base + base_age_c + (1|STUDYID)"
    f0.main_q <- "score ~ years_since_base+I(years_since_base^2) + base_age_c + (1|STUDYID)"


    # directly adjust for PE
    f_direct <- "score ~ years_since_base + base_age_c + phone_ind2 + first_assess + gender + race + edu + (1|STUDYID)"
    f_direct_q <- "score ~ years_since_base +I(years_since_base^2)+ base_age_c + phone_ind2 + first_assess + gender + race + edu + (1|STUDYID)"

    # omit PE
    f_omit <- "score ~ years_since_base + base_age_c + phone_ind2 + gender + race + edu + (1|STUDYID)"
    f_omit_q <- "score ~ years_since_base +I(years_since_base^2)+ base_age_c + phone_ind2 + gender + race + edu + (1|STUDYID)"


    # grip strength
    grip.omit <- "score ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"
    grip.omit_q <- "score ~ years_since_base  +I(years_since_base^2)+ base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"

    grip.omit.gender <- "score ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+years_since_base:gripstrength_w1:gender+ (1|STUDYID)"
    grip.omit.gender_q <- "score ~ years_since_base +I(years_since_base^2)  + base_age_c+ gripstrength_w1 + phone_ind2 + gender + race + edu +years_since_base:gripstrength_w1+years_since_base:gripstrength_w1:gender+ (1|STUDYID)"

    grip.direct <- "score ~ years_since_base  + base_age_c+ gripstrength_w1 + phone_ind2 + first_assess + gender + race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"
    grip.direct_q <- "score ~ years_since_base +I(years_since_base^2)+ base_age_c+ gripstrength_w1 + phone_ind2 + first_assess + gender + race + edu +years_since_base:gripstrength_w1+ (1|STUDYID)"



  # tab 3 models
  # without forcing
  m_omit <- lmer(f_omit, dat = dat, REML = F)
  m_omit_q <- lmer(f_omit_q, dat = dat, REML = F)
  m_direct <- lmer(f_direct, dat = dat, REML = F)
  m_direct_q <- lmer(f_direct_q, dat = dat, REML = F)

  # forcing coef
  m_force <- lmer(f_omit, offset = off, dat = dat, REML = F)
  m_force_q <- lmer(f_omit_q, offset = off, dat = dat, REML = F)


  # # grip (time since baseline as the time scale)
  m_omit_grip <- lmer(grip.omit, dat = dat, REML = F)
  m_omit_grip_q <- lmer(grip.omit_q, dat = dat, REML = F)
  m_direct_grip<- lmer(grip.direct, dat = dat, REML = F)
  m_direct_grip_q<- lmer(grip.direct_q, dat = dat, REML = F)
  m_force_grip <- lmer(grip.omit, offset = off, dat = dat, REML = F)
  m_force_grip_q <- lmer(grip.omit_q, offset = off, dat = dat, REML = F)
  m_force_gender_grip <- lmer(grip.omit.gender, offset = off, dat = dat, REML = F)
  m_force_gender_grip_q <- lmer(grip.omit.gender_q, offset = off, dat = dat, REML = F)



  res = list(main=      list(m_omit = m_omit,
                             m_omit_q = m_omit_q,
                             m_direct = m_direct,
                             m_direct_q = m_direct_q,
                             m_force = m_force,
                             m_force_q = m_force_q),
             grip = list(m_omit = m_omit_grip,
                         m_omit_q = m_omit_grip_q,
                         m_direct = m_direct_grip,
                         m_direct_q = m_direct_grip_q,
                         m_force = m_force_grip,
                         m_force_q = m_force_grip_q))

  return(res)
}





