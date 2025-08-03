# load packages
library(pacman)
p_load(
  "here", "haven", "tidyverse", "stargazer", "gridExtra", "lmerTest", "ggplot2",
  "kableExtra", "fs","dplyr"
  )

# ---- KHANDLE ----
# read dataset
if(users == "Ruijia"){
  dat <- read_sas("/Users/chenruijia/Box/LA90 Study/KHANDLE_shared by camilla/Raw KHANDLE Data/Most Recent Raw Data Sets/all waves and MHC/khandle_all_waves_20210818.sas7bdat")
} else if(users == "Kaitlin"){
  dat <- read_sas("~/Dropbox/KHANDLE/Raw KHANDLE Data/Most Recent Raw Data Sets/all waves and MHC/khandle_all_waves_20210818.sas7bdat")
} else if(users == "Kaiser"){
  # this file is specified in the file '00_run-all.R``
  dat <- read_sas(dat_KHANDLE)
}






# selecting variables
dat2 <- dat %>%
  select(STUDYID,
         W1_SENAS_vrmem, W2_SENAS_vrmem, W3_SENAS_vrmem,
         W1_SENAS_exec, W2_SENAS_exec, W3_SENAS_exec,
         W1_INTERVIEW_AGE, W2_INTERVIEW_AGE, W3_INTERVIEW_AGE,
         W2_SENAS_telephone, W3_SENAS_telephone,
         W1_EDU_EDUCATION, W1_EDU_EDUCATION_TEXT, W1_EDU_LONGCERT,
         W1_D_GENDER, W1_D_RACE_SUMMARY, starts_with("W1_DAILY_LIVING_ADL"),
         starts_with("W1_PHYS_GRI"))


##remove missing executive function or verbal moemory at baseline
dat2<-dat2[!(is.na(dat2$W1_SENAS_vrmem) | is.na(dat2$W1_SENAS_exec)), ]




# long format cognitive scores (executive function and verbal memory)
### Kaitlin note: FYI, in the original code there are rows that are preserved
### for W3 that don't have SENAS assessments (e.g. someone with only two time
### points will have a row for wave 3 that has NA values for "score")
### Depending on how this is handled later, it may not be a problem, but in
### my opinion, it would be cleaner to remove those NA rows.
### I've added a line to the pivot_longer that will do that
scores_long <- dat2 %>%
  select(STUDYID, W1_SENAS_vrmem, W2_SENAS_vrmem, W3_SENAS_vrmem,
         W1_SENAS_exec, W2_SENAS_exec, W3_SENAS_exec) %>%
  pivot_longer(cols = W1_SENAS_vrmem:W3_SENAS_exec,
               names_to = "wave",
               values_to = "score" #,
               ### added this row to drop
               # values_drop_na = TRUE
               ) %>%
  mutate(wave = str_replace(wave, "SENAS_", "")) %>%
  separate(wave, into = c("wave", "test"), sep = "_")

# long format age
age_long <- dat2 %>%
  select(STUDYID, W1_INTERVIEW_AGE, W2_INTERVIEW_AGE, W3_INTERVIEW_AGE) %>%
  pivot_longer(cols = W1_INTERVIEW_AGE:W3_INTERVIEW_AGE,
               names_to = "wave",
               values_to = "age"#,
               ### added this row to drop
               # values_drop_na = TRUE
               ) %>%
  mutate(wave = str_replace(wave, "_INTERVIEW_AGE", ""))

# baseline age
### This is unecessary
base_age <- dat2 %>% select(STUDYID, "base_age" = W1_INTERVIEW_AGE)

# long format phone indicator
phone_long <- dat2 %>%
  select(STUDYID, W2_SENAS_telephone, W3_SENAS_telephone) %>%
  pivot_longer(cols = W2_SENAS_telephone:W3_SENAS_telephone,
               names_to = "wave",
               values_to = "phone_ind") %>%
  mutate(wave = str_replace(wave, "_SENAS_telephone", ""),
         phone_ind2 = case_when(phone_ind == "" ~ NA_real_,
                                phone_ind == "N" ~ 0,
                                phone_ind == "Y" ~ 1))

# recode covariates
covars <- dat2 %>%
  select(STUDYID, starts_with("W1_EDU"), W1_D_GENDER, W1_D_RACE_SUMMARY,
         starts_with("W1_DAILY_LIVING_ADL"),starts_with("W1_PHYS_GRI")) %>%
  filter(W1_D_RACE_SUMMARY != "Native American") %>%
  mutate(
    educ_cat = case_when(
      W1_EDU_EDUCATION == 1 ~ "Some college but no degree",
      W1_EDU_EDUCATION == 2 ~ "Associate's degree",
      W1_EDU_EDUCATION %in% c(3,4,5)  ~ "Bachelor's degree",
      ### Kaitlin: you don't need these rows: the case_when will return NA
      ### for any cases not explicitly defined in the statement
      W1_EDU_EDUCATION == 88 ~ NA_character_,
      W1_EDU_EDUCATION == 99 ~ NA_character_,
      W1_EDU_EDUCATION == 0 ~ "High school or lower"
    ),
    W1_D_GENDER = as.factor(ifelse(W1_D_GENDER == 1, "Male", "Female")),
    W1_D_GENDER = relevel(W1_D_GENDER, ref = "Male"),
    ### Kaitlin: this works, but I'd recomend using mutate across() instead
    ### to reduce the chance of a copy paste error
    ADL1 = ifelse(W1_DAILY_LIVING_ADL1 >= 1 & W1_DAILY_LIVING_ADL1 <= 4,
                  W1_DAILY_LIVING_ADL1, NA),
    ADL2 = ifelse(W1_DAILY_LIVING_ADL2 >= 1 & W1_DAILY_LIVING_ADL2 <= 4,
                  W1_DAILY_LIVING_ADL2, NA),
    ADL3 = ifelse(W1_DAILY_LIVING_ADL3 >= 1 & W1_DAILY_LIVING_ADL3 <= 4,
                  W1_DAILY_LIVING_ADL3, NA),
    ADL4 = ifelse(W1_DAILY_LIVING_ADL4 >= 1 & W1_DAILY_LIVING_ADL4 <= 4,
                  W1_DAILY_LIVING_ADL4, NA),
    ADL5 = ifelse(W1_DAILY_LIVING_ADL5 >= 1 & W1_DAILY_LIVING_ADL5 <= 4,
                  W1_DAILY_LIVING_ADL5, NA),
    ADL6 = ifelse(W1_DAILY_LIVING_ADL6 >= 1 & W1_DAILY_LIVING_ADL6 <= 4,
                  W1_DAILY_LIVING_ADL6, NA),
    ADL7 = ifelse(W1_DAILY_LIVING_ADL7 >= 1 & W1_DAILY_LIVING_ADL7 <= 4,
                  W1_DAILY_LIVING_ADL7, NA),
    ADL8 = ifelse(W1_DAILY_LIVING_ADL8 >= 1 & W1_DAILY_LIVING_ADL8 <= 4,
                  W1_DAILY_LIVING_ADL8, NA),
    ADL9 = ifelse(W1_DAILY_LIVING_ADL9 >= 1 & W1_DAILY_LIVING_ADL9 <= 4,
                  W1_DAILY_LIVING_ADL9, NA)) %>%
  ### this is correct, but I'd recommend a case_when - easier to read and check
  ### than a nested ifelse
  mutate(gripstrength_w1_full = ifelse(W1_PHYS_GRIPDOM == 1,(W1_PHYS_GRIPR1+ W1_PHYS_GRIPR2 + W1_PHYS_GRIPR3)/3,
                                       ifelse(W1_PHYS_GRIPDOM == 2, (W1_PHYS_GRIPL1 + W1_PHYS_GRIPL2 + W1_PHYS_GRIPL3)/3,
                                              ifelse(is.na(W1_PHYS_GRIPDOM) & !is.na(W1_PHYS_GRIPR1), (W1_PHYS_GRIPR1+ W1_PHYS_GRIPR2 + W1_PHYS_GRIPR3)/3,
                                                     ifelse(is.na(W1_PHYS_GRIPDOM) & !is.na(W1_PHYS_GRIPL1), (W1_PHYS_GRIPL1 + W1_PHYS_GRIPL2 + W1_PHYS_GRIPL3)/3, NA)))))%>%
  rowwise() %>%
  mutate(ADL = sum(ADL1, ADL2, ADL3, ADL4, ADL5, ADL6, ADL7, ADL8, ADL9)/9) %>%
  ### this is fine, but you might as well just name these variables what you
  ### want to name them when you create them originally
  select(STUDYID, gripstrength_w1_full,
         'edu' = educ_cat,
         'gender' = W1_D_GENDER,
         'race' = W1_D_RACE_SUMMARY,
         'adl' = ADL) %>%
  ### Kaitlin: add a filter here to remove those with missing covariate data
  ### before joining - this will solve your filtering problem below
  filter(
    !is.na(gender),
    !is.na(edu),
    !is.na(race)
  )

# calculate mean age at baseline to center
### do you want this on the whole sample, or the anayltic sample?
### I checked, and when rounded, it's the same, but something to consider
mean_base_age <- round(mean(base_age$base_age, na.rm = T))

# join all long datasets on ID and wave
# center age and baseline age
# make years since baseline variable
# make first assessment indicator = 1 if wave 1
dat2_long <- scores_long %>%
  left_join(age_long) %>%
  left_join(base_age) %>%
  left_join(phone_long) %>%
  # change to right join to keep only participants with baseline covariate data
  # left_join(covars) %>%
  right_join(covars) %>%
  select(-phone_ind) %>%
  mutate(age_c = age - mean_base_age,
         base_age_c = base_age - mean_base_age,
         years_since_base = age_c - base_age_c,
         first_assess = as.factor(ifelse(wave == "W1", 1, 0)),
         ### I would move this to the place where you code phone_ind2 originally
         phone_ind2 = as.factor(ifelse(wave == "W1", 0, phone_ind2))) %>%
  mutate(gender = as.factor(gender), ### Kaitlin: you already did this above
         gender = relevel(gender, ref = "Male"),
         edu = factor(edu,
                      levels = c("High school or lower",
                                 "Some college but no degree",
                                 "Associate's degree",
                                 "Bachelor's degree")),
         race = factor(race,
                       levels = c("White",
                                  "Black",
                                  "LatinX",
                                  "Asian")))

###remove missing covariates, baseline executive function and baseline verbal memory variables
### This filter is not behaving how (I think) you want it to.
### For people with missing baseline covariates, it only removes their wave 1
### row, but keeps all other rows.
### You also restricted the sample to those not missing baseline SENAS
### previously, but not here. So the comment is incorrect.
### I'd recommend restricting the "covars" dataset to those not missing data,
### and then right joining on "scores_long" to restrict to those with
### baseline covariate data. I've added rows above to implement this.
### I've uncommented this line because it is incorrect, and the filters are
### now applied above
# dat3_long<-dat2_long [!(( is.na(dat2_long$base_age_c)| is.na(dat2_long$gender)| is.na(dat2_long$edu)| is.na(dat2_long$race)) & dat2_long$wave== "W1"), ]

### Kaitlin: rename dat2_long to dat3_long to work with rest of code
dat3_long <- dat2_long %>%
  filter(!is.na(base_age_c))

###after getting the analytical sample - recode the handgrip strength variable through including males and females standard deviation
### Kaitlin: these means and standard deviations are off because
dat3_long %>%
  group_by(wave, gender) %>%
  summarise(mean(gripstrength_w1_full, na.rm=TRUE),
            sd(gripstrength_w1_full, na.rm=TRUE))
###male 0.877, female 0.573
dat3_long <- dat3_long %>%  mutate (gripstrength_w1=ifelse (gender=="Male", gripstrength_w1_full/8.77/10,
                                                            ifelse (gender=="Female",gripstrength_w1_full/5.73/10, NA)))

saveRDS(dat3_long, 'results/01_data/khandle_long.RDS')

#---- verbal memory df----
d_vm <- dat3_long %>% filter(test == "vrmem")

# z-score verbal memory score
mean_vm <- mean(dat$W1_SENAS_vrmem, na.rm = T)
sd_vm <- sd(dat$W1_SENAS_vrmem, na.rm = T)
d_vm$score_z <- (d_vm$score - mean_vm)/(sd_vm)

# d_vm is the final dataframe for verbal memory

#---- executive function df -----
d_e <- dat3_long %>% filter(test == "exec")

# z-score executive function score
mean_exec <- mean(dat$W1_SENAS_exec, na.rm = T)
sd_exec <- sd(dat$W1_SENAS_exec, na.rm = T)
d_e$score_z <- (d_e$score - mean_exec)/(sd_exec)

# d_e is the final dataframe for executive function

saveRDS(d_vm, 'results/01_data/d_vm_k.RDS')
saveRDS(d_e, 'results/01_data/d_e_k.RDS')


# ---- STAR ----
rm(d_vm)
rm(d_e)

# read dataset
if(users == "Ruijia"){
  dat <- read_sas("/Users/chenruijia/Box/LA90 Study/KHANDLE_shared by camilla/Raw STAR Data/star_all_waves_20210914.sas7bdat")
} else if (users == "Kaitlin"){
  dat <- read_sas("~/Dropbox/KHANDLE/Raw STAR Data/__archive/14Sep2021_allwvs/star_all_waves_20210914.sas7bdat")
} else if(users == "Kaiser"){
  # this file is specified in the file '00_run-all.R`
  dat <- read_sas(dat_STAR)
}

# selecting variables
dat2 <- dat %>%
  select(STUDYID,
         W1_SENAS_vrmem, W2_SENAS_vrmem, W3_SENAS_vrmem,
         W1_SENAS_exec, W2_SENAS_exec, W3_SENAS_exec,
         W1_INTERVIEW_AGE, W2_INTERVIEW_AGE, W3_INTERVIEW_AGE,
         W2_SENAS_Telephone, W3_SENAS_Telephone,
         W1_EDU_EDUCATION, W1_EDU_EDUCATION_TEXT, W1_EDU_LONGCERT,
         W1_D_GENDER, W1_D_RACE_SUMMARY, starts_with("W1_DAILY_LIVING_ADL"),starts_with("W1_PHYS_GRI"))

# remove missing scores at baseline
dat2<-dat2[!(is.na(dat2$W1_SENAS_vrmem) | is.na(dat2$W1_SENAS_exec)), ]


# long format cognitive scores (executive function and verbal memory)
scores_long <- dat2 %>%
  select(STUDYID, W1_SENAS_vrmem, W2_SENAS_vrmem, W3_SENAS_vrmem,
         W1_SENAS_exec, W2_SENAS_exec, W3_SENAS_exec) %>%
  pivot_longer(cols = W1_SENAS_vrmem:W3_SENAS_exec,
               names_to = "wave",
               values_to = "score") %>%
  mutate(wave = str_replace(wave, "SENAS_", "")) %>%
  separate(wave, into = c("wave", "test"), sep = "_")

# long format age
age_long <- dat2 %>%
  select(STUDYID, W1_INTERVIEW_AGE, W2_INTERVIEW_AGE, W3_INTERVIEW_AGE) %>%
  pivot_longer(cols = W1_INTERVIEW_AGE:W3_INTERVIEW_AGE,
               names_to = "wave",
               values_to = "age") %>%
  mutate(wave = str_replace(wave, "_INTERVIEW_AGE", ""))

# baseline age
base_age <- dat2 %>% select(STUDYID, "base_age" = W1_INTERVIEW_AGE)

# long format phone indicator
phone_long <- dat2 %>%
  select(STUDYID, W2_SENAS_Telephone, W3_SENAS_Telephone) %>%
  pivot_longer(cols = W2_SENAS_Telephone:W3_SENAS_Telephone,
               names_to = "wave",
               values_to = "phone_ind") %>%
  mutate(wave = str_replace(wave, "_SENAS_Telephone", ""),
         phone_ind2 = case_when(phone_ind == "" ~ 0,
                                phone_ind == "Y" ~ 1))

# recode covariates
covars <- dat2 %>%
  select(STUDYID, starts_with("W1_EDU"), W1_D_GENDER, W1_D_RACE_SUMMARY,
         starts_with("W1_DAILY_LIVING_ADL"),starts_with("W1_PHYS_GRI")) %>%
  mutate(
    educ_cat = case_when(
      W1_EDU_EDUCATION == 1 ~ "Some college but no degree",
      W1_EDU_EDUCATION == 2 ~ "Associate's degree",
      W1_EDU_EDUCATION %in% c(3,4,5)  ~ "Bachelor's degree",
      W1_EDU_EDUCATION == 88 ~ NA_character_,
      W1_EDU_EDUCATION == 99 ~ NA_character_,
      W1_EDU_EDUCATION == 0 ~ "High school or lower"
    ),
    W1_D_GENDER = as.factor(ifelse(W1_D_GENDER == 1, "Male", "Female")),
    W1_D_GENDER = relevel(W1_D_GENDER, ref = "Male"),
    gripstrength_w1_full = ifelse(W1_PHYS_GRIPDOM == 1,(W1_PHYS_GRIPR1+ W1_PHYS_GRIPR2 + W1_PHYS_GRIPR3)/3,
                           ifelse(W1_PHYS_GRIPDOM == 2, (W1_PHYS_GRIPL1 + W1_PHYS_GRIPL2 + W1_PHYS_GRIPL3)/3,
                                                 ifelse(is.na(W1_PHYS_GRIPDOM) & !is.na(W1_PHYS_GRIPR1), (W1_PHYS_GRIPR1+ W1_PHYS_GRIPR2 + W1_PHYS_GRIPR3)/3,
                                                 ifelse(is.na(W1_PHYS_GRIPDOM) & !is.na(W1_PHYS_GRIPL1), (W1_PHYS_GRIPL1 + W1_PHYS_GRIPL2 + W1_PHYS_GRIPL3)/3, NA))))) %>%
  rowwise() %>%
  filter(W1_D_RACE_SUMMARY == "Black") %>%
  select(STUDYID, gripstrength_w1_full,
         'edu' = educ_cat,
         'gender' = W1_D_GENDER,
         'race' = W1_D_RACE_SUMMARY) %>%
  ### Kaitlin: add a filter here to remove those with missing covariate data
  ### before joining - this will solve your filtering problem below
  filter(
    !is.na(gender),
    !is.na(edu),
    !is.na(race)
  )

# calculate mean age at baseline to center
mean_base_age <- round(mean(base_age$base_age, na.rm = T),0)

# join all long datasets on ID and wave
# center age and baseline age
# make years since baseline variable
# make first assessment indicator = 1 if wave 1
dat2_long <- scores_long %>%
  left_join(age_long) %>%
  left_join(base_age) %>%
  left_join(phone_long) %>%
  ### Changed to restrict to those with covariate data
  # left_join(covars) %>%
  right_join(covars) %>%
  select(-phone_ind) %>%
  mutate(age_c = age - mean_base_age,
         base_age_c = base_age - mean_base_age,
         years_since_base = age_c - base_age_c,
         first_assess = as.factor(ifelse(wave == "W1", 1, 0)),
         phone_ind2 = as.factor(ifelse(wave == "W1", 0, phone_ind2)))%>%
  mutate(gender = as.factor(gender),
         gender = relevel(gender, ref = "Male"),
         edu = factor(edu,
                      levels = c("High school or lower",
                                 "Some college but no degree",
                                 "Associate's degree",
                                 "Bachelor's degree"))) %>%
  drop_na(race)


###remove missing covariates, baseline executive function and baseline verbal memory variables

# dat3_long<-dat2_long [!(( is.na(dat2_long$base_age_c)| is.na(dat2_long$gender)| is.na(dat2_long$edu)| is.na(dat2_long$race)) & dat2_long$wave== "W1"), ]
dat3_long <- dat2_long %>% filter(!is.na(base_age_c))
###after getting the analytical sample - recode the handgrip strength variable through including males and females standard deviation
dat3_long %>%
  group_by(wave, gender) %>%
  summarise(mean(gripstrength_w1_full, na.rm=TRUE),
            sd(gripstrength_w1_full, na.rm=TRUE))
###male 10.9, female 6.47
dat3_long <- dat3_long %>%  mutate (gripstrength_w1=ifelse (gender=="Male", gripstrength_w1_full/10.9/10,
                                                            ifelse (gender=="Female",gripstrength_w1_full/6.47/10, NA)))



saveRDS(dat3_long, 'results/01_data/star_long.RDS')
# dat3_long_R <- readRDS("../Code_PE_Paper 1/results/01_data/star_long.RDS")

#---- verbal memory df----
d_vm <- dat3_long %>% filter(test == "vrmem")

# z-score verbal memory score
mean_vm <- mean(dat$W1_SENAS_vrmem, na.rm = T)
sd_vm <- sd(dat$W1_SENAS_vrmem, na.rm = T)
d_vm$score_z <- (d_vm$score - mean_vm)/(sd_vm)

# d_vm is the final dataframe for verbal memory

#---- executive function df -----
d_e <- dat3_long %>% filter(test == "exec")

# z-score executive function score
mean_exec <- mean(dat$W1_SENAS_exec, na.rm = T)
sd_exec <- sd(dat$W1_SENAS_exec, na.rm = T)
d_e$score_z <- (d_e$score - mean_exec)/(sd_exec)

# d_e is the final dataframe for executive function
saveRDS(d_vm, 'results/01_data/d_vm_s.RDS')
saveRDS(d_e, 'results/01_data/d_e_s.RDS')


# # ---- LA90 ----

#users="Ruijia"
## Ingest ----
if(users == "Ruijia"){
  raw_wide_data <- read_sas("/Users/chenruijia/Dropbox (Harvard University)/KHANDLE/Raw LA90 Data/la90_ucd_wide_v1to7_20211229.sas7bdat")
  raw_long_data <- read_sas("/Users/chenruijia/Dropbox (Harvard University)/KHANDLE/Raw LA90 Data/la90_ucd_long_v1to7_20211229.sas7bdat")
  raw_date_data <- read_sas("/Users/chenruijia/Dropbox (Harvard University)/KHANDLE/Raw LA90 Data/la90_ucd_long_v1to7_dt_20211229.sas7bdat")
  #mhc <- read_sas("C:/Users/rchen13/Dropbox (Harvard University)/KHANDLE/Raw LA90 Data/mhcla_sub.sas7bdat")
} else if(users == "Kaiser"){
  raw_wide_data <- read_sas(dat_LA90_wide)
  raw_long_data <- read_sas(dat_LA90_long)
  raw_date_data <- read_sas(dat_LA90_date)
  #mhc <- read_sas(dat_LA90_mhc)
} else if(users == "Kaitlin"){
  raw_wide_data <- read_sas("~/Dropbox/KHANDLE/Raw LA90 Data/la90_ucd_wide_v1to7_20211229.sas7bdat")
  raw_long_data <- read_sas("~/Dropbox/KHANDLE/Raw LA90 Data/la90_ucd_long_v1to7_20211229.sas7bdat")
  raw_date_data <- read_sas("~/Dropbox/KHANDLE/Raw LA90 Data/la90_ucd_long_v1to7_dt_20211229.sas7bdat")
 # mhc <- read_sas("~/Dropbox/KHANDLE/Raw LA90 Data/mhcla_sub.sas7bdat")
}


##first merge wide data,long data, and study date data by ID

dat_la_nodate_mhc <-merge(raw_long_data,  raw_wide_data, by = "StudyID", all = TRUE)
dat_la <-merge (dat_la_nodate_mhc, raw_date_data, by = c("StudyID", "Visit"))


#select variables that will be used in the dataset
dat2_la <- dat_la %>%
  select(
    StudyID,
    starts_with (c("D_SENAS",  "MHC", "ADL", "PHYS_GRI")),
    ends_with(c(
      "INTERVIEW_AGE", "Phone_Visit", "LANGUAGE", "VISIT_DATE"
    )),
    DERIVED_EDUCATION,
    DERIVED_RACE1,
    EDU_EDUCATION,
    D_GENDER,
    Visit,
    GDS_SCORE,
    SENAS_WLL_Visual_Stimuli,
    VISIT_DATE
  )

##recode covariates
# covariates
dat3_la <- dat2_la %>%
  mutate (
    educ_cat = case_when(
      EDU_EDUCATION == 1 ~ "Some college but no degree",
      EDU_EDUCATION == 2 ~ "Associate's degree",
      EDU_EDUCATION %in% c(3, 4, 5)  ~ "Bachelor's degree",
      EDU_EDUCATION == 88 ~ NA_character_,
      EDU_EDUCATION == 99 ~ NA_character_,
      EDU_EDUCATION == 0 ~ "High school or lower"
    ),
    D_GENDER = ifelse(D_GENDER == 1, "Male", "Female"),
    DERIVED_RACE1 = ifelse(DERIVED_RACE1 == 9, NA, DERIVED_RACE1),
    DERIVED_RACE1 = factor(
      DERIVED_RACE1,
      levels = c(0, 1, 2, 3, 4),
      labels = c("LatinX", "White", "Black", "Other", "Asian")
    ),
    GDS_SCORE = ifelse(GDS_SCORE == 77 |
                         GDS_SCORE == 88, NA, GDS_SCORE)
    # hyper = factor(
    #   mhc_hypBP,
    #   levels = c(0, 1),
    #   labels = c("No", "Yes")
    # ),
    # hchol = factor(
    #   mhc_hchol,
    #   levels = c(0, 1),
    #   labels = c("No", "Yes")
    # ),
    # dia = factor(
    #   MHC_DIABETES,
    #   levels = c(0, 1),
    #   labels = c("No", "Yes")
    # ),
    # obe = factor(
    #   mhc_obese,
    #   levels = c(0, 1),
    #   labels = c("No", "Yes")
    # ),
    # smoke_ever = ifelse(MHC_SMOKE == 1, 0, 1)
  ) %>%
  # mutate(smoke_ever = factor(
  #   smoke_ever,
  #   levels = c(0, 1),
  #   labels = c("No", "Yes")
  # )) %>%
  mutate (
    'wave' = Visit,
    'phone' = SENAS_Phone_Visit,
    'edu' = educ_cat,
    'gender' = D_GENDER,
    'race' = DERIVED_RACE1
    # 'hypertension' = hyper,
    # 'high cholesterol' = hchol,
    # 'diabetes' = dia,
    # 'obesity' = obe
  )

#reorder the levels for race and education
dat3_la <- dat3_la %>%
  mutate(race=factor(race, levels= c("White","Black","LatinX","Asian","other")),
         edu=factor(edu, levels=c("High school or lower","Some college but no degree","Associate's degree", "Bachelor's degree")))



#filter data to only have wave 1 and recode variables for wave 1
base_dat <- dat2_la %>% filter(Visit == 1)

base <- base_dat %>%  mutate (
  base_age = INTERVIEW_AGE,
  base_GDS = GDS_SCORE
) %>%
  mutate (mean_base_age_la = round(mean(base_age, na.rm = T), 0),
          ) %>%
  mutate(gripstrength_w1_full = ifelse(PHYS_GRIPDOM == 1,(PHYS_GRIPR1+ PHYS_GRIPR2 + PHYS_GRIPR3)/3,
                                  ifelse(PHYS_GRIPDOM == 2, (PHYS_GRIPL1 + PHYS_GRIPL2 + PHYS_GRIPL3)/3,
                                  ifelse(is.na(PHYS_GRIPDOM) & !is.na(PHYS_GRIPR1), (PHYS_GRIPR1+ PHYS_GRIPR2 + PHYS_GRIPR3)/3,
                                   ifelse(is.na(PHYS_GRIPDOM) & !is.na(PHYS_GRIPL1),
                                (PHYS_GRIPL1+ PHYS_GRIPL2 + PHYS_GRIPL3)/3, NA))))) %>%
  select(
    StudyID,
    base_age,
    base_GDS,
    mean_base_age_la,
    gripstrength_w1_full
  )



#recode baseline variables
##merge dat3_la and AGE
dat4_la <- merge (dat3_la, base, by = "StudyID")


##create new variable
dat5_la <- dat4_la %>%
  mutate (
    age_c = INTERVIEW_AGE - mean_base_age_la,
    base_age_c = base_age - mean_base_age_la,
    years_since_base = age_c - base_age_c,
    first_assess = ifelse(wave == "1", 1, 0)
  ) %>%
  mutate(
    gender = as.factor(gender),
    gender = relevel(gender, ref = "Male"),
    edu = as.factor(edu),
    edu = relevel(edu, ref = "High school or lower"),
    race = relevel(race, ref = "White"),
    StudyID = as.character(StudyID),
    phone_ind2 = as.factor(phone),
    wave = factor(
      wave,
      levels = c(1,2,3,4,5,6),
      labels = c("W1", "W2","W3", "W4","W5", "W6")
    ),
    first_assess = as.factor(first_assess)
  ) %>%
  filter(race != "Other")



######drop missing exposure, covariates, and outcomes- except for handgrip strength. Only a subset of people in the
####remove baseline executive function that is missing
### Kaitlin: this filter is incorrect for same reason above filters were incorrect
### fixed below
# dat6_la <-dat5_la [!((is.na(dat5_la$D_SENAS_EXEC_Z) |is.na(dat5_la$D_SENAS_VRMEM_Z)| is.na(dat5_la$base_age_c)| is.na(dat5_la$gender)| is.na(dat5_la$edu)| is.na(dat5_la$race)) & dat5_la$wave== "W1"), ]
dat6_la <- dat5_la %>%
  filter(wave == "W1") %>%
  filter(complete.cases(D_SENAS_EXEC_Z, D_SENAS_VRMEM_Z, base_age_c, gender, edu, race)) %>%
  select(StudyID, wave, D_SENAS_EXEC_Z, D_SENAS_VRMEM_Z, base_age_c, gender, edu, race) %>%
  distinct(StudyID) %>%
  left_join(dat5_la)

###after getting the analytical sample - recode the handgrip strength variable through including males and females standard deviation
dat6_la %>%
  group_by(wave, gender) %>%
  summarise(mean(gripstrength_w1_full, na.rm=TRUE),
            sd(gripstrength_w1_full, na.rm=TRUE))
# get mean and standard deviation of handgrip strength for rescaling
handgrip_summary <- dat6_la %>%
  group_by(wave, gender) %>%
  summarise(
    mean = round(mean(gripstrength_w1_full, na.rm=TRUE), 2),
    sd   = round(sd(gripstrength_w1_full, na.rm=TRUE), 2),
    .groups = "keep"
  ) %>%
  filter(!is.na(gender))

hgs_w1_f <- handgrip_summary %>%
  filter(
    wave == "W1",
    gender == "Female"
  ) %>%
  pull(sd)

hgs_w1_m <- handgrip_summary %>%
  filter(
    wave == "W1",
    gender == "Male"
  ) %>%
  pull(sd)

# scale handgrip strength by standard deviation
dat6_la <- dat6_la %>%
  mutate(
    gripstrength_w1 = case_when(
      gender == "Male" ~ gripstrength_w1_full/hgs_w1_m/10,
      gender == "Female" ~ gripstrength_w1_full/hgs_w1_f/10
    ),
    .after = gripstrength_w1_full
  )

###male 6.54, female 5.02
### Kaitlin: the means and sds calculated by this code don't match what's in the comments
### I get male: 6.79, female 5.00
# dat6_la <- dat6_la  %>%  mutate (gripstrength_w1=ifelse (gender=="Male", gripstrength_w1_full/6.54/10,
#                                                        ifelse (gender=="Female",gripstrength_w1_full/5.02/10, NA)))


### Kaitlin
### there are rows missing both SENAS scores and some rows that got duplicated
### remove them
dat6_la <- dat6_la %>%
  filter(!is.na(D_SENAS_EXEC_Z) | !is.na(D_SENAS_VRMEM_Z)) %>%
  distinct() %>%
  ungroup()

###########################################make a dataframe for executive function################################################
d_e_la <- dat6_la  %>% select(-contains("VRMEM"))%>% mutate(domain='exec')
##rename the variables
d_e_la<-d_e_la %>% rename(score_z=D_SENAS_EXEC_Z, STUDYID=StudyID,age=INTERVIEW_AGE)
####create a dataset called dataset that excluded missing handgrip strength at baseline
d_e_la2 <-d_e_la [!(is.na(d_e_la$gripstrength_w1)), ]


###########################################make a dataframe for verbal memory n###################################################
d_vm_la <- dat6_la  %>% select(-contains("EXEC"))%>% mutate(domain='vrmem')
##rename the variables
d_vm_la<-d_vm_la %>% rename(score_z=D_SENAS_VRMEM_Z, STUDYID=StudyID,age=INTERVIEW_AGE)
####create a dataset called dataset that excluded missing handgrip strength at baseline
d_vm_la2 <-d_vm_la [!(is.na(d_e_la$gripstrength_w1)), ]


##save the data
saveRDS(d_e_la, 'results/01_data/d_e_la.RDS')
saveRDS(d_e_la2, 'results/01_data/d_e_la2.RDS')
saveRDS(d_vm_la, 'results/01_data/d_vm_la.RDS')
saveRDS(d_vm_la2, 'results/01_data/d_vm_la2.RDS')
