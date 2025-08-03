#****************
#* Make Table 1
#****************
# load packages
library(pacman)
p_load("tableone", "here", "tidyverse")

# ---- main table 1 ----
k <- readRDS("results/01_data/d_e_k.RDS") %>%
  drop_na(score_z, age, gender, race, edu) %>%
  mutate(cohort = "KHANDLE") %>%
  group_by(STUDYID) %>%
  mutate(num_assess = n())

s <- readRDS("results/01_data/d_e_s.RDS") %>%
  drop_na(score, age, gender, race, edu) %>%
  mutate(cohort = "STAR") %>%
  group_by(STUDYID) %>%
  mutate(num_assess = n())

la <- readRDS("results/01_data/d_e_la.RDS") %>%
  rename(score=score_z) %>%
  drop_na(score, age, gender, race, edu) %>%
  mutate(cohort = "LA90") %>%
  group_by(STUDYID) %>%
  mutate(num_assess = n())

k_unique <- k %>%
  group_by(STUDYID) %>%
  filter(age == min(age)) %>%
  select(STUDYID, age, gender, race, edu, cohort) %>%
  distinct()
nrow(k_unique)

s_unique <- s %>%
  drop_na(age, gender, race, edu) %>%
  group_by(STUDYID) %>%
  filter(age == min(age, na.rm=T)) %>%
  select(STUDYID, age, gender, race, edu, cohort) %>%
  distinct()
nrow(s_unique)

la_unique <- la %>%
  group_by(STUDYID) %>%
  filter(age == min(age)) %>%
  select(STUDYID, age, gender, race, edu, cohort) %>%
  distinct()
nrow(la_unique)



d_base <- rbind(k_unique, s_unique,la_unique)

vars <- c("age", "gender", "race", "edu")
vars_cat <- c("gender", "race", "edu")

tab1 <- CreateTableOne(vars = vars,
                       data = d_base,
                       factorVars = vars_cat,
                       strata = "cohort",
                       includeNA = F,
                       addOverall = F,
                       test = F)

tab1_df <- as.data.frame(print(tab1))

# ---- extra columns ----
# ---- **khandle ------
# ---- ****pre-pandemic ----
k_pre <- k %>%
  filter(phone_ind2 != 1 & wave != "W3") %>%
  group_by(STUDYID) %>%
  mutate(num_assess = n()) %>%
  ungroup()

# number of people with at least one wave of FU
one_fu <- k_pre %>%
  select(STUDYID, num_assess) %>%
  distinct()

table(one_fu$num_assess) #1333

# mean duration of follow-up
### Kaitlin note: this is incorrect with the truncated ages, because for those
# with the age set to 89.99 for multiple time periods, this filter keeps more
# than just their first time point. It should should run correctly with actual
# ages. I added a two lines that will be sure filter to just one observation
# per participant.
# You will know that you have the right answer if in Table 1, in the
# "pre-pandemic" section, the mean duration of follow-up and mean interval
# between waves 1 and 2 are identical. (This has to be true since the sample
# is restricted to waves 1 and 2 only).
k_avg_fu <- k_pre %>%
  filter(num_assess == 2) %>%
  group_by(STUDYID) %>%
  filter(years_since_base == max(years_since_base)) %>%
  ### Kaitlin: line I added
  slice_tail() %>%
  ungroup() %>%
  summarise(avg = mean(years_since_base, na.rm = T) %>% round(2),
            sd = sd(years_since_base, na.rm = T) %>% round(2))

#mean interval between waves
k_pre <- k_pre %>%
  group_by(STUDYID) %>%
  mutate(diff = years_since_base -
           lag(years_since_base,
               default = first(years_since_base),
               order_by=wave))

kw1w2 <- k_pre %>%
  group_by(wave) %>%
  summarize(avg = mean(diff, na.rm = T) %>% round(2),
            sd = sd(diff,na.rm=T) %>% round(2))

tab1_df['Longitudinal information (pre-pandemic)', ] <- ""

tab1_df['Individual who had at least 1 wave of follow-up',
        'KHANDLE'] <- table(one_fu$num_assess)[2]

tab1_df['Duration of follow-up (year), mean (SD)',
        'KHANDLE'] <- paste0(k_avg_fu$avg, " (", k_avg_fu$sd, ")")

tab1_df['Average interval between wave 1 and wave 2',
        'KHANDLE'] <- paste0(kw1w2$avg[2], " (", kw1w2$sd[2], ")")
tab1_df['Average interval between wave 2 and wave 3',
        'KHANDLE']<-""


#---- ****full study period -----
# number of people with at least one wave of FU
k_one_fu_whole <- k %>%
  select(STUDYID, num_assess) %>%
  distinct()

table(k_one_fu_whole$num_assess)[2] + table(k_one_fu_whole$num_assess)[3]

# mean duration
k_avg_fu_whole <- k %>%
  filter(num_assess != 1) %>%
  group_by(STUDYID) %>%
  filter(years_since_base == max(years_since_base)) %>%
  ### Kaitlin: line I added
  slice_tail() %>%
  ungroup() %>%
  summarise(avg = mean(years_since_base, na.rm = T)%>% round(2),
            sd = sd(years_since_base, na.rm = T) %>% round(2))

# mean number of assessments
k %>%
  group_by(STUDYID) %>%
  tally() %>%
  ### Kaitlin: this didn't work for me
  # summarise(mean(num_assess))
  # but this did
  summarise(mean(n))

#mean interval between waves
k2 <- k %>%
  filter(num_assess != 1) %>%
  group_by(STUDYID) %>%
  mutate(diff = years_since_base -
           lag(years_since_base,
               default = first(years_since_base),
               order_by=wave))

kw1w2w3 <- k2 %>%
  group_by(wave) %>%
  summarize(avg = mean(diff, na.rm = T) %>% round(2),
            sd = sd(diff,na.rm=T) %>% round(2))


tab1_df['Longitudinal information (Whole study)', ] <- ""

tab1_df['Individual who had at least 1 wave of follow-up (whole)',
        'KHANDLE'] <- table(k_one_fu_whole$num_assess)[2] + table(k_one_fu_whole$num_assess)[3]

tab1_df['Duration of follow-up (year), mean (SD) (whole)',
        'KHANDLE'] <- paste0(k_avg_fu_whole$avg, " (", k_avg_fu_whole$sd, ")")

tab1_df['Average interval between wave 1 and wave 2 (whole)',
        'KHANDLE'] <- paste0(kw1w2w3$avg[2], " (", kw1w2w3$sd[2], ")")

tab1_df['Average interval between wave 2 and wave 3 (whole)',
        'KHANDLE'] <- paste0(kw1w2w3$avg[3], " (", kw1w2w3$sd[3], ")")

tab1_df['Average interval between wave 3 and wave 4 (whole)',
        'KHANDLE'] <- ""

tab1_df['Average interval between wave 4 and wave 5 (whole)',
        'KHANDLE'] <-""

tab1_df['Average interval between wave 5 and wave 6 (whole)',
        'KHANDLE'] <- ""


#---- ** star ----
s_pre <- s %>%
  filter(phone_ind2 != 1 & wave != "W3") %>%
  group_by(STUDYID) %>%
  mutate(num_assess = n()) %>%
  ungroup()

# number of people with at least one wave of FU
one_fu <- s_pre %>%
  select(STUDYID, num_assess) %>%
  distinct()

table(one_fu$num_assess)

# mean duration of follow-up
s_fu <- s_pre %>%
  filter(num_assess == 2) %>%
  group_by(STUDYID) %>%
  filter(years_since_base == max(years_since_base)) %>%
  ### Kaitlin: line I added to get "right" answer with truncated data
  slice_tail() %>%
  ungroup() %>%
  summarise(avg = mean(years_since_base, na.rm = T) %>% round(2),
            sd = sd(years_since_base, na.rm = T) %>% round(2))

# mean wave 1 vs wave 2
s_pre %>%
  filter(wave == "W2") %>%
  select(years_since_base) %>%
  summarise(mean(years_since_base, na.rm = T),
            sd(years_since_base, na.rm = T))

#mean interval between waves
s_pre <- s_pre %>%
  group_by(STUDYID) %>%
  mutate(diff = years_since_base -
           lag(years_since_base,
               default = first(years_since_base),
               order_by=wave))

sw1w2 <- s_pre %>%
  group_by(wave) %>%
  summarize(avg = mean(diff, na.rm = T) %>% round(2),
            sd = sd(diff,na.rm=T) %>% round(2))

tab1_df['Individual who had at least 1 wave of follow-up',
        'STAR'] <- table(one_fu$num_assess)[2]

tab1_df['Duration of follow-up (year), mean (SD)',
        'STAR'] <- paste0(s_fu$avg, " (", s_fu$sd, ")")

tab1_df['Average interval between wave 1 and wave 2',
        'STAR'] <- paste0(sw1w2$avg[2], " (", sw1w2$sd[2], ")")
tab1_df['Average interval between wave 2 and wave 3',
        'STAR']<-""


#---- **full study period -----

# number of people with at least one wave of FU
one_fu <- s %>%
  select(STUDYID, num_assess) %>%
  distinct()

table(one_fu$num_assess)

# mean duration
s_avg_fu_whole <- s %>%
  filter(num_assess != 1) %>%
  group_by(STUDYID) %>%
  filter(years_since_base == max(years_since_base)) %>%
  ### Kaitlin: line I added to get "right" answer with truncated data
  slice_tail() %>%
  ungroup() %>%
  summarise(avg = mean(years_since_base, na.rm = T) %>% round(2),
            sd = sd(years_since_base, na.rm = T) %>% round(2))

#mean interval between waves
s2 <- s %>%
  filter(num_assess != 1) %>%
  group_by(STUDYID) %>%
  mutate(diff = years_since_base -
           lag(years_since_base,
               default = first(years_since_base),
               order_by=wave))

sw1w2w3 <- s2 %>%
  group_by(wave) %>%
  summarize(avg=mean(diff, na.rm = T)%>% round(2),
            sd = sd(diff,na.rm=T) %>% round(2))

tab1_df['Individual who had at least 1 wave of follow-up (whole)',
        'STAR'] <- table(one_fu$num_assess)[2] + table(one_fu$num_assess)[3]




tab1_df['Duration of follow-up (year), mean (SD) (whole)',
        'STAR'] <- paste0(s_avg_fu_whole$avg, " (", s_avg_fu_whole$sd, ")")

tab1_df['Average interval between wave 1 and wave 2 (whole)',
        'STAR'] <- paste0(sw1w2w3$avg[2], " (", sw1w2w3$sd[2], ")")

tab1_df['Average interval between wave 2 and wave 3 (whole)',
        ### Kaitlin: used the wrong index here:
        # 'STAR'] <- paste0(sw1w2w3$avg[2], " (", sw1w2w3$sd[2], ")")
        # fixed here
        'STAR'] <- paste0(sw1w2w3$avg[3], " (", sw1w2w3$sd[3], ")")




tab1_df['Average interval between wave 3 and wave 4 (whole)',
        'STAR'] <- ""

tab1_df['Average interval between wave 4 and wave 5 (whole)',
        'STAR'] <-""

tab1_df['Average interval between wave 5 and wave 6 (whole)',
        'STAR'] <- ""







#---- ** LA90 ----
la_pre <- la %>%
  filter(phone_ind2 != 1 & wave != "W4") %>%
  group_by(STUDYID) %>%
  mutate(num_assess = n()) %>%
  ungroup()

# number of people with at least one wave of FU
la_one_fu <- la_pre %>%
  select(STUDYID, num_assess) %>%
  distinct()

table(la_one_fu$num_assess)

# mean duration of follow-up
la_fu<-la_pre %>%
  filter(num_assess == 2|num_assess == 3) %>%
  group_by(STUDYID) %>%
  filter(years_since_base == max(years_since_base)) %>%
  ### line I added to get "right" answer with truncated data
  slice_tail() %>%
  ungroup() %>%
  summarise(avg=mean(years_since_base, na.rm = T)%>% round(2),
            sd=sd(years_since_base, na.rm = T)%>% round(2))

#mean interval between waves
la_pre <- la_pre %>%
  group_by(STUDYID) %>%
  mutate(diff = years_since_base -
           lag(years_since_base,
               default = first(years_since_base),
               order_by=wave))

law1w2<-la_pre %>%
  group_by(wave) %>%
  summarize(avg = mean(diff, na.rm = T) %>% round(2),
            sd = sd(diff,na.rm=T) %>% round(2))

tab1_df['Individual who had at least 1 wave of follow-up',
        'LA90'] <- table(la_one_fu$num_assess)[2]+table(la_one_fu$num_assess)[3]

tab1_df['Duration of follow-up (year), mean (SD)',
        'LA90'] <- paste0(la_fu$avg, " (", la_fu$sd, ")")

tab1_df['Average interval between wave 1 and wave 2',
        'LA90'] <- paste0(law1w2$avg[2], " (", law1w2$sd[2], ")")


tab1_df['Average interval between wave 2 and wave 3',
        'LA90'] <- paste0(law1w2$avg[3], " (", law1w2$sd[3], ")")


#---- **full study period -----

# number of people with at least one wave of FU
la_one_fu_whole <- la %>%
  select(STUDYID, num_assess) %>%
  distinct()

table(la_one_fu_whole$num_assess)

# mean duration
la_avg_fu_whole<-la  %>%
  filter(num_assess != 1) %>%
  group_by(STUDYID) %>%
  filter(years_since_base == max(years_since_base)) %>%
  ### line I added to get "right" answer with truncated data
  slice_tail() %>%
  ungroup() %>%
  summarise(avg = mean(years_since_base, na.rm = T)%>% round(2),
            sd = sd(years_since_base, na.rm = T)%>% round(2))

# mean number of assessments
la %>%
  group_by(STUDYID) %>%
  ### this didn't work for me
  # summarise(mean(num_assess))
  # but this did
  summarise(mean(n))

#mean interval between waves
la2 <- la %>%
  filter(num_assess != 1) %>%
  group_by(STUDYID) %>%
  mutate(diff = years_since_base -
           lag(years_since_base,
               default = first(years_since_base),
               order_by=wave))

law1w2w3<-la2 %>%
  group_by(wave) %>%
  summarize(avg=mean(diff, na.rm = T)%>% round(2),
            sd = sd(diff,na.rm=T) %>% round(2))

tab1_df['Duration of follow-up (year), mean (SD) (whole)',
        'LA90'] <- paste0(la_avg_fu_whole$avg, " (", la_avg_fu_whole$sd, ")")


tab1_df['Individual who had at least 1 wave of follow-up (whole)',
        'LA90'] <- table(la_one_fu_whole $num_assess)[2] + table(la_one_fu_whole $num_assess)[3]+ table(la_one_fu_whole $num_assess)[4]+
                   table(la_one_fu_whole $num_assess)[5]+ table(la_one_fu_whole $num_assess)[6]


tab1_df['Average interval between wave 1 and wave 2 (whole)',
        'LA90'] <- paste0(law1w2w3$avg[2], " (", law1w2w3$sd[2], ")")

tab1_df['Average interval between wave 2 and wave 3 (whole)',
        'LA90'] <- paste0(law1w2w3$avg[3], " (", law1w2w3$sd[3], ")")

tab1_df['Average interval between wave 3 and wave 4 (whole)',
        'LA90'] <- paste0(law1w2w3$avg[4], " (", law1w2w3$sd[4], ")")

tab1_df['Average interval between wave 4 and wave 5 (whole)',
        'LA90'] <- paste0(law1w2w3$avg[5], " (", law1w2w3$sd[5], ")")

tab1_df['Average interval between wave 5 and wave 6 (whole)',
        'LA90'] <- paste0(law1w2w3$avg[6], " (", law1w2w3$sd[6], ")")

### Kaitlin: adding this line so it outputs the columns in the same order as
### they appear in the manuscript version
tab1_df <- tab1_df %>% select(KHANDLE, STAR, LA90)


Table1<-saveRDS(tab1_df, "results/02_output/Table_1.RDS")

