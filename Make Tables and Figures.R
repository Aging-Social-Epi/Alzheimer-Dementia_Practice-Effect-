#****************
#* Make Table 1
#****************

library(tableone)
library(here)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(ggsci)
library(showtext)
library(ggplot2)
library(ggpubr)
font_add_google("Times New Roman")
windowsFonts(A = windowsFont("Times New Roman"))



#read outputs from Andrew
readRDS(here("results/02_output/Table_1.RDS"))


##figure 2 - read pe estimating results from Kaiser
fig2.df<-readRDS(here("results/02_output/executive_function/PE_estimating_models.RDS"))

##khanlde
fig2.df_k<- fig2.df %>% filter (cohort=="KHANDLE",
                                 sample_type=="full_b",
                                 model=="m2")
fig2.df_k<- fig2.df_k[2:10,] %>%
  select(cohort, term, estimate, conf.low, conf.high)
##star
fig2.df_s<- fig2.df %>% filter (cohort=="STAR",
                                 sample_type=="full_b",
                                 model=="m2")
fig2.df_s<- fig2.df_s[2:7,] %>%
  select(cohort, term, estimate, conf.low, conf.high)

#la90
fig2.df_la<- fig2.df %>% filter (cohort=="LA90",
                            sample_type=="full_b",
                            model=="m3")
##select columns 2-13
fig2.df_la<- fig2.df_la[2:13,] %>%
             select(cohort, term, estimate, conf.low, conf.high)


###rbind the dfs
fig2.df.all<-rbind(fig2.df_k,fig2.df_s,fig2.df_la)


##remove the intercepts and reorder the variables
all <- fig2.df.all %>%
  mutate(
    var = fct_relevel(
      term,
      "age_c",
      "genderFemale",
      "raceBlack",
      "raceLatinX",
      "raceAsian",
      "eduSome college but no degree",
      "eduAssociate's degree",
      "eduBachelor's degree",
      "waveW2",
      "waveW3",
      "waveW2:genderFemale",
      "waveW3:genderFemale"
    )
  )

all <- all %>% group_by(var) %>% mutate(num_rows = n()) %>%
                                 rename(Cohort=cohort)


fig2 <-  ggplot(all,aes(
  x = estimate,
  y = var,
  xmin = conf.low,
  xmax = conf.high,
  colour = Cohort,
  group = Cohort
))  +
  geom_point(position = position_dodge(width = 0.5), size = 1)+
  geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
                width = 0.1*all$num_rows)+
  xlab("Differences in Executive Function in SD units") +
  ylab("") +
  geom_vline(xintercept = 0,
             color = "grey",
             linetype = "dashed") +
  scale_y_discrete(
    labels = rev(
      c(
        "Current Age",
        "Female vs. Male",
        "Black vs. White",
        "Latinx vs. White",
        "Asian vs. White",
        "Some college but no degree \n (vs. High School degree)",
        "Associate's degree \n (vs. High School degree)",
        "Bachelor's degree \n (vs. High School degree)",
        "Wave 2 vs. Wave 1",
        "Wave 3 vs. Wave 1",
        "Wave 2*Female",
        "Wave 3*Female"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(text = element_text(family = "A", size = 12,color = "black"),
        legend.text = element_markdown(),
        axis.text = element_markdown())+scale_color_jama()

##save figure 2
ggsave(here("results/03_figures/figure2.jpg"),fig2, width = 8, height = 4, scale = 1)


##figure 3- read predicted results
fig3.df<-readRDS(here("results/02_output/executive_function/fig3_predictions.RDS"))

###khandle
fig3.df_k<-fig3.df %>% filter(cohort=="KHANDLE")
pk <- ggplot(fig3.df_k, aes(x, predicted,
                      group = model,
                      color = model,
                      alpha = model
)) +
  geom_line(size = fig3.df_k$size_line) +
  scale_alpha_manual(name = "model",
                     values = c(.5, .5, 1, .5, .5),
                     guide = F) +
  scale_color_jama(
    labels = c("No PE specification",
               "PE Indicator",
               "**Constrained PE**",
               "Constrained PE (lower bound or 25% CI for LA90)",
               "Constrained PE (upper bound or 75% CI for LA90)")) +
  xlab("Time since baseline (year)") +
  ylab("Executive Function (z-score)")+
  ggtitle(paste("KHANDLE")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,0.4)







fig3.df_s<-fig3.df %>% filter(cohort=="STAR")

ps <- ggplot(fig3.df_s, aes(x, predicted,
                      group = model,
                      color = model,
                      alpha = model)) +
  geom_line(size = fig3.df_s$size_line) +
  scale_alpha_manual(name = "model",
                     values = c(.5, .5, 1, .5, .5),
                     guide = F) +
  scale_color_jama(
    labels = c("No PE Specification",
               "PE Indicator",
               "**Constrained PE**",
               "Constrained PE (lower bound or 25% CI for LA90)",
               "Constrained PE (upper bound or 75% CI for LA90)")) +
  xlab("Time since baseline (year)") +
  ylab("Executive Function (z-score)")+
  ggtitle(paste("STAR")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,0.4)


###la90
fig3.df_la90<-fig3.df %>% filter(cohort=="LA90")
pla <- ggplot(fig3.df_la90, aes(x, predicted,
                       group = model,
                       color = model,
                       alpha = model)) +
  geom_line(size = fig3.df_la90$size_line) +
  scale_alpha_manual(name = "model",
                     values = c(.5, .5, 1, .5, .5),
                     guide = F) +
  scale_color_jama(
    labels = c("No PE Specification",
               "PE Indicator",
               "**Constrained PE**",
               "Constrained PE (lower bound or 25% CI for LA90)",
               "Constrained PE (upper bound or 75% CI for LA90)")) +
  xlab("Time since baseline (year)") +
  ylab("Executive Function (z-score)")+
  ggtitle(paste("LA90")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,0.4)



###la90_2 (PE estimated by stratification, fitted in total data)
fig3.df_la90_2<-fig3.df %>% filter(cohort=="LA90_2")
pla_2 <- ggplot(fig3.df_la90_2, aes(x, predicted,
                                group = model,
                                color = model,
                                alpha = model)) +
  geom_line(size = fig3.df_la90_2$size_line) +
  scale_alpha_manual(name = "model",
                     values = c(.5, .5, 1, .5, .5),
                     guide = F) +
  scale_color_jama(
    labels = c("No PE Specification",
               "PE Indicator",
               "**Constrained PE**",
               "Constrained PE (lower bound or 25% CI for LA90)",
               "Constrained PE (upper bound or 75% CI for LA90)")) +
  xlab("Time since baseline (year)") +
  ylab("Executive Function (z-score)")+
  ggtitle(paste("LA90")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,0.4)



###la90_2 (PE estimated by stratification, fitted in M)
fig3.df_la90_MALE<-fig3.df %>% filter(cohort=="LA90_MALE")
pla_MALE <- ggplot(fig3.df_la90_MALE, aes(x, predicted,
                                    group = model,
                                    color = model,
                                    alpha = model)) +
  geom_line(size = fig3.df_la90_MALE$size_line) +
  scale_alpha_manual(name = "model",
                     values = c(.5, .5, 1, .5, .5),
                     guide = F) +
  scale_color_jama(
    labels = c("No PE Specification",
               "PE Indicator",
               "**Constrained PE**",
               "Constrained PE (lower bound or 25% CI for LA90)",
               "Constrained PE (upper bound or 75% CI for LA90)")) +
  xlab("Time since baseline (year)") +
  ylab("Executive Function (z-score)")+
  ggtitle(paste("LA90")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,0.4)




###la90_2 (PE estimated by stratification, fitted in F)
fig3.df_la90_FEMALE<-fig3.df %>% filter(cohort=="LA90_FEMALE")
pla_FEMALE <- ggplot(fig3.df_la90_MALE, aes(x, predicted,
                                          group = model,
                                          color = model,
                                          alpha = model)) +
  geom_line(size = fig3.df_la90_FEMALE$size_line) +
  scale_alpha_manual(name = "model",
                     values = c(.5, .5, 1, .5, .5),
                     guide = F) +
  scale_color_jama(
    labels = c("No PE Specification",
               "PE Indicator",
               "**Constrained PE**",
               "Constrained PE (lower bound or 25% CI for LA90)",
               "Constrained PE (upper bound or 75% CI for LA90)")) +
  xlab("Time since baseline (year)") +
  ylab("Executive Function (z-score)")+
  ggtitle(paste("LA90")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,0.4)





fig3<-ggarrange(pk,ps,pla,nrow = 1,common.legend = TRUE,legend='bottom')


ggsave(here("results/03_figures/figure3.jpg"),fig3, width = 10, height = 4, scale = 1)

###figure 4
fig4.df<-readRDS(here("results/02_output/executive_function/qr_results.RDS"))
fig4.df_k<-fig4.df %>% filter(cohort=="KHANDLE",term=="waveW2") %>%
                       mutate (lower.bd=Value-1.98*Std..Error,
                               upper.bd=Value+1.98*Std..Error)
pk <- ggplot(fig4.df_k, aes(x = tau, y = Value,
                          ymin = lower.bd,
                          ymax = upper.bd,
                          group = term,
                          color = term)) +
  geom_ribbon(aes(fill = term), alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  xlab("Percentiles of Executive Function Scores") +
  ylab("Estimated Practice Effects") +
  theme_minimal() +
  theme(axis.line = element_line(linetype = "solid")) +
  ggtitle("KHANDLE")+  theme(legend.position = 'none',
                             text=element_text(family="A"))+ scale_fill_nejm()


fig4.df_s<-fig4.df %>% filter(cohort=="STAR",term=="waveW2") %>%
  mutate (lower.bd=Value-1.98*Std..Error,
          upper.bd=Value+1.98*Std..Error)
ps <- ggplot(fig4.df_s, aes(x = tau, y = Value,
                            ymin = lower.bd,
                            ymax = upper.bd,
                            group = term,
                            color = term)) +
  geom_ribbon(aes(fill = term), alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  xlab("Percentiles of Executive Function Scores") +
  ylab("Estimated Practice Effects") +
  theme_minimal() +
  theme(axis.line = element_line(linetype = "solid")) +
  ggtitle("STAR")+  theme(legend.position = 'none',
                          text=element_text(family="A"))+ scale_fill_nejm()

fig4.df_la90<-fig4.df %>% filter(cohort %in% c("LA90_F","LA90_M"),term %in% c("waveW2","waveW3")) %>%
  mutate (lower.bd=Value-1.98*Std..Error,
          upper.bd=Value+1.98*Std..Error,
          sex=ifelse(cohort=="LA90_F", "Female", "Male"))

fig4.df_la90$term <- ifelse(fig4.df_la90$term == "waveW2", "Wave 2 vs. Wave 1",
                         "Wave 3 vs. Wave 1")

pla <- ggplot(
  fig4.df_la90,
  aes(
    x = tau,
    y = Value,
    ymin = lower.bd,
    ymax = upper.bd,
    group = term,
    color = term
  )) +
  geom_ribbon(aes(fill = term), alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  xlab("Percentiles of Executive Function Scores") +
  ylab("Estimated Practice Effects") +
  labs(color = "term", fill = "term") +
  theme_minimal() +
  theme(axis.line = element_line(linetype = "solid")) +
  ggtitle("LA90") +
  facet_wrap(~sex)+ theme(legend.title = element_blank(),
                              legend.position = 'bottom',
                             text=element_text(family="A"))+ scale_fill_nejm()+ scale_color_nejm()

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



pla_legend <- get_legend(
  pla +
    theme(legend.direction = "horizontal",
          legend.justification="center",
          legend.box.just = "bottom")
)


fig4 <- ggarrange(pk, ps, pla,
                  ncol = 3,
                  legend.grob = pla_legend,
                  legend="bottom")
ggsave(here("results/03_figures/figure4.jpg"),fig4, width = 10, height = 4, scale = 1)

####figure 5
###red the hand grip strength findings

fig5.df<-readRDS(here("results/02_output/executive_function/applying_PE_models.RDS"))
fig5.df.k<-fig5.df %>% filter(cohort=="KHANDLE", sample_type=="grip",
                              !model %in% c("m_force_high","m_force_low", "m1","m2","m3"),
                               term  %in% c("years_since_base","base_age_c","first_assess1",
                                            "phone_ind21","gripstrength_w1",
                                           "years_since_base:gripstrength_w1" )) %>%
                       select(term, model, estimate, conf.low, conf.high)

fig5.df.k<-fig5.df.k %>% group_by(term) %>%
  mutate(
    term = fct_relevel(
      term,
      c("years_since_base",
        "base_age_c",
        "phone_ind21",
        "first_assess1",
        "gripstrength_w1",
        "years_since_base:gripstrength_w1")))

fig5.df.k  <- fig5.df.k  %>% group_by(var) %>% mutate(num_rows = n()) %>%
              rename(Model=model)

fig3_k <-  ggplot(fig5.df.k,aes(
  x = estimate,
  y = var,
  xmin = conf.low,
  xmax = conf.high,
  colour = Model,
  group = Model
))  +
  geom_point(position = position_dodge(width = 0.5), size = 1)+
  geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
                width = 0.1*fig5.df.k$num_rows)+
  xlab("Per 1-SD change in Executive Function") +
  ylab("") +
  geom_vline(xintercept = 0,
             color = "grey",
             linetype = "dashed") +
  scale_y_discrete(
    labels = rev(
      c("Time since baseline (year)",
        "Baseline age (year)",
        "Phone vs. in-person",
        "First Assessment vs. Others",
        "Grip Strength",
        "Time*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 14),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
                     labels = c("No Specifications for PE",
                                "Adjusted for PE",
                                "Constrained PE"))+ xlim(-1, 3)



####star
fig5.df.s<-fig5.df %>% filter(cohort=="STAR", sample_type=="grip",
                              !model %in% c("m_force_high","m_force_low", "m1","m2","m3"),
                              term  %in% c("years_since_base","base_age_c","first_assess1",
                                           "phone_ind21","gripstrength_w1",
                                           "years_since_base:gripstrength_w1" )) %>%
  select(term, model, estimate, conf.low, conf.high)

fig5.df.s<-fig5.df.s %>% group_by(term) %>%
  mutate(
    var = fct_relevel(
      term,
      c("years_since_base",
        "base_age_c",
        "phone_ind21",
        "first_assess1",
        "gripstrength_w1",
        "years_since_base:gripstrength_w1")))

fig5.df.s  <- fig5.df.s  %>% group_by(var) %>% mutate(num_rows = n()) %>%
  rename(Model=model)

fig3_k <-  ggplot(fig5.df.s,aes(
  x = estimate,
  y = var,
  xmin = conf.low,
  xmax = conf.high,
  colour = Model,
  group = Model
))  +
  geom_point(position = position_dodge(width = 0.5), size = 1)+
  geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
                width = 0.1*fig5.df.s$num_rows)+
  xlab("Per 1-SD change in Executive Function") +
  ylab("") +
  geom_vline(xintercept = 0,
             color = "grey",
             linetype = "dashed") +
  scale_y_discrete(
    labels = rev(
      c("Time since baseline (year)",
        "Baseline age (year)",
        "Phone vs. in-person",
        "First Assessment vs. Others",
        "Grip Strength",
        "Time*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 14),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
    labels = c("No Specifications for PE",
               "Adjusted for PE",
               "Constrained PE"))+ xlim(-0.2, 2)


