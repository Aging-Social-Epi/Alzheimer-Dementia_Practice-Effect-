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

weird <- scales::trans_new("uneven",
                           transform=function(x) sign(x)*abs(x)*exp(-0.5*sign(x)),
                           inverse=function(x) sign(x)*abs(x)*exp(0.5*sign(x)))


#read outputs from Andrew
readRDS(here("results/02_output/Table_1.RDS"))



####################################################################appendix figure 1###############################################################
fig1.df<-readRDS(here("results/02_output/executive_function/PE_estimating_models.RDS"))

##khanlde
fig1.df_k<- fig1.df %>% filter (cohort=="KHANDLE",
                                sample_type=="full",
                                model=="m2")
fig1.df_k<- fig1.df_k[2:10,] %>%
  select(cohort, term, estimate, conf.low, conf.high)
##star
fig1.df_s<- fig1.df %>% filter (cohort=="STAR",
                                sample_type=="full",
                                model=="m2")
fig1.df_s<- fig1.df_s[2:7,] %>%
  select(cohort, term, estimate, conf.low, conf.high)

#la90
fig1.df_la<- fig1.df %>% filter (cohort=="LA90",
                                 sample_type=="full",
                                 model=="m3")
##select columns 2-13
fig1.df_la<- fig1.df_la[2:13,] %>%
  select(cohort, term, estimate, conf.low, conf.high)


###rbind the dfs
fig1.df.all<-rbind(fig1.df_k,fig1.df_s,fig1.df_la)


##remove the intercepts and reorder the variables
all <- fig1.df.all %>%
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
      "waveW3"
    )
  )

all <- all %>% group_by(var) %>% mutate(num_rows = n()) %>%
  rename(Cohort=cohort)


app_fig1<-  ggplot(all,aes(
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
        "waveW2:genderFemale",
        "waveW3:genderFemale"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(text = element_text(family = "A", size = 12,color = "black"),
        legend.text = element_markdown(),
        axis.text = element_markdown())+scale_color_jama()

##save appendix figure 1
ggsave(here("results/03_figures/appendix_figure1.jpg"),app_fig1 , width = 8, height = 4, scale = 1)


####################################################################appendix figure 2###############################################################

fig2.df<-readRDS(here("results/02_output/executive_function/applying_PE_models.RDS"))
fig2.df.k<-fig2.df %>% filter(cohort=="KHANDLE", sample_type=="grip_c",timescale=='age',
                              term  %in% c("gripstrength_w1",
                                           "age_c:gripstrength_w1" )) %>%
  select(term, model, estimate, conf.low, conf.high)


fig2.df.k$term<- factor(fig2.df.k$term, levels=c(
                                                 "gripstrength_w1",
                                                 "age_c:gripstrength_w1"))


fig2.df.k  <- fig2.df.k  %>% group_by(term) %>% mutate(num_rows = n()) %>%
  rename(Model=model)

app_fig2_k <-  ggplot(fig2.df.k,aes(
  x = estimate,
  y = term,
  xmin = conf.low,
  xmax = conf.high,
  colour = Model,
  group = Model
))  +
  geom_point(position = position_dodge(width = 0.5), size = 1)+
  geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
                width = 0.1*fig2.df.k$num_rows)+
  xlab("Per 1-SD change in Executive Function") +
  ylab("") +
  geom_vline(xintercept = 0,
             color = "grey",
             linetype = "dashed") +
  scale_y_discrete(
    labels = rev(
      c(
        "Grip Strength",
        "Current Age*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 12),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
    labels = c("No Specifications for PE",
               "Adjusted for PE",
               "Constrained PE"))+ xlim(-1, 2)



####star
fig2.df.s<-fig2.df %>% filter(cohort=="STAR", sample_type=="grip_c",timescale=='age',
                              term  %in% c("gripstrength_w1",
                                           "age_c:gripstrength_w1" )) %>%
  select(term, model, estimate, conf.low, conf.high)


fig2.df.s$term<-factor(fig2.df.s$term, levels=c(
                                                "gripstrength_w1",
                                                "age_c:gripstrength_w1"))

fig2.df.s  <- fig2.df.s  %>% group_by(term) %>% mutate(num_rows = n()) %>%
  rename(Model=model)

app_fig2_s <-  ggplot(fig2.df.s,aes(
  x = estimate,
  y = term,
  xmin = conf.low,
  xmax = conf.high,
  colour = Model,
  group = Model
))  +
  geom_point(position = position_dodge(width = 0.5), size = 1)+
  geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
                width = 0.1*fig2.df.s$num_rows)+
  xlab("Per 1-SD change in Executive Function") +
  ylab("") +
  geom_vline(xintercept = 0,
             color = "grey",
             linetype = "dashed") +
  scale_y_discrete(
    labels = rev(
      c(
        "Grip Strength",
        "Current Age*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 12),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
    labels = c("No Specifications for PE",
               "Adjusted for PE",
               "Constrained PE"))+ xlim(-1, 2)

##LA90
fig2.df.la90<-fig2.df %>% filter(cohort=="LA90", sample_type=="grip_c",timescale=='age',
                                 term  %in% c("gripstrength_w1",
                                              "age_c:gripstrength_w1" )) %>%
  select(term, model, estimate, conf.low, conf.high)

fig2.df.la90$term<-factor(fig2.df.la90$term, levels=c(
                                                      "gripstrength_w1",
                                                      "age_c:gripstrength_w1"))

fig2.df.la90  <- fig2.df.la90  %>% group_by(term) %>% mutate(num_rows = n()) %>%
  rename(Model=model)

app_fig2_la <-  ggplot(fig2.df.la90,aes(
  x = estimate,
  y = term,
  xmin = conf.low,
  xmax = conf.high,
  colour = Model,
  group = Model
))  +
  geom_point(position = position_dodge(width = 0.5), size = 1)+
  geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
                width = 0.1*fig2.df.s$num_rows)+
  xlab("Per 1-SD change in Executive Function") +
  ylab("") +
  geom_vline(xintercept = 0,
             color = "grey",
             linetype = "dashed") +
  scale_y_discrete(
    labels = rev(
      c(
        "Grip Strength",
        "Current Age*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 12),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
    labels = c("No Specifications for PE",
               "Adjusted for PE",
               "Constrained PE"))+ xlim(-1, 3)

App_fig2<-ggarrange(app_fig2_k, app_fig2_s, app_fig2_la,labels = c("KHANDLE", "STAR", "LA90"), nrow = 1,font.label=list(color="black",size=12, family="A"), common.legend = TRUE, legend="bottom")



##save figure 2
ggsave(here("results/03_figures/appendix_figure2.jpg"),
       App_fig2, width =14, height = 4, scale = 1)


####################################################################appendix figure 3###############################################################
fig2.df<-readRDS(here("results/02_output/verbal_memory/PE_estimating_models.RDS"))

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



###rbind the dfs
fig2.df.all<-rbind(fig2.df_k,fig2.df_s)


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
      "waveW2"
    )
  )

all <- all %>% group_by(var) %>% mutate(num_rows = n()) %>%
                                 rename(Cohort=cohort)


fig2_vm <-  ggplot(all,aes(
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
        "Wave 2 vs. Wave 1"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(text = element_text(family = "A", size = 12,color = "black"),
        legend.text = element_markdown(),
        axis.text = element_markdown())+scale_color_jama()

##save figure 3
ggsave(here("results/03_figures/appendix_figure3_vm.jpg"),fig2_vm, width = 8, height = 4, scale = 1)


####################################################################appendix figure 4###############################################################
fig3.df<-readRDS(here("results/02_output/verbal_memory/fig3_predictions.RDS"))

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
               "Lower bound",
               "Upper bound")) +
  xlab("Time since baseline (year)") +
  ylab("Verbal Memory (z-score)")+
  ggtitle(paste("KHANDLE")) +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,-0.4)



##star

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
               "Lower bound",
               "Upper bound")) +
  xlab("Time since baseline (year)") +
  ylab("Verbal Memory (z-score)")+
  ggtitle(paste("STAR")) +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        axis.line = element_line(linetype = "solid"),
        legend.text = element_markdown(),
        text=element_text(family="A"))+ylim(-0.8,-0.4)




fig5_vm<-ggarrange(pk,ps,nrow = 1,common.legend = TRUE,legend='bottom')


ggsave(here("results/03_figures/appendix_figure5_vm.jpg"),fig5_vm, width = 10, height = 4, scale = 1)

####################################################################Appendix figure 6###############################################################
fig4.df<-readRDS(here("results/02_output/verbal_memory/qr_results.RDS"))
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
  xlab("Percentiles of Verbal Memory Scores") +
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
  xlab("Percentiles of Verbal Memory Scores") +
  ylab("Estimated Practice Effects") +
  theme_minimal() +
  theme(axis.line = element_line(linetype = "solid")) +
  ggtitle("STAR")+  theme(legend.position = 'none',
                          text=element_text(family="A"))+ scale_fill_nejm()

fig4.df_la90<-fig4.df %>% filter(cohort %in% c("LAFULL"),term %in% c("waveW2","waveW3")) %>%
  mutate (lower.bd=Value-1.98*Std..Error,
          upper.bd=Value+1.98*Std..Error)

fig4.df_la90$term <- ifelse(fig4.df_la90$term == "waveW2", "Wave 2 vs. Wave 1",
                         "Wave 3 vs. Wave 1")

# pla <- ggplot(
#   fig4.df_la90,
#   aes(
#     x = tau,
#     y = Value,
#     ymin = lower.bd,
#     ymax = upper.bd,
#     group = term,
#     color = term
#   )) +
#   geom_ribbon(aes(fill = term), alpha = 0.2, color = NA) +
#   geom_point() +
#   geom_line() +
#   xlab("Percentiles of Verbal Memory Scores") +
#   ylab("Estimated Practice Effects") +
#   labs(color = "term", fill = "term") +
#   theme_minimal() +
#   theme(axis.line = element_line(linetype = "solid")) +
#   ggtitle("LA90") + theme(legend.title = element_blank(),
#                               legend.position = 'bottom',
#                              text=element_text(family="A"))+ scale_fill_nejm()+ scale_color_nejm()

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


app_fig6 <- ggarrange(pk, ps,
                  ncol = 2,
                  legend.grob = pla_legend,
                  legend="bottom")
ggsave(here("results/03_figures/Appendix_figure6_vm.jpg"),app_fig6, width = 10, height = 4, scale = 1)

####################################################################Appendix figure 7###############################################################
###red the hand grip strength findings

fig5.df<-readRDS(here("results/02_output/verbal_memory/applying_PE_models.RDS"))
fig5.df.k<-fig5.df %>% filter(cohort=="KHANDLE", sample_type=="grip",
                              !model %in% c("m_force_high","m_force_low", "m1","m2","m3"),
                               term  %in% c("gripstrength_w1",
                                           "years_since_base:gripstrength_w1" )) %>%
                       select(term, model, estimate, conf.low, conf.high)


fig5.df.k$term<- factor(fig5.df.k$term, levels=c(
                                                   "gripstrength_w1",
                                                   "years_since_base:gripstrength_w1"))


fig5.df.k  <- fig5.df.k  %>% group_by(term) %>% mutate(num_rows = n()) %>%
              rename(Model=model)

fig3_k <-  ggplot(fig5.df.k,aes(
  x = estimate,
  y = term,
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
      c(
        "Grip Strength",
        "Time*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 12),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
                     labels = c("No Specifications for PE",
                                "Adjusted for PE",
                                "Constrained PE"))+ xlim(-0.5, 2)


####star
fig5.df.s<-fig5.df %>% filter(cohort=="STAR", sample_type=="grip",
                              !model %in% c("m_force_high","m_force_low", "m1","m2","m3"),
                              term  %in% c("gripstrength_w1",
                                           "years_since_base:gripstrength_w1" )) %>%
  select(term, model, estimate, conf.low, conf.high)


fig5.df.s$term<-factor(fig5.df.s$term, levels=c(
                                                "gripstrength_w1",
                                                "years_since_base:gripstrength_w1"))

fig5.df.s  <- fig5.df.s  %>% group_by(term) %>% mutate(num_rows = n()) %>%
  rename(Model=model)

fig3_s <-  ggplot(fig5.df.s,aes(
  x = estimate,
  y = term,
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
      c(
        "Grip Strength",
        "Time*Grip Strength"
      )
    ), limits = rev)+
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(size = 12),
        text = element_text(family = "A", size = 12),
        legend.text = element_markdown(),
        axis.text = element_markdown())+
  scale_color_jama(
    labels = c("No Specifications for PE",
               "Adjusted for PE",
               "Constrained PE"))+ xlim(-0.8, 2)
# ##LA90
# fig5.df.la90<-fig5.df %>% filter(cohort=="LA90", sample_type=="grip",
#                               !model %in% c("m_force_high","m_force_low", "m1","m2","m3"),
#                               term  %in% c("gripstrength_w1",
#                                            "years_since_base:gripstrength_w1" )) %>%
#   select(term, model, estimate, conf.low, conf.high)
#
# fig5.df.la90$term<-factor(fig5.df.la90$term, levels=c(
#                                                 "gripstrength_w1",
#                                                 "years_since_base:gripstrength_w1"))
#
# fig5.df.la90  <- fig5.df.la90  %>% group_by(term) %>% mutate(num_rows = n()) %>%
#   rename(Model=model)
#
# fig3_la <-  ggplot(fig5.df.la90,aes(
#   x = estimate,
#   y = term,
#   xmin = conf.low,
#   xmax = conf.high,
#   colour = Model,
#   group = Model
# ))  +
#   geom_point(position = position_dodge(width = 0.5), size = 1)+
#   geom_errorbar(position = position_dodge(width = 0.5), size = 0.5,
#                 width = 0.1*fig5.df.s$num_rows)+
#   xlab("Per 1-SD change in Executive Function") +
#   ylab("") +
#   geom_vline(xintercept = 0,
#              color = "grey",
#              linetype = "dashed") +
#   scale_y_discrete(
#     labels = rev(
#       c(
#         "Grip Strength",
#         "Time*Grip Strength"
#       )
#     ), limits = rev)+
#   theme(panel.background = element_blank())+
#   theme(axis.text.x = element_text(size = 12),
#         text = element_text(family = "A", size = 14),
#         legend.text = element_markdown(),
#         axis.text = element_markdown())+
#   scale_color_jama(
#     labels = c("No Specifications for PE",
#                "Adjusted for PE",
#                "Constrained PE"))+ xlim(-1, 3)

app.fig7<-ggarrange(fig3_k, fig3_s, labels = c("KHANDLE", "STAR"), nrow = 1,font.label=list(color="black",size=12, family="A"), common.legend = TRUE, legend="bottom")

##save figure
ggsave(here("results/03_figures/Appendix_figure7_vm.jpg"),app.fig7, width = 8, height = 5, scale = 1)
