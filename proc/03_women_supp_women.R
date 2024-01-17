#0. Preparation ---

options(scipen=999) #No scientific notation

pacman::p_load(tidyverse, #Data manipulation
               gt, #Tables 
               gtsummary, #Descriptive table
               sjmisc, #Label manipulation
               labelled, #Label manipulation
               skimr, #Data manipulation
               sjPlot, #Correlation
               corrplot, #Correlation
               Hmisc, #Correlation
               lme4, #Multilevel modeling
               reghelper, #Get ICC
               texreg, #View models
               AICcmodavg #Best fit model
               )

#Chile ICILS 2018 data
student_proc_2018 <- readRDS("input/proc_data/03_student_proc_2018.rds")

#Select variables
data <- student_proc_2018 |> 
  select(
    #ID
    idschool,idstud,
    #Student level
    s_sex,s_pv1cil,s_speceff,
    #School level
    c_pv1cil,c_s_f_ratio,
    #Control variables
    s_hisced,s_homlit,
    #Items speceff
    is2g27b,is2g27e,is2g27g,is2g27h,
    #Weights
    starts_with(c("wg","tot"))
    )

#Delete missing data labels
val_label(data$s_sex,8) <- NULL
val_label(data$s_sex,9) <- NULL
val_label(data$is2g27b,8) <- NULL
val_label(data$is2g27b,9) <- NULL
val_label(data$is2g27e,8) <- NULL
val_label(data$is2g27e,9) <- NULL
val_label(data$is2g27g,8) <- NULL
val_label(data$is2g27g,9) <- NULL
val_label(data$is2g27h,8) <- NULL
val_label(data$is2g27h,9) <- NULL

#Change labels speceff items
var_label(data) <- list(
  is2g27b = "How well can you do/Create a database (e.g. using [Microsoft Access ®])",
  is2g27e = "How well can you do/Build or edit a webpage",
  is2g27g = "How well can you do/Create a computer program, macro, or [app]",
  is2g27h = "How well can you do/Set up a local area network of computers or other ICT"
)


#1. Exploring data ----

#Descriptive Specialized Self-efficacy

data |>
  mutate(s_sex=to_label(s_sex),
         is2g27b=to_label(is2g27b),
         is2g27e=to_label(is2g27e),
         is2g27g=to_label(is2g27g),
         is2g27h=to_label(is2g27h)) |>
  tbl_summary(include=c(is2g27b,is2g27e,is2g27g,is2g27h),
              by=s_sex,
              missing="no") |>
  modify_header(label="**Variable**")

#Descriptive index table
data |> 
  mutate(s_sex=to_label(s_sex)) |>
  tbl_summary(include=c(s_sex,s_speceff,s_pv1cil,c_pv1cil,c_s_f_ratio),
              missing = "no") |>
  modify_header(label="**Variable**")

#Differences by group table
data |> 
  mutate(s_sex=to_label(s_sex)) |>
  tbl_summary(include = c(s_pv1cil,s_speceff),
              by = s_sex,
              missing = "no") |>
  add_p() |>
  add_n() |>
  modify_header(label="**Variables**")

#Anova test
aov_1 <- aov(s_speceff~s_sex,data = data) #Significative differences
aov_2 <- aov(s_speceff~s_sex+s_pv1cil,data=data) #Significative differences
aov_3 <- aov(s_speceff~s_sex*s_pv1cil,data=data) #Non-significative differences

summary(aov_1)
summary(aov_2)
summary(aov_3)


aictab(list(aov_1,aov_2,aov_3))

#Checking homocedasticity
par(mfrow=c(2,2))
plot(aov_2)
par(mfrow=c(1,1))

#Correlation
data_corr <- data |> select(-c(idschool,idstud,totwgts,
                               starts_with("is2")))

data_corr <- rcorr(as.matrix(data_corr))

corrplot.mixed(data_corr$r,order = 'AOE',
               pmat = data_corr$P, sig.level = 0.05)

#Other option
tab_corr(data |> select(-c(idschool,idstud,
                           starts_with("is2"))))

data |>
  group_by(to_label(s_sex)) |>
  dplyr::summarize(corr=cor(s_speceff,s_pv1cil,use="complete.obs")|>
                     round(3)) |>
  gt() |>
  tab_header("Reinforcement between ICT competences and specialized self-efficacy",
             subtitle = "Correlation by gender") |>
  cols_label(corr=md("**Pearson score**"))


#2. Models ----

# Apply weights


# Null Model

m0_speceff <- lmer(s_speceff ~ 1 + (1 | idschool), data=data)

ICC(m0_speceff) #0.6 rounded ICC

# Level 1 fixed effects

data <- data |> mutate (s_sex=to_label(s_sex))

m1_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     s_sex*s_pv1cil +
                     (1 | idschool),  
                   data=data,
                   weights = wgtfac3s*wgtadj3s) #student within-school weights

screenreg(m1_speceff)

# Level 2 fixed effects

m2_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     s_sex*s_pv1cil +
                     c_pv1cil+ c_s_f_ratio +
                     (1 | idschool),  
                   data=data)

screenreg(m2_speceff)



# Multilevel random effects
m3_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     s_sex*s_pv1cil +
                     c_pv1cil + c_s_f_ratio +
                     (1 + s_sex| idschool),  
                   data=data)

screenreg(m3_speceff)

anova(m2_speceff,m3_speceff)

#Competences moderation
m4_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     s_sex*s_pv1cil +
                     c_pv1cil + c_s_f_ratio +
                     c_pv1cil*s_sex +
                     (1 + s_sex| idschool),  
                   data=data)

screenreg(m4_speceff)

anova(m3_speceff,m4_speceff)

#Female composition moderation
m5_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     s_sex*s_pv1cil +
                     c_pv1cil + c_s_f_ratio +
                     c_s_f_ratio*s_sex +
                     (1 + s_sex| idschool),  
                   data=data)

screenreg(m5_speceff)

#Final table
tab_model(m1_speceff,m2_speceff,m3_speceff,m4_speceff,m5_speceff, 
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = TRUE,
          show.obs = FALSE,show.ngroups = FALSE)

#Plot moderations
plot_model(m4_speceff, type ="pred", terms = c("c_pv1cil","s_sex"))
plot_model(m5_speceff, type="pred", terms = c("c_s_f_ratio","s_sex"))



