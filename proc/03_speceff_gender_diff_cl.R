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
    s_sex,s_pv1cil,s_speceff,s_geneff,
    #School level
    c_pv1cil,c_s_f_ratio,
    #Control variables
    s_hisced,s_homlit,
    #Items specific self-efficacy
    is2g27b,is2g27e,is2g27g,is2g27h,
    #Items general self-efficacy
    is2g27a,is2g27c,is2g27d,is2g27i,is2g27j,is2g27k,is2g27l,is2g27m,
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
val_label(data$is2g27a, 8) <- NULL
val_label(data$is2g27a, 9) <- NULL
val_label(data$is2g27c, 8) <- NULL
val_label(data$is2g27c, 9) <- NULL
val_label(data$is2g27d, 8) <- NULL
val_label(data$is2g27d, 9) <- NULL
val_label(data$is2g27i, 8) <- NULL
val_label(data$is2g27i, 9) <- NULL
val_label(data$is2g27j, 8) <- NULL
val_label(data$is2g27j, 9) <- NULL
val_label(data$is2g27k, 8) <- NULL
val_label(data$is2g27k, 9) <- NULL
val_label(data$is2g27l, 8) <- NULL
val_label(data$is2g27l, 9) <- NULL
val_label(data$is2g27m, 8) <- NULL
val_label(data$is2g27m, 9) <- NULL


#Change labels speceff items
var_label(data) <- list(
  is2g27b = "How well can you do/Create a database (e.g. using [Microsoft Access ®])",
  is2g27e = "How well can you do/Build or edit a webpage",
  is2g27g = "How well can you do/Create a computer program, macro, or [app]",
  is2g27h = "How well can you do/Set up a local area network of computers or other ICT"
)

#Change labels geneff items
var_label(data) <- list(
  is2g27a = "Edit digital photographs or other graphic images",
  is2g27c = "Write or edit text for a school assignment",
  is2g27d = "Search for and find relevant information for a school project	
  on the Internet",
  is2g27i = "Create a multi-media presentation (with sound, pictures, or video)",
  is2g27j = "Upload text, images, or video to an online profile",
  is2g27k = "Insert an image into a document or message",
  is2g27l = "Install a program or [app]",
  is2g27m = "Judge whether you can trust information you find on the Internet	"
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

data |>
  mutate(s_sex=to_label(s_sex),
         is2g27a=to_label(is2g27a),
         is2g27c=to_label(is2g27c),
         is2g27d=to_label(is2g27d),
         is2g27i=to_label(is2g27i),
         is2g27j=to_label(is2g27j),
         is2g27k=to_label(is2g27k),
         is2g27l=to_label(is2g27l),
         is2g27m=to_label(is2g27m)) |>
  tbl_summary(include=c(is2g27a,is2g27c,is2g27d,is2g27i,is2g27j,
                        is2g27k,is2g27l,is2g27m),
              by=s_sex,
              missing="no") |>
  modify_header(label="**Variable**")


#Descriptive index table
data |> 
  mutate(s_sex=to_label(s_sex)) |>
  tbl_summary(include=c(s_sex,s_geneff,s_speceff,s_pv1cil,c_pv1cil,c_s_f_ratio),
              missing = "no") |>
  modify_header(label="**Variable**")

#Differences by group table
data |> 
  mutate(s_sex=to_label(s_sex)) |>
  tbl_summary(include = c(s_pv1cil,s_geneff,s_speceff),
              by = s_sex,
              missing = "no") |>
  add_p() |>
  add_n() |>
  modify_header(label="**Variables**")

#Plot differences
data |>
  ggplot(aes(x=to_label(s_sex), y=s_speceff, fill=to_label(s_sex))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="green",fill="red") #No recognosible pattern

data |>
  ggplot(aes(x=to_label(s_sex), y=s_geneff, fill=to_label(s_sex))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="green",fill="red") #No recognosible pattern

data |>
  ggplot(aes(x=to_label(s_sex), y=s_pv1cil, fill=to_label(s_sex))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="green",fill="red") #No recognosible pattern

#Anova test speceff
aov_1_sp <- aov(s_speceff~s_sex,data = data) #Significative differences
aov_2_sp <- aov(s_speceff~s_sex+s_pv1cil,data=data) #Significative differences
aov_3_sp <- aov(s_speceff~s_sex*s_pv1cil,data=data) #Non-significative differences

summary(aov_1_sp)
summary(aov_2_sp)
summary(aov_3_sp)

#Anova test geneff
aov_1_ge <- aov(s_geneff~s_sex,data = data) #Significative differences
aov_2_ge <- aov(s_geneff~s_sex+s_pv1cil,data=data) #Significative differences
aov_3_ge <- aov(s_geneff~s_sex*s_pv1cil,data=data) #Non-significative differences

summary(aov_1_ge)
summary(aov_2_ge)
summary(aov_3_ge)

#Correlation
data_corr <- data |> select(-c(idschool,idstud,
                               starts_with(c("is2","tot","wgt"))))

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

data |>
  group_by(to_label(s_sex)) |>
  dplyr::summarize(corr=cor(s_geneff,s_pv1cil,use="complete.obs")|>
                     round(3)) |>
  gt() |>
  tab_header("Reinforcement between ICT competences and general self-efficacy",
             subtitle = "Correlation by gender") |>
  cols_label(corr=md("**Pearson score**"))

#Fast apply model
ggplot(data, aes(x=s_pv1cil, y=s_speceff,color=to_label(s_sex))) + 
  geom_point()+
  geom_smooth(method=lm)

#Fast apply model
ggplot(data, aes(x=s_pv1cil, y=s_geneff,color=to_label(s_sex))) + 
  geom_point()+
  geom_smooth(method=lm)

#2. Models ----

#Rescale test score (500 points) and change class of categoricals
data <- data |> mutate (s_sex=to_label(s_sex),
                        s_pv1cil=scale(s_pv1cil)) 

##2.1 Specialized self-efficacy ----

# Null Model
m0_speceff <- lmer(s_speceff ~ 1 + (1 | idschool), data=data)

ICC(m0_speceff) #0.6 rounded ICC

# Level 1 fixed effects
m1_a_speceff <- lmer(s_speceff ~ 1 +
                     s_sex +
                     (1 | idschool),  
                   data=data) 

m1_b_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     (1 | idschool),  
                   data=data) 

m1_c_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     s_sex*s_pv1cil +
                     (1 | idschool),  
                   data=data) 

# Level 2 fixed effects
m2_a_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     c_s_f_ratio +
                     (1 | idschool),  
                   data=data)


m2_b_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     c_pv1cil +
                     (1 | idschool),  
                   data=data)

m2_c_speceff <- lmer(s_speceff ~ 1 +
                     s_sex + s_pv1cil +
                     c_pv1cil+ c_s_f_ratio +
                     (1 | idschool),  
                   data=data)

# Test random effects
m3_a_speceff <- lmer(s_speceff ~ 1 +
                       s_sex + s_pv1cil +
                       (1 + s_sex| idschool),  
                     data=data)

m3_b_speceff <- lmer(s_speceff ~ 1 +
                       s_sex + s_pv1cil +
                       (1 + s_pv1cil| idschool),  
                     data=data)

anova(m1_b_speceff,m3_a_speceff) #No random slope (between-differences)
anova(m1_b_speceff,m3_b_speceff) #No random slope (between-differences)

#Final table
tab_model(m0_speceff,
          m1_a_speceff,m1_b_speceff,m1_c_speceff,
          m2_a_speceff,m2_b_speceff,m2_c_speceff,
          m3_a_speceff,m3_b_speceff,
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = TRUE,
          show.obs = FALSE,show.ngroups = FALSE,
          dv.labels = c("Null",
                        "Only sex","Sex+CIL","Int. Lev.1",
                        "+ women ratio", "+ mean CIL", "Complete",
                        "Sex random slope", "CIL random slope"))

#2.2 General Self-efficacy ----

#Null Model
m0_geneff <- lmer(s_geneff ~ 1 + (1 | idschool), data=data)
ICC(m0_geneff) #0.5 rounded ICC

#Level 1 fixed effects
m1_a_geneff  <- lmer(s_geneff ~ 1 +
                       s_sex +
                       (1 | idschool),  
                     data=data) 

m1_b_geneff  <- lmer(s_geneff ~ 1 +
                       s_sex + s_pv1cil +
                       (1 | idschool),  
                     data=data) 

m1_c_geneff <- lmer(s_geneff ~ 1 +
                        s_sex + s_pv1cil +
                        s_sex*s_pv1cil +
                        (1 | idschool),  
                      data=data) 


# Level 2 fixed effects
m2_a_geneff <- lmer(s_geneff ~ 1 +
                       s_sex + s_pv1cil +
                       c_s_f_ratio +
                       (1 | idschool),  
                     data=data)


m2_b_geneff <- lmer(s_geneff ~ 1 +
                       s_sex + s_pv1cil +
                       c_pv1cil +
                       (1 | idschool),  
                     data=data)

m2_c_geneff <- lmer(s_geneff ~ 1 +
                       s_sex + s_pv1cil +
                       c_pv1cil+ c_s_f_ratio +
                       (1 | idschool),  
                     data=data)

#Test Random Effects
m3_a_geneff <- lmer(s_geneff ~ 1 +
                       s_sex + s_pv1cil +
                       (1 + s_sex| idschool),  
                     data=data)

m3_b_geneff <- lmer(s_geneff ~ 1 +
                      s_sex + s_pv1cil +
                      (1 + s_pv1cil| idschool),  
                    data=data)

anova(m1_b_geneff,m3_a_geneff) #No random slope
anova(m1_b_geneff,m3_b_geneff) #Random slope!

#Random effects CIL level 2
m4_a_geneff <- lmer(s_geneff ~ 1 +
                      s_sex + s_pv1cil+
                      c_s_f_ratio + c_pv1cil +
                      c_s_f_ratio*s_pv1cil+
                      (1 + s_pv1cil| idschool),
                    data = data)

m4_b_geneff <- lmer(s_geneff ~ 1 +
                      s_sex + s_pv1cil+
                      c_s_f_ratio + c_pv1cil +
                      c_pv1cil*s_pv1cil+
                      (1 + s_pv1cil| idschool),
                    data = data)


#Final table
tab_model(m0_geneff,
          m1_a_geneff,m1_b_geneff,m1_c_geneff,
          m2_a_geneff,m2_b_geneff,m2_c_geneff,
          m3_a_geneff,m3_b_geneff,
          m4_a_geneff,m4_b_geneff,
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = TRUE,
          show.obs = FALSE,show.ngroups = FALSE,
          dv.labels = c("Null",
                        "Only sex","Sex+CIL","Int. Lev.1",
                        "+ women ratio", "+ mean CIL", "Complete",
                        "Sex random slope", "CIL random slope",
                        "Mod. CIL*WR","Mod CIL*C_CIL"))


sjPlot::plot_model(m1_b_a_geneff,type = "re")

# Nwe model set ---
# Geneff Models -----

m1_a_speceff <- lmer(s_speceff ~ 1 +
                       s_sex +
                       (1 | idschool),  
                     data=data) 

m1_b_speceff <- lmer(s_speceff ~ 1 +
                       s_sex + s_pv1cil +
                       (1 | idschool),  
                     data=data) 

m1_c_speceff <- lmer(s_speceff ~ 1 +
                       s_sex + s_pv1cil +
                       s_geneff +
                       (1 | idschool),  
                     data=data) 

m1_d_speceff <- lmer(s_speceff ~ 1 +
                       s_sex + s_pv1cil +
                       s_geneff +
                       s_sex*s_geneff+
                       (1 | idschool),  
                     data=data) 

m1_e_speceff <- lmer(s_speceff ~ 1 +
                       s_sex + s_pv1cil +
                       s_geneff +
                       s_geneff*s_pv1cil+
                       (1 | idschool),  
                     data=data) 

#Final table
tab_model(m1_a_speceff,m1_b_speceff,m1_c_speceff,
          m1_d_speceff,m1_e_speceff,
          show.ci = FALSE, auto.label = FALSE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = TRUE,
          
          show.obs = FALSE,show.ngroups = FALSE)

plot_model(m1_d_speceff,terms = c("s_geneff","s_sex"),type = "pred")
