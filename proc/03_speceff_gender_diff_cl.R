#0. Preparation ---

options(scipen=999) #No scientific notation

nudos_color_binary <- c("#5f5758","#ff3057") #Nudos theme


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
               AICcmodavg, #Best fit model
               responsePatterns)

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

#Change labels speceff items
var_label(data) <- list(
  is2g27b = "Create a database (e.g. using [Microsoft Access ®])",
  is2g27e = "Build or edit a webpage",
  is2g27g = "Create a computer program, macro, or [app]",
  is2g27h = "Set up a local area network of computers or other ICT"
)

#1. Exploring data ----

#Items General Self-efficacy
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
  modify_header(label="**Variable**")|>
  add_p() |>
  modify_caption("**General Self-efficacy** *(How well can you do each of these tasks when using ICT?)*")

#Items Specialized Self-efficacy

gtsummary::theme_gtsummary_journal()

data |>
  mutate(s_sex=to_label(s_sex),
         is2g27b=to_label(is2g27b),
         is2g27e=to_label(is2g27e),
         is2g27g=to_label(is2g27g),
         is2g27h=to_label(is2g27h)) |>
  tbl_summary(include=c(is2g27b,is2g27e,is2g27g,is2g27h),
              by=s_sex,
              missing="no") |>
  modify_header(label="**Item**") |>
  add_p() |>
  modify_caption("**Specialized Self-efficacy** *(How well can you do each of these tasks when using ICT?)*")

#Plot items General self-efficacy
geneff_plot <- data |>
  mutate(s_sex=to_label(s_sex),
         is2g27a=to_label(is2g27a),
         is2g27c=to_label(is2g27c),
         is2g27d=to_label(is2g27d),
         is2g27i=to_label(is2g27i),
         is2g27j=to_label(is2g27j),
         is2g27k=to_label(is2g27k),
         is2g27l=to_label(is2g27l),
         is2g27m=to_label(is2g27m))

rbind(
  geneff_plot|>count(variable=is2g27a)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Edit digital photographs or other graphic images"),
  geneff_plot|>count(variable=is2g27c)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Write or edit text for a school assignment"),
  geneff_plot|>count(variable=is2g27d)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Search for and find relevant information for a school project on the Internet"),
  geneff_plot|>count(variable=is2g27i)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Create a multi-media presentation (with sound, pictures, or video)"),
  geneff_plot|>count(variable=is2g27j)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Upload text, images, or video to an online profile"),
  geneff_plot|>count(variable=is2g27k)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Insert an image into a document or message"),
  geneff_plot|>count(variable=is2g27l)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Install a program or [app]"),
  geneff_plot|>count(variable=is2g27m)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Judge whether you can trust information you find on the Internet")
  ) |>
  mutate(prop=round(prop,2)) |>
  ggplot(aes(x=name,y=prop,fill=variable))+
  geom_bar(position = "fill",stat="identity",color="black")+
  scale_fill_manual(values = c("#ff3057","#fdffde","#5f5758"))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  labs(title="General Self-efficacy",
       subtitle="How well can you do each of these tasks when using ICT?",
       x="",y="",fill="")+
  theme(legend.position = "top")+ 
  scale_y_continuous(labels = scales::percent)

#Plot items Specialized Self-efficacy
speceff_plot <- data |>
  mutate(s_sex=to_label(s_sex),
         is2g27a=to_label(is2g27a),
         is2g27c=to_label(is2g27c),
         is2g27d=to_label(is2g27d),
         is2g27i=to_label(is2g27i))

rbind(
  speceff_plot|>count(variable=is2g27a)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Create a database (e.g. using [Microsoft Access ®])"),
  speceff_plot|>count(variable=is2g27c)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Build or edit a webpage"),
  speceff_plot|>count(variable=is2g27d)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Create a computer program, macro, or [app]"),
  speceff_plot|>count(variable=is2g27i)|>drop_na()|>
    mutate(prop=n/sum(n),
           name="Set up a local area network of computers or other ICT")
) |>
  mutate(prop=round(prop,2)) |>
  ggplot(aes(x=name,y=prop,fill=variable))+
  geom_bar(position = "fill",stat="identity",color="black",width=0.6)+
  scale_fill_manual(values = c("#ff3057","#fdffde","#5f5758"))+
  coord_flip()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  labs(title="Specialized Self-efficacy",
       subtitle="How well can you do each of these tasks when using ICT?",
       x="",y="",fill="")+
  theme(legend.position = "top")+ 
  scale_y_continuous(labels = scales::percent)

#Descriptive index table
data |> 
  mutate(s_sex=to_label(s_sex)) |>
  tbl_summary(include=c(s_sex,s_geneff,s_speceff,s_pv1cil,c_pv1cil,c_s_f_ratio),
              missing = "no",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} / {N} ({p}%)")
              ) |>
  modify_header(label="**Variable**")

#Histograms of level 2 variables
data |>
  group_by(idschool) |>
  summarise(c_pv1cil=mean(c_pv1cil)) |>
  as.data.frame() |>
  ggplot(aes(x=c_pv1cil))+
  geom_density(fill="#ff3057", color="#5f5758", alpha=0.8)

data |>
  group_by(idschool) |>
  summarise(c_s_f_ratio=mean(c_s_f_ratio)) |>
  as.data.frame() |>
  ggplot(aes(x=c_s_f_ratio))+
  geom_density(fill="#ff3057", color="#5f5758", alpha=0.8)

#Differences by group table
data |> 
  mutate(s_sex=to_label(s_sex)) |>
  tbl_summary(include = c(s_pv1cil,s_geneff,s_speceff),
              by = s_sex,
              missing = "no",
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} / {N} ({p}%)")
              ) |>
  add_p() |>
  add_n() |>
  modify_header(label="**Variables**")

#Plot differences"
data |>
  ggplot(aes(x=to_label(s_sex), y=s_geneff, fill=to_label(s_sex))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#5f5758",fill="red") #No recognosible pattern

data |>
  ggplot(aes(x=to_label(s_sex), y=s_speceff, fill=to_label(s_sex))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#5f5758",fill="red") #No recognosible pattern

data |>
  ggplot(aes(x=to_label(s_sex), y=s_pv1cil, fill=to_label(s_sex))) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="#5f5758",fill="red") #No recognosible pattern

#Anova test geneff
aov_1_ge <- aov(s_geneff~s_sex,data = data) #Significative differences
aov_2_ge <- aov(s_geneff~s_sex+s_pv1cil,data=data) #Significative differences
aov_3_ge <- aov(s_geneff~s_sex*s_pv1cil,data=data) #Non-significative differences

summary(aov_1_ge)
summary(aov_2_ge)
summary(aov_3_ge)

#Anova test speceff
aov_1_sp <- aov(s_speceff~s_sex,data = data) #Significative differences
aov_2_sp <- aov(s_speceff~s_sex+s_pv1cil,data=data) #Significative differences
aov_3_sp <- aov(s_speceff~s_sex*s_pv1cil,data=data) #Non-significative differences

summary(aov_1_sp)
summary(aov_2_sp)
summary(aov_3_sp)

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
  dplyr::summarize(corr=cor(s_geneff,s_pv1cil,use="complete.obs")|>
                     round(3)) |>
  gt() |>
  tab_header("Reinforcement between ICT competences and general self-efficacy",
             subtitle = "Correlation by gender") |>
  cols_label(corr=md("**Pearson score**"))

data |>
  group_by(to_label(s_sex)) |>
  dplyr::summarize(corr=cor(s_speceff,s_pv1cil,use="complete.obs")|>
                     round(3)) |>
  gt() |>
  tab_header("Reinforcement between ICT competences and specialized self-efficacy",
             subtitle = "Correlation by gender") |>
  cols_label(corr=md("**Pearson score**"))

#Fast apply model
ggplot(data, aes(x=s_pv1cil, y=s_geneff,color=to_label(s_sex))) + 
  geom_point()+
  geom_smooth(method=lm)

#Fast apply model
ggplot(data, aes(x=s_pv1cil, y=s_speceff,color=to_label(s_sex))) + 
  geom_point()+
  geom_smooth(method=lm)

#2. Models ----

#Rescale test score (500 points) and change class of categoricals
data <- data |> mutate (s_sex=to_label(s_sex),
                        s_pv1cil=scale(s_pv1cil)) 

#Relabel variable
label(data[["s_pv1cil"]]) <- "Computer and Information Literacy Score"


##2.1 General Self-efficacy ----

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

sjPlot::plot_model(m1_b_geneff,type = "re")

#Random effects CIL level 2
m4_x_geneff <- lmer(s_geneff ~ 1 +
                      s_sex + s_pv1cil+
                      c_s_f_ratio + c_pv1cil +
                      (1 + s_pv1cil| idschool),
                    data = data)

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

anova(m4_x_geneff,m4_a_geneff)
anova(m4_x_geneff,m4_b_geneff)

#Final table
tab_model(m0_geneff,
          m1_a_geneff,m1_b_geneff,m1_c_geneff,
          m2_a_geneff,m2_b_geneff,m2_c_geneff,
          m3_b_geneff,
          m4_a_geneff,m4_b_geneff,
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = TRUE,
          show.obs = FALSE,show.ngroups = FALSE,
          dv.labels = c("Null",
                        "Only sex","Sex+CIL","Int. Lev.1",
                        "+ women ratio", "+ mean CIL", "Complete",
                        "CIL random slope",
                        "Mod. CIL*WR","Mod CIL*C_CIL"))


##2.2 Specialized self-efficacy ----

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