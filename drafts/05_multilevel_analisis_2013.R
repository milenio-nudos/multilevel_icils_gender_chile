rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

pacman::p_load(lme4, #multilevel analysis
               descr, #Graficos descriptivos
               ggplot2, #Gráficos
               sjmisc, #Manipulación de etiquetas
               naniar, #Manipulación de casos perdidos
               hrbrthemes, #Themes de los gráficos
               tidyverse, #Manipulación bb.dd
               stargazer, #Descriptive table
               ggExtra, #Marginales plots
               reghelper, #Correlación intraclase
               texreg #Ver resultados lm
               )

base <- readRDS(file="input/proc_data/base_proc.rds")

#1. Analisis univariados ----

stargazer(base%>%select(-c(idschool,idstud)), title = "Descriptivos generales", type='html')

hist(base$s_adveff)
hist(base$s_baseff)
hist(base$s_intrst)

#2. Cruce descriptivo de variables ----

#Explorar cruce de género con variables de interés
base%>%
  drop_na(p_priv)%>% #Eliminar establecimientos con casos perdidos en sostenedor
  mutate(gender_by_school= case_when(
    p_sex == 0 & p_priv == 0 ~ "Público con director hombre",
    p_sex == 1 & p_priv == 0 ~ "Público con directora mujer",
    p_sex == 0 & p_priv == 1 ~ "Privado con director hombre",
    p_sex == 1 & p_priv == 1 ~ "Privado con directora mujer"))%>%
  group_by(s_sex,gender_by_school)%>%
  summarise(s_baseff=mean(s_baseff,na.rm=T))%>%

    ggplot(aes(x=to_label(s_sex), y=s_baseff, fill=to_label(s_sex)))+
    geom_bar(stat = "identity",position = "dodge", color="black", width = 0.6) + 
    facet_wrap(~to_label(gender_by_school))+
    theme(axis.text=element_text(size=5),
          axis.title=element_text(size=10,face="bold"),
          legend.position = "none") + 
    labs(x="Género del estudiante", y="Puntaje",
        title = "Promedio de expectativas de autoeficacia básica por género de directivo y sostenedor del establecimiento")+
    theme_ipsum()+
    guides(fill=FALSE)

base%>%
  drop_na(p_priv)%>% #Eliminar establecimientos con casos perdidos en sostenedor
  mutate(gender_by_school= case_when(
    p_sex == 0 & p_priv == 0 ~ "Público con director hombre",
    p_sex == 1 & p_priv == 0 ~ "Público con directora mujer",
    p_sex == 0 & p_priv == 1 ~ "Privado con director hombre",
    p_sex == 1 & p_priv == 1 ~ "Privado con directora mujer"))%>%
  group_by(s_sex,gender_by_school)%>%
  summarise(s_adveff=mean(s_adveff,na.rm=T))%>%
  
  ggplot(aes(x=to_label(s_sex), y=s_adveff, fill=to_label(s_sex)))+
  geom_bar(stat = "identity",position = "dodge", color="black", width = 0.6) + 
  facet_wrap(~to_label(gender_by_school))+
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=10,face="bold"),
        legend.position = "none") + 
  labs(x="Género del estudiante", y="Puntaje",
       title = "Promedio de expectativas de autoeficacia avanzada por género de directivo y sostenedor del establecimiento")+
  theme_ipsum()+
  guides(fill=FALSE)

base%>%
  drop_na(p_priv)%>% #Eliminar establecimientos con casos perdidos en sostenedor
  mutate(gender_by_school= case_when(
    p_sex == 0 & p_priv == 0 ~ "Público con director hombre",
    p_sex == 1 & p_priv == 0 ~ "Público con directora mujer",
    p_sex == 0 & p_priv == 1 ~ "Privado con director hombre",
    p_sex == 1 & p_priv == 1 ~ "Privado con directora mujer"))%>%
  group_by(s_sex,gender_by_school)%>%
  summarise(s_adveff=mean(s_adveff,na.rm=T))%>%
  
  ggplot(aes(x=to_label(s_sex), y=s_adveff, fill=to_label(s_sex)))+
  geom_bar(stat = "identity",position = "dodge", color="black",width = 0.6) + 
  facet_wrap(~to_label(gender_by_school))+
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=10,face="bold"),
        legend.position = "none") + 
  labs(x="Género del estudiante", y="Puntaje",
       title = "Promedio de motivación intrínseca por género de directivo y sostenedor del establecimiento")+
  theme_ipsum()+
  guides(fill=FALSE)

#Se crea una base de datos del nivel 2 para hacer gráficos descriptivos
school_level <- base %>%
  group_by(idschool)%>%
  summarise(gen_ratio_total=mean(gen_ratio_total),
            gen_ratio_grade=mean(gen_ratio_grade),
            p_priv=as.character(mean(p_priv)),
            p_sex=as.character(mean(p_sex)),
            c_adveff=mean(c_adveff),
            c_baseff=mean(c_baseff),
            c_intrst=mean(c_intrst))%>%
  mutate(p_sex = case_when(
                  p_sex == 0 ~ "Hombre",
                  p_sex == 1 ~ "Mujer"))


#Explorar composición de género de cada escuela e influencia del género de la dirección en variables de interes
school_level%>%
  ggplot(aes(x=idschool,y=gen_ratio_total,fill=p_sex))+
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=10,face="bold")) + 
  labs(fill="Género de director/a", 
       title = "Proporción de alumnas mujeres (total) según el género del directivo de cada escuela")+
  theme_ipsum()+
  theme(legend.position = "top",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())+
  scale_y_continuous(labels = scales::percent)


school_level%>%
  ggplot(aes(x=idschool,y=gen_ratio_total,fill=p_sex))+
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=10,face="bold")) + 
  labs(fill="Género de director/a", 
       title = "Proporción de alumnas mujeres de 8vo básico según el género del directivo de cada escuela")+
  theme_ipsum()+
  theme(legend.position = "top",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())+
  scale_y_continuous(labels = scales::percent)

school_level%>%
  mutate(prop_distinct = case_when(
                         gen_ratio_total > 0.5 ~ "Mayoría hombres",
                         gen_ratio_total <= 0.5 ~ "Mayoría mujeres"))%>%
  ggplot(aes(x=c_baseff))+
  geom_histogram( color="#e9ecef",fill="#69b3a2", alpha=0.6, position = 'identity') +
  theme_ipsum()+
  facet_grid(prop_distinct~p_sex)+
  labs(x="Puntaje indicador",y="Frecuencia",
       title="Distribución del promedio autoeficacia básica del colegio según director y composición estudiantil de género")

school_level%>%
  mutate(prop_distinct = case_when(
    gen_ratio_total > 0.5 ~ "Mayoría hombres",
    gen_ratio_total <= 0.5 ~ "Mayoría mujeres"))%>%
  ggplot(aes(x=c_adveff))+
  geom_histogram( color="#e9ecef",fill="#69b3a2", alpha=0.6, position = 'identity') +
  theme_ipsum()+
  facet_grid(prop_distinct~p_sex)+
  labs(x="Puntaje indicador",y="Frecuencia",
       title="Distribución del promedio autoeficacia avanzada del colegio según director y composición estudiantil de género")

school_level%>%
  mutate(prop_distinct = case_when(
    gen_ratio_total > 0.5 ~ "Mayoría hombres",
    gen_ratio_total <= 0.5 ~ "Mayoría mujeres"))%>%
  ggplot(aes(x=c_intrst))+
  geom_histogram( color="#e9ecef",fill="#69b3a2", alpha=0.6, position = 'identity') +
  theme_ipsum()+
  facet_grid(prop_distinct~p_sex)+
  labs(x="Puntaje indicador",y="Frecuencia",
       title="Distribución del promedio motivación intrínseca del colegio según director y composición estudiantil de género")

#3. Análisis correlacional ----

#Nivel 1
ggMarginal(base%>%
             ggplot(aes(x=s_baseff,y=s_intrst,color=to_label(s_sex)))+
             geom_point()+
             geom_smooth(method=lm , color="red", se=TRUE,fill="#69b3a2")+
             theme_ipsum()+
             labs(title = "Nivel 1: Relación lineal Motivación intrínseca y expectativas de autoeficacia básica",
                  x = "Expectativas de autoeficacia básica",
                  y = "Motivación intrínseca",
                  color= "Género del estudiante")+
             theme(legend.position = "bottom"),
           type = "boxplot")

ggMarginal(base%>%
  ggplot(aes(x=s_adveff,y=s_intrst,color=to_label(s_sex)))+
  geom_point()+
  geom_smooth(method=lm , color="red", se=TRUE,fill="#69b3a2")+
  theme_ipsum()+
  labs(title = "Nivel 1: Relación lineal Motivación intrínseca y expectativas de autoeficacia avanzada",
       x = "Expectativas de autoeficacia avanzada",
       y = "Motivación intrínseca",
       color= "Género del estudiante")+
    theme(legend.position = "bottom"),
  type = "boxplot")
  
filter(base, s_sex==0) %>% select(s_adveff,s_baseff, s_intrst)%>% tab_corr(triangle="lower")
filter(base, s_sex==1) %>% select(s_adveff,s_baseff, s_intrst,
                           s_homlit,s_hisced,s_hisei)%>% cor(use = "complete.obs")


#Nivel 2
ggMarginal(school_level%>%
             ggplot(aes(x=gen_ratio_total,y=c_intrst,color=to_label(p_sex)))+
             geom_point()+
             geom_smooth(method=lm , color="red", se=TRUE,fill="#69b3a2")+
             theme_ipsum()+
             labs(title = "Nivel 2: Relación lineal Motivación intrínseca y Ratio de estudiantes mujeres total",
                  x = "Ratio de mujeres total",
                  y = "Motivación intrínseca",
                  color= "Género directivo")+
             theme(legend.position = "bottom"),
           type = "boxplot")

ggMarginal(school_level%>%
             ggplot(aes(x=gen_ratio_grade,y=c_intrst,color=to_label(p_sex)))+
             geom_point()+
             geom_smooth(method=lm , color="red", se=TRUE,fill="#69b3a2")+
             theme_ipsum()+
             labs(title = "Nivel 2: Relación lineal Motivación intrínseca y Ratio de estudiantes mujeres 8vo básico",
                  x = "Ratio de mujeres total",
                  y = "Motivación intrínseca",
                  color= "Género directivo")+
             theme(legend.position = "bottom"),
           type = "boxplot")

filter(school_level, p_sex=="Hombre") %>% select(c_adveff,c_baseff, c_intrst,
                                  gen_ratio_total,gen_ratio_grade)%>% tab_corr(triangle="lower")

filter(school_level, p_sex=="Mujer") %>% select(c_adveff,c_baseff, c_intrst,
                                  gen_ratio_total,gen_ratio_grade)%>% tab_corr(triangle="lower")

#4. Modelos de un nivel ----

reg_student <- lm(s_intrst~s_adveff+s_baseff,data = base)

reg_school <- lm(s_intrst~s_adveff+s_baseff+gen_ratio_total+gen_ratio_grade+p_priv+p_sex,
                 data=school_level%>%rename(s_adveff=c_adveff,  #Se cambian los nombres para comparar mejor el modelo
                                            s_baseff=c_baseff,
                                            s_intrst=c_intrst))

stargazer(reg_student,reg_school, title = "Comparación de modelos",column.labels=c("Individual","Agregado"), type ='text')

#5. Modelos multinivel

#Modelo nulo

nulo <- lmer(s_intrst ~ 1 + (1 | idschool), data = base)

screenreg(nulo)

reghelper::ICC(nulo) #calcular correlación intraclase

#Modelo predictores estudiantes

nivel_1 <- lmer(s_intrst ~ 1 + s_sex + s_baseff + s_adveff + (1 | idschool), data = base)

screenreg(nivel_1)

#Modelo predictores escuelas

nivel_2 <- lmer(s_intrst ~ 1 + p_sex+ gen_ratio_total + gen_ratio_grade + c_adveff + c_baseff +
                p_priv + (1 | idschool), data = base)

screenreg(nivel_2)

#Modelo predictores estudiantes y escuelas

multinivel_fijo <-  lmer(s_intrst ~ 1 + s_sex + s_baseff + s_adveff + p_sex + 
                         gen_ratio_total + gen_ratio_grade + c_adveff + c_baseff +
                         p_priv + (1 | idschool), data = base)

screenreg(multinivel_fijo)

#Modelo mixto (pendiente aleatoria)
multinivel_aleatorio <-  lmer(s_intrst ~ 1 + s_sex + s_baseff + s_adveff + p_sex + 
                              gen_ratio_total + gen_ratio_grade + c_adveff + c_baseff + 
                              p_priv + (1 + s_sex + s_baseff + s_adveff + gen_ratio_total + 
                              gen_ratio_grade + p_sex | idschool), data = base)

screenreg(multinivel_aleatorio)
