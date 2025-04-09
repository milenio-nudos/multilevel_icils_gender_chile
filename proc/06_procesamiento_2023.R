#0. Preparación ----

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

pacman::p_load(haven, #Abrir bb.dd
               summarytools, #Exploración de datos
               sjPlot, #Visualizaciones 
               tidyverse, #Procesamiento
               janitor, #Procesamiento
               Hmisc #Manipulación etiquetas
)

#Abrir bases de datos
load("input/raw_data/2023/BSGCHLI3.Rdata") #Base de estudiantes
load("input/raw_data/2023/BCGCHLI3.Rdata") #Base de colegios
load("input/raw_data/2023/BTGCHLI3.Rdata") #Base de profesores

dim(BSGCHLI3) #Dimension de la base

names(BSGCHLI3) #Se observan nombres nombres en mayúscula

bsgchli3 <- BSGCHLI3%>%clean_names() #Nombres en minúscula
btgchli3 <- BTGCHLI3%>%clean_names()
bcgchli3 <- BCGCHLI3%>%clean_names()

student_data <- bsgchli3 %>% 
  #Change direction of Self-efficacy items
  mutate(across(starts_with("is3g24"),~ ifelse(.x %in% c(8,9), NA, .x) )) %>%
  mutate(across(starts_with("is3g24"),~ 4 - .x)) %>%
  select (#Demográficas/ID
    idschool, idstud, s_sex,
    #Self-efficacy
    s_speceff, s_geneff,
    #Demographics
    s_hisced,s_homlit,s_hisei,
    #Puntaje prueba
    s_pv1cil=pv1cil,
    #Specialized self-efficacy items
    is3g24d,is3g24k,is3g24l,
    #General self-efficacy items
    is3g24a,is3g24b,is3g24c,is3g24e,is3g24f,
    is3g24g,is3g24h,is3g24i,is3g24j,is3g24m,
    #Weights
    totwgts,wgtadj1s,wgtfac3s,wgtadj3s
  )%>%
  mutate(s_sex = set_na(s_sex, na = c(8,9), as.tag = TRUE))
           
#Aggregate variables to school level
student_data <- student_data %>% 
  group_by(idschool) %>% 
  mutate(c_speceff=round(mean(s_speceff, na.rm=TRUE),2), 
         c_geneff=round(mean(s_geneff, na.rm=TRUE),2),
         c_pv1cil=round(mean(s_pv1cil, na.rm=TRUE),2)
  )

# La base pública de 2023 no contiene la proporción de hombres y mujeres por escuela.
# Se Estima la proporción de mujeres en 8vo básico, considerando la representatividad de la muestra.
student_data <- student_data %>% 
  group_by(idschool) %>% 
  mutate(c_s_f_ratio= sum(s_sex,na.rm = T),
         c_s_f_ratio=c_s_f_ratio/n(),
         c_s_f_ratio=round(c_s_f_ratio,2)) %>%
  ungroup()

#2. Label new variables ----
label(student_data[["c_speceff"]]) <- "School mean specific self-efficacy"
label(student_data[["c_geneff"]]) <- "School mean general self-efficacy"
label(student_data[["c_pv1cil"]]) <- "School mean score CIL Test"
label(student_data[["c_s_f_ratio"]]) <- "School proportion of females students in eigth grade"

# Save data ----
saveRDS(student_data,file="input/proc_data/05_student_proc_2023.rds")
