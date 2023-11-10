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
bsgchli2 <- read_sav("input/raw_data/2018/BSGCHLI2.sav") #Base de estudiantes
bcgchli2 <- read_sav("input/raw_data/2018/BCGCHLI2.sav") #Base de colegios
btgchli2 <- read_sav("input/raw_data/2018/BTGCHLI2.sav") #Base de profesores

dim(bsgchli2) #Dimension de la base

names(bsgchli2) #Se observan nombres nombres en mayúscula

bsgchli2 <- bsgchli2%>%clean_names() #Nombres en minúscula
btgchli2 <- btgchli2%>%clean_names()
bcgchli2 <- bcgchli2%>%clean_names()

student_data <- bsgchli2 %>% select (#Demográficas/ID
                                         idschool, idstud, s_sex,
                                         #Self-efficacy
                                         s_speceff, s_geneff,
                                         #Demographics
                                         s_hisced,s_homlit,
                                         #Attitudes
                                         s_ictneg, s_ictpos, s_ictfut,
                                         #Digital activities (out of school)
                                         s_usecom, s_useinf, s_accont,
                                         s_usestd, 
                                         #Scholar activities
                                         s_genclass, s_speclass,
                                         s_ictlrn, s_codlrn,
                                         #Puntaje prueba
                                         s_pv1cil=pv1cil)

#Agregación de variables nivel 1 a 2
student_data <- student_data %>% group_by(idschool) %>% 
  mutate(c_speceff=round(mean(s_speceff, na.rm=TRUE),2), 
         c_geneff=round(mean(s_geneff, na.rm=TRUE),2),
         c_ictneg=round(mean(s_ictneg, na.rm=TRUE),2),
         c_ictpos=round(mean(s_ictpos, na.rm=TRUE),2),
         c_ictfut=round(mean(s_ictfut, na.rm=TRUE),2),
         c_usecom=round(mean(s_usecom, na.rm=TRUE),2),
         c_useinf=round(mean(s_useinf, na.rm=TRUE),2),
         c_accont=round(mean(s_accont, na.rm=TRUE),2),
         c_usestd=round(mean(s_usestd, na.rm=TRUE),2),
         c_ictlrn=round(mean(s_ictlrn, na.rm=TRUE),2),
         c_speclass=round(mean(s_speclass, na.rm=TRUE),2),
         c_genclass=round(mean(s_genclass, na.rm=TRUE),2),
         c_codlrn=round(mean(s_codlrn, na.rm=TRUE),2),
         c_pv1cil=round(mean(s_pv1cil, na.rm=TRUE),2),
         )

# La base pública de 2018 no contiene la proporción de hombres y mujeres por escuela.
# Se Estima la proporción de mujeres en 8vo básico, considerando la representatividad de la muestra.
student_data <- student_data %>% group_by(idschool) %>% 
                mutate(c_f_ratio_grade= sum(s_sex,na.rm = T),
                       c_f_ratio_grade=c_f_ratio_grade/n(),
                       c_f_ratio_grade=round(c_f_ratio_grade,2)) %>%
                ungroup()

#2. Preparación base de segundo nivel -----

# Obtener sexo del director.
school_data<- bcgchli2 %>%  
  select(idschool, p_sex) %>% #Seleccionar variables a fundir con base de estudiantes
  rename(c_p_sex = p_sex) #Rename to school level

#Unir ambas bases de datos a partir de id de escuelas
base <- merge(student_data, school_data, by = "idschool", all.x = TRUE)

#Agregar etiquetas variables de nivel 2
label(base[["c_speceff"]]) <- "School mean specific self-efficacy"
label(base[["c_geneff"]]) <- "School mean general self-efficacy"
label(base[["c_ictneg"]]) <- "School mean negative ICT percepctions"
label(base[["c_ictpos"]]) <- "School mean positive ICT perceptions"
label(base[["c_ictlrn"]]) <- "School mean learning tsks ICT at school"
label(base[["c_usestd"]]) <- "School mean use ICT study purposes"
label(base[["c_ictfut"]]) <- "School mean expectations ICT work-study"
label(base[["c_speclass"]]) <- "School mean specialist application ICT in class"
label(base[["c_genclass"]]) <- "School mean general applications ICT in class"
label(base[["c_codlrn"]]) <- "School mean Learning coding in class"
label(base[["c_pv1cil"]]) <- "School mean score CIL Test"
label(base[["c_f_ratio_grade"]]) <- "School proportion of females students in eigth grade"


#3. Agregar datos de profesores ----
teacher_data <- btgchli2 %>% clean_names() %>% #Nombres en minúscula
  select(idschool,idteach,
         t_sex, #Género profe (Not representative)
         t_icteff,
         t_vwpos,
         t_vwneg) %>%
  group_by(idschool) %>%
  mutate(c_t_eff = round(mean(t_icteff,na.rm=T),2),
         c_t_vwpos = round(mean(t_vwpos,na.rm=T),2),
         c_t_vwneg = round(mean(t_vwneg,na.rm=T),2))%>%
  ungroup()

#Merge mean by school teacher variables with students database
base <- merge(base,
              teacher_data%>%group_by(idschool)%>%
                summarise_at(vars(c_t_eff,c_t_vwpos,c_t_vwneg),mean),
              by="idschool",all.x = TRUE, all.y = FALSE)

#Label new variables
label(base[["c_t_eff"]]) <- "School mean teacher ICT self-efficacy"
label(base[["c_t_vwpos"]]) <- "School mean teacher positive ICT views"
label(base[["c_t_vwneg"]]) <- "School mean teacher negative ICT views"


# Merge school data and teacher data
base_2 <- merge(school_data, teacher_data, by = "idschool", all.y = TRUE, all.x=FALSE)

#Guardar base de datos estudiante-colegio
saveRDS(base,file="input/proc_data/03_student_proc_2018.rds")
#Guardar base de datos profesor-colegio
saveRDS(base_2,file="input/proc_data/04_teach_proc_2018.rds")

