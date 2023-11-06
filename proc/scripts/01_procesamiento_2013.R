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
bsgchli1 <- read_sav("input/raw_data/2013/BSGCHLI1.sav") #Base de estudiantes
bcgchli1 <- read_sav("input/raw_data/2013/BCGCHLI1.sav") #Base de colegios
btgchli1 <- read_sav("input/raw_data/2013/BtGCHLI1.sav") #Base de profesores


#1. Preparación base de primer nivel ----

dim(bsgchli1) #Dimension de la base

names(bsgchli1) #Se observan nombres nombres en mayúscula

bsgchli1 <- bsgchli1%>%clean_names() #Nombres en minúscula
                           
names(bsgchli1) #Revisión

bsgchli1 %>% count(idschool) #Cantidad de casos en escuela (Nivel 2)

#Selección de las variables a trabajar
base <- bsgchli1 %>% select (idstud, idschool, #ID nivel 1 y 2
                             s_sex, #Género del estudiante
                             s_adveff, s_baseff, s_intrst, #Variables de autoeficacia y motivación
                             s_tsklrn,s_uselrn,s_usestd,s_useapp,s_userec,s_usecom,s_useinf) #Tipos de uso

#Agregación de variables nivel 1 a 2
base <- base %>% group_by(idschool) %>% 
  mutate(c_adveff=round(mean(s_adveff, na.rm=TRUE),2), #promedio autoeficacia avanzada por colegio
         c_baseff=round(mean(s_baseff, na.rm=TRUE),2), #promedio autoeficacia básica por colegio
         c_intrst=round(mean(s_intrst, na.rm=TRUE),2), #promedio interés y motivación por colegio
         c_tsklrn=round(mean(s_tsklrn, na.rm=TRUE),2),
         c_uselrn=round(mean(s_uselrn, na.rm=TRUE),2),
         c_usestd=round(mean(s_usestd, na.rm=TRUE),2),
         c_useapp=round(mean(s_useapp, na.rm=TRUE),2),
         c_userec=round(mean(s_userec, na.rm=TRUE),2),
         c_usecom=round(mean(s_usecom, na.rm=TRUE),2),
         c_useinf=round(mean(s_useinf, na.rm=TRUE),2),
  )

#Tenemos las variables de nivel 2 que pueden conformarse desde la base de estudiantes.
#Sin embargo, también necesitamos variables de nivel 2 que están solo en la base de colegios.
#Por lo que a continuación se unifican ambas bases.

#2. Preparación base de segundo nivel -----

#Revisamos si hay NA en las variables de cantidad de hombres y mujeres total y en 8vo
sum(is.na(bcgchli1$IP1G03A))
sum(is.na(bcgchli1$IP1G03B))
sum(is.na(bcgchli1$IP1G04A))
sum(is.na(bcgchli1$IP1G04B))

#Hacemos vectores con el id de las escuelas que tienen NA
deleted_schools_03a <- bcgchli1[which(is.na(bcgchli1$IP1G03A)),]$IDSCHOOL
deleted_schools_03b <- bcgchli1[which(is.na(bcgchli1$IP1G03B)),]$IDSCHOOL
deleted_schools_04a <- bcgchli1[which(is.na(bcgchli1$IP1G04A)),]$IDSCHOOL
deleted_schools_04b <- bcgchli1[which(is.na(bcgchli1$IP1G04B)),]$IDSCHOOL

`%!in%` = Negate(`%in%`) #Crear función de exclusión conjunta

#Eliminamos filas con NA para poder calcular los ratios de composición de género
bcgchli1 <- filter(bcgchli1, IDSCHOOL %!in% c(deleted_schools_03a,
                                              deleted_schools_03b,
                                              deleted_schools_04a,
                                              deleted_schools_04b))

#Verificar
sum(is.na(bcgchli1$IP1G03A))
sum(is.na(bcgchli1$IP1G04A))
sum(is.na(bcgchli1$IP1G03B))
sum(is.na(bcgchli1$IP1G04B))

names(bcgchli1) #También tiene nombres de columna en mayúscula


#Calculamos el ratio de genero total y de 8vo básico por escuela.
#Para evitar números indefinidos, se pide que si no hay mujeres en la escuela, ponga valor 0
# Si no hay hombres, ponga valor 100. 
school_data<- bcgchli1 %>% clean_names() %>% 
  mutate(gen_ratio_total = ifelse(ip1g03a==0,0,
                           round(ip1g03a/(ip1g03a+ip1g03b),2)),
         gen_ratio_grade = ifelse(ip1g04a==0,0,
                           round(ip1g04a/(ip1g04a+ip1g04b),2))) %>%
  select(idschool, gen_ratio_total, gen_ratio_grade, p_sex,p_priv) #Seleccionar variables a fundir con base de estudiantes

#También se elimina de la base original las escuelas con NA en ratio de genero
base <- filter(base, idschool %!in% c(deleted_schools_03a,
                                      deleted_schools_03b,
                                      deleted_schools_04a,
                                      deleted_schools_04b))

#Unir ambas bases de datos a partir de id de escuelas
base <- merge(base, school_data, by = "idschool", all.x = TRUE)

#Agregar etiquetas variables de nivel 2
label(base[["c_adveff"]]) <- "School mean advanced self-efficacy"
label(base[["c_baseff"]]) <- "School mean basic self-efficacy"
label(base[["c_intrst"]]) <- "School mean interest and enjoyment ICT"
label(base[["gen_ratio_total"]]) <- "Ratio of total women students"
label(base[["gen_ratio_grade"]]) <- "Ratio of 8th grade women students"
label(base[["c_tsklrn"]]) <- "School mean Learning of ICT tasks at school"
label(base[["c_uselrn"]]) <- "School mean Use of ICT during lessons at school"
label(base[["c_usestd"]]) <- "School mean Use of ICT for study purposes"
label(base[["c_useapp"]]) <- "School mean Use of specific ICT applications"
label(base[["c_userec"]]) <- "School mean Use of ICT for recreation"
label(base[["c_usecom"]]) <- "School mean Use of ICT for social communication"
label(base[["c_useinf"]]) <- "School mean Use of ICT for exchanging information"

#Guardar base de datos estudiante-colegio
saveRDS(base,file="input/proc_data/01_student_proc_2013.rds")

#3. Agregar datos de profesores ----
teacher_data <- btgchli1 %>% clean_names() %>% #Nombres en minúscula
                select(idschool,idteach,
                       t_sex, #Género profe
                       t_eff
                       ) %>%
                mutate(c_t_eff=round(mean(t_eff,na.rm=T),2))%>%
                filter(idschool %!in% c(deleted_schools_03a,
                                        deleted_schools_03b,
                                        deleted_schools_04a,
                                        deleted_schools_04b))

label(teacher_data[["c_t_eff"]]) <- "School mean teacher self-efficacy"

base_2 <- merge(school_data, teacher_data, by = "idschool", all.y = TRUE, all.x=FALSE)


#Guardar base de datos profesor-colegio
saveRDS(base_2,file="input/proc_data/02_teach_proc_2013.rds")



