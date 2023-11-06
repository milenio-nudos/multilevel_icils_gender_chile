#0. Preparation ----

pacman::p_load(tidyverse, #Data manipulation
               stargazer, #Descriptive tables
               sjmisc #Descriptive tables
               )

data <- readRDS("./input/proc_data/03_student_proc_2018.rds")

#1. Descriptive analysis ----

#Student level
stargazer(data %>% select(starts_with("s_")), 
          title = "Descriptivos generales Estudiantes",type="text")

sjmisc::descr(data %>% select(starts_with("s_")))

#School level
data_aggregated <- data %>% select(starts_with("c_"),idschool) %>% group_by(idschool) %>% 
                  summarise(across(everything(),~ mean(.x, na.rm = TRUE))) %>% ungroup()

sjmisc
(data_aggregated, title = "Descriptivos generales Escuelas",type="text")

#2. 

df %>%
  filter(str_detect(Action, 'Field')) %>%
  na.omit %>%  
  summarise(across(-c(Difficulty, Action), ~cor(.x, Difficulty)))
