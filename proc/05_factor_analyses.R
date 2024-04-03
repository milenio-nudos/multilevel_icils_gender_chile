pacman::p_load(tidyverse,
               gtsummary,
               sjmisc, #Label manipulation
               labelled, #Label manipulation
               summarytools,
               sjPlot,
               effects
               #responsePatterms
               )

student_proc_2018 <- readRDS("/Users/daniel/Dropbox (Personal)/github/multilevel_icils_gender_chile/input/proc_data/03_student_proc_2018.rds")



base<- student_proc_2018 |> 
  select(s_sex,
         s_speceff,s_pv1cil,
         is2g27b,is2g27e,is2g27g,is2g27h) |>
  na.omit()

#Negative Correlation speceff wit cil, but positive items with cil

cor(base) #why?

#Delete missing data labels
val_label(base$s_sex,8) <- NULL
val_label(base$s_sex,9) <- NULL
val_label(base$is2g27b,8) <- NULL
val_label(base$is2g27b,9) <- NULL
val_label(base$is2g27e,8) <- NULL
val_label(base$is2g27e,9) <- NULL
val_label(base$is2g27g,8) <- NULL
val_label(base$is2g27g,9) <- NULL
val_label(base$is2g27h,8) <- NULL
val_label(base$is2g27h,9) <- NULL

#Descriptive Specialized Self-efficacy items
base |>
  mutate(s_sex=to_label(s_sex),
         is2g27b=to_label(is2g27b),
         is2g27e=to_label(is2g27e),
         is2g27g=to_label(is2g27g),
         is2g27h=to_label(is2g27h)) |>
  dfSummary() |> 
  view()

#Labels values are in the contrary direction

#Change direction of values
base <-base |> 
       mutate_at(vars(starts_with("is2")),
                 ~4-.)

#Test correlation (Now is positive between items and index, and negativa between items and cil)
cor(base)

#What happens if we create manually the index?
base <- base |>
         mutate(s_speceff2=rowMeans(
          base[,4:7]
          ))

cor(base) #Practically is the same index (0.98 alpha)
  

# Fast plot model
base <- base |> 
         mutate(s_sex=to_label(s_sex))

ggplot(base, aes(x=s_pv1cil, y=s_speceff,color=s_sex)) + 
  geom_point()+
  geom_smooth(method=lm)

# At more cil, gender efficacy differences increase

