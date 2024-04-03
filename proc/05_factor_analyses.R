pacman::p_load(tidyverse,
               gtsummary,
               sjmisc, #Label manipulation
               labelled, #Label manipulation
               summarytools,
               sjPlot,
               effects,
               #responsePatterms
               stargazer, # Reporte a latex
                sjPlot, sjmisc, # reporte y gráficos
                corrplot, # grafico correlaciones
                xtable, # Reporte a latex
                Hmisc, # varias funciones
                psych, # fa y principal factors
                psy, # scree plot function
                nFactors, # parallel
                GPArotation, devtools, # rotación
               table1,
               xtable,
               apaTables, kableExtra
               )

student_proc_2018 <- readRDS("/Users/daniel/Dropbox (Personal)/github/multilevel_icils_gender_chile/input/proc_data/03_student_proc_2018.rds")

dim(student_proc_2018)

base <- student_proc_2018 |> 
  select(#s_sex,
         #s_speceff,s_geneff,
         #Items specific self-efficacy
         is2g27b,is2g27e,is2g27g,is2g27h,
         #Items general self-efficacy
         is2g27a,is2g27c,is2g27d,is2g27i,is2g27j,is2g27k,is2g27l,is2g27m)

dim(base)
str(base)

#Negative Correlation speceff wit cil, but positive items with cil

cor(base, use="complete.obs") #why?

#Delete missing data labels
#val_label(base$s_sex,8) <- NULL
#val_label(base$s_sex,9) <- NULL
val_label(base$is2g27b,8) <- NULL
val_label(base$is2g27b,9) <- NULL
val_label(base$is2g27e,8) <- NULL
val_label(base$is2g27e,9) <- NULL
val_label(base$is2g27g,8) <- NULL
val_label(base$is2g27g,9) <- NULL
val_label(base$is2g27h,8) <- NULL
val_label(base$is2g27h,9) <- NULL
val_label(base$is2g27a,8) <- NULL
val_label(base$is2g27a,9) <- NULL
val_label(base$is2g27c,8) <- NULL
val_label(base$is2g27c,9) <- NULL
val_label(base$is2g27d,8) <- NULL
val_label(base$is2g27d,9) <- NULL
val_label(base$is2g27i,8) <- NULL
val_label(base$is2g27i,9) <- NULL
val_label(base$is2g27j,8) <- NULL
val_label(base$is2g27j,9) <- NULL
val_label(base$is2g27k,8) <- NULL
val_label(base$is2g27k,9) <- NULL
val_label(base$is2g27l,8) <- NULL
val_label(base$is2g27l,9) <- NULL
val_label(base$is2g27m,8) <- NULL
val_label(base$is2g27m,9) <- NULL

#Descriptive Specialized Self-efficacy items
base |>
  mutate(#s_sex=to_label(s_sex),
         is2g27b=to_label(is2g27b), #database
         is2g27e=to_label(is2g27e), #webpage
         is2g27g=to_label(is2g27g), #app
         is2g27h=to_label(is2g27h), #network
         is2g27a=to_label(is2g27a), #editphoto
         is2g27c=to_label(is2g27c), #edittext
         is2g27d=to_label(is2g27d), #findinfo
         is2g27i=to_label(is2g27i), #multimedia
         is2g27j=to_label(is2g27j), #upload
         is2g27k=to_label(is2g27k), #insertimage
         is2g27l=to_label(is2g27l), #install
         is2g27m=to_label(is2g27m)  #judgeinfo
         ) |>
  dfSummary() |> 
  view()

table1(~is2g27b + is2g27e , data=base, transpose=FALSE)

help(table1)

#Labels values are in the contrary direction

#Change direction of values
base <-base |> 
       mutate_at(vars(starts_with("is2g")),
                 ~4-.)


#Test correlation (Now is positive between items and index, and negativa between items and cil)
mat = round(cor(base, use="complete.obs"),3)
mat

#Hide lower triangle
lower<-mat
lower[lower.tri(mat, diag=TRUE)]<-""
lower<-as.data.frame(lower)

#Table using kableExtra
lower |>
  kable() |>
#kbl(caption = "Recreating booktabs style table") |>
  kable_classic()


#print(xtable(lower), type="html") # to html or pdf


# Tables in apa using apaTables
table1 <- apa.cor.table(base, 
                        #filename = "",
                        table.number = 1,
                        show.conf.interval = FALSE,
                        show.sig.stars = TRUE,
                        #show.pvalue = TRUE,
                        landscape = FALSE)
print(table1)
#help(apa.cor.table)
#apa.save(filename = "table1.doc", table1)

#Polychoric correlations
m=psych::polychoric(base)
polmat=m$rho

#Hide lower triangle
lowerpol<-round(polmat, 3)
lowerpol[lower.tri(polmat, diag=TRUE)]<-""
lowerpol<-as.data.frame(lowerpol)

#Table using kableExtra
lowerpol |>
kable() |>
  kable_classic_2()

#Correlations graph
corrplot(polmat, type="upper") #lower x bajo diagonal

cor.plot(m$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)

save(polmat, file = "polychoric")

# Factor analyses: 3 factors
load("polychoric")
fa.parallel(polmat, fm="pa", fa="fa", main = "Scree Plot", n.obs=3092)

poly_model3 = fa(base, nfactor=3, cor="poly", fm="pa", rotate = "oblimin")

save(poly_model3, file = "poly_model")

poly_model3$loadings

# Cluster analysis plot
fa.diagram(poly_model3)

# Factor analyses: 2 factors
poly_model2 = fa(base, nfactor=2, cor="poly", fm="pa", rotate = "oblimin")

save(poly_model2, file = "poly_model2")

poly_model2$loadings

# Cluster analysis plot
fa.diagram(poly_model2)



