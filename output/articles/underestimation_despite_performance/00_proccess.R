
#Select variables
data <- student_proc_2018 |> 
  select(
    #ID
    idschool,idstud,
    #Student level
    s_sex,s_pv1cil,s_speceff,s_geneff,
    #School level
    c_pv1cil,c_s_f_ratio,c_speceff,c_geneff,
    #Control variables
    s_hisced,s_homlit,s_hisei,s_nisb,
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
  is2g27a = "Edit graphic images",
  is2g27c = "Write or edit text",
  is2g27d = "Search and find information on internet",
  is2g27i = "Create multi-media presentation",
  is2g27j = "Upload multimedia to an online profile",
  is2g27k = "Insert an image into a document/message",
  is2g27l = "Install a program/app",
  is2g27m = "Judge internet information veracity"
)

#Change labels speceff items
var_label(data) <- list(
  is2g27b = "Create a database",
  is2g27e = "Build a webpage",
  is2g27g = "Create a computer program/app",
  is2g27h = "Set up a local area network"
)

# Create gender composition categorical variable
data <- data |>
  mutate(
    c_gender_type = case_when(
      c_s_f_ratio <= 0.33 ~ 1,
      c_s_f_ratio > 0.34 & c_s_f_ratio < 0.66 ~ 2,
      c_s_f_ratio >= 0.67 ~ 3
    ),
    c_gender_type = labelled(c_gender_type,
                             labels = c("Masculinized school (0-33% girls)" = 1, 
                                        "Mixed school (34%-66% girls)" = 2, 
                                        "Feminized school (67%-100% girls)" = 3)
    )
  )

# Recode CIL
data <- data |>
  mutate(s_pv1cil=s_pv1cil*0.1,
         c_pv1cil=c_pv1cil*0.1)

label(data[["s_pv1cil"]]) <- "Computer and Information Literacy Score"
label(data[["c_pv1cil"]]) <- "School mean score CIL test"
label(data[["c_gender_type"]]) <- "School gender composition"

class(data$c_gender_type)
