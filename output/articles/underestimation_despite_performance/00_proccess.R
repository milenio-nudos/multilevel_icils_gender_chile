#No capital in names
bsgchli2 <- bsgchli2%>%clean_names()
bcgchli2 <- bcgchli2%>%clean_names()

#Prepare student data
student_data <- bsgchli2|>
  #Select variables
  select(
    #ID
    idschool,idstud,
    s_sex, #Sex
    s_pv1cil=pv1cil, #CIL
    #ICT Self-efficacies
    s_speceff,s_geneff,
    #Control variables
    s_hisced,s_homlit,
    #Items specific self-efficacy
    is2g27b,is2g27e,is2g27g,is2g27h,
    #Items general self-efficacy
    is2g27a,is2g27c,is2g27d,is2g27i,is2g27j,is2g27k,is2g27l,is2g27m,
    #Weights
    starts_with(c("wg","tot"))
  )

school_data <- bcgchli2|>
  select(
    idschool, #ID
    ip2g04a, #n of girls
    ip2g04b, #n of boys
    #weights
    wgtadj1c,
  )

base <- merge(student_data, school_data, by = "idschool", all.x = TRUE)

#Aggregate CIL school mean and create gender composition
base <- base|>
  group_by(idschool)|>
  mutate(
  c_pv1cil = round(mean(s_pv1cil, na.rm = TRUE), 2),
  c_pv1cil_var = var(s_pv1cil, na.rm = TRUE),
  ip2g04a = ifelse(is.na(ip2g04a), 0, ip2g04a),
  ip2g04b = ifelse(is.na(ip2g04b), 0, ip2g04b),
  c_total_eight_grade = ip2g04a + ip2g04b,
  c_s_female_ratio = ip2g04a/c_total_eight_grade
  )|>
  ungroup()

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
val_label(base$is2g27a, 8) <- NULL
val_label(base$is2g27a, 9) <- NULL
val_label(base$is2g27c, 8) <- NULL
val_label(base$is2g27c, 9) <- NULL
val_label(base$is2g27d, 8) <- NULL
val_label(base$is2g27d, 9) <- NULL
val_label(base$is2g27i, 8) <- NULL
val_label(base$is2g27i, 9) <- NULL
val_label(base$is2g27j, 8) <- NULL
val_label(base$is2g27j, 9) <- NULL
val_label(base$is2g27k, 8) <- NULL
val_label(base$is2g27k, 9) <- NULL
val_label(base$is2g27l, 8) <- NULL
val_label(base$is2g27l, 9) <- NULL
val_label(base$is2g27m, 8) <- NULL
val_label(base$is2g27m, 9) <- NULL

#Change labels geneff items
var_label(base) <- list(
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
var_label(base) <- list(
  is2g27b = "Create a database",
  is2g27e = "Build a webpage",
  is2g27g = "Create a computer program/app",
  is2g27h = "Set up a local area network"
)

# Create gender composition categorical variable
base <- base |>
  mutate(
    c_gender_type = case_when(
      c_s_female_ratio <= 0.33 ~ 1,
      c_s_female_ratio > 0.34 & c_s_female_ratio < 0.66 ~ 2,
      c_s_female_ratio >= 0.67 ~ 3
    ),
    c_gender_type = labelled(c_gender_type,
                             labels = c("Masculinized school (0-33% girls)" = 1, 
                                        "Mixed school (34%-66% girls)" = 2, 
                                        "Feminized school (67%-100% girls)" = 3)
    )
  )

# Recode CIL (*0.1)
base <- base |>
  mutate(s_pv1cil=s_pv1cil*0.1,
         c_pv1cil=c_pv1cil*0.1,
         c_pv1cil_var=c_pv1cil_var*0.1)

label(base[["s_pv1cil"]]) <- "Computer and Information Literacy Score"
label(base[["c_pv1cil"]]) <- "School mean score CIL test"
label(base[["c_gender_type"]]) <- "School gender composition"
label(base[["c_pv1cil_var"]]) <- "School variance score CIL test"

#Create definitive object
data <- base |>
  select(
    #ID's
    idschool, idstud,
    # Level 1
    ## Dependent variables
    s_geneff, s_speceff,
    # Independent variables
    s_sex, s_pv1cil,
    # Control variables
    s_hisced, s_homlit,
    # General self-efficacy items
    is2g27a,is2g27c,is2g27d,is2g27i,is2g27j,is2g27k,is2g27l,is2g27m,
    # Specialized self-efficacy items
    is2g27b,is2g27e,is2g27g,is2g27h,
    # Level 2
    ## CIL mean, CIL variance, Gender composition
    c_pv1cil,c_pv1cil_var,c_gender_type
  )
