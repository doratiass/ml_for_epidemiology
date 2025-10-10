# ============================================================================ #
# Script: 01 - int.R
# Description: This script imports raw data (in Stata format) and cleans and transforms it
# for the Longevity Predictors Study. It performs several tasks including:
#   - Importing the data and removing attributes.
#   - Creating a cleaned data frame (clean_df) with selected and transformed variables.
#   - Deriving composite variables for BMI, blood pressure, laboratory values, smoking,
#     diabetes, myocardial infarction (MI), angina, cancer, pulmonary measures, and general
#     medical conditions.
#   - Merging these computed datasets with external outcome data.
#   - Saving the final merged dataset for later analysis.
#
# The script is organized in sections with header comments for clarity.
# ============================================================================ #

# ============================================================================ #
# Load Required Packages
# ============================================================================ #
library(haven) # For importing Stata (.dta) files.
library(tidyverse) # Provides functions for data manipulation, transformation, and plotting.
library(lubridate) # Facilitates date manipulation and conversion.
library(Rfast) # Provides fast implementations for certain operations.
library(gtExtras) # Extensions for the gt package for creating tables.

# ------------------------------- #
## Define Utility Functions
# ------------------------------- #
# Function: check_two_vars
# Purpose:
#   - Compares two variables (columns) from a data frame to check if they are equal.
# Arguments:
#   - var_1: A character string indicating the name of the first variable.
#   - var_2: A character string indicating the name of the second variable.
#   - df: The data frame to operate on (default is raw_data).
# Usage:
#   - Selects the two specified columns, creates a new column (eq) that is TRUE if the
#     values are equal, then groups and summarizes the data to count occurrences.
check_two_vars <- function(var_1, var_2, df = raw_data) {
  df %>%
    select((!!sym(var_1)), (!!sym(var_2))) %>%
    mutate(eq = (!!sym(var_1)) == (!!sym(var_2))) %>%
    group_by(eq, (!!sym(var_1)), (!!sym(var_2))) %>%
    summarise(n = n())
}

# ============================================================================ #
# Initial Data Import and Clean ------------------------------------------------
# ============================================================================ #
# Import raw data from a Stata file.
raw_data <- read_dta("raw_data/New_42_yr_but_Shortened_DEC2022.dta")

# Remove all attributes from each column so that subsequent processing is simplified.
raw_data[] <- lapply(raw_data, function(x) {
  attributes(x) <- NULL
  x
})

# Create a cleaned data frame (clean_df) by transforming and recoding variables.
clean_df <- raw_data %>%
  transmute(
    # Identifier variable
    id_nivdaki = nivdaki,

    # Date variables: create dates from year and month columns using lubridate.
    exam_date_63 = make_date(Year_exam, Month_exam),
    death_date = make_date(1900 + shana, hodesh),

    # Demographic and clinical variables with renaming and recoding
    dmg_admission_age = age,
    lab_nonhdl_63 = nonhdl,

    # Ethnicity recoding using factor levels
    dmg_ethnic = factor(
      case_when(
        area == 1 ~ "Israel",
        area == 2 ~ "Europe",
        area == 3 ~ "Europe",
        area == 4 ~ "Balkan",
        area == 5 ~ "Middle East & North Africa",
        area == 6 ~ "Middle East & North Africa"
      ),
      levels = c("Israel", "Europe", "Balkan", "Middle East & North Africa")
    ),

    # Birth date conversion: using a numeric origin.
    dmg_birth_date = as.Date(Exact_birth, origin = "1970-01-01"),
    dmg_birth_year_1 = birthyr,
    dmg_birth_year_2 = Year_Birth,

    # Immigration year with NA handling if value is negative.
    dmg_immigration_year = case_when(
      immigyr < 0 ~ NA,
      TRUE ~ immigyr
    ),

    # Marital status recoding with descriptive labels.
    dmg_martial_status = factor(
      case_when(
        famstat == 1 ~ "Remarried",
        famstat == 2 ~ "Married Once",
        famstat == 3 ~ "Seperated",
        famstat == 4 ~ "Widower",
        famstat == 5 ~ "Bachelor"
      ),
      levels = c(
        "Married Once",
        "Remarried",
        "Seperated",
        "Widower",
        "Bachelor"
      )
    ),

    # Number of children: convert -1 to NA.
    dmg_num_children = ifelse(Number_children == -1, NA, Number_children),

    # Number of people living together and number of rooms: handling negative values.
    dmg_num_pp_living_together = case_when(
      Number_live_together < 0 ~ NA,
      TRUE ~ Number_live_together
    ),
    dmg_num_room = case_when(
      Number_roomd < 0 ~ NA,
      TRUE ~ Number_roomd
    ),

    # Boolean variable for kibbutz status based on specific numeric codes.
    dmg_kibbutz = Number_roomd == -3 | Number_live_together == -3,

    # Work grade recoding to factor with defined levels.
    dmg_work_grade = factor(
      case_when(
        Work_grade == 0 ~ "Administrative",
        Work_grade == 1 ~ "Professional",
        Work_grade == 2 ~ "Laborer",
        Work_grade == 3 ~ "Special",
        Work_grade == 4 ~ "Technicians",
        Work_grade == 8 ~ "Teachers"
      ),
      levels = c(
        "Administrative",
        "Professional",
        "Laborer",
        "Special",
        "Technicians",
        "Teachers"
      )
    ),

    # Salary recoding with descriptive labels.
    dmg_salary = factor(
      case_when(
        Salari == 1 ~ "Basic Salary 345+",
        Salari == 2 ~ "Basic Salary 135-344",
        Salari == 3 ~ "Basic Salary 135-344",
        Salari == 4 ~ "Basic Salary 100-134",
        Salari == 5 ~ "Apprentice 90",
        Salari == 6 ~ "Special",
        Salari == 8 ~ "Special"
      ),
      levels = c(
        "Basic Salary 345+",
        "Basic Salary 135-344",
        "Basic Salary 100-134",
        "Apprentice 90",
        "Special"
      )
    ),

    # Education recoding for self and wife with ordered factor levels.
    dmg_education = factor(
      case_when(
        educ == 1 ~ "No formal school",
        educ == 2 ~ "Partial Elementary",
        educ == 3 ~ "Full Elementary",
        educ == 4 ~ "Partial High School",
        educ == 5 ~ "Full High School",
        educ == 6 ~ "Seminar - no diploma",
        educ == 7 ~ "Seminar - with diploma",
        educ == 8 ~ "Partial University",
        educ == 9 ~ "Full University"
      ),
      ordered = TRUE,
      levels = c(
        "No formal school",
        "Partial Elementary",
        "Full Elementary",
        "Partial High School",
        "Full High School",
        "Seminar - no diploma",
        "Seminar - with diploma",
        "Partial University",
        "Full University"
      )
    ),

    dmg_wife_education = factor(
      case_when(
        wifeedc == 1 ~ "No formal school",
        wifeedc == 2 ~ "Partial Elementary",
        wifeedc == 3 ~ "Full Elementary",
        wifeedc == 4 ~ "Partial High School",
        wifeedc == 5 ~ "Full High School",
        wifeedc == 6 ~ "Seminar - no diploma",
        wifeedc == 7 ~ "Seminar - with diploma",
        wifeedc == 8 ~ "Partial University",
        wifeedc == 9 ~ "Full University"
      ),
      ordered = TRUE,
      levels = c(
        "No formal school",
        "Partial Elementary",
        "Full Elementary",
        "Partial High School",
        "Full High School",
        "Seminar - no diploma",
        "Seminar - with diploma",
        "Partial University",
        "Full University"
      )
    ),

    dmg_wife_occupation = factor(
      case_when(
        q19 == 0 ~ "No Wife",
        q19 == 1 ~ "Housewife",
        q19 == 2 ~ "Additional Work At Home",
        q19 == 3 ~ "Work Outside Home"
      ),
      levels = c(
        "Housewife",
        "No Wife",
        "Additional Work At Home",
        "Work Outside Home"
      )
    ),

    dmg_wife_work_outside = factor(
      case_when(
        q20 == 0 ~ "Housewife or No Wife",
        q20 == 1 ~ "1-9",
        q20 == 2 ~ "10-19",
        q20 == 3 ~ "20-39",
        q20 == 4 ~ "40 or more"
      ),
      ordered = TRUE
    ),

    # Medical variables: weight, height, and BMI computation.
    med_weight_admission = weight,
    med_height_admission = height,
    med_bmi_admission = med_weight_admission / ((med_height_admission / 100)^2),

    # Additional measurements
    med_right_arm_circum_cm = q25,
    med_triceps_thickness_mm = triceps,
    med_subscapular_thickness_mm = Subscapular,

    # History of heart attack and other conditions
    med_past_MI = Previous_Hattack_360men,
    med_past_peptic = ifelse(peptic == 1, TRUE, FALSE),
    med_past_angina_pectoris = factor(case_when(
      ap == 1 ~ "Definite",
      ap == 2 ~ "Suspect",
      ap == 3 ~ "Functional Pain",
      ap == 4 ~ "None of the above",
      ap == 5 ~ "Functional Pain (II)",
      ap == 6 ~ "Not Functional Pain",
      ap == 9 ~ "No diagnosis"
    )),

    med_past_intermittent_claudication = factor(case_when(
      intcld == 1 ~ "IC",
      intcld == 2 ~ "Negative",
      intcld == 9 ~ "Multiple answers"
    )),

    # Smoking variables at exam 63
    med_smoke_63 = factor(case_when(
      smok63 == 1 ~ "1-10",
      smok63 == 2 ~ "11-20",
      smok63 == 3 ~ "20+",
      smok63 == 4 ~ "5 pipes or more",
      smok63 == 5 ~ "less than 5 pipes",
      smok63 == 6 ~ "ex-smoker",
      smok63 == 7 ~ "never smoked"
    )),

    med_sbp_63 = sbp63,
    med_dbp_63 = dbp63,

    # Pulse recoding to ordered factor with specified levels.
    med_puls = factor(
      case_when(
        pulse == 0 ~ "<50",
        pulse == 1 ~ "51-60",
        pulse == 2 ~ "61-70",
        pulse == 3 ~ "71-80",
        pulse == 4 ~ "81-90",
        pulse == 5 ~ "91-100",
        pulse == 6 ~ "101-110",
        pulse == 7 ~ "111-120",
        pulse == 8 ~ "121-130",
        pulse == 9 ~ "131-140",
        pulse == 10 ~ ">141"
      ),
      ordered = TRUE,
      levels = c(
        "<50",
        "51-60",
        "61-70",
        "71-80",
        "81-90",
        "91-100",
        "101-110",
        "111-120",
        "121-130",
        "131-140",
        ">141"
      )
    ),

    med_peripheral_art_dis = factor(case_when(
      perifhd == 1 ~ "Definite",
      perifhd == 2 ~ "Suspect",
      perifhd == 3 ~ "None"
    )),

    med_anxiety = anxiety,

    # Laboratory values at exam 63
    lab_cholesterol_63 = cholesterol63,
    lab_hdl_63 = HDL_1963,
    lab_uacid_63 = uacid63,
    lab_glucose_63 = BloodGlucose1963,
    lab_hemoglobin_63 = Hemoglobin_1963,
    lab_hematocrit_63 = Hematocrit_1963,

    # Other medical history and lifestyle variables
    med_varified_MI = ifelse(verified_Heart_Attack == 1, TRUE, FALSE),
    physical_activity_work = factor(
      case_when(
        physwork == 1 ~ "Mainly Sitting",
        physwork == 2 ~ "Mainly Sitting",
        physwork == 3 ~ "Mainly Standing/Walking",
        physwork == 4 ~ "Mainly Standing/Walking",
        physwork == 5 ~ "Physical Work",
        physwork == 6 ~ "Does not work",
        physwork == 7 ~ "Other activity"
      ),
      levels = c(
        "Mainly Sitting",
        "Mainly Standing/Walking",
        "Physical Work",
        "Does not work",
        "Other activity"
      )
    ),
    physical_activity_after_work = factor(
      case_when(
        leisure == 1 ~ "Almost None",
        leisure == 2 ~ "Sporadic",
        leisure == 3 ~ "Light",
        leisure == 4 ~ "Energetic"
      ),
      ordered = TRUE
    ),

    # Smoking variables at exam 65
    med_smoke_65 = factor(case_when(
      smok65 == 1 ~ "1-10",
      smok65 == 2 ~ "11-20",
      smok65 == 3 ~ "20+",
      smok65 == 4 ~ "5 pipes or more",
      smok65 == 5 ~ "less than 5 pipes",
      smok65 == 6 ~ "never smoked",
      smok65 == 7 ~ "ex-smoker"
    )),
    med_sbp_65 = sbp65,
    med_dbp_65 = dbp65,

    # Laboratory values at exam 65
    lab_glucose_65 = BloodGlucose1965,
    lab_cholesterol_65 = cholesterol65,
    lab_hdl_65 = hdl65,
    lab_nonhdl_65 = nonhdl65,

    lab_blood_group = factor(
      case_when(
        blood == 1 ~ "A",
        blood == 2 ~ "A",
        blood == 3 ~ "A",
        blood == 4 ~ "AB",
        blood == 5 ~ "AB",
        blood == 6 ~ "AB",
        blood == 7 ~ "B",
        blood == 8 ~ "O"
      ),
      levels = c("O", "A", "AB", "B")
    ),

    med_sbp_68 = sbp68,
    med_dbp_68 = dbp68,
    med_vital_capacity_index = vc_index,
    med_fev = fev,

    # Financial and psychosocial variables
    work_past_finance_trouble = factor(
      case_when(
        pafinan == 1 ~ "Very Serious",
        pafinan == 2 ~ "Serious",
        pafinan == 3 ~ "Not Serious",
        pafinan == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    work_present_finance_trouble = factor(
      case_when(
        prfinan == 1 ~ "Very Serious",
        prfinan == 2 ~ "Serious",
        prfinan == 3 ~ "Not Serious",
        prfinan == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    family_past_trouble = factor(
      case_when(
        pafam == 1 ~ "Very Serious",
        pafam == 2 ~ "Serious",
        pafam == 3 ~ "Not Serious",
        pafam == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    family_present_trouble = factor(
      case_when(
        prfam == 1 ~ "Very Serious",
        prfam == 2 ~ "Serious",
        prfam == 3 ~ "Not Serious",
        prfam == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    family_conflict_with_wife = factor(
      case_when(
        conflict_with_wife == 1 ~ "Always show it",
        conflict_with_wife == 2 ~ "Generally show it",
        conflict_with_wife == 3 ~ "Generally keep to himself",
        conflict_with_wife == 4 ~ "Always keep to himself"
      ),
      ordered = TRUE,
      levels = c(
        "Always show i",
        "Generally show it",
        "Generally keep to himself",
        "Always keep to himself"
      )
    ),
    family_hurt_by_wife_child_forget = factor(
      case_when(
        Hurt_by_wife_or_child_forget == 1 ~ "Usually Forget",
        Hurt_by_wife_or_child_forget == 2 ~ "Tend to Forget",
        Hurt_by_wife_or_child_forget == 3 ~ "Tend to Brood",
        Hurt_by_wife_or_child_forget == 4 ~ "Usually Brood"
      ),
      ordered = TRUE,
      levels = c(
        "Usually Forget",
        "Tend to Forget",
        "Tend to Brood",
        "Usually Brood"
      )
    ),
    family_hurt_by_wife_child_retaliate = factor(
      case_when(
        Hurt_by_wife_or_child_retaliate == 1 ~ "Very Often",
        Hurt_by_wife_or_child_retaliate == 2 ~ "Sometimes",
        Hurt_by_wife_or_child_retaliate == 3 ~ "Seldom",
        Hurt_by_wife_or_child_retaliate == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Very Often", "Sometimes", "Seldom", "Never")
    ),
    family_hurt_by_wife_child_restrain_retaliate = factor(
      case_when(
        q48 == 1 ~ "Very Often",
        q48 == 2 ~ "Sometimes",
        q48 == 3 ~ "Seldom",
        q48 == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Very Often", "Sometimes", "Seldom", "Never")
    ),
    family_wife_shows_love = factor(
      case_when(
        wife_shows_life == 1 ~ "Shows it often",
        wife_shows_life == 2 ~ "Shows it seldom",
        wife_shows_life == 3 ~ "Not as much as Id like",
        wife_shows_life == 4 ~ "Never shows it",
        wife_shows_life == 5 ~ "Doesnt love me"
      ),
      ordered = TRUE
    ),
    family_wife_children_listen = factor(
      case_when(
        Wife_children_listen == 1 ~ "Always",
        Wife_children_listen == 2 ~ "Usually",
        Wife_children_listen == 3 ~ "Sometimes",
        Wife_children_listen == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Always", "Usually", "Sometimes", "Never")
    ),
    family_affected_by_family_not_listen = factor(
      case_when(
        Family_not_listening == 1 ~ "Very Upset",
        Family_not_listening == 2 ~ "Little Upset",
        Family_not_listening == 3 ~ "Not Affected",
        Family_not_listening == 4 ~ "Never Happens"
      ),
      ordered = TRUE,
      levels = c("Very Upset", "Little Upset", "Not Affected", "Never Happens")
    ),
    work_past_work_probs = factor(
      case_when(
        past_work_probs == 1 ~ "Very Serious",
        past_work_probs == 2 ~ "Serious",
        past_work_probs == 3 ~ "Not Serious",
        past_work_probs == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("Very Serious", "Serious", "Not Serious", "None")
    ),
    work_present_work_probs = factor(
      case_when(
        present_work_probs == 1 ~ "Very Serious",
        present_work_probs == 2 ~ "Serious",
        present_work_probs == 3 ~ "Not Serious",
        present_work_probs == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("Very Serious", "Serious", "Not Serious", "None")
    ),
    work_try_improve = factor(
      case_when(
        q54 == 1 ~ "Certain to succeed",
        q54 == 2 ~ "Not certain to succeed",
        q54 == 3 ~ "Not trying - no change",
        q54 == 4 ~ "Not trying - satisfied"
      ),
      levels = c(
        "Not trying - no change",
        "Certain to succeed",
        "Not certain to succeed",
        "Not trying - satisfied"
      )
    ),
    work_coworker_like = factor(
      case_when(
        q55 == 1 ~ "Very often",
        q55 == 2 ~ "Sometimes",
        q55 == 3 ~ "Not as much as Id like",
        q55 == 4 ~ "Never",
        q55 == 5 ~ "Dont like me"
      ),
      ordered = TRUE,
      levels = c(
        "Very often",
        "Sometimes",
        "Not as much as Id like",
        "Never",
        "Dont like me"
      )
    ),
    work_supervisor_appreciate = factor(
      case_when(
        q56 == 1 ~ "Very often",
        q56 == 2 ~ "Sometimes",
        q56 == 3 ~ "Not as much as Id like",
        q56 == 4 ~ "Never",
        q56 == 5 ~ "Dont like me"
      ),
      ordered = TRUE,
      levels = c(
        "Very often",
        "Sometimes",
        "Not as much as Id like",
        "Never",
        "Dont like me"
      )
    ),
    work_past_coworker_probs = factor(
      case_when(
        q57 == 1 ~ "Very Serious",
        q57 == 2 ~ "Serious",
        q57 == 3 ~ "Not Serious",
        q57 == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    work_present_coworker_probs = factor(
      case_when(
        q58 == 1 ~ "Very Serious",
        q58 == 2 ~ "Serious",
        q58 == 3 ~ "Not Serious",
        q58 == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    work_past_superior_probs = factor(
      case_when(
        q59 == 1 ~ "Very Serious",
        q59 == 2 ~ "Serious",
        q59 == 3 ~ "Not Serious",
        q59 == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    work_present_superior_probs = factor(
      case_when(
        q60 == 1 ~ "Very Serious",
        q60 == 2 ~ "Serious",
        q60 == 3 ~ "Not Serious",
        q60 == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("None", "Not Serious", "Serious", "Very Serious")
    ),
    work_hurt_by_coworker_forget = factor(
      case_when(
        q61 == 1 ~ "Usually Forget",
        q61 == 2 ~ "Tend to Forget",
        q61 == 3 ~ "Tend to Brood",
        q61 == 4 ~ "Usually Brood"
      ),
      ordered = TRUE,
      levels = c(
        "Usually Forget",
        "Tend to Forget",
        "Tend to Brood",
        "Usually Brood"
      )
    ),
    work_hurt_by_superior_forget = factor(
      case_when(
        q62 == 1 ~ "Usually Forget",
        q62 == 2 ~ "Tend to Forget",
        q62 == 3 ~ "Tend to Brood",
        q62 == 4 ~ "Usually Brood"
      ),
      ordered = TRUE,
      levels = c(
        "Usually Forget",
        "Tend to Forget",
        "Tend to Brood",
        "Usually Brood"
      )
    ),
    work_hurt_by_coworker_shout = factor(
      case_when(
        q63 == 1 ~ "Very Often",
        q63 == 2 ~ "Sometimes",
        q63 == 3 ~ "Seldom",
        q63 == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Never", "Seldom", "Sometimes", "Very Often")
    ),
    work_hurt_by_superior_shout = factor(
      case_when(
        q64 == 1 ~ "Very Often",
        q64 == 2 ~ "Sometimes",
        q64 == 3 ~ "Seldom",
        q64 == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Never", "Seldom", "Sometimes", "Very Often")
    ),
    work_hurt_by_coworker_restrain_retaliate = factor(
      case_when(
        q65 == 1 ~ "Very Often",
        q65 == 2 ~ "Sometimes",
        q65 == 3 ~ "Seldom",
        q65 == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Very Often", "Sometimes", "Seldom", "Never")
    ),
    work_hurt_by_superior_restrain_retaliate = factor(
      case_when(
        q66 == 1 ~ "Very Often",
        q66 == 2 ~ "Sometimes",
        q66 == 3 ~ "Seldom",
        q66 == 4 ~ "Never"
      ),
      ordered = TRUE,
      levels = c("Very Often", "Sometimes", "Seldom", "Never")
    ),

    # Work and family related variables
    work_closed_person = factor(
      case_when(
        Closed_person == 1 ~ "Yes",
        Closed_person == 2 ~ "Generally yes",
        Closed_person == 3 ~ "Somtimes talk",
        Closed_person == 4 ~ "Often talk"
      ),
      ordered = TRUE,
      levels = c("Often talk", "Somtimes talk", "Generally yes", "Yes")
    ),
    family_prob_reach_living_standart = factor(
      case_when(
        Reach_living_Standards == 1 ~ "Very many",
        Reach_living_Standards == 2 ~ "Many",
        Reach_living_Standards == 3 ~ "Some",
        Reach_living_Standards == 4 ~ "None"
      ),
      ordered = TRUE,
      levels = c("Very many", "Many", "Some", "None")
    ),
    family_fight_injustice = factor(case_when(
      DoYou_Fight_justice == 1 ~ "Yes",
      DoYou_Fight_justice == 2 ~ "Cant do Anything",
      DoYou_Fight_justice == 3 ~ "Not personally concerned",
      DoYou_Fight_justice == 4 ~ "Never came across it"
    )),
    diet_type = factor(
      case_when(
        on_Diet_Which_kind == 0 ~ "Not On Diet",
        on_Diet_Which_kind == 1 ~ "Slimming",
        on_Diet_Which_kind == 2 ~ "CHD",
        on_Diet_Which_kind == 3 ~ "HTN",
        on_Diet_Which_kind == 4 ~ "DM",
        on_Diet_Which_kind == 5 ~ "Ulcer",
        on_Diet_Which_kind == 6 ~ "Fat Reduction",
        on_Diet_Which_kind == 7 ~ "7",
        on_Diet_Which_kind == 8 ~ "8"
      ),
      levels = c(
        "Not On Diet",
        "Slimming",
        "CHD",
        "HTN",
        "DM",
        "Ulcer",
        "Fat Reduction",
        "7",
        "8"
      )
    ),

    # Diet and nutritional variables with TODO notes for further completion
    diet_after_main_meal_activity = factor(After_main_mail),
    diet_after_second_main_meal_activity = factor(After_second_mail),
    diet_food_snacks = factor(Food_snacks),
    diet_drink_snacks = factor(Drink_snacks),
    diet_main_meal_work = factor(Main_meal_work),
    diet_main_meal_home = factor(Main_meal_home),
    diet_main_meal_elsewhere = factor(Main_meal_elsewhere),

    med_revised_MI_hx = ifelse(q88 == 1, TRUE, FALSE),
    diet_data_useable = case_when(
      diet == 1 ~ FALSE,
      diet == 0 ~ TRUE
    ),

    # Diet consumption and nutrient intake variables
    diet_gms_protein_wk = protein,
    diet_gms_animal_protein_wk = anprot,
    diet_gms_tot_fat_wk = totfat,
    diet_gms_tot_sat_fat_wk = satfat,
    diet_oleic = oleic,
    diet_linoleic = linoleic,
    diet_tot_cho = totcho,
    diet_cho_strach = chostarc,
    diet_calories_eggs_wk = eggscal,
    diet_calories_bread_wk = breadcal,
    diet_calories_suger_wk = sugarcal,
    diet_calories_potatoes_wk = potatcal,
    diet_calories_rice_wk = ricecal,
    diet_calories_cereal_wk = cerlcal,
    diet_tot_calories_wk = totcal,

    # Death and cancer-related variables
    death_reason_70 = factor(case_when(
      siba70 == 1 ~ "MI",
      siba70 == 2 ~ "presumed MI",
      siba70 == 3 ~ "CVA",
      siba70 == 4 ~ "Malignancy",
      siba70 == 5 ~ "Trauma",
      siba70 == 6 ~ "Other",
      siba70 == 9 ~ "Unknown"
    )),
    death_reason_86 = siba86,
    med_cancer_diagnosis = yrdiag,
    med_cancer_86 = make_date(sumyear, summnth),
    death_all_cause_86 = ifelse(mort == 1, TRUE, FALSE),
    med_diabetes_at_admission_63 = ifelse(diabprev == 1, TRUE, FALSE),
    death_date_86 = as_date(date_die),
    death_timetoevent_86 = follow_up,
    med_cancer_68 = cancer,

    death_stroke_86 = ifelse(dstrok == 1, TRUE, FALSE),
    med_ihd_unk = ifelse(ihd == 1, TRUE, FALSE),
    med_dm_test_68 = factor(case_when(
      alldiab == 0 ~ "All DM tests Negative",
      alldiab == 2 ~ "Abnormal GTT",
      alldiab == 1 ~ "Any postive test out 3"
    )),

    exam_date_65 = make_date(Year_exam65, Month_exam65),
    med_weight_65 = weight65,
    med_bmi_65 = med_weight_65 / ((med_height_admission / 100)^2),
    med_shoulder_measure_cm_biacromial = case_when(
      q112 < 0 ~ NA,
      TRUE ~ q112
    ),
    med_pelvic_measure_cm_bicristal = case_when(
      q113 < 0 ~ NA,
      TRUE ~ q113
    ),
    med_left_eye_pressure = case_when(
      q114 < 0 ~ NA,
      TRUE ~ q114
    ),
    med_right_eye_pressure = case_when(
      q115 < 0 ~ NA,
      TRUE ~ q115
    ),
    med_vital_capacity = case_when(
      q116 < 0 ~ NA,
      TRUE ~ q116
    ),
    med_timed_vital_capacity = case_when(
      q117 < 0 ~ NA,
      TRUE ~ q117
    ),
    med_past_diabetes_65 = factor(case_when(
      q118 == 1 ~ "Yes, new case",
      q118 == 2 ~ "Yes, old case",
      q118 == 3 ~ "Negative"
    )),
    med_past_MI_65 = factor(case_when(
      q119 == 1 ~ "Yes, new case",
      q119 == 2 ~ "Yes, old case",
      q119 == 3 ~ "Negative"
    )),
    med_takes_any_medication_65 = case_when(
      q120 == 2 ~ TRUE,
      q120 == 1 ~ FALSE
    ),
    med_past_angina_pectoris_65 = factor(case_when(
      q121 == 1 ~ "Definite AP",
      q121 == 2 ~ "Suspect AP",
      q121 == 3 ~ "Undefined chest pain",
      q121 == 4 ~ "No Chest Pain",
      q121 == 5 ~ "Secondary AP",
      q121 == 6 ~ "Susp. Sec. AP",
      q121 == 7 ~ "Sec. undefined Chest Pain"
    )),
    med_past_intermittent_claudication_65 = factor(case_when(
      q122 == 1 ~ "Positive",
      q122 == 2 ~ "Negative",
      q122 == 3 ~ "No Diagnosis"
    )),
    med_current_smoke_65 = factor(case_when(
      Current_smoking_2 == 1 ~ "1-10",
      Current_smoking_2 == 2 ~ "11-20",
      Current_smoking_2 == 3 ~ "20+",
      Current_smoking_2 == 4 ~ "5 pipes or more",
      Current_smoking_2 == 5 ~ "less than 5 pipes",
      Current_smoking_2 == 6 ~ "never smoked",
      Current_smoking_2 == 7 ~ "ex-smoker"
    )),
    med_past_smoke_65 = factor(case_when(
      Previous_smoking_2 == 1 ~ "1-10",
      Previous_smoking_2 == 2 ~ "11-20",
      Previous_smoking_2 == 3 ~ "20+",
      Previous_smoking_2 == 4 ~ "5 pipes or more",
      Previous_smoking_2 == 5 ~ "less than 5 pipes",
      Previous_smoking_2 == 6 ~ "never smoked",
      Previous_smoking_2 == 7 ~ "Smoker Now"
    )),
    med_when_stoped_smoke_65 = factor(case_when(
      When_Discont_smoking_2 == 1 ~ "In the past 2 years",
      When_Discont_smoking_2 == 2 ~ "2-5 years",
      When_Discont_smoking_2 == 3 ~ "Over 5 years ago",
      When_Discont_smoking_2 == 6 ~ "Never Smoked",
      When_Discont_smoking_2 == 7 ~ "Current Smoker"
    )),
    med_diabetes_details_63 = factor(case_when(
      q166 == 0 ~ "Normal",
      q166 == 1 ~ "Definite diabetes",
      q166 == 2 ~ "Definite diabetes",
      q166 == 3 ~ "Probable diabetes",
      q166 == 4 ~ "Definite diabetes",
      q166 == 5 ~ "Possible diabetes",
      q166 == 6 ~ "Probable diabetes",
      q166 == 7 ~ "Abnormal GTT",
      q166 == 8 ~ "Possible diabetes",
      q166 == 9 ~ "Abnormal GTT",
      q166 == 10 ~ "Normal GTT",
      q166 == 11 ~ "Inadequate data",
      q166 == 12 ~ "Not a suspect"
    )),
    med_diabetes_summary_63 = factor(case_when(
      q167 == 0 ~ "Negative",
      q167 == 1 ~ "Diabetes",
      q167 == 2 ~ "Abnormal GTT"
    )),
    med_diabetes_details_66 = factor(case_when(
      q168 == 0 ~ "Normal",
      q168 == 1 ~ "Definite diabetes",
      q168 == 2 ~ "Definite diabetes",
      q168 == 3 ~ "Probable diabetes",
      q168 == 4 ~ "Definite diabetes",
      q168 == 5 ~ "Possible diabetes",
      q168 == 6 ~ "Probable diabetes",
      q168 == 7 ~ "Abnormal GTT",
      q168 == 8 ~ "Possible diabetes",
      q168 == 9 ~ "Abnormal GTT",
      q168 == 10 ~ "Normal GTT",
      q168 == 11 ~ "Inadequate data",
      q168 == 12 ~ "Not a suspect"
    )),
    med_diabetes_summary_66 = factor(case_when(
      q169 == 0 ~ "Negative",
      q169 == 1 ~ "Diabetes",
      q169 == 2 ~ "Abnormal GTT"
    )),
    med_diabetes_history_63 = factor(case_when(
      q170 == 0 ~ "Negative",
      q170 == 1 ~ "Positive"
    )),
    unk_month_of_illnes_unk = make_date(1900 + q176, q175),
    unk_outcome_of_illnes_unk = factor(case_when(
      q178 == 1 ~ "Alive",
      q178 == 2 ~ "Dead"
    )),
    med_conclusion_MI = factor(case_when(
      q199 == 0 ~ "No MI",
      q199 == 1 ~ "MI diagnosis based on ECG",
      q199 == 2 ~ "MI diagnosis based on Clinic and lab insufficient ECG",
      q199 == 3 ~ "Physician diagnosis of MI not accepted",
      q199 == 9 ~ "Physician diagnosis of MI not enough data"
    )),
    death_date_68 = make_date(1900 + q214, q213),
    death_sudden_68 = factor(case_when(
      q224 == 0 ~ "No",
      q224 == 1 ~ "Less than 1h",
      q224 == 2 ~ "1-24 hours"
    )),
    death_time_occurrence_since_MI_68 = factor(case_when(
      q225 == 1 ~ "day to week",
      q225 == 2 ~ "1-3 weeks",
      q225 == 3 ~ "3-6 weeks",
      q225 == 4 ~ "6 weeks to 4 months"
    )),
    death_autopsy_68 = factor(case_when(
      q226 == 1 ~ "Yes",
      q226 == 0 ~ "No"
    )),
    death_reason_conculsion_68 = factor(case_when(
      q231 == 1 ~ "MI",
      q231 == 2 ~ "presumed MI",
      q231 == 3 ~ "CVA",
      q231 == 4 ~ "Malignancy",
      q231 == 5 ~ "Trauma",
      q231 == 6 ~ "Other"
    )),
    exam_date_68 = make_date(Year_exam68, Month_exam68),
    med_past_diabetes_68 = factor(case_when(
      q274 == 0 ~ "None",
      q274 == 1 ~ "Since exam II",
      q274 == 2 ~ "Previously"
    )),
    med_past_HA_coronary_68 = factor(case_when(
      q275 == 0 ~ "None",
      q275 == 1 ~ "Since exam II",
      q275 == 2 ~ "Previously"
    )),
    med_past_HTN_68 = factor(case_when(
      q276 == 0 ~ "None",
      q276 == 1 ~ "Since exam II",
      q276 == 2 ~ "Previously"
    )),
    med_past_peptic_68 = factor(case_when(
      q277 == 0 ~ "None",
      q277 == 1 ~ "Since exam II",
      q277 == 2 ~ "Previously"
    )),
    med_past_respiratory_68 = factor(case_when(
      q278 == 0 ~ "None",
      q278 == 1 ~ "Since exam II",
      q278 == 2 ~ "Previously"
    )),
    med_past_kidney_68 = factor(case_when(
      q279 == 0 ~ "None",
      q279 == 1 ~ "Since exam II",
      q279 == 2 ~ "Previously"
    )),
    med_past_htn_medication_68 = case_when(
      q280 == 1 ~ TRUE,
      q280 == 0 ~ FALSE
    ),
    med_past_insulin_68 = case_when(
      q281 == 1 ~ TRUE,
      q281 == 0 ~ FALSE
    ),
    med_past_angina_pectoris_68 = factor(case_when(
      q282 == 1 ~ "Definite",
      q282 == 2 ~ "Suspect",
      q282 == 3 ~ "Undefined chest pain",
      q282 == 4 ~ "No Chest Pain",
      q282 == 5 ~ "Emotional functional chest pain",
      q282 == 6 ~ "Susp. Emotional functional chest pain",
      q282 == 7 ~ "Undefined chest pain"
    )),
    med_past_intermittent_claudication_68 = factor(case_when(
      q283 == 1 ~ "Present",
      q283 == 2 ~ "Not present"
    )),
    med_past_lungs_68 = factor(case_when(
      q284 == 0 ~ "NAD",
      q284 == 1 ~ "Congested ling bases",
      q284 == 2 ~ "Asthma or bronchitis",
      q284 == 3 ~ "Other"
    )),
    med_past_limbs_68 = factor(case_when(
      q285 == 0 ~ "NAD",
      q285 == 1 ~ "Hemiplegia",
      q285 == 2 ~ "Other"
    )),
    med_sbp_start_68 = case_when(
      q291 < 0 ~ NA,
      TRUE ~ q291
    ),
    med_dbp_start_68 = case_when(
      q292 < 0 ~ NA,
      TRUE ~ q292
    ),
    med_sbp_end_68 = case_when(
      q286 < 0 ~ NA,
      TRUE ~ q286
    ),
    med_dbp_end_68 = case_when(
      q287 < 0 ~ NA,
      TRUE ~ q287
    ),
    med_weight_68 = weight68,
    med_bmi_68 = med_weight_68 / ((med_height_admission / 100)^2),
    med_ecg_68 = factor(case_when(
      q290 == 0 ~ "Not Taken",
      q290 == 1 ~ "Paper Only",
      q290 == 2 ~ "Paper and Tape"
    )),
    med_fvc_68 = case_when(
      q293 < 0 ~ NA,
      TRUE ~ q293
    ),

    # Additional diabetes variables commented out.

    med_past_peptic_summary = factor(case_when(
      q373 == 0 ~ "well",
      q373 == 1 ~ "Incidence",
      q373 == 8 ~ "Not at risk"
    )),
    dmg_origin = factor(
      case_when(
        Origin_2 == 1 ~ "Israel",
        Origin_2 == 2 ~ "East europe",
        Origin_2 == 3 ~ "Central europe",
        Origin_2 == 4 ~ "South europe",
        Origin_2 == 5 ~ "Asia",
        Origin_2 == 6 ~ "North america",
        Origin_2 == 7 ~ "Israel",
        Origin_2 == 8 ~ "Israel",
        Origin_2 == 9 ~ "Israel"
      ),
      levels = c(
        "Israel",
        "East europe",
        "Central europe",
        "South europe",
        "Asia",
        "North america"
      )
    ),
    physical_activity = factor(
      case_when(
        q377 == 1 ~ "Lowest",
        q377 == 2 ~ "Low",
        q377 == 3 ~ "Middle",
        q377 == 4 ~ "High",
        q377 == 5 ~ "Highest"
      ),
      ordered = TRUE,
      levels = c("Lowest", "Low", "Middle", "High", "Highest")
    ),

    dmg_rlgeduc = factor(case_when(
      rlgeduc == 1 ~ "Only Religious",
      rlgeduc == 2 ~ "Religious and Secular",
      rlgeduc == 3 ~ "Only Secular",
      rlgeduc == 4 ~ "didn't learn or no category was checked"
    )),
    dmg_rlgself = factor(
      case_when(
        rlgself == 1 ~ "Most Religious",
        rlgself == 2 ~ "Less Religious",
        rlgself == 3 ~ "Traditional",
        rlgself == 4 ~ "Secular",
        rlgself == 5 ~ "Agnostic"
      ),
      ordered = TRUE,
      levels = c(
        "Agnostic",
        "Secular",
        "Traditional",
        "Less Religious",
        "Most Religious"
      )
    ),

    dmg_income_inx_source_salary = ifelse(q383 != 0 & q383 != 9, TRUE, FALSE),
    dmg_income_inx_source_extra_work = ifelse(
      q383 == 2 | q383 == 5 | q383 == 6 | q383 == 8,
      TRUE,
      FALSE
    ),
    dmg_income_inx_source_additional = ifelse(
      q383 == 3 | q383 == 5 | q383 == 7 | q383 == 8,
      TRUE,
      FALSE
    ),
    dmg_income_inx_source_wife = ifelse(
      q383 == 4 | q383 == 6 | q383 == 7 | q383 == 8,
      TRUE,
      FALSE
    ),
    dmg_income_inx_source_other = ifelse(q383 == 9, TRUE, FALSE),
    dmg_income_inx_total = factor(
      case_when(
        q384 == 0 ~ "No Income",
        q384 == 1 ~ "<29",
        q384 == 2 ~ "30-44",
        q384 == 3 ~ "45-59",
        q384 == 4 ~ "60-79",
        q384 == 5 ~ "80-99",
        q384 == 6 ~ "100-149",
        q384 == 7 ~ "150-199",
        q384 == 8 ~ "200-249",
        q384 == 9 ~ "250<"
      ),
      levels = c(
        "No Income",
        "<29",
        "30-44",
        "45-59",
        "60-79",
        "80-99",
        "100-149",
        "150-199",
        "200-249",
        "250<"
      ),
      ordered = TRUE
    ),

    dmg_father_birth_country = factor(q385),
    dmg_residenses_num_after_start_work = q386,
    dmg_countries_num_after_start_work = q387,

    work_satisfaction = work_satisfaction,
    work_pressure = q389,
    family_private_life_satisfaction = q390,
    work_durable_goods = q391,
    dmg_parent_class_discrepancy = case_when(
      q392 == 77 ~ 7,
      q392 > -8 ~ q392
    ),
    unk_occupational_pattern_code = q393,
    unk_occupational_pattern_index = q394,
    unk_effort_disappointment_trouble_work = factor(case_when(
      q395 == 0 ~ "None",
      q395 == 1 ~ "Only one of three",
      q395 == 2 ~ "",
      q395 == 3 ~ "",
      q395 == 4 ~ "",
      q395 == 5 ~ "",
      q395 == 6 ~ "",
      q395 == 7 ~ "",
      q395 == 8 ~ "",
      q395 == 9 ~ "All three"
    )),
    unk_effort_disappointment_trouble_married_life = factor(case_when(
      q396 == 0 ~ "None",
      q396 == 1 ~ "Only one of three",
      q396 == 2 ~ "",
      q396 == 3 ~ "",
      q396 == 4 ~ "",
      q396 == 5 ~ "",
      q396 == 6 ~ "",
      q396 == 7 ~ "",
      q396 == 8 ~ "",
      q396 == 9 ~ "All three"
    )),
    unk_effort_disappointment_trouble_standard_life = factor(case_when(
      q397 == 0 ~ "None",
      q397 == 1 ~ "Only one of three",
      q397 == 2 ~ "",
      q397 == 3 ~ "",
      q397 == 4 ~ "",
      q397 == 5 ~ "",
      q397 == 6 ~ "",
      q397 == 7 ~ "",
      q397 == 8 ~ "",
      q397 == 9 ~ "All three"
    )),
    unk_effort_disappointment_trouble_social_life = factor(case_when(
      q398 == 0 ~ "None",
      q398 == 1 ~ "Only one of three",
      q398 == 2 ~ "",
      q398 == 3 ~ "",
      q398 == 4 ~ "",
      q398 == 5 ~ "",
      q398 == 6 ~ "",
      q398 == 7 ~ "",
      q398 == 8 ~ "",
      q398 == 9 ~ "All three"
    )),
    unk_profession = profess,
    unk_profession_other = q399,
    dmg_birth_order = factor(
      case_when(
        q400 == 1 ~ "1st born",
        q400 == 2 ~ "Neither 1sr nor last",
        q400 == 3 ~ "Last born",
        q400 == 4 ~ "One in pair of twins"
      ),
      levels = c(
        "1st born",
        "Neither 1sr nor last",
        "Last born",
        "One in pair of twins"
      )
    ),
    dmg_crisis_father_death = factor(
      case_when(
        q401 == 1 ~ "No crisis",
        q401 > 1 ~ "Crisis"
      ),
      levels = c("No crisis", "Crisis")
    ),
    dmg_crisis_mother_death = factor(
      case_when(
        q402 == 1 ~ "No crisis",
        q402 > 1 ~ "Crisis"
      ),
      levels = c("No crisis", "Crisis")
    ),
    dmg_concentration_camp = factor(
      case_when(
        q403 == 1 ~ "Never was",
        q403 != 1 ~ "Was"
      ),
      levels = c("Never was", "Was")
    ),
    work_family_interested = factor(case_when(
      q404 == 1 ~ "Yes",
      q404 == 2 ~ "Not very much",
      q404 == 3 ~ "Only in certain aspects",
      q404 == 4 ~ "No",
      q404 == 5 ~ "No wife"
    )),
    dmg_success_marriage = factor(
      case_when(
        q405 == 1 ~ "Very successful",
        q405 == 2 ~ "Rather successful",
        q405 == 3 ~ "Not so successful",
        q405 == 4 ~ "Unsuccessful",
        q405 == 5 ~ "No wife"
      ),
      levels = c(
        "Very successful",
        "Rather successful",
        "Not so successful",
        "Unsuccessful",
        "No wife"
      )
    ),
    work_station = factor(
      case_when(
        station == 1 ~ "Jerusalem",
        station == 2 ~ "Tel Aviv",
        station == 3 ~ "Haifa"
      ),
      levels = c("Tel Aviv", "Jerusalem", "Haifa")
    ),
    comp_ihd_incidence = case_when(
      ihd_wide == 0 ~ FALSE,
      ihd_wide == 1 ~ TRUE
    ),
    unk_never_discuss = never_discuss,
    comp_wt_change_63_65 = factor(case_when(
      wt_change == 1 ~ "increased serially",
      wt_change == 2 ~ "decreased serially",
      wt_change == 3 ~ "3",
      wt_change == 4 ~ "4"
    )),
    comp_no_changewt = no_changewt,
    comp_incr_changewt = incr_changewt,
    comp_decl_changewt = decl_changewt,
    comp_wtlow_high = wtlow_high,
    comp_wthigh_low = wthigh_low,
    comp_fluctuate = fluctuate,
    diet_pcpoly = pcpoly,
    comp_CHD63_wide = CHD63_wide,
    comp_SBPdiff = SBPdiff,
    comp_MedianSBPdiff = MedianSBPdiff,
    death_died_all_2006 = died_all,
    death_age_died_2006 = age_died,
    death_year_died_2006 = year_died,
    comp_reach_80_2006 = reach_80,
    comp_reach_85_2006 = reach_85,
    comp_max_age_2006 = max_age,
    comp_Dwt68_63 = Dwt68_63,
    unk_Stroke_42yr = Stroke_42yr,
    comp_HighBP63 = factor(case_when(
      HighBP63 == 1 ~ ">140",
      HighBP63 == 0 ~ "140<"
    )),
    comp_died_strok = died_strok,
    comp_agnostic = `_agnostic_`,
    dmg_SES = SES_4cat,
    comp_Metasynd = Metasynd,
    comp_num_Metasynd = num_Metasynd,
    comp_trunk_periph_fat = trunk_periph_fat,
    comp_Trunk_quartile = Trunk_quartile,
    comp_father_crisis = father_crisis,
    comp_mother_crisis = mother_crisis,
    comp_parent_crisis = parent_crisis,
    dmg_concentration = case_when(
      concentration == 0 ~ FALSE,
      concentration == 1 ~ TRUE
    ),
    comp_year_imm_group = factor(case_when(
      year_imm_group == 1 ~ "<1935",
      year_imm_group == 2 ~ "1936-48",
      year_imm_group == 3 ~ "1949-50",
      year_imm_group == 4 ~ ">1950"
    )),
    comp_restrictive_lung = restrictive_lung,
    comp_obstructive_lung = obstructive_lung,
    comp_Lung_ratio = Lung_ratio,
    comp_Lungs_anyside = Lungs_anyside,
    comp_Forced_VC = Forced_VC,
    comp_FEV_FVC_ratio = FEV_FVC_ratio,
    comp_FEVtert = FEVtert,
    comp_tert_ratio_1 = tert_ratio_1,
    comp_satisfaction_marital = satisfaction_marital,
    comp_prctfat_quart = prctfat_quart,
    comp_meanChol_40 = meanChol_40,
    comp_bicristal = bicristal,
    comp_acrom_crist_ratio = acrom_crist_ratio,
    comp_acrom_quart = acrom_quart,
    comp_incid_MI = incid_MI,
    comp_generation = generation,
    death_died_97 = died_97,
    comp_cal_oleic = cal_oleic,
    comp_PP63 = PP63,
    comp_Rel_PP_index63 = Rel_PP_index63,
    comp_sedentary = sedentary,
    comp_seperate_widower = seperate_widower,
    comp_MAP63 = MAP63,
    comp_fathercrisis_rerank = fathercrisis_rerank,
    comp_alive_asses = alive_asses,
    comp_diameter_ratio_quart = diameter_ratio_quart,
    comp_Holocaust = Holocaust,
    comp_HDL_percent = HDL_percent,
    comp_Concent_Europe = Concent_Europe,
    comp_nonhdl65_Gr = nonhdl65_Gr,
    comp_CKD = CKD,
    unk_examined_1965 = examined_1965,
    unk_holocaust_Threeway = holocaust_Threeway,
    death_died_CHD_97 = died_CHD_97,
    comp_COPD = COPD,
    comp_RENAL_chronic = RENAL_chronic,
    comp_Type_MI_Incidence = Type_MI_Incidence,
    comp_unrecognized_MI = unrecognized_MI,
    comp_Incid_AP = Incid_AP,
    comp_forced_pv = forced_pv,
    comp_Pastsmok_65 = Pastsmok_65,
    comp_Discont_65 = Discont_65,
    comp_Visit_by_SBP = Visit_by_SBP,
    FAMPROB = FAMPROB
  ) %>%
  filter(!is.na(id_nivdaki)) # Filter out rows with missing identifier.

# ============================================================================ #
# Composite Variables and Subsets ----------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## BMI and Blood Pressure Summary ---------------------------------------------
# ---------------------------------------------------------------------------- #
# Identify variable names for weight, BMI, SBP, DBP, and height based on pattern matching.
weigh_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "med_weight")]
bmi_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "med_bmi")]
sbp_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "med_sbp")]
dbp_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "med_dbp")]
height_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "med_height")]
bmi_sbp_vars <- c(weigh_vars, bmi_vars, height_vars, sbp_vars, dbp_vars)

# Calculate summary statistics such as mean, standard deviation, and range.
bmi_sbp_data <- clean_df %>%
  rowwise() %>%
  mutate(
    med_sbp_mean = mean(c_across(all_of(sbp_vars)), na.rm = TRUE),
    med_sbp_sd = sd(c_across(all_of(sbp_vars)), na.rm = TRUE),
    med_dbp_mean = mean(c_across(all_of(dbp_vars)), na.rm = TRUE),
    med_dbp_sd = sd(c_across(all_of(dbp_vars)), na.rm = TRUE),
    med_weight_range = case_when(
      !is.na(med_weight_admission) & !is.na(med_weight_68) ~
        med_weight_admission - med_weight_68,
      !is.na(med_weight_admission) & !is.na(med_weight_65) ~
        med_weight_admission - med_weight_65,
      !is.na(med_weight_65) & !is.na(med_weight_68) ~
        med_weight_65 - med_weight_68,
      TRUE ~ 0
    ),
    med_weight_mean = mean(c_across(all_of(weigh_vars)), na.rm = TRUE),
    med_bmi_sd = sd(c_across(all_of(bmi_vars)), na.rm = TRUE),
    med_bmi_mean = mean(c_across(all_of(bmi_vars)), na.rm = TRUE),
    med_height_mean = mean(c_across(all_of(height_vars)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(
    id_nivdaki,
    med_sbp_mean,
    med_sbp_sd,
    med_dbp_mean,
    med_dbp_sd,
    med_weight_mean,
    med_weight_range,
    med_bmi_mean,
    med_bmi_sd,
    med_height_mean
  )

# ---------------------------------------------------------------------------- #
## Laboratory Data Summary ----------------------------------------------------
# ---------------------------------------------------------------------------- #
# Select laboratory variables (excluding lab_hdl_63) and calculate summary statistics.
labs_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "lab")]
labs_vars <- labs_vars[!(labs_vars %in% c("lab_hdl_63"))]
labs_data <- clean_df %>%
  rowwise() %>%
  transmute(
    id_nivdaki,
    lab_mean_cholesterol = mean(
      c_across(starts_with("lab_cholesterol")),
      na.rm = TRUE
    ),
    lab_range_cholesterol = case_when(
      !is.na(lab_cholesterol_65) & !is.na(lab_cholesterol_63) ~
        lab_cholesterol_63 - lab_cholesterol_65,
      TRUE ~ 0
    ),
    lab_mean_nonhdl = mean(c_across(starts_with("lab_nonhdl")), na.rm = TRUE),
    lab_range_nonhdl = case_when(
      !is.na(lab_nonhdl_65) & !is.na(lab_nonhdl_63) ~
        lab_nonhdl_63 - lab_nonhdl_65,
      TRUE ~ 0
    ),
    lab_mean_hdl = mean(c_across(starts_with("lab_hdl")), na.rm = TRUE),
    lab_glucose = lab_glucose_65,
    lab_hemoglobin = lab_hemoglobin_63,
    lab_hematocrit = lab_hematocrit_63,
    lab_uacid = lab_uacid_63,
    lab_blood_group
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
## Smoking Data ---------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Identify smoking-related variables and recode them into numeric scores and categories.
smoke_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "smok")]
smoke_data <- clean_df %>%
  select(id_nivdaki, all_of(smoke_vars)) %>%
  mutate(
    num_63 = case_when(
      med_smoke_63 == "never smoked" ~ 0,
      med_smoke_63 == "ex-smoker" ~ 0,
      med_smoke_63 == "1-10" ~ 1,
      med_smoke_63 == "11-20" ~ 2,
      med_smoke_63 == "20+" ~ 3,
      med_smoke_63 == "less than 5 pipes" ~ 2,
      med_smoke_63 == "5 pipes or more" ~ 3
    ),
    num_65 = case_when(
      med_smoke_65 == "never smoked" ~ 0,
      med_smoke_65 == "ex-smoker" ~ 0,
      med_smoke_65 == "1-10" ~ 1,
      med_smoke_65 == "11-20" ~ 2,
      med_smoke_65 == "20+" ~ 3,
      med_smoke_65 == "less than 5 pipes" ~ 2,
      med_smoke_65 == "5 pipes or more" ~ 3
    ),
    diff = num_65 - num_63,
    med_smoke_status = ifelse(
      !is.na(med_smoke_63),
      as.character(med_smoke_63),
      as.character(med_smoke_65)
    ),
    med_smoke_status = factor(
      case_when(
        med_smoke_status == "less than 5 pipes" ~ "11-20",
        med_smoke_status == "5 pipes or more" ~ "20+",
        TRUE ~ as.character(med_smoke_status)
      ),
      levels = c("never smoked", "ex-smoker", "1-10", "11-20", "20+")
    ),
    med_smoke_change = factor(
      case_when(
        diff > 0 ~ "Increased",
        diff < 0 ~ "Decreased",
        diff == 0 ~ "No change"
      ),
      levels = c("No change", "Decreased", "Increased")
    )
  ) %>%
  select(id_nivdaki, med_smoke_status, med_smoke_change)

# ---------------------------------------------------------------------------- #
## Diabetes Data ---------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Select diabetes-related variables and create a summary variable for diabetes status.
diabetes_vars <- c(
  colnames(clean_df)[str_detect(colnames(clean_df), "diabetes")],
  "med_dm_test_68"
)
diabetes_data <- clean_df %>%
  select(id_nivdaki, all_of(diabetes_vars)) %>%
  mutate(
    med_diabetes_status = factor(case_when(
      med_diabetes_summary_63 == "Diabetes" |
        med_diabetes_summary_66 == "Diabetes" ~
        "Diabetes",
      med_diabetes_summary_63 == "Abnormal GTT" |
        med_diabetes_summary_66 == "Abnormal GTT" ~
        "Abnormal GTT",
      med_diabetes_summary_63 == "Negative" |
        med_diabetes_summary_66 == "Negative" ~
        "Negative"
    )),
    num_63 = case_when(
      med_diabetes_summary_63 == "Negative" ~ 0,
      med_diabetes_summary_63 == "Abnormal GTT" ~ 1,
      med_diabetes_summary_63 == "Diabetes" ~ 2
    ),
    num_65 = case_when(
      med_diabetes_summary_66 == "Negative" ~ 0,
      med_diabetes_summary_66 == "Abnormal GTT" ~ 1,
      med_diabetes_summary_66 == "Diabetes" ~ 2
    ),
    med_diabetes_new = num_65 - num_63 > 0
  ) %>%
  transmute(
    id_nivdaki,
    med_dm = factor(
      case_when(
        med_diabetes_status == "Diabetes" ~ "Present",
        med_diabetes_status == "Abnormal GTT" ~ "Present",
        med_diabetes_at_admission_63 == TRUE ~ "Present",
        med_diabetes_new == TRUE ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    )
  )

# ---------------------------------------------------------------------------- #
## MI Data --------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Create a summary variable for myocardial infarction (MI) status by combining past MI,
# verified MI, new MI (including unrecognized) and IHD variables.
mi_vars <- c(
  colnames(clean_df)[str_detect(colnames(clean_df), "MI")],
  colnames(clean_df)[str_detect(colnames(clean_df), "ihd")],
  "med_past_HA_coronary_68",
  "comp_CHD63_wide"
)
mi_data <- clean_df %>%
  select(id_nivdaki, all_of(mi_vars)) %>%
  mutate(
    med_past_MI = ifelse(med_past_MI == 1, TRUE, FALSE),
    med_varified_MI,
    med_new_MI = ifelse(comp_incid_MI == 1, TRUE, FALSE),
    med_new_MI_unrecognized = ifelse(comp_unrecognized_MI == 1, TRUE, FALSE),
    med_past_ihd = med_ihd_unk,
    med_new_ihd = comp_ihd_incidence
  ) %>%
  transmute(
    id_nivdaki,
    med_mi = factor(case_when(
      med_past_HA_coronary_68 == "Previously" ~ "Present",
      med_past_HA_coronary_68 == "Since exam II" ~ "Present",
      med_past_MI == TRUE ~ "Present",
      med_varified_MI == TRUE ~ "Present",
      med_new_MI == TRUE ~ "Present",
      med_new_MI_unrecognized == TRUE ~ "Present",
      TRUE ~ "Negative"
    )),
    med_ihd = factor(
      case_when(
        comp_CHD63_wide == 1 ~ "Present",
        med_past_ihd == TRUE ~ "Present",
        med_new_ihd == TRUE ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    )
  )

# ---------------------------------------------------------------------------- #
## Angina Data -----------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Combine different angina-related variables to create a single angina status variable.
angina_vars <- c(
  colnames(clean_df)[str_detect(colnames(clean_df), "angina")],
  "comp_Incid_AP"
)
angina_data <- clean_df %>%
  select(id_nivdaki, all_of(angina_vars)) %>%
  mutate(
    med_angina_63 = factor(case_when(
      med_past_angina_pectoris == "Definite" ~ "Definite AP",
      med_past_angina_pectoris == "Suspect" ~ "Suspect AP",
      med_past_angina_pectoris == "Functional Pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "Functional Pain (II)" ~
        "Chest pain - not AP",
      med_past_angina_pectoris == "Not Functional Pain" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "None of the above" ~ "Chest pain - not AP",
      med_past_angina_pectoris == "No diagnosis" ~ "No Chest Pain"
    )),
    med_angina_65 = factor(case_when(
      med_past_angina_pectoris_65 == "Definite AP" ~ "Definite AP",
      med_past_angina_pectoris_65 == "Suspect AP" ~ "Suspect AP",
      med_past_angina_pectoris_65 == "Secondary AP" ~ "Chest pain - not AP",
      med_past_angina_pectoris_65 == "Susp. Sec. AP" ~ "Chest pain - not AP",
      med_past_angina_pectoris_65 == "Undefined chest pain" ~
        "Chest pain - not AP",
      med_past_angina_pectoris_65 == "No Chest Pain" ~ "No Chest Pain"
    )),
    med_angina_68 = factor(case_when(
      med_past_angina_pectoris_68 == "Definite" ~ "Definite AP",
      med_past_angina_pectoris_68 == "Suspect" ~ "Suspect AP",
      med_past_angina_pectoris_68 == "Emotional functional chest pain" ~
        "Chest pain - not AP",
      med_past_angina_pectoris_68 == "Susp. Emotional functional chest pain" ~
        "Chest pain - not AP",
      med_past_angina_pectoris_68 == "Undefined chest pain" ~
        "Chest pain - not AP",
      med_past_angina_pectoris_68 == "No Chest Pain" ~ "No Chest Pain"
    ))
  ) %>%
  transmute(
    id_nivdaki,
    med_angina = factor(
      case_when(
        med_angina_63 == "Definite AP" ~ "Present",
        med_angina_63 == "Suspect AP" ~ "Present",
        med_angina_65 == "Definite AP" ~ "Present",
        med_angina_65 == "Suspect AP" ~ "Present",
        med_angina_68 == "Definite AP" ~ "Present",
        med_angina_68 == "Suspect AP" ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    )
  )

# ---------------------------------------------------------------------------- #
## Cancer Data -----------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# A placeholder for cancer variables. (TODO: Review if cancer data is needed post exam 68)
cancer_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "cancer")]
cancer_data <- clean_df %>%
  select(id_nivdaki, all_of(angina_vars)) %>%
  transmute(
    id_nivdaki
  )

# ---------------------------------------------------------------------------- #
## Pulmonary Data --------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Derive pulmonary function indicators by recoding obstructive lung disease and COPD.
pulmonary_vars <- c(
  "comp_obstructive_lung",
  "comp_Lung_ratio",
  "comp_Lungs_anyside",
  "comp_Forced_VC",
  "comp_FEV_FVC_ratio",
  "comp_FEVtert",
  "comp_tert_ratio_1",
  "comp_COPD",
  "med_past_lungs_68",
  "med_past_respiratory_68"
)
pulmonary_data <- clean_df %>%
  mutate(
    med_obstructive_lung = ifelse(comp_obstructive_lung == 1, TRUE, FALSE),
    med_Lungs_anyside = ifelse(comp_Lungs_anyside == 1, TRUE, FALSE),
    med_COPD = ifelse(comp_COPD == 1, TRUE, FALSE)
  ) %>%
  transmute(
    id_nivdaki,
    med_lung = factor(
      case_when(
        med_past_lungs_68 == "Asthma or bronchitis" ~ "Present",
        med_past_lungs_68 == "Other" ~ "Present",
        med_past_respiratory_68 == "Previously" ~ "Present",
        med_past_respiratory_68 == "Since exam II" ~ "Present",
        med_obstructive_lung == TRUE ~ "Present",
        med_Lungs_anyside == TRUE ~ "Present",
        med_COPD == TRUE ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    )
  )

# ---------------------------------------------------------------------------- #
## General Medical Data --------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Summarize additional general medical conditions including hypertension, peptic history,
# kidney issues, and medication use.
gen_med_vars <- c(
  "med_past_HTN_68",
  "med_past_limbs_68",
  "med_past_intermittent_claudication",
  "med_past_intermittent_claudication_65",
  "med_past_intermittent_claudication_68",
  "med_past_peptic_summary",
  "med_past_peptic",
  "med_past_peptic_68",
  "med_peripheral_art_dis",
  "med_takes_any_medication_65",
  "med_past_htn_medication_68",
  "comp_CKD",
  "med_past_kidney_68",
  "comp_RENAL_chronic",
  "diet_pcpoly"
)
gen_med <- clean_df %>%
  transmute(
    id_nivdaki,
    med_peripheral_art_dis = factor(
      case_when(
        med_peripheral_art_dis == "Definite" ~ "Present",
        med_peripheral_art_dis == "Suspect" ~ "Present",
        med_peripheral_art_dis == "None" ~ "Negative"
      ),
      levels = c("Negative", "Present")
    ),
    med_htn = factor(
      case_when(
        med_past_HTN_68 == "Since exam II" ~ "Present",
        med_past_HTN_68 == "Previously" ~ "Present",
        med_past_HTN_68 == "None" ~ "Negative"
      ),
      levels = c("Negative", "Present")
    ),
    med_limbs = factor(
      case_when(
        med_past_limbs_68 == "Hemiplegia" ~ "Present",
        med_past_limbs_68 == "Other" ~ "Present",
        med_past_limbs_68 == "NAD" ~ "Negative"
      ),
      levels = c("Negative", "Present")
    ),
    med_intermittent_claudication = factor(
      case_when(
        med_past_intermittent_claudication == "IC" ~ "Present",
        med_past_intermittent_claudication == "Multiple answers" ~ "Present",
        med_past_intermittent_claudication_65 == "Positive" ~ "Present",
        med_past_intermittent_claudication_68 == "Present" ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    ),
    med_peptic = factor(
      case_when(
        med_past_peptic_summary == "Incidence" ~ "Present",
        med_past_peptic == TRUE ~ "Present",
        med_past_peptic_68 == "Previously" ~ "Present",
        med_past_peptic_68 == "Since exam II" ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    ),
    med_renal = factor(
      case_when(
        comp_CKD == 1 ~ "Present",
        comp_CKD == 2 ~ "Present",
        med_past_kidney_68 == "Previously" ~ "Present",
        med_past_kidney_68 == "Since exam II" ~ "Present",
        comp_RENAL_chronic == 1 ~ "Present",
        comp_RENAL_chronic == 2 ~ "Present",
        TRUE ~ "Negative"
      ),
      levels = c("Negative", "Present")
    ),
    med_medication = factor(
      case_when(
        med_takes_any_medication_65 == TRUE ~ "Yes",
        med_past_htn_medication_68 == TRUE ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    diet_pcpoly
  )

# ---------------------------------------------------------------------------- #
## Composite Data Subset -------------------------------------------------------
# ---------------------------------------------------------------------------- #
comp_vars <- colnames(clean_df)[str_detect(colnames(clean_df), "comp")]
comp_data <- clean_df %>%
  transmute(
    id_nivdaki,
    comp_forced_pv
  )

# ---------------------------------------------------------------------------- #
## Outcome Data ---------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Import additional outcome data from a CSV file and convert date columns.
reach_95 <- read_csv(
  "raw_data/IIHD_reach95_Dor&Saar.csv",
  show_col_types = FALSE
) %>%
  transmute(
    death_date_2019 = as.Date(dateptira_2019, format = "%m/%d/%Y"),
    exam_start_fu = as.Date(beginfu, format = "%m/%d/%Y"),
    dmg_birth_year = yearob,
    outcome_age_lfu = age_lfu,
    id_nivdaki = nivdaki
  )

# ============================================================================ #
# Create Final Merged Data Frame ----------------------------------------------
# ============================================================================ #
# Merge the clean_df with the computed subsets using left_join on id_nivdaki.
# Also, compute additional variables such as time abroad and concentration camp status.
death_vars <- colnames(clean_df)[str_starts(colnames(clean_df), "death")]
unk_vars <- colnames(clean_df)[str_starts(colnames(clean_df), "unk")]
final_df <- clean_df %>%
  select(
    -c(
      all_of(smoke_vars),
      all_of(diabetes_vars),
      all_of(labs_vars),
      all_of(bmi_sbp_vars),
      all_of(mi_vars),
      all_of(angina_vars),
      all_of(pulmonary_vars),
      all_of(gen_med_vars),
      all_of(cancer_vars),
      all_of(comp_vars),
      "dmg_concentration",
      "lab_hdl_63",
      all_of(death_vars),
      all_of(unk_vars)
    )
  ) %>%
  left_join(bmi_sbp_data, by = "id_nivdaki") %>%
  left_join(smoke_data, by = "id_nivdaki") %>%
  left_join(diabetes_data, by = "id_nivdaki") %>%
  left_join(angina_data, by = "id_nivdaki") %>%
  left_join(mi_data, by = "id_nivdaki") %>%
  left_join(gen_med, by = "id_nivdaki") %>%
  left_join(labs_data, by = "id_nivdaki") %>%
  left_join(pulmonary_data, by = "id_nivdaki") %>%
  left_join(comp_data, by = "id_nivdaki") %>%
  left_join(reach_95, by = "id_nivdaki") %>%
  mutate(
    dmg_time_abroad = case_when(
      dmg_immigration_year - dmg_birth_year >= 0 ~
        dmg_immigration_year - dmg_birth_year,
      TRUE ~ 0
    ),
    dmg_concentration_camp = factor(ifelse(
      dmg_concentration_camp == "Never was",
      "Never was",
      "Was"
    ))
  )

# Save the final merged data frame to an RData file for further analysis.
save(final_df, file = "raw_data/final_df.RData")
