################################################################################
############################  PRENATAL ELS SCORE  ##############################
################################################################################

# The following script builds a dataset with all indicators and domain scores in
# the prenatal cumulative risk score used in Rijlaarsdam et al. (2016) and 
# Schuumans (in preparation), here referred to as : Jolie and Isabel.

# First, let's point to the necessary libraries and define all the functions that 
# are going to be used: readquick, percent_missing, domainscore (also repmeas, 
# bsi_scores, fad_scores that are used in addition in the postnatal stress script)
source("0-Setup_and_functions.R")

# ATTENTION! You will be prompted with an "Enter path to data:" message 
# -> Enter the location of your datafiles. The code assumes that all (raw) data is 
# stored in ONE folder. Do not forget the final slash in your path, and, speaking of slashes, 
# beware of OS sensitive changes when you want to modify the structure of your dirs!

# For this version of the score
# You will need the following files (or updated versions from datamanagemet)
# GR1001-A_22112016.sav; GR1001-H_22112016.sav; GR1003-A1-7_02092013.sav; GR1003-A8-11_02092013.sav;
# GR1003-C_08042016.sav; GR1003-Family Assessment Device J1-J12_22112016.sav; GR1003-G_01072012.sav;
# GR1003-BSI D1_22112016.sav; GR1005-A_22112016.sav; GR1005-E_22112016.sav
# FETALPERIOD-ALLGENERALDATA_29012018.sav & CHILD-ALLGENERALDATA_07072020.sav

#-------------------------------------------------------------------------------
# I first read in the data and select the necessary columns (i.e. indicators), 
# and dichotomize when necessary. Then I merge them, calculate the five domain 
# scores and save two files: (1) the full dataset and (2) a summary overview of 
# how many risk, no-risk and missing values are present for each indicator. 

# LE = Life Events, CR = Contextual Risk, PR = Parental risk, IR = Interpers. risk

################################################################################
#### ------------------------ reading and merging ------------------------- ####
################################################################################

# NOTE, before we get going: when you call readquick on a file, the function will 
# replace values of 777, 888 or 999 with NAs unless they are IDCs or IDMs (see 
# repleacenas function). If you do NOT want this to happen for any other column 
# (for some reason) use the exclude_col argument with the name of the column that 
# has "real" 777, 888, or 999 values. 

#-------------------------------------------------------------------------------
GR1001v1A<- readquick("GR1001-A_22112016.sav") # 9778 obs. of 17 var

# Construct GR1001A # used for LE domain.
GR1001A <- data.frame('IDM' = GR1001v1A$idm, # pregnancy id
        'pregnancy_planned' = car::recode(GR1001v1A$a0500101, '0=1; 1=0')) 
          # Was this pregnancy planned? (1 = no; 0 = yes)

#-------------------------------------------------------------------------------
GR1001v1H<- readquick("GR1001-H_22112016.sav") # 9778 obs. of 25 var

# Construct GR1001H # used for LE domain.
GR1001H <- data.frame('IDM' = GR1001v1H$idm,
        'pregnancy_worried' = ifelse(GR1001v1H$h0700701 < 3, yes = 1, no = 0), 
              # I worry whether the pregnancy will go well or not.
              # DICH: "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
             'baby_worried' = ifelse(GR1001v1H$h0700301 < 3, yes = 1, no = 0)) 
              # I am worried about the health of my baby.
              # DICH: "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.

#-------------------------------------------------------------------------------
GR1003v1A<- readquick("GR1003-A1-7_02092013.sav") # 9778 obs. of 17 var

# Construct GR1003A # used for LE domain.
GR1003A <- data.frame('IDM' = GR1003v1A$idm, 
                   'health' = ifelse(GR1003v1A$a0100103 > 3, yes = 1, no = 0)) 
                    # Generally how would you describe your health?
                    # DICH: "Poor"/"Moderate" = risk. "Good"/"Very Good"/"Excellent" = no risk.

#-------------------------------------------------------------------------------
GR1003v2A<- readquick("GR1003-A8-11_02092013.sav") # 9778 obs. of 72 var

# used for LE domain.
GR1003A8 <- data.frame('IDM' = GR1003v2A$idm, 
                'blood_loss' = ifelse(GR1003v2A$a0801703 < 5, yes = 1, no = 0)) 
                 # Have you suffered from vaginal blood loss in the past 2 months?
                 # DICH: "Daily"/"Few days a week"/"Once a week"/"Less than once a week" = risk. "Never" = no risk. 

#-------------------------------------------------------------------------------
GR1003v1C<- readquick("GR1003-C_08042016.sav") # 9778 obs. of 90 var

# First I need to recode some "Not applicable" answers into NAs.
problem_vars <- c('c1200103', # Have you had problems with or at work?
                  'c1300103', # Problems with school or with your studies?
                  'c0600103', # Difficulties between you and your parents in law?
                  'c0800103', # Difficulties between you and one or more brothers or sisters? 
                  'c1000103', # Have there been problems with people who are your friends?
                  'c1500103', # Have you had housing problems in the past year?
                  'c1400103', # Have you had any financial problems in the past year?
                  'c0700103', # Difficulties between you and your partner?
                  'c0500103') # Difficulties in contact with others in the past year?
for (var in problem_vars) { GR1003v1C[, var] <- ifelse(GR1003v1C[, var] == 5, NA, GR1003v1C[, var]) }

# Finally, construct GR1003C
GR1003C <- data.frame(  'IDM' = GR1003v1C$idm, 
         'family_member_died' = ifelse(GR1003v1C$c0100803 == 0 | GR1003v1C$c0100903 == 0, yes = 1, no = 0), # LE
  # One of your children died in the last 12 months? YES | OR | Your partner died in the last 12 months? YES
'family_member_ill_pregnancy' = ifelse(GR1003v1C$c0100603 == 0 | GR1003v1C$c0100703 == 0, yes = 1, no = 0), # LE
  # One or more of your children seriously been ill in the last 12 months? YES | OR | Other family member seriously been ill in the last 12 months? YES
        'work_study_problems' = ifelse(GR1003v1C$c1200103 > 1 | GR1003v1C$c1300103 > 1, yes = 1, no = 0),   # LE
  # Have you had problems with or at work? | OR | Problems with school or with your studies in the past year? 
  # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
       'friend_relative_died' = car::recode(GR1003v1C$c0101003,'0=1; 1=0'), # LE
  # Member of your family or a good friend died in the last 12 months? (1=yes; 0=no) 
             'victim_robbery' = car::recode(GR1003v1C$c0100303,'0=1; 1=0'), # LE
  # Have you been a victim of robbery in the last 12 months? (1=yes; 0=no) 
                 'unemployed' = car::recode(GR1003v1C$c0100403,'0=1; 1=0'), # LE
  # Have you become unemployed in the last 12 months? (1=yes; 0=no) 
                'moved_house' = car::recode(GR1003v1C$c0100103,'0=1; 1=0'), # LE
  # Have you yourself moved house in the last 12 months? (1=yes; 0=no) 
           'housing_adequacy' = ifelse(GR1003v1C$c1500103 > 1, yes = 1, no = 0), # CR
  # Have you had housing problems in the past year? 
  # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
         'financial_problems' = ifelse(GR1003v1C$c1400103 > 1, yes = 1, no = 0), # CR
  # Have you had any financial problems in the past year? 
  # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
             'income_reduced' = car::recode(GR1003v1C$c0100203,'0=1; 1=0'),  # CR
  # Downturn in financial situation in the last 12 months? (1=yes; 0=no) 
       'difficulties_partner' = ifelse(GR1003v1C$c0700103 > 1, yes = 1, no = 0),  # IR
  # Difficulties between you and your partner?
  # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
 'difficulties_family_friend' = ifelse(GR1003v1C$c0600103 > 1 | GR1003v1C$c0800103 > 1 | GR1003v1C$c1000103 > 1, yes = 1, no = 0), # IR
  # Difficulties between you and your parents in law? | OR |Difficulties between you and one or more brothers or sisters? | OR |
  # Have there been problems with people who are your friends? 
  # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
      'difficulties_contacts' = ifelse(GR1003v1C$c0500103 > 1, yes = 1, no = 0), # IR
  # Difficulties in contact with others in the past year? 
  # DICH: "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
          'divorce_pregnancy' = car::recode(GR1003v1C$c0101103,'0=1; 1=0'), # IR
  # Have you had a divorce in the last 12 months? (1=yes; 0=no) 
      'family_size_pregnancy' = ifelse(GR1003v1C$crowding > 3, yes = 1, no = 0) ) # IR
  # With how many people does the pregnant woman live? 
  # DICH: more than 3 people = risk. 3 or less people = no risk. 

#-------------------------------------------------------------------------------
GR1003v1J<- readquick("GR1003-Family Assessment Device J1-J12_22112016.sav") # 9778 obs. of 13 vars

# Construct GR1003J
# DICH: |"Totally disagree"/"Disagree" (= NO)| vs |"Fully agree"/"Agree" (= YES)|
GR1003J <- data.frame('IDM' = GR1003v1J$idm,
         'family_affection' = ifelse(GR1003v1J$j0500103 < 3, yes = 1, no = 0), 
  # We can express our feelings to each other. NO # IR
'family_decisions_problems' = ifelse(GR1003v1J$j0900103 < 3, yes = 1, no = 0), 
  # We are able to make decisions. NO # IR
        'family_acceptance' = ifelse(GR1003v1J$j0300103 < 3, yes = 1, no = 0), 
  # People in our family accept each other. NO # IR
         'family_acception' = ifelse(GR1003v1J$j0700103 < 3, yes = 1, no = 0), 
  # People in our family feel accepted. NO # IR
             'family_trust' = ifelse(GR1003v1J$j1100103 < 3, yes = 1, no = 0), 
  # We trust each other. NO # IR
  'family_painful_feelings' = ifelse(GR1003v1J$j0800103 > 2, yes = 1, no = 0), 
  # A lot of unpleasant and painful feelings. YES # IR
         'family_decisions' = ifelse(GR1003v1J$j1000103 > 2, yes = 1, no = 0), 
  # Decision making is a problem in our family. YES # IR
          'family_conflict' = ifelse(GR1003v1J$j1200103 > 2, yes = 1, no = 0), 
  # We do not get on well with each other. YES # IR
             'family_plans' = ifelse(GR1003v1J$j0200103 > 2, yes = 1, no = 0), 
  # Difficult making plans to do something with family. YES # IR
      'family_talk_sadness' = ifelse(GR1003v1J$j0400103 > 2, yes = 1, no = 0), 
  # We cannot talk to each other about any sadness. YES # IR
      'family_talk_worries' = ifelse(GR1003v1J$j0600103 > 2, yes = 1, no = 0), 
  # We avoid talking about worries and problems. YES # IR
           'family_support' = ifelse(GR1003v1J$j0100103 < 3, yes = 1, no = 0) ) 
  # If there are problems we can count on each other. NO # IR

#-------------------------------------------------------------------------------
# Psychopathology - mother & father
GR1003dep <- readquick("GR1003-BSI D1_22112016.sav") # 9778 obs of 261 vars
GR1004dep <- readquick("GR1004-BSI G1_22112016.sav") # 9778 obs of 261 

# DICH: cut-off for maternal psychopathology in as provided in addendum to the BSI manual (De Beurs, 2009).
GR1003BSI <- data.frame(   'IDM' = GR1003dep$idm, 
        'm_depression_pregnancy' = ifelse(GR1003dep$dep >= 0.80, yes = 1, no = 0), # depression for PR
           'm_anxiety_pregnancy' = ifelse(GR1003dep$anx >= 0.71, yes = 1, no = 0), # anxiety for PR
'm_interp_sensitivity_pregnancy' = ifelse(GR1003dep$i_s >= 0.95, yes = 1, no = 0)) # interpersonal sensitivity for PR
  
# DICH: cut-off for paternal psychopathology in as provided in addendum to the BSI manual (De Beurs, 2009).
GR1004BSI <- data.frame(   'IDM' = GR1003dep$idm, 
        'p_depression_pregnancy' = ifelse(GR1004dep$dep_p >= 0.71, yes = 1, no = 0), # depression for PR
           'p_anxiety_pregnancy' = ifelse(GR1004dep$anx_p >= 0.65, yes = 1, no = 0), # anxiety for PR
'p_interp_sensitivity_pregnancy' = ifelse(GR1004dep$i_s_p >= 0.78, yes = 1, no = 0)) # interpersonal sensitivity for PR

#-------------------------------------------------------------------------------
GR1003v1G<- readquick("GR1003-G_01072012.sav") # 9778 obs of 22 vars

# First recode a "Do not know" answer into NA.
GR1003v1G$g0300103[GR1003v1G$g0300103 == 2] <- NA # Do you have a criminal record?

# Construct GR1003G
GR1003G <- data.frame('IDM' = GR1003v1G$idm, 
  'm_criminal_record' = GR1003v1G$g0300103, # Do you have a criminal record? YES # PR
'm_violence_property' = ifelse((GR1003v1G$g0100503 + GR1003v1G$g0100603  - 2) > 0, yes = 1, no = 0), # PR
  # Effectively: Deliberately damaged or vandalized in the past two years ("Once" or "2-3 times") 
  # | AND/OR | Damaged property of another person in the past two years ("Once" or "2-3 times").
  # In GenR sample, none of the mothers committed neither crime "4-5 times" or "more than 6 times" 
  # but both of those options were possible answers in the questionnaire.
  # DICH: "Once"/"2-3 times"/"4-5 times"/"more than 6 times" = risk. "Never" = no risk.
  'm_violence_people' = ifelse((GR1003v1G$g0101603 + GR1003v1G$g0101703 + GR1003v1G$g0101803 - 3) > 0, yes = 1, no = 0)) # PR
  # Threatened anyone in the past two years | AND/OR | Hit anyone so hard that he or 
  # she was injured in the past two years | AND/OR | Injured anyone with a knife or 
  # weapon in the past two years ("Once"/"2-3 times"/"4-5 times"/"more than 6 times")
  # DICH: "Once"/"2-3 times"/"4-5 times"/"more than 6 times" = risk. "Never" = no risk.

# Paternal variables are NOT USED IN THIS SCORE but can be integrated provided 
GR1004v1I <- readquick("GR1004-I_01072012.sav")
# Construct GR1003G
GR1004I <- data.frame('IDM' = GR1004v1I$idm, 
        'p_criminal_record' = ifelse(GR1004v1I$i0300104 == 1 | GR1004v1I$i0200104 == 1, 1, 
                                     ifelse(GR1004v1I$i0300104 == 0 & GR1004v1I$i0200104 == 0, 0, NA)))
         # Do you have a criminal record? | Have you ever been arrested by the police? YES # PR

#-------------------------------------------------------------------------------
GR1005v1A<- readquick("GR1005-A_22112016.sav") # 9778 obs of 17 vars

# Construct GR1005A # used for LE domain.
GR1005A <- data.frame('IDM' = GR1005v1A$idm,
'admitted_to_hospital' = GR1005v1A$a0300105, 
  # Admitted to a hospital (>24 hours) during this pregnancy? YES
         'examination' = ifelse(GR1005v1A$a0100105 == 1 | GR1005v1A$a0100205 == 1 | GR1005v1A$a0100305 == 1 | GR1005v1A$a0100405 == 1, yes = 1, no = 0),
  # Chorionic villus sampling test during this pregnancy? YES | OR | Aminiocentesis 
  # test during this pregnancy? YES | OR | Triple (maternal serum) test during this 
  # pregnancy? YES | OR | Ultrasound for nuchal translucency test during this pregnancy? YES
      'obstetric_care' = ifelse(GR1005v1A$a0600105 > 2, yes = 1, no = 0)) 
  # General satisfaction about the obstetric care so far
  # DICH: "Very dissatisfied"/"Somewhat dissatisfied" = risk. "Very satisfied"/"Fairly satisfied" = no risk.


#-------------------------------------------------------------------------------
GR1005v1E <- readquick("GR1005-E_22112016.sav") # 9778 obs of 40 vars

# Construct GR1005E # used for CR domain.
GR1005E <- data.frame('IDM' = GR1005v1E$idm,
 'housing_basic_living' = ifelse(GR1005v1E$e0500305 == 2 | GR1005v1E$e0501105 == 2 | GR1005v1E$e0501305 == 2, yes = 1, no = 0), 
  # Possession of adequate heating in your home in cold weather. NO | OR |
  # Possession of a washing machine. NO | OR | Possession of a refrigerator. NO. 
      'housing_defects' = ifelse(GR1005v1E$e0800505 == 1 | GR1005v1E$e0800605 == 1 | GR1005v1E$e0800705 == 1, yes = 1, no = 0), 
  # Often had problems with cold or draft in your home in the last year? YES | OR | 
  # Often had problems with misted windows in your sitting room in the last year? YES | OR | 
  # Often had problems with damp patches, mould on the wall, furniture in the last year? YES
'trouble_pay_pregnancy' = ifelse(GR1005v1E$e0400105 > 1, yes = 1, no = 0)) 
  # Difficulty in paying for food, rent, electricity bill and suchlike in the past year?
  # DICH: "Great difficulty"/"Some difficulty" = risk. "No difficulty at all" = no risk. 

#-------------------------------------------------------------------------------
fetal_general <- readquick("FETALPERIOD-ALLGENERALDATA_29012018.sav") # 9778 obs of 95 var

demogr <- data.frame('IDM' = fetal_general$idm, 
         'early_pregnancy' = ifelse(fetal_general$age_m_v2 < 19, yes = 1, no = 0), 
  # Mother younger than 19 years at intake (for PR)
  # DICH: based on Cecil et al. (2014); Rijlaarsdam et al. (2016).
'marital_status_pregnancy' = fetal_general$mardich, # Marital status: "No partner" (for IR)
   'm_education_pregnancy' = ifelse(fetal_general$educm <= 3, yes = 1, no = 0), # Highest education finished mother (for CR)
   'p_education_pregnancy' = ifelse(fetal_general$educp <= 3, yes = 1, no = 0)) # Highest education finished partner (for CR)
  # DICH: "No education"/"Primary"/"Secondary-phase 1"/"Secondary-phase 2" = risk. "Higher-phase 1"/"Higher-phase 2" = no risk.
  # based on Centraal Bureau voor de Statistiek (2016). 

#-------------------------------------------------------------------------------
# This function merges together all separate dataframes according to the IDM 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip: because merge can only take two dataframes at the time and I am lazy, I use Reduce.
prenatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDM',  all.x = T),
                          list(GR1001A, GR1001H, GR1003A, GR1003A8, GR1003C, GR1003J, 
                               GR1003BSI, GR1004BSI, GR1003G, GR1004I, GR1005A, GR1005E, demogr)) 

#-------------------------------------------------------------------------------
# In order to later merge the dataset with postnatal variables, I link the pregnancy
# id (idm) to the child id (idc). 
child_general <- readquick("CHILD-ALLGENERALDATA_07072020.sav") # 9901 obs of 122 
child_id <- data.frame('IDM' = child_general$idm, 'IDC' = child_general$idc)

# ATTENTION! Only need to run this line if you are using the prenatal score together
# with postnatal outcomes/scores. Note: it will change the number of observations (from 9778 to 9901)
prenatal_stress <- merge(child_id, prenatal_stress, by = 'IDM', all.x = T)

################################################################################
# Before we get to the final scores, some summary stats that may come in handy #

# Let's have a look at risk distribution and missing data per indicator.
prenatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))
for (i in 3:ncol(prenatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / nrow(prenatal_stress))*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / nrow(prenatal_stress))*100, 2)
  }

#-------------------------------------------------------------------------------
# Apply the percent_missing function to the rows (1) of the entire dataset (total 50 indicators)
prenatal_stress$pre_percent_missing = apply(prenatal_stress[,3:ncol(prenatal_stress)], # Same as above, if not merged with child_id, count from 2.
                                            1, percent_missing)

################################################################################
#### -------------- create the (un-weighted) domain scores ---------------- ####
################################################################################

# ATTENTION! Here we use de default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) when missingness is < 25%. If you prefer 
# working with the actual number of risks (i.e. sum score) or the weighted version 
# of it as in e.g. Rijlaarsdam et al. (2016), you can set the argument score_type
# to 'sum_simple' or 'sum_weighted' respectively (see Setup and functions script
# for calculation details). 

# LE
prenatal_stress[,c('pre_LE_percent_missing','pre_life_events')] <- domainscore(prenatal_stress[,c(
  "family_member_died", # One of your children |OR| Your partner died in the last 12 months? YES
  "friend_relative_died", # Member of your family or a good friend died in the last 12 months? YES
  "family_member_ill_pregnancy", # One or more of your children |OR| Other family member seriously been ill in the last 12 months? YES
  "admitted_to_hospital", # Admitted to a hospital (>24 hours) during this pregnancy? YES
  "health", # Generally how would you describe your health? "Poor"/"Moderate" = risk. "Good"/"Very Good"/"Excellent" = no risk.
  "unemployed", # Have you become unemployed in the last 12 months? YES
  "work_study_problems", # Have you had problems with or at work? |OR| Problems with school or with your studies in the past year? "Slight"/"Moderate"/"Serious".
  "moved_house", # Have you yourself moved house in the last 12 months? YES
  "blood_loss", # Have you suffered from vaginal blood loss in the past 2 months? "Daily"/"Few days a week"/"Once a week"/"Less than once a week" = risk. "Never" = no risk. 
  "examination", # Chorionic villus sampling test |OR| Aminiocentesis test |OR| Triple (maternal serum) test |OR| Ultrasound for nuchal translucency test during this pregnancy? YES
  "baby_worried", # I am worried about the health of my baby. "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
  "pregnancy_worried", # I worry whether the pregnancy will go well or not. "Almost always"/"Often" = risk. "Few"/"Sometimes"/"Never" = no risk.
  "obstetric_care", # General satisfaction about the obstetric care so far. "Very dissatisfied"/"Somewhat dissatisfied" = risk. "Very satisfied"/"Fairly satisfied" = no risk.
  "pregnancy_planned", # Was this pregnancy planned? NO
  "victim_robbery")]) # Have you been a victim of robbery in the last 12 months? (1=yes; 0=no) # LE

# CR
prenatal_stress[,c('pre_CR_percent_missing','pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  "financial_problems", # Have you had any financial problems in the past year? Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "trouble_pay_pregnancy", # Difficulty in paying for food, rent, electricity bill and suchlike in the past year? "Great difficulty"/"Some difficulty" = risk. "No difficulty at all" = no risk.
  "income_reduced", # Downturn in financial situation in the last 12 months? YES
  "housing_defects", # Often had problems with cold or draft in your home |OR| misted windows in your sitting room |OR| damp patches, mold on the wall, furniture in the last year? YES
  "housing_adequacy", # Have you had housing problems in the past year? "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "housing_basic_living", # Possession of adequate heating in your home in cold weather |OR| a washing machine |OR| a refrigerator
  "m_education_pregnancy", # Highest education finished "No education"/"Primary"/"Secondary-phase 1"/"Secondary-phase 2" = risk. "Higher-phase 1"/"Higher-phase 2" = no risk.
  "p_education_pregnancy")]) # Highest education finished "No education"/"Primary"/"Secondary-phase 1"/"Secondary-phase 2" = risk. "Higher-phase 1"/"Higher-phase 2" = no risk.

# PR
prenatal_stress[,c('pre_PR_percent_missing','pre_parental_risk')] <- domainscore(prenatal_stress[,c(
  "early_pregnancy", # Mother younger than 19 years at intake. 
  "m_depression_pregnancy", # BSI depression score > 0.80 (De Beurs, 2009).
  "m_anxiety_pregnancy", # BSI anxiety score > 0.71 (De Beurs, 2009).
  "m_interp_sensitivity_pregnancy", # BSI interpersonal sensitivity score > 0.95 (De Beurs, 2009).
  "p_depression_pregnancy", # BSI depression score > 0.78 (De Beurs, 2009).
  "p_anxiety_pregnancy", # BSI anxiety score > 0.65 (De Beurs, 2009).
  "p_interp_sensitivity_pregnancy", # BSI interpersonal sensitivity score > 0.71 (De Beurs, 2009).
  "m_violence_people", # Threatened anyone |OR| Hit anyone so hard that he or she was injured |OR| Injured anyone with a knife or weapon in the past two years ("Once"/"2-3 times"/"4-5 times"/"more than 6 times")
  "m_violence_property", # Deliberately damaged or vandalised |OR| Damaged property of another person in the past two years ("Once" or "2-3 times").
  "m_criminal_record", # Do you have a criminal record? YES
  "p_criminal_record")]) # Do you have a criminal record? | Have you ever been arrested by the police? YES # PR

# IR
prenatal_stress[,c('pre_IR_percent_missing','pre_interpersonal_risk')] <- domainscore(prenatal_stress[,c(
  "difficulties_contacts", # Difficulties in contact with others in the past year? "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "difficulties_partner", # Difficulties between you and your partner? "Slight"/"Moderate"/"Serious" = risk. "No" = no risk.
  "difficulties_family_friend", # Difficulties between you and your parents in law? |OR| between you and one or more brothers or sisters? |OR| problems with people who are your friends? "Slight"/"Moderate"/"Serious"
  "marital_status_pregnancy", # Marital status: "No partner"
  "divorce_pregnancy", # Have you had a divorce in the last 12 months? YES
  "family_support", # If there are problems we can count on each other. NO
  "family_acceptance", # People in our family accept each other. NO
  "family_affection", # We can express our feelings to each other. NO
  "family_acception", # People in our family feel accepted. NO
  "family_trust", # We trust each other. NO
  "family_painful_feelings", # A lot of unpleasant and painful feelings. YES
  "family_decisions", # Decision making is a problem in our family. YES
  "family_conflict", # We do not get on well with each other. YES
  "family_decisions_problems", # We are able to make decisions. NO
  "family_plans", # Difficult making plans to do something with family. YES
  "family_talk_sadness", # We cannot talk to each other about any sadness. YES
  "family_talk_worries", # We avoid talking about worries and problems. YES
  "family_size_pregnancy")]) # With how many people does the pregnant woman live? >3 people = risk. <= 3 people = no risk.

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where you have the raw data
saveRDS(prenatal_stress, paste0(pathtodata,'prenatal_stress.rds'))
saveRDS(prenatal_summary, paste0(pathtodata,'prenatal_stress_summary.rds'))

