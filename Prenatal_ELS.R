
################################################################################
############################  PRENATAL ELS SCORE  ##############################
################################################################################

# The following script builds a dataset with all indicators and domain scores in
# the prenatal cumulative risk score used in Rijlaarsdam et al. (2016) and 
# Schuumans (in preparation), here: Jolie and Isabel.

# I first read in the data and select the necessary columns (i.e. indicators), 
# merge them, calculate the four domain scores and save two files: (1) the full 
# dataset and (2) a summary overview of how many risk, no-risk and missing values 
# are present for each indicator. 

# Point to the necessary libraries
library(foreign)

# The code assumes that all raw data is stored in ONE folder:
dir = "/Users/Serena/Desktop/Data/" # -> CUSTOMIZE TO APPROPRIATE PATH

#### ----------------------------- FUNCTIONS ----------------------------- ####

## replace NAs
replacenas <- function(dat, miss_spec = c(777,888,999), col = c(2:length(dat))){
  for (j in 1:length(miss_spec)){
    for (i in 1:length(col)){
      dat[,col[i]] <- ifelse(dat[,col[i]] == miss_spec[j], NA, dat[,col[i]])
    }
  }
  return (dat)
}

## read in data quickly
readquick <- function(filename, rootdir=dir){ # only works for SPSS files
  dataframe <- read.spss(paste(rootdir, filename, sep=""), 
                         use.value.labels = F, to.data.frame = T)
  names(dataframe) <- tolower(names(dataframe))
  dataframe <- replacenas(dataframe)
  return(dataframe)
}

# Domain scores measure how many adversities are reported. Together with mean nr 
# of adversities (ranging from 0 to 1) the function can also provide a cumulative 
# number of adversities (just uncomment it).
# If a domain is not complete, the domain score is NA. This missing value needs to 
# be accounted for by multiple imputation.

# calculate the domain scores
domainscore <- function(df){
  # dataframe with all the included items
  df <- data.frame(df)
  # check if all variables in df are dichotomized 
  for (i in 1:ncol(df)){
    if (range(df[,i], na.rm = T)[1] == 1 & range(df[,i], na.rm = T)[2] == 2){ df[,i] <- df[,i] - 1}
    else {
      if (range(df[,i], na.rm = T)[1] != 0 | range(df[,i], na.rm = T)[2] != 1 ){
        stop('Items are not dichotomized')} 
    }
  }
  # calculate the mean number of events (weighted)
  temp_mean <- rowMeans(df, na.rm = F) #temp_sum <- rowSums(df, na.rm = F)
  return(temp_mean)
}
# ------------------------------------------------------------------------------

################################################################################
#### ------------------------ reading and merging ------------------------- ####
################################################################################

# ATTENTION! The code assumes you have run the SPSS Syntax on the original datasets
# (20141124_Cumulative environmental risk scorewithNamechanges_Acortes.sps) MODIFIED

# I read in the raw data and select the necessary columns.
# LE = Life Events, CR = Contextual Risk, PS = Personal risk, IR = Interpers. risk

#-------------------------------------------------------------------------------
GR1001v1A<- readquick("New GR1001-A_22112016.sav") # 9778 obs. of 18 var

# Construct GR1001A # used for LE domain.
GR1001A <- data.frame(GR1001v1A$idm, # pregnancy id
                      GR1001v1A$pregnancy_planned)
colnames(GR1001A) <- c("IDM","pregnancy_planned")
#-------------------------------------------------------------------------------
GR1001v1H<- readquick("New GR1001-H_22112016.sav") # 9778 obs. of 27 var

# Construct GR1001H # used for LE domain.
GR1001H <- data.frame(GR1001v1H$idm,
                      GR1001v1H$pregnancy_worried,
                      GR1001v1H$baby_worried)
colnames(GR1001H) <- c("IDM","pregnancy_worried","baby_worried")

#-------------------------------------------------------------------------------
GR1003v1A<- readquick("New GR1003-A1-7_02092013.sav") # 9778 obs. of 18 var

# Construct GR1003A # used for LE domain.
GR1003A <- data.frame(GR1003v1A$idm, GR1003v1A$health)
colnames(GR1003A) <- c("IDM","health")

#-------------------------------------------------------------------------------
GR1003v2A<- readquick("New GR1003-A8-11_02092013.sav") # 9778 obs. of 73 var

# used for LE domain.
GR1003A8 <- data.frame(GR1003v2A$idm, GR1003v2A$blood_loss)
colnames(GR1003A8) <- c("IDM","blood_loss")

#-------------------------------------------------------------------------------
GR1003v1C<- readquick("New GR1003-C_08042016.sav") # 9778 obs. of 115 var

# Construct GR1003C
GR1003C <- data.frame(GR1003v1C$idm, 
                      GR1003v1C$family_member_died, # LE
                      GR1003v1C$friend_relative_died, # LE
                      GR1003v1C$family_member_ill, # LE
                      GR1003v1C$mother_work_study_problems, # LE
                      GR1003v1C$victim_robbery, # LE
                      GR1003v1C$unemployed, # LE
                      GR1003v1C$moved_house, # LE
                      GR1003v1C$housing_adequacy, # CR
                      GR1003v1C$financial_problems, # CR
                      GR1003v1C$income_reduced, # CR
                      GR1003v1C$difficulties_partner, # IR
                      GR1003v1C$difficulties_family_friend, # IR
                      GR1003v1C$difficulties_contacts, # IR
                      GR1003v1C$divorce, # IR
                      GR1003v1C$famsize) # IR
colnames(GR1003C) <- c("IDM","family_member_died", "friend_relative_died", "family_member_ill",
                       "mother_work_study_problems", "victim_robbery", "unemployed",
                       "moved_house", "housing_adequacy", "financial_problems","income_reduced", 
                       "difficulties_partner", "difficulties_family_friend", "difficulties_contacts", 
                       "divorce", "famsize")

#-------------------------------------------------------------------------------
GR1003v1J<- readquick("New GR1003-Family Assessment Device J1-J12_22112016.sav") # 9778 obs. of 25 vars

# Construct GR1003J
GR1003J <- data.frame(GR1003v1J$idm,
                      GR1003v1J$family_affection, # IS
                      GR1003v1J$family_decisions_problems, # IS
                      GR1003v1J$family_acceptance, # IS
                      GR1003v1J$family_acception, # IS
                      GR1003v1J$family_trust, # IS
                      GR1003v1J$family_painful_feelings, # IS
                      GR1003v1J$family_decisions, # IS
                      GR1003v1J$family_conflict, # IS
                      GR1003v1J$family_plans, # IS
                      GR1003v1J$family_talk_sadness, # IS
                      GR1003v1J$family_talk_worries, # IS
                      GR1003v1J$family_support) # IS
colnames(GR1003J) <- c("IDM","family_affection","family_decisions_problems","family_acceptance",
                       "family_acception","family_trust","family_painful_feelings",
                       "family_decisions","family_conflict","family_plans","family_talk_sadness",
                       "family_talk_worries","family_support")

#-------------------------------------------------------------------------------
# gsi = Global Sensitivity index, for the PS domain - mother
GR1003dep <- readquick("New GR1003-BSI D1_22112016.sav") # 9778 obs of 261 

GR1003BSI <- data.frame(GR1003dep$idm, GR1003dep$gsi_mdich) # for PS
colnames(GR1003BSI) <- c("IDM","gsi_mdich")

# - father # this was in Isabel script but it is not used in the score.
# GR1004dep <- readquick("GR1004-BSI G1_22112016.sav") # 9778 obs of 261 

#-------------------------------------------------------------------------------
GR1003v1G<- readquick("New GR1003-G_01072012.sav")

# Construct GR1003G
GR1003G <- data.frame(GR1003v1G$idm, 
                      GR1003v1G$criminal_record_m, # PS
                      GR1003v1G$publicordermdich, # PS
                      GR1003v1G$forcemdich) # PS
colnames(GR1003G) <- c("IDM","criminal_record_m","publicordermdich","forcemdich")

#-------------------------------------------------------------------------------
# NOT USED IN THIS SCORE BUT IN JOLIE'S SCRIPT
#GR1004v1I<- readquick("New GR1004-I_01072012.sav")

# Construct GR1004I 
#GR1004I <- data.frame(GR1004v1I$idm,
#                      GR1004v1I$criminal_record_p, # PS
#                      GR1004v1I$publicorderpdich, # PS
#                      GR1004v1I$forcepdich) # PS
#colnames(GR1004I) <- c("IDM","criminal_record_p","publicorderpdich","forcepdich")

#-------------------------------------------------------------------------------
GR1005v1A<- readquick("New GR1005-A_22112016.sav")

# Construct GR1005A # used for LE domain.
GR1005A <- data.frame(GR1005v1A$idm,
                      GR1005v1A$admitted_to_hospital, 
                      GR1005v1A$examination,
                      GR1005v1A$obstetric_care)
colnames(GR1005A) <- c("IDM","admitted_to_hospital","examination","obstetric_care")

#-------------------------------------------------------------------------------
# ATTENTION, I changed the var name from GR1005 (Isabel scripts) to GR1005E!!
GR1005v1E <- readquick("New GR1005-E_22112016.sav")

# Construct GR1005E # used for CR domain.
GR1005E <- data.frame(GR1005v1E$idm,
                      GR1005v1E$housing_basic_living, 
                      GR1005v1E$housing_defects,
                      GR1005v1E$financial_difficulties)
colnames(GR1005E) <- c("IDM","housing_basic_living","housing_defects","financial_difficulties")

#-------------------------------------------------------------------------------
fetal_general <- readquick("New FETALPERIOD-ALLGENERALDATA_29012018.sav") # 9778 obs of 96 var

demogr <- data.frame(fetal_general$idm, 
                     fetal_general$age_mdich, # younger than 19 for PS
                     fetal_general$mardich, # marital status for IS
                     ifelse(fetal_general$educm <= 3, yes = 1, no = 0))
                     # NOTE: in Jolie's script she creates EDUCMc and EDUCMc2 but I follow 
                     # Isabel's approach and dichotomize the original variable (educm)
colnames(demogr) <- c("IDM","age_mdich","mardich","edu")

#-------------------------------------------------------------------------------
# This function merges together all separate data.frames according to the IDM 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip: because merge can only take two data.frames at the time and I am lazy, I used Reduce.
prenatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDM',  all.x = TRUE),
                          list(GR1001A, GR1001H, GR1003A, GR1003A8, GR1003C, GR1003J, 
                               GR1003BSI, GR1003G, GR1005A, GR1005E, demogr)) 
# remember to add GR1004I & GR1004BSI if you use them. 

#-------------------------------------------------------------------------------
# In order to later merge the dataset with postnatal variables, I link the pregnancy
# id (idm) to the child id (idc). 
child_general <- readquick("CHILD-ALLGENERALDATA_07072020.sav") # 9901 obs of 122 
child_id <- data.frame(child_general$idm, child_general$idc)
colnames(child_id) <- c("IDM","IDC")

# ATTENTION! Only need to run this line if you are using the prenatal score together
# with postnatal outcomes/scores. Note: it will change the number of observations (from 9778 to 9901)
prenatal_stress <- merge(child_id, prenatal_stress, by = 'IDM', all.x = T)

#-------------------------------------------------------------------------------
# Let's have a look at risk distribution and missing data per indicator.

prenatal_summary = data.frame(row.names=c("no risk","risk","NA"))
for (i in 3:ncol(prenatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[,c] <- s }

#-------------------------------------------------------------------------------

################################################################################
#### --------------- create the (weighted) domain scores ------------------ ####
################################################################################

# LE
prenatal_stress[,c('pre_life_events')] <- domainscore(prenatal_stress[,c(
  "family_member_died", 
  "friend_relative_died",
  "family_member_ill", 
  "admitted_to_hospital",
  "health", 
  "unemployed", 
  "mother_work_study_problems",
  "moved_house", 
  "blood_loss", 
  "examination", 
  "baby_worried", 
  "pregnancy_worried",
  "obstetric_care", 
  "pregnancy_planned", 
  "victim_robbery")])

# CR
prenatal_stress[,c('pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  "financial_problems", 
  "financial_difficulties", 
  "income_reduced",
  "housing_defects", 
  "housing_adequacy", 
  "housing_basic_living")])

# PS
prenatal_stress[,c('pre_personal_stress')] <- domainscore(prenatal_stress[,c(
  "age_mdich", 
  "gsi_mdich", 
  "forcemdich", 
  "publicordermdich", 
  "criminal_record_m", 
  "edu")])

# IS
prenatal_stress[,c('pre_interpersonal_stress')] <- domainscore(prenatal_stress[,c(
  "difficulties_contacts",
  "difficulties_partner",
  "difficulties_family_friend", 
  "mardich", 
  "divorce",
  "family_support", 
  "family_acceptance", 
  "family_affection",
  "family_acception", 
  "family_trust", 
  "family_painful_feelings",
  "family_decisions", 
  "family_conflict", 
  "family_decisions_problems",
  "family_plans", 
  "family_talk_sadness", 
  "family_talk_worries", 
  "famsize")])

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where you have the raw data
saveRDS(prenatal_stress, paste(dir,'prenatal_stress.rds'))
saveRDS(prenatal_summary, paste(dir,'prenatal_stress_summary.rds'))

