
# Hi there, 
# the following code meant to explore the ELS score we have created in previous scripts
# (i.e. Prenatal_ELS.R and Postnatal_ELS.R). We are going to look at the correlation
# matrix and the pattern of missingness. We are then going to run a Confermatory Factor
# Analysis (CFA), as well as an Exploratory Factor Analysis (EFA) on the dataset 
# after imputation that was performed following this script:
# https://github.com/SereDef/association-ELS-PCM-project/Imputation.R

# load necessary packages
library(lavaan)
library(psych)
library(tibble)

# check if the path to the data is already in memory, otherwise ask for it. 
if (exists("pathtodata") == F) { pathtodata = readline(prompt="Enter path to data: ") }

# ------------------------------------------------------------------------------

################################################################################
####################################  CFA  #####################################
################################################################################

# -----------------------------------------------------------------------------#
#---------------------------- PRENATAL ELS SCORE  -----------------------------#
# -----------------------------------------------------------------------------#

# Load dataset (created using the Prenatal_ELS.R script)
pre_risk <- readRDS(paste(pathtodata, 'prenatal_stress.rds', sep=""))

# ------------------------------------------------------------------------------
# specify the models

# I first attempted a comprehensive model including all domains, but it turned out 
# to be problematic, mainly due to the empty cells. Attempt nr 2 after imputation...

# prenatal.model <- ' 
# LE # life_events =~ family_member_died + friend_relative_died + family_member_ill_pregnancy + admitted_to_hospital + health + unemployed + work_study_problems + moved_house + blood_loss + examination + baby_worried + pregnancy_worried + obstetric_care + pregnancy_planned + victim_robbery
# CR # contextual_risk =~ financial_problems + trouble_pay_pregnancy + income_reduced + housing_defects + housing_adequacy + housing_basic_living
# PS # personal_stress =~ early_pregnancy + m_psychopathology + m_violence_people + m_violence_property + m_criminal_record + m_education_pregnancy
# IS # interpersonal_stress =~ difficulties_contacts + difficulties_partner + difficulties_family_friend + marital_status_pregnancy + divorce_pregnancy + family_support + family_acceptance + family_affection + family_acception + family_trust + family_painful_feelings + family_decisions + family_conflict + family_decisions_problems + family_plans + family_talk_sadness + family_talk_worries + family_size_pregnancy '

prenatal_LE.model <- ' 
life_events =~ NA*family_member_died + friend_relative_died + family_member_ill_pregnancy + admitted_to_hospital + health + unemployed + work_study_problems + moved_house + blood_loss + examination + baby_worried + pregnancy_worried + obstetric_care + pregnancy_planned + victim_robbery '

prenatal_CR.model <- ' 
contextual_risk =~ NA*financial_problems + trouble_pay_pregnancy + income_reduced + housing_defects + housing_adequacy + housing_basic_living + m_education_pregnancy '

prenatal_PR.model <- ' 
parental_risk =~ NA*early_pregnancy + m_depression_pregnancy + m_anxiety_pregnancy + m_interp_sensitivity_pregnancy + p_depression_pregnancy + p_anxiety_pregnancy + p_interp_sensitivity_pregnancy + m_violence_people + m_violence_property + m_criminal_record + p_criminal_record'

prenatal_IR.model <- ' 
interpersonal_risk =~ NA*difficulties_contacts + difficulties_partner + difficulties_family_friend + marital_status_pregnancy + divorce_pregnancy + family_support + family_acceptance + family_affection + family_acception + family_trust + family_painful_feelings + family_decisions + family_conflict + family_decisions_problems + family_plans + family_talk_sadness + family_talk_worries + family_size_pregnancy '

# ------------------------------------------------------------------------------
# fit the models
# pre_fit <- cfa(prenatal.model, data=pre_risk) # does not converge properly for now

# I estimate each domain model using ML estimator { = WLSMV? }
# { orthogonal = T?, std.lv = T? --> orthogonal latent variables and standardized latent variables
# { verbose = T, to see whether anything is computed and if not, where it gets stuck
# { ordered = ... }

pre_LE_fit <- cfa(prenatal_LE.model, data=pre_risk, estimator = 'DWLS', std.lv = TRUE) 
pre_CR_fit <- cfa(prenatal_CR.model, data=pre_risk, estimator = 'DWLS', std.lv = TRUE)
pre_PR_fit <- cfa(prenatal_PR.model, data=pre_risk, estimator = 'DWLS', std.lv = TRUE)
pre_IR_fit <- cfa(prenatal_IR.model, data=pre_risk, estimator = 'DWLS', std.lv = TRUE)

# ------------------------------------------------------------------------------
# display summary output
# summary(pre_fit, fit.measures=TRUE)
summary(pre_LE_fit, fit.measures=TRUE, standardized=TRUE)
summary(pre_CR_fit, fit.measures=TRUE, standardized=TRUE)
summary(pre_PR_fit, fit.measures=TRUE, standardized=TRUE)
summary(pre_IR_fit, fit.measures=TRUE, standardized=TRUE)

# -----------------------------------------------------------------------------#
#--------------------------- POSTNATAL ELS SCORE  -----------------------------#
# -----------------------------------------------------------------------------#

# Load dataset (created using the Postnatal_ELS.R script)
post_risk <- readRDS(paste(pathtodata, 'postnatal_stress.rds', sep = ""))

# ------------------------------------------------------------------------------
# specify the models

# postnatal.model <- ' 
# LE # life_events =~ sick_or_accident + family_member_ill + smbd_important_ill + parent_died + smbd_important_died + pet_died + school_workload + repeated_grade + lost_smth_important + moved + changed_school + friend_moved + fire_or_burglary
# CR # contextual_risk =~ tension_at_work + material_deprivation + financial_difficulties + neiborhood_problems + trouble_pay_childhood + income_once + income_chronic + unemployed_once + unemployed_chronic
# PR # parental_risk =~ m_education + p_education + m_interpersonal_sensitivity + p_interpersonal_sensitivity + m_depression + p_depression + m_anxiety + p_anxiety + m_age + p_age
# IR # interpersonal_risk =~ marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs + conflict_family_member + conflict_smbd_else + conflict_in_family + divorce_childhood + argument_friend
# DV # direct_victimization =~ m_harsh_parent + p_harsh_parent + bullying + physical_violence + physical_threats + sexual_harrasment + sexual_behavior + rumors_or_gossip '

postnatal_LE.model <- '
life_events =~ NA*sick_or_accident + family_member_ill + smbd_important_ill + parent_died + smbd_important_died + pet_died + school_workload + repeated_grade + lost_smth_important + moved + changed_school + friend_moved + fire_or_burglary '

postnatal_CR.model <- ' 
contextual_risk =~ NA*material_deprivation + financial_difficulties + neiborhood_problems + trouble_pay_childhood + income_once + income_chronic + unemployed_once + unemployed_chronic + m_education + p_education '

postnatal_PR.model <- ' 
parental_risk =~ NA*tension_at_work + m_interpersonal_sensitivity + p_interpersonal_sensitivity + m_depression + p_depression + m_anxiety + p_anxiety + m_age + p_age '

postnatal_IR.model <- ' 
interpersonal_risk =~ NA*marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs + conflict_family_member + conflict_smbd_else + conflict_in_family + divorce_childhood + argument_friend '

postnatal_DV.model <- ' 
direct_victimization =~ NA*m_harsh_parent + p_harsh_parent + bullying + physical_violence + physical_threats + sexual_harrasment + sexual_behavior + rumors_or_gossip '

# ------------------------------------------------------------------------------
# fit the models
# post_fit <- cfa(postnatal.model, data=post_risk)

post_LE_fit <- cfa(postnatal_LE.model, data=post_risk, estimator = 'DWLS', std.lv = TRUE)
post_CR_fit <- cfa(postnatal_CR.model, data=post_risk, estimator = 'DWLS', std.lv = TRUE)
post_PR_fit <- cfa(postnatal_PR.model, data=post_risk, estimator = 'DWLS', std.lv = TRUE)
post_IR_fit <- cfa(postnatal_IR.model, data=post_risk, estimator = 'DWLS', std.lv = TRUE)
post_DV_fit <- cfa(postnatal_DV.model, data=post_risk, estimator = 'DWLS', std.lv = TRUE)

# ------------------------------------------------------------------------------
# display summary output
# summary(post_fit, fit.measures=TRUE)

summary(post_LE_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_CR_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_PR_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_IR_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_DV_fit, fit.measures=TRUE, standardized=TRUE)

# ------------------------------------------------------------------------------

################################################################################
####################################  EFA  #####################################
################################################################################

# Quick Exploratory Factor Analysis ### AFTER IMPUTATION ###

# Load the imputed dataset
datarisk <- readRDS(paste(pathtodata, 'ELSPCM_imputed.rds', sep = ""))

# Divide the dataset into prenatal and postnatal items
pre = datarisk[2:which(colnames(datarisk) == 'family_size_pregnancy')] # first column is IDC
pre = add_column(pre, datarisk$m_age, .before = 'm_psychopathology'); # add the maternal age variable to the prenatal set 

post = datarisk[which(colnames(datarisk) == 'sick_or_accident'):which(colnames(datarisk) == 'rumors_or_gossip')];

prefact = factanal(pre, factors = 4, rotation = 'promax')
print(prefact, cutoff = .25)

postfact = factanal(post, factors = 5, rotation = 'promax')
print(postfact, cutoff = .25)

psych::scree(pre)
psych::scree(post)

# ------------------------------------------------------------------------------
