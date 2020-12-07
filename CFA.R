# load the lavaan package
library(lavaan)

if (exists("pathtodata") == F) { pathtodata = readline(prompt="Enter path to data: ") }

# ------------------------------------------------------------------------------

################################################################################
############################  PRENATAL ELS SCORE  ##############################
################################################################################

# Load dataset
pre_risk <- readRDS(paste(pathtodata, 'prenatal_stress.rds'))

# ------------------------------------------------------------------------------
# specify the models

# I first attempted a comprehensive model including all domains, but it turned out 
# to be problematic, mainly due to the empty cells. Attempt nr 2 after imputation...

# prenatal.model <- ' 
# LE # life_events =~ family_member_died + friend_relative_died + family_member_ill + admitted_to_hospital + health + unemployed + mother_work_study_problems + moved_house + blood_loss + examination + baby_worried + pregnancy_worried + obstetric_care + pregnancy_planned + victim_robbery
# CR # contextual_risk =~ financial_problems + financial_difficulties + income_reduced + housing_defects + housing_adequacy + housing_basic_living
# PS # personal_stress =~ age_mdich + gsi_mdich + forcemdich + publicordermdich + criminal_record_m + edu
# IS # interpersonal_stress =~ difficulties_contacts + difficulties_partner + difficulties_family_friend + mardich + divorce + family_support + family_acceptance + family_affection + family_acception + family_trust + family_painful_feelings + family_decisions + family_conflict + family_decisions_problems + family_plans + family_talk_sadness + family_talk_worries + famsize '

prenatal_LE.model <- ' 
life_events =~ family_member_died + friend_relative_died + family_member_ill + admitted_to_hospital + health + unemployed + mother_work_study_problems + moved_house + blood_loss + examination + baby_worried + pregnancy_worried + obstetric_care + pregnancy_planned + victim_robbery '

prenatal_CR.model <- ' 
contextual_risk =~ financial_problems + financial_difficulties + income_reduced + housing_defects + housing_adequacy + housing_basic_living '

prenatal_PS.model <- ' 
personal_stress =~ age_mdich + gsi_mdich + forcemdich + publicordermdich + criminal_record_m + edu '

prenatal_IS.model <- ' 
interpersonal_stress =~ difficulties_contacts + difficulties_partner + difficulties_family_friend + mardich + divorce + family_support + family_acceptance + family_affection + family_acception + family_trust + family_painful_feelings + family_decisions + family_conflict + family_decisions_problems + family_plans + family_talk_sadness + family_talk_worries + famsize '

# ------------------------------------------------------------------------------
# fit the models
# pre_fit <- cfa(prenatal.model, data=pre_risk) # does not converge properly for now

# I estimate each domain model using ML estimator { = WLSMV? }
# { orthogonal = T?, std.lv = T? --> orthogonal latent variables and standardized latent variables
# { verbose = T, to see whether anything is computed and if not, where it gets stuck
# { ordered = ... }

pre_LE_fit <- cfa(prenatal_LE.model, data=pre_risk) 
pre_CR_fit <- cfa(prenatal_CR.model, data=pre_risk)
pre_PS_fit <- cfa(prenatal_PS.model, data=pre_risk)
pre_IS_fit <- cfa(prenatal_IS.model, data=pre_risk)

# ------------------------------------------------------------------------------
# display summary output
# summary(pre_fit, fit.measures=TRUE)
summary(pre_LE_fit, fit.measures=TRUE, standardized=TRUE)
summary(pre_CR_fit, fit.measures=TRUE, standardized=TRUE)
summary(pre_PS_fit, fit.measures=TRUE, standardized=TRUE)
summary(pre_IS_fit, fit.measures=TRUE, standardized=TRUE)

# ------------------------------------------------------------------------------

################################################################################
###########################  POSTNATAL ELS SCORE  ##############################
################################################################################

# Load dataset
post_risk <- readRDS(paste(pathtodata, 'postnatal_stress.rds'))

# ------------------------------------------------------------------------------
# specify the models

# postnatal.model <- ' 
# LE # life_events =~ le1 + le2 + le3 + le4 + le5 + le6 + le7 + rep_grade + le17 + le23 + le24 + friend_moved + fire_burglary
# CR # contextual_risk =~ tension_at_work + material_deprivation + financial_difficulties + le9 + trouble_pay + income_once + income_chronic + unemployed_once + unemployed_chronic
# PR # parental_risk =~ m_education + p_education + m_interpersonal_sensitivity + p_interpersonal_sensitivity + m_depression + p_depression + m_anxiety + p_anxiety + m_age + p_age
# IR # interpersonal_risk =~ marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs + le11 + le12 + le13 + le14 + le16
# DV # direct_victimization =~ m_harsh_parent + p_harsh_parent + bullying + le18 + le19 + le20 + le21 + le22 '

postnatal_LE.model <- '
life_events =~ le1 + le2 + le3 + le4 + le5 + le6 + le7 + rep_grade + le17 + le23 + le24 + friend_moved + fire_burglary '

postnatal_CR.model <- ' 
contextual_risk =~ tension_at_work + material_deprivation + financial_difficulties + le9 + trouble_pay + income_once + income_chronic + unemployed_once + unemployed_chronic '

postnatal_PR.model <- ' 
parental_risk =~ m_education + p_education + m_interpersonal_sensitivity + p_interpersonal_sensitivity + m_depression + p_depression + m_anxiety + p_anxiety + m_age + p_age '

postnatal_IR.model <- ' 
interpersonal_risk =~ marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs + le11 + le12 + le13 + le14 + le16 '

postnatal_DV.model <- ' 
direct_victimization =~ m_harsh_parent + p_harsh_parent + bullying + le18 + le19 + le20 + le21 + le22 '

# ------------------------------------------------------------------------------
# fit the models
# post_fit <- cfa(postnatal.model, data=post_risk)

post_LE_fit <- cfa(postnatal_LE.model, data=post_risk)
post_CR_fit <- cfa(postnatal_CR.model, data=post_risk)
post_PR_fit <- cfa(postnatal_PR.model, data=post_risk)
post_IR_fit <- cfa(postnatal_IR.model, data=post_risk)
post_DV_fit <- cfa(postnatal_DV.model, data=post_risk)

# ------------------------------------------------------------------------------
# display summary output
# summary(post_fit, fit.measures=TRUE)

summary(post_LE_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_CR_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_PR_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_IR_fit, fit.measures=TRUE, standardized=TRUE)
summary(post_DV_fit, fit.measures=TRUE, standardized=TRUE)

# ------------------------------------------------------------------------------
