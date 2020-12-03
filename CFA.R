# load the lavaan package
library(lavaan)

if (exists("pathtodata") == F) { pathtodata = readline(prompt="Enter path to data: ") }

# Load datasets
pre_risk <- readRDS(paste(pathtodata, 'prenatal_stress.rds'))
post_risk <- readRDS(paste(pathtodata, 'postnatal_stress.rds'))

# specify the models
prenatal.model <- ' 
# LE
life_events =~ family_member_died + friend_relative_died + family_member_ill + admitted_to_hospital + health + unemployed + mother_work_study_problems + moved_house + blood_loss + examination + baby_worried + pregnancy_worried + obstetric_care + pregnancy_planned + victim_robbery
# CR
contextual_risk =~ financial_problems + financial_difficulties + income_reduced + housing_defects + housing_adequacy + housing_basic_living
# PS
personal_stress =~ age_mdich + gsi_mdich + forcemdich + publicordermdich + criminal_record_m + edu
# IS
interpersonal_stress =~ difficulties_contacts + difficulties_partner + difficulties_family_friend + mardich + divorce + family_support + family_acceptance + family_affection + family_acception + family_trust + family_painful_feelings + family_decisions + family_conflict + family_decisions_problems + family_plans + family_talk_sadness + family_talk_worries + famsize '

postnatal.model <- ' 
# LE
life_events =~ le1 + le2 + le3 + le4 + le5 + le6 + le7 + rep_grade + le17 + le23 + le24 + friend_moved + fire_burglary
# CR
contextual_risk =~ tension_at_work + material_deprivation + financial_difficulties + le9 + trouble_pay + income_once + income_chronic + unemployed_once + unemployed_chronic
# PR
parental_risk =~ m_education + p_education + m_interpersonal_sensitivity + p_interpersonal_sensitivity + m_depression + p_depression + m_anxiety + p_anxiety + m_age + p_age
# IR
interpersonal_risk =~ marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs + le11 + le12 + le13 + le14 + le16
# DV
direct_victimization =~ m_harsh_parent + p_harsh_parent + bullying + le18 + le19 + le20 + le21 + le22 '

# fit the models
pre_fit <- cfa(prenatal.model, data=pre_risk)
post_fit <- cfa(postnatal.model, data=post_risk)

# display summary output
summary(pre_fit, fit.measures=TRUE)
summary(post_fit, fit.measures=TRUE)
