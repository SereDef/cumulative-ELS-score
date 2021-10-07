
# COLLAPSING VARIABLES - PRENATAL 

# This script is used to collapse items of a similar type into a smaller number 
# of variables. The collapsing is done across type, NOT time.
# The only exception (i.e, collapsing across time) is for m_anxiety and m_depression
# that are the only variables measured twice in pregnancy.
# The second part of the script computes prenatal domain scores using the domainscore()
# function in functions.R


source('ALSPAC.0-Setup_and_functions.R') # where the repmeas() and domainscore() are defined

# Load the dataframe with dich variables created in script 1.1.Recoding_Items_prenatal.R
# and 1.4.CCEI_EPDS_calculation.R
pren <- readRDS(file.path(pathtoresults, "raw_prenatal_stress.rds"))
anxdep <- readRDS(file.path(pathtoresults,"raw_parent_depr_anxiety.rds"))

# Initiate a prenatal_stress dataframe with id of the child as first column
prenatal_stress <- data.frame("IDC" = paste(alspac.table$cidB2957, alspac.table$qlet, sep = "_"))

################################################################################
################################################################################
                            # 1. LIFE EVENTS
################################################################################

prenatal_stress$sick_or_accident_pre	<- repmeas(pren[,c('b576a_rec', 'b580a_rec', 'b610a_rec')]) 
# Admitted to hospital since PREG | V ill since PREG | Had an accident since PREG

prenatal_stress$family_member_died_pre	<-  repmeas(pren[,c('b570a_rec','b571a_rec')]) 
# PTNR died since PREG | CH died since PREG

prenatal_stress$family_member_ill_pre <-	repmeas(pren[,c('b573a_rec', 'b574a_rec')])	
# CH was ill since PREG | PTNR was ill since PREG

prenatal_stress$friend_relative_died_pre <- pren$b572a_rec # Friend or relative died since PREG 

prenatal_stress$friend_relative_ill_pre	<- pren$b575a_rec	# Friend or relative was ill since PREG

prenatal_stress$moved_pre <-	pren$b591a_rec	# Moved house since PREG

prenatal_stress$blood_loss <- pren$b599a_rec	# Bled & thought might miscarry

prenatal_stress$pregnancy_worried <-	pren$b602a_rec # Test result suggesting POSS abnormality

prenatal_stress$baby_worried	<- pren$b604a_rec	# POSS harm to baby

prenatal_stress$burglary_or_car_theft_pre <-	pren$b609a_rec	# House or car burgled since PREG

prenatal_stress$work_problems_pre <- pren$b583a_rec	# PROBS at work since PREG

prenatal_stress$abortion_pre	<- pren$b605a_rec # Tried to have abortion

prenatal_stress$married_pre <-	pren$b595a_rec	# Got married since PREG

prenatal_stress$unemployed_pre	<- pren$b584a_rec	# Lost job since PREG

################################################################################
                          # 2. CONTEXTUAL RISK
################################################################################

prenatal_stress$income_reduced_pre <-	repmeas(pren[,c('b588a_rec', 'b581a_rec')]) 
#	Income reduced since PREG | PTNR lost job since PREG

prenatal_stress$homeless_pregnancy	<- pren$b593	# Became homeless since PREG

prenatal_stress$major_financial_problems_pre <-	pren$b594a_rec # Major financial PROB since PREG

prenatal_stress$housing_adequacy_pre	<- pren$p2n # Housing adequacy 

prenatal_stress$housing_basic_living_pre	<- pren$p3n # Housing Basic Living 

prenatal_stress$housing_defects_pre	<- pren$p4n # Housing Defects 

prenatal_stress$m_education_pre <- pren$c645a_rec # Mums highest ed qualification: No degree

prenatal_stress$p_education_pre <- pren$c666a_rec # PTRN highest ed qualification: No degree

################################################################################
                          # 3. PARENTAL RISK 
################################################################################

prenatal_stress$m_criminal_record_pre <- repmeas(pren[,c('b577a_rec', 'b598', 'p14n')]) 
#	In trouble with the law since PREG | Convicted of an offence since PREG | Crime trouble with police |

prenatal_stress$p_criminal_record_pre <- repmeas(pren[,c('b586a_rec', 'pb188a_rec')]) 
# PTNR in trouble with law since PREG | PTNR convicted of an offence since PREG 

prenatal_stress$m_depression_pre <- repmeas(anxdep[,c('m_EPDS_total_18wga_rec', 'm_EPDS_total_32wga_rec')]) 
# EPDS (>12 risk, <12 no risk) 18w gest, 32w gest  # corr = 0.4286
prenatal_stress$m_anxiety_pre	<- repmeas(anxdep[,c('b351a_rec', 'c573a_rec')]) 
# CCEI anxiety subscale (complete) 18w gest, 32w gest # corr = 0.4625
prenatal_stress$m_interpersonal_sensitivity_pre <- pren$b916a_rec 
#  80th percentile (Interpersonal awareness score)

prenatal_stress$p_depression_pre <- anxdep$p_EPDS_total_18wga_rec 
# EPDS total score I partner complete cases (>12 risk, <=12 no risk)
prenatal_stress$p_anxiety_pre <- anxdep$pb233a_rec 
# CCEI anxiety subscale II partner > 8 is risk, <= 8 no risk
prenatal_stress$p_interpersonal_sensitivity_pre <- pren$pb551a_rec 
# 80th percentile (Total interpersonal sensitivity)

prenatal_stress$m_attempted_suicide_pre <-	pren$b597 # Attempted suicide since PREG

prenatal_stress$early_pregnancy <- pren$mz028ba_rec # mother age < 19 at delivery 

################################################################################
                          # 4. INTERPERSONAL RISK
################################################################################

prenatal_stress$divorce_pre <-	repmeas(pren[,c('b578', 'b587')]) 
# Divorced since PREG | Separated since PREG

prenatal_stress$conflict_in_family_pre	<- repmeas(pren[,c('b607a_rec', 'b608')]) 
# PTNR was EMOT cruel to mum since PREG | PTNR was EMOT cruel to child since PREG

prenatal_stress$conflict_family_violence_pre	<- repmeas(pren[,c('b592a_rec', 'b596a_rec')]) 
# PTNR hurt mum since PREG | PTNR hurt CH since PREG # corr = 0.124

prenatal_stress$p_rejected_child_pre <- pren$b579a_rec	# PTNR rejected PREG

prenatal_stress$p_went_away_pre	<- pren$b585a_rec #	PTNR went away since PREG

prenatal_stress$argued_fam_friends_pre	<- pren$b590a_rec	# Argued with family or friends since PREG

prenatal_stress$marital_status_pregnancy <-	pren$p7n  # Partner Status 

prenatal_stress$family_affection	<- pren$p8n	# Partner Affection 

prenatal_stress$family_size_pregnancy <- pren$p10n	# Family Size  

prenatal_stress$family_problems	<- pren$p11n # Family Major problems 

prenatal_stress$family_support	<- pren$p16n # Partner Support

prenatal_stress$social_network_emotional	<- pren$p17n	# Social Network - Emotional 

prenatal_stress$social_network_practical <- pren$p18n # Social Network - Practical 

################################################################################
################################################################################
################################################################################

# SUMMARY STATISTICS 
# Let's have a look at risk distribution and missing data per indicator

prenatal_summary <- data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 2:ncol(prenatal_stress)) {
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / nrow(prenatal_stress))*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / nrow(prenatal_stress))*100, 2)
}

################################################################################

# Calculate the percentage of missing data. Apply the percent_missing function 
# defined in functions.R to the rows (1) of the entire dataset 
prenatal_stress$pre_percent_missing <- apply(prenatal_stress[,2:ncol(prenatal_stress)],
                                            1, percent_missing)

################################################################################
################################################################################
#### -------------- create the (un-weighted) domain scores ---------------- ####
################################################################################

# ATTENTION! Here we use the default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) when missingness is < 25%. This is not the
# weighted version of the score used by Rijlaarsdam et al. (2016), you can set the 
# argument score_type to 'sum_simple' or 'sum_weighted' to change this (see 0.functions 
# script for calculation details).

prenatal_stress[,c('pre_LE_percent_missing','pre_life_events')] <- domainscore(prenatal_stress[,c(
  'family_member_died_pre',
  'friend_relative_died_pre',
  'family_member_ill_pre', 
  'friend_relative_ill_pre',
  'sick_or_accident_pre',
  'moved_pre',
  'blood_loss',
  'pregnancy_worried',
  'baby_worried',
  'burglary_or_car_theft_pre',
  'work_problems_pre',
  'abortion_pre',
  'married_pre',
  'unemployed_pre')])

prenatal_stress[,c('pre_CR_percent_missing','pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  'income_reduced_pre',
  'homeless_pregnancy',
  'major_financial_problems_pre',
  'housing_adequacy_pre',
  'housing_basic_living_pre',
  'housing_defects_pre',
  'm_education_pre', 
  'p_education_pre')]) 

prenatal_stress[,c('pre_PR_percent_missing','pre_parental_risk')] <- domainscore(prenatal_stress[,c(
  'm_criminal_record_pre',
  'p_criminal_record_pre',
  'm_attempted_suicide_pre',
  'early_pregnancy',
  'm_depression_pre',
  'm_anxiety_pre',
  'm_interpersonal_sensitivity_pre',
  'p_depression_pre',
  'p_anxiety_pre',
  'p_interpersonal_sensitivity_pre')]) 

prenatal_stress[,c('pre_IR_percent_missing','pre_interpersonal_risk')] <- domainscore(prenatal_stress[,c(
  'divorce_pre',
  'p_rejected_child_pre',
  'p_went_away_pre',
  'conflict_in_family_pre',
  'argued_fam_friends_pre',
  'conflict_family_violence_pre',
  'marital_status_pregnancy',
  'family_affection',
  'family_size_pregnancy',
  'family_problems',
  'family_support',
  'social_network_emotional',
  'social_network_practical')]) 

# compute sum scores for prenatal stress exposure ##############################
prenatal_stress$prenatal_stress <- rowSums(prenatal_stress[,c(
                                      "pre_life_events", 
                                      "pre_contextual_risk", 
                                      "pre_parental_risk", 
                                      "pre_interpersonal_risk")], na.rm = F)

################################################################################
                              # SAVE DATASET
################################################################################

# Save the dataset in the directory where you have the raw data
saveRDS(prenatal_stress, file.path(pathtoresults, "prenatal_stress.rds"))
saveRDS(prenatal_summary, file.path(pathtoresults, "prenatal_summary.rds"))

# Also save the dataset in a .csv format
# write.csv(prenatal_stress, file = "prenatal_stress.csv", row.names = F, quote = F)
# write.csv(prenatal_summary, file = "prenatal_summary.csv", row.names = T, quote = F)

