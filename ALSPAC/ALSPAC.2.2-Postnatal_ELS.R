

# COLLAPSING VARIABLES - POSTNATAL 

# This script is used to collapse items of a similar type into a smaller number 
# of variables. The collapsing is done across type, NOT time.
# The only exception (i.e, collapsing across time) is for m_anxiety and m_depression
# that are the only variables measured twice in pregnancy.
# The second part of the script computes prenatal domain scores using the domainscore()
# function in functions.R

# ATTENTION: we will not be using the 8wk timepoint in this project because it spans 
# from MID pregnancy to 8 weeks after birth, and we are interested in disentangling the 
# role of postnatal from prenatal stress, hence we need the measures to be separated. 
# If you do want to include them in the postnatal stress score, simply uncomment those lines.

source('ALSPAC.0-Setup_and_functions.R') # where the repmeas() and domainscore() are defined

# Load the dataframe with dich variables created in script 1.2.Recoding_Items_postnatal0-7.R
# 1.3.Recoding_Items_postnatal8-9.R and 1.4.CCEI_EPDS_calculation.R
postn <- readRDS(file.path(pathtoresults, "raw_postnatal_stress.rds"))
anxdep <- readRDS(file.path(pathtoresults, "raw_parent_depr_anxiety.rds"))


# Initiate a postnatal_stress dataframe with id of the child as first column
postnatal_stress <- data.frame("IDC" = paste(alspac.table$cidB2957, alspac.table$qlet, sep = "_"))

################################################################################
################################################################################
                              # 1. LIFE EVENTS
################################################################################

# ------------------------------------------------------------------------------
# SICK_OR_ACCIDENT
# ------------------------------------------------------------------------------
# postnatal_stress$sick_or_accident_8wk <- repmeas(postn[,c('e403a_rec','e431a_rec','e432a_rec')])
# Ch ill since MID PREG | Test for CH abnormality | Test suggested CH PROB.
postnatal_stress$sick_or_accident_18m <- repmeas(postn[,c('f223a_rec', 'kd510a_rec')]) 
# Ch ill since birth [8m] | Ch admitted to hospital since 6 months [18m]
postnatal_stress$sick_or_accident_30m <- repmeas(postn[,c('g303a_rec', 'kf460a_rec')])  
# One of Mums children ill since 8 months [21m] | Child admitted to hospital since 18 months [30m]
postnatal_stress$sick_or_accident_3y  <- repmeas(postn[,c('h213a_rec', 'kj470a_rec')]) 
# One of mums children ill since 18 months [2.75] | Ch admitted to hospital in te past 12 months [3.5]
postnatal_stress$sick_or_accident_4y  <- repmeas(postn[,c('j303a_rec', 'kl480a_rec')]) 
# One of mums children ill since 30 months [9.92] | Child admitted to hospital since age 3 [4.75]
postnatal_stress$sick_or_accident_5y  <- repmeas(postn[,c('k4003a_rec', 'kn4010a_rec')]) 
# Mothers child was ill in past year [5.08] | Child admitted to hospital in past 15 months [5.75]
postnatal_stress$sick_or_accident_6y  <- repmeas(postn[,c('l4003a_rec', 'kq371a_rec')]) 
# One of Respondent's children was ill | Child admitted to hospital } since 5th birthday
postnatal_stress$sick_or_accident_9y  <- repmeas(postn[,c('p2003a_rec', 'kt5011a_rec')]) 
# One of mother's children was ill since 6th birthday [9y] | Child admitted to hospital since 7th birthday [8.58]
# ------------------------------------------------------------------------------
# FAMILY_MEMBER_ILL
# ------------------------------------------------------------------------------
# postnatal_stress$family_member_ill_8wk <- repmeas(postn[,c('e404a_rec', 'e406a_rec', 'e410a_rec', 'e440a_rec')]) 
# PTNR ill | Admitted to HOSP | Mum ill | mum accident since MID PREG
postnatal_stress$family_member_ill_8m  <- repmeas(postn[,c('f224a_rec', 'f226a_rec', 'f230a_rec')]) 
# PTNR ill | Admitted to HOSP | Mum ill } since birth [8m]
postnatal_stress$family_member_ill_21m <- repmeas(postn[,c('g304a_rec', 'g306a_rec', 'g310a_rec', 'g342a_rec')]) 
# PTNR ill | Mum in hospital | Mum very ill | Mum had accident } since 8 months [1.75]
postnatal_stress$family_member_ill_3y  <- repmeas(postn[,c('h214a_rec', 'h216a_rec', 'h220a_rec', 'h252a_rec')]) 
# PTNR ill | Mum admitted to HOSP | Mum very ill | Mum had accident } since 18 months [2.75]
postnatal_stress$family_member_ill_4y  <- repmeas(postn[,c('j304a_rec', 'j306a_rec', 'j310a_rec', 'j342a_rec')]) 
# PTNR ill | Mum in hospital HOSP | Mum very ill | Mum had accident } since 30 months [3.92]
postnatal_stress$family_member_ill_5y  <- repmeas(postn[,c('k4004a_rec', 'k4006a_rec', 'k4010a_rec', 'k4042a_rec')]) 
# PTNR ill | Mum admitted to HOSP | Mum very ill | Mum had accident } in the past year [5.08]
postnatal_stress$family_member_ill_6y  <- repmeas(postn[,c('l4004a_rec', 'l4006a_rec', 'l4010a_rec', 'l4044a_rec')]) 
# Respondent's partner was ill | Respondent admitted to HOSP | Respondent very ill | Respondent had accident } since 5th birthday
postnatal_stress$family_member_ill_9y  <- repmeas(postn[,c('p2004a_rec', 'p2006a_rec', 'p2010a_rec', 'p2044a_rec')]) 
# PTNR ill | Mum admitted to HOSP | Mum very ill | Mum had accident } since 6th birthday
# ------------------------------------------------------------------------------
# SMBD_IMPORTANT_DIED
# ------------------------------------------------------------------------------
# postnatal_stress$smbd_important_died_8wk <- repmeas(postn[,c('e401a_rec', 'e402a_rec')]) 
# CH died since MID PREG | FRD or REL died since MID PREG
postnatal_stress$smbd_important_died_8m  <- repmeas(postn[,c('f221a_rec', 'f222a_rec')])
# Death of one of children | Death of friend or relative } since birth
postnatal_stress$smbd_important_died_21m <- repmeas(postn[,c('g301a_rec', 'g302a_rec')]) 
# One of Mums children died | Friend or relative died } since 8 months
postnatal_stress$smbd_important_died_3y  <- repmeas(postn[,c('h211a_rec', 'h212a_rec')]) 
# One of Mums children died | Friend or relative died } since 18 months
postnatal_stress$smbd_important_died_4y  <- repmeas(postn[,c('j301a_rec', 'j302a_rec')])
# One of Mums children died | Friend or relative died } since 30 months
postnatal_stress$smbd_important_died_5y  <- repmeas(postn[,c('k4001a_rec', 'k4002a_rec')])
# One of Mums children died | Mum's Friend or relative died } in the past year
postnatal_stress$smbd_important_died_6y  <- repmeas(postn[,c('l4001a_rec', 'l4002a_rec', 'kq366a_rec')]) 
# One of Respondent's children died | Respondent's Friend or relative died | Smbd in the family died } since 5th birthday
postnatal_stress$smbd_important_died_9y  <- repmeas(postn[,c('p2001a_rec', 'p2002a_rec', 'kt5006a_rec')])
# One of Mums children died | Mum's Friend or relative died } since 6th birthday [9y] | Someone in family died since 7th birthday [8y]
# ------------------------------------------------------------------------------
# SEPARATED_FROM_PARENT
# ------------------------------------------------------------------------------
postnatal_stress$separated_from_parent_18m <- repmeas(postn[,c('kd500a_rec', 'kd506a_rec', 'kd507a_rec')]) 
# Ch taken into care | Ch separated from mum for > 1wk | Ch separated from dad for > 1wk } since 6m
postnatal_stress$separated_from_parent_30m <- repmeas(postn[,c('kf450a_rec', 'kf456a_rec', 'kf457a_rec')]) 
# Ch taken into care | Ch separated from mum for > 1wk | Ch separated from dad for > 1wk } since 18 months 
postnatal_stress$separated_from_parent_3y  <- repmeas(postn[,c('kj460a_rec', 'kj466a_rec', 'kj467a_rec')]) 
# Ch taken into care | Ch separated from mum | Ch separated from dad } in past 12 months
postnatal_stress$separated_from_parent_4y  <- repmeas(postn[,c('kl470a_rec', 'kl476a_rec', 'kl477a_rec')]) 
# Ch taken into care | Ch separated from mum | Ch separated from dad } since age 3
postnatal_stress$separated_from_parent_5y  <- repmeas(postn[,c('kn4000a_rec', 'kn4006a_rec', 'kn4007a_rec')]) 
# Ch taken into care | Ch separated from mum | Ch separated from dad } in past 15 months
postnatal_stress$separated_from_parent_6y  <- repmeas(postn[,c('kq360a_rec', 'kq367a_rec', 'kq368a_rec')]) 
# Ch taken into care | Ch separated from mum | Ch separated from dad } since 5th birthday
postnatal_stress$separated_from_parent_8y  <- repmeas(postn[,c('kt5000a_rec', 'kt5007a_rec', 'kt5008a_rec')]) 
# Ch taken into care | Ch separated from mum | Ch separated from dad } since 7th birthday
# ------------------------------------------------------------------------------
# MOVED
# ------------------------------------------------------------------------------
# postnatal_stress$moved_8wk <- postn$e421a_rec  # Moved house since PREG
postnatal_stress$moved_18m <- repmeas(postn[,c('f241a_rec', 'kd502a_rec')]) 
# Mum moved house [8m] | Ch moved home [18m]
postnatal_stress$moved_30m <- repmeas(postn[,c('g321a_rec', 'kf452a_rec')]) 
# Mum moved house since 8 months [21m] | Ch moved home since 18 months [30m]
postnatal_stress$moved_3y  <- repmeas(postn[,c('h231a_rec', 'kj462a_rec')])
# Mum moved house since 18 months | Ch moved home in the past 12 months [3.5]
postnatal_stress$moved_4y  <- repmeas(postn[,c('j321a_rec', 'kl472a_rec')]) 
# Mum moved house since 30 months | Ch moved home since age 3
postnatal_stress$moved_5y  <- repmeas(postn[,c('k4021a_rec', 'kn4002a_rec')]) 
# Mum moved house in the past year | Ch moved home in the past 15 months
postnatal_stress$moved_6y  <- repmeas(postn[,c('l4021a_rec', 'kq362a_rec')]) 
# Respondent moved house | Ch moved home } since 5th birthday
postnatal_stress$moved_9y  <- repmeas(postn[,c('p2021a_rec', 'kt5002a_rec')])
# Mum moved house since 6th birthday [9y] | Ch moved home since 7th birthday [8y]
# ------------------------------------------------------------------------------
# PET_DIED
# ------------------------------------------------------------------------------
postnatal_stress$pet_died_18m <- repmeas(postn[,c('f261a_rec', 'kd501a_rec')])
# Pet died [8m] | Pet died [18m]
postnatal_stress$pet_died_30m <- repmeas(postn[,c('g341a_rec', 'kf451a_rec')]) 
# Pet died since 8 months [21m] | Pet died since 18 months [30m]
postnatal_stress$pet_died_3y  <- repmeas(postn[,c('h251a_rec', 'kj461a_rec')])
# Pet died since 18 months | Pet died in the past 12 months [3.5]
postnatal_stress$pet_died_4y  <- repmeas(postn[,c('j341a_rec', 'kl471a_rec')]) 
# Mum's pet died since 30 months | Pet died since age 3
postnatal_stress$pet_died_5y  <- repmeas(postn[,c('k4041a_rec', 'kn4001a_rec')])
# Mum's pet died in the past year | Child's pet died in the past 15 months
postnatal_stress$pet_died_6y  <- repmeas(postn[,c('l4043a_rec', 'kq361a_rec')]) 
# Respondent's pet died | Child's pet died } since 5th birthday
postnatal_stress$pet_died_9y  <- repmeas(postn[,c('p2043a_rec', 'kt5001a_rec')]) 
# Pet died since 6th birthday [9y] | Child's pet died since 7th birthday [8y]
# ------------------------------------------------------------------------------
# STARTED_NURSERY
# ------------------------------------------------------------------------------
postnatal_stress$started_nursery_18m <- postn$kd513a_rec	# CH started nursery since 6 months
postnatal_stress$started_nursery_30m <- postn$kf463a_rec	# CH started new creche since 18 months
postnatal_stress$started_nursery_3y	 <- postn$kj473a_rec	# CH started new creche in the past 12 months
postnatal_stress$started_nursery_4y  <- postn$kl483a_rec  # CH started new nursery/kindergarten since age 3
postnatal_stress$started_nursery_5y	 <- postn$kn4013a_rec # CH started new nursery in past 15 months
# ------------------------------------------------------------------------------
# ACQUIRED_NEW_PARENT 
# ------------------------------------------------------------------------------
postnatal_stress$acquired_new_parent_18m	<- postn$kd508a_rec  # CH acquired new parent since 6 moths
postnatal_stress$acquired_new_parent_30m	<- postn$kf458a_rec  # CH got new parent since 18 months
postnatal_stress$acquired_new_parent_3y   <- postn$kj468a_rec  # CH got new parent in the past 12 months
postnatal_stress$acquired_new_parent_4y   <- postn$kl478a_rec  # CH acquired new mother or father since age 3
postnatal_stress$acquired_new_parent_5y	  <- postn$kn4008a_rec # CH acquired new parent in past 15 months
postnatal_stress$acquired_new_parent_6y   <- postn$kq369a_rec  # CH acquired a new mother/father since 5th birthday
postnatal_stress$acquired_new_parent_8y   <- postn$kt5009a_rec # CH acquired a new mother/father since 7th birthday
# ------------------------------------------------------------------------------
# CHAGE_CARER
# ------------------------------------------------------------------------------
postnatal_stress$change_carer_18m <- postn$kd511a_rec  # CH changed carer since 6 months
postnatal_stress$change_carer_30m <- postn$kf461a_rec  # CH changed carer since 18 months
postnatal_stress$change_carer_3y	<- postn$kj471a_rec  # CH changed carer in the past 12 months
postnatal_stress$change_carer_4y	<- postn$kl481a_rec  # CH changed care taker since age 3 
postnatal_stress$change_carer_5y	<- postn$kn4011a_rec # CH 's main carer changed in past 15 months
postnatal_stress$change_carer_6y	<- postn$kq372a_rec  # CH changed care taker since 5th birthday 
postnatal_stress$change_carer_8y	<- postn$kt5012a_rec # CH changed care taker since 7th birthday 
# ------------------------------------------------------------------------------
# friend_relative_ill
# ------------------------------------------------------------------------------
# postnatal_stress$friend_relative_ill_8wk <- postn$e405a_rec  # Friend or REL ill since MID PREG
postnatal_stress$friend_relative_ill_8m  <- postn$f225a_rec  # Friend or relative ill since birth
postnatal_stress$friend_relative_ill_21m <- postn$g305a_rec  # Friend or relative ill since 8 months
postnatal_stress$friend_relative_ill_3y  <- postn$h215a_rec  # Friend or relative ill since 18 months
postnatal_stress$friend_relative_ill_4y  <- postn$j305a_rec  # Mum's friend or relative ill since 30 months
postnatal_stress$friend_relative_ill_5y  <- postn$k4005a_rec # Mum's friend or relative ill in the past year
postnatal_stress$friend_relative_ill_6y  <- postn$l4005a_rec # Respondent's friend/relative ill since 5th birthday
postnatal_stress$friend_relative_ill_9y  <- postn$p2005a_rec # Mum's friend or relative ill since 6th birthday
# ------------------------------------------------------------------------------
# PARTNER_DIED (instead of 'parent_died' in GenR, we have no mother died item) 
# ------------------------------------------------------------------------------
# postnatal_stress$partner_died_8wk <- postn$e400a_rec  # PTNR died since MID PREG
postnatal_stress$partner_died_8m  <- postn$f220a_rec  # Death of partner since birth
postnatal_stress$partner_died_21m <- postn$g300a_rec  # Partner died since 8 months
postnatal_stress$partner_died_3y  <- postn$h210a_rec  # Partner died since 18 months
postnatal_stress$partner_died_4y  <- postn$j300a_rec  # Partner died since 30 months
postnatal_stress$partner_died_5y  <- postn$k4000a_rec # Mum's partner died in past year
postnatal_stress$partner_died_6y  <- postn$l4000a_rec # Respondent's partner died  since 5th birthday
postnatal_stress$partner_died_9y  <- postn$p2000a_rec	 # Husband/partner died since 6th birthday
# ------------------------------------------------------------------------------
# BURGLARY_OR_CAR_THEFT
# ------------------------------------------------------------------------------
# postnatal_stress$burglary_or_car_theft_8wk <- postn$e439a_rec  # House burglary/car theft since MID PREG
postnatal_stress$burglary_or_car_theft_21m <- postn$g339a_rec  # Mum's house or car burgled since 8 months
postnatal_stress$burglary_or_car_theft_3y  <- postn$h249a_rec  # House or car was burgled since 18 months
postnatal_stress$burglary_or_car_theft_4y  <- postn$j339a_rec  # Mum's house or car burgled since 30 months
postnatal_stress$burglary_or_car_theft_5y  <- postn$k4039a_rec # Mum's house or car burgled in the past year
postnatal_stress$burglary_or_car_theft_6y  <- postn$l4039a_rec # Respondent's house/car burgled since 5th birthday
postnatal_stress$burglary_or_car_theft_9y  <- postn$p2039a_rec	# Mum's house or car burgled since 6th birthday
# ------------------------------------------------------------------------------
# SEPARATED_FROM_SMBD
# ------------------------------------------------------------------------------
postnatal_stress$separated_from_smbd_18m <- postn$kd512a_rec  # CH separated from someone else since 6 months
postnatal_stress$separated_from_smbd_30m <- postn$kf462a_rec  # CH sep.from somebody since 18 months
postnatal_stress$separated_from_smbd_3y  <- postn$kj472a_rec  # CH separated from someone else in the past 12 months
postnatal_stress$separated_from_smbd_4y  <- postn$kl482a_rec  # CH separated from someone else since age 3
postnatal_stress$separated_from_smbd_5y  <- postn$kn4012a_rec # CH separated from another person in past 15 months
postnatal_stress$separated_from_smbd_6y  <- postn$kq373a_rec  # CH separated from another close person since 5th birthday
postnatal_stress$separated_from_smbd_8y  <- postn$kt5013a_rec # CH separated from someone else since 7th birthday
# ------------------------------------------------------------------------------
# LOST_BEST_FRIEND ('lost_smth_important' in GenR)
# ------------------------------------------------------------------------------
postnatal_stress$lost_best_friend_8y <- postn$kt5015a_rec	# CH lost best friend since 7th birthday
# ------------------------------------------------------------------------------
# NEW_SIBLING
# ------------------------------------------------------------------------------
postnatal_stress$new_sibling_18m <- repmeas(postn[,c('f250a_rec', 'kd509a_rec')]) 
# Mum became pregnant since birth [8m] | CH had a new sibling since 6 months [18m]
postnatal_stress$new_sibling_30m <- repmeas(postn[,c('g330a_rec', 'kf459a_rec')]) 
# Mum pregnant since 8 months [21m] | CH got new sibling since 18 months [30m]
postnatal_stress$new_sibling_3y  <- repmeas(postn[,c('h240a_rec', 'kj469a_rec')]) 
# Mum became pregnant since 18 months | CH got new sibling in past 12 months
postnatal_stress$new_sibling_4y  <- repmeas(postn[,c('j330a_rec', 'kl479a_rec')]) 
# Mum became pregnant since 30 months | CH had new brother or sister since age 3
postnatal_stress$new_sibling_5y  <- repmeas(postn[,c('k4030a_rec', 'kn4009a_rec')]) 
# Mum became pregnant in the past year | CH had new brother or sister in past 15 months
postnatal_stress$new_sibling_6y  <- repmeas(postn[,c('l4030a_rec', 'kq370a_rec')]) 
# Mum became pregnant | CH had new brother or sister } since 5th birthday
postnatal_stress$new_sibling_9y  <- repmeas(postn[,c('p2030a_rec', 'kt5010a_rec')]) 
# Mum became pregnant since 6th birthday | CH had new brother or sister since 7th birthday
# ------------------------------------------------------------------------------
# CH_HAD_FRIGHT 
# ------------------------------------------------------------------------------
postnatal_stress$ch_had_fright_18m <-	postn$kd503a_rec  # CH had fright since 6 months
postnatal_stress$ch_had_fright_30m <-	postn$kf453a_rec  # CH had fright since 18 months
postnatal_stress$ch_had_fright_3y  <-	postn$kj463a_rec  # CH had shock in the past year
postnatal_stress$ch_had_fright_4y  <-	postn$kl473a_rec  # CH had shock or fright since age 3
postnatal_stress$ch_had_fright_5y  <-	postn$kn4003a_rec # CH had shock or fright in past 15 months
postnatal_stress$ch_had_fright_6y  <-	postn$kq363a_rec  # CH had shock/fright since 5th birthday
postnatal_stress$ch_had_fright_8y  <-	postn$kt5003a_rec # CH had shock or fright since 7th birthday


################################################################################
                        # 2. CONTEXTUAL RISK
################################################################################

# ------------------------------------------------------------------------------
# homeless_childhood
# ------------------------------------------------------------------------------
# postnatal_stress$homeless_childhood_8wk <- postn$e423a_rec  # Became homeless since PREG
postnatal_stress$homeless_childhood_8m  <- postn$f243a_rec  # Became homeless
postnatal_stress$homeless_childhood_21m <- postn$g323a_rec 
postnatal_stress$homeless_childhood_3y  <- postn$h233a_rec 
postnatal_stress$homeless_childhood_4y  <- postn$j323a_rec
postnatal_stress$homeless_childhood_5y  <- postn$k4023a_rec
postnatal_stress$homeless_childhood_6y  <- postn$l4023a_rec
postnatal_stress$homeless_childhood_9y  <- postn$p2023a_rec	 # Mum became homeless since 6th birthday
# ------------------------------------------------------------------------------
# major_financial_problems (trouble_pay_chidlhood in GenR)
# ------------------------------------------------------------------------------
# postnatal_stress$major_financial_problems_8wk <- postn$e424a_rec  #  Major financial PROB since MID PREG	
postnatal_stress$major_financial_problems_8m  <- postn$f244a_rec  # Major financial problems since birth
postnatal_stress$major_financial_problems_21m <- postn$g324a_rec  # Mum had money problems since 8 months
postnatal_stress$major_financial_problems_3y  <- postn$h234a_rec  # Mum had major financial problems since 18 months
postnatal_stress$major_financial_problems_4y  <- postn$j324a_rec  # Mum had major money problems since 30 months
postnatal_stress$major_financial_problems_5y  <- postn$k4024a_rec # Mum had major financial problems in the past year
postnatal_stress$major_financial_problems_6y  <- postn$l4024a_rec # Respondent had major financial problem since 5th birthday
postnatal_stress$major_financial_problems_9y  <- postn$p2024a_rec # Mum had a major financial problem since 6th birthday
# ------------------------------------------------------------------------------
# income_reduced
# ------------------------------------------------------------------------------
# postnatal_stress$income_reduced_8wk <- postn$e418a_rec  # Income reduced since MID PREG	
postnatal_stress$income_reduced_8m  <- postn$f238a_rec  # Reduced income
postnatal_stress$income_reduced_21m <- postn$g318a_rec 
postnatal_stress$income_reduced_3y  <- postn$h228a_rec
postnatal_stress$income_reduced_4y  <- postn$j318a_rec 
postnatal_stress$income_reduced_5y  <- postn$k4018a_rec 
postnatal_stress$income_reduced_6y  <- postn$l4018a_rec 
postnatal_stress$income_reduced_9y  <- postn$p2018a_rec  # Mum's income reduced since 6th birthday
# ------------------------------------------------------------------------------
# unemployed
# ------------------------------------------------------------------------------
# postnatal_stress$unemployed_8wk <- repmeas(postn[,c('e411a_rec', 'e414a_rec')]) 
#	PTNR lost job | Mum lost job since MID PREG
postnatal_stress$unemployed_8m  <- repmeas(postn[,c('f231a_rec', 'f234a_rec')])
# Partner lost job | Mum lost job } since birth
postnatal_stress$unemployed_21m <- repmeas(postn[,c('g311a_rec', 'g314a_rec')])
# PTNR lost job | Mum lost job } since 8 months
postnatal_stress$unemployed_3y  <- repmeas(postn[,c('h221a_rec', 'h224a_rec')])
# PTNR lost job | Mum lost job } since 18 months
postnatal_stress$unemployed_4y  <- repmeas(postn[,c('j311a_rec', 'j314a_rec')]) 
# PTNR lost job | Mum lost job } since 30 months
postnatal_stress$unemployed_5y  <- repmeas(postn[,c('k4011a_rec', 'k4014a_rec')])
# PTNR lost job | Mum lost job } in the past year
postnatal_stress$unemployed_6y  <- repmeas(postn[,c('l4011a_rec', 'l4014a_rec')])
# Respondent's PTNR lost job | Respondent lost job } since 5th birthday
postnatal_stress$unemployed_9y  <- repmeas(postn[,c('p2011a_rec', 'p2014a_rec')]) 
# PTNR lost job | Mum lost job } since 6th birthday
# ------------------------------------------------------------------------------
# Housing adequacy
# ------------------------------------------------------------------------------
postnatal_stress$housing_adequacy_2y     <- postn$b2n  # Housing adequacy 0-2y composite
postnatal_stress$housing_adequacy_4y     <- postn$t2n  # Housing adequacy 2-4y composite
# ------------------------------------------------------------------------------
# Housing Basic Living
# ------------------------------------------------------------------------------
postnatal_stress$housing_basic_living_2y <-	postn$b3n  # Housing Basic Living 0-2y  composite
postnatal_stress$housing_basic_living_4y <-	postn$t3n  # Housing Basic Living 2-4y composite
# ------------------------------------------------------------------------------
# Housing Defects
# ------------------------------------------------------------------------------
postnatal_stress$housing_defects_2y	     <- postn$b4n  # Housing Defects 0-2y composite
postnatal_stress$housing_defects_4y	     <- postn$t4n  # Housing Defects 2-4y composite
# ------------------------------------------------------------------------------
# m_education 
# ------------------------------------------------------------------------------
postnatal_stress$m_education <- postn$m_ed # Mum's highest education < degree
# ------------------------------------------------------------------------------
# p_education 
# ------------------------------------------------------------------------------
postnatal_stress$p_education <- postn$p_ed # PTNR's highest education < degree
# ------------------------------------------------------------------------------
# neighbourhood_problems
# ------------------------------------------------------------------------------
postnatal_stress$neighbourhood_problems_21m <- postn$g496a_rec 
postnatal_stress$neighbourhood_problems_3y  <- postn$h366a_rec 


################################################################################
                            # 3. PARENTAL RISK 
################################################################################

# ------------------------------------------------------------------------------
# work_problems
# ------------------------------------------------------------------------------
# postnatal_stress$work_problems_8wk <- repmeas(postn[,c('e412a_rec', 'e413a_rec')])
# PTNR had PROBS at work | PROBS at work since MID PREG
postnatal_stress$work_problems_8m  <- repmeas(postn[,c('f232a_rec', 'f233a_rec')])
# Work problems for partner | Work problems for Mum
postnatal_stress$work_problems_21m <- repmeas(postn[,c('g312a_rec', 'g313a_rec')]) 
# PTNR had PROBS with work | Mum had PROBS with work } since 8 months
postnatal_stress$work_problems_3y  <- repmeas(postn[,c('h222a_rec', 'h223a_rec')])
# PTNR had PROBS at work | Mum had PROBS ay work } since 18 months
postnatal_stress$work_problems_4y  <- repmeas(postn[,c('j312a_rec', 'j313a_rec')]) 
# PTNR had PROBS at work | Mum had PROBS ay work } since 30 months
postnatal_stress$work_problems_5y  <- repmeas(postn[,c('k4012a_rec', 'k4013a_rec')]) 
# PTNR had PROBS at work | Mum had PROBS ay work } in the past year
postnatal_stress$work_problems_6y  <- repmeas(postn[,c('l4012a_rec', 'l4013a_rec')]) 
# Respondent's PTNR had PROBS at work | Respondent had PROBS ay work } since 5th birthday
postnatal_stress$work_problems_9y  <- repmeas(postn[,c('p2012a_rec', 'p2013a_rec')]) 
# PTNR had PROBS at work | Mum had PROBS ay work } since 6th birthday
# ------------------------------------------------------------------------------
# criminal_record_parent
# ------------------------------------------------------------------------------
# postnatal_stress$criminal_record_parent_8wk <- repmeas(postn[,c('e407a_rec', 'e416a_rec', 'e428a_rec')]) 
# Mum in trouble with law | PTNR in trouble with law | Court conviction } since MID PREG
postnatal_stress$criminal_record_parent_8m  <- repmeas(postn[,c('f227a_rec', 'f236a_rec', 'f249a_rec')])
# Mum in trouble with law | PTNR in trouble with law | Court conviction
postnatal_stress$criminal_record_parent_21m <- repmeas(postn[,c('g307a_rec', 'g316a_rec', 'g329a_rec')])

postnatal_stress$criminal_record_parent_3y  <- repmeas(postn[,c('h217a_rec', 'h226a_rec', 'h239a_rec')])

postnatal_stress$criminal_record_parent_4y  <- repmeas(postn[,c('j307a_rec', 'j316a_rec', 'j329a_rec')])

postnatal_stress$criminal_record_parent_5y  <- repmeas(postn[,c('k4007a_rec', 'k4016a_rec', 'k4029a_rec')])

postnatal_stress$criminal_record_parent_6y  <- repmeas(postn[,c('l4007a_rec', 'l4016a_rec', 'l4029a_rec')])

postnatal_stress$criminal_record_parent_9y  <- repmeas(postn[,c('p2007a_rec', 'p2016a_rec', 'p2029a_rec')]) 
# Mum in trouble with law | PTNR/husband in trouble with law | Mum convicted of an offence } since 6th birthday
# ------------------------------------------------------------------------------
# miscarriage_or_abortion
# ------------------------------------------------------------------------------
# postnatal_stress$miscarriage_or_abortion_8wk <- postn$e435a_rec # Attempted abortion since MID PREG
postnatal_stress$miscarriage_or_abortion_8m  <- repmeas(postn[,c('f254a_rec', 'f253a_rec')]) 
# Mum had abortion | Mum had miscarriage
postnatal_stress$miscarriage_or_abortion_21m <- repmeas(postn[,c('g334a_rec', 'g333a_rec')])
# Mum had abortion | Mum had miscarriage } since 8 months
postnatal_stress$miscarriage_or_abortion_3y  <- repmeas(postn[,c('h244a_rec', 'h243a_rec')]) 
# Mum had abortion | Mum had miscarriage } since 18 months
postnatal_stress$miscarriage_or_abortion_4y  <- repmeas(postn[,c('j334a_rec', 'j333a_rec')])
# Mum had abortion | Mum had miscarriage } since 30 months
postnatal_stress$miscarriage_or_abortion_5y  <- repmeas(postn[,c('k4034a_rec', 'k4033a_rec')])
# Mum had abortion | Mum had miscarriage } in the past year
postnatal_stress$miscarriage_or_abortion_6y  <- repmeas(postn[,c('l4034a_rec', 'l4033a_rec')]) 
# Respondent had abortion | Respondent had miscarriage } since 5th birthday
postnatal_stress$miscarriage_or_abortion_9y  <- repmeas(postn[,c('p2034a_rec', 'p2033a_rec')]) 
# Mum had abortion | Mum had miscarriage } since 6th birthday
# ------------------------------------------------------------------------------
# m_attempted_suicide
# ------------------------------------------------------------------------------
# postnatal_stress$m_attempted_suicide_8wk <- postn$e427a_rec  # Attempted suicide since MID PREG
postnatal_stress$m_attempted_suicide_8m  <- postn$f248a_rec  # Attempted suicide
postnatal_stress$m_attempted_suicide_21m <- postn$g328a_rec
postnatal_stress$m_attempted_suicide_3y  <- postn$h238a_rec
postnatal_stress$m_attempted_suicide_4y  <- postn$j328a_rec
postnatal_stress$m_attempted_suicide_5y  <- postn$k4028a_rec
postnatal_stress$m_attempted_suicide_6y  <- postn$l4028a_rec
postnatal_stress$m_attempted_suicide_9y  <- postn$p2028a_rec  # Mum attempted suicide since 6th birthday
# ------------------------------------------------------------------------------
# m_age
# ------------------------------------------------------------------------------
postnatal_stress$m_age <- postn$mz028ba_rec # mother younger than 19 at baseline
# ------------------------------------------------------------------------------
# p_age 
# ------------------------------------------------------------------------------
postnatal_stress$p_age <- postn$pb910a_rec # partner younger than 19 at baseline 
# ------------------------------------------------------------------------------
# m_depression
# ------------------------------------------------------------------------------
# When both EPDS and self reported depression were available we combined both else 
# we used the available information source
# postnatal_stress$m_depression_8wk <- anxdep$e391a_rec  # EPDS total score
postnatal_stress$m_depression_8m  <- repmeas(anxdep[,c('f021a_rec', 'f200a_rec')]) # had depression since birth | EPDS total score
postnatal_stress$m_depression_21m <- repmeas(anxdep[,c('g021a_rec', 'g290a_rec')]) # had depression | EPDS total score
postnatal_stress$m_depression_3y  <- repmeas(anxdep[,c('h013a_rec', 'h200a_rec')]) # had depression | EPDS total score
postnatal_stress$m_depression_4y  <- anxdep$j012a_rec # had depression
postnatal_stress$m_depression_5y  <- repmeas(anxdep[,c('k1011a_rec', 'm_EPDS_total_5ya_rec')]) # had depression | EPDS total score
postnatal_stress$m_depression_6y  <- anxdep$l3011a_rec # had depression
postnatal_stress$m_depression_9y  <- repmeas(anxdep[,c('p1011a_rec', 'm_EPDS_total_8ya_rec')]) # had depression [9y] | EPDS total score [8y]
# ------------------------------------------------------------------------------
# p_depression (lots of NAs)
# ------------------------------------------------------------------------------
# When both EPDS and self reported depression were available we combined both else 
# we used the available information source
# postnatal_stress$p_depression_8wk <- anxdep$pc103a_rec  #  EPDS total score 
postnatal_stress$p_depression_8m  <- repmeas(anxdep[,c('pd021a_rec', 'pd200a_rec')])  # had depression since birth | EPDS total score 
postnatal_stress$p_depression_21m <- repmeas(anxdep[,c('pe021a_rec', 'pe290a_rec')])  # had depression | EPDS total score
postnatal_stress$p_depression_3y  <- repmeas(anxdep[,c('pf1011a_rec', 'p_EPDS_total_3ya_rec')]) # had depression | EPDS total score
postnatal_stress$p_depression_4y  <- anxdep$pg1011a_rec  # had depression
postnatal_stress$p_depression_5y  <- repmeas(anxdep[,c('ph1011a_rec', 'p_EPDS_total_5ya_rec')]) # had depression | EPDS total score
postnatal_stress$p_depression_6y  <- repmeas(anxdep[,c('pj3011a_rec', 'p_EPDS_total_6ya_rec')]) # had depression | EPDS total score
postnatal_stress$p_depression_9y  <- repmeas(anxdep[,c('pl1061a_rec', 'pm1011a_rec')])  # had depression [9y] | had depression [8y]
# ------------------------------------------------------------------------------
# m_anxiety
# ------------------------------------------------------------------------------
# Maternal anxiety is based on the CCEI anxiety subscale score
# postnatal_stress$m_anxiety_8wk <- anxdep$e371a_rec  # CCEI anxiety subscale (complete)
postnatal_stress$m_anxiety_8m  <- anxdep$f173a_rec  # CCEI anxiety subscale (complete)
postnatal_stress$m_anxiety_21m <- anxdep$g268a_rec  # CCEI anxiety subscale (complete)
postnatal_stress$m_anxiety_3y  <- anxdep$h178a_rec  # CCEI anxiety subscale (complete)
postnatal_stress$m_anxiety_5y  <- anxdep$CCEI_total_5ya_rec # CCEI anxiety subscale (complete)
postnatal_stress$m_anxiety_6y  <- anxdep$CCEI_total_6ya_rec # CCEI anxiety subscale (complete)
# ------------------------------------------------------------------------------
# p_anxiety
# ------------------------------------------------------------------------------
# Paternal anxiety is based on self reports
postnatal_stress$p_anxiety_8m  <- anxdep$pd020a_rec  # had anxiety/nerves
postnatal_stress$p_anxiety_21m <- anxdep$pe020a_rec  # had anxiety/nerves
postnatal_stress$p_anxiety_3y  <- anxdep$pf1010a_rec # had anxiety/nerves
postnatal_stress$p_anxiety_4y  <- anxdep$pg1010a_rec # had anxiety/nerves
postnatal_stress$p_anxiety_5y  <- anxdep$ph1010a_rec # had anxiety/nerves
postnatal_stress$p_anxiety_6y  <- anxdep$pj3010a_rec # had anxiety/nerves
postnatal_stress$p_anxiety_9y  <- anxdep$pm1010a_rec # had anxiety/nerves


################################################################################
                          # 4. INTERPERSONAL RISK
################################################################################

# ------------------------------------------------------------------------------
# Divorce
# ------------------------------------------------------------------------------
# postnatal_stress$divorce_8wk <- postn$e417a_rec  # Separated since MID PREG
postnatal_stress$divorce_8m	 <- repmeas(postn[,c('f228a_rec', 'f237a_rec')])  # Divorce | Separated from partner } since birth
postnatal_stress$divorce_21m <- repmeas(postn[,c('g308a_rec', 'g317a_rec')])  # Mum divorced | Mum and partner separated } since 8 months
postnatal_stress$divorce_3y	 <- repmeas(postn[,c('h218a_rec', 'h227a_rec')])  # Divorced | Mum and partner separated } since 18 months
postnatal_stress$divorce_4y  <- repmeas(postn[,c('j308a_rec', 'j317a_rec')])  # Mum divorced | Mum and partner separated } since 30 months
postnatal_stress$divorce_5y	 <- repmeas(postn[,c('k4008a_rec','k4017a_rec')]) # Mum divorced | Mum and partner separated } in the past year
postnatal_stress$divorce_6y	 <- repmeas(postn[,c('l4008a_rec','l4017a_rec')]) # Respondent divorced | Respondent separated partner } since 5th birthday
postnatal_stress$divorce_9y	 <- repmeas(postn[,c('p2008a_rec','p2017a_rec')]) # Mum divorced | Mum and partner/husband separated } since 6th birthday
# ------------------------------------------------------------------------------
# Partner rejected child 
# ------------------------------------------------------------------------------
# postnatal_stress$p_rejected_child_8wk	<- postn$e409a_rec	 # PTNR rejected CH since MID PREG
postnatal_stress$p_rejected_child_8m	<- postn$f229a_rec	# CH not wanted by partner
postnatal_stress$p_rejected_child_21m	<- postn$g309a_rec	# PTNR rejected CH since 8 months
postnatal_stress$p_rejected_child_3y	<- postn$h219a_rec	# PTNR rejected children since 18 months
postnatal_stress$p_rejected_child_4y	<- postn$j309a_rec	# Mum found PTNR does not want CH since 30 months
postnatal_stress$p_rejected_child_5y	<- postn$k4009a_rec # Mum found PTNR does not want CH in the past year
postnatal_stress$p_rejected_child_6y	<- postn$l4009a_rec # Respondent found PTNR does not want CH since 5th birthday
postnatal_stress$p_rejected_child_9y	<- postn$p2009a_rec # Mum found PTNR/husband does not want CH since 6th birthday
# ------------------------------------------------------------------------------
# Partner went away
# ------------------------------------------------------------------------------
# postnatal_stress$p_went_away_8wk <- postn$e415a_rec	# PTNR went away since MID PREG
postnatal_stress$p_went_away_8m	 <- postn$f235a_rec  # PTNR went away
postnatal_stress$p_went_away_21m <- postn$g315a_rec	 # PTNR went away since 8 months
postnatal_stress$p_went_away_3y	 <- postn$h225a_rec	 # PTNR went away since 18 months
postnatal_stress$p_went_away_4y	 <- postn$j315a_rec	 # PTNR went away since 30 months
postnatal_stress$p_went_away_5y	 <- postn$k4015a_rec # Mum's PTNR went away in the past year
postnatal_stress$p_went_away_6y	 <- postn$l4015a_rec # Respondent's PTNR went away since 5th birthday
postnatal_stress$p_went_away_9y	 <- postn$p2015a_rec # Mum's PTNR/husband went away since 6th birthday
# ------------------------------------------------------------------------------
# conflict_in_family
# ------------------------------------------------------------------------------
# postnatal_stress$conflict_in_family_8wk <- postn$e437a_rec  # PTNR EMOT cruel to Mum since MID PREG
# postnatal_stress$conflict_in_family_8m  <- postn$f239a_rec  # Argued with partner
postnatal_stress$conflict_in_family_21m <- postn$g336a_rec  # PTNR EMOT cruel to Mum since 8 months
postnatal_stress$conflict_in_family_3y  <- postn$h246a_rec  # PTNR EMOT cruel to Mum since 18 months
postnatal_stress$conflict_in_family_4y  <- postn$j336a_rec  # PTNR EMOT cruel to Mum since 30 months
postnatal_stress$conflict_in_family_5y  <- postn$k4036a_rec # PTNR EMOT cruel to Mum in the past year
postnatal_stress$conflict_in_family_6y  <- postn$l4036a_rec # Respondent's PTNR EMOT cruel to her/him since 5th birthday
postnatal_stress$conflict_in_family_9y  <- postn$p2036a_rec # PTNR EMOT cruel to Mum since 6th birthday
# ------------------------------------------------------------------------------
# conflict_family_violence
# ------------------------------------------------------------------------------
# postnatal_stress$conflict_family_violence_8wk <- postn$e422a_rec  # PTNR hurt Mum since MID PREG
postnatal_stress$conflict_family_violence_8m  <- postn$f242a_rec  # Physically hurt by PTNR
postnatal_stress$conflict_family_violence_21m <- repmeas(postn[,c('g322a_rec', 'g712a_rec', 'g713a_rec')]) 
# PTNR physically cruel to Mum since 8 months | Mum/PTNR hit or slapped one another | Mum/PTNR threw something in anger
postnatal_stress$conflict_family_violence_3y  <- repmeas(postn[,c('h232a_rec', 'h581a_rec')]) 
# PTNR physically cruel to Mum since 18 months | Mum/PTNR hit or slapped one another
postnatal_stress$conflict_family_violence_4y  <- postn$j322a_rec  # PTNR physically cruel to Mum since 30 months
postnatal_stress$conflict_family_violence_5y  <- postn$k4022a_rec # PTNR physically cruel to Mum in the past year
postnatal_stress$conflict_family_violence_6y  <- postn$l4022a_rec # PTNR physically cruel to Mum since 5th birthday
postnatal_stress$conflict_family_violence_9y  <- repmeas(postn[,c('p2022a_rec', 'p3154a_rec', 'p3155a_rec')]) 
# PTNR physically cruel to Mum since 6th birthday | Mum/PTNR hit or slapped one another | Mum/PTNR threw or broke things } in the past 3 months
# ------------------------------------------------------------------------------
# m_new_partner
# ------------------------------------------------------------------------------
# postnatal_stress$m_new_partner_8wk <- postn$e425a_rec  # Married since MID PREG
postnatal_stress$m_new_partner_8m  <- postn$f245a_rec  # Got married
postnatal_stress$m_new_partner_21m <- postn$g325a_rec  # Mum got married since 8 months
postnatal_stress$m_new_partner_3y  <- postn$h235a_rec  # Mum got married since 18 months
postnatal_stress$m_new_partner_4y  <- postn$j325a_rec  # Mum got married since 30 months
postnatal_stress$m_new_partner_5y  <- postn$k4025a_rec # Mum got married in the past year
postnatal_stress$m_new_partner_6y  <- repmeas(postn[,c('l4025a_rec', 'l4040a_rec')]) 
# Respondent got married | Respondent found new partner } since 5th birthday
postnatal_stress$m_new_partner_9y  <- repmeas(postn[,c('p2025a_rec', 'p2040a_rec')]) 
# Mum got married | Mum found new partner } since 6th birthday
# ------------------------------------------------------------------------------
# argued_fam_friends
# ------------------------------------------------------------------------------
# postnatal_stress$argued_fam_friends_8wk <- postn$e420a_rec  # Argued with FAM/FRDS since MID PREG
postnatal_stress$argued_fam_friends_8m  <- postn$f240a_rec  # Argued with FAM/FRDS
postnatal_stress$argued_fam_friends_21m <- postn$g320a_rec  # Mum argued with FAM/FRDS since 8 months
postnatal_stress$argued_fam_friends_3y  <- postn$h230a_rec  # Mum argued with FAM/FRDS since 18 months
postnatal_stress$argued_fam_friends_4y  <- postn$j320a_rec  # Mum argued with FAM/FRDS since 30 months
postnatal_stress$argued_fam_friends_5y  <- postn$k4020a_rec # Mum argued with FAM/FRDS in the past year
postnatal_stress$argued_fam_friends_6y  <- postn$l4020a_rec # Respondent argued with FAM/FRDS since 5th birthday
postnatal_stress$argued_fam_friends_9y  <- postn$p2020a_rec # Mum argued with FAM/FRDS since 6th birthday


################################################################################
                          # 5. DIRECT VICTIMISATION
################################################################################

# ------------------------------------------------------------------------------
# bullying
# ------------------------------------------------------------------------------
postnatal_stress$bullying_8y <- repmeas(postn[,c('f8fp475a_rec', 'f8fp470a_rec')]) 
# Bullying, CH is relational victim | Bullying, CH is overt victim 
# ------------------------------------------------------------------------------
# physical_violence	
# ------------------------------------------------------------------------------
postnatal_stress$physical_violence_18m <- postn$kd504a_rec  # CH physically hurt by someone
postnatal_stress$physical_violence_30m <- postn$kf454a_rec  # CH physically hurt by someone since 18 months
postnatal_stress$physical_violence_3y  <- postn$kj464a_rec  # CH physically hurt in the past 12 months
postnatal_stress$physical_violence_4y  <- postn$kl474a_rec  # CH physically hurt by someone since age 3
postnatal_stress$physical_violence_5y  <- postn$kn4004a_rec # CH physically hurt by someone in past 15 months
postnatal_stress$physical_violence_6y  <- postn$kq364a_rec  # CH physically hurt by someone since 5th birthday
postnatal_stress$physical_violence_8y  <- postn$kt5004a_rec # CH physically hurt by someone since 7th birthday [8y]  
# NOT: repmeas(postn[,c('kt5004a_rec', 'ku298a_rec')]) i.e., | Child is hit or slapped [9y]
# ------------------------------------------------------------------------------
# sexual_abuse
# ------------------------------------------------------------------------------
postnatal_stress$sexual_abuse_18m <- postn$kd505a_rec  # CH sexually abused since 6 months
postnatal_stress$sexual_abuse_30m <- postn$kf455a_rec  # CH sexually abused since 18 months
postnatal_stress$sexual_abuse_3y  <- postn$kj465a_rec  # CH sexually abused in the past 12 months
postnatal_stress$sexual_abuse_4y  <- postn$kl475a_rec  # CH sexually abused since age 3
postnatal_stress$sexual_abuse_5y  <- postn$kn4005a_rec # CH sexually abused in past 15 months
postnatal_stress$sexual_abuse_6y  <- postn$kq365a_rec  # CH sexually abused since 5th birthday
postnatal_stress$sexual_abuse_8y  <- postn$kt5005a_rec # CH sexually abused since 7th birthday
# ------------------------------------------------------------------------------
# p_cruelty_physical
# ------------------------------------------------------------------------------
# postnatal_stress$p_cruelty_physical_8wk <- postn$e426a_rec  # PTNR hurt children since MID PREG
postnatal_stress$p_cruelty_physical_8m  <- postn$f246a_rec  # PTNR physically cruel to children
postnatal_stress$p_cruelty_physical_21m <- postn$g326a_rec  # PTNR physically cruel to children since 8 months
postnatal_stress$p_cruelty_physical_3y  <- postn$h236a_rec  # PTNR physically cruel to children since 18 months
postnatal_stress$p_cruelty_physical_4y  <- postn$j326a_rec  # PTNR physically cruel to children since 30 months
postnatal_stress$p_cruelty_physical_5y  <- postn$k4026a_rec # PTNR physically cruel to children in the past year
postnatal_stress$p_cruelty_physical_6y  <- postn$l4026a_rec # PTNR physically cruel to children since 5th birthday
postnatal_stress$p_cruelty_physical_9y  <- postn$p2026a_rec # PTNR physically cruel to children sicne 6th birthday
# ------------------------------------------------------------------------------
# m_cruelty_physical
# ------------------------------------------------------------------------------
postnatal_stress$m_cruelty_physical_8m  <- postn$f247a_rec  # Mum physically cruel to children
postnatal_stress$m_cruelty_physical_21m <- postn$g327a_rec  # Mum physically cruel to children since 8 months
postnatal_stress$m_cruelty_physical_3y  <- postn$h237a_rec  # Mum physically cruel to children since 18 months
postnatal_stress$m_cruelty_physical_4y  <- postn$j327a_rec  # Mum physically cruel to children since 30 months
postnatal_stress$m_cruelty_physical_5y  <- postn$k4027a_rec # Mum physically cruel to children in the past year
postnatal_stress$m_cruelty_physical_6y  <- postn$l4027a_rec # Respondent physically cruel to children since 5th birthday
postnatal_stress$m_cruelty_physical_9y  <- postn$p2027a_rec # Mum physically cruel to children since 6th birthday
# ------------------------------------------------------------------------------
# p_cruelty_emotional
# ------------------------------------------------------------------------------
# postnatal_stress$p_cruelty_emotional_8wk <- postn$e438a_rec  # PTNR EMOT cruel to CH since MID PREG
postnatal_stress$p_cruelty_emotional_8m  <- postn$f257a_rec  # PTNR emotionally cruel to children
postnatal_stress$p_cruelty_emotional_21m <- postn$g337a_rec  # PTNR emotionally cruel to children since 8 months
postnatal_stress$p_cruelty_emotional_3y  <- postn$h247a_rec  # PTNR emotionally cruel to children since 18 months
postnatal_stress$p_cruelty_emotional_4y  <- postn$j337a_rec  # PTNR emotionally cruel to children since 30 months
postnatal_stress$p_cruelty_emotional_5y  <- postn$k4037a_rec # PTNR emotionally cruel to children in the past year
postnatal_stress$p_cruelty_emotional_6y  <- postn$l4037a_rec # Respondent's PTNR emotionally cruel to children since 5th birthday
postnatal_stress$p_cruelty_emotional_9y  <- postn$p2037a_rec # PTNR/husband emotionally cruel to children since 6th birthday
# ------------------------------------------------------------------------------
# m_cruelty_emotional
# ------------------------------------------------------------------------------
postnatal_stress$m_cruelty_emotional_8m  <- postn$f258a_rec  # Mum emotionally cruel to children since birth
postnatal_stress$m_cruelty_emotional_21m <- postn$g338a_rec  # Mum emotionally cruel to children since 8 months
postnatal_stress$m_cruelty_emotional_3y  <- postn$h248a_rec  # Mum emotionally cruel to children since 18 months
postnatal_stress$m_cruelty_emotional_4y  <- postn$j338a_rec  # Mum emotionally cruel to children since 30 months
postnatal_stress$m_cruelty_emotional_5y  <- postn$k4038a_rec # Mum emotionally cruel to children in the past year
postnatal_stress$m_cruelty_emotional_6y  <- postn$l4038a_rec # Respondent emotionally cruel to children since 5th birthday
postnatal_stress$m_cruelty_emotional_9y  <- postn$p2038a_rec # Mum emotionally cruel to children since 6th birthday
 

################################################################################
################################################################################
################################################################################

# SUMMARY STATISTICS 
# Let's have a look at risk distribution and missing data per indicator

postnatal_summary <- data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 2:ncol(postnatal_stress)) {
  s = summary(as.factor(postnatal_stress[,i]))
  c = colnames(postnatal_stress)[i]
  postnatal_summary[1:3,c] <- s
  postnatal_summary[4,c] <- round((postnatal_summary[2,c] / nrow(postnatal_stress))*100, 2)
  postnatal_summary[5,c] <- round((postnatal_summary[3,c] / nrow(postnatal_stress))*100, 2)
}

################################################################################

# Calculate the percentage of missing data. Apply the percent_missing function 
# defined in functions.R to the rows (1) of the entire dataset 
postnatal_stress$post_percent_missing <- apply(postnatal_stress[,2:ncol(postnatal_stress)],
                                             1, percent_missing)

################################################################################
################################################################################
#### -------------- create the (un-weighted) domain scores ---------------- ####
################################################################################

# ATTENTION! Here we use the domainscore() function: calculating  a *mean domain score*
# that is weighted by the number of different types of stressors (i.e. ranging from 0 to >1)
# when missingness *across time* is < 25%. This different compared to GenR postnatal 
# ELS score and both GenR and ALSPAC prenatal ELS scores( please see 0.functions 
# script for calculation details).
# Add the argument check_corrs = T if you want to see the correlation between timepoints

postnatal_stress[,c('post_LE_percent_missing','post_life_events')] <- domainscore(
  c('sick_or_accident',
    'family_member_ill',
    'smbd_important_died',
    'separated_from_parent',
    'moved',
    'pet_died',
    'started_nursery',
    'acquired_new_parent',
    'chage_carer',
    'friend_relative_ill',
    'partner_died',
    'burglary_or_car_theft',
    'separated_from_smbd',
    'lost_best_friend',
    'new_sibling',
    'ch_had_fright'), postnatal = T, save_cor_mat = "le_mat.csv")

postnatal_stress[,c('post_CR_percent_missing','post_contextual_risk')] <- domainscore(
  c('homeless_childhood',
    'major_financial_problems' ,
    'income_reduced',
    'unemployed',
    'housing_adequacy',
    'housing_basic_living',
    'housing_defects',
    'm_education' ,
    'p_education' ,
    'neighbourhood_problems'), postnatal = T, save_cor_mat = "cr_mat.csv")

postnatal_stress[,c('post_PR_percent_missing','post_parental_risk')] <- domainscore( 
  c('work_problems',
    'criminal_record_parent',
    'miscarriage_or_abortion',
    'm_attempted_suicide',
    'm_age',
    'p_age',
    'm_depression',
    'p_depression',
    'm_anxiety',
    'p_anxiety'), postnatal = T, save_cor_mat = "pr_mat.csv")

postnatal_stress[,c('post_IR_percent_missing','post_interpersonal_risk')] <- domainscore( 
  c('divorce',
    'p_rejected_child',
    'p_went_away',
    'conflict_in_family',
    'conflict_family_violence',
    'm_new_partner',
    'argued_fam_friends'),  postnatal = T, save_cor_mat = "ir_mat.csv")

postnatal_stress[,c('post_DV_percent_missing','post_direct_victimization')] <- domainscore(
  c('bullying',
    'physical_violence'	,
    'sexual_abuse',
    'p_cruelty_physical',
    'm_cruelty_physical',
    'p_cruelty_emotional',
    'm_cruelty_emotional'),  postnatal = T, save_cor_mat = "dv_mat.csv")

# compute sum scores for postnatal stress exposure #############################

postnatal_stress$postnatal_stress <- rowSums(postnatal_stress[,c( 
                                       "post_life_events", 
                                       "post_contextual_risk", 
                                       "post_parental_risk", 
                                       "post_interpersonal_risk", 
                                       "post_direct_victimization")], na.rm = F)

################################################################################
                              # SAVE DATASET
################################################################################

# Save the dataset in the directory where you have the raw data
saveRDS(postnatal_stress, file.path(pathtoresults, "postnatal_stress.rds"))
saveRDS(postnatal_summary, file.path(pathtoresults, "postnatal_summary.rds"))

# Also save the dataset in a .csv format
# write.csv(postnatal_stress, file = "postnatal_stress.csv", row.names = F, quote = F)
# write.csv(postnatal_summary, file = "postnatal_summary.csv", row.names = T, quote = F)
