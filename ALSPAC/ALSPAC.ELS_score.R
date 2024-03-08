# devtools::install_github("SereDef/ALSPAC.helpR")
library(ALSPAC.helpR)
library(dplyr)
library(UpSetR)

filepath <- './ALSPAC_AFAR/Cecil_B4195_07Dec23_2.sav' # EarlyCause_AHupdated_CIDB2957_21OCT21.sav
  
data <- load_alspac( # filepath or nothing (for interactive window)
                    lower.case = TRUE,
                    keep.value.labels = TRUE,
                    load.metadata = FALSE)

# --------------
# Because the labels used in factors vary quite a bit across variables, we define 
# manually all levels to be recoded a 1 (yes) and 0 (no)
yes = c('Yes','1',
        'affected a lot','fairly affected','mildly affected','N effect at all','N affect at all', # pregnancy
        'Y much affected','Y MOD affected','Y mildly affected','Y but N effect', # 8m [f]
        'Yes & CH Very Upset','Yes & CH Quite Upset','Yes & CH Bit Upset','Yes & CH Not Upset', # 18m [kd]
        'Yes Big Effect','Yes Some Effect','Yes Mild Effect','Yes No Effect', # 21m [g]
        'yes child very upset','yes quite upset', 'yes bit upset', 'yes not upset', # 30m [kf]
        'yes had big effect','yes medium effect','yes mild effect','yes but no effect', # 3y [h]
        'Yes CH Very Upset', 'Yes Quite Upset', 'Yes Bit Upset', 'Yes Not Upset', # 3y [kj]
        'Yes and affected a lot','Yes, moderately affected','Yes, mildly affected','Yes, not affect at all', # 4y [j]
        'Yes, child was very upset','Yes, child was quite upset','Yes, child was a bit upset','Yes, child was not upset', # 4y [kl]
        'Yes, affected a lot','Yes, did not affect at all', # 5y [k]
        'Yes And Was Very Upset','Yes And Was Quite Upset','Yes And Was A Bit Upset','Yes But Was Not Upset', # 5y [kn]
        'Yes & affected respondent a lot','Yes, did not affect respondent at all', # 6y [l]
        'Yes, very upset', 'Yes, quite upset', 'Yes, a bit upset','Yes, bit upset', 'Yes, not upset', # 6y [kq] and 8y [kt]
        'Yes, when the study child was 6 or 7','Yes, since the study child\'s 8th birthday','Yes, both when the study child was 6/7 and 8+', # 9y [p]
        # marital status
        'Never married','Never Married','never married','widowed','Widowed','Divorced','divorced','Separated','separated', 
        # housing conditions 
        'V serious','Very Serious','very serious','Y seriously','Serious','Fairly serious','fairly serious',
        'Serious leak', 'Y slightly','Slight','Slight leak', 
        'Y often','Yes Frequently','yes frequently','Frequently','Occasionally','Yes Occasionally','yes occasionally',
        # Arguments with partner
        'Yes mum did','Yes partner did','Yes both did',
        'Yes mum Did','Yes Partner Did','Yes both Did',
        'Yes, mother did this','Yes, partner did this','Yes, both did this',
        # Physical abuse
        'Every day','SEV times WK','C once WK','Rarely',
        'Several times a week','Once or twice a week','Once or twice a month')

no = c('No','0',
       'N did not happen','Did Not Happen','didnt happen','No, did not happen','no didnt happen','No didnt Happen','No Did Not Happen',
       'No, did not happen in past 3 years',
       # marital status
       '1st marriage','1ST marriage','Married Once','Marriage 2 or 3','marriage 2 or 3','Married > Once','2ND+ marriage',
       # housing conditions 
       'No Do Not Have','N','None','N serious','Not serious','Not Serious','not serious',
       'No Mould','no mould','No leak','No roof','Doesn\'t apply',
       # Arguments with partner / physical abuse / other
       'Never','Not at all','No Not at All','not at all', 'No not at all','No not at All')

# --------------
# Define some useful function (that are not included in ALSPAC.helpR)

percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }

combine_items <- function(df, prenatal=FALSE, domain='') {

  for (v in names(df)){
    if(substr(v, nchar(v), nchar(v))=='1') {
      name <- substr(v, 1, nchar(v)-2)
      # print(name)
      group <- df[, grep( name, names(df))]
      print(suppressWarnings(UpSetR::upset(group)))

      # Remove individual components
      df <- df[, -grep(name, names(df))]
      # Compute total score
      df[,name] <- ifelse(rowSums(is.na(group)) == ncol(group), NA,
                          ifelse(rowSums(group, na.rm=TRUE)>0, 1, 0))
      # Print summary
      cat('\nCOMBINED VAR:', name,'\n')
      summ <- base::table(df[,name], useNA = 'ifany')
      summ <- as.table(rbind(summ,
                             paste0(round(base::prop.table(summ)*100), '%')))
      row.names(summ) <- c('n','Percent')
      print(summ)
    }
  }

  # Construct missing percents and scores

  if (prenatal) {
    # Adjust names of prenatal items
    names(df) <- paste0(names(df), '_preg')

    domain_miss = apply(df, 1, percent_missing)
    score <- ifelse(domain_miss >= 25, NA, rowMeans(df, na.rm = TRUE) )

    domain <- paste0('pre_',domain)

  } else {
    # Identify unique items
    items <- unique(sapply( names(df), function(v) {
      # Remove the temporal specification (last bit after "_")
      namevec <- unlist(strsplit(v, '_'))
      names <- paste0(namevec[-length(namevec)], collapse='_')
      return(names)})
    )

    iscores <- sapply(items, function(i){
      group <- as.data.frame(df[, grep(i, names(df))])
      if (ncol(group) > 1){
        cat('\n',i,'\n', sep='')
        # Correlation matrix
        cmat <- round(cor(group, method = "spearman", use = "pairwise.complete.obs"), 2)
        colnames(cmat) <- gsub(i, '', colnames(cmat)) # shorter column names
        print(cmat)
        # Upset plot
        print(upset(group))
      }

      domain_miss = apply(group, 1, percent_missing)
      iscore <- ifelse(domain_miss >= 25, NA, rowSums(group, na.rm = TRUE) )
      return(iscore)
    })

    # calculate number of missing items per participant
    domain_miss = apply(iscores, 1, percent_missing)
    # calculate the domain score
    score <- ifelse(domain_miss >= 25, NA, rowMeans(iscores, na.rm = TRUE) )

    domain <- paste0('pos_',domain)
  }

  df[, paste0(domain,'_missing_perc')] <- domain_miss
  df[, paste0(domain,'_domain_score')] <- score

  return(df)
}

# ==============================================================================
# Create variables that are missing in the set

idc <- make_idc(mom.id='cidb4195')

# Parental psychopathology -------------------

# Maternal anxiety (CCEI)
data$m_anx_5y <- ccei_score(sev1 = c('k3000', 'k3014', 'k3016'),
                            sev2 = c('k3002', 'k3005', 'k3011', 'k3019'),
                            sev3 = c('k3008'))
data$m_anx_6y <- ccei_score(sev1 = c('l2000', 'l2005', 'l2006'),
                            sev2 = c('l2001', 'l2002', 'l2004', 'l2007'),
                            sev3 = c('l2003'))

# Maternal depression (EPDS)
data$m_dep_18wg <- epds_score(set = c('b360','b361','b363'),
                           revset = c('b362','b364','b365','b366','b367','b368','b369'))
data$m_dep_32wg <- epds_score(set = c('c590','c591','c593'),
                           revset = c('c592','c594','c595','c596','c597','c598','c599'))
data$m_dep_5y <- epds_score(set = c('k3030','k3031','k3033'),
                         revset = c('k3032','k3034','k3035','k3036','k3037','k3038','k3039'))
data$m_dep_8y <- epds_score(set = c('n6060','n6061','n6063'),
                         revset = c('n6062','n6064','n6065','n6066','n6067','n6068','n6069'))

# Partner depression (EPDS)
data$p_dep_18wg <- epds_score(set = c('pb250','pb251','pb253'),
                           revset = c('pb252','pb254','pb255','pb256','pb257','pb258','pb259'))
data$p_dep_3y <- epds_score(set = c('pf4030','pf4031','pf4033'),
                         revset = c('pf4032','pf4034','pf4035','pf4036','pf4037','pf4038','pf4039'))
data$p_dep_5y <- epds_score(set = c('ph3030','ph3031','ph3033'),
                         revset = c('ph3032','ph3034','ph3035','ph3036','ph3037','ph3038','ph3039'))
data$p_dep_6y <- epds_score(set = c('pj2010','pj2011','pj2013'),
                         revset = c('pj2012','pj2014','pj2015','pj2016','pj2017','pj2018','pj2019'))

# Hosing variables ---------------------------

comb_housing <- function(v1, v2, d=data, verbose=TRUE) {
  no <- c('No','N','No Do Not Have','no')
  output <- ifelse((d[,v1] %in% no & d[,v2] %in% no) | 
                   (d[,v1] %in% no & is.na(d[,v2])) | 
                   (is.na(d[,v1]) & d[,v2] %in% no), 'No', 
                   ifelse((is.na(d[,v1]) & is.na(d[,v2])), NA, 'Yes'))
  if (verbose) print(table(d[,v1], d[,v2], output, useNA = 'ifany'))
  return(output)
}

data$a04_kitchen <- comb_housing('a040','a041') # Home has kitchen to sit & eat OR Home has cooking only kitchen
data$f34_kitchen <- comb_housing('f340','f341')
data$g41_kitchen <- comb_housing('g410','g411')
data$h29_kitchen <- comb_housing('h290','h291')

data$a04_shower <- comb_housing('a047','a048') # Use of bath OR Use of shower
data$f35_shower <- comb_housing('f351','f352') 
data$g41_shower <- comb_housing('g416','g417')
data$h29_shower <- comb_housing('h296','h297')


# Parental education (5y) --------------------

# Finally, recode parental education (measured at 5y)
data[,c('m_education','p_education')] <- data %>%
  select(c(paste0('k62', 80:92), paste0('k63', sprintf('%02d', 0:15)))) %>%
  mutate(across(everything(),
                function(x) { droplevels(x); ifelse(!is.na(x) & x=='Yes', 1, 0) } )) %>%
  mutate(.,
         m_education = with(.,case_when(
           # Mother has a university degree
           (k6292 == 1) ~ 0,
           # Mother is a state enrolled / registered nurse
           # Mother has City & Guilds intermediate / final / full technical qualification
           # Mother has a teaching qualification
           (k6286 == 1 | k6287 == 1 | k6288 == 1 | k6289 == 1 | k6290 == 1 | k6291 == 1) ~ 1, # 0.25,
           # Mother has A-levels
           # Mother has vocational qualification
           # Mother has done apprenticeship
           (k6283 == 1 | k6284 == 1 | k6285 == 1 ) ~ 2, # 0.5,
           # Mother has CSE/GCSE (D,E,F,G)
           # Mother has O-level/GCSE (A,B,C)
           (k6281 == 1 | k6282 == 1) ~ 3, # 0.75,
           # Mother has no educational qualification
           (k6280 == 1) ~ 4,
           TRUE ~ NA)),
         p_education = with(.,case_when(
           # Partner has a university degree
           (k6312 == 1) ~ 0,
           # Partner is a state enrolled / registered nurse
           # Partner has City & Guilds intermediate / final / full technical qualification
           # Partner has a teaching qualification
           (k6306 == 1 | k6307 == 1 | k6308 == 1 | k6309 == 1 | k6310 == 1 | k6311 == 1 ) ~ 1, # 0.25,
           # Partner has A-levels
           # Partner has vocational qualification
           # Partner has done apprenticeship
           (k6303 == 1 | k6304 == 1 | k6305 == 1 ) ~ 2, # 0.5,
           # Partner has CSE/GCSE (D,E,F,G)
           # Partner has O-level/GCSE (A,B,C)
           (k6301 == 1 | k6302 == 1) ~ 3, # 0.75,
           # Partner has no educational qualification
           (k6300 == 1) ~ 5,
           TRUE ~ NA))
  ) %>% select(m_education, p_education)

# ==============================================================================

sink('ELS_score_report.txt')
pdf('ELS_score_report.pdf')

cat('################################################################################\n
# PRENATAL STRESS\n
################################################################################\n\n
# 1. LIFE EVENTS {PRENATAL}
================================================================================\n')

LE_prenatal <- combine_items(
  dichotomize(vars = data.frame(
    'b570'='family_member_died_1', # PTNR died since PREG
    'b571'='family_member_died_2', # CH died since PREG

    'b572'='friend_relative_died', # Friend or relative died since PREG

    'b573'='family_member_ill_1', # CH was ill since PREG
    'b574'='family_member_ill_2', # PTNR was ill since PREG

    'b575'='friend_relative_ill', # Friend or relative was ill since PREG

    'b576'='sick_or_accident_1', # Admitted to hospital since PREG
    'b580'='sick_or_accident_2', # V ill since PREG
    'b610'='sick_or_accident_3', # Had an accident since PREG

    'b583'='work_problems',# PROBS at work since PREG
    'b584'='unemployed',   # Lost job since PREG
    'b591'='moved',        # Moved house since PREG
    'b595'='got_married',  # Got married since PREG
    'b599'='blood_loss',   # Bled & thought might miscarry
    'b602'='pregnancy_worried', # Test result suggesting POSS abnormality
    'b604'='baby_worried', # POSS harm to baby
    'b605'='abortion',     # Tried to have abortion
    'b609'='burglary'      # House or car burgled since PREG
    ), yes=yes, no=no, 
), prenatal=TRUE, domain='LE')

# ------------------------------------------------------------------------------
cat('
================================================================================
# 2. CONTEXTUAL RISK {PRENATAL}
================================================================================\n')

CR_prenatal <- combine_items(
  cbind(
  dichotomize(vars = data.frame(
    'b581'='income_reduced_1',    # PTNR lost job since PREG
    'b588'='income_reduced_2',    # Income reduced since PREG
    'b594'='financial_problems_1',# Major financial PROB since PREG
    'b593'='homeless',            # Became homeless since PREG
    # 'p2'='housing_adequacy',     # FAI: Housing adequacy (homeless + crowding)
    # 'p3'='housing_basic_living', # FAI: Housing Basic Living (see below)
    # 'p4'='housing_defects'       # FAI: Housing Defects
    'a082'='housing_defects_1', # Level of mould
    'a090'='housing_defects_2', # Leaky roof
    'a070'='housing_defects_3', # Rat PROB
    'a071'='housing_defects_4', # Mice PROB
    'a074'='housing_defects_5'  # Cockroach PROB
  ), yes=yes, no=no),
  # Add financial difficulties 
  dichotomize_cont(
    vars = data.frame('c525'='financial_problems_2'), # Financial difficulties
    rule = '> 8' # Cutoff based on FAI documentation
  ),
  # Add crowding (ex. combined in Housing adequacy)
  dichotomize(vars = data.frame('a551'='crowding'), 
              yes=c('> 1'), # Less than 1 room per person [as in FAI documentation]
              no=c('<= 0.5','>0.5 - 0.75','>0.75 - 1') 
  ),
  # Add Housing basic living (FAI)
  dichotomize(vars = data.frame(
    'a04_kitchen'='housing_basic_living_1', # Home has kitchen to sit & eat OR cooking only kitchen
           'a042'='housing_basic_living_2', # Home has indoor WC
           'a046'='housing_basic_living_3', # Use of running hot water
     'a04_shower'='housing_basic_living_4'),# Use of bath OR shower
    yes=c('No'), 
    no=c('Yes','Yes, sole use','Yes, shared')
  ),
  # Add maternal and paternal education
  dichotomize(vars = data.frame(
    'c645a'='m_education', # Mums highest ed qualification
    # NOTE: FAI uses c642 = Mum has no qualification & c643 = Mum has qualification NK
    'c666a'='p_education'  # PTRN highest ed qualification
    # NOTE: FAI uses c662 = PTNR has no qualification & c663 = PTNR has qualification NK
    ), yes = c('None', 'CSE', 'Vocational', 'O level', 'A level'), no = c('Degree')
  )
  
), prenatal=TRUE, domain='CR')

# ------------------------------------------------------------------------------
cat('
================================================================================
# 3. PARENTAL RISK {PRENATAL}
================================================================================\n')

PR_prenatal <- combine_items(
  cbind(
  dichotomize(vars = data.frame(
    'b577'='m_criminal_record_1', # In trouble with the law since PREG
    'b598'='m_criminal_record_2', # Convicted of an offence since PREG
   # 'p14'='m_criminal_record_3', # Crime trouble with police
    'b586'='p_criminal_record_1', # PTNR in trouble with law since PREG
   'pb188'='p_criminal_record_2', # PTNR convicted of an offence since PREG --> NOTE: typo (i.e., 'affect' instead of 'effect')
    'b597'='m_attempted_suicide'  # Attempted suicide since PREG
   ), yes=yes, no=no),
  # Add maternal age
  dichotomize_cont(
    vars = data.frame('mz028b'='early_pregnancy'), # Grouped age of mother at delivery
    rule = '< 19' # mother younger than 19 at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)
  ),
  # Add interpersonal sensitivity
  dichotomize_cont(
    vars=data.frame('pb551'='p_interpersonal_sensitivity',  # Total interpersonal sensitivity
                     'b916'='m_interpersonal_sensitivity'), # Interpersonal awareness
    rule='>= .8', quantile=TRUE # NOTE: no manual-advised cut-off so we are use a 80th percentile cutoff.
    # Higher scores = greater interpersonal sensitivity, based on items such as
    #  - 'feel insecure when saying goodbye'; 'I avoid saying what I think for fear of being rejected'
  ),
  # Add depression
  dichotomize_cont(
    vars=data.frame('m_dep_18wg'='m_depression_1', # EPDS - 18w gest
                    'm_dep_32wg'='m_depression_2', # EPDS - 32w gest # corr = 0.4286
                    'p_dep_18wg'='p_depression'),  # EPDS partner
    rule='> 12' # As described in ALSPAC FAI documentation (preparation for EPDS total score calculation)
    # a total score of 13 or more is considered a flag for the need for follow up of
    # possible depressive symptoms.
  ),
  # Add anxiety
  dichotomize(
    vars=data.frame('b351'='m_anxiety_1', # CCEI anxiety subscale (complete) - 18w gest
                    'c573'='m_anxiety_2', # CCEI anxiety subscale (complete) - 32w gest # corr = 0.4625
                   'pb233'='p_anxiety'),  # CCEI partner
    no = c('not anxious', 'Not anxious', 1:8),
    yes = c('very anxious','Very anxious', 9:15)
    # Scores on the CCEI anxiety subscale range from 0 to 16. Women who scored 9 or
    # higher were classified as having anxiety (Heron et al., 2004).
  )
), prenatal=TRUE, domain='PR')

# ------------------------------------------------------------------------------
cat('
================================================================================
# 4. INTERPERSONAL RISK {PRENATAL}
================================================================================\n')

IR_prenatal <- combine_items(
  cbind(
  dichotomize(vars = data.frame(
    'b578'='divorce_1', # Divorced since PREG
    'b587'='divorce_2', # Separated since PREG
    'b579'='p_rejected_child',   # PTNR rejected PREG
    'b585'='p_went_away',        # PTNR went away since PREG
    'b590'='argued_fam_friends', # Argued with family or friends since PREG
    'b592'='family_conflict_violence_1', # PTNR hurt mum since PREG
    'b596'='family_conflict_violence_2', # PTNR hurt CH since PREG
    'b589'='family_conflict_emotional_1',  # Argued with PTNR since PREG # NOTE: prev not included...
    'b607'='family_conflict_emotional_2',  # PTNR was EMOT cruel to mum since PREG
    'b608'='family_conflict_emotional_3',  # PTNR was EMOT cruel to child since PREG
    'a525'='marital_status'
      # 'p7'='marital_status',    # Partner Status
      # 'p8'='family_affection',  # Partner Affection
      # 'p10'='family_size',      # Family Size
      # 'p11'='family_problems',  # Family Major problems
      # 'p16'='family_support',   # Partner Support
      # 'p17'='social_network_emotional', # Social Network - Emotional
      # 'p18'='social_network_practical'  # Social Network - Practical
    ), yes=yes, no=no
  ),
  dichotomize(vars = data.frame(
    'd790'='social_network_emotional_1', # No one to share feelings with
    'd774'='social_network_emotional_2', # No of people who confide
    'd776'='social_network_practical_1', # No of people to borrow 100 pounds from
    'd777'='social_network_practical_2'),# No of helpers if in trouble
    yes=c('Exactly feel','Often feel','None'), 
    no=c('Never feel','SMTS feel','1','2-4','>4')
  ),
  dichotomize(vars = data.frame(
    'd791'='partner_support_1', # PTNR provides emotional support needed
    'd796'='partner_support_2', # When tired can rely on PTNR
    'd797'='social_network_practical_3'), # Family would help with money
    yes=c('Never feel'),
    no=c('Exactly feel','Often feel','SMTS feel')
  ),
  dichotomize(vars = data.frame(
    'd794'='partner_support_3'), # Worried that PTNR might leave
    yes=c('Exactly feel'),
    no=c('Never feel','SMTS feel','Often feel')
  ),
  # Affection score : modes/missing data
  dichotomize_cont(vars = data.frame('d370'='partner_affection'), rule = '> 17'),
  # Aggression score: modes/missing data
  dichotomize_cont(vars = data.frame('d373'='partner_aggression'), rule = '< 8'),
  # NO of 0 to 15s in household
  dichotomize_cont(vars = data.frame('a502'='family_size'),  rule = '> 3')
  
), prenatal=TRUE, domain='IR')

# ==============================================================================
# Wrap this up and copute total score
prenatal_stress <- cbind(idc, LE_prenatal, CR_prenatal, PR_prenatal, IR_prenatal)

prenatal_stress$pre_tot_missing_perc = apply(
  prenatal_stress[,-grep('score|missing|idc',names(prenatal_stress))], 1, percent_missing)

prenatal_stress$pre_ELS_score <- rowSums(prenatal_stress[,grep('score', names(prenatal_stress))], na.rm = FALSE)

for (s in names(prenatal_stress)[grep('score', names(prenatal_stress))]) {
  cat('\nPRENATAL ELS -', s,'\n')
  print(summary(prenatal_stress[,s]))
}


# ==============================================================================

cat('\n\n################################################################################\n
# POSTNATAL STRESS\n
################################################################################\n\n
# 1. LIFE EVENTS {POSTNATAL}
================================================================================\n')

LE_postnatal <- combine_items(
  dichotomize(vars = data.frame(
    'f220'='partner_died_8m', # Death of partner
    'g300'='partner_died_21m',# Partner died >CH8MTHs
    'h210'='partner_died_3y', # Whether partner died since study child was 18 months old
    'j300'='partner_died_4y', # Partner Died > CH 30 MTHs
   'k4000'='partner_died_5y', # Mothers partner died in past year
   'l4000'='partner_died_6y', # Respondent's partner died since study child's 5th birthday
   'p2000'='partner_died_9y', # Husband/partner died since the study child's 6th birthday

    'f221'='smbd_important_died_8m_1', # Death of one of children
    'f222'='smbd_important_died_8m_2', # Death of friend or relative
    'g301'='smbd_important_died_21m_1',# One of Mums children died >CH8MTHs
    'g302'='smbd_important_died_21m_2',# Friend or relative died >CH8MTHs
    'h211'='smbd_important_died_3y_1', # Whether one of mums children died since study child was 18 months old
    'h212'='smbd_important_died_3y_2', # Whether a friend or relative died since study child was 18 months old
    'j301'='smbd_important_died_4y_1', # 1 of MUMs Children Died> CH 30 MTHs
    'j302'='smbd_important_died_4y_2', # MUMs FRD or Relative Died> CH 30 MTHs
   'k4001'='smbd_important_died_5y_1', # Mothers child died in past year
   'k4002'='smbd_important_died_5y_2', # Mothers friend or relative died in past year
   'l4001'='smbd_important_died_6y_1', # One of respondent's children died since study child's 5th birthday
   'l4002'='smbd_important_died_6y_2', # Respondent's friend/relative died since study child's 5th birthday
   'kq366'='smbd_important_died_6y_3', # Somebody in the family died since child's 5th birthday
   'p2001'='smbd_important_died_9y_1', # One of mother's children died since the study child's 6th birthday
   'p2002'='smbd_important_died_9y_2', # Mother's friend or relative died since the study child's 6th birthday
  'kt5006'='smbd_important_died_9y_3', # Since 7th birthday child has had someone in family die # NOTE: 8y but collapsed with 9y variable

    'f223'='sick_or_accident_18m_1',# Child ill # NOTE: 8m but collapsed with 18 months variable
  'kd510a'='sick_or_accident_18m_2',# Ch admitted to hospital (adj)
    'g303'='sick_or_accident_30m_1',# One of Mums children ill >CH8MTHs # NOTE: 21m but collapsed with 30 months variable
   'kf460'='sick_or_accident_30m_2',# Child admitted to hospital >18 mths
    'h213'='sick_or_accident_3y_1', # Whether one of mums children was ill since study child was 18 months old
   'kj470'='sick_or_accident_3y_2', # Child Admitted To Hospital
    'j303'='sick_or_accident_4y_1', # 1 of MUMs CDRN Ill> CH 30 MTHs
   'kl480'='sick_or_accident_4y_2', # Child admitted to hospital since age 3
   'k4003'='sick_or_accident_5y_1', # Mothers child was ill in past year
  'kn4010'='sick_or_accident_5y_2', # Child admitted to hospital in past 15 months
   'l4003'='sick_or_accident_6y_1', # One of respondent's children was ill since study child's 5th birthday
   'kq371'='sick_or_accident_6y_2', # Child was admitted to hospital since his/her 5th birthday
   'p2003'='sick_or_accident_9y_1', # One of mother's children was ill since the study child's 6th birthday
  'kt5011'='sick_or_accident_9y_2', # Since 7th birthday child has been admitted to hospital # NOTE: 8y but collapsed with 9y variable

    'f224'='family_member_ill_8m_1', # PTNR ill
    'f226'='family_member_ill_8m_2', # Admitted to hospital
    'f230'='family_member_ill_8m_3', # Mum ill
    'g304'='family_member_ill_21m_1',# Partner ill >CH8MTHs
    'g306'='family_member_ill_21m_2',# Mum in hospital >CH8MTHs
    'g310'='family_member_ill_21m_3',# Mum very ill >CH8MTHs
    'g342'='family_member_ill_21m_4',# Mum had accident >CH8MTHs
    'h214'='family_member_ill_3y_1', # Whether partner was ill since study child was 18 months old
    'h216'='family_member_ill_3y_2', # Whether mum was admitted to hospital since study child was 18 months old
    'h220'='family_member_ill_3y_3', # Whether mum was very ill since study child was 18 months old
    'h252'='family_member_ill_3y_4', # Whether mum had an accident since study child was 18 months old
    'j304'='family_member_ill_4y_1', # Partner Ill> CH 30 MTHs
    'j306'='family_member_ill_4y_2', # MUM in HOSP> CH 30 MTHs
    'j310'='family_member_ill_4y_3', # MUM was Very Ill> CH 30 MTHs
    'j342'='family_member_ill_4y_4', # MUM Had Accident> CH 30 MTHs
   'k4004'='family_member_ill_5y_1', # Mothers partner was ill in past year
   'k4006'='family_member_ill_5y_2', # Mother was admitted to hospital in past year
   'k4010'='family_member_ill_5y_3', # Mother was very ill in past year
   'k4042'='family_member_ill_5y_4', # Mother had Accident in past year
   'l4004'='family_member_ill_6y_1', # Respondent's partner was ill since study child's 5th birthday
   'l4006'='family_member_ill_6y_2', # Respondent was admitted to hospital since study child's 5th birthday
   'l4010'='family_member_ill_6y_3', # Respondent was very ill since study child's 5th birthday
   'l4044'='family_member_ill_6y_4', # Respondent had an accident since study child's 5th birthday
   'p2004'='family_member_ill_9y_1', # Mother's husband/partner was ill since the study child's 6th birthday
   'p2006'='family_member_ill_9y_2', # Mother was admitted to hospital since the study child's 6th birthday
   'p2010'='family_member_ill_9y_3', # Mother was very ill since the study child's 6th birthday
   'p2044'='family_member_ill_9y_4', # Mother had an accident since the study child's 6th birthday

    'f225'='friend_relative_ill_8m', # Friend or relative ill
    'g305'='friend_relative_ill_21m',# Friend or relative ill >CH8MTHs
    'h215'='friend_relative_ill_3y', # Whether a friend or relative was ill since study child was 18 months old
    'j305'='friend_relative_ill_4y', # MUM FRD or Relative Ill> CH 30 MTHs
   'k4005'='friend_relative_ill_5y', # Mothers friend or relative was ill in past year
   'l4005'='friend_relative_ill_6y', # Respondent's friend/relative was ill since study child's 5th birthday
   'p2005'='friend_relative_ill_9y', # Mother's friend or relative was ill since the study child's 6th birthday

  'kd500a'='separated_from_parent_18m_1', # CH Taken Into Care Since 6 MTHS
  'kd506a'='separated_from_parent_18m_2', # Ch separated from mum for > a wk (adj)
  'kd507a'='separated_from_parent_18m_3', # Ch separated from dad for > a wk (adj)
   'kf450'='separated_from_parent_30m_1', # Child taken into care > 18 months
   'kf456'='separated_from_parent_30m_2', # Child sep.from mother >1wk >18 mths
   'kf457'='separated_from_parent_30m_3', # Child sep.from father >1wk >18 mths
   'kj460'='separated_from_parent_3y_1',  # Child Taken Into Care
   'kj466'='separated_from_parent_3y_2',  # Child & Mum Separated
   'kj467'='separated_from_parent_3y_3',  # Child & Dad Separated
   'kl470'='separated_from_parent_4y_1',  # Child taken into care since age 3
   'kl476'='separated_from_parent_4y_2',  # Child separated from mother since age 3
   'kl477'='separated_from_parent_4y_3',  # Child separated from father since age 3
  'kn4000'='separated_from_parent_5y_1',  # Child taken into care in past 15 months
  'kn4006'='separated_from_parent_5y_2',  # Child separated from mother in past 15 months
  'kn4007'='separated_from_parent_5y_3',  # Child separated from Father in past 15 months
   'kq360'='separated_from_parent_6y_1',  # Child was taken into care since his/her 5th birthday
   'kq367'='separated_from_parent_6y_2',  # Child was separated from his/her mother since his/her 5th birthday
   'kq368'='separated_from_parent_6y_3',  # Child was separated from his/her father since his/her 5th birthday
  'kt5000'='separated_from_parent_8y_1',  # Since 7th birthday child has been taken into care
  'kt5007'='separated_from_parent_8y_2',  # Since 7th birthday child has been separated from mother
  'kt5008'='separated_from_parent_8y_3',  # Since 7th birthday child has been separated from father

  'kd512a'='separated_from_smbd_18m', # Ch separated from someone else (adj)
   'kf462'='separated_from_smbd_30m', # Child sep.from somebody > 18 months
   'kj472'='separated_from_smbd_3y',  # Child Separated From Someone Else
   'kl482'='separated_from_smbd_4y',  # Child separated from someone else since age 3
  'kn4012'='separated_from_smbd_5y',  # Child separated from another person in past 15 months
   'kq373'='separated_from_smbd_6y',  # Child was separated from another close person since his/her 5th birthday
  'kt5013'='separated_from_smbd_8y',  # Since 7th birthday child has been separated from someone else

  'kt5015'='lost_best_friend_8y', # Since 7th birthday child has lost their best friend

    'f241'='moved_18m_1', # Moved house # NOTE: 8m but collapsed with 18 months variable
  'kd502a'='moved_18m_2', # Ch moved home (adj)
    'g321'='moved_30m_1', # Mum moved house >CH8MTHs # NOTE: 21m but collapsed with 30 months variable
   'kf452'='moved_30m_2', # Child moved home > 18 months
    'h231'='moved_3y_1',  # Whether mum moved house since study child was 18 months old
   'kj462'='moved_3y_2',  # Child Moved Home
    'j321'='moved_4y_1',  # MUM Moved House> CH 30 MTHs
   'kl472'='moved_4y_2',  # Child moved home since age 3
   'k4021'='moved_5y_1',  # Mother moved house in past year
  'kn4002'='moved_5y_2',  # Child move home in past 15 months
   'l4021'='moved_6y_1',  # Respondent moved house since study child's 5th birthday
   'kq362'='moved_6y_2',  # Child moved home since his/her 5th birthday
   'p2021'='moved_9y_1',  # Mother moved house since the study child's 6th birthday
  'kt5002'='moved_9y_2',  # Since 7th birthday child moved home # NOTE: 8y but collapsed with 9y variable

    'f250'='new_sibling_18m_1', # Mum became pregnant # NOTE: 8m but collapsed with 18 months variable
  'kd509a'='new_sibling_18m_2', # Ch had a new sibling (adj)
    'g330'='new_sibling_30m_1', # Mum pregnant >CH8MTHs # NOTE: 21m but collapsed with 30 months variable
   'kf459'='new_sibling_30m_2', # Child got new sibling > 18 months
    'h240'='new_sibling_3y_1',  # Whether mum became pregnant since study child was 18 months old
   'kj469'='new_sibling_3y_2',  # Child Got a New Sibling
    'j330'='new_sibling_4y_1',  # MUM Became PREG> CH 30 MTHs
   'kl479'='new_sibling_4y_2',  # Child had new brother or sister since age 3
   'k4030'='new_sibling_5y_1',  # Mother became pregnant in past year
  'kn4009'='new_sibling_5y_2',  # Child have a new brother or sister in past 15 months
   'l4030'='new_sibling_6y_1',  # Respondent became pregnant since study child's 5th birthday
   'kq370'='new_sibling_6y_2',  # Child had a new brother or sister since his/her 5th birthday
   'p2030'='new_sibling_9y_1',  # Mother became pregnant since the study child's 6th birthday
  'kt5010'='new_sibling_9y_2',  # Since 7th birthday child has had a new brother or sister # NOTE: 8y but collapsed with 9y variable

  'kd508a'='new_parent_18m', # CH Acquired New Parent > 6 MTHS
   'kf458'='new_parent_30m', # Child got new parent > 18 months
   'kj468'='new_parent_3y',  # Child Got a New Parent
   'kl478'='new_parent_4y',  # Child acquired new mother or father since age 3
  'kn4008'='new_parent_5y',  # Child acquire new parent in past 15 months
   'kq369'='new_parent_6y',  # Child acquired a new mother/father since his/her 5th birthday
  'kt5009'='new_parent_8y',  # Since 7th birthday child has had a new mother or father

  'kd511a'='changed_carer_18m', # Ch changed carer (adj)
   'kf461'='changed_carer_30m', # Child changed carer > 18 months
   'kj471'='changed_carer_3y',  # Child Changed Carer
   'kl481'='changed_carer_4y',  # Child changed care taker since age 3
  'kn4011'='changed_carer_5y',  # Child's main carer change in past 15 months
   'kq372'='changed_carer_6y',  # Child changed care taker since his/her 5th birthday
  'kt5012'='changed_carer_8y',  # Since 7th birthday child has changed their caretaker

  'kd513a'='new_school_18m', # Ch started nursery (adj)
   'kf463'='new_school_30m', # Child started new creche >18 months
   'kj473'='new_school_3y',  # Child Started New Creche
   'kl483'='new_school_4y',  # Child started new nursery/kindergarten since age 3
  'kn4013'='new_school_5y',  # Child start a new nursery in past 15 months
  'kt5014'='new_school_8y',  # Since 7th birthday child has started a new school

    'f261'='pet_died_18m_1', # Pet died # NOTE: 8m but collapsed with 18 months variable
  'kd501a'='pet_died_18m_2', # A pet died (adj)
    'g341'='pet_died_30m_1', # A pet died >CH8MTHs # NOTE: 21m but collapsed with 30 months variable
   'kf451'='pet_died_30m_2', # A pet died > 18 months,
    'h251'='pet_died_3y_1',  # Whether a pet died since study child was 18 months old
   'kj461'='pet_died_3y_2',  # Pet died
    'j341'='pet_died_4y_1',  # MUMs Pet Died> CH 30 MTHs
   'kl471'='pet_died_4y_2',  # A pet died since child age 3
   'k4041'='pet_died_5y_1',  # Mothers pet died in past year
  'kn4001'='pet_died_5y_2',  # Child's pet die in past 15 months
   'l4043'='pet_died_6y_1',  # A pet of respondent died since study child's 5th birthday
   'kq361'='pet_died_6y_2',  # A pet died since child's 5th birthday
   'p2043'='pet_died_9y_1',  # A pet died since the study child's 6th birthday
  'kt5001'='pet_died_9y_2',  # Since 7th birthday child's pet died # NOTE: 8y but collapsed with 9y variable

    'g339'='burglary_21m',# Mums house or car burgled >CH 8MTHs
    'h249'='burglary_3y', # Whether house or car was burgled since study child was 18 months old
    'j339'='burglary_4y', # MUMs House or Car Burgled> CH 30 MTHs
   'k4039'='burglary_5y', # Mothers house or car was burgled in past year
   'l4039'='burglary_6y', # Respondent's house/car was burgled since study child's 5th birthday
   'p2039'='burglary_9y', # Mother's house or car was burgled since the study child's 6th birthday

  'kd503a'='shock_fright_18m', # Ch had fright (adj)
   'kf453'='shock_fright_30m', # Child had fright > 18 months
   'kj463'='shock_fright_3y',  # Child Had Shock
   'kl473'='shock_fright_4y',  # Child had shock or fright since age 3
  'kn4003'='shock_fright_5y',  # Child have a fright or shock in past 15 months
   'kq363'='shock_fright_6y',  # Child had a shock/fright since his/her 5th birthday
  'kt5003'='shock_fright_8y'   # Since 7th birthday child had shock or fright
  ), yes=yes, no=no
), domain='LE')

# ------------------------------------------------------------------------------
cat('
================================================================================
# 2. CONTEXTUAL RISK {POSTNATAL}
================================================================================\n')

CR_postnatal <- combine_items(
  cbind(
  dichotomize(vars = data.frame(
    'f231'='unemployed_8m_1', # Partner lost job
    'f234'='unemployed_8m_2', # Mum lost job
    'g311'='unemployed_21m_1',# Partner lost job >CH8MTHs
    'g314'='unemployed_21m_2',# Mum lost job >CH8MTHs
    'h221'='unemployed_3y_1', # Whether partner lost job since study child was 18 months old
    'h224'='unemployed_3y_2', # Whether mum lost job since study child was 18 months old
    'j311'='unemployed_4y_1', # Partner Lost Job> CH 30 MTHs
    'j314'='unemployed_4y_2', # MUM lost Job> CH 30 MTHs
   'k4011'='unemployed_5y_1', # Mothers partner lost job in past year
   'k4014'='unemployed_5y_2', # Mother lost her job in past year
   'l4011'='unemployed_6y_1', # Respondent's partner lost their job since study child's 5th birthday
   'l4014'='unemployed_6y_2', # Respondent lost their job since study child's 5th birthday
   'p2011'='unemployed_9y_1', # Mother's husband/partner lost his job since the study child's 6th birthday
   'p2014'='unemployed_9y_2', # Mother lost her job since the study child's 6th birthday

    'f238'='income_reduced_8m',  # Reduced income >CH born
    'g318'='income_reduced_21m', # Mums income reduced >CH8MTHs
    'h228'='income_reduced_3y',	 # Whether mums income was reduced since study child was 18 months old
    'j318'='income_reduced_4y',  # MUMs Income Reduced> CH 30 MTHs
   'k4018'='income_reduced_5y',	 # Mothers income was reduced in past year
   'l4018'='income_reduced_6y',	 # Respondent's income reduced since study child's 5th birthday
   'p2018'='income_reduced_9y',  # Mother's income was reduced since the study child's 6th birthday

    'f243'='became_homeless_8m', # Became homeless >CH born
    'g323'='became_homeless_21m',# Mum became homeless >CH8MTHs
    'h233'='became_homeless_3y', # Whether mum became homeless since study child was 18 months old
    'j323'='became_homeless_4y', # MUM Became Homeless> CH 30 MTHs
   'k4023'='became_homeless_5y', # Mother became homeless in past year
   'l4023'='became_homeless_6y', # Respondent became homeless since study child's 5th birthday
   'p2023'='became_homeless_9y', # Mother became homeless since the study child's 6th birthday

    'f244'='financial_problems_8m_1', # Major financial PROBS >CH born
    'g324'='financial_problems_21m_1',# Mum had money problems >CH8MTHs
    'h234'='financial_problems_3y_1', # Whether mum had major financial problems since study child was 18 months old
    'j324'='financial_problems_4y', # MUM Major Money PROB> CH 30 MTHs
   'k4024'='financial_problems_5y', # Mother had a major financial problem in past year
   'l4024'='financial_problems_6y', # Respondent had major financial problem since study child's 5th birthday
   'p2024'='financial_problems_9y',  # Mother had a major financial problem since the study child's 6th birthday
   
   'f402'='housing_defects_8m_1', # Level of mould
   'f410'='housing_defects_8m_2', # Leaky roof
   'f390'='housing_defects_8m_3', # Rat PROB
   'f391'='housing_defects_8m_4', # Mice PROB
   'f394'='housing_defects_8m_5', # Cockroach PROB
   'g442'='housing_defects_21m_1', # Mould is problem
   'g450'='housing_defects_21m_2', # Does Roof Leak
   'g570'='housing_defects_21m_3', # Home invaded by rats (or a?)
   'g571'='housing_defects_21m_4', # Home invaded by mice
   'g574'='housing_defects_21m_5', # Home invaded by cockroaches
   'h317'='housing_defects_3y_1', # Mould is a problem in home
     # no leaky roof variable
   'h460'='housing_defects_3y_3', # Home invaded by rats (or a? Y/N)
   'h461'='housing_defects_3y_4', # Home invaded by mice
   'h464'='housing_defects_3y_5'  # Home invaded by cockroaches
   
    # 'b2'='housing_adequacy_2y', # FAI : Housing adequacy 0-2y (homeless + crowding)
    # 't2'='housing_adequacy_4y', # FAI : Housing adequacy 2-4y (homeless + crowding)
    # 'b3'='housing_basic_living_2y', # Housing basic living 0-2y composite
    # 't3'='housing_basic_living_4y', # Housing basic living 2-4y composite
    # 'b4'='housing_defects_2y',  # Housing defects 0-2y composite
    # 't4'='housing_defects_4y'   # Housing defects 2-4y composite
  ), yes=yes, no=no),
  # Add financial difficulties score
  dichotomize_cont(
    vars = data.frame('f805'='financial_problems_8m_2',  # Financial difficulties score
                      'g840'='financial_problems_21m_2', # Financial Difficulties Score
                      'h735'='financial_problems_3y_2'), # Financial difficulties score
    # Difficulty affording food, clothing, heating, rent or items for child
    # NOTE: also partner reported 8m (pd685) and 21m (pe465)
    rule = '> 8' # Cutoff based on FAI documentation
  ),
  # Add crowding (ex. combined in Housing adequacy)
  dichotomize(vars = data.frame('g504'='crowding_21m',
                                'h374'='crowding_3y'), 
              yes=c('1 or more','> 1'), # Less than 1 room per person [as in FAI documentation]
              no=c('< 0.5','<= 0.5','0.5 - < 0.75','>0.5 - 0.75','0.75 - < 1','>0.75 - 1') 
  ),
  # Add Housing basic living (FAI)
  dichotomize(vars = data.frame(
    'f34_kitchen'='housing_basic_living_8m_1', # Home has kitchen to sit & eat or cook
           'f342'='housing_basic_living_8m_2', # Home has indoor WC
           'f350'='housing_basic_living_8m_3', # Use of running hot water
     'f35_shower'='housing_basic_living_8m_4', # Use of bath or shower
   
    'g41_kitchen'='housing_basic_living_21m_1', # Home has kitchen diner OR for cooking only
           'g412'='housing_basic_living_21m_2', # Home has indoor flushing toilet
           'g415'='housing_basic_living_21m_3', # Home has running hot water
     'g41_shower'='housing_basic_living_21m_4', # Home has bath OR shower
    
    'h29_kitchen'='housing_basic_living_3y_1', # Home has kitchen with sitting space OR for cooking only
           'h292'='housing_basic_living_3y_2', # Home has indoor flushing toilet
           'h295'='housing_basic_living_3y_3', # Shared or sole use hot running water
     'h29_shower'='housing_basic_living_3y_4'),# Shared or sole use bath OR shower
    yes=c('No','N','no'), 
    no=c('Yes','Y sole use','Yes Sole Use','yes sole use','Yes Shared','Y shared','yes shared')
  ),
  # Add neighbourhood problems
  dichotomize_cont(
    vars=data.frame('g496'='neighbourhood_problems_21m',
                    'h366'='neighbourhood_problems_3y'),
    rule='>= .8', quantile=TRUE # NOTE: no manual-advised cut-off so we are use a 80th percentile cutoff.
    # Neighborhood stress score:  based on the following items:
    # •	G485 Badly fitted doors and windows are problem •	G486 Poor ventilation
    # •	G487 Noise in rooms of home is problem • G488 Noise from other homes is problem
    # •	G489 Noise from outside is problem • G490 Rubbish dumped in neighbourhood is problem
    # •	G491 Dog dirt on pavement is problem • G492 Worry about vandalism is problem
    # •	G493 Worry about burglaries is problem • G494 Worry about attacks is problem
    # •	G495 Disturbance from youth is problem.
    # Variables G485-G495 were recoded (4,3=0)(2=1)(1=2). G496 = G485+.......+G495
    # higher scores = worse neighborhood problems
  ),
  # Parental education 
  dichotomize_cont(
    vars=data.frame('m_education'='m_education_5y',
                    'p_education'='p_education_5y'), rule='> 1') # No university degree or nurse
), domain='CR')

# ------------------------------------------------------------------------------
cat('
================================================================================
# 3. PARENTAL RISK {POSTNATAL}
================================================================================\n')

PR_postnatal <- combine_items(
  cbind(
  dichotomize(vars = data.frame(
    'f227'='criminal_record_parent_8m_1',	# MUM in trouble with law >CH born
    'f236'='criminal_record_parent_8m_2', # PTNR in trouble with law >CH born
    'f249'='criminal_record_parent_8m_3', # Court conviction >CH born
    'g307'='criminal_record_parent_21m_1',# Mum in trouble with law >CH8MTHs
    'g316'='criminal_record_parent_21m_2',# Partner in trouble with law >CH8MTHs
    'g329'='criminal_record_parent_21m_3',# Mum convicted of offence >CH8MTHs
    'h217'='criminal_record_parent_3y_1', # Whether mum was in trouble with the law since study child was 18 months old
    'h226'='criminal_record_parent_3y_2', # Whether partner was in trouble with the law since study child was 18 months
    'h239'='criminal_record_parent_3y_3', # Whether mum was convicted of an offence since study child was 18 months old
    'j307'='criminal_record_parent_4y_1', # MUM in Trouble W Law> CH 30 MTHs
    'j316'='criminal_record_parent_4y_2', # PTR in Trouble W Law> CH 30 MTHs
    'j329'='criminal_record_parent_4y_3', # MUM Convicted of Offence> CH 30 MTHs
   'k4007'='criminal_record_parent_5y_1', # Mother was in trouble with the law in past year
   'k4016'='criminal_record_parent_5y_2', # Mothers partner was in trouble with the law in past year
   'k4029'='criminal_record_parent_5y_3', # Mother was convicted of an offence in past year
   'l4007'='criminal_record_parent_6y_1', # Respondent was in trouble with the law since study child's 5th birthday
   'l4016'='criminal_record_parent_6y_2', # Respondent's partner in trouble with the law since study child's 5th birthday
   'l4029'='criminal_record_parent_6y_3', # Respondent convicted of an offence since study child's 5th birthday
   'p2007'='criminal_record_parent_9y_1', # Mother was in trouble with the law since the study child's 6th birthday
   'p2016'='criminal_record_parent_9y_2', # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
   'p2029'='criminal_record_parent_9y_3', # Mother was convicted of an offence since the study child's 6th birthday

    'f232'='work_problems_8m_1', # Work PROBS for PTNR >CH born
    'f233'='work_problems_8m_2', # Work PROBS for MUM >CH born
    'g312'='work_problems_21m_1',# Partner had problems with work >CH8MTHs
    'g313'='work_problems_21m_2',# Mum had problems with work >CH8MTHs
    'h222'='work_problems_3y_1', # Whether partner had problems at work since study child was 18 months old
    'h223'='work_problems_3y_2', # Whether mum had problems at work since study child was 18 months old
    'j312'='work_problems_4y_1', # Partner Had PROBs at Work> CH 30 MTHs
    'j313'='work_problems_4y_2', # MUM Had PROBs at Work> CH 30 MTHs
   'k4012'='work_problems_5y_1', # Mothers partner had problems at work in past year
   'k4013'='work_problems_5y_2', # Mother had problems at work in past year
   'l4012'='work_problems_6y_1', # Respondent's partner had problems at work since study child's 5th birthday
   'l4013'='work_problems_6y_2', # Respondent had problems at work since study child's 5th birthday
   'p2012'='work_problems_9y_1', # Mother's husband/partner had problems at work since the study child's 6th birthday
   'p2013'='work_problems_9y_2',  # Mother had problems at work since the study child's 6th birthday

    'f248'='m_attempted_suicide_8m', # Attempted suicide > CH born
    'g328'='m_attempted_suicide_21m',# Mum attempted suicide >CH8MTHs
    'h238'='m_attempted_suicide_3y', # Whether mum attempted suicide since study child was 18 months old
    'j328'='m_attempted_suicide_4y', # MUM Attempted Suicide> CH 30 MTHs
   'k4028'='m_attempted_suicide_5y', # Mother attempted suicide in past year
   'l4028'='m_attempted_suicide_6y', # Respondent attempted suicide since study child's 5th birthday
   'p2028'='m_attempted_suicide_9y',  # Mother attempted suicide since the study child's 6th birthday

    'f253'='miscarriage_or_abortion_8m_1', # MUM had MC >CH born
    'f254'='miscarriage_or_abortion_8m_2', # MUM had abortion >CH born
    'g333'='miscarriage_or_abortion_21m_1',# Mum had miscarriage >CH8MTHs
    'g334'='miscarriage_or_abortion_21m_2',# Mum had abortion >CH8MTHs
    'h243'='miscarriage_or_abortion_3y_1', # Whether mum had a miscarriage since study child was 18 months old
    'h244'='miscarriage_or_abortion_3y_2', # Whether mum had an abortion since study child was 18 months old
    'j333'='miscarriage_or_abortion_4y_1', # MUM Miscarried> CH 30 MTHs
    'j334'='miscarriage_or_abortion_4y_2', # MUM Had An Abortion> CH 30 MTHs
   'k4033'='miscarriage_or_abortion_5y_1', # Mother had a miscarriage in past year
   'k4034'='miscarriage_or_abortion_5y_2', # Mother had an abortion in past year
   'l4033'='miscarriage_or_abortion_6y_1', # Respondent had miscarriage since study child's 5th birthday
   'l4034'='miscarriage_or_abortion_6y_2', # Respondent had an abortion since study child's 5th birthday
   'p2033'='miscarriage_or_abortion_9y_1', # Mother had a miscarriage since the study child's 6th birthday
   'p2034'='miscarriage_or_abortion_9y_2'  # Mother had an abortion since the study child's 6th birthday
  ), yes=yes, no=no),
  # Add maternal age (same as prenatal) & paternal age
  dichotomize_cont(
    vars = data.frame('mz028b'='m_age_birth', # Grouped age of mother at delivery # NOTE: FAI also uses b023 = Mums age when 1ST PREG
                      'pb910'='p_age_birth'), # Age of PTNR at completion of ?aire
    rule = '< 19' # based on Cecil et al. (2014); Rijlaarsdam et al. (2016) [see prenatal score]
  ),
  # Add maternal anxiety
  dichotomize(vars=data.frame(
    'f173'='m_anxiety_8m', # CCEI anxiety subscale (complete)
    'g268'='m_anxiety_21m',# CCEI anxiety subscale (complete)
    'h178a'='m_anxiety_3y', # CCEI anxiety subscale (complete)
    'm_anx_5y'='m_anxiety_5y', # CCEI score [created above]
    'm_anx_6y'='m_anxiety_6y'),# CCEI score [created above]
    
    no = c('not anxious', 0:8), yes = c('very anxious', 9:16)
  ),
  # Add parental depression and paternal anxiety
  dichotomize(vars=data.frame(
    'f200'='m_depression_8m_1',  # EPDS total score
    'f021'='m_depression_8m_2',  # Self reported depression
    'g290'='m_depression_21m_1', # EPDS total score
    'g021'='m_depression_21m_2', # Self reported depression
   'h200a'='m_depression_3y_1',  # EPDS total score
    'h013'='m_depression_3y_2',  # Self reported depression
    'j012'='m_depression_4y',    # Self reported depression
'm_dep_5y'='m_depression_5y_1',  # EPDS score [created above]
   'k1011'='m_depression_5y_2',  # Self reported depression
   'l3011'='m_depression_6y',    # Self reported depression
'm_dep_8y'='m_depression_9y_1',  # EPDS score [created above] # NOTE: 8y but combined with 9y
   'p1011'='m_depression_9y_2', # Self reported depression

   'pd200'='p_depression_8m_1',  # EPDS total score
   'pd021'='p_depression_8m_2',  # Self reported depression
   'pe290'='p_depression_21m_1', # EPDS total score
   'pe021'='p_depression_21m_2', # Self reported depression
'p_dep_3y'='p_depression_3y_1',  # EPDS score [created above]
  'pf1011'='p_depression_3y_2',  # Self reported depression
  'pg1011'='p_depression_4y',    # Self reported depression
'p_dep_5y'='p_depression_5y_1',  # EPDS score [created above]
  'ph1011'='p_depression_5y_2',  # Self reported depression
'p_dep_6y'='p_depression_6y_1',  # EPDS score [created above]
  'pj3011'='p_depression_6y_2',  # Self reported depression
  'pl1061'='p_depression_9y_1',  # Self reported depression # NOTE: 8y but combined with 9y
  'pm1011'='p_depression_9y_2', # Self reported depression

   'pd020'='p_anxiety_8m', # had anxiety/nerves
   'pe020'='p_anxiety_21m',# had anxiety/nerves
  'pf1010'='p_anxiety_3y', # had anxiety/nerves
  'pg1010'='p_anxiety_4y', # had anxiety/nerves
  'ph1010'='p_anxiety_5y', # had anxiety/nerves
  'pj3010'='p_anxiety_6y', # had anxiety/nerves
  'pm1010'='p_anxiety_9y'),# had anxiety/nerves

no = c('not depressed', 0:12, 'N','No','no','No, never'),
yes = c('very depressed', 13:30, # EPDS cutoffs
        'Y saw DR','Y did not see DR', # Self reports
        'Yes & saw Dr', 'Yes,Did not see  Dr',
        'yes and saw doctor','yes didnt see doctor',
        'Yes & saw DR', 'Yes didnt see DR',
        'Yes, consulted doctor','Yes, did not consult doctor',
        'Yes & consulted doctor','Yes but did not consult doctor',
        'Yes, Saw Doctor', 'Yes, No Doctor',
        'Yes Consulted Dr', 'Yes Not Consult Dr',
        'Yes, and consulted doctor', 'Yes, but did not consult doctor',
        'Yes and consulted doctor', 'Yes but did not consult doctor',
        'Yes, consulted a doctor', 'Yes, did not consult a doctor',
        'Yes and consulted a doctor', 'Yes but did not consult a doctor',
        'Yes, recently', 'Yes, in past not now')
  )
), domain='PR')

# ------------------------------------------------------------------------------

cat('
================================================================================
# 4. INTERPERSONAL RISK {POSTNATAL}
================================================================================\n')

IR_postnatal <- combine_items(
  cbind(
  dichotomize(vars = data.frame(
    'f228'='divorce_8m_1', # Divorce
    'f237'='divorce_8m_2', # Separation with partner
    'g308'='divorce_21m_1',# Mum divorced >CH8MTHs
    'g317'='divorce_21m_2',# Mum and partner separated >CH8MTHs
    'h218'='divorce_3y_1', # Whether got divorced since study child was 18 months old
    'h227'='divorce_3y_2', # Whether mum and partner separated since study child was 18 months old
    'j308'='divorce_4y_1', # MUM Divorced> CH 30 MTHs
    'j317'='divorce_4y_2', # MUM & Partner Separated> CH 30 MTHs
   'k4008'='divorce_5y_1', # Mother was divorced in past year
   'k4017'='divorce_5y_2', # Mother and partner separated in past year
   'l4008'='divorce_6y_1', # Respondent was divorced since study child's 5th birthday
   'l4017'='divorce_6y_2', # Respondent separated from partner since study child's 5th birthday
   'p2008'='divorce_9y_1', # Mother was divorced since the study child's 6th birthday
   'p2017'='divorce_9y_2', # Mother and husband/partner separated since the study child's 6th birthday

    'f229'='p_rejected_child_8m', # Child not wanted by partner
    'g309'='p_rejected_child_21m',# Partner rejected child >CH8MTHs
    'h219'='p_rejected_child_3y', # Whether partner rejected children since study child was 18 months old
    'j309'='p_rejected_child_4y', # MUM Found PTR Not Want CH> CH 30 MTHs y/n
   'k4009'='p_rejected_child_5y', # Mother found that her partner did not want her child in past year
   'l4009'='p_rejected_child_6y', # Respondent found their partner did not want their child since study child's 5th birthday
   'p2009'='p_rejected_child_9y', # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday

    'f235'='p_went_away_8m', # Partner went away
    'g315'='p_went_away_21m',# Partner went away >CH8MTHs
    'h225'='p_went_away_3y', # Whether partner went away since study child was 18 months old
    'j315'='p_went_away_4y', # Partner Went Away> CH 30 MTHs
   'k4015'='p_went_away_5y', # Mothers partner went away in past year
   'l4015'='p_went_away_6y', # Respondent's partner went away since study child's 5th birthday
   'p2015'='p_went_away_9y', # Mother's husband/partner went away since the study child's 6th birthday

   'f245'='m_new_partner_8m',  # Got married
   'g325'='m_new_partner_21m', # Mum got married >CH8MTHs
   'h235'='m_new_partner_3y',  # Whether mum got married since study child was 18 months old
   'j325'='m_new_partner_4y',  # MUM Got Married> CH 30 MTHs
  'k4025'='m_new_partner_5y',  # Mother got married in past year
  'l4025'='m_new_partner_6y_1',# Respondent got married since study child's 5th birthday
  'l4040'='m_new_partner_6y_2',# Respondent found new partner since study child's 5th birthday
  'p2025'='m_new_partner_9y_1',# Mother got married since the study child's 6th birthday
  'p2040'='m_new_partner_9y_2',# Mother found a new partner since the study child's 6th birthday

   'f239'='family_conflict_emotional_8m_1', # Argued with partner
   'f256'='family_conflict_emotional_8m_2', # Partner emotionally cruel to Mum
   'g319'='family_conflict_emotional_21m_1',# Mum argued with partner >CH8MTHs
   'g336'='family_conflict_emotional_21m_2',# Partner emotionally cruel to Mum >CH8MTHs
   'h229'='family_conflict_emotional_3y_1', # Whether mum argued with partner since study child was 18 months old
   'h246'='family_conflict_emotional_3y_2', # Whether partner was emotionally cruel to mum since study child was 18 months old
   'h580'='family_conflict_emotional_3y_3', # In past 3 months, mum shouted at partner and/or called them names # NOTE: prev not included
   'j319'='family_conflict_emotional_4y_1', # MUM Argued W Partner> CH 30 MTHs
   'j336'='family_conflict_emotional_4y_2', # PTR Emotionally Cruel to MUM> CH 30 MTHs
  'k4019'='family_conflict_emotional_5y_1', # Mother argued with her partner in past year
  'k4036'='family_conflict_emotional_5y_2', # Mothers partner was emotionally cruel to her in past year
  'l4019'='family_conflict_emotional_6y_1', # Respondent argued with partner since study child's 5th birthday
  'l4036'='family_conflict_emotional_6y_2', # Respondent's partner was emotionally cruel to them since study child's 5th birthday
  'p2019'='family_conflict_emotional_9y_1', # Mother argued with husband/partner since the study child's 6th birthday
  'p2036'='family_conflict_emotional_9y_2', # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
  'p3153'='family_conflict_emotional_9y_3', # Mother/husband/partner shouted or called one another names in the past 3 months (in Char script: DV_Shouted_9Y)
  
   'f242'='family_conflict_violence_8m',   # Physically hurt by PTNR >CH born
   'g322'='family_conflict_violence_21m_1',# Partner physically cruel to Mum >CH8MTHs
   'g712'='family_conflict_violence_21m_2',# Hit or slapped PTNR in past 3 Months
   'g713'='family_conflict_violence_21m_3',# Threw something in anger in past 3 mths
   'h232'='family_conflict_violence_3y_1', # Whether partner was physically cruel to mum since study child was 18 months old
   'h581'='family_conflict_violence_3y_2', # In past 3 months, mum hit or slapped partner
   'j322'='family_conflict_violence_4y',   # Partner PHYS Cruel to MUM> CH 30 MTHs
  'k4022'='family_conflict_violence_5y',   # Mothers partner was physically cruel to her in past year
  'l4022'='family_conflict_violence_6y',   # Respondent's partner was physically cruel to them since study child's 5th birthday
  'p2022'='family_conflict_violence_9y_1', # Mother's husband/partner was physically cruel to her since the study child's 6th birthday 
  'p3154'='family_conflict_violence_9y_2', # Mother/husband/partner hit or slapped one another in the past 3 months (in Char script: DV_Hit_9Y)
  'p3155'='family_conflict_violence_9y_3', # Mother/husband/partner threw or broke things in the past 3 months (in Char script: DV_Break_9Y)

    'f240'='argued_fam_friends_8m', # Argued with family or friend
    'g320'='argued_fam_friends_21m',# Mum argued with family and friends >CH8MTHs
    'h230'='argued_fam_friends_3y', # Whether mum argued with family and friends since study child was 18 months old
    'j320'='argued_fam_friends_4y', # MUM Argued W FMLY & FRDs> CH 30 MTHs
   'k4020'='argued_fam_friends_5y', # Mother argued with family and friends in past year
   'l4020'='argued_fam_friends_6y', # Respondent argued with family/friends since study child's 5th birthday
   'p2020'='argued_fam_friends_9y'  # Mother argued with family and friends since the study child's 6th birthday
  ), yes=yes, no=no),
  dichotomize(vars = data.frame(
    'f500'='marital_status_8m', # Current PTNR              --OR f460 # PRES marital status         
    'g517'='marital_status_21m',# Mums marital status       --OR g590 # Mum has partner
    'h480'='marital_status_3y', # Mum has a current partner --OR h386 # Mums present marital status
    'j600'='marital_status_4y'),# MUM Currently has Partner --OR j370 # PRES Marital Status
    yes=c('N','No Partner','no partner','Never Married','Widowed','Divorced','Separated'), 
    no=c('Y','Yes Male Partner','Yes Female Partner','yes male partner','yes female partner',
        'Married Once','Married > Once')
  ),
  dichotomize(vars = data.frame(
    'f911'='partner_support_8m_1',  # PTNR provides EMOT support
    'f916'='partner_support_8m_2',  # Can rely on PTNR if tired
    
    'g234'='social_network_emotional_21m', # No. people discuss personal problems with mum
   
    'f917'='social_network_practical_8m', # FAM would help if financial PROB
    'g236'='social_network_practical_21m_1',  # No. people mum could borrow 100 from
    'g237'='social_network_practical_21m_2'), # No. family and friends would help in adversity
    
    yes=c('None','Never felt'), # FAI documentation
    no=c('1','2-4','>4','Felt exactly','Felt often','Felt SMTS')
  ),
  dichotomize(vars = data.frame(
    'f914'='partner_support_8m_3'),  # Worried PTNR might leave
    yes=c('Felt exactly'), # FAI documentation
    no=c('Never felt','Felt SMTS','Felt often')
  ),
  dichotomize(vars = data.frame(
    'f910'='social_network_emotional_8m'), # No-one to share feelings with
    yes=c('Felt exactly','Felt often'), # FAI documentation
    no=c('Never felt','Felt SMTS')
  ),
  # Affection score with modes
  dichotomize_cont(vars = data.frame('f595'='partner_affection_8m'), rule = '> 31'), # FAI documentation
  # Affection score with modes
  dichotomize_cont(vars = data.frame('h629'='partner_affection_3y'), rule = '> 34'), # FAI documentation
  # Agression score with modes
  dichotomize_cont(vars = data.frame('f598'='partner_aggression_8m'), rule = '< 8'), # FAI documentation
  # Family size
  dichotomize_cont(vars = data.frame('g502'='family_size_21m', # No. of children in household now
                                     'h372'='family_size_3y',  # No of children < 16 yrs in home
                                     'j362'='family_size_4y'), # NO of Children< 16 YRs in Home
                   rule = '> 3')
  
), domain='IR')

# ------------------------------------------------------------------------------
cat('
================================================================================
# 5. DIRECT VICTIMIZATION {POSTNATAL}
================================================================================\n')

DV_postnatal <- combine_items(
  dichotomize(vars = data.frame(
    'f8fp475'='bullying_8y_1', # Bullying, Child is relational victim: F8
    'f8fp470'='bullying_8y_2', # Bullying, Child is overt victim: F8

    'kd504a'='physical_violence_18m',  # Ch physically hurt by someone (adj)
     'kf454'='physical_violence_30m_1',# Child physically hurt > 18 months # NOTE variables below
     'ke017'='physical_violence_30m_2',# CH slapped (2y)
     'kj464'='physical_violence_3y',	 # Child Was Physically Hurt By Person
     'kl474'='physical_violence_4y',	 # Child was physically hurt by someone since age 3
    'kn4004'='physical_violence_5y',	 # Child physically hurt by someone in past 15 months
     'kq364'='physical_violence_6y',   # Child was physically hurt by someone since his/her 5th birthday
    'kt5004'='physical_violence_9y_1', # Since 7th birthday child has been physically hurt by someone # NOTE: 8y but combined with 9y
     'ku298'='physical_violence_9y_2', # Child is slapped or hit
    
    'kd505a'='sexual_abuse_18m', # Ch sexually abused (adj)
     'kf455'='sexual_abuse_30m', # Child sexually abused > 18 months
     'kj465'='sexual_abuse_3y',	 # Child Sexually Abused
     'kl475'='sexual_abuse_4y',	 # Child was sexually abused since age 3
    'kn4005'='sexual_abuse_5y',	 # Child sexually abused in past 15 months
     'kq365'='sexual_abuse_6y',  # Child was sexually abused by someone since his/her 5th birthday
    'kt5005'='sexual_abuse_8y',  # Since 7th birthday child has been sexually abused

      'f246'='p_cruelty_physical_8m', # PTNR physically cruel to CHDR >CH born
      'g326'='p_cruelty_physical_21m',# Partner physically cruel to children >CH8MTHs
      'h236'='p_cruelty_physical_3y', # Whether partner was physically cruel to children since study child was 18 months old
      'j326'='p_cruelty_physical_4y', # PTR PHYS Cruel to CDRN> CH 30 MTHs
     'k4026'='p_cruelty_physical_5y', # Mothers partner was physically cruel to children in past year
     'l4026'='p_cruelty_physical_6y', # Respondent's partner physically cruel to respondent's children since study child's 5th birthday
     'p2026'='p_cruelty_physical_9y', # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday

      'f247'='m_cruelty_physical_8m', # Mum physically cruel to children
      'g327'='m_cruelty_physical_21m',# Mum physically cruel to children >CH8MTHs
      'h237'='m_cruelty_physical_3y', # Whether mum was physically cruel to children since study child was 18 months old
      'j327'='m_cruelty_physical_4y', # MUM PHYS Cruel to CDRN> CH 30 MTHs
     'k4027'='m_cruelty_physical_5y', # Mother was physically cruel to children in past year
     'l4027'='m_cruelty_physical_6y', # Respondent physically cruel to own children since study child's 5th birthday
     'p2027'='m_cruelty_physical_9y', # Mother was physically cruel to her children since the study child's 6th birthday

     'f257'='p_cruelty_emotional_8m', # Partner emotionally cruel to children
     'g337'='p_cruelty_emotional_21m',# Partner emotionally cruel to children >CH8MTHs
     'h247'='p_cruelty_emotional_3y', # Whether partner was emotionally cruel to children since study child was 18 months old
     'j337'='p_cruelty_emotional_4y', # PTR Emotional Cruel to CDRN> CH 30 MTHs
    'k4037'='p_cruelty_emotional_5y', # Mothers partner was emotionally cruel to children in past year
    'l4037'='p_cruelty_emotional_6y', # Respondent's partner was emotionally cruel to respondent's children since study child's 5th birthday
    'p2037'='p_cruelty_emotional_9y', # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday

     'f258'='m_cruelty_emotional_8m', # MUM EMOT cruel to CHDR >CH born
     'g338'='m_cruelty_emotional_21m',# Mum emotionally cruel to children >CH8MTHs
     'h248'='m_cruelty_emotional_3y', # Whether mum was emotionally cruel to children since study child was 18 months old
     'j338'='m_cruelty_emotional_4y', # MUM Emotional Cruel to CDRN> CH 30 MTHs
    'k4038'='m_cruelty_emotional_5y', # Mother was emotionally cruel to children in past year
    'l4038'='m_cruelty_emotional_6y', # Respondent was emotionally cruel to their children since study child's 5th birthday
    'p2038'='m_cruelty_emotional_9y'  # Mother was emotionally cruel to her children since the study child's 6th birthday
  ), yes=yes, no=no
  
), domain='DV')

# ==============================================================================

postnatal_stress <- cbind(idc, LE_postnatal, CR_postnatal, PR_postnatal, IR_postnatal, DV_postnatal)

postnatal_stress$pos_tot_missing_perc = apply(
  postnatal_stress[,-grep('score|missing|idc',names(postnatal_stress))], 1, percent_missing)

postnatal_stress$pos_ELS_score <- rowSums(postnatal_stress[,grep('score', names(postnatal_stress))], na.rm = FALSE)

for (s in names(postnatal_stress)[grep('score', names(postnatal_stress))]) {
  cat('\nPOSTNATAL ELS -', s,'\n')
  print(summary(postnatal_stress[,s]))
}

# ==============================================================================

sink()
dev.off()


save(prenatal_stress, postnatal_stress, file='ELS_score.RData')
