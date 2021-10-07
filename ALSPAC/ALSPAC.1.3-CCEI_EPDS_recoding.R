
# This script dichotomizes all (prenatal and postnatal) anxiety and depression 
# variables from mothers and partners. These include self-reported depression and 
# anxiety that will be dichotomized usign the dichotomize() function & total scores 
# from CCEI ANXIETY SUBSCALE & EPDS DEPRESSION. 
# This script calculates CCEI anxiety subscale scores & total EPDS depresssion score
# when they were not already available, using the functions ccei_score() and epds_score() 
# respectively. Then dichotomises them into 1 = risk, 0 = no risk using the 
# dich_psychopath() function. 

source("ALSPAC.0-Setup_and_functions.R") # Where ccei_score(), epds_score(), dichotomize() and dich_psychopath() are defined 

################################################################################ 
                        # ANXIETY MOTHER AND PARTNER
################################################################################ 

## Recoding anxiety variable for MOTHER  #######################################

# CCEI anxiety subscale for mothers consists of the following items:
        ## CCEI: Mother feels upset for no obvious reason	b328, c550, e348, f150, g245, h155, k3000, l2000, r4000 (F1)
        ## CCEI: Mother felt like fainting	b330, c552, e350, f152, g247, h157, k3002, l2001, r4001 (F3)
        ## CCEI: Mother feels uneasy & restless	b333, c555, e353, f155, g250, h160, k3005, l2002, r4002 (F6)
        ## CCEI: Mother sometimes feels panicky	b336, c558, e356, f158, g253, h163, k3008, l2003, r4003 (F9)
        ## CCEI: Mother worries a lot	b339, c561, e359, f161, g256, h166, k3011, l2004, r4004 (F12)
        ## CCEI: Mother feels strung up inside	b342, c564, e362, f164, g259, h169, k3014, l2005, r4005 (F15)
        ## CCEI: Mother feels to be going to pieces	b344, c566, e364, f166, g261, h171, k3016, l2006, r4006 (F17)
        ## CCEI: Mother has bad upsetting dreams	b347, c569, e367, f169, g264, h174, k3019, l2007, r4007 (F20)

# Total scores for CCEI anxiety subscale are available at timepoint: 18w gest, 32w gest,
# 8w, 8m, 1y 9m, 2y 9m, But not at: 5y 1m, 6y 1m.
# So we first calculate total CCEI anxiety scores for the missing timepoints. 

# The calculation method was tested using already available total scores and is 
# performed by calling the function ccei_score

# CALCULATING CCEI SCORES FOR 5 YEAR MISSING TIMEPOINT
M_ANX_5Y <- ccei_score(set1 = c('k3000', 'k3014', 'k3016'), 
                       set2 = c('k3002', 'k3005', 'k3011', 'k3019'),
                       set3 = c('k3008'))

# CALCULATING CCEI SCORES FOR 6 YEAR MISSING TIMEPOINT
M_ANX_6Y <- ccei_score(set1 = c('l2000', 'l2005', 'l2006'), 
                       set2 = c('l2001', 'l2002', 'l2004', 'l2007'),
                       set3 = c('l2003'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DICHOTOMISING 

# Scores on the CCEI anxiety subscale range from 0 to 16. Women who scored 9 or 
# higher, a cutoff point which has been used previously in ALSPAC (Heron et al., 2004), 
# were classified as having anxiety.

# Prenatal CCEI total anxiety scores
M_ANX_18Wg <- dich_psychopath("b351", yes = c(9:15), no = c(1:8), 
                           yes_label = 'very anxious', no_label = 'not anxious')

M_ANX_32Wg <- dich_psychopath("c573",  yes = c(9:15), no = c(1:8), 
                           yes_label = 'very anxious', no_label = 'not anxious')

# Postnatal CCEI total anxiety scores
M_ANX_8wk <- dich_psychopath("e371",  yes = c(9:15), no = c(1:8), 
                           yes_label = 'very anxious', no_label = 'not anxious')

M_ANX_8m  <- dich_psychopath("f173",   yes = c(9:15), no = c(1:8), 
                           yes_label = 'very anxious', no_label = 'not anxious')

M_ANX_21m <- dich_psychopath("g268",   yes = c(9:15), no = c(1:8), 
                           yes_label = 'very anxious', no_label = 'not anxious')

M_ANX_3y  <- dich_psychopath("h178a",  yes = c(9:16), no = c(0:8), 
                           yes_label = 'very anxious', no_label = 'not anxious')

# Merge them together
M_ANX <- cbind(M_ANX_18Wg, M_ANX_32Wg, M_ANX_8wk, M_ANX_8m, M_ANX_21m, M_ANX_3y)

M_ANX$CCEI_total_5ya_rec <- ifelse(M_ANX_5Y$sumscore >= 9, 1, 0)
M_ANX$CCEI_total_6ya_rec <- ifelse(M_ANX_6Y$sumscore >= 9, 1, 0)

## Recoding anxiety variable for PARTNER #######################################

# Prenatal paternal anxiety is measured using the anxiety subscale of the CCEI.
# Cut point has been used previously in ALSPAC (Heron et al., 2004): >8 is risk, <= 8 no risk
P_ANX_pren <- dich_psychopath("pb233", yes = c(9:15), no = c(1:8), 
                           yes_label = 'Very anxious', no_label = 'Not anxious')


# Postnatal paternal anxiety is only available as self-reported "anxiety/nerves". 
# We now dichotomize items for had anxiety/nerves (8m, 21m, 2y 9m, 3y 11m, 5y 1m, 6y 1m, 8y 1m, 9y 2m)
P_ANX_post <- dichotomize(
  vars = c("pd020",   # 8m
           "pe020",   # 21m
           "pf1010",  # 3y
           "pg1010",  # 4y
           "ph1010",  # 5y
           "pj3010",  # 6y
           "pm1010"), # 9y
  yes = c("Yes, Saw Doctor",            "Yes, No Doctor", 
          "Yes Consulted Dr",           "Yes Not Consult Dr", 
          "Yes, and consulted doctor",  "Yes, but did not consult doctor", 
          "Yes and consulted doctor",   "Yes but did not consult doctor",
          "Yes, consulted a doctor",    "Yes, did not consult a doctor", 
          "Yes and consulted a doctor", "Yes but did not consult a doctor",
          "Yes, consulted doctor",      "Yes, did not consult doctor"),
  no = c("No")
) # Note: warning of labels not present is expected.

P_ANX <- cbind(P_ANX_pren, P_ANX_post)

################################################################################
################################################################################
                     # DEPRESSION MOTHER AND PARTNER
################################################################################

## MOTHER DEPRESSION - EPDS ####################################################

# EPDS total score for mothers consists of the following items:
    #EPDS: Sense of humour in past WK	k3030, n6060 #D24
    #EPDS: Looked forward to things in past WK k3031, n6061  #D25
    #EPDS: Unnecessary self blame in past WK	k3032, n6062 #D26
    #EPDS: Unnecessary anxiety or worry in past WK	k3033, n6063 #D27
    #EPDS: Unnecessary panic or fear in past WK k3034, n6064 #D28
    #EPDS: Things getting too much in past WK	k3035, n6065 #D29
    #EPDS: Sleeping PROB due to sadness in past WK k3036, n6066  #D30
    #EPDS: Sad or miserable in past WK k3037, n6067  #D31
    #EPDS: Crying due to unhappiness in past WK k3038, n6068 #D32
    #EPDS: Considered self-harm in past WK k3039, n6069 #D33

# Total scores for EPDS total score are available at timepoint:
# 8w, 8m, 1y 9m, 2y 9m, But not at: 5y 1m, and 8y
# So we first calculate total scores for EPDS for the missing timepoints using the
# epds_score() function.

# CALCULATE EPDS TOTAL SCORE 18w gestation
M_DEP_18Wg <- epds_score(set = c('b360', 'b361', 'b363'), # d24, d25, d27, (1 = 0) (2 = 1) (3 = 2) (4 = 3)
                      revset = c('b362', 'b364', 'b365', # d26, d28, d29, d30, d31, d32, d33 (1 = 3) (2 = 2) (3 = 1) (4 = 0)
                                 'b366', 'b367', 'b368', 'b369'))

# CALCULATE EPDS TOTAL SCORE 32w gestation
M_DEP_32Wg <- epds_score(set = c('c590', 'c591', 'c593'), # d24, d25, d27, (1 = 0) (2 = 1) (3 = 2) (4 = 3)
                      revset = c('c592', 'c594', 'c595',  # d26, d28, d29, d30, d31, d32, d33 (1 = 3) (2 = 2) (3 = 1) (4 = 0)
                                 'c596', 'c597', 'c598', 'c599'))

# CALCULATE EPDS TOTAL SCORE 5y
M_DEP_5Y <- epds_score(set = c('k3030', 'k3031', 'k3033'), # d24, d25, d27, (1 = 0) (2 = 1) (3 = 2) (4 = 3)
                    revset = c('k3032', 'k3034', 'k3035',  # d26, d28, d29, d30, d31, d32, d33 (1 = 3) (2 = 2) (3 = 1) (4 = 0)
                               'k3036', 'k3037', 'k3038', 'k3039'))

# CALCULATE EPDS TOTAL SCORE 8y
M_DEP_8Y <- epds_score(set = c('n6060', 'n6061', 'n6063'), # d24, d25, d27, (1 = 0) (2 = 1) (3 = 2) (4 = 3)
                    revset = c('n6062', 'n6064', 'n6065', # d26, d28, d29, d30, d31, d32, d33 (1 = 3) (2 = 2) (3 = 1) (4 = 0)
                               'n6066', 'n6067', 'n6068', 'n6069'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DICHOTOMISING EPDS TOTAL SCORE - MOTHER (>12 risk, <=12 no risk)
# As described in ALSPAC FAI documentation (preparation for EPDS total score calculation)
# a total score of 13 or more is considered a flag for the need for follow up of 
# possible depressive symptoms.

M_DEP_8W  <- dich_psychopath("e390",  yes = c(13:29), no = c(1:12), 
                       yes_label = 'very depressed', no_label = 'not depressed')

M_DEP_8M  <- dich_psychopath("f200",  yes = c(13:29), no = c(1:12), 
                       yes_label = 'very depressed', no_label = 'not depressed')

M_DEP_21M <- dich_psychopath("g290",  yes = c(13:29), no = c(1:12), 
                       yes_label = 'very depressed', no_label = 'not depressed')

M_DEP_3Y  <- dich_psychopath("h200a", yes = c(13:30), no = c(0:12))

# Merge them together
M_DEP <- cbind(M_DEP_8W, M_DEP_8M, M_DEP_21M, M_DEP_3Y)

# Add missing prenatal scores
M_DEP$m_EPDS_total_18wg <- M_DEP_18Wg$sumscore # for auriliary variable prenatal dep
M_DEP$m_EPDS_total_32wg <- M_DEP_32Wg$sumscore # for auriliary variable prenatal dep
M_DEP$m_EPDS_total_18wga_rec <- ifelse(M_DEP_18Wg$sumscore > 12, 1, 0)
M_DEP$m_EPDS_total_32wga_rec <- ifelse(M_DEP_32Wg$sumscore > 12, 1, 0)

# Add missing postnatal scores
M_DEP$m_EPDS_total_5ya_rec <- ifelse(M_DEP_5Y$sumscore > 12, 1, 0)
M_DEP$m_EPDS_total_8ya_rec <- ifelse(M_DEP_8Y$sumscore > 12, 1, 0)

## MOTHER DEPRESSION - SELF-REPORTS ############################################

# Dichotomising postnatal items for "had depression"
M_DEP_selfr <- dichotomize(
  vars = c("f021",   # 8m
           "g021",   # 21m
           "h013",   # 3y
           "j012",   # 4y
           "k1011",  # 5y
           "l3011",  # 6y
           "p1011"), # 9y
  yes = c("Y saw DR",               "Y did not see DR", 
          "Yes & saw Dr",           "Yes,Did not see  Dr", 
          "yes and saw doctor",     "yes didnt see doctor", 
          "Yes & saw DR",           "Yes didnt see DR", 
          "Yes, consulted doctor",  "Yes, did not consult doctor", 
          "Yes & consulted doctor", "Yes but did not consult doctor"),
  no = c("N","No","no")  
)

## PARTNER DEPRESSION - EPDS ###################################################

# EPDS total score for mothers consists of the following items:
      #EPDS: Sense of humour in past WK	pf4030, ph3030, pj2010  # c24
      #EPDS: Looked forward to things in past WK  pf4031, ph3031, pj2011 #c25
      #EPDS: Unnecessary self blame in past WK	 pf4032, ph3032, pj2012 #c26
      #EPDS: Unnecessary anxiety or worry in past WK	 pf4033, ph3033, pj2013 #c27
      #EPDS: Unnecessary panic or fear in past WK	 pf4034, ph3034, pj2014 #c28
      #EPDS: Things getting too much in past WK	pf4035, ph3035, pj2015 #c29
      #EPDS: Sleeping PROB due to sadness in past WK	 pf4036, ph3036, pj2016 #c30
      #EPDS: Sad or miserable in past WK	pf4037, ph3037, pj2017  #c31
      #EPDS: Crying due to unhappiness in past WK	pf4038, ph3038, pj2018 #c32
      #EPDS: Considered selfharm in past WK	pf4039, ph3039, pj2019 #c33

# Total scores for EPDS total score are available at timepoint: 18w gest, 8w, 8m,
# 1y 9m, but not at: 2y 9m, 5y 1m, and 6y.
# So we first calculate total scores for EPDS for the missing timepoints using the
# epds_score() function.

# CALCULATE EPDS TOTAL SCORE 18W GEST
P_DEP_18Wg <- epds_score(set = c('pb250', 'pb251', 'pb253'), # c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
                      revset = c('pb252','pb254', 'pb255', # c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
                                 'pb256', 'pb257', 'pb258', 'pb259'))

# CALCULATE EPDS TOTAL SCORE 3y
P_DEP_3Y <- epds_score(set = c('pf4030', 'pf4031', 'pf4033'), # c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
                    revset = c('pf4032', 'pf4034', 'pf4035', # c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
                               'pf4036', 'pf4037', 'pf4038', 'pf4039'))

# CALCULATE EPDS TOTAL SCORE 5y
P_DEP_5Y <- epds_score(set = c('ph3030', 'ph3031', 'ph3033'), # c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
                    revset = c('ph3032', 'ph3034', 'ph3035', # c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
                               'ph3036', 'ph3037', 'ph3038', 'ph3039'))

# CALCULATE EPDS TOTAL SCORE 6y
P_DEP_6Y <- epds_score(set = c('pj2010', 'pj2011', 'pj2013'), # c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
                    revset = c('pj2012', 'pj2014', 'pj2015', # c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
                               'pj2016', 'pj2017', 'pj2018', 'pj2019'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DICHOTOMISING EPDS TOTAL SCORE - PARTNER (>12 risk, <=12 no risk)
# EPDS total score partner mode imputed (>12 risk, <=12 no risk) based on ALSPAC FAI documentation

P_DEP_8W    <- dich_psychopath("pc102", yes = c(13:29), no = c(1:12), 
                       yes_label = 'Very depressed', no_label = 'Not depressed')

P_DEP_8M   <- dich_psychopath("pd200", yes = c(13:29), no = c(1:12), 
                       yes_label = 'very depressed', no_label = 'not depressed')

P_DEP_21M  <- dich_psychopath("pe290", yes = c(13:30), no = c(0:12))

# Merge them together
P_DEP <- cbind(P_DEP_8W, P_DEP_8M, P_DEP_21M)

P_DEP$p_EPDS_total_18wg <- P_DEP_18Wg$sumscore # for auxiliary variable
P_DEP$p_EPDS_total_18wga_rec <- ifelse(P_DEP_18Wg$sumscore > 12, 1, 0)
P_DEP$p_EPDS_total_3ya_rec   <- ifelse(P_DEP_3Y$sumscore   > 12, 1, 0)
P_DEP$p_EPDS_total_5ya_rec   <- ifelse(P_DEP_5Y$sumscore   > 12, 1, 0)
P_DEP$p_EPDS_total_6ya_rec   <- ifelse(P_DEP_6Y$sumscore   > 12, 1, 0)

## PARTNER DEPRESSION - SELF-REPORTS ############################################

# Dichotomising postnatal items for "had depression"
P_DEP_selfr <- dichotomize(
  vars = c("pd021",   # 8m
           "pe021",   # 21m
           "pf1011",  # 3y
           "pg1011",  # 4y
           "ph1011",  # 5y
           "pj3011",  # 6y
           "pl1061",  # 8y
           "pm1011"), # 9y
  yes = c("Yes, Saw Doctor",            "Yes, No Doctor", 
          "Yes Consulted Dr",           "Yes Not Consult Dr", 
          "Yes, and consulted doctor",  "Yes, but did not consult doctor", 
          "Yes and consulted doctor",   "Yes but did not consult doctor",
          "Yes, consulted a doctor",    "Yes, did not consult a doctor", 
          "Yes and consulted a doctor", "Yes but did not consult a doctor",
          "Yes, recently",              "Yes, in past not now",
          "Yes, consulted doctor",      "Yes, did not consult doctor"),
  no = c("No", "No, never") 
)
#  Note: gives a warning that some labels are not present in certain variables. 
# This is expected, as not all labels apply to each variable.


################################################################################ 
################################################################################ 
# Merge everithing together
parent_depr_anxiety <-cbind(M_ANX, P_ANX, M_DEP, M_DEP_selfr, P_DEP, P_DEP_selfr)

saveRDS(parent_depr_anxiety, file.path(pathtoresults, "raw_parent_depr_anxiety.rds"))


