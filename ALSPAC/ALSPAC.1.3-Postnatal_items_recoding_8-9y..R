
# POSTNATAL 8-9Y ITEMS RECODING

# This script is used for dichotomising postnatal ELS variables into 1 = risk and 0 = no risk. 

################################################################################
################################################################################

# Source dichotomization function and also automatically load the data into alspac.table
source("ALSPAC.0-Setup_and_functions.R")

################################################################################
################################################################################
                              # 1. LIFE EVENTS
################################################################################

# MOTHER-BASED LIFE EVENTS
LE_9Y <- dichotomize(
  vars =  c("p2000",  # Husband/partner died since the study child's 6th birthday
            "p2001",  # One of mother's children died since the study child's 6th birthday
            "p2002",  # Mother's friend or relative died since the study child's 6th birthday
            "p2003",  # One of mother's children was ill since the study child's 6th birthday
            "p2004",  # Mother's husband/partner was ill since the study child's 6th birthday
            "p2005",  # Mother's friend or relative was ill since the study child's 6th birthday
            "p2006",  # Mother was admitted to hospital since the study child's 6th birthday
            "p2010",  # Mother was very ill since the study child's 6th birthday
            "p2011",  # Mother's husband/partner lost his job since the study child's 6th birthday
            "p2012",  # Mother's husband/partner had problems at work since the study child's 6th birthday
            "p2013",  # Mother had problems at work since the study child's 6th birthday
            "p2014",  # Mother lost her job since the study child's 6th birthday
            "p2021",  # Mother moved house since the study child's 6th birthday
            "p2030",  # Mother became pregnant since the study child's 6th birthday
            "p2031",  # Mother started a new job since the study child's 6th birthday
            "p2032",  # Mother returned to work since the study child's 6th birthday
            "p2035",  # Mother took an examination since the study child's 6th birthday
            "p2039",  # Mother's house or car was burgled since the study child's 6th birthday
            "p2041",  # One of mother's children started school since the study child's 6th birthday
            "p2042",  # Mother's husband/partner started a new job since the study child's 6th birthday
            "p2043",  # A pet died since the study child's 6th birthday
            "p2044"), #  Mother had an accident since the study child's 6th birthday
  yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+"),
  no = c("No, did not happen in past 3 years") )
# "Relevant comment", "Relevant text but no box ticked" set to  NA

# CHILD-BASED LIFE EVENTS
LE_child_8Y <- dichotomize(
  vars =  c("kt5000",  # Since 7th birthday child has been taken into care
            "kt5001",  # Since 7th birthday child's pet died
            "kt5002",  # Since 7th birthday child moved home
            "kt5003",  # Since 7th birthday child had shock or fright
            "kt5006",  # Since 7th birthday child has had someone in family die
            "kt5007",  # Since 7th birthday child has been separated from mother
            "kt5008",  # Since 7th birthday child has been separated from father
            "kt5009",  # Since 7th birthday child has had a new mother or father
            "kt5010",  # Since 7th birthday child has had a new brother or sister
            "kt5011",  # Since 7th birthday child has been admitted to hospital
            "kt5012",  # Since 7th birthday child has changed their caretaker
            "kt5013",  # Since 7th birthday child has been separated from someone else
            "kt5014",  # Since 7th birthday child has started a new school
            "kt5015"), # Since 7th birthday child has lost their best friend
  yes = c("Yes, very upset","Yes, quite upset","Yes, bit upset", "Yes, not upset"),
  no = c("No") )
# "Other text answer", "DK", "Relevant text but no box ticked" set to NA

################################################################################
                            # 2. CONTEXTUAL RISK
################################################################################

CR_9Y <- dichotomize(
  vars = c("p2018",  # Mother's income was reduced since the study child's 6th birthday
           "p2023",  # Mother became homeless since the study child's 6th birthday
           "p2024"), # Mother had a major financial problem since the study child's 6th birthday
  yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+"),
  no = c("No, did not happen in past 3 years") )

################################################################################
                               # 3. PARENTAL RISK
################################################################################

PR_9Y <- dichotomize(
  vars = c("p2007",  # Mother was in trouble with the law since the study child's 6th birthday
           "p2016",  # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
           "p2028",  # Mother attempted suicide since the study child's 6th birthday
           "p2029",  # Mother was convicted of an offence since the study child's 6th birthday
           "p2033",  # Mother had a miscarriage since the study child's 6th birthday
           "p2034"), # Mother had an abortion since the study child's 6th birthday
  yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+"),
  no = c("No, did not happen in past 3 years") )
# "Relevant comment" set to NA

################################################################################
                             # INTERPERSONAL RISK
################################################################################

# Split into 3 sections due to different labels

IR_9Y1 <- dichotomize(
  vars = c("p2008",  # Mother was divorced since the study child's 6th birthday
           "p2009",  # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday
           "p2015",  # Mother's husband/partner went away since the study child's 6th birthday
           "p2017",  # Mother and husband/partner separated since the study child's 6th birthday
           "p2019",  # Mother argued with husband/partner since the study child's 6th birthday
           "p2020",  # Mother argued with family and friends since the study child's 6th birthday
           "p2022",  # Mother's husband/partner was physically cruel to her since the study child's 6th birthday
           "p2025",  # Mother got married since the study child's 6th birthday
           "p2026",  # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday
           "p2027",  # Mother was physically cruel to her children since the study child's 6th birthday
           "p2036",  # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
           "p2037",  # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday
           "p2038",  # Mother was emotionally cruel to her children since the study child's 6th birthday
           "p2040"), # Mother found a new partner since the study child's 6th birthday
  yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+"),
  no = c("No, did not happen in past 3 years") )

IR_9Y2 <- dichotomize(
  vars = c("p3153",  # Mother/husband/partner shouted or called one another names in the past 3 months (in Char script: DV_Shouted_9Y)
           "p3154",  # Mother/husband/partner hit or slapped one another in the past 3 months (in Char script: DV_Hit_9Y)
           "p3155"), # Mother/husband/partner threw or broke things in the past 3 months (in Char script: DV_Break_9Y)
  yes = c("Yes, mother did this","Yes, partner did this","Yes, both did this"),
  no = c("No") )

IR_9Y3 <- dichotomize(
  vars = c("ku298"), # Child is slapped or hit (in Char script: Par_Smack_9Y_Any)
  yes = c("Every day","Several times a week","Once or twice a week","Once or twice a month", "Rarely"),
  no = c("Never") )

IR_9Y <- cbind(IR_9Y1, IR_9Y2, IR_9Y3)

################################################################################
                            # DIRECT VICTIMIZATION
################################################################################

# Again, split according to possible answers 

DV_8Y <- dichotomize(
  vars = c("f8fp475",  # Bullying, Child is relational victim: F8
           "f8fp470"), # Bullying, Child is overt victim: F8
  yes = c("Yes"), no = c("No") )

DV_9Y <- dichotomize(
  vars = c("kt5004",   # Since 7th birthday child has been physically hurt by someone
           "kt5005"),  # Since 7th birthday child has been sexually abused
  yes = c("Yes, very upset", "Yes, quite upset", "Yes, bit upset", "Yes, not upset"),
  no = c("No") )
# "Other text answer", "DK" set to NA

################################################################################
################################################################################

postnatal_stress_8to9_raw <-cbind(LE_child_8Y, LE_9Y, CR_9Y, PR_9Y, IR_9Y, DV_8Y, DV_9Y)

saveRDS(postnatal_stress_8to9_raw, file.path(alspac_folder, "raw_postnatal_stress_8to9.rds"))
