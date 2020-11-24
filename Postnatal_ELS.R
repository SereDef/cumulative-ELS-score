
################################################################################
###########################  POSTNATAL ELS SCORE  ##############################
################################################################################

# The following script builds a dataset with all indicators and domain scores in
# the postnatal cumulative risk score used in Schuumans (in preparation). 

# I first read in the data and select the necessary columns (i.e. indicators), 
# merge them, calculate the five domain scores and save two files: (1) the full 
# dataset and (2) a summary overview of how many risk, no-risk and missing values 
# are present for each indicator. 

# ATTENTION !! Before you run this, you need to run the one SPSS Syntax. 
# (V:\Promovendi\Datasets\Gedragsgroep\Questionnaire data\After birth\0-3 years\Harsh parenting_Parent Child Conflict Tactics Scale\Syntax harsh parenting 3jr_PJ10012011)
# or most recent version.

# LE = Life Events, CR = Contextual Risk, PR = Parental risk, IR = Interpersonal risk
# DV = Direct Victimization

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
#-------------------------------------------------------------------------------
## read in data quickly
readquick <- function(filename, rootdir=dir){ # only works for SPSS files
  dataframe <- read.spss(paste(rootdir, filename, sep=""), 
                         use.value.labels = F, to.data.frame = T)
  names(dataframe) <- tolower(names(dataframe))
  dataframe <- replacenas(dataframe)
  return(dataframe)
}
#-------------------------------------------------------------------------------
# This function calculates (continuous) BSI scores based on sex and subscale first 
# and then dichotomizes the scores according to sex specific cutoffs
bsi_scores <- function(items, threshold=length(items)-1, filledinby, cutoff, persons=c(1:5)){
  it <- data.frame(BSI[,items])
  no_na_sum <- rowSums(!is.na(it))
  cont_score <- ifelse(no_na_sum >= threshold, yes = (rowSums(it)/no_na_sum)-1, no = NA)
  
  # Dichotomizing the scores according to sex specific cutoffs
  if(length(persons) != length(cutoff)){
    print('Length persons and length cutoff must be equal. Please, provide as many cutoff values as person values')
    break 
  }else{
    dich_score = matrix(nrow = length(cont_score))
    for (i in 1:length(cont_score)) {
      if (is.na(cont_score[i]) | is.na(filledinby[i]) | cutoff[filledinby[i]] == c(999)){ 
        dich_score[i] = NA
      }else{ 
        dich_score[i] = ifelse(cont_score[i] >= cutoff[filledinby[i]], yes = 1, no = 0) }
    }
  }
  return(as.data.frame(dich_score))
}
#-------------------------------------------------------------------------------
# General functioning of the family was measured with the family assessment device (FAD).
# This function calculates FAD scores accounting for (max 25%) missing values and 
# dichotomizes the scores according to the cutoff for unhealthy family functioning 
# provided by the manual (Byles et al., 1988), = 2.17. 
fad_scores <- function(set){
  fadtotal <- rowSums(set) # Sum items
  no_na_fad <- rowSums(!is.na(set)) # Number of endorsed items
  # Compute mean score + do not calculate when more than 25% of items are missing
  fad_score <- ifelse(no_na_fad >= 9, yes = fadtotal/no_na_fad, no = NA) 
  dich_fad_score <- ifelse(fad_score > 2.17, yes = 1, no = 0) 
  return(dich_fad_score) 
}

#-------------------------------------------------------------------------------
# For several risk factors, we have repeated measurements. To keep the score as 
# comprehensive as possible, we design a function that could apply two different 
# strategies to combine measurements over time:
# • once a risk, always a risk strategy: when at least one of the repeated 
#   measurements reported the adversity, the risk factor is present. Default.
# • chronic risk strategy: when the adversities were always present, the risk 
#   factor is present.
repmeas <- function(items, strategy = 'oncealways'){
  x <- data.frame(items)
  # making sure all repeated measures are binary variables
  for (i in 1:ncol(x)){
    if (range(x[,i], na.rm = T)[1] == 1 & range(x[,i], na.rm = T)[2] == 2){
      x[,i] <- x[,i] - 1}
    else {
      if ( (range(x[,i], na.rm = T)[1] != 0 | range(x[,i], na.rm = T)[2] != 1) & ( range(x[,i], na.rm = T)[1] != range(x[,i], na.rm = T)[2] ) ) { 
        stop('Items are not dichotomized')}
    } 
  }
  # combine and dichotomize the repeated measures
  if (strategy == 'oncealways') { 
    temp <- rowMeans(x, na.rm=T) 
    temp[temp > 0] <- 1 
    return(temp)}
  if (strategy == 'chronic') { 
    temp <- rowMeans(x, na.rm=T) 
    temp[temp < 1] <- 0 
    return(temp)}
}
#-------------------------------------------------------------------------------
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

# NOTE: repmeas and domainscore functions require dichotomized variables 
# (0 and 1, or 1 and 2). They assume that the highest value indicates the risk!
# ------------------------------------------------------------------------------

################################################################################
#### ----------------------- reading in the data  ------------------------- ####
################################################################################

# I read in the raw data, select the necessary columns and dichotomize when necessary.

#-------------------------------------------------------------------------------
## Life Events Interview (9 yr)
# some of the tot 26 items are used mainly for lE domain but also for IR, DV, CR. 
LE_interview <- readquick("MOTHERTRAUMAINTERVIEW9_24112016.sav") # 5862 obs of 96 columns

# Exclude unreliable interviews
LE_interview <- subset(LE_interview, LE_interview$unreliable == 0) # 5570 obs of 96 columns

# Select the necessary item 
life_events <- 
  data.frame(LE_interview$idc, # child id 
             LE_interview$le1, # Did your child get seriously sick or did he/she have an accident? Yes
             LE_interview$le2, # Did a family member get seriously sick or a serious accident? Yes
             LE_interview$le3, # Did somebody else, who is important to your child, get seriously sick or have a serious accident? Yes
             LE_interview$le4, # Is the father/mother or other caretaker still alive? No 
             LE_interview$le5, # Did somebody else, who your child cared about a lot, die? Yes
             LE_interview$le6, # Did ever a pet, who you child cared about a lot, die? Yes
             LE_interview$le7, # Does or did your child have to deal with a high workload at school? Yes
             LE_interview$le8, # Did your child ever repeat a grade? Yes
             LE_interview$le9, # Are/were there problems in the neighborhood? (e.g. vandalism or insecurity); yes
             LE_interview$le10, # Does your family have or ever had financial difficulties? Yes 
             LE_interview$le11, # Has your child ongoing conflicts with a family member (or ever had them)? Yes
             LE_interview$le12, # Has your child ongoing conflict with somebody else (or ever had them)? Yes 
             LE_interview$le13, # Do other family members have ongoing conflicts with each other (or ever had them)? Yes
             LE_interview$le14, # Are you and your partner divorced or separated? Yes
             LE_interview$le16, # Did your child lose a good friend due to an argument? Yes
             LE_interview$le17, # Did your child ever lose something which was important to him/her? Yes
             LE_interview$le18, # Did someone ever use physical violence against your child? For example, beating him/her up. Yes
             LE_interview$le19, # Did somebody use almost physical violence against your child? So that it did not actually happen, but the child was frightened. Yes
             LE_interview$le20, # Did anybody make sexual comments or movements towards your child? Yes 
             LE_interview$le21, # Did your child participate in inappropriate sexual behavior? Yes
             LE_interview$le22, # Did somebody spread mean rumors or gossip about your child? Yes
             LE_interview$le23, # Did your child move? Yes 
             LE_interview$le24) # Did your child change schools? Yes
colnames(life_events) <- c("IDC","le1","le2","le3","le4","le5","le6","le7","le8",
                           "le9","le10","le11","le12","le13","le14","le16","le17",
                           "le18","le19","le20","le21","le22","le23","le24")

#-------------------------------------------------------------------------------
## Parenting behavior (3 yr)

# ATTENTION! The code assumes you have run the SPSS Syntax that calculates 80th percentile harsh parenting scores.
# (V:\Promovendi\Datasets\Gedragsgroep\Questionnaire data\After birth\0-3 years\Harsh parenting_Parent Child Conflict Tactics Scale\Syntax harsh parenting 3jr_PJ10012011)
# or most recent version.
parentingv1 <- readquick("New Parenting 3 years of age_GR1065 F1F6 -GR1066 B1-B5_22112016.sav") # 9897 obs of 130 columns

# NOTE: There are no official guidelines for dichotomization of the Parent Child Conflict Tactics scale. 
# We based dichotomization on previous work in Gen R (Jansen et al., 2017): i.e. 80th percentile split. 

# Construct GR1065 # used for DV domain.
parenting <- data.frame(parentingv1$idc, parentingv1$harsh80_p, parentingv1$harsh80_m)
colnames(parenting) <- c("IDC", "p_harsh_parent", "m_harsh_parent")

#-------------------------------------------------------------------------------
## GR1065-G2 (3 yrs)
GR1065v1 <- readquick("GR1065-G2_01072012.sav") # 9901 obs. of  28 

# Construct GR1065 # items used for LE (mainly), CR and IR.
GR1065 <- data.frame(GR1065v1$idc, 
                     GR1065v1$g0200365, # Did one of your child’s friends move to a new house? Yes
                     GR1065v1$g0201165, # Was there a fire or burglary? Yes 
                     GR1065v1$g0200765, # financial problems (with le10)
                     GR1065v1$g0202365, # unemployment (with GR075 and GR1081)
                     GR1065v1$g0200565, # Tension at the parents’ work that has been felt at home; yes
                     GR1065v1$g0201965) # Problems with marriage relations; yes
colnames(GR1065) <- c("IDC", "friend_moved", "fire_burglary", "fidi_3yrs", "empl_3yrs",
                      "tension_at_work", "marital_problems")

#-------------------------------------------------------------------------------
## GR1065-G3-6 (3 yrs)
GR1065v1G <- readquick("GR1065-G3-6_01072012.sav") # 9901 obs. of  9 

# Construct GR1065 # items used for CR and IR.
GR1065G <- data.frame(GR1065v1G$idc, 
                      ifelse(GR1065v1G$g0600165 > 1, yes = 1, no = 0), # difficulty paying rent/bills (with GR1081)
                             # DICH: ‘no problems at all’ = no risk, ‘few problems’ to ‘a lot problems’ = risk.
                      ifelse(GR1065v1G$g0300165 < 5, yes = 1, no = 0),  # household income
                             # DICH: according to the Central Statistic Netherlands (2013). 
                             # Net household income below 1600 €/month (basic needs level) = risk. 
                      ifelse(GR1065v1G$g0400265 > 3, yes = 1, no = 0)) # how many children in the household
                             # DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
                             # Family of more than three children = risk. 
colnames(GR1065G) <- c("IDC", "trouble_pay_3yrs","income_3yrs","fam_size_3yrs")

#-------------------------------------------------------------------------------
## GR1065X (3 yrs)
GR1065v1X <- readquick("GR1065-X_01072012.sav") # 9901 obs. of 5

# Construct GR1065 # item used for IR.
GR1065X <- data.frame(GR1065v1X$idc, 
                      ifelse(GR1065v1X$x0500165 > 1, yes = 1, no = 0)) # do you have a partner present?
                             # DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
                             # Having no partner or having a partner without living together = risk.
colnames(GR1065X) <- c("IDC", "marital_status_3yrs")

#-------------------------------------------------------------------------------
## GR1075 (3-6 yrs)
GR1075v1 <- readquick("SESFOLLOWUP_03022020.sav") # 9901 obs. of  10 variables # ATTENTION: this is different from the file Isabel used

GR1075 <- data.frame(GR1075v1$idc, 
                     ifelse(GR1075v1$educm3 <= 3, yes = 1, no = 0), # maternal education 3 yrs (for PR)
                     ifelse(GR1075v1$educm5 <= 3, yes = 1, no = 0), # maternal education 5 yrs (for PR)
                     ifelse(GR1075v1$educp3 <= 3, yes = 1, no = 0), # paternal education 3 yrs (for PR)
                     ifelse(GR1075v1$educp5 <= 3, yes = 1, no = 0), # paternal education 5 yrs (for PR)
                            # DICH: according to Statistics Netherlands (2016), 
                            # an educational level of secondary phase 1 or below = risk.
                     GR1075v1$mar_dich5, # Marital status: single (with GR1065 and le14 in IR)
                     ifelse(GR1075v1$income5 < 4, yes = 1, no = 0)) # household income (for CR)
                            # DICH: according to the Central Statistic Netherlands (2013). 
                            # Net household income below 1600 €/month (basic needs level) = risk.
colnames(GR1075) <- c("IDC", "m_educ_3yrs", "m_educ_5yrs", "p_educ_3yrs", "p_educ_5yrs", 
                      "marital_status_5yrs", "income_5yrs")

#-------------------------------------------------------------------------------
## GR1075B3 (5 yrs)
GR1075v1B3 <- readquick("GR1075-B3_17072015.sav") # 9901 obs. of  10 : family composition

# Construct GR1075 # used in IR
GR1075B3 <- data.frame(GR1075v1B3$idc, 
                       ifelse(GR1075v1B3$children_household_clean > 3, yes = 1, no = 0)) # Family size > 3 children
                              # DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
colnames(GR1075B3) <- c("IDC", "fam_size_5yrs")

#-------------------------------------------------------------------------------
## GR1075 - employment (5 yrs)
GR1075v1E <- readquick("PARENTEMPLOYMENT5_13082012.sav") # 9901 obs. of  7 

# Construct GR1075
GR1075E <- data.frame(GR1075v1E$idc, 
                      ifelse(GR1075v1E$b0500175_clean <= 3, yes = 0, no = 1), # mother occupation
                      ifelse(GR1075v1E$b1200175_clean<= 3, yes = 0, no = 1)) # father occupation
                             # DICH: unemployed if described themselves other than ‘paid worker’ 
                             # or ‘self-employed’ (e.g. 'student')
colnames(GR1075E) <- c("IDC", "m_empl_5yrs", "p_empl_5yrs")

#-------------------------------------------------------------------------------
## GR1079 (5-8 yrs) - bullying assessed by teacher
GR1079v1 <- readquick("CHILDTRF_27102014/20141027_TRFteacherCleaned.sav") # 7580 obs. of  336
# ATTENTION!! OS sensitive: if you are using a mac, change \ to / in the path.

# Construct GR1079 # used in DV score 
GR1079 <- data.frame(GR1079v1$idc, 
                     ifelse(GR1079v1$d0100179 >= 4, yes = 1, no = 0), # physical bulying (hit,kick,pinch,bite) according to the teacher
                     ifelse(GR1079v1$d0100279 >= 4, yes = 1, no = 0), # verbal bullying (laugh at, insult, tease) according to the teacher
                     ifelse(GR1079v1$d0100379 >= 4, yes = 1, no = 0)) # excluded, according to the teacher
                            # DICH: based on previous work on bullying in Gen R (Muetzel et al., 2019).
                            # If child was verbally/physically/relationally bullied once a week or more = risk. 
colnames(GR1079) <- c("IDC", "bully_physical_t", "bully_verbal_t", "bully_excluded_t")
#-------------------------------------------------------------------------------
## GR1080E (8 yrs) - bullying assessed by main caregiver
GR1080v1E <- readquick("GR1080-E_Bullying_17072015.sav") # 9901 obs. of  11 

# Construct GR1080 # used in DV score      # CHECK THE DATA, MAX IS 88, WHA?
GR1080E <- data.frame(GR1080v1E$idc, 
                      ifelse(GR1080v1E$e0100280 >= 4, yes = 1, no = 0), # how often your bullied (insulted, calling names or laughed at)
                      ifelse(GR1080v1E$e0100380 >= 4, yes = 1, no = 0), # how often your bullied (spitting, hitting, kicking or pinching)
                      ifelse(GR1080v1E$e0100480 >= 4, yes = 1, no = 0)) # how often your bullied (excluded, ignored or gossiped about)
                             # DICH: based on previous work on bullying in Gen R (Muetzel et al., 2019).
                             # If child was verbally/physically/relationally bullied once a week or more = risk. 
colnames(GR1080E) <- c("IDC", "bully_physical_m", "bully_verbal_m", "bully_excluded_m")

#-------------------------------------------------------------------------------
## GR1080 (8 yrs)
GR1080v1C <- readquick("GR1080-C10-11_04042017.sav") # 9901 obs. of  23 (academic performance)

# Construct GR1080 # used in LE
GR1080C <- data.frame(GR1080v1C$idc,
                      GR1080v1C$cleaned_c1001280v2) # has your child repeated any grades?
colnames(GR1080C) <- c("IDC", "rep_grade_8yrs")
#-------------------------------------------------------------------------------
## GR1082 (10 yrs)
GR1082v1 <- readquick("GR1082_C1-5_22092016.sav") # 9901 obs. of  16 (academic performance)

# Construct GR1082 # used in LE
GR1082 <- data.frame(GR1082v1$idc, 
                     GR1082v1$c0300182_cleaned) # has your child repeated any grades?
colnames(GR1082) <- c("IDC", "rep_grade_10yrs")

#-------------------------------------------------------------------------------
## GR1081 E3-6 (10 yrs)
GR1081v1 <- readquick("GR1081_E1-3_5-6_08082016.sav") # 9901 obs. of  35 

# Construct GR1081 # used in CR (mainly) and in IR (family size)
GR1081 <- data.frame(GR1081v1$idc, 
                     ifelse(GR1081v1$e0600181_v2 > 1, yes = 1, no = 0), # trouble paying for food, rent, electricity
                            # DICH: ‘no problems at all’ = no risk, ‘few problems’ to ‘a lot problems’ = risk.
                     GR1081v1$e0100281_v2, # sufficient heating in house during cold weather # INVERSE
                     GR1081v1$e0100381_v2, # pay rent of mortgage without problems # INVERSE
                     GR1081v1$e0100481_v2, # on average one hot meal per day # INVERSE
                     GR1081v1$e0100581_v2, # own or lease a car # INVERSE
                     GR1081v1$e0100681_v2, # own a washing machine # INVERSE
                     GR1081v1$e0100781_v2, # own a refrigerator # INVERSE
                     GR1081v1$e0100881_v2, # own a telephone # INVERSE
                     GR1081v1$e0101181_v2, # holidays away from home 1 or more weeks per year # INVERSE
                          # the above items are taken from the Material deprivation questionnaire.
                     ifelse(GR1081v1$e0200181_v2 < 4, yes = 1, no = 0), # household income per month
                            # DICH: according to the Central Statistic Netherlands (2013). 
                            # Net household income below 1600 €/month (basic needs level) = risk.
                     ifelse(GR1081v1$e0200381_v2 > 3, yes = 1, no = 0)) # how many children live with this income.
                            # DICH: based on Cecil et al. (2014); Rijlaarsdam et al, (2016).
                            # Family of more than three children = risk. 
colnames(GR1081) <- c("IDC", "trouble_pay_9yrs", "mat_depr_heating", "mat_depr_rent", 
                      "mat_depr_meal", "mat_depr_car", "mat_depr_washingmachine", 
                      "mat_depr_refrigerator", "mat_depr_telephone", "mat_depr_holidays",
                      "income_9yrs","fam_size_9yrs")
#-------------------------------------------------------------------------------
## GR1081 - employment (10 yrs)
GR1081v1E4 <- readquick("GR1081_E4_30082016.sav")

# Construct GR1081E
GR1081E4 <- data.frame(GR1081v1E4$idc, 
                       ifelse(GR1081v1E4$e0400181_v2 > 1, yes = 1, no = 0)) # do you receive any benefits?
                              # any kind of benefit (social, unemployment or disability allowances) = risk.
colnames(GR1081E4) <- c("IDC", "empl_9yrs")
#-------------------------------------------------------------------------------
## GR1081 I - TV (10 yrs)
GR1081v1I <- readquick("GR1081_I1-9_08082016.sav") # 9901 obs. of  29

# Construct GR1081 I # used in CR
GR1081I <- data.frame(GR1081v1I$idc, 
                      ifelse(GR1081v1I$i0100181_cleaned > 1, yes = 1, no = 0)) # how many televisions in the house?
                      # DICH: 0 = no television, 1 = at least one television.
                      # This item used in the material deprivation index, together with variables from the 
                      # Material deprivation questionnaire, to increase resemblance with EU-SILC guidelines.
colnames(GR1081I) <- c("IDC", "mat_depr_tv")

#-------------------------------------------------------------------------------
# Brief Symptoms Inventory (3 and 9 yrs)
BSI3 <- readquick("BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav")
BSI9 <- readquick("GR1081-GR1083_D1_BSI_19042017.sav")
BSI <- merge(BSI3, BSI9, by = 'idc', all = T)

# Psychopathology includes three syndrome domains: interpersonal sensitivity, 
# depression and anxiety, measured in main caregivers and partners at the ages 3 and 9.
# DICH: according to the Dutch BSI manual (De Beurs, 2009). Cutoffs values denote the 
# transition from health to sickness and are gender and scale specific. 

# INTERPERSONAL SENSITIVITY: Items 20, 21, 22, and 42
#                             # mother
m_is_3yrs = bsi_scores(items = c('g0101065', 'g0101165', 'g0101265', 'g0101765'), 
                       filledinby = BSI$gr1065_filledinby,
                       cutoff = c(.95, .78, .78, 1.12, 999))
m_is_9yrs = bsi_scores(items = c('d0101081_cleaned', 'd0101181_cleaned', 'd0101281_cleaned', 'd0101781_cleaned'),
                       filledinby = BSI$gr1081_filledinby,
                       cutoff = c(.95, .95, .78, .78, 1.12))
#                            # father
f_is_3yrs = bsi_scores(items = c('c0101066', 'c0101166', 'c0101266', 'c0101766'),
                       filledinby = BSI$gr1066_filledinby,
                       cutoff = c(.78, .78, .95, 1.12, 999))
f_is_9yrs = bsi_scores(items = c('d0101083_cleaned', 'd0101183_cleaned', 'd0101283_cleaned', 'd0101783_cleaned'),
                       filledinby = BSI$gr1083_filledinby,
                       cutoff = c(.78, .78, .95, .95, 1.12))
# DEPRESSION: Items 9, 16, 17, 18, 35, and 50
#                           # mother
m_dep_3yrs = bsi_scores(items = c('g0100365', 'g0100665', 'g0100765', 'g0100865', 'g0101365', 'g0102165'),
                       filledinby = BSI$gr1065_filledinby,
                       cutoff = c(.80, .71, .71, .75, 999))
m_dep_9yrs = bsi_scores(items = c('d0100381_cleaned', 'd0100681_cleaned', 'd0100781_cleaned', 'd0100881_cleaned', 'd0101381_cleaned', 'd0102181_cleaned'),
                       filledinby = BSI$gr1081_filledinby,
                       cutoff = c(.80, .80, .71, .71, .75))
#                           # father
f_dep_3yrs = bsi_scores(items = c('c0100366', 'c0100666', 'c0100766', 'c0100866', 'c0101366', 'c0102166'),
                       filledinby = BSI$gr1066_filledinby,
                       cutoff = c(.71, .71, .80, .75, 999))
f_dep_9yrs = bsi_scores(items = c('d0100383_cleaned', 'd0100683_cleaned', 'd0100783_cleaned', 'd0100883_cleaned', 'd0101383_cleaned', 'd0102183_cleaned'),
                       filledinby = BSI$gr1083_filledinby,
                       cutoff = c(.71, .71, .80, .80, .75))
# ANXIETY: Items 1, 12, 19, 38, 45, and 49 
#                           # mother
m_anx_3yrs = bsi_scores(items = c('g0100165', 'g0100465', 'g0100965', 'g0101465', 'g0101865', 'g0102065'),
                        filledinby = BSI$gr1065_filledinby,
                        cutoff = c(.71, .65, .65, .75, 999))
m_anx_9yrs = bsi_scores(items = c('d0100181_cleaned', 'd0100481_cleaned', 'd0100981_cleaned', 'd0101481_cleaned', 'd0101881_cleaned', 'd0102081_cleaned'),
                        filledinby = BSI$gr1081_filledinby,
                        cutoff = c(.71, .71, .65, .65, .75))
#                           # father
f_anx_3yrs = bsi_scores(items = c('c0100166', 'c0100466', 'c0100966', 'c0101466', 'c0101866', 'c0102066'),
                        filledinby = BSI$gr1066_filledinby,
                        cutoff = c(.71, .71, .80, .75, 999))
f_anx_9yrs = bsi_scores(items = c('d0100183_cleaned', 'd0100483_cleaned', 'd0100983_cleaned', 'd0101483_cleaned', 'd0101883_cleaned', 'd0102083_cleaned'),
                        filledinby = BSI$gr1083_filledinby,
                        cutoff = c(.71, .71, .80, .80, .75))

# Construct the set to add to the dataset
BSI_dich <- data.frame(BSI$idc, BSI$idm, BSI$age_gr1065,
                       m_is_3yrs, m_is_9yrs, f_is_3yrs, f_is_9yrs, 
                       m_dep_3yrs, m_dep_9yrs, f_dep_3yrs, f_dep_9yrs,
                       m_anx_3yrs, m_anx_9yrs, f_anx_3yrs, f_anx_9yrs)
                       # BSI$gr1065_filledinby, BSI$gr1066_filledinby, BSI$gr1081_filledinby, BSI$gr1083_filledinby # Isabel also included these, but I don't think they are used
colnames(BSI_dich) <- c("IDC", "IDM", "BSI_age", "m_is_3yrs", "m_is_9yrs", "f_is_3yrs", "f_is_9yrs", 
                        "m_dep_3yrs", "m_dep_9yrs", "f_dep_3yrs", "f_dep_9yrs",
                        "m_anx_3yrs", "m_anx_9yrs", "f_anx_3yrs", "f_anx_9yrs")
                        
#-------------------------------------------------------------------------------
# Family Assessment Device 
FAD5 <- readquick("GR1075-B4_17072015.sav")
FAD9m <- readquick("GR1081_E1-3_5-6_08082016.sav")
FAD9f <- readquick("GR1083_E7_19102016.sav")

m_fad_9yrs <- data.frame(5 - FAD9m$e0500181_v2, # Recode inverse item
                         5 - FAD9m$e0500381_v2, # Recode inverse item
                         5 - FAD9m$e0500581_v2, # Recode inverse item
                         5 - FAD9m$e0500781_v2, # Recode inverse item
                         5 - FAD9m$e0500981_v2, # Recode inverse item
                         5 - FAD9m$e0501181_v2, # Recode inverse item
                         FAD9m$e0500281_v2, 
                         FAD9m$e0500481_v2, 
                         FAD9m$e0500681_v2, 
                         FAD9m$e0500881_v2, 
                         FAD9m$e0501081_v2, 
                         FAD9m$e0501281_v2)

p_fad_9yrs <- data.frame(5 - FAD9f$e0700183v2, # Recode inverse item
                         5 - FAD9f$e0700383v2, # Recode inverse item
                         5 - FAD9f$e0700583v2, # Recode inverse item
                         5 - FAD9f$e0700783v2, # Recode inverse item
                         5 - FAD9f$e0700983v2, # Recode inverse item
                         5 - FAD9f$e0701183v2, # Recode inverse item
                         FAD9f$e0700283v2, 
                         FAD9f$e0700483v2, 
                         FAD9f$e0700683v2, 
                         FAD9f$e0700883v2, 
                         FAD9f$e0701083v2, 
                         FAD9f$e0701283v2) 
# The total score at age 5 is already dichotomous, age 9 need to be dichotomized manually.
m_fad_9y = fad_scores(m_fad_9yrs)
p_fad_9y = fad_scores(p_fad_9yrs)

FAD_dich <- data.frame(FAD5$idc, FAD5$fad_5y, m_fad_9y, p_fad_9y)
colnames(FAD_dich) <- c("IDC","m_fad_5yrs","m_fad_9yrs","p_fad_9yrs")

#-------------------------------------------------------------------------------
# This function merges together all separate data.frames according to the IDC
# results in a dataframe with all relevant items for postnatal stress.
# tech-tip: because merge can only take two data.frames at the time and I am lazy, I used Reduce.
postnatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC',  all = TRUE),
                           list(BSI_dich, FAD_dich, life_events, parenting, GR1065, 
                                GR1065G, GR1065X, GR1075, GR1075B3, GR1075E, GR1079, 
                                GR1080E, GR1080C, GR1082, GR1081, GR1081E4, GR1081I)) 
#-------------------------------------------------------------------------------
fetal_general <- readquick("FETALPERIOD-ALLGENERALDATA_29012018.sav") # 9778 obs of 95 var

early_parent <- data.frame(fetal_general$idm, 
                           ifelse(fetal_general$age_m_v2 < 19, yes = 1, no = 0), # mother younger than 19 years (for PR)
                           # DICH: based on Cecil et al. (2014); Rijlaarsdam et al. (2016).
                           ifelse(fetal_general$age_p_v2 < 19, yes = 1, no = 0), # partner younger than 19 years (for PR)
                           ifelse(fetal_general$age_bf_v2 < 19, yes = 1, no = 0)) # biological father younger than 19 years (for PR). 
                           # In many cases only one of the last two was filled, but cases 
                           # which had both variables available will be combined later.
colnames(early_parent) <- c("IDM","m_age","partner_age","biofather_age")

postnatal_stress <- merge(postnatal_stress, early_parent, by = 'IDM', all = T)

################################################################################
#### -------------------- Combine repeated measures  ---------------------- ####
################################################################################

# Repeating a grade was measured by maternal report at age 8 (GR1080), 9 (le8) 
# and 10 (GR1082). # Once a risk, always a risk strategy:
postnatal_stress$rep_grade <- repmeas(postnatal_stress[,c('rep_grade_8yrs','le8','rep_grade_10yrs')])

# Financial difficulties were measured at age 3 (GR1065) and 9 (le10). 
# Once a risk, always a risk strategy:
postnatal_stress$financial_difficulties <- repmeas(postnatal_stress[,c('fidi_3yrs', 'le10')]) 

# Trouble paying for food was measured at age 3 (GR1065) and 9 (GR1081). 
# Once a risk, always a risk strategy:
postnatal_stress$trouble_pay <- repmeas(postnatal_stress[,c('trouble_pay_3yrs','trouble_pay_9yrs')]) 

# Income was assessed at age 3 (GR1065), 5 (GR1075) and 9 (GR1081).
# Two items: once-always + chronic risk strategy.
postnatal_stress$income_once <- repmeas(postnatal_stress[,c('income_3yrs', 'income_5yrs', 'income_9yrs')])
postnatal_stress$income_chronic <- repmeas(postnatal_stress[,c('income_3yrs', 'income_5yrs', 'income_9yrs')], 
                                           strategy = 'chronic')

# Unemployment was measured at age 3 (GR1065), 5 (GR1075, by mother and partner)
# and 9 (GR1081). # Two items: once-always + chronic risk strategy.
postnatal_stress$unemployed_once <- repmeas(postnatal_stress[,c('empl_3yrs','m_empl_5yrs', 'p_empl_5yrs', 'empl_9yrs')]) 
postnatal_stress$unemployed_chronic <- repmeas(postnatal_stress[,c('empl_3yrs','m_empl_5yrs', 'p_empl_5yrs', 'empl_9yrs')],
                                               strategy = 'chronic')

# Material deprivation measure is based on Statistics Netherlands and EU-SILC,
# (e.g. family possessed a car or a fridge, yes/no). To increase resemblance with EU-SILC, 
# we added an item about how many televisions the family possessed. 
# Combine material deprivation + television items. 
made <- postnatal_stress[, c("mat_depr_heating", "mat_depr_rent", "mat_depr_meal", "mat_depr_car", 
                             "mat_depr_washingmachine", "mat_depr_refrigerator", "mat_depr_telephone", 
                             "mat_depr_holidays","mat_depr_tv")]
# material_deprivation reflects the family’s ability to afford basic needs and services.
# If ‘possession rate’ is lower than 75%, risk is present. Rate is based on EU-SILC (2009).
postnatal_stress$material_deprivation <- ifelse((rowMeans(is.na(made)) < .25),
                                                yes = ifelse(rowMeans(made, na.rm = T) <= .75, yes = 1, no = 0), no = NA) 
# NOTE: we set material deprivation values to NA if the missing frequency within the questionnaire is >25%

# Education level of main caregiver and partner was assessed at age 3 and 5 (GR1075).
# Education level of each parent is included separately in the domain score. 
# Once a risk, always a risk strategy:
postnatal_stress$m_education <- repmeas(postnatal_stress[,c("m_educ_3yrs", "m_educ_5yrs")])
postnatal_stress$p_education <- repmeas(postnatal_stress[,c("p_educ_3yrs", "p_educ_5yrs")])

# Early parenthood. Age of father (partner or biological father) was assessed during intake. 
# In many cases only one of these was filled, but for cases which had both variables 
# available (and different values), we apply the once-always strategy.
postnatal_stress$p_age <- repmeas(postnatal_stress[,c("partner_age","biofather_age")])

# Psychopathology included three syndrome domains: interpersonal sensitivity, 
# depression and anxiety, measured in main caregivers and partners at the ages 3 and 9.
postnatal_stress$m_interpersonal_sensitivity <- repmeas(postnatal_stress[,c("m_is_3yrs", "m_is_9yrs")])
postnatal_stress$p_interpersonal_sensitivity <- repmeas(postnatal_stress[,c("f_is_3yrs", "f_is_9yrs")])
postnatal_stress$m_depression <- repmeas(postnatal_stress[,c("m_dep_3yrs", "m_dep_9yrs")])
postnatal_stress$p_depression <- repmeas(postnatal_stress[,c("f_dep_3yrs", "f_dep_9yrs")])
postnatal_stress$m_anxiety <- repmeas(postnatal_stress[,c("m_anx_3yrs", "m_anx_9yrs")])
postnatal_stress$p_anxiety <- repmeas(postnatal_stress[,c("f_anx_3yrs", "f_anx_9yrs")])

# Marital status was measured at age 3 (GR1065X), 5 (GR1075) and 9 (le14).
# Once a risk, always a risk strategy:
postnatal_stress$marital_status <- repmeas(postnatal_stress[,c('marital_status_3yrs','marital_status_5yrs','le14')])

# Family size was measured at age 3 (GR1065G), 5 (GR1075B) and 9 (GR1081)
# Once a risk, always a risk strategy:
postnatal_stress$family_size <- repmeas(postnatal_stress[,c('fam_size_3yrs', 'fam_size_5yrs', 'fam_size_9yrs')])

# Bullying was assessed by main caregiver and teacher. 
# Once a risk, always a risk strategy: if child was verbally/physically/relationally 
# bullied once a week or more, as reported by the main caregiver or the teacher = risk. 
postnatal_stress$bullying <- repmeas(postnatal_stress[,c('bully_physical_m','bully_verbal_m','bully_excluded_m', 
                                                         'bully_physical_t','bully_verbal_t','bully_excluded_t')])


#-------------------------------------------------------------------------------
# Let's have a look at risk distribution and missing data per indicator.

postnatal_summary = data.frame(row.names=c("no risk","risk","NA"))
for (i in 4:ncol(postnatal_stress)) { # because the third column is not dichotomous (BSI_age)
  s = summary(as.factor(postnatal_stress[,i]))
  c = colnames(postnatal_stress)[i]
  postnatal_summary[,c] <- s }

################################################################################
#### --------------- create the (weighted) domain scores ------------------ ####
################################################################################

# For the specific items and time of measurement, consult the excel file. 
# The scores are cumulative: how many risk factors are present for each person. 
# We work with a *mean score* of items reported (hence domain score range = 0 to 1). 
# If preferred the code allows for working with the actual number as well.

# LE
postnatal_stress[,c('post_life_events')] <- domainscore(postnatal_stress[,c(
  'le1', # Did your child get seriously sick or did he/she have an accident? Yes
  'le2', # Did a family member get seriously sick or a serious accident? Yes
  'le3', # Did somebody else, who is important to your child, get seriously sick or have a serious accident? Yes
  'le4', # Is the father/mother or other caretaker still alive? No 
  'le5', # Did somebody else, who your child cared about a lot, die? Yes
  'le6', # Did ever a pet, who you child cared about a lot, die? Yes
  'le7', # Does or did your child have to deal with a high workload at school? Yes
  'rep_grade', # Did your child ever repeat a grade? Yes
  'le17', # Did your child ever lose something which was important to him/her? Yes
  'le23', # Did your child move? Yes 
  'le24', # Did your child change schools? Yes
  'friend_moved', # Did one of your child’s friends move to a new house? Yes
  'fire_burglary' # Was there a fire or burglary? Yes 
  )])

# CR
postnatal_stress[,c('post_contextual_risk')] <- domainscore(postnatal_stress[,c(
  'tension_at_work', # Tension at the parents’ work that has been felt at home; yes
  'material_deprivation', # Material deprivation; yes
  'financial_difficulties', # Does your family have or ever had financial difficulties? Yes 
  'le9', # Are/were there problems in the neighborhood? (e.g. vandalism or insecurity); yes
  'trouble_pay', # Trouble paying for your food, rent, electricity bill and such in the past year? Yes
  'income_once', # Income household; < 1600 euros p/m; once 
  'income_chronic', # Income household; < 1600 euros p/m; chronically
  'unemployed_once', # Unemployment within the family, once
  'unemployed_chronic' # Unemployment within the family, chronically
  )])

# PR
postnatal_stress[,c('post_parental_risk')] <- domainscore(postnatal_stress[,c(
  'm_education', # Education main caregiver; < phase 2 (higher) secondary education, once 
  'p_education', # Education partner; < phase 2 (higher) secondary education , once
  'm_interpersonal_sensitivity', # Interpersonal sensitivity main caregiver; > 0.95
  'p_interpersonal_sensitivity', # Interpersonal sensitivity partner; > 0.78
  'm_depression', # Depression main caregiver; > 0.80
  'p_depression', # Depression partner; > 0.71
  'm_anxiety', # Anxiety main caregiver; > 0.71
  'm_anxiety', # Anxiety partner; > 0.65
  'm_age', # Early parenthood; age mother <19 yrs
  'p_age' # Early parenthood; age partner <19 yrs
  )])

# IR
postnatal_stress[,c('post_interpersonal_risk')] <- domainscore(postnatal_stress[,c(
  'marital_problems', # Problems with marriage relations; yes
  'marital_status', # Marital status; single
  'family_size', # Family size; > 3 children
  'm_fad_5yrs', # Family distress according to main caregiver; > 2.17
  'm_fad_9yrs', # Family distress according to main caregiver; > 2.17
  'p_fad_9yrs', # Family distress according to partner; > 2.17
  'le11', # Has your child ongoing conflicts with a family member (or ever had them)? Yes
  'le12', # Has your child ongoing conflict with somebody else (or ever had them)? Yes 
  'le13', # Do other family members have ongoing conflicts with each other (or ever had them)? Yes
  'le14', # Are you and your partner divorced or separated? Yes
  'le16' # Did your child lose a good friend due to an argument? Yes
)])

# DV
postnatal_stress[,c('post_direct_victimization')] <- domainscore(postnatal_stress[,c(
  'm_harsh_parent', # Harsh parenting main caregiver > 80th percentile
  'p_harsh_parent', # Harsh parenting partner > 80th percentile
  'bullying', # Bullying more than once a week according to teacher or main caregiver; yes
  'le18', # Did someone ever use physical violence against your child? For example, beating him/her up. Yes
  'le19', # Did somebody use almost physical violence against your child? So that it did not actually happen, but the child was frightened. Yes
  'le20', # Did anybody make sexual comments or movements towards your child? Yes 
  'le21', # Did your child participate in inappropriate sexual behavior? Yes
  'le22' # Did somebody spread mean rumors or gossip about your child? Yes
  )])


################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where the raw data are stored
saveRDS(postnatal_stress, paste(dir,'postnatal_stress.rds'))
#saveRDS(postnatal_summary, paste(dir,'postnatal_stress_summary.rds'))

