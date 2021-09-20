
# Load the necessary libraries
library(foreign)
library(tidyverse)

# Chose the folder where the input file is stored and all results will be saved
if (exists("alspac_file") == F) { 
  alspac_file <- file.choose() 
  alspac_folder <- dirname(alspac_file)
  # Read in the data
  alspac.table <- foreign::read.spss(alspac_file, use.value.label=TRUE, to.data.frame=TRUE) }

################################################################################
################################################################################

# Specify some functions:
# These functions are adapted from SereDef/cumulative-ELS-score '0-Setup_and_functions.R'

#-------------------------------------------------------------------------------
# DICHOTOMIZE
#-------------------------------------------------------------------------------
# Most variable in alspac.table are factors with levels corresponding to a 5-level
# Likert from did not happen to affected a lot + other noise levels (e.g. other or 
# do not know). Here we define a function that dichotomizes these factors into 
# 1 = risk present, 0 = no risk present. The function takes a list of variable names
# and a list of values corresponding to risk (i.e. "yes") and no risk (i.e., "no"). 
# It performs the recoding for the input variables, renaming them by adding suffix 
# "a_rec" and sets everything else (NA as well as "noise" factor levels) to NA. 
# It outputs a dataframe containing the original and the recoded variables, and it 
# also stacks some additional variables to it if specified in the already_dich argument. 
# By setting check_transf = T when calling the function, you can check whether the 
# recoding worked as expected. 

dichotomize <- function(vars, 
                        yes = c("affected a lot","fairly affected","mildly affected","N effect at all"), 
                        no = c("didnt happen"), 
                        already_dich = c(), 
                        check_transf = F) {
  
  dset <- as.data.frame(alspac.table[, vars])  # call columns from alspac.table
  names(dset) <- vars  # rename them using the original variable names 
  answ = c(yes, no) # 
  
  # Check if required levels are present / unexpected levels are present
  for (i in vars) {
    lvls = levels(dset[,i])
    
    if (length(setdiff(lvls, answ)) > 0) {   # unexpected levels are present
      message("Column ", i, " has these unexpected values: ", setdiff(lvls, answ), 
              ". These will be recoded as NA.")
      for (h in setdiff(lvls, answ)) {
        lvls[lvls == h] <- NA } # recode noise values as NA
      
      if (length(setdiff(answ, lvls)) > 0) {  # expected levels are not present
        message("Column ", i, " has no registered '", setdiff(answ, lvls), "' answers.") }
      
    } else { final <- "All variables behave as expected, mazel tov :)" }
  }
  if (exists("final")) { cat(final) }
  
  # Recoding
  for(i in vars) {
    # some var names already have an "a" at the end, in which case we add "_rec" else "a_rec"
    if (substr(i, nchar(i), nchar(i)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(i, appendix) # create new (recoded) variable name
    
    # perform the recoding
    dset[,var.out] <- ifelse(dset[,i] %in% no, 0, ifelse(dset[,i] %in% yes, 1, NA))
    
    # Check continuous vs. binary columns 
    if (check_transf == T) { 
      mat <- table(dset[,i], dset[,var.out])
      message("\n", i); print(mat) }
  }
  
  # Add variables that were already dichotomous to the output dataframe (if any)
  if (length(already_dich) > 0) {
    # First loop through them and check if they are actually dichotomous
    for (v in already_dich) {
      if (length(levels(alspac.table[, v])) > 2) { 
        message(" But are you sure variable ", v, " is already dichotomous?") }
    }
    # Add them to the dataset
    dset <- cbind(dset, alspac.table[, already_dich]) 
    # When only one variable is in already_dich, we need to rename the column manually
    if (length(already_dich) == 1) { names(dset)[ncol(dset)] <- already_dich }
  }
  
  return(dset)
}

#-------------------------------------------------------------------------------
# CCEI SCORE calculation
#-------------------------------------------------------------------------------
# The anxiety scale total score needs to be computed for some timepoints. The function 
# takes 3 sets of CCEI items (i.e. variable names) that are assigned different scores 
# depending on the answer given. The original variables are factors with levels: 
# "Very often", "Often", "Not very often", "Never". After recoding, the item scores 
# are summed and a dataset including the original input items, their recoded versions 
# ("a_rec") and the sum score is returned.
# By default, check_transf = T so you can check whether the recoding worked as expected. 

ccei_score <- function(set1, 
                       set2, 
                       set3, 
                       check_transf = T) {
  
  vars <- c(set1, set2, set3)
  dset <- alspac.table[, vars] # select relevant columns from alspac.table
  
  for (v in set1) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix) # create new (recoded) variable name
    # perform the recoding
    dset[,var.out] <- ifelse(dset[, v] %in% c("Very often" , "Often"), 2, 
                             ifelse(dset[, v] %in% c("Not very often", "Never"), 0, NA)) 
    if (check_transf == T) { 
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  for (v in set2) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix) # create new (recoded) variable name
    # perform the recoding
    dset[,var.out] <- ifelse(dset[, v] %in% c("Very often" , "Often"), 2,
                             ifelse(dset[, v] == "Not very often", 1,
                                    ifelse(dset[, v] == "Never", 0, NA)))
    if (check_transf == T) {
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  for (v in set3) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix) # create new (recoded) variable name
    # perform the recoding
    dset[,var.out] <- ifelse(dset[, v] %in% c("Very often" , "Often", "Not very often"), 2,
                             ifelse(dset[, v] == "Never", 0, NA))
    if (check_transf == T) {
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  
  dset[, "sumscore"] <- rowSums(dset[,grep("a_rec",names(dset), value=T)], na.rm = F)
  
  return(dset)
}

#-------------------------------------------------------------------------------
# EPDS SCORE calculation
#-------------------------------------------------------------------------------
# The depression total score needs to be computed for some timepoints. The function 
# takes 2 sets of EPDS items (i.e. variable names) that are either direct or inverted items. 
# The original variables are factors with levels: 1, 2, 3 and 4. After recoding, 
# the item scores are summed and a dataset including the original input items, their 
# recoded versions ("a_rec") and the sum score is returned.
# By default, check_transf = T so you can check whether the recoding worked as expected. 

epds_score <- function(set, 
                       revset,
                       check_transf = T) {
  
  vars <- c(set, revset)
  # recode variables as integer, so that labels would become numerical values
  alspac.table[, vars] <- sapply(alspac.table[, vars], as.integer) 
  dset <- alspac.table[, vars] # extract transformed columns from alspac.table
  
  for (v in vars) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix) # create new (recoded) variable name
    # perform the recoding
    if (v %in% set) { tr = min(as.integer(dset[, v]), na.rm = T) 
    } else if (v %in% revset) { tr = max(as.integer(dset[, v]), na.rm = T) }
    
    dset[,var.out] <- abs(dset[, v] - tr) 
    
    if (check_transf == T) { 
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  
  dset[, "sumscore"] <- rowSums(dset[,grep("a_rec",names(dset), value=T)], na.rm = F)
  
  return(dset)
}

#-------------------------------------------------------------------------------
# DICHOTOMIZE CCEI and EPDS total scores
#-------------------------------------------------------------------------------
# This function facilitates the dichotomization of psychopathology variables (i.e.
# total scores from CCEI and EPDS) according the a specified cut off value. 
# The function takes the variable name corresponding to a total score that is expected 
# to be coded as a factor with either numerical levels or some a string. After recoding, 
# a dataset including the original input variable and its recoded version ("a_rec") 
# is returned.
# By default, check_transf = T so you can check whether the recoding worked as expected. 

dich_psychopath <- function(var, 
                            yes, no, 
                            yes_label = "", no_label = "", 
                            check_transf = T) {
  
  dset <- data.frame(as.character(alspac.table[, var])) # extract transformed columns from alspac.table
  names(dset) <- var # assign the appropriate variable name
  
  if (substr(var, nchar(var), nchar(var)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
  var.out = paste0(var, appendix)  # create new (recoded) variable name
  
  # perform the recoding
  dset[,var.out] <- ifelse(dset[,var] %in% yes | dset[,var] == yes_label , 1,
                           ifelse(dset[,var] %in% no | dset[,var] == no_label, 0, NA))
  
  # Checking continuous and binary columns 
  if (check_transf == T) { 
    mat <- table(dset[,var], dset[,var.out])
    message(var); print(mat) }
  
  return(dset)
}

#-------------------------------------------------------------------------------
# REPMEAS (COLLAPSE ITEMS ACROSS TYPES OR TIME)
#-------------------------------------------------------------------------------
# In several cases, we need to combine similar measurements into a single risk factor.
# We do so by reusing the GenR function repmeas() in its default strategy (i.e. 
# 'oncealways') that first checks if the measures to combine are all binary and 
# then assigns a 1 to the output vector if at least one of the input variables 
# is = to 1 and a 0 if all the input variables are = to 0. 
# By default, check_corrs = T so you can check the correlations between collapsed items.

repmeas <- function(items, 
                    check_corrs = T) {
  
  x <- data.frame(items)
  # making sure all repeated measures are binary variables
  for (i in 1:ncol(x)){
    if (range(x[,i], na.rm = T)[1] == 1 & range(x[,i], na.rm = T)[2] == 2){  x[,i] <- x[,i] - 1 
    } else {
      if ( (range(x[,i], na.rm = T)[1] != 0 | range(x[,i], na.rm = T)[2] != 1) & ( range(x[,i], na.rm = T)[1] != range(x[,i], na.rm = T)[2] ) ) { 
        stop('Items are not dichotomized') }
    } 
  }
  
  if (check_corrs == T) {
    mat <- cor(x, method = "spearman", use = "complete.obs") 
    print(mat)
  }
  
  # combine and dichotomize the repeated measures
  temp <- rowMeans(x, na.rm = T) 
  temp[temp > 0] <- 1 
  
  return(temp) 
}

#-------------------------------------------------------------------------------
# PERCENT_MISSING
#-------------------------------------------------------------------------------
# Calculate the percentage of missing data (will come in handy in the next function)
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }

#-------------------------------------------------------------------------------
# DOMAINSCORE
#-------------------------------------------------------------------------------
# Domain scores measure how many adversities are reported. This function returns 
# the domain risk score and the percentage of missing values within the domain. 
# Default score_type is the mean nr of adversities (ranging from 0 to 1), that is 
# calculated whenever a domain is at least 75% complete, otherwise the domain score 
# is NA. This missing value needs to be accounted for by multiple imputation. 
# If postnatal argument is set to T, the missingness cut off is computed taking into 
# account the repeated measurements of each risk item.
# By default, check_corrs = T so you can check the correlations between timepoints.
# The function can be adapted to provide a cumulative number of adversities (set 
# score_type = 'sum_simple') or a weighted sum score like the one used in 
# Rijlaarsdam et al. (2016) and Cecil et al. (2014) (set score_type = 'sum_weighted') 
# that both also allow for 25% missing.

domainscore <- function(df, save_cor_mat = "",
                        score_type = 'mean_simple', 
                        postnatal = F, 
                        check_corrs = T) {
  
  # ALSPAC POSTANTAL SCORE
  if (postnatal == T) { 
    item_scores <- as.data.frame(rep(NA, nrow(postnatal_stress))) # initiate a dataframe
    
    for (item in df) {
      items <- as.data.frame(postnatal_stress[, grepl(item , names(postnatal_stress))]) # get all timepoints
      colnames(items) <- names(postnatal_stress)[grepl(item , names(postnatal_stress))] # rename for the lols
      
      if (ncol(items) > 1 & check_corrs == T) {
        mat <- cor(items, method = "spearman", use = "complete.obs") 
        print(round(mat, 2))
        if (length(save_cor_mat) > 1) { write.csv(mat, file = save_cor_mat, row.names = T, quote = F) }
      }
      
      if (ncol(items) > 1) {
        # calculate number of missing across timepoints per participant
        items_miss <- apply(items, 1, percent_missing)
        # calculate sum score if NA are less than 25% of all timepoints
        item_score <- as.data.frame(ifelse(items_miss >= 25, NA, rowSums(items, na.rm = T) ))
        colnames(item_score) <- item
      } else { item_score <- items } # when only one timepoint is available just stack it
      item_scores[ , ncol(item_scores) + 1] <- item_score
    } 
    item_scores <- item_scores[, -1] # get rid of NA column we used for structure
    
    # calculate number of missing items per participant
    domain_miss = apply(item_scores, 1, percent_missing)
    # calculate the domain score
    score <- ifelse(domain_miss >= 25, NA, rowMeans(item_scores, na.rm = T) )
    
  } else { # REGULAR SCORE COMPUTATION (used in GenR and for prenatal ALSPAC domains)
    # dataframe with all the included items
    df <- data.frame(df)
    # calculate number of missing items per participant
    domain_miss = apply(df, 1, percent_missing)
    # calculate the domain score
    if (score_type == 'mean_simple') { 
      score <- ifelse(domain_miss >= 25, NA, rowMeans(df, na.rm = T) )
    } else if (score_type == 'sum_simple') {
      score <- ifelse(domain_miss >= 25, NA, rowSums(df, na.rm = T) )
    } else if (score_type == 'sum_weighted') { 
      score <- ifelse(domain_miss >= 25, NA, 
                      rowSums(df, na.rm = T) * length(df)/(length(df) - rowSums(is.na(df)))) 
    }
  }
  return(c(domain_miss, score))
}

# NOTE: repmeas and domainscore functions require dichotomized variables 
# (0 and 1, or 1 and 2). They assume that the highest value indicates the risk!
