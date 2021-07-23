# Point to the necessary libraries
library(foreign)
library(car)

# check if the path to the data is already in memory, otherwise ask for it. 
if (exists("pathtodata") == F) { pathtodata = readline(prompt="Enter path to data: ") } # -> CUSTOMIZE TO APPROPRIATE PATH
# The code assumes that all raw data is stored in ONE folder.

if (substr(pathtodata, 1, 1) == "") {
  cat("You did not provide a the path to the data I asked for! RUDE!")
  pathtodata <- readline(prompt="Enter path to data: ")
} 
# else if (substr(pathtodata, nchar(pathtodata), nchar(pathtodata)) != "/" | substr(pathtodata, nchar(pathtodata), nchar(pathtodata)) != "\\") {
#  cat("Do not forget the last slash in your path!") } else { cat("OK, let's go!") }

#### ----------------------------- FUNCTIONS ----------------------------- ####

# read in data quickly
readquick <- function(filename, rootdir = pathtodata, exclude_col = "") { # only works for SPSS files
  dat <- read.spss(paste(rootdir, filename, sep=""), 
                   use.value.labels = F, to.data.frame = T)
  # Get rid of all capital letters in column names (so you don't have to worry)
  names(dat) <- tolower(names(dat))
  # Replace values of 777, 888 or 999 with NAs unless they are IDCs or IDMs 
  # If you do not want this to happen for any other column use the exclude_col argument. 
  for (i in 1:length(dat)) {
    if (colnames(dat)[i] == "idm" | colnames(dat)[i] == "idc" | colnames(dat)[i] == exclude_col) {
      dat[,i] <- dat[,i]
    } else {
      dat[,i] <- ifelse(dat[,i] == 777 | dat[,i] == 888 | dat[,i] == 999, NA, dat[,i]) }
  } 
  return(dat)
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
  return(dich_score)
}
#-------------------------------------------------------------------------------
# General functioning of the family was measured with the family assessment device (FAD).
# This function calculates FAD scores accounting for (max 25%) missing values and 
# dichotomizes the scores according to the cutoff for unhealthy family functioning 
# provided by the manual (Byles et al., 1988), = 2.17. 
fad_scores <- function(set){
  fadtotal <- rowSums(abs(set)) # Sum items
  no_na_fad <- rowSums(!is.na(set)) # Number of endorsed items
  # Compute mean score + do not calculate when more than 25% of items are missing
  fad_score <- ifelse(no_na_fad >= 9, yes = fadtotal/no_na_fad, no = NA) 
  dich_fad_score <- ifelse(fad_score > 2.17, yes = 1, no = 0) 
  return(dich_fad_score) 
}

#-------------------------------------------------------------------------------
# Calculate the percentage missing data
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }

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
      x[,i] <- x[,i] - 1 
      } else if ( (range(x[,i], na.rm = T)[1] != 0 | range(x[,i], na.rm = T)[2] != 1) 
              & ( range(x[,i], na.rm = T)[1] != range(x[,i], na.rm = T)[2] ) ) { 
        stop('Items are not dichotomized') }
  } 
  temp <- rowMeans(x, na.rm = T) 
  # combine and dichotomize the repeated measures
  if (strategy == 'oncealways') {  temp[temp > 0] <- 1 
  } else if (strategy == 'chronic') { temp[temp < 1] <- 0 }
  
  temp[is.nan(temp)] <- NA
  
  return(temp)
}
#-------------------------------------------------------------------------------
# Domain scores measure how many adversities are reported. This function returns 
# the domain risk score and the percentage of missing values within the domain. 
# Default score_type is the mean nr of adversities (ranging from 0 to 1), that is 
# calculated whenever a domain is at least 75% complete, otherwise the domain score 
# is NA. This missing value needs to be accounted for by multiple imputation. 
# However, the function can also provide a cumulative number of adversities 
# (set score_type = 'sum_simple') or a weighted sum score like the one used in 
# Rijlaarsdam et al. (2016) and Cecil et al. (2014) (set score_type = 'sum_weighted')
# that both also allow for 25% missing.

# calculate the domain scores
domainscore <- function(df, score_type = 'mean_simple'){
  # dataframe with all the included items
  df <- data.frame(df)
  # check if all variables in df are dichotomized 
  for (i in 1:ncol(df)){
    if (range(df[,i], na.rm = T)[1] == 1 & range(df[,i], na.rm = T)[2] == 2){ df[,i] <- df[,i] - 1}
    else {
      if (range(df[,i], na.rm = T)[1] != 0 | range(df[,i], na.rm = T)[2] != 1 ){
        stop('Items are not dichotomized') } 
    }
  }
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
  return(c(domain_miss, score))
}

# NOTE: repmeas and domainscore functions require dichotomized variables 
# (0 and 1, or 1 and 2). They assume that the highest value indicates the risk!
# ------------------------------------------------------------------------------