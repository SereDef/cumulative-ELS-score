# Point to the necessary libraries
library(foreign)

pathtodata <- readline(prompt="Enter path to data: ")

# The code assumes that all raw data is stored in ONE folder:
#dir = "/Users/Serena/Desktop/Data/" # -> CUSTOMIZE TO APPROPRIATE PATH

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
readquick <- function(filename, rootdir=pathtodata){ # only works for SPSS files
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