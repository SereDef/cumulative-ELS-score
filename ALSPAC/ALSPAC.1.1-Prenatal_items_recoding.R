
# PRENATAL ITEMS RECODING

# This script is used for dichotomising prenatal ELS variables into 1 = risk and 0 = no risk. 

# Originally, variables have been coded as:
        # 1 = affected a lot
        # 2 = fairly affected
        # 3 = mildly affected
        # 4 = not effected at all
        # 5 = didn't happen
# for total number of options inlcuded need to check the dataset 

# Below script recodes them into: 
        # 1 = risk (for values between 1-4)
        # 0 = no risk (for value of 5) 
        # any other number = NA (missing)

# The dichotomize() function defined in the 0.functions.R performs this transformation
# using default arguments: 
        # yes = c("affected a lot","fairly affected","mildly affected","N effect at all")
        # no = c("didnt happen")
# if you want to check correlations btw continuous and binary columns, set check_transf
# argument to TRUE (default = F).

################################################################################
################################################################################

# Source dichotomization function and also automatically load the data into alspac.table
source("ALSPAC.0-Setup_and_functions.R")

################################################################################
################################################################################
                              # 1. LIFE EVENTS
################################################################################

# Creating a data frame with the original LE variables + newly created dichotomous vars
LE_prenatal <- dichotomize(
  vars = c("b570", # PTNR died since PREG
           "b571", # CH died since PREG
           "b572", # Friend or relative died since PREG
           "b573", # CH was ill since PREG	
           "b574", # PTNR was ill since PREG	
           "b575", # Friend or relative was ill since PREG
           "b576", # Admitted to hospital since PREG
           "b580", # V ill since PREG
           "b583", # PROBS at work since PREG
           "b584", # Lost job since PREG		
           "b591", # Moved house since PREG	
           "b595", # Got married since PREG	
           "b599", # Bled & thought might miscarry	
           "b602", # Test result suggesting POSS abnormality	
           "b604", # POSS harm to baby
           "b605", # Tried to have abortion 
           "b609", # House or car burgled since PREG
           "b610") # Had an accident since PREG
   )

# Note: Variables will have unexpected values: other, DK. These values will be recoded to NA.

################################################################################
                            # 2. CONTEXTUAL RISK
################################################################################

# Creating a data frame with the original CR variables + newly created dichotomous vars
CR_prenatal <- dichotomize(
  vars = c("b581",  # PTNR lost job since PREG
           "b588",  # Income reduced since PREG
           "b594"), # Major financial PROB since PREG
  already_dich = c("b593") # Became homeless since PREG
)

# Add maternal and paternal education
CR_education <- dichotomize( 
  vars = c("c645a", # maternal education 
           "c666a"), # paternal education
  yes = c("None", "CSE", "Vocational", "O level", "A level"), 
  no = c("Degree") )

# Merge 
CR_prenatal <- cbind(CR_prenatal, CR_education)

# Add Housing variables
CR_prenatal$p2n	<- ifelse(alspac.table$p2 == 1, 1,
                         ifelse(alspac.table$p2 == 0, 0, NA)) # Housing adequacy 
CR_prenatal$p3n	<- ifelse(alspac.table$p3 == 1, 1,
                         ifelse(alspac.table$p3 == 0, 0, NA)) # Housing Basic Living 
CR_prenatal$p4n	<- ifelse(alspac.table$p4 == 1, 1,
                         ifelse(alspac.table$p4 == 0, 0, NA)) # Housing Defects 

################################################################################
                            # 3. PARENTAL RISK
################################################################################

# Creating a data frame with the original CR variables + newly created dichotomous vars
PR_prenatal <- dichotomize(
  vars = c("b577",   # In trouble with the law since PREG
           "b586",   # PTNR in trouble with law since PREG
           "pb188"), # PTNR convicted of an offence since PREG
   already_dich = c("b597",  # Attempted suicide since PREG
                    "b598"), # Convicted of an offence since PREG; EW: already binary
  yes = c("affected a lot","fairly affected","mildly affected","N effect at all", "N affect at all"), 
  no = c("didnt happen")
)

# Note: some variables will have no registered 'N affect at all' answers. This warning is safe to ignore, 
# as only pb188 has the typo (i.e., 'affect' instead of 'effect')

PR_prenatal$p14n <- ifelse(alspac.table$p4 == 1, 1, # Crime trouble with police
                           ifelse(alspac.table$p4 == 0, 0, NA))

PR_prenatal$mz028bn <- as.numeric(as.character(alspac.table$mz028b)) # NA introduced by coercion because "Consent withdrawn by mother" turned to NA
# Because mz028b is a factor, we use as.character before as.numeric.
# Factors are stored internally as integers with a table to give the factor level labels.
PR_prenatal$mz028ba_rec <- ifelse(PR_prenatal$mz028bn < 19, yes = 1, no = 0) # early parenthood 
# mother age younger than 19 at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)

# PARENTAL PSYCHOPATHOLOGY in CCCEI-EPDS script ################################

## NOTE: For p_interpersonal_sensitivity and m_interpersonal_sensitivity (Interpersonal awareness score) 
## there is no manual-advised cut-off so we are using a 80th percentile cutoff.

# Higher scores = greater interpersonal sensitivity, based on items such as 
#  - 'feel insecure when saying goodbye'; 'I avoid saying what I think for fear of being rejected'
IS <- data.frame("pb551" = as.numeric(as.character(alspac.table[, "pb551"])),  # partner IS
                 "b916"  = as.numeric(as.character(alspac.table[, "b916"])) )  # mother IS
# Compute 80th percentile value
cutoff_p <- quantile(IS[, "pb551"], .8, na.rm = T) # 95
cutoff_m <- quantile(IS[, "b916"], .8, na.rm = T)  # 22 # different cut-off point for partner and mother due to differences in scale

# Dichotomize based on 80th percentile value
IS[, "pb551a_rec"] <- ifelse( IS[, "pb551"] >= cutoff_p, 1, 
                                ifelse(IS[, "pb551"] < cutoff_p, 0, NA))
IS[, "b916a_rec"] <- ifelse( IS[, "b916"] >= cutoff_m, 1, 
                              ifelse(IS[, "b916"] < cutoff_m, 0, NA))
# Check resulting variables
# table(IS$pb551, IS$pb551a_rec); table(IS$b916, IS$b916a_rec)

################################################################################
                          # 4. INTERPERSONAL RISK
################################################################################

IR_prenatal <- dichotomize(
  vars = c( "b579", # PTNR rejected PREG
            "b585", # PTNR went away since PREG	
            "b589", # Argued with PTNR since PREG
            "b590", # Argued with family or friends since PREG
            "b592", # PTNR hurt mum since PREG	
            "b596", # PTNR hurt CH since PREG	
            "b607"), # PTNR was EMOT cruel to mum since PREG
  already_dich = c("b578", # Divorced since PREG; 
                   "b608", # PTNR was EMOT cruel to child since PREG; 
                   "b587") # Separated since PREG;
)

IR_prenatal$p7n <-	 ifelse(alspac.table$p7 == 1, 1,
                           ifelse(alspac.table$p7 == 0, 0, NA)) # Partner Status 
IR_prenatal$p8n	<-   ifelse(alspac.table$p8 == 1, 1,
                           ifelse(alspac.table$p8 == 0, 0, NA))	# Partner Affection 
IR_prenatal$p10n <-	 ifelse(alspac.table$p10 == 1, 1,
                           ifelse(alspac.table$p10 == 0, 0, NA)) # Family Size  
IR_prenatal$p11n	<- ifelse(alspac.table$p11 == 1, 1,
                           ifelse(alspac.table$p11 == 0, 0, NA)) # Family Major problems 
IR_prenatal$p16n	<- ifelse(alspac.table$p16 == 1, 1,
                           ifelse(alspac.table$p16 == 0, 0, NA)) # Partner Support
IR_prenatal$p17n	<- ifelse(alspac.table$p17 == 1, 1,
                           ifelse(alspac.table$p17 == 0, 0, NA)) # Social Network - Emotional 
IR_prenatal$p18n	<- ifelse(alspac.table$p18 == 1, 1,
                           ifelse(alspac.table$p18 == 0, 0, NA)) # Social Network - Practical 

################################################################################
################################################################################

prenatal_stress_raw <-cbind(LE_prenatal, CR_prenatal, PR_prenatal, IS, IR_prenatal)

saveRDS(prenatal_stress_raw, file.path(pathtoresults, "raw_prenatal_stress.rds"))
