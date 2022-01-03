
# POSTNATAL 0-9Y ITEMS RECODING

# This script is used for dichotomising postnatal ELS variables into 1 = risk and 0 = no risk. 

# In most cases, variables have been coded as:
        # 1 = affected a lot
        # 2 = fairly affected
        # 3 = mildly affected
        # 4 = not effected at all
        # 5 = didn't happen
# for total number of options included need to check the dataset 
# However, answers are phrased slightly different at each time point, so we will 
# adjust the yes and no argument of dichotomize() function accordingly.

# Below script recodes them into: 
        # 1 = risk (for values between 1-4)
        # 0 = no risk (for value of 5) 
        # any other number = NA (missing)

# NOTE: we will not be using the 8 weeks timepoint in this project as 
################################################################################
################################################################################

# Source dichotomization function and also automatically load the data into alspac.table
source("ALSPAC.0-Setup_and_functions.R")

################################################################################
################################################################################
                              # 1. LIFE EVENTS
################################################################################

# LE_8W <- dichotomize(
#   vars = c("e400", # PTNR died since MID PREG
#            "e401", # CH died since MID PREG
#            "e402", # FRD or REL died since MID PREG
#            "e403", # CH ill since MID PREG
#            "e404", # PTNR ill since MID PREG
#            "e405", # Friend or REL ill since MID PREG
#            "e406", # Admitted to HOSP since MID PREG
#            "e410", # Ill since MID PREG
#            "e421", # Moved house since PREG
#            "e429", # MC scare since MID PREG
#            "e430", # Started new job since MID PREG
#            "e431", # Test for CH abnormality since MID PREG
#            "e432", # Test suggested CH PROB since MID PREG
#            "e433", # Discovered having twins since MID PREG
#            "e434", # Heard event might harm CH since MID PREG
#            "e436", # Took an exam since MID PREG
#            "e439", # House burglary/car theft since MID PREG
#            "e440"), # Accident since MID PREG
#   yes = c("Affected a lot","MOD affected","Mildly affected","No effect"),
#   no = c("Did not happen") )
# Factor levels missed out: Other, DK

LE_8M <- dichotomize(
  vars = c("f220",  # Death of partner
           "f221",  # Death of one of children
           "f222",  # Death of friend or relative
           "f223",  # Child ill
           "f224",  # PTNR ill 
           "f225",  # Friend or relative ill
           "f226",  # Admitted to hospital
           "f230",  # Mum ill
           "f241",  # Moved house
           "f251",  # New job for Mum
           "f250",  # Mum became pregnant
           "f252",  # Return to work
           "f255",  # Exam taken
           "f260",  # New job for partner
           "f261"), # Pet died
  yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect"), 
  no = c("N did not happen") )
# NK set to NA

LE_21M <- dichotomize(
  vars = c("g300",  # Partner died >CH8MTHs
           "g301",  # One of Mums children died >CH8MTHs
           "g302",  # Friend or relative died >CH8MTHs
           "g303",  # One of Mums children ill >CH8MTHs
           "g304",  # Partner ill >CH8MTHs
           "g305",  # Friend or relative ill >CH8MTHs
           "g306",  # Mum in hospital >CH8MTHs
           "g310",  # Mum very ill >CH8MTHs
           "g321",  # Mum moved house >CH8MTHs
           "g330",  # Mum pregnant >CH8MTHs
           "g331",  # Mum started new job >CH8MTHs
           "g332",  # Mum returned to work >CH8MTHs
           "g335",  # Mum took exam >CH8MTHs
           "g339",  # Mums house or car burgled >CH 8MTHs
           "g340",  # Partner started new job >CH8MTHs
           "g341",  # A pet died >CH8MTHs
           "g342"), # Mum had accident >CH8MTHs
  yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect"),
  no = c("Did Not Happen") )
# Other, DK set to NA

LE_3Y <- dichotomize(
  vars = c("h210",  # Whether partner died since study child was 18 months old, Y/N
           "h211",  # Whether one of mums children died since study child was 18 months old, Y/N
           "h212",  # Whether a friend or relative died since study child was 18 months old, Y/N
           "h213",  # Whether one of mums children was ill since study child was 18 months old, Y/N
           "h214",  # Whether partner was ill since study child was 18 months old, Y/N
           "h215",  # Whether a friend or relative was ill since study child was 18 months old, Y/N
           "h216",  # Whether mum was admitted to hospital since study child was 18 months old, Y/N
           "h220",  # Whether mum was very ill since study child was 18 months old, Y/N
           "h231",  # Whether mum moved house since study child was 18 months old, Y/N
           "h240",  # Whether mum became pregnant since study child was 18 months old, Y/N
           "h241",  # Whether mum started a new job since study child was 18 months old, Y/N
           "h242",  # Whether mum returned to work since study child was 18 months old, Y/N
           "h245",  # Whether mum took an exam since study child was 18 months old, Y/N
           "h249",  # Whether house or car was burgled since study child was 18 months old, Y/N
           "h250",  # Whether partner started a new job since study child was 18 months old, Y/N
           "h251",  # Whether a pet died since study child was 18 months old, Y/N
           "h252"), # Whether mum had an accident since study child was 18 months old, Y/N
  yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect"),
  no = c("didnt happen") )
# other, dk set to NA

LE_4Y <- dichotomize(
  vars = c("j300",  # Partner Died > CH 30 MTHs y/n
           "j301",  # 1 of MUMs Children Died> CH 30 MTHs y/n
           "j302",  # MUMs FRD or Relative Died> CH 30 MTHs y/n
           "j303",  # 1 of MUMs CDRN Ill> CH 30 MTHs y/n
           "j304",  # Partner Ill> CH 30 MTHs y/n
           "j305",  # MUM FRD or Relative Ill> CH 30 MTHs y/n
           "j306",  # MUM in HOSP> CH 30 MTHs y/n
           "j310",  # MUM was Very Ill> CH 30 MTHs y/n
           "j321",  # MUM Moved House> CH 30 MTHs y/n
           "j330",  # MUM Became PREG> CH 30 MTHs y/n
           "j331",  # MUM Began New Job> CH 30 MTHs y/n
           "j332",  # MUM Returned to Work> CH 30 MTHs y/n
           "j335",  # MUM Took An Exam> CH 30 MTHs y/n
           "j339",  # MUMs House or Car Burgled> CH 30 MTHs y/n
           "j340",  # Partner Began New Job> CH 30 MTHs y/n
           "j341",  # MUMs Pet Died> CH 30 MTHs y/n
           "j342"), # MUM Had Accident> CH 30 MTHs y/n
  yes = c("Yes and affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, not affect at all"),
  no = c("No") )
# "No/Missing" set to NA

LE_5Y <- dichotomize(
  vars = c("k4000",  # Mothers partner died in past year
           "k4001",  # Mothers child died in past year
           "k4002",  # D3: Mothers friend or relative died in past year
           "k4003",  # D4: Mothers child was ill in past year
           "k4004",  # D5: Mothers partner was ill in past year
           "k4005",  # D6: Mothers friend or relative was ill in past year
           "k4006",  # D7: Mother was admitted to hospital in past year
           "k4010",  # D11: Mother was very ill in past year
           "k4021",  # Mother moved house in past year
           "k4030",  # Mother became pregnant in past year
           "k4031",  # Mother started a new job in past year
           "k4032",  # Mother returned to work in past year
           "k4035",  # Mother took an examination in past year
           "k4039",  # Mothers house or car was burgled in past year
           "k4040",  # Mothers partner started a new job in past year
           "k4041",  # Mothers pet died in past year
           "k4042"), # Mother had Accident in past year
  yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all"),
  no = c("No, did not happen") )
# "Other text answer" and "Relevant text but no box ticked" set to NA 

LE_6Y <- dichotomize(
  vars = c("l4000",  # Respondent's partner died since study child's 5th birthday
           "l4001",  # One of respondent's children died since study child's 5th birthday
           "l4002",  # Respondent's friend/relative died since study child's 5th birthday
           "l4003",  # One of respondent's children was ill since study child's 5th birthday
           "l4004",  # Respondent's partner was ill since study child's 5th birthday
           "l4005",  # Respondent's friend/relative was ill since study child's 5th birthday
           "l4006",  # Respondent was admitted to hospital since study child's 5th birthday
           "l4010",  # Respondent was very ill since study child's 5th birthday
           "l4021",  # Respondent moved house since study child's 5th birthday
           "l4030",  # Respondent became pregnant since study child's 5th birthday
           "l4031",  # Respondent started new job since study child's 5th birthday
           "l4032",  # Respondent returned to work since study child's 5th birthday
           "l4035",  # Respondent took an examination since study child's 5th birthday
           "l4039",  # Respondent's house/car was burgled since study child's 5th birthday
           "l4042",  # Respondent's partner started new job since study child's 5th birthday
           "l4043",  # A pet of respondent died since study child's 5th birthday
           "l4044",  # Respondent had an accident since study child's 5th birthday
           "l4041"), # One of respondent's children started new school since study child's 5th birthday
  yes = c("Yes & affected respondent a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect respondent at all"),
  no = c("No, did not happen") )
# 'Other', 'DK', and 'Relevant text but no box ticked' set to NA

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

LE_child_18M <- dichotomize(
  vars = c("kd500a",  # Ch taken into care
           "kd501a",  # A pet died (adj)
           "kd502a",  # Ch moved home (adj)
           "kd503a",  # Ch had fright (adj)
           "kd506a",  # Ch separated from mum for > a wk (adj)
           "kd507a",  # Ch separated from dad for > a wk (adj)
           "kd508a",  # CH Acquired New Parent > 6 MTHS
           "kd509a",  # Ch had a new sibling (adj)
           "kd510a",  # Ch admitted to hospital (adj)
           "kd511a",  # Ch changed carer (adj)
           "kd512a",  # Ch separated from someone else (adj)
           "kd513a"), # Ch started nursery (adj)
  yes = c("Yes & CH Very Upset", "Yes & CH Quite Upset", "Yes & CH Bit Upset", "Yes & CH Not Upset"),
  no = c("Did Not Happen") )   
# Other, DK set to NA

LE_child_30M <- dichotomize(
  vars = c("kf450",  # Child taken into care > 18 months, Y/N
           "kf451",  # A pet died > 18 months, Y/N
           "kf452",  # Child moved home > 18 months, Y/N
           "kf453",  # Child had fright > 18 months, Y/N
           "kf456",  # Child sep.from mother >1wk >18 mths, Y/N
           "kf457",  # Child sep.from father >1wk >18 mths, Y/N
           "kf458",  # Child got new parent > 18 months, Y/N
           "kf459",  # Child got new sibling > 18 months, Y/N
           "kf460",  # Child admitted to hospital >18 mths, Y/N
           "kf461",  # Child changed carer > 18 months, Y/N
           "kf462",  # Child sep.from somebody > 18 months, Y/N
           "kf463"), # Child started new creche >18 months, Y/N
  yes = c("yes child very upset", "yes quite upset", "yes bit upset", "yes not upset"),
  no = c("no didnt happen") )
# other, dk set to NA

LE_child_3Y <- dichotomize(
  vars = c("kj460",  # Child Taken Into Care
           "kj461",  # Pet died 
           "kj462",  # Child Moved Home 
           "kj463",  # Child Had Shock 
           "kj466",  # Child & Mum Separated 
           "kj467",  # Child & Dad Separated 
           "kj468",  # Child Got a New Parent 
           "kj469",  # Child Got a New Sibling 
           "kj470",  # Child Admitted To Hospital 
           "kj471",  # Child Changed Carer 
           "kj472",  # Child Separated From Someone Else
           "kj473"), # Child Started New Creche 
  yes = c("Yes CH Very Upset", "Yes Quite Upset", "Yes Bit Upset", "Yes Not Upset"),
  no = c("No didnt Happen") )
# Other, DK set to NA

LE_child_4Y <- dichotomize(
  vars = c("kl470",  # Child taken into care since age 3
           "kl471",  # A pet died since child age 3
           "kl472",  # Child moved home since age 3
           "kl473",  # Child had shock or fright since age 3
           "kl476",  # Child separated from mother since age 3
           "kl477",  # Child separated from father since age 3
           "kl478",  # Child acquired new mother or father since age 3
           "kl479",  # Child had new brother or sister since age 3
           "kl480",  # Child admitted to hospital since age 3
           "kl481",  # Child changed care taker since age 3
           "kl482",  # Child separated from someone else since age 3
           "kl483",  # Child started new nursery/kindergarten since age 3
           "kl484"), # Child started school since age 3
  yes = c("Yes, child was very upset","Yes, child was quite upset","Yes, child was a bit upset","Yes, child was not upset"),
  no = c("No, did not happen") )
# "Relevant text but no box ticked" set to NA

LE_child_5Y <- dichotomize(
  vars = c("kn4000",  # Child taken into care in past 15 months
           "kn4001",  # Child's pet die in past 15 months
           "kn4002",  # Child move home in past 15 months
           "kn4003",  # Child have a fright or shock in past 15 months
           "kn4006",  # Child separated from mother in past 15 months
           "kn4007",  # Child separated from Father in past 15 months
           "kn4008",  # Child acquire new parent in past 15 months
           "kn4009",  # Child have a new brother or sister in past 15 months
           "kn4010",  # Child admitted to hospital in past 15 months
           "kn4011",  # Child's main carer change in past 15 months
           "kn4012",  # Child separated from another person in past 15 months
           "kn4013",  # Child start a new nursery in past 15 months
           "kn4014"), # Child start school in past 15 months)
  yes = c("Yes And Was Very Upset","Yes And Was Quite Upset","Yes And Was A Bit Upset","Yes But Was Not Upset"),
  no = c("No Did Not Happen") )
# "Other Text Answer", "DK" set to NA

LE_child_6Y <-  dichotomize(
  vars = c("kq360",  # Child was taken into care since his/her 5th birthday (Y/N)
           "kq361",  # A pet died since child's 5th birthday (Y/N)
           "kq362",  # Child moved home since his/her 5th birthday (Y/N)
           "kq363",  # Child had a shock/fright since his/her 5th birthday (Y/N)
           "kq366",  # Somebody in the family died since child's 5th birthday (Y/N)
           "kq367",  # Child was separated from his/her mother since his/her 5th birthday (Y/N)
           "kq368",  # Child was separated from his/her father since his/her 5th birthday (Y/N)
           "kq369",  # Child acquired a new mother/father since his/her 5th birthday (Y/N)
           "kq370",  # Child had a new brother or sister since his/her 5th birthday (Y/N)
           "kq371",  # Child was admitted to hospital since his/her 5th birthday (Y/N)
           "kq372",  # Child changed care taker since his/her 5th birthday (Y/N)
           "kq373"), # Child was separated from another close person since his/her 5th birthday (Y/N)
  yes = c("Yes, very upset", "Yes, quite upset", "Yes, a bit upset", "Yes, not upset"),
  no = c("No, did not happen") )

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


# Merge all timepoints together 
LE_postnatal <- cbind(LE_8M, LE_21M, LE_3Y, LE_4Y, LE_5Y, LE_6Y, LE_9Y, # LE_8W
                      LE_child_18M, LE_child_30M, LE_child_3Y, LE_child_4Y, LE_child_5Y, LE_child_6Y, LE_child_8Y)

################################################################################
                          # 2. CONTEXTUAL RISK
################################################################################

# CR_8W <- dichotomize(
#   vars = c("e411",  # PTNR lost job since MID PREG
#            "e414",  # Lost job since MID PREG
#            "e418",  # Income reduced since MID PREG	
#            "e423",  # Became homeless since PREG
#            "e424"), # Major financial PROB since MID PREG	
#   yes = c("Affected a lot","MOD affected","Mildly affected","No effect"),
#   no = c("Did not happen") )
# "Other" "DK" set to NA

CR_8M <- dichotomize(
  vars = c("f231",  # Partner lost job
           "f234",  # Mum lost job
           "f238",  # Reduced income
           "f243",  # Became homeless
           "f244"), # Major financial problems
  yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect"),
  no =c("N did not happen") )
# "NK" set to NA

CR_21M <- dichotomize(
  vars = c("g311", # Partner lost job >CH8MTHs
           "g314", # Mum lost job >CH8MTHs
           "g318",	
           "g323",	
           "g324"),
  yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect"),
  no = c("Did Not Happen") )
# Other, DK set to NA

CR_3Y <- dichotomize(
  vars = c("h221", # Whether partner lost job since study child was 18 months old, Y/N
           "h224", # Whether mum lost job since study child was 18 months old, Y/N
           "h228",	
           "h233",	
           "h234"),
  yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect"),
  no = c("didnt happen") )
# "other" "dk" set to NA

CR_4Y <- dichotomize(
  vars = c("j311", # Partner Lost Job> CH 30 MTHs y/n
           "j314", # MUM lost Job> CH 30 MTHs y/n
           "j318",	
           "j323",
           "j324"),
  yes = c("Yes, not affect at all","Yes, mildly affected","Yes, moderately affected","Yes and affected a lot"),
  no = c("No") )
# "No/Missing" set to NA

CR_5Y <- dichotomize(
  vars = c("k4011", # Mothers partner lost job in past year
           "k4014", # Mother lost her job in past year
           "k4018",	
           "k4023",
           "k4033", # Mother had a miscarriage in past year
           "k4024"),
  yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all"),
  no = c("No, did not happen") )
# "Other text answer" set to NA

CR_6Y <- dichotomize(
  vars = c("l4011", # Respondent's partner lost their job since study child's 5th birthday
           "l4014", # Respondent lost their job since study child's 5th birthday
           "l4018",
           "l4023",
           "l4024"),
  yes = c("Yes & affected respondent a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect respondent at all"),
  no = c("No, did not happen") )
# "Other", "DK" set to NA

CR_9Y <- dichotomize(
  vars = c("p2018",  # Mother's income was reduced since the study child's 6th birthday
           "p2023",  # Mother became homeless since the study child's 6th birthday
           "p2024"), # Mother had a major financial problem since the study child's 6th birthday
  yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+"),
  no = c("No, did not happen in past 3 years") )

# Neighborhood stress score:  based on the following items:
# •	G485 Badly fitted doors and windows are problem •	G486 Poor ventilation
# •	G487 Noise in rooms of home is problem • G488 Noise from other homes is problem 
# •	G489 Noise from outside is problem • G490 Rubbish dumped in neighbourhood is problem 
# •	G491 Dog dirt on pavement is problem • G492 Worry about vandalism is problem 
# •	G493 Worry about burglaries is problem • G494 Worry about attacks is problem
# •	G495 Disturbance from youth is problem.
# Variables G485-G495 were recoded (4,3=0)(2=1)(1=2). G496 = G485+.......+G495 
# higher scores = worse neighborhood problems

# Transform from factor to numeric
NP <- data.frame("g496" = as.numeric(levels(alspac.table$g496))[alspac.table$g496],  # neighborhood problems 21m
                 "h366" = as.numeric(levels(alspac.table$h366))[alspac.table$h366])  # neighborhood problems 3y

# Compute 80th percentile value
cutoff_21m <- quantile(NP[, "g496"], .8, na.rm = T) # 6
cutoff_3y  <- quantile(NP[, "h366"], .8, na.rm = T) # 6

# Dichotomize based on 80th percentile value
NP[, "g496a_rec"] <- ifelse( NP[, "g496"] >= cutoff_21m, 1, 
                             ifelse(NP[, "g496"] < cutoff_21m, 0, NA))
NP[, "h366a_rec"] <- ifelse( NP[, "h366"] >= cutoff_3y, 1, 
                             ifelse(NP[, "h366"] < cutoff_3y, 0, NA))

# Merge all CR variables together 
CR_postnatal <- cbind(CR_8M, CR_21M, CR_3Y, CR_4Y, CR_5Y, CR_6Y, CR_9Y, NP) # CR_8W

# Add maternal and paternal education
CR_postnatal$m_ed = ifelse( !is.na(alspac.table$k6292) & alspac.table$k6292 == 'Yes', 0,
                    ifelse((!is.na(alspac.table$k6281) & alspac.table$k6281 == 'Yes')
                          |(!is.na(alspac.table$k6282) & alspac.table$k6282 == 'Yes')
                          |(!is.na(alspac.table$k6283) & alspac.table$k6283 == 'Yes')
                          |(!is.na(alspac.table$k6284) & alspac.table$k6284 == 'Yes')
                          |(!is.na(alspac.table$k6280) & alspac.table$k6280 == 'Yes'), 1, NA))

CR_postnatal$p_ed = ifelse( !is.na(alspac.table$k6312) & alspac.table$k6312 == 'Yes', 0,
                    ifelse((!is.na(alspac.table$k6300) & alspac.table$k6300 == 'Yes')
                          |(!is.na(alspac.table$k6301) & alspac.table$k6301 == 'Yes')
                          |(!is.na(alspac.table$k6302) & alspac.table$k6302 == 'Yes')
                          |(!is.na(alspac.table$k6303) & alspac.table$k6303 == 'Yes')
                          |(!is.na(alspac.table$k6304) & alspac.table$k6304 == 'Yes'), 1, NA))

# Add Housing variables
CR_postnatal$b2n <- ifelse(alspac.table$b2 == 1, 1, ifelse(alspac.table$b2 == 0, 0, NA)) # Housing adequacy 0-2y composite
CR_postnatal$t2n <-	ifelse(alspac.table$t2 == 1, 1, ifelse(alspac.table$t2 == 0, 0, NA)) # Housing adequacy 2-4y composite
CR_postnatal$b3n <-	ifelse(alspac.table$b3 == 1, 1, ifelse(alspac.table$b3 == 0, 0, NA)) # Housing Basic Living 0-2y  composite
CR_postnatal$t3n <-	ifelse(alspac.table$t3 == 1, 1, ifelse(alspac.table$t3 == 0, 0, NA)) # Housing Basic Living 2-4y composite
CR_postnatal$b4n <- ifelse(alspac.table$b4 == 1, 1, ifelse(alspac.table$b4 == 0, 0, NA)) # Housing Defects 0-2y composite
CR_postnatal$t4n <- ifelse(alspac.table$t4 == 1, 1, ifelse(alspac.table$t4 == 0, 0, NA)) # Housing Defects 2-4y composite

################################################################################
                           # 3. PARENTAL RISK
################################################################################

# PR_8W <- dichotomize(
#   vars = c("e407",  # Trouble with law since MID PREG
#            "e412",  # PTNR had PROBS at work since MID PREG
#            "e413",  # PROBS at work since MID PREG
#            "e416",  # PTNR in trouble with law since MID PREG	
#            "e427",  # Attempted suicide since MID PREG
#            "e428",  # Convicted since MID PREG
#            "e435"), # Attempted abortion since MID PREG
#   yes = c("Affected a lot","MOD affected","Mildly affected","No effect"),
#   no = c("Did not happen") )
# "Other", "DK" set to NA

PR_8M <- dichotomize(
  vars = c("f227",	# Mum in trouble with law Y/N
           "f232",  # Work problems for partner
           "f233",  # Work problems for Mum
           "f236",  # Partner in trouble with law	
           "f248",  # Attempted suicide
           "f249",  # Court conviction
           "f253",  # Mum had miscarriage
           "f254"),	# Mum had abortion
  yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect"),
  no = c("N did not happen") )
# NK set to NA

PR_21M <- dichotomize(
  vars = c("g307",
           "g312",  # Partner had problems with work >CH8MTHs
           "g313",  # Mum had problems with work >CH8MTHs
           "g316",
           "g328",
           "g329",
           "g333",  # Mum had miscarraige >CH8MTHs
           "g334"), 
  yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect"),
  no = c("Did Not Happen") )
# "Other", "DK" set to NA

PR_3Y <- dichotomize(
  vars = c("h217",
           "h222", # Whether partner had problems at work since study child was 18 months old, Y/N
           "h223", # Whether mum had problems at work since study child was 18 months old, Y/N
           "h226",
           "h238",
           "h239",
           "h243", # Whether mum had a miscarriage since study child was 18 months old, Y/N
           "h244"),
  yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect"),
  no = c("didnt happen") )
# "other", "dk" set to NA

PR_4Y <- dichotomize(
  vars = c("j307",
           "j312", # Partner Had PROBs at Work> CH 30 MTHs y/n
           "j313", # MUM Had PROBs at Work> CH 30 MTHs y/n
           "j316",
           "j328",
           "j329",
           "j333", # MUM Miscarried> CH 30 MTHs y/n
           "j334"),
  yes = c("Yes, not affect at all","Yes, mildly affected","Yes, moderately affected","Yes and affected a lot"),
  no = c("No") )
# "No/Missing" set to NA

PR_5Y <- dichotomize(
  vars = c("k4007",
           "k4012", # Mothers partner had problems at work in past year
           "k4013", # Mother had problems at work in past year
           "k4016",
           "k4028",
           "k4029",
           "k4034"),
  yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all"),
  no = c("No, did not happen") )
# 'Other text answer' set to NA

PR_6Y <- dichotomize(
  vars = c("l4007",
           "l4012", # Respondent's partner had problems at work since study child's 5th birthday
           "l4013", # Respondent had problems at work since study child's 5th birthday
           "l4016",
           "l4028",
           "l4029",
           "l4033", # Respondent had miscarriage since study child's 5th birthday
           "l4034"),
  yes = c("Yes & affected respondent a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect respondent at all"),
  no = c("No, did not happen") )
# "Other", "DK" set to NA

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

# Merge all timepoints together 
PR_postnatal <- cbind(PR_8M, PR_21M, PR_3Y, PR_4Y, PR_5Y, PR_6Y, PR_9Y) # PR_8W

# Add maternal and paternal early parenthood
PR_postnatal$mz028bn <- as.numeric(as.character(alspac.table$mz028b)) # NAs introduced by coercion because "Consent withdrawn by mother" converted to  NA
# Because mz028b is a factor, we use as.character before as.numeric.
# Factors are stored internally as integers with a table to give the factor level labels.
PR_postnatal$mz028ba_rec <- ifelse(PR_postnatal$mz028bn < 19, yes = 1, no = 0) # early parenthood 
# mother younger than 19 at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)

PR_postnatal$pb910n <- as.numeric(as.character(alspac.table$pb910)) # ditto
PR_postnatal$pb910a_rec <- ifelse(PR_postnatal$pb910n < 19, yes = 1, no = 0) # early parenthood 
# partner younger than 19 at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)

# SELF REPOTED PARENTAL PSYCHOPATHOLOGY in CCCEI-EPDS script ###################

################################################################################
                        # 4. INTERPERSONAL RISK
################################################################################

# IR_8W <- dichotomize(
#   vars = c("e408",  # Divorced since MID PREG
#            "e409",  # PTNR rejected CH since MID PREG
#            "e415",  # PTNR went away since MID PREG
#            "e417",  # Separated since MID PREG
#            "e419",  # Argued with PTNR since MID PREG
#            "e420",  # Argued with FAM/FRDS since MID PREG
#            "e422",  # PTNR hurt MUM since MID PREG
#            "e425",  # Married since MID PREG
#            "e426",  # PTNR hurt CHDR since MID PREG
#            "e437",  # PTNR EMOT cruel to MUM since MID PREG
#            "e438"), # PTNR EMOT cruel to CH since MID PREG
#   yes = c("Affected a lot","MOD affected","Mildly affected","No effect"),
#   no = c("Did not happen") )
# # "Other", "DK" set to NA

IR_8M <- dichotomize(
  vars = c("f228",  # Divorce 
           "f229",  # Child not wanted by partner
           "f235",  # Partner went away
           "f237",  # Separation with partner
           "f239",  # Argued with partner
           "f240",  # Argued with family or friend
           "f242",  # Physically hurt by partner
           "f245",  # Got married
           "f246",  # Partner physically cruel to children
           "f256",  # Partner emotionally cruel to Mum
           "f257",  # Partner emotionally cruel to children
           "f258",  # Mum emotionally cruel to children
           "f247"), # Mum physically cruel to children
  yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect"),
  no = c("N did not happen") )
# "NK" set to NA

IR_21M <- dichotomize(
  vars = c("g308", # Mum divorced >CH8MTHs
           "g309", # Partner rejected child >CH8MTHs
           "g315", # Partner went away >CH8MTHs
           "g317", # Mum and partner separated >CH8MTHs
           "g319", # Mum argued with partner >CH8MTHs
           "g320", # Mum argued with family and friends >CH8MTHs
           "g322", # Partner physically cruel to Mum >CH8MTHs
           "g325", # Mum got married >CH8MTHs
           "g326", # Partner physically cruel to children >CH8MTHs
           "g327", # Mum physically cruel to children >CH8MTHs
           "g336", # Partner emotionally cruel to Mum >CH8MTHs
           "g337", # Partner emotionally cruel to children >CH8MTHs
           "g338"), # Mum emotionally cruel to children >CH8MTHs
  yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect"),
  no = c("Did Not Happen") )
# "Other", "DK" set to NA

IR_3Y <- dichotomize(
  vars = c("h218", # Whether got divorced since study child was 18 months old, Y/N
           "h219", # Whether partner rejected children since study child was 18 months old, Y/N
           "h225", # Whether partner went away since study child was 18 months old, Y/N
           "h227", # Whether mum and partner separated since study child was 18 months old, Y/N
           "h229", # Whether mum argued with partner since study child was 18 months old, Y/N
           "h230", # Whether mum argued with family and friends since study child was 18 months old, Y/N
           "h232", # Whether partner was physically cruel to mum since study child was 18 months old, Y/N
           "h235", # Whether mum got married since study child was 18 months old, Y/N
           "h236", # Whether partner was physically cruel to children since study child was 18 months old, Y/N
           "h237", # Whether mum was physically cruel to children since study child was 18 months old, Y/N
           "h246", # Whether partner was emotionally cruel to mum since study child was 18 months old, Y/N
           "h247", # Whether partner was emotionally cruel to children since study child was 18 months old, Y/N
           "h248"), # Whether mum was emotionally cruel to children since study child was 18 months old, Y/N
  yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect"),
  no = c("didnt happen") )
# "other", "dk" set to NA

IR_4Y <- dichotomize(
  vars = c("j308", # MUM Divorced> CH 30 MTHs y/n
           "j309", # MUM Found PTR Not Want CH> CH 30 MTHs y/n
           "j315", # Partner Went Away> CH 30 MTHs y/n
           "j317", # MUM & Partner Separated> CH 30 MTHs y/n
           "j319", # MUM Argued W Partner> CH 30 MTHs y/n
           "j320", # MUM Argued W FMLY & FRDs> CH 30 MTHs y/n
           "j322", # Partner PHYS Cruel to MUM> CH 30 MTHs y/n
           "j325", # MUM Got Married> CH 30 MTHs y/n
           "j326", # PTR PHYS Cruel to CDRN> CH 30 MTHs y/n
           "j327", # MUM PHYS Cruel to CDRN> CH 30 MTHs y/n
           "j336", # PTR Emotionally Cruel to MUM> CH 30 MTHs y/n
           "j337", # PTR Emotional Cruel to CDRN> CH 30 MTHs y/n
           "j338"), # MUM Emotional Cruel to CDRN> CH 30 MTHs y/n
  yes = c("Yes, not affect at all","Yes, mildly affected","Yes, moderately affected","Yes and affected a lot"),
  no = c("No") )
# "No/Missing" set to NA

IR_5Y <- dichotomize(
  vars = c("k4008",  # Mother was divorced in past year
           "k4009",  # Mother found that her partner did not want her child in past year
           "k4015",  # Mothers partner went away in past year
           "k4017",  # Mother and partner separated in past year
           "k4019",  # Mother argued with her partner in past year
           "k4020",  # Mother argued with family and friends in past year
           "k4022",  # Mothers partner was physically cruel to her in past year
           "k4025",  # Mother got married in past year
           "k4026",  # Mothers partner was physically cruel to children in past year
           "k4027",  # Mother was physically cruel to children in past year
           "k4036",  # Mothers partner was emotionally cruel to her in past year
           "k4037",  # Mothers partner was emotionally cruel to children in past year
           "k4038"), # Mother was emotionally cruel to children in past year
  yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all"),
  no = c("No, did not happen") )
# "Other text answer" set to NA

IR_6Y <- dichotomize(
  vars = c("l4008",  # Respondent was divorced since study child's 5th birthday
           "l4009",  # Respondent found their partner did not want their child since study child's 5th birthday
           "l4015",  # Respondent's partner went away since study child's 5th birthday
           "l4017",  # Respondent separated from partner since study child's 5th birthday
           "l4019",  # Respondent argued with partner since study child's 5th birthday
           "l4020",  # Respondent argued with family/friends since study child's 5th birthday
           "l4022",  # Respondent's partner was physically cruel to them since study child's 5th birthday
           "l4025",  # Respondent got married since study child's 5th birthday
           "l4026",  # Respondent's partner physically cruel to respondent's children since study child's 5th birthday
           "l4027",  # Respondent physically cruel to own children since study child's 5th birthday
           "l4036",  # Respondent's partner was emotionally cruel to them since study child's 5th birthday
           "l4037",  # Respondent's partner was emotionally cruel to respondent's children since study child's 5th birthday
           "l4038",  # Respondent was emotionally cruel to their children since study child's 5th birthday
           "l4040"), # Respondent found new partner since study child's 5th birthday
  yes = c("Yes & affected respondent a lot", "Yes, moderately affected", "Yes, mildly affected", "Yes, did not affect respondent at all"),
  no = c("No, did not happen") )
# "Other", "DK" set to NA

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

# ------------------------------------------------------------------------------
# The following are variables included in Cecil et al. (2014) script for which we 
# have numerous missing values, I substituted those which we have and included them
# as individual items as in Charlotte's script. 

Cha1 <- dichotomize(
  vars = c("h580",  # DV_Shouted_33M 
           "h581"), # Mum/PTNR hit or slapped one another (DV_Hit_33M)
  yes = c("Yes mum did","Yes partner did","Yes both did"),
  no = c("No not at all") )
# "other", "dk" set to NA

Cha2 <- dichotomize(
  vars = c("g712",  # Mum/PTNR hit or slapped one another (DV_Hit_21M)
           "g713"), # Mum/PTNR threw something in anger (DV_Break_21M)
  yes = c("Yes mum Did","Yes Partner Did","Yes both Did"),
  no = c("No not at All") )

Cha3 <- dichotomize(
  vars = c("ke017"), # Par_Smack_2Y
  # EW: unclear how coded (below is my interpretation)
  yes = c("Every day","SEV times WK","C once WK","Rarely"),
  no = c("Never") )

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Merge all timepoints together 
IR_postnatal <- cbind(IR_8M, IR_21M, IR_3Y, IR_4Y, IR_5Y, IR_6Y, IR_9Y, # IR_8W
                      Cha1, Cha2, Cha3)

################################################################################
                        # 5. DIRECT VICTIMISATION
################################################################################

DV_18M <- dichotomize(
  vars = c("kd504a",	# Ch physically hurt by someone (adj)
           "kd505a"),	# Ch sexually abused (adj)
  yes = c("Yes & CH Very Upset", "Yes & CH Quite Upset", "Yes & CH Bit Upset", "Yes & CH Not Upset"),
  no = c("Did Not Happen") )
# "Other", "DK" set to NA

DV_30M <- dichotomize(
  vars = c("kf454",	  # Child physically hurt > 18 months, Y/N
           "kf455"),	# Child sexually abused > 18 months, Y/N
  yes = c("yes child very upset", "yes quite upset", "yes bit upset","yes not upset"),
  no = c("no didnt happen") )
# "other", "dk" set to NA

DV_3Y <- dichotomize(
  vars = c("kj464",	  # Child Was Physically Hurt By Person Y/N
           "kj465"),	# Child Sexually Abused Y/N
  yes = c("Yes CH Very Upset", "Yes Quite Upset", "Yes Bit Upset", "Yes Not Upset"),
  no = c("No didnt Happen") )
# "Other", "DK" set to NA

DV_4Y <- dichotomize(
  vars = c("kl474",	  # Child was physically hurt by someone since age 3 (not Y/N)
           "kl475"),	# Child was sexually abused since age 3
  yes = c("Yes, child was very upset", "Yes, child was quite upset", "Yes, child was a bit upset", "Yes, child was not upset"),
  no = c("No, did not happen") )

DV_5Y <- dichotomize(
  vars = c("kn4004",	# Child physically hurt by someone in past 15 months
           "kn4005"),	# Child sexually abused in past 15 months
  yes = c("Yes And Was Very Upset", "Yes And Was Quite Upset", "Yes And Was A Bit Upset", "Yes But Was Not Upset"),
  no = c("No Did Not Happen") )
# "Relevant text, but no box ticked", "DK", "Other Text Answer" set to NA

DV_6Y <- dichotomize(
  vars = c("kq364",  # Child was physically hurt by someone since his/her 5th birthday (Y/N)
           "kq365"), 
  yes = c("Yes, very upset", "Yes, quite upset", "Yes, a bit upset", "Yes, not upset"),
  no = c("No, did not happen") )

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

# Merge all timepoints together 
DV_postnatal <- cbind(DV_18M, DV_30M, DV_3Y, DV_4Y, DV_5Y, DV_6Y, DV_8Y, DV_9Y)

################################################################################
################################################################################

postnatal_stress_raw <-cbind(LE_postnatal, CR_postnatal, PR_postnatal, IR_postnatal, DV_postnatal)

saveRDS(postnatal_stress_raw, file.path(pathtoresults, "raw_postnatal_stress.rds"))
