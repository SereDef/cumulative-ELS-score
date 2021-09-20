# cumulative-ELS-score
Hello there! 
Here you can find a set of scripts for building a **cumulative psycho-social risk exposure** score spanning across two developmental periods (during ***pregnancy*** and across ***childhood***) and attempting to capture a global and additive measure of Early-life stress (ELS). 

Prenatal and postnatal ELS scores are composed of five cumulative (additive) risk domains: **Life events** (e.g. death of a parent or pregnancy complications), **contextual risk** (e.g. financial difficulties or neighbourhood problems), **parental risk** (e.g. parental criminal record or parental depression) and **interpersonal risk** (e.g. family conflicts or loss of a friend) and ***direct victimization*** (only available postnatally, e.g. bullying or harsh parenting). Each domain is constructed by dichotomizing the belonging risk factors (0 = no risk; 1 = risk) and then summing the dichotomous scores.

These scores are based on Generation R (https://generationr.nl/) and ALSPAC (http://www.bristol.ac.uk/alspac/) data on (from pregnancy to 10 years of age). They have been originally conceived and used by Cecil et al., 2014; Rijlaarsdam et al., 2016; and Schuurmans (in preparation). All indicators were harmonized across Generation R and ALSPAC cohorts with respect to closest item-similarity. 

Keep reading if you want to find out more about this score. We will try to outline advantages and disadvantages of using it, the outcomes that have already been related (or not) to it, and track and trace the decision process behind every step of it's construction. If something is unclear, or if you can spot any mistake, please contact me. Any input is very welcome! 

## Why should you consider using a cumulative ELS score to begin with?
Cumulative scores, similar to this one are widely used in developmental psychology and medicine because they proved to be a *parsimonious* and statistically sensitive metric. They make no assumptions about the relative strengths of multiple risk factors or their collinearity, and they tend to fit well with underlying theoretical models. Of course, they also come with a number of shortcomings, that you should be aware of before deciding to use the score. (check e.g. Evans, Li and Whipple, 2013).

## Shortcomings (and patches!) of using this score
As his fellow additive indices, this one also has quite a few shortcomings. Importantly, for example, risk is inevitably designated with some degree of *arbitrariness*, and dichotomization into risk or no risk was not always a straightforward decision. Because we are aware of how problematic this may be for replicability, we tried to list as meticulously as possible the decisions that were taken and the rationale under each one of them. Note also that information on *risk intensity* is lost, together with the possibility of statistical interactions between risk factors.

## Score overview
Generation R:
![alt text](imgs/overviewR.png)
ALSPAC:
![alt text](imgs/overviewA.png)

Some items are mirrowed in prenatal and postnatal corresponding domains, but some are more period-specific. Here is an overview of the item overlap between periods for the two cohorts: 

<img src="imgs/overlapR.png" width="400"/> <img src="imgs/overlapA.png" width="400"/> 

Let's take a look at the correlation patterns underlying the score. The black and blue lines can help you orient between the dirrent domains and periods. Those depicted here are Spearman correations. 
![alt text](imgs/matrixels.png)


## Prenatal stress: raw items and prevalence
![alt text](imgs/preLE.png)
![alt text](imgs/preCR.png)
![alt text](imgs/prePR.png)
![alt text](imgs/preIR.png)

## Postnatal stress: raw items and prevalence
![alt text](imgs/postLE.png)
![alt text](imgs/postCR.png)
![alt text](imgs/postPR.png)
![alt text](imgs/postIR.png)
![alt text](imgs/postDV.png)

## Prediction of negative health outcomes, what do we know so far
Similar versions of the score have been succesfully used to predict later mismatch between cognitive abilities (i.e. IQ) and accademic achievement (Schuurmans, in preparation), internalizing problems and cardio-metablic health in 10 years old children. 
Exposure to prenatal ELS was examined in relation to DNA methylation (DNAm) at birth, as a proxy of stable epigenetic changes (Rijlaarsdam et al., 2016). We did not find a robust assocaitions. However, we are furthr examining whether prental ELS in interaction with genetic predisposition may have better explainatory power over DNAm patterns. 
Note that a paralel version of the postnatal ELS score, spanning from birth to 6 years of age, was created in Generation R and is being examined in relation to internalizing and externalizing behavior (moderated by temperament and executive functioning; de Maat et al, in preparation) and problem behavior (measured with the Berkeley puppet interview) ()

## Decision dictionary: item inclusion / exclusion and dichotomization stratergies 
Summary of key references for variable collapsing strategies:
* Material deprivation: EU-SILC
* Income & education: CBS
* BSI & FAD: manual 
* Early parenthood, marital status and family size: Cecil et al. (2014), Cortes Hidalgo et al (2018) & Rijlaarsdam (2016) 
* Harsh parenting: Jansen et al.(2012)
* Bullying: Muetzel et al (2019) 

The score is designed to be a comprehensive measure of exposure to *psycho-social* stressors. However, early stress factors that are more biological in nature (e.g. maternal smoking or alcohol consumption, pollution) are left out of the score. 

