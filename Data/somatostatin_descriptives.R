## Fractional Polynomials
## Load data-set

### Load the data-set
## Caroli patients had 2 measurements so we omit half


#somatostatin <- read_sav("C:/Users/Michael Belias/Documents/Continuous-effect-modifier/Tom_Gevers data analysis/IPD meta-analysis somatostatin.sav")
# glimpse(somatostatin)
names(somatostatin)


# Only pick the ones that are important

somatostatin = somatostatin%>%
  select(-contains("SF36"))%>%
  select(-contains("Baseln"))%>%
  select(-contains("MDRD"))%>%
  select(-contains("pred_"))%>%
  select(-contains("stb"))%>%
  select(-contains("age_gender_"))%>%
  select(-contains("Remarks"))%>%
  select(-contains("Frequence"))%>%
  select(-contains("Race"))%>%
  select(-contains("Kid"))%>%
  select(-contains("Mutation"))%>%
  select(-contains("RESID_1"))%>%
  select(-contains("Age_30"))%>%
  select(-contains("Age_50"))%>%
  select(-contains("Age_70"))%>%
  select(-contains("Liver_analysis"))

somatostatin$Gender = factor(somatostatin$Gender,levels = c(0,1), labels = c("Female","Male") )

somatostatin$Drug = factor(somatostatin$Drug,levels = c(0,1),labels = c("placebo","somatostatin"))

somatostatin$Study = factor(somatostatin$Study,levels = c(1:3), labels = c("Keimpema","Hogan","Caroli"))

#somatostatin$Age_at_start = as.numeric(somatostatin$Age_at_start)/100

names(somatostatin)[which(names(somatostatin)=="Age_at_start")] = "Age"
