
##### All IPD-MA sets consist of 5 studies with aprroximatelly 500 participants.
##### First scenario (Heterogeneous regression lines with equal BMI ranges)
##### Across studies the regression lines are different and the ranges of BMI are the same.


source("Assisting functions/IPD-MA_Data_creator.R")

df1= data.creator(name.of.Clustering = "Study",       ### This is the name of the Study variable
                  name.of.Treatment = "Treatment",     ### This is the name of the Treatment variable
                  number.of.studies = 5,               ### Number of Sutdies
                  values.of.clustering.variable =  c("1st Study","2nd Study","3rd Study","4th Study","5th Study"), ### Names of Studies
                  step.size =  0.0861,                 ### Here we set the step which the effect modifier values are generated
                  Treatment=  c(0,1),                  ### Values of Treatment
                  name.of.Continuous = "BMI",          ### Name of the effect modifier
                  ranges.of.Continuous =  c( 18.5,   40, 
                                             18.5,   40,
                                             18.5,   40,
                                             18.5,   40,
                                             18.5,   40)) ### Per study range of the effect modifier

## We create a pseudo-variable that will help us create the curves we described in the paper
df1$BMI.standardised =  with(df1, 2*(BMI-25)/40)

## And add a column to generate the true underlying mortality risk 
df1$`Mortality risk` = NA

#### Random parameters that shift the regression lines across studies creating heterogeneity. 
set.seed(32)
shift =  round(runif(5, -0.05,0.05),2)
shift2 = round(runif(5, -0.05,0.05),2)



df1$Study.shift.intercept =  df1$Study 
df1$Study.shift.slope =  df1$Study
df1=df1%>%
  mutate(Study.shift.intercept=recode(Study.shift.intercept,
                                      '1st Study' = shift[1],
                                      '2nd Study' = shift[2],
                                      '3rd Study' = shift[3],
                                      '4th Study' = shift[4],
                                      '5th Study'= shift[5]  ), 
         Study.shift.slope = recode(Study.shift.slope,
                                    '1st Study' = shift2[1],
                                    '2nd Study' = shift2[2],
                                    '3rd Study' = shift2[3],
                                    '4th Study' = shift2[4],
                                    '5th Study'= shift2[5]  ))


### Generate the mortality risk as function of BMI
df1$`Mortality risk` =  with(df1, 
                             0.2+ Study.shift.intercept + BMI.standardised^2  + 
                               BMI.standardised^4*Treatment + Study.shift.slope*Treatment -  
                               BMI.standardised^2* Treatment)

### Create the binary outcome given the mortality risks calculated above
set.seed(59663)
df1$Y <- rbinom(dim(df1)[1],1,df1$`Mortality risk`) 

### Make treatment variable a factor with two levels "Control" and "Treated" 
df1$Treatment <- factor(df1$Treatment , levels = c(0,1), labels = c("Control","Treated"))


### Remove unnecessary objects
rm(shift, shift2, data.creator)
