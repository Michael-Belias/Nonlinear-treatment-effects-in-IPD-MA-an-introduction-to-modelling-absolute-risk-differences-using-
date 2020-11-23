


##### Third scenario (Heterogeneous regression lines with unequal BMI ranges)
##### Across studies both the regression lines and the ranges of BMI are different

source("Assisting functions/IPD-MA_Data_creator.R")


df3= data.creator(name.of.Clustering = "Study",
                  name.of.Treatment = "Treatment",number.of.studies = 5,
                  values.of.clustering.variable =  c("1st Study","2nd Study","3rd Study","4th Study","5th Study"),
                  step.size =  0.03571,
                  Treatment=  c(0,1),
                  name.of.Continuous = "BMI",
                  ranges.of.Continuous =  c( 18.5,   27, 
                                             21.25,   30.25,
                                             24.50,   33.50,
                                             27.75,   36.75, 
                                             31,   40))



## We create a pseudo-variable that will help us create the curves we described in the paper
df3$BMI.standardised =  with(df3, 2*(BMI-25)/40)

## And add a column to generate the true underlying mortality risk 
df3$`Mortality risk` = NA



#### Random parameters that shift the regression lines across studies creating heterogeneity. 
set.seed(32)
shift =  round(runif(5, -0.05,0.05),2)
shift2 = round(runif(5, -0.05,0.05),2)



df3$Study.shift.intercept =  df3$Study 
df3$Study.shift.slope =  df3$Study


df3=df3%>%
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
df3$`Mortality risk` =  with(df3, 0.2+ Study.shift.intercept + BMI.standardised^2+ BMI.standardised^4*Treatment + Study.shift.slope*Treatment -  BMI.standardised^2* Treatment)



### Create the binary outcome given the mortality risks calculated above
set.seed(59663)
df3$Y <- rbinom(dim(df3)[1],1,df3$`Mortality risk`) 

### Make treatment variable a factor with two levels "Control" and "Treated" 
df3$Treatment <- factor(df3$Treatment , levels = c(0,1), labels = c("Control","Treated"))


### Remove unnecessary objects
rm(shift, shift2, data.creator)
