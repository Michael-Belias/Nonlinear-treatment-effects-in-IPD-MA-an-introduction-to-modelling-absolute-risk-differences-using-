

##### Second scenario (Non-heterogeneous regression lines with unequal BMI ranges)
##### Across studies the regression lines the same while the ranges of BMI are different

source("Assisting functions/IPD-MA_Data_creator.R")


df2= data.creator(name.of.Clustering = "Study",
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
df2$BMI.standardised =  with(df2, 2*(BMI-25)/40)

## And add a column to generate the true underlying mortality risk 
df2$`Mortality risk` = NA

### Generate the mortality risk as function of BMI
df2$`Mortality risk` =  with(df2, 0.2+ BMI.standardised^2 + BMI.standardised^4*Treatment-  BMI.standardised^2* Treatment)

### Create the binary outcome given the mortality risks calculated above
set.seed(32) # 59663
df2$Y <- rbinom(dim(df2)[1],1,df2$`Mortality risk`) 

### Make treatment variable a factor with two levels "Control" and "Treated" 
df2$Treatment <- factor(df2$Treatment , levels = c(0,1), labels = c("Control","Treated"))



### Remove unnecessary objects
rm(data.creator)