## ----Simulation of third IPD-MA scenario --------------------------------------------------------------------------------------

## Load tidyverse for data manipulation
if(!require("tidyverse")) install.packages("tidyverse")



data.creator = function(name.of.Clustering,
                        name.of.Treatment ,
                        number.of.studies, 
                        values.of.clustering.variable,
                        step.size,
                        name.of.Continuous ,
                        Treatment,
                        ranges.of.Continuous 
){
  
  
  min.max = as.data.frame(matrix(ranges.of.Continuous, ncol = 2, 
                                 nrow = number.of.studies,
                                 byrow = T, 
                                 dimnames = list(c(1:5),c("Min","Max"))))
  
  min.max = cbind(min.max,values.of.clustering.variable)
  
  colnames(min.max) =  c("Min","Max",name.of.Clustering)
  
  
  Cont = vector()
  study = vector()
  
  for(i in min.max[,name.of.Clustering]){
    
    
    mindata= min.max%>%
      filter(!!as.name(name.of.Clustering) == i)
    
    Cont.to.add = rep(seq( from  = mindata$Min,to =  mindata$Max,by =  step.size),each =2)
    Cont= c(Cont,Cont.to.add)
    l= length(rep(seq( from  = mindata$Min,to =  mindata$Max, by =  step.size),2))
    study = c(study,rep(i,l ))
    
  }
  
  new.dat = data.frame(study,Treatment = rep(x = Treatment,length(Cont)/2),Cont)
  new.dat =  new.dat[order(new.dat$Cont),]
  colnames(new.dat) =  c(name.of.Clustering, name.of.Treatment,name.of.Continuous)
  
  return(new.dat)
  
  
}





##### Third scenario (Heterogeneous regression lines with unequal BMI ranges)
##### Both the regression lines and the ranges of BMI are different across studies 



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
set.seed(32) ## 59663
df3$Y <- rbinom(dim(df3)[1],1,df3$`Mortality risk`) 

### Make treatment variable a factor with two levels "Control" and "Treated" 
df3$Treatment <- factor(df3$Treatment , levels = c(0,1), labels = c("Control","Treated"))


### Remove unnecessary objects
rm(shift, shift2, data.creator)







## ----Pointwise meta-analysis---------------------------------------------------------------------------------



## Fit a RCS model per study
RCS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5),
                 knots = list(BMI = quantile(.$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Fit a B-splines model per study
BS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,m=c(2,0),k = 5),
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Fit a P-splines model per study
PS.Comb= df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="ps",fx=F,by = Treatment,k=17), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Fit a Smoothing splines model per study
SS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="tp",fx=F,by = Treatment), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))


## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 250),each = 10),
                    Treatment = rep(unique(df3$Treatment),1250), 
                    Study =  rep(rep(unique(df3$Study),each =2),250))


### Calculate the predicted outcomes per study 

### RCS
predictions.RCS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

### B-splines
predictions.BS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

### P-splines
predictions.PS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

### Smoothing splines
predictions.SS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

## Second stage (pooling the regression lines per X*)


#### code for running the point-wise meta-analysis
#### The following is an assisting function that performs a poitwise meta-analysis per X
#### Note that in order to calculate the predicted outcome correctly you need to have matching BMI values in at least 2 Studies.
####
#### The output is a data-set with the results of 3 meta-analyses (Fixed effects, Random effects and Random effects with HKSJ correction)
#### You can choose based on your assumptions which assumption you wish to apply for your meta-analysis.




pointwise.ma = function(data, ### Give the data-set with the predicted outcomes
                        clustering.variable = "Study",              ### Provide the name of the clustering variable 
                        combining.variables=  NULL,                 ### The names of the predictors which we wish to match
                        predicted.outcome =  NULL,                  ### Provide the name of the predicted outcome variable
                        predicted.outcome.se = NULL,                ### Provide the name of the predicted outcome standard errors or 
                        predicted.outcome.CI = c("lower", "upper"), ### Provide the name of the predicted outcome confidence intervals
                        tau.method = "EB" ){                        ### Provide the pooling method we wish to apply 
  
  
  split= data%>%
    group_by(!!as.name(clustering.variable) )%>%
    group_split()
  
  data <- merge(merge(merge(merge(
    split[[1L]],
    split[[2L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(1,2)),
    split[[3L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(2,3)),
    split[[4L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(3,4)),
    split[[5L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(4,5))
  
  
  data$FE.meta =  NA
  data$FE.meta.upper =  NA
  data$FE.meta.lower =  NA
  data$FE.se =  NA
  
  
  data$RE.meta =  NA
  data$RE.meta.upper =  NA
  data$RE.meta.lower =  NA
  data$RE.se =  NA
  
  data$RE.meta.HKSJ =  NA
  data$RE.meta.HKSJ.upper =  NA
  data$RE.meta.HKSJ.lower =  NA
  data$RE.HKSJ.se =  NA
  
  
  data$Q =  NA
  data$H = NA
  
  data$H.HKSJ =  NA
  data$Q.HKSJ =  NA
  
  
  data$tau =  NA
  data$pval.Q =NA
  
  
  
  
  for(i in 1:dim(data)[1]){
    
    TE= data[i,]%>%
      select(contains(predicted.outcome))%>%
      t()%>%na.omit()%>%
      as.vector()
    
    if(!is.null(predicted.outcome.se)){
      seTE= data[i,]%>%
        select(contains(predicted.outcome.se))%>%
        t()%>%na.omit()%>%
        as.vector()
      
      meta1= metagen(TE= TE, seTE =seTE, method.tau = tau.method,hakn = T, adhoc.hakn = "ci" )
      meta2= metagen(TE= TE, seTE =seTE, method.tau = tau.method,hakn = F )
      
    }else if(!is.null(predicted.outcome.CI)){
      lower =  data[i,]%>%
        select(contains(predicted.outcome.CI[1]))%>%
        t()%>%na.omit()%>%
        as.vector()
      
      upper =  data[i,]%>%
        select(contains(predicted.outcome.CI[2]))%>%
        t()%>%na.omit()%>%
        as.vector()
      
      meta1= metagen(TE= TE, lower = lower, upper = upper, method.tau = tau.method,hakn = T, adhoc.hakn = "ci" )
      meta2= metagen(TE= TE, lower = lower, upper = upper, method.tau = tau.method,hakn = F )
      
    }
    
    ### Store the Heterogeneity measures
    data[i,]$Q =  meta2$Q
    data[i,]$Q.HKSJ =  meta1$Q
    data[i,]$H =  meta2$H
    data[i,]$H.HKSJ =  meta1$H
    
    ### Store the Fixed-effects model results
    data[i,]$FE.meta =  meta2$TE.fixed
    data[i,]$FE.meta.upper =  meta2$upper.fixed
    data[i,]$FE.meta.lower =  meta2$lower.fixed
    data[i,]$FE.se =  meta1$seTE.fixed
    
    data[i,]$tau =  meta2$tau2
    data[i,]$pval.Q =  meta2$pval.Q
    
    ### Store the Random-effects model results
    data[i,]$RE.meta =  meta2$TE.random
    data[i,]$RE.meta.upper =  meta2$upper.random
    data[i,]$RE.meta.lower =  meta2$lower.random
    data[i,]$RE.se =  meta2$seTE.random
    
    ### Store the Random-effects model with HKSJ results
    data[i,]$RE.meta.HKSJ =  meta1$TE.random
    data[i,]$RE.meta.HKSJ.upper =  meta1$upper.random
    data[i,]$RE.meta.HKSJ.lower =  meta1$lower.random
    data[i,]$RE.HKSJ.se =  meta1$seTE.random
    
    
    if(round(i*100/dim(data)[1]) != round((i- 1)*100/dim(data)[1] )){
      cat("\014") 
      print(paste(round(i*100/dim(data)[1]),"%",sep = ""))
    }
    
  }
  
  
  return(data)
}                     



### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


### RCS
point.wise.DF.RCS.Comb = pointwise.ma(data = predictions.RCS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.RCS.Comb$RE.meta =  expit(point.wise.DF.RCS.Comb$RE.meta )
point.wise.DF.RCS.Comb$RE.meta.upper =  expit(point.wise.DF.RCS.Comb$RE.meta.upper )
point.wise.DF.RCS.Comb$RE.meta.lower =  expit(point.wise.DF.RCS.Comb$RE.meta.lower )

### B-splines
point.wise.DF.BS.Comb =  pointwise.ma(predictions.BS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML") 

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.BS.Comb$RE.meta  =  expit(point.wise.DF.BS.Comb$RE.meta )
point.wise.DF.BS.Comb$RE.meta.upper  =  expit(point.wise.DF.BS.Comb$RE.meta.upper )
point.wise.DF.BS.Comb$RE.meta.lower  =  expit(point.wise.DF.BS.Comb$RE.meta.lower)

### P-splines
point.wise.DF.PS.Comb =  pointwise.ma(predictions.PS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.PS.Comb$RE.meta  =  expit(point.wise.DF.PS.Comb$RE.meta )
point.wise.DF.PS.Comb$RE.meta.lower  =  expit(point.wise.DF.PS.Comb$RE.meta.lower)
point.wise.DF.PS.Comb$RE.meta.upper  =  expit(point.wise.DF.PS.Comb$RE.meta.upper )

### Smoothing splines
point.wise.DF.SS.Comb =  pointwise.ma(predictions.SS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML")


## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.SS.Comb$RE.meta.upper  =  expit(point.wise.DF.SS.Comb$RE.meta.upper )
point.wise.DF.SS.Comb$RE.meta  =  expit(point.wise.DF.SS.Comb$RE.meta )
point.wise.DF.SS.Comb$RE.meta.lower  =  expit(point.wise.DF.SS.Comb$RE.meta.lower)


### Draw the corresponding plots

### RCS
point.wise.DF.RCS.Comb.plot = point.wise.DF.RCS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.9, size = 10, label = "a") +ylim(c(0,1))


## B-splines
point.wise.DF.BS.Comb.plot = point.wise.DF.BS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.9, size = 10, label = "b") +ylim(c(0,1))

## P-splines
point.wise.DF.PS.Comb.plot = point.wise.DF.PS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.9, size = 10,label = "c") +ylim(c(0,1))


## Smoothing splines
point.wise.DF.SS.Comb.plot = point.wise.DF.SS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))



## ----Absolute risk differences ---------------------------------------------------------------------------------
### The right way to perform a pointwise meta-analysis is first to estimate the risk differences per Study and BMI value and then pool them. 

### First stage
### RCS

predictions.RCS.Comb= predictions.RCS.Comb %>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### B-splines
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.BS.Comb=predictions.BS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### P-splines
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.PS.Comb=predictions.PS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))     

### Smoothing splines
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.SS.Comb=predictions.SS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI)) 



### Load assisting function to calculate absolute risk differences and their SEs
### The following function will calculate the risk differences between interventions



### The output is a data.frame with the risk differences

risk.diff.creator =  function(dataframe = NULL,          ### Provide the data-set with the predicted outcomes
                              treatment = "treat",       ### Provide the name of the intervention variable
                              matching.variables=  NULL, ### Provide the names of any other predictors 
                              outcome= NULL,             ### Provide the name of the outcome 
                              predicted.outcome = NULL,  ### Provide the name of the predicted outcome
                              predicted.CI = NULL){      ### Provide the name of the confidence intervals of the predicted outcome
  
  
  
  
  dataframe = dataframe[,c(outcome,treatment,matching.variables, predicted.outcome, predicted.CI)]
  
  
  
  split= dataframe%>%
    group_by(!!as.name(treatment) )%>%
    group_split()
  
  
  split.df= full_join(x = split[[1]],y = split[[2]], by= matching.variables  )
  
  
  
  p1 =  as.matrix(split.df[, paste(predicted.outcome,"y",sep = ".")])
  p2 =  as.matrix(split.df[, paste(predicted.outcome,"x",sep = ".")])
  l1 =  as.matrix(split.df[, paste(predicted.CI[1],"y",sep = ".")])
  l2 =  as.matrix(split.df[, paste(predicted.CI[1],"x",sep = ".")])
  u1 =  as.matrix(split.df[, paste(predicted.CI[2],"y",sep = ".")])
  u2 =  as.matrix(split.df[, paste(predicted.CI[2],"x",sep = ".")])
  
  split.df$fit.diff = p1 - p2 
  
  split.df$diff.lower =  split.df$fit.diff - sqrt((p1-l1)^2 + (u2-p2)^2)
  
  split.df$diff.upper =  split.df$fit.diff +  sqrt((p2-l2)^2 + (u1-p1)^2)
  
  split.df= as.data.frame(split.df)
  return(split.df)
  
  
}







### RCS

absolute_diff_RCS.Comb = risk.diff.creator(dataframe = predictions.RCS.Comb,
                                           treatment = "Treatment", 
                                           outcome = NULL,
                                           matching.variables = c("BMI","Study"),
                                           predicted.outcome = "fit", 
                                           predicted.CI = c("Lower","Upper"))

### B-splines

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_BS.Comb = risk.diff.creator(dataframe = predictions.BS.Comb,
                                          treatment = "Treatment",outcome = NULL, 
                                          matching.variables = c("BMI","Study"),
                                          predicted.outcome = "fit", 
                                          predicted.CI = c("Lower","Upper"))


### P-splines

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_PS.Comb = risk.diff.creator(dataframe = predictions.PS.Comb,
                                          treatment = "Treatment", outcome = NULL,
                                          matching.variables = c("BMI","Study"),
                                          predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


### Smoothing splines

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_SS.Comb = risk.diff.creator(dataframe = predictions.SS.Comb,
                                          treatment = "Treatment", outcome = NULL,
                                          matching.variables = c("BMI","Study"),
                                          predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))




### Clean the data above. 
absolute_diff_RCS.Comb=  absolute_diff_RCS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_BS.Comb=  absolute_diff_BS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_PS.Comb=  absolute_diff_PS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_SS.Comb=  absolute_diff_SS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)



### Second stage (pooling absolute risk differences per X*)
### RCS

point.wise.absolute_diff_RCS.Comb =  pointwise.ma(data = absolute_diff_RCS.Comb,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_RCS.Comb =  point.wise.absolute_diff_RCS.Comb%>%
  mutate(BMI =  as.numeric(BMI))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_BS.Comb  =  pointwise.ma(data = absolute_diff_BS.Comb ,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper")
)

point.wise.absolute_diff_BS.Comb =  point.wise.absolute_diff_BS.Comb%>%
  mutate(BMI =  as.numeric(BMI))


### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_PS.Comb  =  pointwise.ma(data = absolute_diff_PS.Comb ,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper")
)

point.wise.absolute_diff_PS.Comb =  point.wise.absolute_diff_PS.Comb%>%
  mutate(BMI =  as.numeric(BMI))






### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_SS.Comb  =  pointwise.ma(data = absolute_diff_SS.Comb ,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper")
)

point.wise.absolute_diff_SS.Comb =  point.wise.absolute_diff_SS.Comb%>%
  mutate(BMI =  as.numeric(BMI))





point.wise.DF.RCS.Comb.diff.plot = point.wise.absolute_diff_RCS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "a")+ ylim(c(-0.8,0.3))


point.wise.DF.BS.Comb.diff.plot=point.wise.absolute_diff_BS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "b")+ ylim(c(-0.8,0.3))

point.wise.DF.PS.Comb.diff.plot=point.wise.absolute_diff_PS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10,label = "c")+ ylim(c(-0.8,0.3))



point.wise.DF.SS.Comb.diff.plot=point.wise.absolute_diff_SS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "d")+ ylim(c(-0.8,0.3))







### MVmeta
### Clear enviroment 
rm(list=ls()[! ls() %in% c("df3","expit")])


## In MVmeta we use the same parametrisation in all studies. 
## Some positions of knots, some degree of the basis functions etc. 
## However, some studies there may be values missing. 

df3%>%
  group_by(Study)%>%
  summarise(range(BMI))


## For instance, as you can see in our simulated data-set the range of BMI in Study 1 is [18.5,27] while in Study 5 [31,40]
## To avoid errors we may need to create pseudo-data on the boundaries and give them very small weight to avoid biased regression curves. 

## We introduce the weight variable 
## In the original data-set the weight will be 1
df3$weight =  1


## We save into objects the values near the overall boundaries 18.5 and 40 
## Note that how close we wish to be on the boundaries is arbitrary. 

lower=df3[df3$BMI<19,] ;   n.lower =  dim(lower)[1]
upper=df3[df3$BMI >39.5,]; n.upper =  dim(upper)[1]


## We produce 4 copies of the lower and upper data-sets and give them very small weights.

rep.lower=  do.call(rbind, replicate(4, lower, simplify = FALSE)) ;  rep.lower$weight= 0.0000000000001
rep.upper=  do.call(rbind, replicate(4, upper, simplify = FALSE)) ;  rep.upper$weight= 0.0000000000001

## As you can see the lower and upper data-sets use the original study information
head(rep.lower); head(rep.upper)
## So we change the names of the studies within the data-sets. 
rep.lower$Study = rep(unique(df3$Study)[-1], each= n.lower)
rep.upper$Study = rep(unique(df3$Study)[-5], each= n.upper)

## And we add them to the original data-set.


df3.with.pseudo =  rbind(df3, rep.lower, rep.upper)

### RCS  
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df3 =   list(BMI=quantile(df3$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))) 

## The formula for all Studies is the same so we save it 

formula.RCS = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5)

## Number of studies

nstudies.RCS.df3 = length(unique(df3$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df3 = gam( formula =formula.RCS,
                   knots = Knots.RCS.df3,
                   family = binomial("logit"), 
                   data = df3)

### Get the model matrices for each data-set

X.p.RCS.df3 =  model.matrix(fit.RCS.df3)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.RCS.df3 = matrix(NA,
                                        ncol = length(fit.RCS.df3$coefficients),nrow=nstudies.RCS.df3,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.RCS.df3)),1, paste, collapse=" "),
                                                         c(1:length(fit.RCS.df3$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.RCS.df3 = matrix(NA, ncol=sum(c(1:length(fit.RCS.df3$coefficients))), nrow = nstudies.RCS.df3 )

k=3
j=1

for( i in unique(df3.with.pseudo$Study)){
  
  ### Get a mini df3 with only one study
  minidf3 = df3.with.pseudo%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.RCS ,knots = Knots.RCS.df3, family = binomial("logit"),weights = weight, data = minidf3)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df3[j,] = fit$coefficients
  S.RCS.df3[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  #rm(i,minidf3,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.RCS.df3 = mvmeta(estimated.coefficients.RCS.df3, S.RCS.df3)


prediction.interval.mvmeta.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3)

prediction.interval.mvmeta.lower.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3) - sqrt( rowSums(X.p.RCS.df3  * (X.p.RCS.df3  %*% vcov(mv.fit.RCS.df3)))) * qt(0.025, dim(df3)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3) + sqrt( rowSums(X.p.RCS.df3  * (X.p.RCS.df3  %*% vcov(mv.fit.RCS.df3)))) * qt(0.025, dim(df3)[1] - 3)


mvmeta.df3.RCS = cbind(df3[,c("Study","BMI","Treatment")],
                       fit =  expit(prediction.interval.mvmeta.RCS.df3), 
                       Lower= expit(prediction.interval.mvmeta.lower.RCS.df3),
                       Upper =expit(prediction.interval.mvmeta.upper.RCS.df3 ))





g.mvmeta.total.RCS.Comb = ggplot(mvmeta.df3.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30), 
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS.Comb


### B-splines
###

## The formula for all studies is the same so we save it 

Knots.BS.df3 =  list(BMI =  c(11.29786, 14.88818, 18.5, 21.2, 24.5, 27.0, 30.2, 33.5, 36.7, 40.0,  43.61074, 47.20106))
formula.BS = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,k = 9,m=c(2,0))

## Number of studies

nstudies.BS.df3 = length(unique(df3$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.BS.df3 = gam( formula =formula.BS,knots=Knots.BS.df3,
                  family = binomial("logit"), 
                  data = df3)

### Get the model matrices for each data-set

X.p.BS.df3 =  model.matrix(fit.BS.df3)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.BS.df3 = matrix(NA,
                                       ncol = length(fit.BS.df3$coefficients),nrow=nstudies.BS.df3,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.BS.df3)),1, paste, collapse=" "),
                                                        c(1:length(fit.BS.df3$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.BS.df3 = matrix(NA, ncol=sum(c(1:length(fit.BS.df3$coefficients))), nrow = nstudies.BS.df3 )

k=3
j=1

for( i in unique(df3.with.pseudo$Study)){
  
  ### Get a mini df3 with only one study
  minidf3 = df3.with.pseudo%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.BS , knots = Knots.BS.df3,
            family = binomial("logit"), data = minidf3)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.BS.df3[j,] = fit$coefficients
  S.BS.df3[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  rm(i,minidf3,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.BS.df3 = mvmeta(estimated.coefficients.BS.df3, S.BS.df3)


prediction.interval.mvmeta.BS.df3 =  X.p.BS.df3 %*% coef(mv.fit.BS.df3)

prediction.interval.mvmeta.lower.BS.df3 =  X.p.BS.df3 %*% coef(mv.fit.BS.df3) - sqrt( rowSums(X.p.BS.df3  * (X.p.BS.df3  %*% vcov(mv.fit.BS.df3)))) * qt(0.025, dim(df3)[1] - 3)

prediction.interval.mvmeta.upper.BS.df3 =  X.p.BS.df3 %*% coef(mv.fit.BS.df3) + sqrt( rowSums(X.p.BS.df3  * (X.p.BS.df3  %*% vcov(mv.fit.BS.df3)))) * qt(0.025, dim(df3)[1] - 3)


mvmeta.df3.BS = cbind(df3[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.BS.df3), 
                      Lower= expit(prediction.interval.mvmeta.lower.BS.df3),
                      Upper =expit(prediction.interval.mvmeta.upper.BS.df3 ))





g.mvmeta.total.BS.Comb = ggplot(mvmeta.df3.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30), 
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " b", size= 12 ) 


g.mvmeta.total.BS.Comb


p1=  ggplot(mvmeta.df3.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30), 
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom") + 
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))



legend = gtable_filter(ggplotGrob(p1), "guide-box") 



library(grid)
MV_meta_plot_Comb = grid.arrange(arrangeGrob(g.mvmeta.total.RCS.Comb,g.mvmeta.total.BS.Comb,
                                             ncol = 2, 
                                             right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                             bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32))),
                                 legend, heights=  c(10,1))




## ----GAMM --------------------------------------------------------------------------------------


rm(list=ls()[! ls() %in% c("df3","expit")]) ### To clear all environment besides the data-set
Knots= list (BMI = (quantile(df3$BMI , probs = c(0.05,0.275,0.5,0.725,0.95))))




fit.RCS.Combined = gam(Y~  BMI+ Treatment+ 
                         s(BMI,by = Treatment,bs="cr",fx = T, k = 5) +  
                         s(Study,bs = "re") +  
                         s(Study,BMI,bs = "re")+  
                         s(Study,Treatment,bs = "re"),knots = Knots,
                       family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.BS.Combined = gam(Y~  BMI+ Treatment+ 
                        s(BMI,by = Treatment,fx = T,bs="bs",k=5, m=c(2,0)) +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.PS.Combined = gam(Y~  BMI+ Treatment+ 
                        s(BMI,by = Treatment,bs="ps", k = 19) +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.SS.Combined = gam(Y~  BMI+ Treatment+ 
                        s(BMI,by = Treatment,bs="tp") +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")

new.data = data.frame(cbind(df3[,c("Study","BMI","Treatment")]))



preds.RCS.Combined=  predict.gam(fit.RCS.Combined, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%as.data.frame()%>%cbind(new.data)

preds.BS.Combined=  predict.gam(fit.BS.Combined, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%as.data.frame()%>%cbind(new.data)


preds.PS.Combined=  predict.gam(fit.PS.Combined, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%as.data.frame()%>%cbind(new.data)


preds.SS.Combined=  predict.gam(fit.SS.Combined, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%as.data.frame()%>%cbind(new.data)



preds.RCS.Combined$lower = preds.RCS.Combined$fit -1.96*preds.RCS.Combined$se.fit
preds.BS.Combined$lower = preds.BS.Combined$fit   -1.96*preds.BS.Combined$se.fit
preds.PS.Combined$lower = preds.PS.Combined$fit   -1.96*preds.PS.Combined$se.fit
preds.SS.Combined$lower = preds.SS.Combined$fit   -1.96*preds.SS.Combined$se.fit



preds.RCS.Combined$upper = preds.RCS.Combined$fit +1.96*preds.RCS.Combined$se.fit
preds.BS.Combined$upper = preds.BS.Combined$fit   +1.96*preds.BS.Combined$se.fit
preds.PS.Combined$upper = preds.PS.Combined$fit   +1.96*preds.PS.Combined$se.fit
preds.SS.Combined$upper = preds.SS.Combined$fit   +1.96*preds.SS.Combined$se.fit

g.GAMM.RCS.Combined=ggplot(preds.RCS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "a") +ylim(c(0,1))


g.GAMM.BS.Combined=ggplot(preds.BS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "b") +ylim(c(0,1))


g.GAMM.PS.Combined=ggplot(preds.PS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.8, size = 10,label = "c") +ylim(c(0,1))


g.GAMM.SS.Combined=ggplot(preds.SS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "d") +ylim(c(0,1))



g.GAMM.Combined = grid.arrange(g.GAMM.RCS.Combined,g.GAMM.BS.Combined,g.GAMM.PS.Combined,g.GAMM.SS.Combined, ncol=4, right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)))



preds.RCS.Combined= preds.RCS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.BS.Combined = preds.BS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.PS.Combined=preds.PS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.SS.Combined=preds.SS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))

## ----GAMMs Treatment effect plots------------------------------------------------------------------------------------------------------

### Load assisting function to calculate absolute risk differences and their SEs
### The following function will calculate the risk differences between interventions



### The output is a data.frame with the risk differences

risk.diff.creator =  function(dataframe = NULL,          ### Provide the data-set with the predicted outcomes
                              treatment = "treat",       ### Provide the name of the intervention variable
                              matching.variables=  NULL, ### Provide the names of any other predictors 
                              outcome= NULL,             ### Provide the name of the outcome 
                              predicted.outcome = NULL,  ### Provide the name of the predicted outcome
                              predicted.CI = NULL){      ### Provide the name of the confidence intervals of the predicted outcome
  
  
  
  
  dataframe = dataframe[,c(outcome,treatment,matching.variables, predicted.outcome, predicted.CI)]
  
  
  
  split= dataframe%>%
    group_by(!!as.name(treatment) )%>%
    group_split()
  
  
  split.df= full_join(x = split[[1]],y = split[[2]], by= matching.variables  )
  
  
  
  p1 =  as.matrix(split.df[, paste(predicted.outcome,"y",sep = ".")])
  p2 =  as.matrix(split.df[, paste(predicted.outcome,"x",sep = ".")])
  l1 =  as.matrix(split.df[, paste(predicted.CI[1],"y",sep = ".")])
  l2 =  as.matrix(split.df[, paste(predicted.CI[1],"x",sep = ".")])
  u1 =  as.matrix(split.df[, paste(predicted.CI[2],"y",sep = ".")])
  u2 =  as.matrix(split.df[, paste(predicted.CI[2],"x",sep = ".")])
  
  split.df$fit.diff = p1 - p2 
  
  split.df$diff.lower =  split.df$fit.diff - sqrt((p1-l1)^2 + (u2-p2)^2)
  
  split.df$diff.upper =  split.df$fit.diff +  sqrt((p2-l2)^2 + (u1-p1)^2)
  
  split.df= as.data.frame(split.df)
  return(split.df)
  
  
}


absolute_diff_RCS.Comb = risk.diff.creator(dataframe = preds.RCS.Combined,
                                           treatment = "Treatment", 
                                           outcome = NULL,
                                           matching.variables = c("BMI"),
                                           predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))




absolute_diff_BS.Comb = risk.diff.creator(dataframe = preds.BS.Combined,
                                          treatment = "Treatment",outcome = NULL, 
                                          matching.variables = c("BMI"),
                                          predicted.outcome = "fit", 
                                          predicted.CI = c("Lower","Upper"))

absolute_diff_PS.Comb = risk.diff.creator(dataframe = preds.PS.Combined,
                                          treatment = "Treatment", outcome = NULL,
                                          matching.variables = c("BMI"),
                                          predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


absolute_diff_SS.Comb = risk.diff.creator(dataframe = preds.SS.Combined,
                                          treatment = "Treatment", outcome = NULL,
                                          matching.variables = c("BMI"),
                                          predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



absolute_diff_RCS.Comb=  absolute_diff_RCS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_BS.Comb=  absolute_diff_BS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_PS.Comb=  absolute_diff_PS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_SS.Comb=  absolute_diff_SS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)




GAMM.DF.RCS.Comb.diff.plot = absolute_diff_RCS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "a")+ ylim(c(-0.8,0.3))




GAMM.DF.BS.Comb.diff.plot=absolute_diff_BS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "b")+ ylim(c(-0.8,0.3))


GAMM.DF.PS.Comb.diff.plot=absolute_diff_PS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10,label = "c")+ ylim(c(-0.8,0.3))


GAMM.DF.SS.Comb.diff.plot=absolute_diff_SS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "d")+ ylim(c(-0.8,0.3))





