library(rms)
library(splines)
library(mvmeta)
library(meta)
library(ggplot2)
library(ggsci)
library(zoo)
library(gridExtra)
library(broom)
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")
#Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}

#rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])

## We follow White et al. and Riley et al. recommendations and we performed data augmentation
## In our simulated data-set the range of BMI in Study 1 is [18.5,27] while in Study 5 [31,40]
## To avoid errors we create pseudo-data on the boundaries and give them very small weight to 
## avoid biased regression curves. 

## We introduce the weight variable in our data-set 
## In the original data-set the weight will be 1, while in the augmented data-set 0.000000001
df2$weight =  1



######This is a test data-augmentation procedure


augmentation.df2 =  as.data.frame(with(df2, expand.grid(unique(Study),unique(BMI), unique(Treatment), unique(Y)))); 
colnames(augmentation.df2) = c("Study","BMI", "Treatment", "Y") ;  augmentation.df2$weight = 0.000001

df2 =  rbind(augmentation.df2, df2[ ,colnames(df2) %in% colnames(augmentation.df2)])



### This is the ordinary data-augmentation
## We save into objects the values near the overall boundaries 18.5 and 40 
## Note that how close we wish to be on the boundaries is arbitrary. 

lower=df2[df2$BMI<19,] ;   n.lower =  dim(lower)[1]; lower$BMI = 18.5
upper=df2[df2$BMI >39.5,]; n.upper =  dim(upper)[1]; upper$BMI = 40


## We produce 4 copies of the lower and upper data-sets and give them very small weights.

rep.lower=  do.call(rbind, replicate(4, lower, simplify = FALSE)) ;  rep.lower$weight= 0.0000001
rep.upper=  do.call(rbind, replicate(4, upper, simplify = FALSE)) ;  rep.upper$weight= 0.0000001

## As you can see the lower and upper data-sets use the original study information
head(rep.lower); head(rep.upper)
## So we change the names of the studies within the data-sets. 

rep.lower$Study = rep(unique(df2$Study)[-1], each= n.lower)
rep.upper$Study = rep(unique(df2$Study)[-5], each= n.upper)

## And we add them to the original data-set.


df2 =  rbind(df2, rep.lower, rep.upper)
## Now each study has values near the boundaries of BMI [18.5,40]

### remove the object we don't need

rm(lower,upper, rep.lower,rep.upper,n.lower, n.upper)


### Get the knots from the overall model to use the same per study
Knots.ns.df2 =  c(25.66667, 32.83333)  ### This leads to 8 df
Knots.rcs.df2=  quantile(df2[df2$weight==1,]$BMI, probs = c(0.1,0.5,0.9)) ### This leads to 8 df
Knots.bs.df2 =  c(29.25) ### This leads to 10 df, but I can't place less





### %>% is a pipeline function 
### each line uses the result of the previous pipe 
### 


## Fit a RCS model per study
RCS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>% 
  do(model = glm(formula = Y~ Treatment + rcs(BMI,knots= Knots.rcs.df2 )*Treatment, ### For each study we fit RCS 
                 family = binomial("logit"), 
                 weights = weight,
                 data = .))

## Fit a NS model per study
NS.DR = df2%>%                                ### We call the data-set of the first scenario
  arrange(desc(Study))%>%                      ### Order the date based on study                
  group_by(Study) %>%                          ### Group them based on study   
  do(model = glm(Y~ Treatment + 
                   ns(BMI, knots = Knots.ns.df2)*Treatment, ### For each study we fit RCS 
                 family = binomial("logit"),   ### Note that in pointwise meta-analysis one can use different modelling techniques per study
                 weights = weight,
                 data = .                     ### different models per study 
  ))    



## Fit a B-spline model per study
BS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = glm(formula = Y~ Treatment + 
                   bs(BMI,knots = Knots.bs.df2)*Treatment,  ### For each study we fit B-splines 
                 family = binomial("logit"), weights = weight, 
                 data = .))




### Extract the coefficients and variance covariance matrix

### RCS coefficients 
## Extract the RCS coefficients
coefficients.RCS.DR= t(sapply(RCS.DR$model,FUN = coefficients))

## Extract the Vcov lower triangular
Vcov.RCS.DR = t(sapply(RCS.DR$model,function(x)  vcov(x)[lower.tri(vcov(x), diag = T)]))

### RCS mv-meta
mvmeta.RCS.DR = mvmeta(coefficients.RCS.DR, Vcov.RCS.DR, control= list(maxiter=1000))    ### Error in chol.default(X[[i]], ...) : the leading minor of order 20 is not positive definite


### NS coefficients 
## Extract the NS coefficients
coefficients.NS.DR= t(sapply(NS.DR$model,FUN = coefficients))

## Extract the Vcov lower triangular
Vcov.NS.DR = t(sapply(NS.DR$model,function(x)  vcov(x)[lower.tri(vcov(x), diag = T)]))

### NS mv-meta
mvmeta.NS.DR = mvmeta(coefficients.NS.DR, Vcov.NS.DR) ### Suprising NO ERROR



### BS coefficients 
## Extract the BS coefficients
coefficients.BS.DR= t(sapply(BS.DR$model,FUN = coefficients))

## Extract the Vcov lower triangular
Vcov.BS.DR = t(sapply(BS.DR$model,function(x)  vcov(x)[lower.tri(vcov(x), diag = T)]))

### BS mv-meta
mvmeta.BS.DR = mvmeta(coefficients.BS.DR, Vcov.BS.DR) ## ERROR


## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 50),each = 2),
                    Treatment = rep(unique(df2$Treatment),50), 
                    Y =  rep(0:1,50))



### create the model matrices to cross-validate that we are multiplying the correct transformations of X


### RCS
model.matrix.RCS.DR = model.matrix(glm(formula = formula.RCS, 
                                                     family = binomial("logit"), 
                                                     data = new.dat))



model.matrices.RCS.DR = as.data.frame(model.matrix.RCS.DR)%>%
  group_by(TreatmentTreated)%>%
  do(plots=autoplot(zoo(.), facets=NULL)+ ggtitle(.$TreatmentTreated)+theme_bw())



grid.arrange(model.matrices.RCS.DR$plots[[1]], model.matrices.RCS.DR$plots[[2]])



### NS B-splines
model.matrix.NS.DR = model.matrix(glm(formula = Y~ Treatment + 
                                                      ns(BMI,knots = Knots.ns.df2)*Treatment,   
                                                    family = binomial("logit"), 
                                                    data = new.dat))  



model.matrices = as.data.frame(model.matrix.NS.DR)%>%
  group_by(TreatmentTreated)%>%
  do(plots=autoplot(zoo(.), facets=NULL)+ ggtitle(.$TreatmentTreated)+theme_bw())



grid.arrange(model.matrices$plots[[1]], model.matrices$plots[[2]])


### B-splines
model.matrix.BS.DR = model.matrix(glm(formula = Y~ Treatment + 
                                                      bs(BMI,knots = Knots.bs.df2)*Treatment,  
                                                    family = binomial("logit"), 
                                                    data = new.dat))  


model.matrices = as.data.frame(model.matrix.BS.DR)%>%
  group_by(TreatmentTreated)%>%
  do(plots=autoplot(zoo(.), facets=NULL)+ ggtitle(.$TreatmentTreated)+theme_bw())



grid.arrange(model.matrices$plots[[1]], model.matrices$plots[[2]])



### RCS (didn't converge)
predicted.curves.RCS.DR = data.frame(
  fit =  model.matrix.RCS.DR %*% coef(mvmeta.RCS.DR),
  fit.lower=  model.matrix.RCS.DR %*% coef(mvmeta.RCS.DR)   ### This is the fitted curve above
  - 1.96* sqrt(rowSums(model.matrix.RCS.DR  * (model.matrix.RCS.DR  %*% vcov(mvmeta.RCS.DR)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  fit.upper=   model.matrix.RCS.DR %*% coef(mvmeta.RCS.DR)   ### This is the fitted curve above
  + 1.96* sqrt(rowSums(model.matrix.RCS.DR  * (model.matrix.RCS.DR  %*% vcov(mvmeta.RCS.DR)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  new.dat
)

### NS
predicted.curves.NS.DR = data.frame(
  fit =  model.matrix.NS.DR %*% coef(mvmeta.NS.DR),
  fit.lower=  model.matrix.NS.DR %*% coef(mvmeta.NS.DR)   ### This is the fitted curve above
  - 1.96* sqrt(rowSums(model.matrix.NS.DR  * (model.matrix.NS.DR  %*% vcov(mvmeta.NS.DR)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  fit.upper=   model.matrix.NS.DR %*% coef(mvmeta.NS.DR)   ### This is the fitted curve above
  + 1.96* sqrt(rowSums(model.matrix.NS.DR  * (model.matrix.NS.DR  %*% vcov(mvmeta.NS.DR)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  new.dat
)

### BS (didn't converge)
predicted.curves.BS.DR = data.frame(
  fit =  model.matrix.BS.DR %*% coef(mvmeta.BS.DR),
  fit.lower=  model.matrix.BS.DR %*% coef(mvmeta.BS.DR)   ### This is the fitted curve above
  - 1.96* sqrt(rowSums(model.matrix.BS.DR  * (model.matrix.BS.DR  %*% vcov(mvmeta.BS.DR)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  fit.upper=   model.matrix.BS.DR %*% coef(mvmeta.BS.DR)   ### This is the fitted curve above
  + 1.96* sqrt(rowSums(model.matrix.BS.DR  * (model.matrix.BS.DR  %*% vcov(mvmeta.BS.DR)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  new.dat
)



### This is a nice looking theme

newtheme <- theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 48,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 34,face = "italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=24, hjust = 0.5),
        axis.text.y = element_text(face="bold",  size=32),
        axis.title.y = element_text(size = 56),
        axis.title.x = element_text(size = 56),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.position = "bottom",
        legend.title=element_blank())



### Plot the mv-meta curves

### RCS (didn't converge)
mvmeta.plot.RCS.DR = ggplot(predicted.curves.RCS.DR, aes(BMI,expit(fit), color= Treatment)) + newtheme+
  geom_line()+ geom_ribbon(aes(ymin= expit(fit.lower), ymax= expit(fit.upper)), alpha= 0.1) + ggtitle("RCS multivariate meta-analysis plot")



### NS
mvmeta.plot.NS.DR =ggplot(predicted.curves.NS.DR, aes(BMI,expit(fit), color= Treatment)) + newtheme+ylim(c(0,1))+
  geom_line()+ geom_ribbon(aes(ymin= expit(fit.lower), ymax= expit(fit.upper)), alpha= 0.1) + 
  ggtitle("NS multivariate meta-analysis plot")

mvmeta.plot.NS.DR

### BS (didn't converge)
mvmeta.plot.BS.DR =ggplot(predicted.curves.BS.DR, aes(BMI,expit(fit), color= Treatment)) + newtheme+
  geom_line()+ geom_ribbon(aes(ymin= expit(fit.lower), ymax= expit(fit.upper)), alpha= 0.1) + ggtitle("BS multivariate meta-analysis plot")

### Didn't converge
grid.arrange(mvmeta.plot.RCS.DR,mvmeta.plot.NS.DR,mvmeta.plot.BS.DR,ncol=2)



#### I am still working on that. Don't RUN  until row 310

### Check the basis functions per study


Model.matrices.RCS.DR.per.study =  sapply(RCS.DR$model,FUN = model.matrix)
names(Model.matrices.RCS.DR.per.study) =  unique(df2$Study)


myplots<- lapply(Model.matrices.RCS.DR.per.study, 
                 function(x) as.data.frame(x)%>%
                   group_by(TreatmentTreated)%>%
                   do(plots=autoplot(zoo(.), facets=NULL)+ ggtitle(.$TreatmentTreated)+theme_bw()+theme(legend.position = "none")))

library(cowplot)
Basis.functions.per.study.plots =  lapply(myplots,FUN = function(x) plot_grid(plotlist = x$plots))

plot_grid(plotlist = Basis.functions.per.study.plots)



### Check the predicted curves per study


new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 50),each = 10),
                    Treatment = rep(unique(df2$Treatment),250), 
                    Study =  rep(rep(unique(df2$Study),each =2),50))


### RCS
predictions.RCS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

predictions.RCS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study)+newtheme + 
  ggtitle("Per study predicted outcomes (Harrel's approach)")+
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)

### NS
predictions.NS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(NS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 



predictions.NS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study)+ 
  ggtitle("Per study predicted outcomes (NS B-splines approach)") + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



### BS
predictions.BS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

predictions.BS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study)+ 
  ggtitle("Per study predicted outcomes (B-splines approach)") + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



source("Assisting functions/Point-wise meta-analysis.R")


### RCS
point.wise.RCS.DR = pointwise.ma(data = predictions.RCS.DR,
                                 clustering.variable = "Study",
                                 combining.variables = c("Treatment","BMI"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")



point.wise.RCS.DR$RE.meta =  expit(point.wise.RCS.DR$RE.meta )
point.wise.RCS.DR$RE.meta.upper =  expit(point.wise.RCS.DR$RE.meta.upper )
point.wise.RCS.DR$RE.meta.lower =  expit(point.wise.RCS.DR$RE.meta.lower )

### NS
point.wise.NS.DR =  pointwise.ma(data = predictions.NS.DR,
                                 clustering.variable = "Study",
                                 combining.variables = c("Treatment","BMI"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")




point.wise.NS.DR$RE.meta  =  expit(point.wise.NS.DR$RE.meta )
point.wise.NS.DR$RE.meta.upper  =  expit(point.wise.NS.DR$RE.meta.upper )
point.wise.NS.DR$RE.meta.lower  =  expit(point.wise.NS.DR$RE.meta.lower)

## BS
point.wise.BS.DR =  pointwise.ma(data = predictions.BS.DR,
                                 clustering.variable = "Study",
                                 combining.variables = c("Treatment","BMI"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")




point.wise.BS.DR$RE.meta  =  expit(point.wise.BS.DR$RE.meta )
point.wise.BS.DR$RE.meta.upper  =  expit(point.wise.BS.DR$RE.meta.upper )
point.wise.BS.DR$RE.meta.lower  =  expit(point.wise.BS.DR$RE.meta.lower)


### Pointwise RCS plot
point.wise.RCS.DR%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=32),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.9, size = 10, label = "RCS") +ylim(c(0,1))



### Pointwise NS plot
point.wise.NS.DR%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=32),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.9, size = 10, label = "NS") +ylim(c(0,1))



### Pointwise BS plot
point.wise.BS.DR%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=32),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19.25,y=0.9, size = 10, label = "BS") +ylim(c(0,1))

