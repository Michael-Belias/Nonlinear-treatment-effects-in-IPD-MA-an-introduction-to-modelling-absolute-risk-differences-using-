###--------------------MVmeta--------------------------------------

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


### RCS  


## And we add them to the original data-set.


df2 =  rbind(df2, rep.lower, rep.upper)
## Now each study has values near the boundaries of BMI [18.5,40]

#df2%>%
#  group_by(Study)%>%
#  summarise(range(BMI))


### remove the object we don't need

rm(lower,upper, rep.lower,rep.upper, n.lower,n.upper)



## The formula for all Studies is the same so we save it 
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df2 =   quantile(df2[df2$weight==1,]$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))

formula.RCS = Y~ Treatment +  rcs(BMI, knots =  Knots.RCS.df2)*Treatment

## Number of studies

nstudies.RCS.df2 = length(unique(df2$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df2 = glm( formula=formula.RCS,
                   family = binomial("logit"), 
                   data = df2[df2$weight==1,])

### Get the model matrices for each data-set

X.p.RCS.df2 =  model.matrix(fit.RCS.df2)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.RCS.df2 = matrix(NA,
                                        ncol = length(fit.RCS.df2$coefficients),nrow=nstudies.RCS.df2,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.RCS.df2)),1, paste, collapse=" "),
                                                         c(1:length(fit.RCS.df2$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.RCS.df2 = matrix(NA, ncol=sum(c(1:length(fit.RCS.df2$coefficients))), nrow = nstudies.RCS.df2 )

j=1

for( i in unique(df2$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2%>%
    filter(Study == i)
  
  # Fit the GLM
  fit = glm(formula = formula.RCS ,
            weights= weight,
            family = binomial("logit"), 
            data = minidf2)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df2[j,] = fit$coefficients
  S.RCS.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  

  j=j+1
  #rm(i,minidf2,fit)
}
rm(j)


#### Perform a multivariate meta-analysis
mv.fit.RCS.df2 = mvmeta(estimated.coefficients.RCS.df2, S.RCS.df2,control = list(maxiter=1000))


prediction.interval.mvmeta.RCS.df2 =  X.p.RCS.df2 %*% coef(mv.fit.RCS.df2)

prediction.interval.mvmeta.lower.RCS.df2 =  X.p.RCS.df2 %*% coef(mv.fit.RCS.df2) - sqrt( rowSums(X.p.RCS.df2  * (X.p.RCS.df2  %*% vcov(mv.fit.RCS.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df2 =  X.p.RCS.df2 %*% coef(mv.fit.RCS.df2) + sqrt( rowSums(X.p.RCS.df2  * (X.p.RCS.df2  %*% vcov(mv.fit.RCS.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df2.RCS = cbind(df2[df2$weight==1,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.RCS.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.RCS.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.RCS.df2 ))





g.mvmeta.total.RCS.DR = ggplot(mvmeta.df2.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  #geom_line(size=2)+ 
  ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  #geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate(geom = "text",x = 19,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS.DR


### NS-splines
###

## The formula for all studies is the same so we save it 
### Save the knots to use the same per study
Knots.NS.df2 =  BMI=c( 25.6463,32.7926)
formula.NS = Y~ Treatment+ ns(BMI,knots = Knots.NS.df2)*Treatment

## Number of studies

nstudies.NS.df2 = length(unique(df2$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.NS.df2 = glm( formula =formula.NS,
                  family = binomial("logit"), 
                  data = df2[df2$weight==1,])

### Get the model matrices for each data-set

X.p.NS.df2 =  model.matrix(fit.NS.df2)



### In order for mvmeta to work we need the applied B-spline to be fitted on the same domain




### Create empty matrix for the estimated splines coefficients

estimated.coefficients.NS.df2 = matrix(NA,
                                       ncol = length(fit.NS.df2$coefficients),nrow=nstudies.NS.df2,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.NS.df2)),1, paste, collapse=" "),
                                                        c(1:length(fit.NS.df2$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.NS.df2 = matrix(NA, ncol=sum(c(1:length(fit.NS.df2$coefficients))), nrow = nstudies.NS.df2 )


j=1

for( i in unique(df2$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2%>%
    filter(Study == i)
  
  # Fit the GLM
  fit = glm(formula = formula.NS ,
            family = binomial("logit"),weights = weight, 
            data = minidf2)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.NS.df2[j,] = fit$coefficients
  S.NS.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  

  j=j+1
  #rm(i,minidf2,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.NS.df2 = mvmeta(estimated.coefficients.NS.df2, S.NS.df2,control = list(maxiter=1000))

prediction.interval.mvmeta.NS.df2 =  X.p.NS.df2 %*% coef(mv.fit.NS.df2)

prediction.interval.mvmeta.lower.NS.df2 =  X.p.NS.df2 %*% coef(mv.fit.NS.df2) - sqrt( rowSums(X.p.NS.df2  * (X.p.NS.df2  %*% vcov(mv.fit.NS.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.NS.df2 =  X.p.NS.df2 %*% coef(mv.fit.NS.df2) + sqrt( rowSums(X.p.NS.df2  * (X.p.NS.df2  %*% vcov(mv.fit.NS.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df2.NS = cbind(df2[df2$weight==1,c("Study","BMI","Treatment")],
                     fit =  expit(prediction.interval.mvmeta.NS.df2), 
                     Lower= expit(prediction.interval.mvmeta.lower.NS.df2),
                     Upper =expit(prediction.interval.mvmeta.upper.NS.df2 ))





g.mvmeta.total.NS.DR = ggplot(mvmeta.df2.NS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
        axis.text.y = element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19,y = 0.8,label=  " b", size= 12 ) 


g.mvmeta.total.NS.DR


p1=  ggplot(mvmeta.df2.NS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
        axis.text.y = element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom") + 
  annotate("text",x = 19,y=0.9, size = 10, label = "d") +ylim(c(0,1))



legend = gtable_filter(ggplotGrob(p1), "guide-box") 



library(grid)
MV_meta_plot_DR = grid.arrange(arrangeGrob(g.mvmeta.total.RCS.DR,g.mvmeta.total.NS.DR,
                                           ncol = 2, 
                                           right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                           bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32))),
                               legend, heights=  c(10,1))




###########


## ----Absolute risk differences ---------------------------------------------------------------------------------------------------------
### MVmeta

source("Assisting functions/Create risk differences.R")


MV.meta_absolute_difference.RCS.DR =  risk.diff.creator(dataframe = mvmeta.df2.RCS,
                                                        treatment = "Treatment",
                                                        matching.variables=  c("BMI"),
                                                        outcome= NULL, 
                                                        predicted.outcome = "fit", 
                                                        predicted.CI = c("Lower","Upper"))



MV.meta_absolute_difference.NS.DR =  risk.diff.creator(dataframe = mvmeta.df2.NS,
                                                       treatment = "Treatment",
                                                       matching.variables=  c("BMI"),
                                                       outcome= NULL, 
                                                       predicted.outcome = "fit", 
                                                       predicted.CI = c("Lower","Upper"))




MV.meta_absolute_difference.RCS.DR.plot = ggplot(MV.meta_absolute_difference.RCS.DR,
                                                 aes(x = BMI, fit.diff)) + 
  #geom_line(size=2)+ 
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  #geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  #geom_hline(yintercept = 0, linetype=2)+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10, label = "a")+ ylim(c(-1,1))


MV.meta_absolute_difference.NS.DR.plot = 
  ggplot(MV.meta_absolute_difference.NS.DR,aes(x = BMI, fit.diff)) + 
  geom_line(size=2)+ 
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10, label = "b")+ ylim(c(-1,1))


MV_meta_plot_absolute_diff_DR = grid.arrange(MV.meta_absolute_difference.RCS.DR.plot,MV.meta_absolute_difference.NS.DR.plot,
                                             ncol = 2, 
                                             right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                             bottom= textGrob(label = expression(BMI (Kg/m^2)),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32)))







