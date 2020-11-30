###--------------------MVmeta--------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")
#rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])
### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


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

rep.lower=  do.call(rbind, replicate(4, lower, simplify = FALSE)) ;  rep.lower$weight= 0.0000000001
rep.upper=  do.call(rbind, replicate(4, upper, simplify = FALSE)) ;  rep.upper$weight= 0.0000000001

## As you can see the lower and upper data-sets use the original study information
head(rep.lower); head(rep.upper)
## So we change the names of the studies within the data-sets. 

rep.lower$Study = rep(unique(df2$Study)[-1], each= n.lower)
rep.upper$Study = rep(unique(df2$Study)[-5], each= n.upper)

## And we add them to the original data-set.


df2 =  rbind(df2, rep.lower, rep.upper)
## Now each study has values near the boundaries of BMI [18.5,40]

df2%>%
  group_by(Study)%>%
  summarise(range(BMI))


### remove the object we don't need

rm(lower,upper, rep.lower,rep.upper)


### RCS  
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df2 =   list(BMI=c(21, 25, 29, 31, 33.5, 37.8)) 

K <- length(c(21, 25, 29, 31, 33.5, 37.8))

## The formula for all Studies is the same so we save it 

formula.RCS = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = K)

## Number of studies

nstudies.RCS.df2 = length(unique(df2$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df2 = gam( formula =formula.RCS,
                   knots = Knots.RCS.df2,
                   family = binomial("logit"), 
                   data = df2)

### Get the model matrices for each data-set

X.p.RCS.df2 =  model.matrix(fit.RCS.df2)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.RCS.df2 = matrix(NA,
                                        ncol = length(fit.RCS.df2$coefficients),nrow=nstudies.RCS.df2,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.RCS.df2)),1, paste, collapse=" "),
                                                         c(1:length(fit.RCS.df2$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.RCS.df2 = matrix(NA, ncol=sum(c(1:length(fit.RCS.df2$coefficients))), nrow = nstudies.RCS.df2 )

k=3
j=1

for( i in unique(df2$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.RCS ,
            knots = Knots.RCS.df2, 
            weights= weight,
            family = binomial("logit"), data = minidf2)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df2[j,] = fit$coefficients
  S.RCS.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  #rm(i,minidf2,fit)
}
rm(k,j)


table(df2$weight)

#### Perform a multivariate meta-analysis
mv.fit.RCS.df2 = mvmeta(estimated.coefficients.RCS.df2, S.RCS.df2)


prediction.interval.mvmeta.RCS.df2 =  X.p.RCS.df2 %*% coef(mv.fit.RCS.df2)

prediction.interval.mvmeta.lower.RCS.df2 =  X.p.RCS.df2 %*% coef(mv.fit.RCS.df2) - sqrt( rowSums(X.p.RCS.df2  * (X.p.RCS.df2  %*% vcov(mv.fit.RCS.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df2 =  X.p.RCS.df2 %*% coef(mv.fit.RCS.df2) + sqrt( rowSums(X.p.RCS.df2  * (X.p.RCS.df2  %*% vcov(mv.fit.RCS.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df2.RCS = cbind(df2[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.RCS.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.RCS.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.RCS.df2 ))





g.mvmeta.total.RCS.DR = ggplot(mvmeta.df2.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.RCS.DR


### B-splines
###

## The formula for all studies is the same so we save it 

formula.BS = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,k = 5,m=c(2,0))

## Number of studies

nstudies.BS.df2 = length(unique(df2$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.BS.df2 = gam( formula =formula.BS,
                  family = binomial("logit"), 
                  data = df2)

### Get the model matrices for each data-set

X.p.BS.df2 =  model.matrix(fit.BS.df2)


### Get the knots from the overall model to use the same per study
Knots.BS.df2 =  list(BMI=fit.BS.df2$smooth[[1]]$knots) 



### In order for mvmeta to work we need the applied B-spline to be fitted on the same domain




### Create empty matrix for the estimated splines coefficients

estimated.coefficients.BS.df2 = matrix(NA,
                                       ncol = length(fit.BS.df2$coefficients),nrow=nstudies.BS.df2,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.BS.df2)),1, paste, collapse=" "),
                                                        c(1:length(fit.BS.df2$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.BS.df2 = matrix(NA, ncol=sum(c(1:length(fit.BS.df2$coefficients))), nrow = nstudies.BS.df2 )

k=3
j=1

for( i in unique(df2$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.BS ,
            knots = Knots.BS.df2, 
            family = binomial("logit"), data = minidf2)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.BS.df2[j,] = fit$coefficients
  S.BS.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  rm(i,minidf2,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.BS.df2 = mvmeta(estimated.coefficients.BS.df2, S.BS.df2)


prediction.interval.mvmeta.BS.df2 =  X.p.BS.df2 %*% coef(mv.fit.BS.df2)

prediction.interval.mvmeta.lower.BS.df2 =  X.p.BS.df2 %*% coef(mv.fit.BS.df2) - sqrt( rowSums(X.p.BS.df2  * (X.p.BS.df2  %*% vcov(mv.fit.BS.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.BS.df2 =  X.p.BS.df2 %*% coef(mv.fit.BS.df2) + sqrt( rowSums(X.p.BS.df2  * (X.p.BS.df2  %*% vcov(mv.fit.BS.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df2.BS = cbind(df2[,c("Study","BMI","Treatment")],
                     fit =  expit(prediction.interval.mvmeta.BS.df2), 
                     Lower= expit(prediction.interval.mvmeta.lower.BS.df2),
                     Upper =expit(prediction.interval.mvmeta.upper.BS.df2 ))





g.mvmeta.total.BS.DR = ggplot(mvmeta.df2.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.BS.DR


p1=  ggplot(mvmeta.df2.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
MV_meta_plot_DR = grid.arrange(arrangeGrob(g.mvmeta.total.RCS.DR,g.mvmeta.total.BS.DR,
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



MV.meta_absolute_difference.BS.DR =  risk.diff.creator(dataframe = mvmeta.df2.BS,
                                                       treatment = "Treatment",
                                                       matching.variables=  c("BMI"),
                                                       outcome= NULL, 
                                                       predicted.outcome = "fit", 
                                                       predicted.CI = c("Lower","Upper"))




MV.meta_absolute_difference.RCS.DR.plot = ggplot(MV.meta_absolute_difference.RCS.DR,
                                                 aes(x = BMI, fit.diff)) + 
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
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "a")


MV.meta_absolute_difference.BS.DR.plot = 
  ggplot(MV.meta_absolute_difference.BS.DR,aes(x = BMI, fit.diff)) + 
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
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.2, size = 10, label = "b")


MV_meta_plot_absolute_diff_DR = grid.arrange(MV.meta_absolute_difference.RCS.DR.plot,MV.meta_absolute_difference.BS.DR.plot,
                                             ncol = 2, 
                                             right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                             bottom= textGrob(label = expression(BMI (Kg/m^2)),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32)))







