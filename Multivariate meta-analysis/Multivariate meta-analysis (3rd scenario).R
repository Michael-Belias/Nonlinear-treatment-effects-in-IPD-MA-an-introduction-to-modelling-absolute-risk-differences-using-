###--------------------MVmeta--------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")
#Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}

#rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])

## We follow White et al. and Riley et al. recommendations and we performed data augmentation
## In our simulated data-set the range of BMI in Study 1 is [18.5,27] while in Study 5 [31,40]
## To avoid errors we create pseudo-data on the boundaries and give them very small weight to 
## avoid biased regression curves. 

## We introduce the weight variable in our data-set 
## In the original data-set the weight will be 1, while in the augmented data-set 0.000000001
df3$weight =  1


## We save into objects the values near the overall boundaries 18.5 and 40 
## Note that how close we wish to be on the boundaries is arbitrary. 

lower=df3[df3$BMI<19,] ;   n.lower =  dim(lower)[1]; lower$BMI = 18.5
upper=df3[df3$BMI >39.5,]; n.upper =  dim(upper)[1]; upper$BMI = 40


## We produce 4 copies of the lower and upper data-sets and give them very small weights.

rep.lower=  do.call(rbind, replicate(4, lower, simplify = FALSE)) ;  rep.lower$weight= 0.0000001
rep.upper=  do.call(rbind, replicate(4, upper, simplify = FALSE)) ;  rep.upper$weight= 0.0000001

## As you can see the lower and upper data-sets use the original study information
head(rep.lower); head(rep.upper)
## So we change the names of the studies within the data-sets. 

rep.lower$Study = rep(unique(df3$Study)[-1], each= n.lower)
rep.upper$Study = rep(unique(df3$Study)[-5], each= n.upper)

## And we add them to the original data-set.

### RCS  

df3 =  rbind(df3, rep.lower, rep.upper)
## Now each study has values near the boundaries of BMI [18.5,40]

#df3%>%
#  group_by(Study)%>%
#  summarise(range(BMI))


### remove the object we don't need

rm(lower,upper, rep.lower,rep.upper,n.lower, n.upper)


## The formula for all Studies is the same so we save it 

formula.RCS = Y~ Treatment +  rcs(BMI, knots =  Knots.RCS.df3)*Treatment

### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df3 =   quantile(df3[df3$weight==1,]$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))

## Number of studies

nstudies.RCS.df3 = length(unique(df3$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df3 = glm( formula =formula.RCS,
                   weights = weight,
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


j=1

for( i in unique(df3$Study)){
  
  ### Get a mini df3 with only one study
  minidf3 = df3%>%
    filter(Study == i)
  
  # Fit the glm
  fit = glm(formula = formula.RCS ,
            weights = weight,
            family = binomial("logit"), data = minidf3)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df3[j,] = fit$coefficients
  S.RCS.df3[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  

  j=j+1
  #rm(i,minidf3,fit)
}
rm(j)


#### Perform a multivariate meta-analysis
mv.fit.RCS.df3 = mvmeta(estimated.coefficients.RCS.df3, S.RCS.df3, control= list(maxiter=1000))


prediction.interval.mvmeta.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3)

prediction.interval.mvmeta.lower.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3) - sqrt( rowSums(X.p.RCS.df3  * (X.p.RCS.df3  %*% vcov(mv.fit.RCS.df3)))) * qt(0.025, dim(df3)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3) + sqrt( rowSums(X.p.RCS.df3  * (X.p.RCS.df3  %*% vcov(mv.fit.RCS.df3)))) * qt(0.025, dim(df3)[1] - 3)


mvmeta.df3.RCS = cbind(df3[,c("Study","BMI","Treatment")],
                       fit =  expit(prediction.interval.mvmeta.RCS.df3), 
                       Lower= expit(prediction.interval.mvmeta.lower.RCS.df3),
                       Upper =expit(prediction.interval.mvmeta.upper.RCS.df3 ))





g.mvmeta.total.RCS.Comb = ggplot(mvmeta.df3.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + theme_bw()+
  #geom_line(size=2)+ 
  ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  # geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
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
        legend.position = "none")+
  annotate(geom = "text",x = 19,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS.Comb



### NS-splines
###

## The formula for all studies is the same so we save it 

formula.NS = Y~ Treatment+ ns(BMI, knots = c(25.6, 32.8))*Treatment

## Number of studies

nstudies.NS.df3 = length(unique(df3$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.NS.df3 = glm( formula =formula.NS,
                  family = binomial("logit"), 
                  data = df3[df3$weight==1,])


### Get the model matrices for each data-set

X.p.NS.df3 =  model.matrix(fit.NS.df3)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.NS.df3 = matrix(NA,
                                       ncol = length(fit.NS.df3$coefficients),nrow=nstudies.NS.df3,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.NS.df3)),1, paste, collapse=" "),
                                                        c(1:length(fit.NS.df3$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.NS.df3 = matrix(NA, ncol=sum(c(1:length(fit.NS.df3$coefficients))), nrow = nstudies.NS.df3 )


j=1

for( i in unique(df3$Study)){
  
  ### Get a mini df3 with only one study
  minidf3 = df3%>%
    filter(Study == i)
  
  # Fit the glm
  fit = glm(formula = formula.NS ,
            weights = weight,
            family = binomial("logit"), data = minidf3)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.NS.df3[j,] = fit$coefficients
  S.NS.df3[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  

  j=j+1
  rm(i,minidf3,fit)
}
rm(j)




#### Perform a multivariate meta-analysis
mv.fit.NS.df3 = mvmeta(estimated.coefficients.NS.df3, S.NS.df3,control = list(maxiter=1000))


prediction.interval.mvmeta.NS.df3 =  X.p.NS.df3 %*% coef(mv.fit.NS.df3)

prediction.interval.mvmeta.lower.NS.df3 =  X.p.NS.df3 %*% coef(mv.fit.NS.df3) - sqrt( rowSums(X.p.NS.df3  * (X.p.NS.df3  %*% vcov(mv.fit.NS.df3)))) * qt(0.025, dim(df3)[1] - 3)

prediction.interval.mvmeta.upper.NS.df3 =  X.p.NS.df3 %*% coef(mv.fit.NS.df3) + sqrt( rowSums(X.p.NS.df3  * (X.p.NS.df3  %*% vcov(mv.fit.NS.df3)))) * qt(0.025, dim(df3)[1] - 3)


mvmeta.df3.NS = cbind(df3[df3$weight==1,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.NS.df3), 
                      Lower= expit(prediction.interval.mvmeta.lower.NS.df3),
                      Upper =expit(prediction.interval.mvmeta.upper.NS.df3 ))





g.mvmeta.total.NS.Comb = ggplot(mvmeta.df3.NS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.NS.Comb



###########


source("Assisting functions/Create risk differences.R")


MV.meta_absolute_difference.RCS.Comb =  risk.diff.creator(dataframe = mvmeta.df3.RCS,
                                                        treatment = "Treatment",
                                                        matching.variables=  c("BMI"),
                                                        outcome= NULL, 
                                                        predicted.outcome = "fit", 
                                                        predicted.CI = c("Lower","Upper"))



MV.meta_absolute_difference.NS.Comb =  risk.diff.creator(dataframe = mvmeta.df3.NS,
                                                       treatment = "Treatment",
                                                       matching.variables=  c("BMI"),
                                                       outcome= NULL, 
                                                       predicted.outcome = "fit", 
                                                       predicted.CI = c("Lower","Upper"))




MV.meta_absolute_difference.RCS.Comb.plot = ggplot(MV.meta_absolute_difference.RCS.Comb,
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


MV.meta_absolute_difference.NS.Comb.plot = 
  ggplot(MV.meta_absolute_difference.NS.Comb,aes(x = BMI, fit.diff)) + 
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


MV_meta_plot_absolute_diff_Comb = grid.arrange(MV.meta_absolute_difference.RCS.Comb.plot,MV.meta_absolute_difference.NS.Comb.plot,
                                             ncol = 2, 
                                             right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                             bottom= textGrob(label = expression(BMI (Kg/m^2)),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32)))


