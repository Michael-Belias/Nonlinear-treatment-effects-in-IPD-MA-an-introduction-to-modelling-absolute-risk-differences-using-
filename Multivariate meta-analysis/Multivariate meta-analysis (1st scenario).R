###--------------------MVmeta--------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/First scenario data-set.R")
#Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


### Clear enviroment 
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")])

### RCS  
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df1 =  quantile(df1$BMI, probs = c(0.05,0.275,0.5,0.725,0.95)) 

## The formula for all Studies is the same so we save it 

formula.RCS = Y~ Treatment +  rcs(BMI, knots =  Knots.RCS.df1)*Treatment

## Number of studies

nstudies.RCS.df1 = length(unique(df1$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df1 = glm( formula =formula.RCS,
               family = binomial("logit"), 
               data = df1)

### Get the model matrices for each data-set

X.p.RCS.df1 =  model.matrix(fit.RCS.df1)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.RCS.df1 = matrix(NA,
                                    ncol = length(fit.RCS.df1$coefficients),nrow=nstudies.RCS.df1,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.RCS.df1)),1, paste, collapse=" "),
                                                     c(1:length(fit.RCS.df1$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.RCS.df1 = matrix(NA, ncol=sum(c(1:length(fit.RCS.df1$coefficients))), nrow = nstudies.RCS.df1 )

k=3
j=1

for( i in unique(df1$Study)){
  
  ### Get a mini df1 with only one study
  minidf1 = df1%>%
    filter(Study == i)
  
  # Fit the GLM
  fit = glm(formula = formula.RCS , family = binomial("logit"), data = minidf1)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df1[j,] = fit$coefficients
  S.RCS.df1[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]

  k=k+2
  j=j+1
  #rm(i,minidf1,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.RCS.df1 = mvmeta(estimated.coefficients.RCS.df1, S.RCS.df1, control = list(maxiter=1000))


prediction.interval.mvmeta.RCS.df1 =  X.p.RCS.df1 %*% coef(mv.fit.RCS.df1)

prediction.interval.mvmeta.lower.RCS.df1 =  X.p.RCS.df1 %*% coef(mv.fit.RCS.df1) - sqrt( rowSums(X.p.RCS.df1  * (X.p.RCS.df1  %*% vcov(mv.fit.RCS.df1)))) * qt(0.025, dim(df1)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df1 =  X.p.RCS.df1 %*% coef(mv.fit.RCS.df1) + sqrt( rowSums(X.p.RCS.df1  * (X.p.RCS.df1  %*% vcov(mv.fit.RCS.df1)))) * qt(0.025, dim(df1)[1] - 3)


mvmeta.df.RCS = cbind(df1[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.RCS.df1), 
                      Lower= expit(prediction.interval.mvmeta.lower.RCS.df1),
                      Upper =expit(prediction.interval.mvmeta.upper.RCS.df1 ))





g.mvmeta.total.RCS = ggplot(mvmeta.df.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
  annotate(geom = "text",x = 19,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS


### NS-splines
###

## The formula for all studies is the same so we save it 

formula.NS= Y~ Treatment  +  ns(BMI, df = 3)*Treatment



## Number of studies

nstudies.NS.df1 = length(unique(df1$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.NS.df1 = glm( formula =formula.NS,
                  family = binomial("logit"), 
                  data = df1)

### Get the model matrices for each data-set

X.p.NS.df1 =  model.matrix(fit.NS.df1)


### Save the knots to use the same per study
Knots.NS.df =  BMI=c( 25.6463,32.7926)


### Create empty matrix for the estimated splines coefficients

estimated.coefficients.NS.df1 = matrix(NA,
                                       ncol = length(fit.NS.df1$coefficients),nrow=nstudies.NS.df1,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.NS.df1)),1, paste, collapse=" "),
                                                        c(1:length(fit.NS.df1$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.NS.df1 = matrix(NA, ncol=sum(c(1:length(fit.NS.df1$coefficients))), nrow = nstudies.NS.df1 )

k=3
j=1

for( i in unique(df1$Study)){
  
  ### Get a mini df1 with only one study
  minidf1 = df1%>%
    filter(Study == i)
  
  # Fit the GLM
  fit = glm(formula = formula.NS, 
            family = binomial("logit"), 
            data = minidf1)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.NS.df1[j,] = fit$coefficients
  S.NS.df1[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  rm(i,minidf1,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.NS.df1 = mvmeta(estimated.coefficients.NS.df1, S.NS.df1)


prediction.interval.mvmeta.NS.df1 =  X.p.NS.df1 %*% coef(mv.fit.NS.df1)

prediction.interval.mvmeta.lower.NS.df1 =  X.p.NS.df1 %*% coef(mv.fit.NS.df1) - sqrt( rowSums(X.p.NS.df1  * (X.p.NS.df1  %*% vcov(mv.fit.NS.df1)))) * qt(0.025, dim(df1)[1] - 3)

prediction.interval.mvmeta.upper.NS.df1 =  X.p.NS.df1 %*% coef(mv.fit.NS.df1) + sqrt( rowSums(X.p.NS.df1  * (X.p.NS.df1  %*% vcov(mv.fit.NS.df1)))) * qt(0.025, dim(df1)[1] - 3)


mvmeta.df.NS= cbind(df1[,c("Study","BMI","Treatment")],
                     fit =  expit(prediction.interval.mvmeta.NS.df1), 
                     Lower= expit(prediction.interval.mvmeta.lower.NS.df1),
                     Upper =expit(prediction.interval.mvmeta.upper.NS.df1 ))





g.mvmeta.total.NS= ggplot(mvmeta.df.NS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.NS


p1=  ggplot(mvmeta.df.NS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
MV_meta_plot_HT = grid.arrange(arrangeGrob(g.mvmeta.total.RCS,g.mvmeta.total.NS,
                                           ncol = 2, 
                                           right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                           bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32))),
                               legend, heights=  c(10,1))




###########



## ----Absolute risk differences ---------------------------------------------------------------------------------------------------------
### MVmeta
source("Assisting functions/Create risk differences.R")


MV.meta_absolute_difference.RCS.HT =  risk.diff.creator(dataframe = mvmeta.df.RCS,
                                                        treatment = "Treatment",
                                                        matching.variables=  c("BMI"),
                                                        outcome= NULL, 
                                                        predicted.outcome = "fit", 
                                                        predicted.CI = c("Lower","Upper"))



MV.meta_absolute_difference.NS.HT =  risk.diff.creator(dataframe = mvmeta.df.NS,
                                                       treatment = "Treatment",
                                                       matching.variables=  c("BMI"),
                                                       outcome= NULL, 
                                                       predicted.outcome = "fit", 
                                                       predicted.CI = c("Lower","Upper"))




MV.meta_absolute_difference.RCS.HT.plot = ggplot(MV.meta_absolute_difference.RCS.HT,
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
        axis.text.y = element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10, label = "a")+ ylim(c(-1,1))


MV.meta_absolute_difference.NS.HT.plot = 
  ggplot(MV.meta_absolute_difference.NS.HT,aes(x = BMI, fit.diff)) + 
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "b") + ylim(c(-1,1))


MV_meta_plot_absolute_diff_HT = grid.arrange(MV.meta_absolute_difference.RCS.HT.plot,MV.meta_absolute_difference.NS.HT.plot,
                                             ncol = 2, 
                                             right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                             bottom= textGrob(label = expression(BMI (Kg/m^2)),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32)))








