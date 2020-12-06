###--------------------MVmeta--------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/First scenario data-set.R")
#Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


### Clear enviroment 
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")])

### RCS  
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df1 =   list(BMI=quantile(df1$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))) 

## The formula for all Studies is the same so we save it 

formula.RCS = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5)

## Number of studies

nstudies.RCS.df1 = length(unique(df1$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df1 = gam( formula =formula.RCS,
               knots = Knots.RCS.df1,
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
  
  # Fit the GAM
  fit = gam(formula = formula.RCS ,knots = Knots.RCS.df1, family = binomial("logit"), data = minidf1)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df1[j,] = fit$coefficients
  S.RCS.df1[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]

  k=k+2
  j=j+1
  #rm(i,minidf1,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.RCS.df1 = mvmeta(estimated.coefficients.RCS.df1, S.RCS.df1)


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


### B-splines
###

## The formula for all studies is the same so we save it 

formula.BS = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,k = 5,m=c(2,0))

## Number of studies

nstudies.BS.df1 = length(unique(df1$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.BS.df1 = gam( formula =formula.BS,
                  family = binomial("logit"), 
                  data = df1)

### Get the model matrices for each data-set

X.p.BS.df1 =  model.matrix(fit.BS.df1)


### Save the knots to use the same per study
Knots.BS.df =  list(BMI=c(4.18, 11.34, 18.50, 25.66, 32.84, 40, 47.16, 54.32)) 


### Create empty matrix for the estimated splines coefficients

estimated.coefficients.BS.df1 = matrix(NA,
                                       ncol = length(fit.BS.df1$coefficients),nrow=nstudies.BS.df1,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.BS.df1)),1, paste, collapse=" "),
                                                        c(1:length(fit.BS.df1$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.BS.df1 = matrix(NA, ncol=sum(c(1:length(fit.BS.df1$coefficients))), nrow = nstudies.BS.df1 )

k=3
j=1

for( i in unique(df1$Study)){
  
  ### Get a mini df1 with only one study
  minidf1 = df1%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.BS , 
            family = binomial("logit"), 
            data = minidf1)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.BS.df1[j,] = fit$coefficients
  S.BS.df1[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  rm(i,minidf1,fit)
}
rm(k,j)




#### Perform a multivariate meta-analysis
mv.fit.BS.df1 = mvmeta(estimated.coefficients.BS.df1, S.BS.df1)


prediction.interval.mvmeta.BS.df1 =  X.p.BS.df1 %*% coef(mv.fit.BS.df1)

prediction.interval.mvmeta.lower.BS.df1 =  X.p.BS.df1 %*% coef(mv.fit.BS.df1) - sqrt( rowSums(X.p.BS.df1  * (X.p.BS.df1  %*% vcov(mv.fit.BS.df1)))) * qt(0.025, dim(df1)[1] - 3)

prediction.interval.mvmeta.upper.BS.df1 =  X.p.BS.df1 %*% coef(mv.fit.BS.df1) + sqrt( rowSums(X.p.BS.df1  * (X.p.BS.df1  %*% vcov(mv.fit.BS.df1)))) * qt(0.025, dim(df1)[1] - 3)


mvmeta.df.BS = cbind(df1[,c("Study","BMI","Treatment")],
                     fit =  expit(prediction.interval.mvmeta.BS.df1), 
                     Lower= expit(prediction.interval.mvmeta.lower.BS.df1),
                     Upper =expit(prediction.interval.mvmeta.upper.BS.df1 ))





g.mvmeta.total.BS = ggplot(mvmeta.df.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.BS


p1=  ggplot(mvmeta.df.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
MV_meta_plot_HT = grid.arrange(arrangeGrob(g.mvmeta.total.RCS,g.mvmeta.total.BS,
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



MV.meta_absolute_difference.BS.HT =  risk.diff.creator(dataframe = mvmeta.df.BS,
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


MV.meta_absolute_difference.BS.HT.plot = 
  ggplot(MV.meta_absolute_difference.BS.HT,aes(x = BMI, fit.diff)) + 
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


MV_meta_plot_absolute_diff_HT = grid.arrange(MV.meta_absolute_difference.RCS.HT.plot,MV.meta_absolute_difference.BS.HT.plot,
                                             ncol = 2, 
                                             right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
                                             bottom= textGrob(label = expression(BMI (Kg/m^2)),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32)))








