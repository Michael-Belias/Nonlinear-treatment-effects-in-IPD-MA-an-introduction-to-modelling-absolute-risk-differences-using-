
## ----Load necessary libraries -----------------------------------------------------------------------
rm(list=ls()) ### To clear enviroment
# set.wd() ## use this function to define your personal workspace



# Libraries for loading and saving data
## Load haven for loading sas data-sets 
if(!require("haven")) install.packages("haven")

################################################
# Libraries for plotting

## Load ggplot2 for basic plotting
if(!require("ggplot2")) install.packages("ggplot2")
## Load ggpubr for plotting
if(!require("ggpubr")) install.packages("ggpubr") # automatically loads the ggplot2 (if installed)
## Load grid and gridExtra  to arrange multiple grid-based plots on a page, and draw tables
if(!require("grid")) install.packages("grid")
if(!require("gridExtra")) install.packages("gridExtra")
## Load ggsci for better looking colors
if(!require("ggsci")) install.packages("ggsci")
## Load gtable to get tools to make it easier to work with graphic ``tables''.
if(!require("gtable")) install.packages("gtable") 
## Load graphics for some base R graphics functions
if(!require("graphics")) install.packages("graphics")


################################################
# Libraries for data manipulation
## Load knitr for fine tuning
if(!require("knitr")) install.packages("knitr")
## Load dplyr for data manipulation
if(!require("dplyr")) install.packages("dplyr")
## Load tidyr for data manipulation
if(!require("tidyr")) install.packages("tidyr")
## Load broom for data manipulation
if(!require("broom")) install.packages("broom")
## Load tidyverse for data manipulation
if(!require("tidyverse")) install.packages("tidyverse")
## Load zoo package for data manipulation of the basis functions
if(!require("zoo")) install.packages("zoo")



################################################
# Libraries for the statistical analysis 
## Load lme4 for lme4
if(!require("lme4")) install.packages("lme4")
## Load lmerTest to add information into the summaries  
if(!require("lmerTest")) install.packages("lmerTest")
## Load mcgv to fit GAMs and GAMMs
if(!require("mgcv")) install.packages("mgcv")
## Load splines to use basic splines functions
if(!require("splines")) install.packages("splines")
## Load rms package to fit restricted cubic splines using Harrel's suggestions
if(!require("rms")) install.packages("rms")
## Load meta for univariate meta-analysis
if(!require("meta")) install.packages("meta")
## Load mvmeta for multivariate meta-analysis
if(!require("mvmeta")) install.packages("mvmeta")
## Load splines2 for splines assisting functions
if(!require("splines2")) install.packages("splines2")



## ----simulation of the single study data-set------------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Single study dataset.R")

## ----Figure 1: Simulated association between mortality risk and BMI in a single study"---------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 1.R")

## ----Simulation of IPD-sets with multiple studies --------------------------------------------------------------------------------------

## ----Simulation of first IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/First scenario data-set.R")

## ----Simulation of second IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")

## ----Simulation of third IPD-MA scenario --------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")

### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}

## ----Figure 2: Association between mortality risk and BMI per study in the heterogeneous IPD-set with equal BMI ranges"----
source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 2.R")

## ----Create a table reporting the mean and ranges of BMI per study for second scenario IPD-set -------------------------------------------------------------------------------------
table2 = df2%>%
    group_by(Study)%>%
  summarise("Range of BMI" =  paste("[", round(min(BMI),1), ",", round(max(BMI),1),"]", sep = ""), "Average BMI" =  round(mean(BMI),1))

colnames(table2) =  c("Studies","BMI Range","Average BMI")

table2

## ----Create a table reporting the mean and ranges of BMI per study for third scenario IPD-set -------------------------------------------------------------------------------------

table3 = df3%>%
  group_by(Study)%>%
  summarise("Range of BMI" =  paste("[", round(min(BMI),1), ",", round(max(BMI),1),"]", sep = ""), "Average BMI" =  round(mean(BMI),1))

colnames(table3) =  c("Studies","BMI Range","Average BMI")

table3


## ----Figure 3: Association between mortality risk and BMI per study in the non-heterogeneous IPD-set with different BMI ranges."----

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 3.R")


## ----Figure 4: Association between mortality risk and BMI per study in the combined IPD-set with different BMI ranges and between study differences in the mortality risks."----

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 4.R")

source("Code for Figures, Tables, Analysis and data-simulation/Appendix/Appendix.R")


## ----Script for splines fitting in single study scenario----------------------------------------------------------------------------

#### Clear enviroment and keep only the datasets
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])


source("Code for Figures, Tables, Analysis and data-simulation/Analysis code/Single study analysis and figures.R")

## ----Pointwise meta-analysis---------------------------------------------------------------------------------
## Pointwise meta-analysis on first scenario

source("Pointwise meta-analysis/Pointwise meta-analysis (1st scenario).R")

## Pointwise meta-analysis on scenario scenario

source("Pointwise meta-analysis/Pointwise meta-analysis (2nd scenario).R")


## ----Point-wise meta-analysis results for third scenario IPD-set-------------------------------------------------------------------------------

source("Pointwise meta-analysis/Pointwise meta-analysis (3rd scenario).R")


#### Figure 8

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 8.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.plot, 
                         point.wise.DF.BS.HT.plot, 
                         point.wise.DF.PS.HT.plot, 
                         point.wise.DF.SS.HT.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = -1,gp = gpar(fontsize=32))),
             arrangeGrob(point.wise.DF.RCS.DR.plot, 
                         point.wise.DF.BS.DR.plot, 
                         point.wise.DF.PS.DR.plot, 
                         point.wise.DF.SS.DR.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = -1,gp = gpar(fontsize=32))),
             arrangeGrob(point.wise.DF.RCS.Comb.plot, 
                         point.wise.DF.BS.Comb.plot, 
                         point.wise.DF.PS.Comb.plot, 
                         point.wise.DF.SS.Comb.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = 0,gp = gpar(fontsize=32)),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                               rot = 0, vjust = 0,hjust= 0.25,
                             gp = gpar(fontsize=32))),
             legend,ncol = 1, heights = c(4,4,5,1),
             left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,
                             gp = gpar(fontsize=32)))

dev.off()



#### Figure 9

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 9.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.diff.plot, 
                         point.wise.DF.BS.HT.diff.plot, 
                         point.wise.DF.PS.HT.diff.plot, 
                         point.wise.DF.SS.HT.diff.plot, ncol=4),
             arrangeGrob(point.wise.DF.RCS.DR.diff.plot, 
                         point.wise.DF.BS.DR.diff.plot, 
                         point.wise.DF.PS.DR.diff.plot, 
                         point.wise.DF.SS.DR.diff.plot, ncol=4),
             arrangeGrob(point.wise.DF.RCS.Comb.diff.plot, 
                         point.wise.DF.BS.Comb.diff.plot, 
                         point.wise.DF.PS.Comb.diff.plot, 
                         point.wise.DF.SS.Comb.diff.plot, ncol=4),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                               rot = 0, vjust = 0,hjust = 0.25,
                             gp = gpar(fontsize=32)),
             left = textGrob(label = "Treatment effect (Absolute risk difference)",
                             rot = 90, vjust = 1,
                             gp = gpar(fontsize=32)))

dev.off()

## ----MVmeta------------------------------------------------------------------------------------------------------
###########
##### First scenario 
###########

source("Multivariate meta-analysis/Multivariate meta-analysis (1st scenario).R")

###########
##### Second scenario 
###########

source("Multivariate meta-analysis/Multivariate meta-analysis (2nd scenario).R")

###########
##### Third scenario 
###########

source("Multivariate meta-analysis/Multivariate meta-analysis (2nd scenario).R")

### Clear enviroment 
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")])
########## Hereby the studies have different domains. 


df2%>%
  group_by(Study)%>%
  summarise(`Minimum BMI` =  min(BMI), `Maximum BMI` =  max(BMI))


### However in order to apply the splines described above we have to fit them in the same domain. 
### A work-around that has been proposed by Riley et al. (2020) is to generate pseudo data with 
### low weight near the boundaries of the effect modifier.
### These pseudo data will help us generate the beta coefficients we need to pool during the second stage. 
### In our case, the boundaries of BMI are 18.5 and 40. 

Lower.Boundary.Outcome.Control =  mean(df2[df2$BMI<19 & df2$Treatment=="Control",]$Y) ### ~27%
Lower.Boundary.Outcome.Treated =  mean(df2[df2$BMI<19 & df2$Treatment=="Treated",]$Y) ### ~13%

Upper.Boundary.Outcome.Control =  mean(df2[df2$BMI>39.5 & df2$Treatment=="Control",]$Y) ### ~71%
Upper.Boundary.Outcome.Treated =  mean(df2[df2$BMI>39.5 & df2$Treatment=="Treated",]$Y) ### ~43%


#### Now we generate pseudo data for all studies. Note that we don't need to be precise. 


tmp.lower =  do.call("rbind", replicate(4, df2[df2$BMI <19, ], simplify = FALSE))
tmp.lower$Study = rep(c("2nd Study", "3rd Study", "4th Study", "5th Study"), each =  dim(tmp.lower)[1]/4)

tmp.upper =  do.call("rbind", replicate(4, df2[df2$BMI >39.5, ], simplify = FALSE))
tmp.upper$Study = rep(c("2nd Study", "3rd Study", "4th Study", "5th Study"), each =  dim(tmp.upper)[1]/4)
tmp =  rbind(tmp.lower, tmp.upper)

rm(tmp.lower, tmp.upper, Lower.Boundary.Outcome.Control, Lower.Boundary.Outcome.Treated, Upper.Boundary.Outcome.Control, Upper.Boundary.Outcome.Treated)



########### RCS  indicator
###########

### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.df2 =   list(BMI=c(21, 25, 29, 31, 33.5, 37.8))

## The formula for all regions is the same so we save it 

formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = length(Knots.df2$BMI))

# Number of studies

nstudies.df2 = length(unique(df2$Study))

### Fit a stacked analysis in the whole data-set to get the model matrix
fit.df2 = gam( formula =formula,
               knots = Knots.df2,
               family = binomial("logit"), 
               data = df2)

### Get the model matrix from the stacked GAM.

X.p.df2 =  model.matrix(fit.df2)

### Create an empty matrix for the estimated coefficients and their variance covariance matrix

estimated.coefficients.df2 = matrix(NA,
                                    ncol = length(fit.df2$coefficients),nrow=nstudies.df2,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.df2)),1, paste, collapse=" "),
                                                     c(1:length(fit.df2$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.df2 = matrix(NA, ncol=sum(c(1:length(fit.df2$coefficients))), nrow = nstudies.df2 )

df2$wgt =  c(1)
tmp$wgt =  c(0.00001)
df2.plus =  rbind(df2, tmp)


k=3
j=1

for( i in unique(df2.plus$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2.plus%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,knots = Knots.df2, family = binomial("logit"),weights = wgt, data = minidf2)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.df2[j,] = fit$coefficients
  S.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
}
rm(k,j)




#### Perform for each region a multivariate meta-analysis
mv.fit.df2 = mvmeta(estimated.coefficients.df2, S.df2)


prediction.interval.mvmeta.df2 =  X.p.df2 %*% coef(mv.fit.df2)

prediction.interval.mvmeta.lower.df2 =  X.p.df2 %*% coef(mv.fit.df2) - sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.df2 =  X.p.df2 %*% coef(mv.fit.df2) + sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df.RCS = cbind(df2[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.df2 ))





g.mvmeta.total.RCS.df2 = ggplot(mvmeta.df.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.RCS.df2
###########

########### BS  indicator
###########

### Define the knots positions
Knots.df2 =   list(BMI=c(21, 25, 29, 31, 33.5, 37.8))

## The formula for all regions is the same so we save it 

formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = length(Knots.df2$BMI))

# Number of studies

nstudies.df2 = length(unique(df2$Study))

### Fit a stacked analysis in the whole data-set to get the model matrix
fit.df2 = gam( formula =formula,
               knots = Knots.df2,
               family = binomial("logit"), 
               data = df2)

### Get the model matrix from the stacked GAM.

X.p.df2 =  model.matrix(fit.df2)

### Create an empty matrix for the estimated coefficients and their variance covariance matrix

estimated.coefficients.df2 = matrix(NA,
                                    ncol = length(fit.df2$coefficients),nrow=nstudies.df2,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.df2)),1, paste, collapse=" "),
                                                     c(1:length(fit.df2$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.df2 = matrix(NA, ncol=sum(c(1:length(fit.df2$coefficients))), nrow = nstudies.df2 )


df2.plus =  rbind(df2, tmp.lower, tmp.upper)


k=3
j=1

for( i in unique(df2.plus$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2.plus%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,knots = Knots.df2, family = binomial("logit"), data = minidf2)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.df2[j,] = fit$coefficients
  S.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
}
rm(k,j)




#### Perform for each region a multivariate meta-analysis
mv.fit.df2 = mvmeta(estimated.coefficients.df2, S.df2)


prediction.interval.mvmeta.df2 =  X.p.df2 %*% coef(mv.fit.df2)

prediction.interval.mvmeta.lower.df2 =  X.p.df2 %*% coef(mv.fit.df2) - sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.df2 =  X.p.df2 %*% coef(mv.fit.df2) + sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df.RCS = cbind(df2[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.df2 ))





g.mvmeta.total.RCS.df2 = ggplot(mvmeta.df.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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


g.mvmeta.total.RCS.df2
###########




## ----GAMMs----------------------------------------------------------------------------------------------------------------
####################
##### First scenario
####################

source("GAMM/GAMM (1st scenario).R")

####################
##### Second scenario
####################

source("GAMM/GAMM (2nd scenario).R")

####################
##### Third scenario
####################
source("GAMM/GAMM (3rd scenario).R")

## Figure 12

grid.arrange(arrangeGrob(g.GAMM.HT,g.GAMM.DR,g.GAMM.Combined, ncol=1,
                         bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))), vjust=0.35,
                                          hjust = 0.35,gp = gpar(fontsize=32)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=32))), heights =c(12,1), g.legend)





## ----GAMMs Treatment effect plot------------------------------------------------------------------------------------------------------


#### Draw the plots
# Figure 13
grid.arrange(arrangeGrob(GAMM.DF.RCS.HT.diff.plot, 
                         GAMM.DF.BS.HT.diff.plot, 
                         GAMM.DF.PS.HT.diff.plot, 
                         GAMM.DF.SS.HT.diff.plot, ncol=4),
             arrangeGrob(GAMM.DF.RCS.DR.diff.plot, 
                         GAMM.DF.BS.DR.diff.plot, 
                         GAMM.DF.PS.DR.diff.plot, 
                         GAMM.DF.SS.DR.diff.plot, ncol=4),
             arrangeGrob(GAMM.DF.RCS.Comb.diff.plot, 
                         GAMM.DF.BS.Comb.diff.plot, 
                         GAMM.DF.PS.Comb.diff.plot, 
                         GAMM.DF.SS.Comb.diff.plot, ncol=4),
             bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                              hjust = 0,gp = gpar(fontsize=32)), 
             left = textGrob(label = "Treatment effect (Absolute risk difference)", rot = 90, vjust = 1,gp = gpar(fontsize=32)))





