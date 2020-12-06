### Single study scenario analysis and plots

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Single study dataset.R")

### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


## Define knots positions for restricted cubic splines
Knots.RCS =   list(BMI=quantile(single.df$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))) 


## Save the formulas for the splines methods
formula.RCS =Y~ BMI +Treatment +  s(BMI,      ### Continuous variable
                                    bs ="cr", ###  Specification of Restricted splines 
                                    fx=T,     ###  Logical value (True/False) whether the spline will be fixed or penalised
                                    by = Treatment, ### using the by argument we introduce interactions 
                                    k = 5)    ### k is the complexity of the model (in RCS the complexity is equal to the number of the knots)

formula.BS =Y~ BMI + Treatment+   s(BMI,      ### Continuous variable
                                    bs = "bs",###  Specification of B-splines  
                                    fx = T,   ###  Logical value (True/False) whether the spline will be fixed or penalised
                                    by = Treatment,  ### using the by argument we introduce interactions
                                    k = 5,    ### k is the complexity of the model (in B-splines the complexity is equal to the number of internal knots + degree of B-spline + 1)
                                    m=c(2,0)  ### Degree of B-spline  -  degree of derivative based penalisation (here since we don't have penalisation we have 0)
                                    ) 


formula.PS =Y ~ BMI + Treatment + s(BMI,      ### Continuous variable
                                    bs="ps",  ###  Specification of P-splines   
                                    k=19,     ### k is the complexity of the model
                                    fx=F,     ###  Logical value (True/False) whether the spline will be fixed or penalised
                                    by = Treatment,### using the by argument we introduce interactions
                                    m=c(2,3), ### Degree of B-spline  -  degree of derivative based penalisation
                                    sp =-1    ### smoothing parameters.Negative value signals auto-initialization
                                    )   



formula.SS =  Y ~ BMI + Treatment+ s(BMI,    ### Continuous variable
                                     bs="tp",###  Specification of Smoothing splines (equivalent to smoothing splines in one dimensional X) 
                                     fx=F,   ###  Logical value (True/False) whether the spline will be fixed or penalised
                                     by = Treatment,### using the by argument we introduce interactions
                                     sp = -1    ### smoothing parameters.Negative value signals auto-initialization
                                     )


## Fit models
fit.RCS = gam( formula =formula.RCS, ### The formula 
               knots = Knots.RCS,    ### The positions of knots
               family = binomial("logit"), ### the link function
               data = single.df,     ### data-set
               nthreads = 8,         ### Multi-threading option
               method = "GCV.Cp"     ### 	 The smoothing parameter estimation method. "GCV.Cp" uses Generalised Cross-validation based on Cp Mallows
               )

fit.BS = gam( formula =formula.BS,  
              family = binomial("logit"),
              data = single.df, 
              nthreads = 8, 
              method = "GCV.Cp")

fit.PS = gam(formula = formula.PS,
             family = binomial("logit"), 
             data = single.df, 
             nthreads = 8, 
             method = "GCV.Cp")

fit.SS = gam(formula = formula.SS,
             family = binomial("logit"), 
             data = single.df, 
             nthreads = 8, 
             method = "GCV.Cp")


## save predicted outcomes
preds.RCS = as.data.frame(predict.gam(fit.RCS, se.fit = T))
preds.BS = as.data.frame(predict.gam(fit.BS, se.fit = T))
preds.PS = as.data.frame(predict.gam(fit.PS, se.fit = T))
preds.SS = as.data.frame(predict.gam(fit.SS, se.fit = T))

## Calculate lower confidence interval of the regression lines
preds.RCS$Lower = preds.RCS$fit - 1.96*preds.RCS$se.fit
preds.BS$Lower  = preds.BS$fit  - 1.96*preds.BS$se.fit
preds.PS$Lower  = preds.PS$fit  - 1.96*preds.PS$se.fit
preds.SS$Lower  = preds.SS$fit  - 1.96*preds.SS$se.fit

## Calculate upper confidence interval of the regression lines
preds.RCS$Upper = preds.RCS$fit + 1.96*preds.RCS$se.fit
preds.BS$Upper  = preds.BS$fit  + 1.96*preds.BS$se.fit
preds.PS$Upper  = preds.PS$fit  + 1.96*preds.PS$se.fit
preds.SS$Upper  = preds.SS$fit  + 1.96*preds.SS$se.fit

### The predicted outcome are on logit scale
### Omit the standard error column and backtransform the predicted outcomes and their corresponing confidence intervals 
preds.RCS = preds.RCS%>%
  select(-contains("se.fit"))%>%
  apply(. , 2, expit)

preds.BS = preds.BS%>%
  select(-contains("se.fit"))%>%
  apply(. , 2, expit)

preds.PS = preds.PS%>%
  select(-contains("se.fit"))%>%
  apply(. , 2, expit)

preds.SS = preds.SS%>%
  select(-contains("se.fit"))%>%
  apply(. , 2, expit)


### Combine with the single study data-set
preds.RCS =cbind(single.df, preds.RCS)
preds.BS  =cbind(single.df, preds.BS)
preds.PS  =cbind(single.df, preds.PS)
preds.SS  =cbind(single.df, preds.SS)



### Draw the B-spline plot
gRCS= ggplot(preds.RCS,aes(x = BMI, y = `Mortality risk`, color= Treatment))+
  geom_line(linetype= 5,size=2) + 
  geom_line(mapping = aes(x = BMI, fit,  color= Treatment))+ 
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  geom_vline(xintercept = fit.RCS$smooth[[1]]$xp, linetype =2)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "a") +ylim(c(0,1))


### Calculate the number of knots
n= length(fit.BS$smooth[[1]]$knots)
### Draw the B-spline plot
gBS= ggplot(preds.BS,aes(x = BMI, y = `Mortality risk`, color= Treatment))+
  geom_line(linetype= 5,size=2) + 
  geom_line(mapping = aes(x = BMI, fit,  color= Treatment))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  geom_vline(xintercept = fit.BS$smooth[[1]]$knots[-c(1:2,(n-1):n)], linetype =2)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "b") +ylim(c(0,1))

### Calculate the number of knots
n= length(fit.PS$smooth[[1]]$knots)

### Draw the P-spline plot
gPS= ggplot(preds.PS,aes(x = BMI, y = `Mortality risk`, color= Treatment))+
  geom_line(linetype= 5,size=2) + 
  geom_line(mapping = aes(x = BMI, fit,  color= Treatment))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  geom_vline(xintercept = fit.PS$smooth[[1]]$knots[-c(1:3,(n-2):n)], linetype =2) +
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "c") +ylim(c(0,1))

### Draw the Smoothing spline plot
gSS= ggplot(preds.SS,aes(x = BMI, y = `Mortality risk`, color= Treatment))+
  geom_line(linetype= 5,size=2) + 
  geom_line(mapping = aes(x = BMI, fit,  color= Treatment))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "d") +ylim(c(0,1))

### Create an pseudo plot to get the legend 
p1=  ggplot(preds.SS,aes(x = BMI, y = `Mortality risk`, color= Treatment))+
  geom_line(linetype= 5,size=2) + 
  geom_line(mapping = aes(x = BMI, fit, color= Treatment))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
        legend.title =element_blank(),
        legend.position = "bottom") + 
  annotate("text",x = 19,y=0.75, size = 10, label = "d") +ylim(c(0,1))

### Get the legend from the pseudo-plot 
legend = gtable_filter(ggplotGrob(p1), "guide-box") 



png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 6.png",width = 1240, height = 1680) 
##### Plot all the plots in a grid
grid.arrange(arrangeGrob(gRCS,gBS,gPS,gSS,
                         bottom= textGrob(label = expression(BMI (kg/m^2)), hjust = 0,gp = gpar(fontsize=32)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=32))), 
             legend, heights= c(10,1))
dev.off()



##### Estimate and draw the risk differences 

source("Assisting functions/Create risk differences.R")



absolute_diff_RCS = risk.diff.creator(dataframe = preds.RCS,treatment = "Treatment", outcome = "Mortality risk",
                                      matching.variables = c("BMI","BMI.standardised"),
                                      predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


absolute_diff_BS = risk.diff.creator(dataframe = preds.BS,treatment = "Treatment",outcome = "Mortality risk", 
                                     matching.variables = c("BMI","BMI.standardised"),
                                     predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



absolute_diff_PS = risk.diff.creator(dataframe = preds.PS,treatment = "Treatment", outcome = "Mortality risk",
                                     matching.variables = c("BMI","BMI.standardised"),
                                     predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



absolute_diff_SS = risk.diff.creator(dataframe = preds.SS,treatment = "Treatment", outcome = "Mortality risk",
                                     matching.variables = c("BMI","BMI.standardised"),
                                     predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_RCS$`Mortality risk.diff` = 
  absolute_diff_RCS$`Mortality risk.y` - absolute_diff_RCS$`Mortality risk.x`
absolute_diff_BS$`Mortality risk.diff`  = 
  absolute_diff_BS$`Mortality risk.y` - absolute_diff_BS$`Mortality risk.x`
absolute_diff_PS$`Mortality risk.diff`  =
  absolute_diff_PS$`Mortality risk.y` - absolute_diff_PS$`Mortality risk.x`
absolute_diff_SS$`Mortality risk.diff`  =
  absolute_diff_SS$`Mortality risk.y` - absolute_diff_SS$`Mortality risk.x`





gRCS_absolute_diff= ggplot(absolute_diff_RCS,aes(x = BMI, fit.diff)) +geom_line()+ 
  geom_line(aes(x = BMI, `Mortality risk.diff`), linetype= 2, size=2)+ 
  geom_hline(yintercept = 0, linetype=4)+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.15)+
  geom_vline(xintercept = fit.RCS$smooth[[1]]$xp, linetype =2)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "a") + ylim(c(-1,1))

n= length(fit.BS$smooth[[1]]$knots)

gBS_absolute_diff= ggplot(absolute_diff_BS,aes(x = BMI, fit.diff))+geom_line()+ 
  geom_line(aes(x = BMI, `Mortality risk.diff`), linetype= 2, size=2)+  
  geom_line()+ geom_hline(yintercept = 0, linetype=4)+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.15)+
  geom_vline(xintercept = fit.BS$smooth[[1]]$knots[-c(1:3,(n-2):n)], linetype =2)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "b")  + ylim(c(-1,1))

n= length(fit.PS$smooth[[1]]$knots)
gPS_absolute_diff= ggplot(absolute_diff_PS,aes(x = BMI, fit.diff)) +geom_line()+ 
  geom_line(aes(x = BMI, `Mortality risk.diff`), linetype= 2, size=2)+ 
  geom_line()+ geom_hline(yintercept = 0, linetype=4)+
  geom_vline(xintercept = fit.PS$smooth[[1]]$knots[-c(1:3,(n-2):n)], linetype =2) +
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.15)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "c")  + ylim(c(-1,1))


gSS_absolute_diff= ggplot(absolute_diff_SS,aes(x = BMI, fit.diff)) +geom_line()+ 
  geom_line(aes(x = BMI, `Mortality risk.diff`), linetype= 2, size=2)+  
  geom_line()+ geom_hline(yintercept = 0, linetype=4)+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.15)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
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
  annotate("text",x = 19,y=0.75, size = 10, label = "d")  + ylim(c(-1,1))



##### Plot all the plots in a grid



png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 7.png",width = 1240, height = 1680) 
##### Plot all the plots in a grid
grid.arrange(gRCS_absolute_diff,gBS_absolute_diff,gPS_absolute_diff,gSS_absolute_diff,
            bottom= textGrob(label = expression(BMI (kg/m^2)), hjust = 0.35,gp = gpar(fontsize=32)), 
            left = textGrob(label = "Treatment effect plot (absolute risk differences)", rot = 90, vjust = 1,gp = gpar(fontsize=32)))

dev.off()




