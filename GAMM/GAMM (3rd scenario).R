####################
##### Third scenario
####################


source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")
#rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")]) ### To clear all environment besides the data-set
Knots= list (BMI = (quantile(df3$BMI , probs = c(0.05,0.275,0.5,0.725,0.95))))



fit.RCS.Combined = gam(Y~  BMI + Treatment + BMI*Treatment + 
                         s(BMI,by = Treatment,bs="cr",fx = T, k = 5) +  
                         s(Study,bs = "re") +  
                         s(Study,BMI,bs = "re")+  
                         s(Study,Treatment,bs = "re"),knots = Knots,
                       family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.BS.Combined = gam(Y~  BMI + Treatment + BMI*Treatment + 
                        s(BMI,by = Treatment,fx = T,bs="bs",k=5, m=c(2,0)) +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.PS.Combined = gam(Y~  BMI + Treatment + BMI*Treatment + 
                        s(BMI,by = Treatment,bs="ps", k = 19) +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.SS.Combined = gam(Y~  BMI + Treatment + BMI*Treatment + 
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19,y=0.8, size = 10, label = "a") +ylim(c(0,1))


g.GAMM.BS.Combined=ggplot(preds.BS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19,y=0.8, size = 10, label = "b") +ylim(c(0,1))


g.GAMM.PS.Combined=ggplot(preds.PS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19,y=0.8, size = 10,label = "c") +ylim(c(0,1))


g.GAMM.SS.Combined=ggplot(preds.SS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  annotate("text",x = 19,y=0.8, size = 10, label = "d") +ylim(c(0,1))



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



source("Assisting functions/Create risk differences.R")

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





## ----GAMMs Treatment effect plot------------------------------------------------------------------------------------------------------




GAMM.DF.RCS.Comb.diff.plot = absolute_diff_RCS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10, label = "a")+ ylim(c(-1,1))




GAMM.DF.BS.Comb.diff.plot=absolute_diff_BS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10, label = "b")+ ylim(c(-1,1))


GAMM.DF.PS.Comb.diff.plot=absolute_diff_PS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10,label = "c")+ ylim(c(-1,1))


GAMM.DF.SS.Comb.diff.plot=absolute_diff_SS.Comb%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") +  
  annotate("text",x = 19.25,y=0.75, size = 10, label = "d")+ ylim(c(-1,1))




grid.arrange(arrangeGrob(GAMM.DF.RCS.Comb.diff.plot, 
                         GAMM.DF.BS.Comb.diff.plot, 
                         GAMM.DF.PS.Comb.diff.plot, 
                         GAMM.DF.SS.Comb.diff.plot, ncol=4),
             bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                              hjust = 0,gp = gpar(fontsize=32)), 
             left = textGrob(label = "Treatment effect (Absolute risk difference)", rot = 90, vjust = 1,gp = gpar(fontsize=32)))
