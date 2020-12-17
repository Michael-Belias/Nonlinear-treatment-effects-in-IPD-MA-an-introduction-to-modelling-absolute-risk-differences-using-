

####################
##### First scenario
####################
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")]) ### To clear all environment besides the data-set
Knots.rcs.df1= list(BMI = quantile(df1$BMI , probs = c(0.05,0.275,0.5,0.725,0.95)))
Knots.ns.df1= c(25.66667 ,32.83333)







fit.RCS.HT = gam(Y ~BMI + Treatment + BMI*Treatment + 
                   s(BMI,by = Treatment,fx = T,bs="cr",k = 5) +  
                   s(Study,bs = "re") +  
                   s(Study,BMI,bs = "re")+  
                   s(Study,Treatment,bs = "re"),knots = Knots.rcs.df1,
                 family = binomial("logit"),data = df1, nthreads = 8, method = "REML")


fit.NS.HT = gam(Y ~BMI + Treatment + BMI*Treatment + 
                  ns(BMI, knots = Knots.ns.df1) +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df1, nthreads = 8, method = "REML")


fit.PS.HT = gam(Y ~BMI + Treatment + BMI*Treatment + 
                  s(BMI,by = Treatment,fx = F,bs="ps",k=17) +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df1, nthreads = 8, method = "REML")


fit.SS.HT = gam(Y ~BMI + Treatment + BMI*Treatment + 
                  s(BMI,by = Treatment,bs="tp") +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df1, nthreads = 8, method = "REML")


new.data = data.frame(cbind(df1[,c("Study","BMI","Treatment")]))


preds.RCS.HT=  predict.gam(fit.RCS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.NS.HT=  predict.gam(fit.NS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.PS.HT=  predict.gam(fit.PS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.SS.HT=  predict.gam(fit.SS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)




preds.RCS.HT$lower = preds.RCS.HT$fit -1.96*preds.RCS.HT$se.fit
preds.NS.HT$lower  = preds.NS.HT$fit  -1.96*preds.NS.HT$se.fit
preds.PS.HT$lower  = preds.PS.HT$fit  -1.96*preds.PS.HT$se.fit
preds.SS.HT$lower  = preds.SS.HT$fit -1.96*preds.SS.HT$se.fit

preds.RCS.HT$upper = preds.RCS.HT$fit +1.96*preds.RCS.HT$se.fit
preds.NS.HT$upper  = preds.NS.HT$fit +1.96*preds.NS.HT$se.fit
preds.PS.HT$upper  = preds.PS.HT$fit +1.96*preds.PS.HT$se.fit
preds.SS.HT$upper  = preds.SS.HT$fit +1.96*preds.SS.HT$se.fit







g.GAMM.RCS.HT= ggplot(preds.RCS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19,y=0.8, size = 10, label = "a") +ylim(c(0,1))


g.GAMM.NS.HT= ggplot(preds.NS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19,y=0.8, size = 10, label = "b") +ylim(c(0,1))


g.GAMM.PS.HT= ggplot(preds.PS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19,y=0.8, size = 10,label = "c") +ylim(c(0,1))


g.GAMM.SS.HT= ggplot(preds.SS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19,y=0.8, size = 10, label = "d") +ylim(c(0,1))


p1= ggplot(preds.SS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.title =element_blank(),
        legend.position = "bottom")+ 
  annotate("text",x = 19,y=0.8, size = 10, label = "d") +ylim(c(0,1))


g.legend =  get_legend(p1)


g.GAMM.HT=grid.arrange(g.GAMM.RCS.HT,g.GAMM.NS.HT,g.GAMM.PS.HT,g.GAMM.SS.HT, ncol=4, right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)))


preds.RCS.HT = preds.RCS.HT %>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))



preds.NS.HT= preds.NS.HT%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))



preds.PS.HT=preds.PS.HT%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))



preds.SS.HT=preds.SS.HT%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


## ----GAMMs Treatment effect plots------------------------------------------------------------------------------------------------------



source("Assisting functions/Create risk differences.R")


absolute_diff_RCS.HT = risk.diff.creator(dataframe = preds.RCS.HT,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                                         matching.variables = c("BMI"),
                                         predicted.outcome = "fit", 
                                         predicted.CI = c("Lower","Upper"))

absolute_diff_NS.HT = risk.diff.creator(dataframe = preds.NS.HT,
                                        treatment = "Treatment",
                                        outcome = NULL, 
                                        matching.variables = c("BMI"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_PS.HT = risk.diff.creator(dataframe = preds.PS.HT,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


absolute_diff_SS.HT = risk.diff.creator(dataframe = preds.SS.HT,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))






absolute_diff_RCS.HT=  absolute_diff_RCS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_NS.HT=  absolute_diff_NS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)



absolute_diff_PS.HT=  absolute_diff_PS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.HT=  absolute_diff_SS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

#### Draw the plots
GAMM.DF.RCS.HT.diff.plot = absolute_diff_RCS.HT%>%
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



GAMM.DF.NS.HT.diff.plot=absolute_diff_NS.HT%>%
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


GAMM.DF.PS.HT.diff.plot=absolute_diff_PS.HT%>%
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





GAMM.DF.SS.HT.diff.plot=absolute_diff_SS.HT%>%
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

