####################
##### Second scenario
####################
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")]) ### To clear all environment besides the data-set
Knots= list (BMI = (quantile(df2$BMI , probs = c(0.05,0.275,0.5,0.725,0.95))))



fit.RCS.DR = gam( Y~ BMI+ Treatment+ 
                    s(BMI,by = Treatment,bs="cr",fx = T,k = 5) +  
                    s(Study,bs = "re") +  
                    s(Study,BMI,bs = "re")+  
                    s(Study,Treatment,bs = "re"),knots = Knots,
                  family = binomial("logit"), data = df2, nthreads = 8, method = "REML")


fit.BS.DR = gam(Y~ BMI+ Treatment+ 
                  s(BMI,by = Treatment,fx = T,bs="bs",k=5, m=c(2,0)) +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df2, nthreads = 8, method = "REML")


fit.PS.DR = gam(Y~ BMI+ Treatment+ 
                  s(BMI,by = Treatment,bs="ps",k= 17) +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df2, nthreads = 8, method = "REML")

fit.SS.DR = gam(Y~ BMI+ Treatment+ 
                  s(BMI,by = Treatment,bs="tp") +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df2, nthreads = 8, method = "REML")

new.data = data.frame(cbind(df2[,c("Study","BMI","Treatment")]))


preds.RCS.DR=  predict.gam(fit.RCS.DR, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.BS.DR=  predict.gam(fit.BS.DR, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.PS.DR=  predict.gam(fit.PS.DR, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.SS.DR=  predict.gam(fit.SS.DR, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)


preds.RCS.DR$lower = preds.RCS.DR$fit -1.96*preds.RCS.DR$se.fit
preds.BS.DR$lower = preds.BS.DR$fit -1.96*preds.BS.DR$se.fit
preds.PS.DR$lower = preds.PS.DR$fit -1.96*preds.PS.DR$se.fit
preds.SS.DR$lower = preds.SS.DR$fit -1.96*preds.SS.DR$se.fit


preds.RCS.DR$upper = preds.RCS.DR$fit +1.96*preds.RCS.DR$se.fit
preds.BS.DR$upper = preds.BS.DR$fit +1.96*preds.BS.DR$se.fit
preds.PS.DR$upper = preds.PS.DR$fit +1.96*preds.PS.DR$se.fit
preds.SS.DR$upper = preds.SS.DR$fit +1.96*preds.SS.DR$se.fit


g.GAMM.RCS.DR=ggplot(preds.RCS.DR, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
  annotate("text",x = 19.25,y=0.8, size = 10, label = "a") +ylim(c(0,1))

g.GAMM.BS.DR=ggplot(preds.BS.DR, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
  annotate("text",x = 19.25,y=0.8, size = 10, label = "b") +ylim(c(0,1))


g.GAMM.PS.DR=ggplot(preds.PS.DR, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
  annotate("text",x = 19.25,y=0.8, size = 10, label = "c") +ylim(c(0,1))


g.GAMM.SS.DR=ggplot(preds.SS.DR, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
  geom_line(size=2) + 
  geom_ribbon(mapping = aes(ymin = expit(lower),ymax=expit(upper)),alpha=0.2)+ 
  ylab("")+xlab("") + scale_color_jama()+  
  theme_bw()+ 
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
  annotate("text",x = 19.25,y=0.8, size = 10, label = "d") +ylim(c(0,1))


g.GAMM.DR=grid.arrange(g.GAMM.RCS.DR,g.GAMM.BS.DR,g.GAMM.PS.DR,g.GAMM.SS.DR, ncol=4,right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)))



preds.RCS.DR= preds.RCS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.BS.DR = preds.BS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))

preds.PS.DR=preds.PS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))

preds.SS.DR=preds.SS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))

## ----GAMMs Treatment effect plots------------------------------------------------------------------------------------------------------

source("Assisting functions/Create risk differences.R")



absolute_diff_RCS.DR = risk.diff.creator(dataframe = preds.RCS.DR,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                                         matching.variables = c("BMI"),
                                         predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



absolute_diff_BS.DR = risk.diff.creator(dataframe = preds.BS.DR,
                                        treatment = "Treatment",outcome = NULL, 
                                        matching.variables = c("BMI"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))





absolute_diff_PS.DR = risk.diff.creator(dataframe = preds.PS.DR,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))





absolute_diff_SS.DR = risk.diff.creator(dataframe = preds.SS.DR,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))




absolute_diff_RCS.DR=  absolute_diff_RCS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_PS.DR=  absolute_diff_PS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_SS.DR=  absolute_diff_SS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_BS.DR=  absolute_diff_BS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


GAMM.DF.RCS.DR.diff.plot = absolute_diff_RCS.DR%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.2, size = 10, label = "a")+ ylim(c(-0.8,0.3))



GAMM.DF.BS.DR.diff.plot=absolute_diff_BS.DR%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.2, size = 10, label = "b")+ ylim(c(-0.8,0.3))


GAMM.DF.PS.DR.diff.plot=absolute_diff_PS.DR%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.2, size = 10,label = "c")+ ylim(c(-0.8,0.3))


GAMM.DF.SS.DR.diff.plot=absolute_diff_SS.DR%>%
  ggplot(aes(x = BMI,fit.diff)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.2, size = 10, label = "d")+ ylim(c(-0.8,0.3))
