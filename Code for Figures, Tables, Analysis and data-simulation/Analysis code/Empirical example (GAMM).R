## ----GAMM meta-analysis on empirical example  ------------------------------------------------------------------------------------------------------



### Ommit Little Study since they don't provide bilaterality information
miniIPD= IPDMA%>% filter((study != "Little"))

# make the bilat variable a factor for graphs. 
miniIPD$bilat_0 =  factor(miniIPD$bilat_0 , labels = c("Unilateral","Bilateral") )
Knots.RCS.miniIPD =   quantile(miniIPD$age, probs = c(0.1,0.5,0.9))
Knots.NS.miniIPD =   c(2.5)


formula.RCS.empirical =  poutcome ~   treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 +
  rcspline.eval(age, knots = Knots.RCS.miniIPD, inclx = F)*treat +
  rcspline.eval(age, knots = Knots.RCS.miniIPD, inclx = F)*bilat_0+ 
  s(age,by =  as.numeric(study),  bs="re")+
  s(treat,by = bilat_0, bs= "fs")+
  s(treat,by =  as.numeric(study),  bs="re")+
  s(bilat_0,by =  as.numeric(study),  bs="re")


formula.NS.empirical =  poutcome ~   treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 + 
  ns(age,knots = Knots.NS.miniIPD)*treat+
  ns(age,knots = Knots.NS.miniIPD)*bilat_0+ 
  s(age,by =  as.numeric(study),  bs="re")+
  s(treat,by = bilat_0, bs= "fs")+
  s(treat,by =  as.numeric(study),  bs="re")+
  s(bilat_0,by =  as.numeric(study),  bs="re")


fit.RCS <- gam(formula = formula.RCS.empirical, 
               family = binomial("logit"),
               data = miniIPD, 
               method="REML")


fit.NS <- gam(formula = formula.NS.empirical,
              family = binomial("logit"), 
              data = miniIPD,
              method="REML")

fit.PS <- gam(formula = poutcome ~  treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 + 
                s(age, by = bilat_0, bs="ps", k=17)+
                s(age, by = treat, bs="ps", k=17) + 
                s(treat,by = bilat_0, bs= "fs")+
                s(age,by =  as.numeric(study),  bs="re")+
                s(treat,by =  as.numeric(study),  bs="re")+
                s(bilat_0,by =  as.numeric(study),  bs="re"), 
              family = binomial("logit"), data = miniIPD, 
              method="REML")


fit.SS <-  gam(formula = poutcome ~   treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 + 
                 s(age, by = bilat_0, bs="tp")+
                 s(age, by = treat, bs="tp") + 
                 s(treat,by = bilat_0, bs= "fs")+
                 s(age,by =  as.numeric(study),  bs="re")+
                 s(treat,by =  as.numeric(study),  bs="re")+
                 s(bilat_0,by =  as.numeric(study),  bs="re"), 
               family = binomial("logit"), data = miniIPD, 
               method="REML")

#### Create a nice data-set to make predictions and plots 
new.dat= data.frame(age= rep(seq(0,9,length.out = 50),each = 20),
                    treat = rep(unique(miniIPD$treat),500), 
                    bilat_0 =  rep(rep(unique(miniIPD$bilat_0),each=2),250),
                    study =  rep(rep(unique(miniIPD$study),each =4),50)
)
expit<-function(rs) {1/(1+exp(-rs))}

exclude.parameters= c('s(age):as.numeric(study)',   
                      's(treat):as.numeric(study)',  
                      's(bilat_0):as.numeric(study)')


preds.RCS=  predict.gam(fit.RCS, se.fit = T,newdata = new.dat,
                        newdata.guaranteed = T, 
                        exclude = exclude.parameters)%>%
  as.data.frame()%>%cbind(new.dat)

preds.RCS$lower =  preds.RCS$fit - 1.96*preds.RCS$se.fit;preds.RCS$lower = expit(preds.RCS$lower)
preds.RCS$upper =  preds.RCS$fit + 1.96*preds.RCS$se.fit;preds.RCS$upper = expit(preds.RCS$upper)
preds.RCS$fit   =  expit(preds.RCS$fit)



preds.NS=  predict.gam(fit.NS, se.fit = T,
                       newdata = new.dat,newdata.guaranteed = T, 
                       exclude = exclude.parameters)%>%
  as.data.frame()%>%cbind(new.dat)

preds.NS$lower =  preds.NS$fit - 1.96*preds.NS$se.fit;preds.NS$lower = expit(preds.NS$lower)
preds.NS$upper =  preds.NS$fit + 1.96*preds.NS$se.fit;preds.NS$upper = expit(preds.NS$upper)
preds.NS$fit   =  expit(preds.NS$fit)


preds.PS=  predict.gam(fit.PS, se.fit = T,newdata = new.dat,
                       newdata.guaranteed = T, 
                       exclude = exclude.parameters)%>%
  as.data.frame()%>%cbind(new.dat)

preds.PS$lower =  preds.PS$fit - 1.96*preds.PS$se.fit;preds.PS$lower = expit(preds.PS$lower)
preds.PS$upper =  preds.PS$fit + 1.96*preds.PS$se.fit;preds.PS$upper = expit(preds.PS$upper)
preds.PS$fit   =  expit(preds.PS$fit)


preds.SS=  predict.gam(fit.SS, se.fit = T,newdata = new.dat,
                       newdata.guaranteed = T, 
                       exclude = exclude.parameters)%>%
  as.data.frame()%>%cbind(new.dat)

preds.SS$lower =  preds.SS$fit - 1.96*preds.SS$se.fit;preds.SS$lower = expit(preds.SS$lower)
preds.SS$upper =  preds.SS$fit + 1.96*preds.SS$se.fit;preds.SS$upper = expit(preds.SS$upper)
preds.SS$fit   =  expit(preds.SS$fit)


### Exlcude random effects to get the pooled effect

g.GAMM.RCS = ggplot(preds.RCS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.RCS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("a", ""),bilat_0   = levels(preds.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18) + ylim(c(0,1))


g.GAMM.NS = ggplot(preds.NS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.NS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("b", ""),bilat_0   = levels(preds.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F,size=18)+ ylim(c(0,1))


g.GAMM.PS =ggplot(preds.PS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.PS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("c", ""),bilat_0   = levels(preds.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18)+ ylim(c(0,1))


g.GAMM.SS = ggplot(preds.SS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.SS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("d", ""),bilat_0   = levels(preds.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18)+ ylim(c(0,1))

p1= ggplot(preds.SS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.SS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom") + 
  geom_text(data= data.frame( label = c("", "d"),bilat_0   = preds.SS$bilat_0[c(1,3)]),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F,  size=18)

p1
g.legend =  get_legend(p1)

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 18.png",width = 1240, height = 1680) 
g.GAMM=grid.arrange(arrangeGrob(
  g.GAMM.RCS,
  g.GAMM.NS,
  g.GAMM.PS,
  g.GAMM.SS, ncol=2,
  left = textGrob(label = "Risk of developing fever after 1 week",
                  rot = 90,vjust = 0.5,gp = gpar(fontsize=48)),
  bottom = textGrob(label = "Children's age", 
                    vjust = 0,gp = gpar(fontsize=48))),
  g.legend ,heights = c(10,1))

dev.off()


source("Assisting functions/Create risk differences.R")



predictions.diff.RCS  = risk.diff.creator(dataframe = preds.RCS,treatment = "treat",
                                          matching.variables = c("age","bilat_0"),
                                          outcome= NULL, 
                                          predicted.outcome = "fit", 
                                          predicted.CI = c("lower","upper")
)

predictions.diff.NS  = risk.diff.creator(dataframe = preds.NS,treatment = "treat",
                                         matching.variables = c("age","bilat_0"),
                                         outcome= NULL, 
                                         predicted.outcome = "fit", 
                                         predicted.CI = c("lower","upper")
)
predictions.diff.PS  = risk.diff.creator(dataframe = preds.PS,treatment = "treat",
                                         matching.variables = c("age","bilat_0"),
                                         outcome= NULL, 
                                         predicted.outcome = "fit", 
                                         predicted.CI = c("lower","upper")
)
predictions.diff.SS  = risk.diff.creator(dataframe = preds.SS,treatment = "treat",
                                         matching.variables = c("age","bilat_0"),
                                         outcome= NULL, 
                                         predicted.outcome = "fit", 
                                         predicted.CI = c("lower","upper")
)





g.GAMM.RCS.diff = ggplot(predictions.diff.RCS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.RCS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("a", ""),
                              bilat_0   = levels(predictions.diff.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18)+ ylim(c(-1,1))


g.GAMM.NS.diff = ggplot(predictions.diff.NS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.NS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("b", ""),
                              bilat_0   = levels(predictions.diff.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18)+ ylim(c(-1,1))



g.GAMM.PS.diff = ggplot(predictions.diff.PS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.PS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("c", ""),
                              bilat_0   = levels(predictions.diff.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18)+ ylim(c(-1,1))




g.GAMM.SS.diff = ggplot(predictions.diff.SS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+ geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.SS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("d", ""),
                              bilat_0   = levels(predictions.diff.RCS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), 
            inherit.aes = F, size=18)+ ylim(c(-1,1))






png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 19.png",width = 1240, height = 1680)
g.GAMM.diff =grid.arrange(
  g.GAMM.RCS.diff,
  g.GAMM.NS.diff,
  g.GAMM.PS.diff,
  g.GAMM.SS.diff, ncol=2,
  left = textGrob(label = "Antibiotics effect",
                  rot = 90,vjust = 0.5,gp = gpar(fontsize=48)),
  bottom = textGrob(label = "Children's age", 
                    vjust = 0,gp = gpar(fontsize=48)))

dev.off()
