## ----Pointwise meta-analysis on empirical example ------------------------------------------------------------------------------------------------------


### Gererate the prediction data.frames

source("Assisting functions/Point-wise meta-analysis.R")


### Ommit Appelman due to limited number of events
miniIPD.splines= miniIPD%>% filter((  study != "Appelman"))
miniIPD.linear= miniIPD%>% filter((   study == "Appelman"))


linear.Models =  miniIPD.linear%>%
  droplevels(miniIPD.linear$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = glm(formula = poutcome ~  treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0, 
                 family = binomial("logit"), data = .))



RCS = miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = glm(formula = poutcome ~   treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 +
                   rcspline.eval(age, knots = quantile(.$age, probs = c(0.1,0.5,0.9)))*treat +
                   rcspline.eval(age, knots = quantile(.$age, probs = c(0.1,0.5,0.9)))*bilat_0, 
                 family = binomial("logit"), data = .))%>%
  rbind(., linear.Models)



NS = miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = glm(formula = poutcome ~  treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 + 
                   ns(age,df = 3)*treat+
                   ns(age,df = 3)*bilat_0,
                 family = binomial("logit"),
                 data = .))%>%
  rbind(., linear.Models)



PS = miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = gam(formula = poutcome ~  treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 + 
                   s(age, by = treat,   bs="ps",fx = F,k=17)+
                   s(age, by = bilat_0, bs="ps",fx = F,k=17), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))%>%
  rbind(., linear.Models)

SS= miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = gam(formula = poutcome ~  treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 + 
                   s(age, by = treat,   bs="tp",fx = F,k=5)+
                   s(age, by = bilat_0, bs="tp",fx = F,k=5), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))%>%
  rbind(., linear.Models)


#### Create a nice data-set to make predictions and plots 



new.dat= data.frame(age= rep(seq(0,9,length.out = 50),each = 20),
                    treat = rep(unique(miniIPD$treat),500), 
                    bilat_0 =  rep(rep(unique(miniIPD$bilat_0),each=2),250),
                    study =  rep(rep(unique(miniIPD$study),each =4),50)
)



predictions.linear= new.dat%>%
  droplevels(new.dat$study)%>% 
  arrange(desc(study))%>%
  group_by(study)%>%
  nest()%>%
  full_join(linear.Models, by = c("study")) %>% 
  group_by(study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T))

ggplot(predictions.linear, aes(age,expit(.fitted), color= treat) ) +
  geom_line()  + 
  geom_ribbon(data = predictions.linear,aes(ymin= expit(.fitted - 1.96*.se.fit),ymax= expit(.fitted + 1.96*.se.fit)),alpha= 0.1)+ 
  facet_wrap(~study + bilat_0)


predictions.RCS= new.dat%>%
  droplevels(new.dat$study)%>% 
  arrange(desc(study))%>%
  group_by(study)%>%
  nest()%>%
  full_join(RCS, by = c("study")) %>% 
  group_by(study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit=T)) 

predictions.RCS[predictions.RCS$study != "Appelman",]%>%
  ggplot( aes(age,expit(.fitted), color= treat) ) + 
  geom_line() + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit), ymax = expit(.fitted + 1.96*.se.fit) , fill= treat),alpha= 0.3)+
  facet_wrap(~study + bilat_0) + scale_color_jama()+ scale_fill_jama()+
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
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom")


predictions.NS= new.dat%>%
  droplevels(miniIPD$study)%>% 
  arrange(desc(study))%>%
  group_by(study)%>%
  nest()%>%
  full_join(NS, by = c("study")) %>% 
  group_by(study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit=T)) 



predictions.NS[predictions.NS$study != "Appelman",]%>%
  ggplot( aes(age,expit(.fitted), color= treat) ) + 
  geom_line() + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit), ymax = expit(.fitted + 1.96*.se.fit) , fill= treat),alpha= 0.3)+
  facet_wrap(~study + bilat_0) + scale_color_jama()+ scale_fill_jama()+
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
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom")


predictions.PS= new.dat%>%
  droplevels(miniIPD$study)%>% 
  arrange(desc(study))%>%
  group_by(study)%>%
  nest()%>%
  full_join(PS, by = "study") %>% 
  group_by(study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit=T)) 

predictions.PS[predictions.PS$study != "Appelman",]%>%
  ggplot( aes(age,expit(.fitted), color= treat) ) + 
  geom_line() + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit), ymax = expit(.fitted + 1.96*.se.fit) , fill= treat),alpha= 0.3)+
  facet_wrap(~study + bilat_0) + scale_color_jama()+ scale_fill_jama()+
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
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom")



predictions.SS= new.dat%>%
  droplevels(miniIPD$study)%>% 
  arrange(desc(study))%>%
  group_by(study)%>%
  nest()%>%
  full_join(SS, by = "study") %>% 
  group_by(study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit=T)) 


predictions.SS[predictions.SS$study != "Appelman",]%>%
  ggplot( aes(age,expit(.fitted), color= treat) ) + 
  geom_line() + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit), ymax = expit(.fitted + 1.96*.se.fit) , fill= treat),alpha= 0.3)+
  facet_wrap(~study + bilat_0) + scale_color_jama()+ scale_fill_jama()+
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
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom")


source("Assisting functions/Point-wise meta-analysis(empirical example).R")



point.wise.DF.RCS = pointwise.ma(data = predictions.RCS,
                                 clustering.variable = "study",
                                 combining.variables = c("treat","bilat_0","age"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")



point.wise.DF.RCS$RE.meta =  expit(point.wise.DF.RCS$RE.meta )
point.wise.DF.RCS$RE.meta.upper =  expit(point.wise.DF.RCS$RE.meta.upper )
point.wise.DF.RCS$RE.meta.lower =  expit(point.wise.DF.RCS$RE.meta.lower )


point.wise.DF.NS =  pointwise.ma(data = predictions.NS,
                                 clustering.variable = "study",
                                 combining.variables = c("treat","bilat_0","age"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")




point.wise.DF.NS$RE.meta  =  expit(point.wise.DF.NS$RE.meta )
point.wise.DF.NS$RE.meta.upper  =  expit(point.wise.DF.NS$RE.meta.upper )
point.wise.DF.NS$RE.meta.lower  =  expit(point.wise.DF.NS$RE.meta.lower)



point.wise.DF.PS =  pointwise.ma(data = predictions.PS,
                                 clustering.variable = "study",
                                 combining.variables = c("treat","bilat_0","age"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")


point.wise.DF.PS$RE.meta  =  expit(point.wise.DF.PS$RE.meta )
point.wise.DF.PS$RE.meta.lower  =  expit(point.wise.DF.PS$RE.meta.lower)
point.wise.DF.PS$RE.meta.upper  =  expit(point.wise.DF.PS$RE.meta.upper )



point.wise.DF.SS =  pointwise.ma(data = predictions.SS,
                                 clustering.variable = "study",
                                 combining.variables = c("treat","bilat_0","age"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")



point.wise.DF.SS$RE.meta.upper  =  expit(point.wise.DF.SS$RE.meta.upper )
point.wise.DF.SS$RE.meta  =  expit(point.wise.DF.SS$RE.meta )
point.wise.DF.SS$RE.meta.lower  =  expit(point.wise.DF.SS$RE.meta.lower)



### Here are the plots for the point-wise meta-analysis

point.wise.DF.RCS.plot = point.wise.DF.RCS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("a", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0, y = 0.75, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size=18)+ ylim(c(0,1))



p1=  ggplot(point.wise.DF.RCS,aes(x = age, y = RE.meta, linetype =treat, color = treat))+ 
  facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama()+ 
  scale_linetype_discrete()+ylab("Risk of fever") + 
  xlab("Children's age")+  theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=28, hjust = 0), 
        legend.title =element_blank(), 
        legend.position = "bottom")+ ylim(c(0,1))

p1
legend = gtable_filter(ggplotGrob(p1), "guide-box") 


point.wise.DF.NS.plot = point.wise.DF.NS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("b", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0, y = 0.75, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size=18)+ ylim(c(0,1))


point.wise.DF.PS.plot = point.wise.DF.PS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("c", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0, y = 0.75, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size=18)+ ylim(c(0,1))



point.wise.DF.SS.plot = point.wise.DF.SS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none")+ 
  geom_text(data= data.frame( label = c("d", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0, y = 0.75, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size=18)+ ylim(c(0,1))

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 14.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(point.wise.DF.RCS.plot, point.wise.DF.NS.plot, point.wise.DF.PS.plot, point.wise.DF.SS.plot, ncol=2,
                         top = textGrob("", vjust = 1, gp = gpar(fontface = "bold", fontsize = 50)),
                         left = textGrob("Risk of fever after 3-7 days", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold", fontsize = 32)),
                         bottom = textGrob("Children's Age", vjust = 0,hjust=0.5, 
                                           gp = gpar(fontface = "bold", fontsize = 32))),
             legend, heights=  c(10,1))

dev.off()

## ----Pointwise meta-analysis on absolute risk differences (AOM) ------------------------------------------------------------------------------------------------------

source("Assisting functions/Create risk differences.R")



### RCS

predictions.RCS= predictions.RCS %>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(age =  as.character(age))

### B-splines

predictions.NS=predictions.NS%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(age =  as.character(age))

### P-splines

predictions.PS=predictions.PS%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(age =  as.character(age))     

### Smoothing splines

predictions.SS=predictions.SS%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(age =  as.character(age)) 



source("Assisting functions/Create risk differences.R")
predictions.RCS.diff=risk.diff.creator(dataframe = predictions.RCS, 
                                       treatment = "treat",
                                       matching.variables = c("age","bilat_0","study"),predicted.outcome = "fit",
                                       predicted.CI = c("Lower", "Upper") 
)


predictions.NS.diff=risk.diff.creator(dataframe = predictions.NS, 
                                      treatment = "treat",
                                      matching.variables = c("age","bilat_0","study"),predicted.outcome = "fit",
                                      predicted.CI = c("Lower", "Upper") 
)




predictions.PS.diff=risk.diff.creator(dataframe = predictions.PS, 
                                      treatment = "treat",
                                      matching.variables = c("age","bilat_0","study"),predicted.outcome = "fit",
                                      predicted.CI = c("Lower", "Upper") 
)



predictions.SS.diff=risk.diff.creator(dataframe = predictions.SS, 
                                      treatment = "treat",
                                      matching.variables = c("age","bilat_0","study"),predicted.outcome = "fit",
                                      predicted.CI = c("Lower", "Upper") 
)

### Clean the data above. 
absolute_diff_RCS.Empirical=  predictions.RCS.diff%>%
  select(study, age,bilat_0, fit.diff, diff.lower, diff.upper)

absolute_diff_NS.Empirical=  predictions.NS.diff%>%
  select(study, age,bilat_0, fit.diff, diff.lower, diff.upper)

absolute_diff_PS.Empirical=  predictions.PS.diff%>%
  select(study, age,bilat_0, fit.diff, diff.lower, diff.upper)

absolute_diff_SS.Empirical=  predictions.SS.diff%>%
  select(study, age,bilat_0, fit.diff, diff.lower, diff.upper)


### Second stage (pooling absolute risk differences per X*)
### RCS

point.wise.absolute_diff_RCS.Empirical =  pointwise.ma(data = absolute_diff_RCS.Empirical,
                                                       clustering.variable = "study",
                                                       combining.variables = c("age","bilat_0"),
                                                       predicted.outcome = "fit.diff",
                                                       predicted.outcome.se = NULL,
                                                       predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_RCS.Empirical =  point.wise.absolute_diff_RCS.Empirical%>%
  mutate(age =  as.numeric(age))

### BS

point.wise.absolute_diff_NS.Empirical =  pointwise.ma(data = absolute_diff_NS.Empirical,
                                                      clustering.variable = "study",
                                                      combining.variables = c("age","bilat_0"),
                                                      predicted.outcome = "fit.diff",
                                                      predicted.outcome.se = NULL,
                                                      predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_NS.Empirical =  point.wise.absolute_diff_NS.Empirical%>%
  mutate(age =  as.numeric(age))


## PS


point.wise.absolute_diff_PS.Empirical =  pointwise.ma(data = absolute_diff_PS.Empirical,
                                                      clustering.variable = "study",
                                                      combining.variables = c("age","bilat_0"),
                                                      predicted.outcome = "fit.diff",
                                                      predicted.outcome.se = NULL,
                                                      predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_PS.Empirical =  point.wise.absolute_diff_PS.Empirical%>%
  mutate(age =  as.numeric(age))

## SS


point.wise.absolute_diff_SS.Empirical =  pointwise.ma(data = absolute_diff_SS.Empirical,
                                                      clustering.variable = "study",
                                                      combining.variables = c("age","bilat_0"),
                                                      predicted.outcome = "fit.diff",
                                                      predicted.outcome.se = NULL,
                                                      predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_SS.Empirical =  point.wise.absolute_diff_SS.Empirical%>%
  mutate(age =  as.numeric(age))





### Here are the plots for the point-wise meta-analysis of absolute risk differences

point.wise.absolute_diff_RCS.Empirical.plot = point.wise.absolute_diff_RCS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  geom_hline(yintercept = 0, linetype=2)+ 
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        axis.text.y.left  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("a", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), size=18)+ ylim(c(-1,1))
point.wise.absolute_diff_RCS.Empirical.plot

point.wise.absolute_diff_NS.Empirical.plot = point.wise.absolute_diff_NS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  geom_hline(yintercept = 0, linetype=2)+ 
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        axis.text.y.left  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("b", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label),  size=18)+ ylim(c(-1,1))


point.wise.absolute_diff_PS.Empirical.plot = point.wise.absolute_diff_PS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  geom_hline(yintercept = 0, linetype=2)+ 
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        axis.text.y.left  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("c", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), size=18)+ ylim(c(-1,1))



point.wise.absolute_diff_SS.Empirical.plot = point.wise.absolute_diff_SS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  geom_hline(yintercept = 0, linetype=2)+ 
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        axis.text.y.left  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",size=24),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("d", ""),bilat_0   = levels(point.wise.DF.PS$bilat_0)),
            mapping = aes(x=0.3, y = 0.75, label = label), size=18)+ ylim(c(-1,1))

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 15.png",width = 1240, height = 1680) 

grid.arrange(point.wise.absolute_diff_RCS.Empirical.plot, 
             point.wise.absolute_diff_NS.Empirical.plot, 
             point.wise.absolute_diff_PS.Empirical.plot,
             point.wise.absolute_diff_SS.Empirical.plot, ncol=2,
             top = textGrob("", vjust = 1, gp = gpar(fontface = "bold", fontsize = 50)),
             left = textGrob("Antibiotics effect", rot = 90, vjust = 1, 
                             gp = gpar(fontface = "bold", fontsize = 32)),
             bottom = textGrob("Children's Age", vjust = 0, 
                               gp = gpar(fontface = "bold", fontsize = 32)))

dev.off()
