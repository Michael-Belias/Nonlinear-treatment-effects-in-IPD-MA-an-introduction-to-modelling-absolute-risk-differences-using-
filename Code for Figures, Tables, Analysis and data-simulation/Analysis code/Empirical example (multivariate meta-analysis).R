## ----Multi-variate meta-analysis on empirical example  ------------------------------------------------------------------------------------------------------

rm(list=ls()[! ls() %in% c("expit","IPDMA")]) ### To clear all
### Ommit Little study since they don't provide bilaterality information
miniIPD= IPDMA%>% filter((study != "Little" & study != "Appelman"))

miniIPD%>%
  group_by(study,bilat_0)%>%
  summarise(Min = min(age), Max= max(age))

### all studies have except of Damoiseaux start at 0.5 years old
### Appelman McCormick reach until 8.5 years old
### Saux and Damoiseaux need data-augmentation
### Data-augmentation
## add weight variable

miniIPD$weight=1

upper=miniIPD[miniIPD$age >6,]; n.upper =  dim(upper)[1]

rep.upper=  do.call(rbind, replicate(2, upper, simplify = FALSE)) ;  rep.upper$weight= 0.000001

rep.upper$study = rep(c("Damoiseaux","Saux"), each= n.upper)
miniIPD =  rbind(miniIPD, rep.upper)
### First we get the knots from each data-set 3 knot in 10%, 50% and 90% quantiles
Knots.RCS.miniIPD =   quantile(miniIPD$age, probs = c(0.1,0.5,0.9))
Knots.NS.miniIPD =   c(2.5)





formula.RCS.empirical =  poutcome ~   treat + bilat_0 + age + treat*bilat_0 + age*treat + age*bilat_0 +
  rcspline.eval(age, knots = Knots.RCS.miniIPD, inclx = F)*treat +
  rcspline.eval(age, knots = Knots.RCS.miniIPD, inclx = F)*bilat_0


formula.NS.empirical =  poutcome ~  treat + bilat_0   + 
  ns(age,knots = Knots.NS.miniIPD)*treat+
  ns(age,knots = Knots.NS.miniIPD)*bilat_0


RCS.empirical = miniIPD%>%
  droplevels(miniIPD$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = glm(formula = formula.RCS.empirical, 
                 family = binomial("logit"), data = .))



NS.empirical = miniIPD%>%
  droplevels(miniIPD$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = glm(formula = formula.NS.empirical,
                 family = binomial("logit"),
                 data = .))





### RCS coefficients 
## Extract the RCS coefficients
coefficients.RCS.empirical= t(sapply(RCS.empirical$model,FUN = coefficients))

## Extract the Vcov lower triangular
Vcov.RCS.empirical = t(sapply(RCS.empirical$model,function(x)  vcov(x)[lower.tri(vcov(x), diag = T)]))

### RCS mv-meta
mvmeta.RCS.empirical = mvmeta(coefficients.RCS.empirical, Vcov.RCS.empirical, control= list(maxiter=1000))    

### NS coefficients 
## Extract the NS coefficients
coefficients.NS.empirical= t(sapply(NS.empirical$model,FUN = coefficients))

## Extract the Vcov lower triangular
Vcov.NS.empirical = t(sapply(NS.empirical$model,function(x)  vcov(x)[lower.tri(vcov(x), diag = T)]))

### NS mv-meta
mvmeta.NS.empirical = mvmeta(coefficients.NS.empirical, Vcov.NS.empirical, control= list(maxiter=1000)) 



## Create a data-set that will be used for estimating the predicted outcome


new.dat= data.frame(age= rep(seq(0,9,length.out = 50),each = 16),
                    treat = rep(unique(miniIPD$treat),400), 
                    bilat_0 =  rep(rep(unique(miniIPD$bilat_0),each=2),200),
                    study =  rep(rep(unique(miniIPD$study),each =4),50), 
                    poutcome=  rep(unique(miniIPD$poutcome), each =2)
)




### create the model matrices to cross-validate that we are multiplying the correct transformations of X


### RCS
model.matrix.RCS.empirical = model.matrix(gam(formula = formula.RCS.empirical, 
                                       family = binomial("logit"), 
                                       data = new.dat))



model.matrices.RCS.empirical = as.data.frame(model.matrix.RCS.empirical)%>%
  group_by(treatAntibiotics, bilat_0Bilateral)%>%
  do(plots=autoplot(zoo(.), facets=NULL)+ ggtitle(.$treatAntibiotics)+theme_bw())



grid.arrange(model.matrices.RCS.empirical$plots[[1]], 
             model.matrices.RCS.empirical$plots[[2]],
             model.matrices.RCS.empirical$plots[[3]], 
             model.matrices.RCS.empirical$plots[[4]])



### NS-splines
model.matrix.NS.empirical = model.matrix(gam(formula = formula.NS.empirical,   
                                      family = binomial("logit"), 
                                      data = new.dat))  



model.matrices.NS.empirical = as.data.frame(model.matrix.NS.empirical)%>%
  group_by(treatAntibiotics, bilat_0Bilateral)%>%
  do(plots=autoplot(zoo(.), facets=NULL)+ ggtitle(.$treatAntibiotics)+theme_bw())



grid.arrange(model.matrices.NS.empirical$plots[[1]], 
             model.matrices.NS.empirical$plots[[2]],
             model.matrices.NS.empirical$plots[[3]], 
             model.matrices.NS.empirical$plots[[4]])




### RCS 
predicted.curves.RCS.empirical= data.frame(
  fit =  model.matrix.RCS.empirical%*% coef(mvmeta.RCS.empirical),
  fit.lower=  model.matrix.RCS.empirical%*% coef(mvmeta.RCS.empirical)   ### This is the fitted curve above
  - 1.96* sqrt(rowSums(model.matrix.RCS.empirical * (model.matrix.RCS.empirical %*% vcov(mvmeta.RCS.empirical)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  fit.upper=   model.matrix.RCS.empirical%*% coef(mvmeta.RCS.empirical)   ### This is the fitted curve above
  + 1.96* sqrt(rowSums(model.matrix.RCS.empirical * (model.matrix.RCS.empirical %*% vcov(mvmeta.RCS.empirical)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  new.dat
)

### NS
predicted.curves.NS.empirical= data.frame(
  fit =  model.matrix.NS.empirical%*% coef(mvmeta.NS.empirical),
  fit.lower=  model.matrix.NS.empirical%*% coef(mvmeta.NS.empirical)   ### This is the fitted curve above
  - 1.96* sqrt(rowSums(model.matrix.NS.empirical * (model.matrix.NS.empirical %*% vcov(mvmeta.NS.empirical)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  fit.upper=   model.matrix.NS.empirical%*% coef(mvmeta.NS.empirical)   ### This is the fitted curve above
  + 1.96* sqrt(rowSums(model.matrix.NS.empirical * (model.matrix.NS.empirical %*% vcov(mvmeta.NS.empirical)))), ## this is equivalent to diag(Xp %*% V %*% t(Xp)) 
  new.dat
)



### RCS (didn't converge)
mvmeta.plot.RCS.empirical = ggplot(predicted.curves.RCS.empirical, aes(age,expit(fit), color= treat)) + 
  geom_line()+ geom_ribbon(aes(ymin= expit(fit.lower), ymax= expit(fit.upper)), alpha= 0.1) + facet_wrap(.~bilat_0)+
ylab("")+ xlab("") + 
  scale_color_jama()+
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
        legend.position = "none")  + 
  geom_text(data= data.frame( label = c("a", ""),bilat_0   = levels(new.dat$bilat_0)),
            mapping = aes(x=0.5, y = 0.75, label = label), 
            inherit.aes = F,size=18)+ ylim(c(0,1))



### NS
mvmeta.plot.NS.empirical =ggplot(predicted.curves.NS.empirical, aes(age,expit(fit), color= treat)) + 
  geom_line()+ geom_ribbon(aes(ymin= expit(fit.lower), ymax= expit(fit.upper)), alpha= 0.1) + facet_wrap(.~bilat_0)+
  ylab("")+ xlab("") + 
  scale_color_jama()+
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
        legend.position = "none")  + 
  geom_text(data= data.frame( label = c("b", ""),bilat_0   = levels(new.dat$bilat_0)),
            mapping = aes(x=0.5, y = 0.75, label = label), 
            inherit.aes = F,size=18)+ ylim(c(0,1))




p1= ggplot(predicted.curves.NS.empirical, aes(age,expit(fit), color= treat)) + 
  geom_line()+ geom_ribbon(aes(ymin= expit(fit.lower), ymax= expit(fit.upper)), alpha= 0.1) + facet_wrap(.~bilat_0)+
  ylab("")+ xlab("") + 
  scale_color_jama()+
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


g.legend =  get_legend(p1)



png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 16.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(mvmeta.plot.RCS.empirical, 
                         mvmeta.plot.NS.empirical,
                         top = textGrob("", vjust = 1, gp = gpar(fontface = "bold", fontsize = 50)),
                         left = textGrob("Risk of fever after 3-7 days", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold", fontsize = 32)),
                         bottom = textGrob("Children's Age", vjust = 0, 
                                           gp = gpar(fontface = "bold", fontsize = 32))),
             g.legend, heights=  c(10,1))

dev.off()



## ----Absolute risk differences ---------------------------------------------------------------------------------------------------------
### MVmeta
source("Assisting functions/Create risk differences.R")

predicted.curves.RCS.empirical$fit =  expit(predicted.curves.RCS.empirical$fit)
predicted.curves.RCS.empirical$fit.lower =  expit(predicted.curves.RCS.empirical$fit.lower)
predicted.curves.RCS.empirical$fit.upper =  expit(predicted.curves.RCS.empirical$fit.upper)



MV.meta_absolute_difference.RCS=  risk.diff.creator(dataframe = predicted.curves.RCS.empirical,
                                                    treatment = "treat",
                                                    matching.variables=  c("study","age","bilat_0"),
                                                    outcome= NULL, 
                                                    predicted.outcome = "fit", 
                                                    predicted.CI = c("fit.lower","fit.upper"))


predicted.curves.NS.empirical$fit =  expit(predicted.curves.NS.empirical$fit)
predicted.curves.NS.empirical$fit.lower =  expit(predicted.curves.NS.empirical$fit.lower)
predicted.curves.NS.empirical$fit.upper =  expit(predicted.curves.NS.empirical$fit.upper)


MV.meta_absolute_difference.NS=  risk.diff.creator(dataframe = predicted.curves.NS.empirical,
                                                   treatment = "treat",
                                                   matching.variables=  c("study","age","bilat_0"),
                                                   outcome= NULL, 
                                                   predicted.outcome = "fit", 
                                                   predicted.CI = c("fit.lower","fit.upper"))



mvmeta.plot.absolute_difference.RCS = ggplot(MV.meta_absolute_difference.RCS,aes(x = age, fit.diff)) +
  geom_line(size=2)+ facet_wrap(.~bilat_0)+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ theme_minimal()+
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
  geom_text(data= data.frame( label = c("a", ""),bilat_0   = levels(MV.meta_absolute_difference.RCS$bilat_0)),
            mapping = aes(x=0.3,  y = 0.75, label = label), 
            inherit.aes = F,  size=18)+ ylim(c(-1,1))

mvmeta.plot.absolute_difference.NS = ggplot(MV.meta_absolute_difference.NS,aes(x = age, fit.diff)) +
  geom_line(size=2)+ facet_wrap(.~bilat_0)+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ theme_minimal()+
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
  geom_text(data= data.frame( label = c("b", ""),bilat_0   = levels(MV.meta_absolute_difference.RCS$bilat_0)),
            mapping = aes(x=0.3,  y = 0.75, label = label), 
            inherit.aes = F,  size=18)+ ylim(c(-1,1))


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 17.png",width = 1240, height = 1680) 

grid.arrange(mvmeta.plot.absolute_difference.RCS, 
             mvmeta.plot.absolute_difference.NS,
             top = textGrob("", vjust = 1, gp = gpar(fontface = "bold", fontsize = 50)),
             left = textGrob("Antibiotics effect", rot = 90, vjust = 1, 
                             gp = gpar(fontface = "bold", fontsize = 32)),
             bottom = textGrob("Children's Age", vjust = -1, 
                               gp = gpar(fontface = "bold", fontsize = 32)))
dev.off()

