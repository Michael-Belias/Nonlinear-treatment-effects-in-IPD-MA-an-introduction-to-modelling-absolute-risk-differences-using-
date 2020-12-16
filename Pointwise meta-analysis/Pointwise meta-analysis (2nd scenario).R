## ----Simulation of second IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")

##    Point-wise meta-analysis results for the second scenario
##    Clear all environment besides the data-sets
# rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])


## ----Pointwise meta-analysis --------------------------------------------------------------------------------------


## Fit a RCS model per study
RCS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>% 
  do(model = glm(formula = Y~ BMI + Treatment + BMI*Treatment+  
                 rcs(BMI, quantile(.$BMI, probs = c(0.1,0.5,0.9)))*Treatment, ### For each study we fit RCS 
                 family = binomial("logit"), data = .))

## Fit a B-spline model per study
BS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = glm(formula = Y~ BMI + Treatment + BMI*Treatment + 
                   bs(BMI, df = 3)*Treatment,  ### For each study we fit B-splines 
                 family = binomial("logit"), 
                 data = .))

## Fit a P-spline model per study
PS.DR= df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+BMI * Treatment+  s(BMI,bs ="ps",fx=F,by = Treatment,m=c(1,2),k = 17), 
                 family = binomial("logit"), 
                 data = ., 
                 method="REML" ))

## Fit a Smoothing spline model per study
SS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+BMI * Treatment+  s(BMI,bs ="tp",fx=F,by = Treatment), 
                 family = binomial("logit"), 
                 data = ., 
                 method="REML" ))

## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 50),each = 10),
                    Treatment = rep(unique(df2$Treatment),250), 
                    Study =  rep(rep(unique(df2$Study),each =2),50))

### Calculate the predicted outcomes per study 

### RCS
predictions.RCS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 


incr=0
for ( i in unique(df2$Study)){
  

  predictions.RCS.DR[incr+ which(predictions.RCS.DR[predictions.RCS.DR$Study == i,]$BMI < min(df2[df2$Study == i,]$BMI) | 
                                 predictions.RCS.DR[predictions.RCS.DR$Study == i,]$BMI > max(df2[df2$Study == i,]$BMI) ),]$.se.fit = 10

  
  incr = incr + 100
  
  
}



### Don't run. 

predictions.RCS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study)+newtheme + ggtitle("Per study predicted outcomes (Harrel's approach)")+
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)


### B-splines
predictions.BS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 



### Don't run. 

predictions.BS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



### P-splines
predictions.PS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

### Don't run. 

predictions.PS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



### Smoothing splines
predictions.SS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 



predictions.SS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



## Second stage (pooling the regression lines per X*)
source("Assisting functions/Point-wise meta-analysis.R")
point.wise.DF.RCS.DR = pointwise.ma(data = predictions.RCS.DR,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

### RCS
point.wise.DF.RCS.DR$RE.meta =  expit(point.wise.DF.RCS.DR$RE.meta )
point.wise.DF.RCS.DR$RE.meta.upper =  expit(point.wise.DF.RCS.DR$RE.meta.upper )
point.wise.DF.RCS.DR$RE.meta.lower =  expit(point.wise.DF.RCS.DR$RE.meta.lower )


### B-splines
point.wise.DF.BS.DR =  pointwise.ma(predictions.BS.DR,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML") 

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.BS.DR$RE.meta  =  expit(point.wise.DF.BS.DR$RE.meta )
point.wise.DF.BS.DR$RE.meta.upper  =  expit(point.wise.DF.BS.DR$RE.meta.upper )
point.wise.DF.BS.DR$RE.meta.lower  =  expit(point.wise.DF.BS.DR$RE.meta.lower)




### P-splines
point.wise.DF.PS.DR =  pointwise.ma(predictions.PS.DR,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.PS.DR$RE.meta  =  expit(point.wise.DF.PS.DR$RE.meta )
point.wise.DF.PS.DR$RE.meta.lower  =  expit(point.wise.DF.PS.DR$RE.meta.lower)
point.wise.DF.PS.DR$RE.meta.upper  =  expit(point.wise.DF.PS.DR$RE.meta.upper )




### Smoothing splines
point.wise.DF.SS.DR =  pointwise.ma(predictions.SS.DR,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.SS.DR$RE.meta.upper  =  expit(point.wise.DF.SS.DR$RE.meta.upper )
point.wise.DF.SS.DR$RE.meta  =  expit(point.wise.DF.SS.DR$RE.meta )
point.wise.DF.SS.DR$RE.meta.lower  =  expit(point.wise.DF.SS.DR$RE.meta.lower)


### Draw the corresponding plots

### RCS
point.wise.DF.RCS.DR.plot = point.wise.DF.RCS.DR%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+ theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "a") +ylim(c(0,1))



### B-splines
point.wise.DF.BS.DR.plot = point.wise.DF.BS.DR%>% 
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2)  +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "b") +ylim(c(0,1))


### P-splines
point.wise.DF.PS.DR.plot = point.wise.DF.PS.DR%>% 
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2)+
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.9, size = 10,label = "c") +ylim(c(0,1))


### Smoothing splines
point.wise.DF.SS.DR.plot = point.wise.DF.SS.DR%>% 
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))

grid.arrange(point.wise.DF.RCS.DR.plot,point.wise.DF.BS.DR.plot,point.wise.DF.PS.DR.plot,point.wise.DF.SS.DR.plot)


##-------- Absolute risk differences (Treatment effect plot) -----------------------------------------------------------------------------
### RCS
predictions.RCS.DR= predictions.RCS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))


### B-splines
### Non-heterogeneous IPD-set with different BMI ranges
predictions.BS.DR=predictions.BS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### P-splines
### Non-heterogeneous IPD-set with different BMI ranges
predictions.PS.DR= predictions.PS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))


### Smoothing splines
### Non-heterogeneous IPD-set with different BMI ranges
predictions.SS.DR=predictions.SS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))   



### Load assisting function to calculate absolute risk differences and SEs
source("Assisting functions/Create risk differences.R")





### RCS
### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_RCS.DR = risk.diff.creator(dataframe = predictions.RCS.DR,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                                         matching.variables = c("BMI","Study"),
                                         predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


### B-splines

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_BS.DR = risk.diff.creator(dataframe = predictions.BS.DR,
                                        treatment = "Treatment",outcome = NULL, 
                                        matching.variables = c("BMI","Study"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))




### P-splines

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_PS.DR = risk.diff.creator(dataframe = predictions.PS.DR,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI","Study"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



### Smoothing splines

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_SS.DR = risk.diff.creator(dataframe = predictions.SS.DR,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI","Study"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))




### Clean the data above. 
absolute_diff_RCS.DR=  absolute_diff_RCS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_BS.DR=  absolute_diff_BS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)



absolute_diff_PS.DR=  absolute_diff_PS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)



absolute_diff_SS.DR=  absolute_diff_SS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


## ----Poiwise meta-analysis absolute risk differences--------------------------------------------------------------------------------------------------------



### Load assisting function
source("Assisting functions/Create risk differences.R")

### Second stage (pooling absolute risk differences per X*)
### RCS
### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_RCS.DR  =  pointwise.ma(data = absolute_diff_RCS.DR ,
                                                 clustering.variable = "Study",
                                                 combining.variables = c("BMI"),
                                                 predicted.outcome = "fit.diff",
                                                 predicted.outcome.se = NULL,
                                                 predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                 tau.method = "REML"
)

point.wise.absolute_diff_RCS.DR =  point.wise.absolute_diff_RCS.DR%>%
  mutate(BMI =  as.numeric(BMI))

### Don't run. 

point.wise.absolute_diff_RCS.DR%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)

### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_BS.DR  =  pointwise.ma(data = absolute_diff_BS.DR ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                tau.method = "REML"
)

point.wise.absolute_diff_BS.DR =  point.wise.absolute_diff_BS.DR%>%
  mutate(BMI =  as.numeric(BMI))



### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_PS.DR  =  pointwise.ma(data = absolute_diff_PS.DR ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                tau.method = "REML"
)
point.wise.absolute_diff_PS.DR =  point.wise.absolute_diff_PS.DR%>%
  mutate(BMI =  as.numeric(BMI))


### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_SS.DR  =  pointwise.ma(data = absolute_diff_SS.DR ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                tau.method = "REML"
)
point.wise.absolute_diff_SS.DR =  point.wise.absolute_diff_SS.DR%>%
  mutate(BMI =  as.numeric(BMI))





point.wise.DF.RCS.DR.diff.plot = point.wise.absolute_diff_RCS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "a")+ ylim(c(-1,1))





point.wise.DF.BS.DR.diff.plot=point.wise.absolute_diff_BS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "b")+ ylim(c(-1,1))


point.wise.DF.PS.DR.diff.plot=point.wise.absolute_diff_PS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.75, size = 10,label = "c")+ ylim(c(-1,1))


point.wise.DF.SS.DR.diff.plot=point.wise.absolute_diff_SS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5,size=32),
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "d")+ ylim(c(-1,1))







