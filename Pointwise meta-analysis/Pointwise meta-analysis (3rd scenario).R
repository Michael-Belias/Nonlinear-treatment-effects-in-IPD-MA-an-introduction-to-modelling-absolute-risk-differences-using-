## ----Simulation of third IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")

##    Point-wise meta-analysis results for the third scenario
##    Clear all environment besides the data-sets
# rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])


## ----Pointwise meta-analysis --------------------------------------------------------------------------------------

## Fit a RCS model per study
RCS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = glm(formula = Y~ Treatment +  
                   rcs(BMI, quantile(.$BMI, probs = c(0.1,0.5,0.9)))*Treatment, ### For each study we fit RCS 
                 family = binomial("logit"), data = .))

## Fit a B-splines model per study
NS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ Treatment +   
                   ns(BMI, df = 2)*Treatment,  ### For each study we fit B-splines 
                 family = binomial("logit"), 
                 data = .))

## Fit a P-splines model per study
PS.Comb= df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment + BMI*Treatment + s(BMI,bs ="ps",fx=F,by = Treatment,k=17), 
                 family = binomial("logit"), 
                 data = ., 
                 method="REML" ))

## Fit a Smoothing splines model per study
SS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment + BMI*Treatment + s(BMI,bs ="tp",fx=F,by = Treatment), 
                 family = binomial("logit"), 
                 data = ., 
                 method="REML" ))


## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 50),each = 10),
                    Treatment = rep(unique(df3$Treatment),250), 
                    Study =  rep(rep(unique(df3$Study),each =2),50))


### Calculate the predicted outcomes per study 

### RCS
predictions.RCS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

predictions.RCS.Comb%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



### B-splines
predictions.NS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(NS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 


predictions.NS.Comb%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)



### P-splines
predictions.PS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

predictions.PS.Comb%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)




### Smoothing splines
predictions.SS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 


predictions.SS.Comb%>%
  ggplot(., aes(BMI,expit(.fitted), color= Treatment,))+ geom_line()+ facet_wrap(.~Study) + 
  geom_ribbon(mapping = aes(ymin= expit(.fitted - 1.96*.se.fit ),ymax = expit(.fitted + 1.96*.se.fit), fill= Treatment), alpha= 0.1)




## Second stage (pooling the regression lines per X*)
source("Assisting functions/Point-wise meta-analysis.R")


### RCS
point.wise.DF.RCS.Comb = pointwise.ma(data = predictions.RCS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.RCS.Comb$RE.meta =  expit(point.wise.DF.RCS.Comb$RE.meta )
point.wise.DF.RCS.Comb$RE.meta.upper =  expit(point.wise.DF.RCS.Comb$RE.meta.upper )
point.wise.DF.RCS.Comb$RE.meta.lower =  expit(point.wise.DF.RCS.Comb$RE.meta.lower )

### B-splines
point.wise.DF.NS.Comb =  pointwise.ma(predictions.NS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML") 

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.NS.Comb$RE.meta  =  expit(point.wise.DF.NS.Comb$RE.meta )
point.wise.DF.NS.Comb$RE.meta.upper  =  expit(point.wise.DF.NS.Comb$RE.meta.upper )
point.wise.DF.NS.Comb$RE.meta.lower  =  expit(point.wise.DF.NS.Comb$RE.meta.lower)

### P-splines
point.wise.DF.PS.Comb =  pointwise.ma(predictions.PS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.PS.Comb$RE.meta  =  expit(point.wise.DF.PS.Comb$RE.meta )
point.wise.DF.PS.Comb$RE.meta.lower  =  expit(point.wise.DF.PS.Comb$RE.meta.lower)
point.wise.DF.PS.Comb$RE.meta.upper  =  expit(point.wise.DF.PS.Comb$RE.meta.upper )

### Smoothing splines
point.wise.DF.SS.Comb =  pointwise.ma(predictions.SS.Comb,
                                      clustering.variable = "Study",
                                      combining.variables = c("BMI","Treatment"), 
                                      predicted.outcome =  ".fitted", 
                                      predicted.outcome.se = ".se.fit", 
                                      predicted.outcome.CI = NULL, 
                                      tau.method = "REML")


## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.SS.Comb$RE.meta.upper  =  expit(point.wise.DF.SS.Comb$RE.meta.upper )
point.wise.DF.SS.Comb$RE.meta  =  expit(point.wise.DF.SS.Comb$RE.meta )
point.wise.DF.SS.Comb$RE.meta.lower  =  expit(point.wise.DF.SS.Comb$RE.meta.lower)


### Draw the corresponding plots

### RCS
point.wise.DF.RCS.Comb.plot = point.wise.DF.RCS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+ theme_minimal()+
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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "a") +ylim(c(0,1))


## B-splines
point.wise.DF.NS.Comb.plot = point.wise.DF.NS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "b") +ylim(c(0,1))

## P-splines
point.wise.DF.PS.Comb.plot = point.wise.DF.PS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.9, size = 10,label = "c") +ylim(c(0,1))


## Smoothing splines
point.wise.DF.SS.Comb.plot = point.wise.DF.SS.Comb%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))



##-------- Absolute risk differences (Treatment effect plot) -----------------------------------------------------------------------------

### RCS
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.RCS.Comb= predictions.RCS.Comb %>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### B-splines
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.NS.Comb=predictions.NS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### P-splines
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.PS.Comb=predictions.PS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))     

### Smoothing splines
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.SS.Comb=predictions.SS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI)) 



### Load assisting function to calculate absolute risk differences and SEs
source("Assisting functions/Create risk differences.R")

### RCS
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_RCS.Comb = risk.diff.creator(dataframe = predictions.RCS.Comb,
                                           treatment = "Treatment", 
                                           outcome = NULL,
                                           matching.variables = c("BMI","Study"),
                                           predicted.outcome = "fit", 
                                           predicted.CI = c("Lower","Upper"))



### B-splines

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_NS.Comb = risk.diff.creator(dataframe = predictions.NS.Comb,
                                          treatment = "Treatment",outcome = NULL, 
                                          matching.variables = c("BMI","Study"),
                                          predicted.outcome = "fit", 
                                          predicted.CI = c("Lower","Upper"))


### P-splines

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_PS.Comb = risk.diff.creator(dataframe = predictions.PS.Comb,
                                          treatment = "Treatment", outcome = NULL,
                                          matching.variables = c("BMI","Study"),
                                          predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


### Smoothing splines

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_SS.Comb = risk.diff.creator(dataframe = predictions.SS.Comb,
                                          treatment = "Treatment", outcome = NULL,
                                          matching.variables = c("BMI","Study"),
                                          predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))




### Clean the data above. 
absolute_diff_RCS.Comb=  absolute_diff_RCS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_NS.Comb=  absolute_diff_NS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_PS.Comb=  absolute_diff_PS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_SS.Comb=  absolute_diff_SS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)



## ----Poiwise meta-analysis absolute risk differences--------------------------------------------------------------------------------------------------------


### Second stage (pooling absolute risk differences per X*)
### RCS
### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_RCS.Comb =  pointwise.ma(data = absolute_diff_RCS.Comb,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                  tau.method = "EB"
)
point.wise.absolute_diff_RCS.Comb =  point.wise.absolute_diff_RCS.Comb%>%
  mutate(BMI =  as.numeric(BMI))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_NS.Comb  =  pointwise.ma(data = absolute_diff_NS.Comb ,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                  tau.method = "EB"
)

point.wise.absolute_diff_NS.Comb =  point.wise.absolute_diff_NS.Comb%>%
  mutate(BMI =  as.numeric(BMI))


### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_PS.Comb  =  pointwise.ma(data = absolute_diff_PS.Comb ,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                  tau.method = "EB"
)

point.wise.absolute_diff_PS.Comb =  point.wise.absolute_diff_PS.Comb%>%
  mutate(BMI =  as.numeric(BMI))






### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_SS.Comb  =  pointwise.ma(data = absolute_diff_SS.Comb ,
                                                  clustering.variable = "Study",
                                                  combining.variables = c("BMI"),
                                                  predicted.outcome = "fit.diff",
                                                  predicted.outcome.se = NULL,
                                                  predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                  tau.method = "EB"
)

point.wise.absolute_diff_SS.Comb =  point.wise.absolute_diff_SS.Comb%>%
  mutate(BMI =  as.numeric(BMI))


point.wise.DF.RCS.Comb.diff.plot = point.wise.absolute_diff_RCS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "a")+ ylim(c(-1,1))


point.wise.DF.NS.Comb.diff.plot=point.wise.absolute_diff_NS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "b")+ ylim(c(-1,1))

point.wise.DF.PS.Comb.diff.plot=point.wise.absolute_diff_PS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.75, size = 10,label = "c")+ ylim(c(-1,1))



point.wise.DF.SS.Comb.diff.plot=point.wise.absolute_diff_SS.Comb%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ylab("") + 
  xlab("")+theme_minimal()+
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
  annotate("text",x = 19.25,y=0.75, size = 10, label = "d")+ ylim(c(-1,1))


