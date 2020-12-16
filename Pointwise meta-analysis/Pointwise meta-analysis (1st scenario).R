## ----Simulation of first IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/First scenario data-set.R")

##    Point-wise meta-analysis results for the first scenario
### Clear all environment besides the data-sets
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])


### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}

source("Assisting functions/B-splines knots.R")

## ----Pointwise meta-analysis --------------------------------------------------------------------------------------

### %>% is a pipeline function 
### each line uses the result of the previous pipe 
### 

## Fit a RCS model per study
RCS.HT = df1%>%                                ### We call the data-set of the first scenario
  arrange(desc(Study))%>%                      ### Order the date based on study                
  group_by(Study) %>%                          ### Group them based on study   
  do(model = glm(Y~ BMI + Treatment + BMI*Treatment + 
                   rcs(BMI, quantile(.$BMI, probs = c(0.05,0.35,0.65,0.95)))*Treatment, ### For each study we fit RCS 
                 family = binomial("logit"),   ### Note that in pointwise meta-analysis one can use different modelling techniques per study
                 data = .                     ### different models per study 
                 ))              

## Similarly we
## Fit a B-spline model per study
BS.HT = df1%>%                                 ### We call the data-set of the first scenario
  arrange(desc(Study))%>%                      ### Order the date based on study 
  group_by(Study) %>%                          ### Group them based on study   
  do(model = glm(Y~ BMI + Treatment + BMI*Treatment +  
                   bs(BMI, df = 3)*Treatment,  ### For each study we fit B-splines 
                 family = binomial("logit"), 
                 data = .))

## Fit a P-spline model per study
PS.HT = df1%>%                                 ### We call the data-set of the first scenario
  arrange(desc(Study))%>%                      ### Order the date based on study 
  group_by(Study) %>%                          ### Group them based on study   
  do(model = gam(Y~ BMI + Treatment + BMI*Treatment + 
                   s(BMI,bs ="ps",fx=F,by = Treatment,m=c(1,2),k = 17), ### For each study we fit P-spline 
                 family = binomial("logit"), 
                 data = ., 
                 method="REML" ))

## Fit a Smoothing splines model per study
SS.HT = df1%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment + BMI*Treatment + 
                   s(BMI,bs ="tp",fx=F,by = Treatment), 
                 family = binomial("logit"), 
                 data = ., 
                 method="REML" ))


## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 50),each = 10),
                    Treatment = rep(unique(df1$Treatment),250), 
                    Study =  rep(rep(unique(df1$Study),each =2),50))


## Calculate the predicted outcome per study (given the models fitted above)
predictions.RCS.HT= new.dat%>%                        ## Call the new data 
  droplevels(new.dat$Study)%>%                        ## We omit the factor levels 
  arrange(desc(Study))%>%                             ## Order by Study 
  group_by(Study)%>%                                  ## Group by Study
  nest()%>%                                           ## Nesting creates a list-column of data frames
  full_join(RCS.HT, by = "Study") %>%                 ## Combine the list to one data.frame
  group_by(Study)%>%                                  ## group by Study
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) ## Call the augment function to get the predicted outcomes per study. 

## Similarly
## For B-splines
predictions.BS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

## For P-splines
predictions.PS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

## For Smoothing-splines
predictions.SS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

## Second stage (pooling the regression lines per each value of X)
## Load the assisting function to perform pointwise meta-analysis 
source("Assisting functions/Point-wise meta-analysis.R")



point.wise.DF.RCS.HT = pointwise.ma(data = predictions.RCS.HT,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

### RCS
point.wise.DF.RCS.HT$RE.meta =  expit(point.wise.DF.RCS.HT$RE.meta )
point.wise.DF.RCS.HT$RE.meta.upper =  expit(point.wise.DF.RCS.HT$RE.meta.upper )
point.wise.DF.RCS.HT$RE.meta.lower =  expit(point.wise.DF.RCS.HT$RE.meta.lower )


### B-splines
point.wise.DF.BS.HT =  pointwise.ma(predictions.BS.HT,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML") 

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.BS.HT$RE.meta  =  expit(point.wise.DF.BS.HT$RE.meta )
point.wise.DF.BS.HT$RE.meta.upper  =  expit(point.wise.DF.BS.HT$RE.meta.upper )
point.wise.DF.BS.HT$RE.meta.lower  =  expit(point.wise.DF.BS.HT$RE.meta.lower)


### P-splines
point.wise.DF.PS.HT =  pointwise.ma(predictions.PS.HT,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.PS.HT$RE.meta        =  expit(point.wise.DF.PS.HT$RE.meta )
point.wise.DF.PS.HT$RE.meta.lower  =  expit(point.wise.DF.PS.HT$RE.meta.lower)
point.wise.DF.PS.HT$RE.meta.upper  =  expit(point.wise.DF.PS.HT$RE.meta.upper )


### Smoothing splines
point.wise.DF.SS.HT =  pointwise.ma(predictions.SS.HT,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML")

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.SS.HT$RE.meta        =  expit(point.wise.DF.SS.HT$RE.meta )
point.wise.DF.SS.HT$RE.meta.upper  =  expit(point.wise.DF.SS.HT$RE.meta.upper )
point.wise.DF.SS.HT$RE.meta.lower  =  expit(point.wise.DF.SS.HT$RE.meta.lower)


### Draw the corresponding plots
### RCS
point.wise.DF.RCS.HT.plot = point.wise.DF.RCS.HT%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +
  geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower, ymax=RE.meta.upper),alpha=0.2) +
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


### B-splines
point.wise.DF.BS.HT.plot = point.wise.DF.BS.HT%>%
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

### P-splines
point.wise.DF.PS.HT.plot = point.wise.DF.PS.HT%>%
  mutate(Treatment = as.factor(Treatment))%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment))+geom_line(size=2)+
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

### Smoothing splines
point.wise.DF.SS.HT.plot = point.wise.DF.SS.HT%>%
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

#grid.arrange(point.wise.DF.RCS.HT.plot,point.wise.DF.BS.HT.plot,point.wise.DF.PS.HT.plot,point.wise.DF.SS.HT.plot)

### Pseudo-plot to get legend
p1=  point.wise.DF.SS.HT%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +
  geom_line(size=2)+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama()+ ylab("") + 
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
        legend.text=element_text(size=42, hjust = 0, vjust =  -15), 
        legend.title =element_blank(),
        legend.position = "bottom") +  
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))

### save the legend
legend = gtable_filter(ggplotGrob(p1), "guide-box") 




#### Absolute risk differences 
### For each scenario, study and BMI calculate the predicted mortality risks
### RCS

predictions.RCS.HT=predictions.RCS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))   

### B-splines
### Heterogeneous IPD-set with equal BMI ranges
predictions.BS.HT=predictions.BS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))
### P-splines

predictions.PS.HT=predictions.PS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### Smoothing splines

predictions.SS.HT=predictions.SS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))


### Load assisting function to calculate absolute risk differences and SEs
source("Assisting functions/Create risk differences.R")




### For each scenario, study and BMI calculate the risk difference between the Control and Treated
### RCS
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_RCS.HT = risk.diff.creator(dataframe = predictions.RCS.HT,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                                         matching.variables = c("BMI","Study"),
                                         predicted.outcome = "fit", 
                                         predicted.CI = c("Lower","Upper"))


### B-splines
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_BS.HT = risk.diff.creator(dataframe = predictions.BS.HT,
                                        treatment = "Treatment",
                                        outcome = NULL, 
                                        matching.variables = c("BMI","Study"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### P-splines
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_PS.HT = risk.diff.creator(dataframe = predictions.PS.HT,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI","Study"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


### Smoothing splines
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_SS.HT = risk.diff.creator(dataframe = predictions.SS.HT,
                                        treatment = "Treatment", outcome = NULL,
                                        matching.variables = c("BMI","Study"),
                                        predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



### Clean the data above. 
absolute_diff_RCS.HT=  absolute_diff_RCS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_BS.HT=  absolute_diff_BS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)

absolute_diff_PS.HT=  absolute_diff_PS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.HT=  absolute_diff_SS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


## ----Poiwise meta-analysis for absolute risk differences--------------------------------------------------------------------------------------------------------

### Load assisting function
source("Assisting functions/Create risk differences.R")

### Second stage (pooling absolute risk differences per X*)
### RCS
### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_RCS.HT  =  pointwise.ma(data = absolute_diff_RCS.HT ,
                                                 clustering.variable = "Study",
                                                 combining.variables = c("BMI"),
                                                 predicted.outcome = "fit.diff",
                                                 predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                 tau.method = "REML"
)

point.wise.absolute_diff_RCS.HT =  point.wise.absolute_diff_RCS.HT%>%
  mutate(BMI =  as.numeric(BMI))






### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_BS.HT  =  pointwise.ma(data = absolute_diff_BS.HT ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                tau.method = "REML"
)

point.wise.absolute_diff_BS.HT =  point.wise.absolute_diff_BS.HT%>%
  mutate(BMI =  as.numeric(BMI))



### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_PS.HT  =  pointwise.ma(data = absolute_diff_PS.HT ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                tau.method = "REML"
)
point.wise.absolute_diff_PS.HT =  point.wise.absolute_diff_PS.HT%>%
  mutate(BMI =  as.numeric(BMI))


### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_SS.HT  =  pointwise.ma(data = absolute_diff_SS.HT ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper"),
                                                tau.method = "REML"
)

point.wise.absolute_diff_SS.HT =  point.wise.absolute_diff_SS.HT%>%
  mutate(BMI =  as.numeric(BMI))




#### Draw the plots
point.wise.DF.RCS.HT.diff.plot = point.wise.absolute_diff_RCS.HT%>%
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

point.wise.DF.BS.HT.diff.plot=point.wise.absolute_diff_BS.HT%>%
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




point.wise.DF.PS.HT.diff.plot=point.wise.absolute_diff_PS.HT%>%
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


point.wise.DF.SS.HT.diff.plot=point.wise.absolute_diff_SS.HT%>%
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


