
## Fit a RCS model per study
RCS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>% 
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5),
                 knots = list(BMI = quantile(.$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Fit a B-spline model per study
BS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,m=c(2,0),k = 5),
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Fit a P-spline model per study
PS.DR= df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="ps",fx=F,by = Treatment,k=12), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Fit a Smoothing spline model per study
SS.DR = df2%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="tp",fx=F,by = Treatment), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))

## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 250),each = 10),
                    Treatment = rep(unique(df1$Treatment),1250), 
                    Study =  rep(rep(unique(df1$Study),each =2),250))

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

### B-splines
predictions.BS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

### P-splines
predictions.PS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 

### Smoothing splines
predictions.SS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit =T)) 


## Second stage (pooling the regression lines per X*)

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
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))








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
                                                 predicted.outcome.CI = c("diff.lower","diff.upper")
)

point.wise.absolute_diff_RCS.DR =  point.wise.absolute_diff_RCS.DR%>%
  mutate(BMI =  as.numeric(BMI))


### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_BS.DR  =  pointwise.ma(data = absolute_diff_BS.DR ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper")
)

point.wise.absolute_diff_BS.DR =  point.wise.absolute_diff_BS.DR%>%
  mutate(BMI =  as.numeric(BMI))



### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_PS.DR  =  pointwise.ma(data = absolute_diff_PS.DR ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_PS.DR =  point.wise.absolute_diff_PS.DR%>%
  mutate(BMI =  as.numeric(BMI))


### Non-heterogeneous IPD-set with different BMI ranges
point.wise.absolute_diff_SS.DR  =  pointwise.ma(data = absolute_diff_SS.DR ,
                                                clustering.variable = "Study",
                                                combining.variables = c("BMI"),
                                                predicted.outcome = "fit.diff",
                                                predicted.outcome.se = NULL,
                                                predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_SS.DR =  point.wise.absolute_diff_SS.DR%>%
  mutate(BMI =  as.numeric(BMI))





point.wise.DF.RCS.DR.diff.plot = point.wise.absolute_diff_RCS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
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





point.wise.DF.BS.DR.diff.plot=point.wise.absolute_diff_BS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
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


point.wise.DF.PS.DR.diff.plot=point.wise.absolute_diff_PS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+geom_hline(yintercept = 0, linetype=2)+ylab("") + 
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


point.wise.DF.SS.DR.diff.plot=point.wise.absolute_diff_SS.DR%>%
  ggplot(aes(x = BMI,RE.meta)) + geom_line(size=2)+
  geom_ribbon(mapping = aes(ymin=RE.meta.lower, ymax=RE.meta.upper),alpha=0.25)+
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







