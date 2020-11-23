
## ----Load necessary libraries -----------------------------------------------------------------------
rm(list=ls()) ### To clear enviroment
# set.wd() ## use this function to define your personal workspace



# Libraries for loading and saving data
## Load haven for loading sas data-sets 
if(!require("haven")) install.packages("haven")

################################################
# Libraries for plotting

## Load ggplot2 for basic plotting
if(!require("ggplot2")) install.packages("ggplot2")
## Load ggpubr for plotting
if(!require("ggpubr")) install.packages("ggpubr") # automatically loads the ggplot2 (if installed)
## Load grid and gridExtra  to arrange multiple grid-based plots on a page, and draw tables
if(!require("grid")) install.packages("grid")
if(!require("gridExtra")) install.packages("gridExtra")
## Load ggsci for better looking colors
if(!require("ggsci")) install.packages("ggsci")
## Load gtable to get tools to make it easier to work with graphic ``tables''.
if(!require("gtable")) install.packages("gtable") 
## Load graphics for some base R graphics functions
if(!require("graphics")) install.packages("graphics")


################################################
# Libraries for data manipulation
## Load knitr for fine tuning
if(!require("knitr")) install.packages("knitr")
## Load dplyr for data manipulation
if(!require("dplyr")) install.packages("dplyr")
## Load tidyr for data manipulation
if(!require("tidyr")) install.packages("tidyr")
## Load broom for data manipulation
if(!require("broom")) install.packages("broom")
## Load tidyverse for data manipulation
if(!require("tidyverse")) install.packages("tidyverse")
## Load zoo package for data manipulation of the basis functions
if(!require("zoo")) install.packages("zoo")



################################################
# Libraries for the statistical analysis 
## Load lme4 for lme4
if(!require("lme4")) install.packages("lme4")
## Load lmerTest to add information into the summaries  
if(!require("lmerTest")) install.packages("lmerTest")
## Load mcgv to fit GAMs and GAMMs
if(!require("mgcv")) install.packages("mgcv")
## Load splines to use basic splines functions
if(!require("splines")) install.packages("splines")
## Load rms package to fit restricted cubic splines using Harrel's suggestions
if(!require("rms")) install.packages("rms")
## Load meta for univariate meta-analysis
if(!require("meta")) install.packages("meta")
## Load mvmeta for multivariate meta-analysis
if(!require("mvmeta")) install.packages("mvmeta")
## Load splines2 for splines assisting functions
if(!require("splines2")) install.packages("splines2")



## ----simulation of the single study data-set------------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Single study dataset.R")


## ----Figure 1: Simulated association between mortality risk and BMI in a single study"---------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 1.R")

## ----Simulation of IPD-sets with multiple studies --------------------------------------------------------------------------------------

## ----Simulation of first IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/First scenario data-set.R")

## ----Simulation of second IPD-MA scenario --------------------------------------------------------------------------------------
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")

## ----Simulation of third IPD-MA scenario --------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")


## ----Figure 2: Association between mortality risk and BMI per study in the heterogeneous IPD-set with equal BMI ranges"----
source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 2.R")

## ----Create a table reporting the mean and ranges of BMI per study for second scenario IPD-set -------------------------------------------------------------------------------------
table2 = df2%>%
    group_by(Study)%>%
  summarise("Range of BMI" =  paste("[", round(min(BMI),1), ",", round(max(BMI),1),"]", sep = ""), "Average BMI" =  round(mean(BMI),1))

colnames(table2) =  c("Studies","BMI Range","Average BMI")

table2

## ----Create a table reporting the mean and ranges of BMI per study for third scenario IPD-set -------------------------------------------------------------------------------------

table3 = df3%>%
  group_by(Study)%>%
  summarise("Range of BMI" =  paste("[", round(min(BMI),1), ",", round(max(BMI),1),"]", sep = ""), "Average BMI" =  round(mean(BMI),1))

colnames(table3) =  c("Studies","BMI Range","Average BMI")

table3


## ----Figure 3: Association between mortality risk and BMI per study in the non-heterogeneous IPD-set with different BMI ranges."----

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 3.R")


## ----Figure 4: Association between mortality risk and BMI per study in the combined IPD-set with different BMI ranges and between study differences in the mortality risks."----

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 4.R")

source("Code for Figures, Tables, Analysis and data-simulation/Appendix/Appendix.R")


## ----Script for splines fitting in single study scenario----------------------------------------------------------------------------


### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}



#### Clear enviroment and keep only the datasets
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])


source("Code for Figures, Tables, Analysis and data-simulation/Analysis code/Single study analysis and figures.R")





#

## Pointwise meta-analysis
## ----Point-wise meta-analysis results for heterogeneous IPD-set with equal BMI ranges---------------------------------------------------------------------------------
### Clear all environment besides the data-sets
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])

## Fit a RCS model per study
RCS.HT = df1%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5),
              knots = list(BMI = quantile(.$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))), 
          family = binomial("logit"), data = ., 
          method="REML" ))

## Fit a B-spline model per study
BS.HT = df1%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,m=c(2,0),k = 5),
                 family = binomial("logit"), data = ., 
    method="REML" ))

## Fit a P-spline model per study
PS.HT = df1%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="ps",fx=F,by = Treatment,k=17), 
          family = binomial("logit"), data = ., 
          method="REML" ))

## Fit a Smoothing splines model per study
SS.HT = df1%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="tp",fx=F,by = Treatment), 
          family = binomial("logit"), data = ., 
          method="REML" ))


## Create a data-set that will be used for estimating the predicted outcome

new.dat= data.frame(BMI= rep(seq(18.5,40,length.out = 250),each = 10),
                     Treatment = rep(unique(df1$Treatment),1250), 
                     Study =  rep(rep(unique(df1$Study),each =2),250))


## Calculate the predicted outcome per study (given the models fitted above)
predictions.RCS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 


predictions.BS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

predictions.PS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 


predictions.SS.HT= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.HT, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

## Second stage (pooling the regression lines per X*)
## Load the assisting function to perform pointwise meta-analysis 
source("Functions/Point-wise meta-analysis.R")



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
point.wise.DF.BS.HT.plot = point.wise.DF.BS.HT%>%
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
point.wise.DF.SS.HT.plot = point.wise.DF.SS.HT%>%
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



### Pseudo-plot to get legend
p1=  point.wise.DF.SS.HT%>%
  ggplot(aes(x = BMI, y = RE.meta,linetype =Treatment, color = Treatment)) +geom_line(size=2)+
    geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama( guide = FALSE)+ ylab("") + 
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
        legend.title =element_blank(),
        legend.position = "bottom") +  
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))

### save the legend
legend = gtable_filter(ggplotGrob(p1), "guide-box") 



## ----Point-wise meta-analysis results for second scenario IPD-set---------------------------------------------------------------------------------

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

### Load an assisting function to generate new data. 

source("Functions/New-data_for_simulated.R")


### Generate new dataset to calculate the predicted outcomes

new.dat= create.new.data(data = df2, step.size = 0.05,continuous.variable = "BMI",
                         clustering.variable = "Study",Treatment = "Treatment")


### Calculate the predicted outcomes per study 

### RCS
predictions.RCS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

### B-splines
predictions.BS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

### P-splines
predictions.PS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

### Smoothing splines
predictions.SS.DR= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.DR, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 


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


## ----Point-wise meta-analysis results for third scenario IPD-set-------------------------------------------------------------------------------

## Fit a RCS model per study
RCS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5),
              knots = list(BMI = quantile(.$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))), 
          family = binomial("logit"), data = ., 
          method="REML" ))

## Fit a B-splines model per study
BS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,m=c(2,0),k = 5),
                 family = binomial("logit"), data = ., 
    method="REML" ))

## Fit a P-splines model per study
PS.Comb= df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="ps",fx=F,by = Treatment,k=17), 
          family = binomial("logit"), data = ., 
          method="REML" ))

## Fit a Smoothing splines model per study
SS.Comb = df3%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = gam(formula = Y~ BMI + Treatment+ s(BMI,bs ="tp",fx=F,by = Treatment), 
          family = binomial("logit"), data = ., 
          method="REML" ))

### Generate new dataset to calculate the predicted outcomes
new.dat= create.new.data(data = df3, step.size = 0.05,continuous.variable = "BMI",
                         clustering.variable = "Study",Treatment = "Treatment")

### Calculate the predicted outcomes per study 

### RCS
predictions.RCS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(RCS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

### B-splines
predictions.BS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(BS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

### P-splines
predictions.PS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(PS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

### Smoothing splines
predictions.SS.Comb= new.dat%>%
  droplevels(new.dat$Study)%>% 
  arrange(desc(Study))%>%
  group_by(Study)%>%
  nest()%>%
  full_join(SS.Comb, by = "Study") %>% 
  group_by(Study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]])) 

## Second stage (pooling the regression lines per X*)


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
point.wise.DF.BS.Comb =  pointwise.ma(predictions.BS.Comb,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI","Treatment"), 
                                    predicted.outcome =  ".fitted", 
                                    predicted.outcome.se = ".se.fit", 
                                    predicted.outcome.CI = NULL, 
                                    tau.method = "REML") 

## Backtransform predicted outcomes and their corresponding confidence intervals

point.wise.DF.BS.Comb$RE.meta  =  expit(point.wise.DF.BS.Comb$RE.meta )
point.wise.DF.BS.Comb$RE.meta.upper  =  expit(point.wise.DF.BS.Comb$RE.meta.upper )
point.wise.DF.BS.Comb$RE.meta.lower  =  expit(point.wise.DF.BS.Comb$RE.meta.lower)

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


## B-splines
point.wise.DF.BS.Comb.plot = point.wise.DF.BS.Comb%>%
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




grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.plot, 
                         point.wise.DF.BS.HT.plot, 
                         point.wise.DF.PS.HT.plot, 
                         point.wise.DF.SS.HT.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = -1,gp = gpar(fontsize=32))),
             arrangeGrob(point.wise.DF.RCS.DR.plot, 
                         point.wise.DF.BS.DR.plot, 
                         point.wise.DF.PS.DR.plot, 
                         point.wise.DF.SS.DR.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = -1,gp = gpar(fontsize=32))),
             arrangeGrob(point.wise.DF.RCS.Comb.plot, 
                         point.wise.DF.BS.Comb.plot, 
                         point.wise.DF.PS.Comb.plot, 
                         point.wise.DF.SS.Comb.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = 0,gp = gpar(fontsize=32)),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                               rot = 0, vjust = 0,hjust= 0.25,
                             gp = gpar(fontsize=32))),
             legend,ncol = 1, heights = c(4,4,5,1),
             left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,
                             gp = gpar(fontsize=32)))




## ----absolute risk differences--------------------------------------------------------------------------------------------------------

### For each scenario, study and BMI calculate the predicted mortality risks
### RCS
### Heterogeneous IPD-set with equal BMI ranges
predictions.RCS.HT=predictions.RCS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))   

### Non-heterogeneous IPD-set with different BMI ranges
predictions.RCS.DR= predictions.RCS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.RCS.Comb= predictions.RCS.Comb %>%
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

### Non-heterogeneous IPD-set with different BMI ranges
predictions.BS.DR=predictions.BS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.BS.Comb=predictions.BS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### P-splines
### Heterogeneous IPD-set with equal BMI ranges
predictions.PS.HT=predictions.PS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### Non-heterogeneous IPD-set with different BMI ranges
predictions.PS.DR= predictions.PS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper))%>%
  mutate(BMI =  as.character(BMI))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.PS.Comb=predictions.PS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))     

### Smoothing splines
### Heterogeneous IPD-set with equal BMI ranges
predictions.SS.HT=predictions.SS.HT%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))

### Non-heterogeneous IPD-set with different BMI ranges
predictions.SS.DR=predictions.SS.DR%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))    

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
predictions.SS.Comb=predictions.SS.Comb%>%
  mutate(Lower =  .fitted - 1.96*.se.fit, 
         Upper =  .fitted + 1.96*.se.fit)%>%
  mutate(fit = expit(.fitted), Lower =  expit(Lower), Upper =  expit(Upper)) %>%
  mutate(BMI =  as.character(BMI))   
  

### Load assisting function
source("Functions/Create risk differences.R")

### For each scenario, study and BMI calculate the risk difference between the Control and Treated
### RCS
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_RCS.HT = risk.diff.creator(dataframe = predictions.RCS.HT,
                                         treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_RCS.DR = risk.diff.creator(dataframe = predictions.RCS.DR,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))


### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_RCS.Comb = risk.diff.creator(dataframe = predictions.RCS.Comb,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### B-splines
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_BS.HT = risk.diff.creator(dataframe = predictions.BS.HT,
                                        treatment = "Treatment",
                                        outcome = NULL, 
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_BS.DR = risk.diff.creator(dataframe = predictions.BS.DR,
                                        treatment = "Treatment",outcome = NULL, 
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_BS.Comb = risk.diff.creator(dataframe = predictions.BS.Comb,
                                          treatment = "Treatment",outcome = NULL, 
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", 
                  predicted.CI = c("Lower","Upper"))

### P-splines
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_PS.HT = risk.diff.creator(dataframe = predictions.PS.HT,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_PS.DR = risk.diff.creator(dataframe = predictions.PS.DR,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_PS.Comb = risk.diff.creator(dataframe = predictions.PS.Comb,
                                          treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Smoothing splines
### Heterogeneous IPD-set with equal BMI ranges
absolute_diff_SS.HT = risk.diff.creator(dataframe = predictions.SS.HT,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Non-heterogeneous IPD-set with different BMI ranges
absolute_diff_SS.DR = risk.diff.creator(dataframe = predictions.SS.DR,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
absolute_diff_SS.Comb = risk.diff.creator(dataframe = predictions.SS.Comb,
                                          treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI","Study"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



### Clean the data above. 
absolute_diff_RCS.HT=  absolute_diff_RCS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_RCS.DR=  absolute_diff_RCS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_RCS.Comb=  absolute_diff_RCS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_BS.HT=  absolute_diff_BS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_BS.DR=  absolute_diff_BS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_BS.Comb=  absolute_diff_BS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_PS.HT=  absolute_diff_PS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_PS.DR=  absolute_diff_PS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_PS.Comb=  absolute_diff_PS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.HT=  absolute_diff_SS.HT%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.DR=  absolute_diff_SS.DR%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.Comb=  absolute_diff_SS.Comb%>%
  select(Study, BMI, fit.diff, diff.lower, diff.upper)



### Second stage (pooling absolute risk differences per X*)
### RCS
### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_RCS.HT  =  pointwise.ma(data = absolute_diff_RCS.HT ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )

point.wise.absolute_diff_RCS.HT =  point.wise.absolute_diff_RCS.HT%>%
    mutate(BMI =  as.numeric(BMI))

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

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_RCS.Comb =  pointwise.ma(data = absolute_diff_RCS.Comb,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )
point.wise.absolute_diff_RCS.Comb =  point.wise.absolute_diff_RCS.Comb%>%
    mutate(BMI =  as.numeric(BMI))


### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_BS.HT  =  pointwise.ma(data = absolute_diff_BS.HT ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )

point.wise.absolute_diff_BS.HT =  point.wise.absolute_diff_BS.HT%>%
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

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_BS.Comb  =  pointwise.ma(data = absolute_diff_BS.Comb ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )

point.wise.absolute_diff_BS.Comb =  point.wise.absolute_diff_BS.Comb%>%
    mutate(BMI =  as.numeric(BMI))

### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_PS.HT  =  pointwise.ma(data = absolute_diff_PS.HT ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )
point.wise.absolute_diff_PS.HT =  point.wise.absolute_diff_PS.HT%>%
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

### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_PS.Comb  =  pointwise.ma(data = absolute_diff_PS.Comb ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )

point.wise.absolute_diff_PS.Comb =  point.wise.absolute_diff_PS.Comb%>%
    mutate(BMI =  as.numeric(BMI))

### Heterogeneous IPD-set with equal BMI ranges
point.wise.absolute_diff_SS.HT  =  pointwise.ma(data = absolute_diff_SS.HT ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )

point.wise.absolute_diff_SS.HT =  point.wise.absolute_diff_SS.HT%>%
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


### Combined IPD-set with different BMI ranges and between study differences in the mortality risks
point.wise.absolute_diff_SS.Comb  =  pointwise.ma(data = absolute_diff_SS.Comb ,
                                    clustering.variable = "Study",
                                    combining.variables = c("BMI"),
                                    predicted.outcome = "fit.diff",
                                    predicted.outcome.se = NULL,
                                    predicted.outcome.CI = c("diff.lower","diff.upper")
                                      )

point.wise.absolute_diff_SS.Comb =  point.wise.absolute_diff_SS.Comb%>%
    mutate(BMI =  as.numeric(BMI))



#### Draw the plots
point.wise.DF.RCS.HT.diff.plot = point.wise.absolute_diff_RCS.HT%>%
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


point.wise.absolute_diff_RCS.Comb[point.wise.absolute_diff_RCS.Comb$BMI == 31,] = NA

point.wise.DF.RCS.Comb.diff.plot = point.wise.absolute_diff_RCS.Comb%>%
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



point.wise.DF.BS.HT.diff.plot=point.wise.absolute_diff_BS.HT%>%
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


point.wise.DF.BS.Comb.diff.plot=point.wise.absolute_diff_BS.Comb%>%
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

point.wise.DF.PS.HT.diff.plot=point.wise.absolute_diff_PS.HT%>%
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





point.wise.DF.PS.Comb.diff.plot=point.wise.absolute_diff_PS.Comb%>%
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


point.wise.DF.SS.HT.diff.plot=point.wise.absolute_diff_SS.HT%>%
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



point.wise.DF.SS.Comb.diff.plot=point.wise.absolute_diff_SS.Comb%>%
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


grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.diff.plot, 
                         point.wise.DF.BS.HT.diff.plot, 
                         point.wise.DF.PS.HT.diff.plot, 
                         point.wise.DF.SS.HT.diff.plot, ncol=4),
             arrangeGrob(point.wise.DF.RCS.DR.diff.plot, 
                         point.wise.DF.BS.DR.diff.plot, 
                         point.wise.DF.PS.DR.diff.plot, 
                         point.wise.DF.SS.DR.diff.plot, ncol=4),
             arrangeGrob(point.wise.DF.RCS.Comb.diff.plot, 
                         point.wise.DF.BS.Comb.diff.plot, 
                         point.wise.DF.PS.Comb.diff.plot, 
                         point.wise.DF.SS.Comb.diff.plot, ncol=4),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                               rot = 0, vjust = 0,hjust = 0.25,
                             gp = gpar(fontsize=32)),
             left = textGrob(label = "Treatment effect (Absolute risk difference)",
                             rot = 90, vjust = 1,
                             gp = gpar(fontsize=32)))



## ----Mortality risk per treament------------------------------------------------------------------------------------------------------
### MVmeta
### Clear enviroment 
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")])

###########
##### First scenario
###########


########### RCS  indicator
###########

### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.df1 =   list(BMI=quantile(df1$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))) 

## The formula for all regions is the same so we save it 

formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = 5)

# Number of studies in each region

nstudies.df1 = length(unique(df1$Study))



### Fit a stacked analysis in the whole data-set to get the model matrix
fit.df1 = gam( formula =formula,
               knots = Knots.df1,
               family = binomial("logit"), 
               data = df1)

### Get the model matrices for each data-set

X.p.df1 =  model.matrix(fit.df1)

### Create empty matrices for the estimated splines coefficients

estimated.coefficients.df1 = matrix(NA,
                                    ncol = length(fit.df1$coefficients),nrow=nstudies.df1,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.df1)),1, paste, collapse=" "),
                                                     c(1:length(fit.df1$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.df1 = matrix(NA, ncol=sum(c(1:length(fit.df1$coefficients))), nrow = nstudies.df1 )




k=3
j=1

for( i in unique(df1$Study)){
  
  ### Get a mini df1 with only one study
  minidf1 = df1%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,knots = Knots.df1, family = binomial("logit"), data = minidf1)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.df1[j,] = fit$coefficients
  S.df1[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
  rm(i,minidf1,fit)
}
rm(k,j)




#### Perform for each region a multivariate meta-analysis
mv.fit.df1 = mvmeta(estimated.coefficients.df1, S.df1)


prediction.interval.mvmeta.df1 =  X.p.df1 %*% coef(mv.fit.df1)

prediction.interval.mvmeta.lower.df1 =  X.p.df1 %*% coef(mv.fit.df1) - sqrt( rowSums(X.p.df1  * (X.p.df1  %*% vcov(mv.fit.df1)))) * qt(0.025, dim(df1)[1] - 3)

prediction.interval.mvmeta.upper.df1 =  X.p.df1 %*% coef(mv.fit.df1) + sqrt( rowSums(X.p.df1  * (X.p.df1  %*% vcov(mv.fit.df1)))) * qt(0.025, dim(df1)[1] - 3)


mvmeta.df.RCS = cbind(df1[,c("Study","BMI","Treatment")],
                   fit =  expit(prediction.interval.mvmeta.df1), 
                   Lower= expit(prediction.interval.mvmeta.lower.df1),
                   Upper =expit(prediction.interval.mvmeta.upper.df1 ))





g.mvmeta.total.RCS = ggplot(mvmeta.df.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
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
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS
###########



########### B-splines    indicator
###########


### First we get the knots from each data-set 3 inner equidistant knots and 6 outer (3 on both sides of the boundaries) 


## The formula for all regions is the same so we save it 

formula = Y~ BMI + Treatment+ s(BMI,bs ="bs",fx=T,by = Treatment,k = 5,m=c(2,0))

# Number of studies in each region

nstudies.df1 = length(unique(df1$Study))



### Fit a stacked analysis for each data-set to get the model matrix
fit.df1 = gam( formula =formula,family = binomial("logit"), data = df1)

### Get the model matrices for each data-set

X.p.df1 =  model.matrix(fit.df1)


### Create empty matrices for the estimated splines coefficients

estimated.coefficients.df1 = matrix(NA,
                                    ncol = length(fit.df1$coefficients),nrow=nstudies.df1,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.df1)),1, paste, collapse=" "),
                                                     c(1:length(fit.df1$coefficients))))

### Create empty matrices for the variance-covariance matrix of the coefficients

S.df1 = matrix(NA, ncol=sum(c(1:length(fit.df1$coefficients))), nrow = nstudies.df1 )



k=3
j=1

for( i in unique(df1$Study)){
  
  ### Get a mini df1 with only one study
  minidf1 = df1%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,family = binomial("logit"), data = minidf1)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.df1[j,] = fit.df1$coefficients
  S.df1[j,] = vcov(fit.df1)[lower.tri(vcov(fit.df1), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
  rm(i,minidf1,fit)
}
rm(k,j)






#### Perform for each region a multivariate meta-analysis
mv.fit.df1 = mvmeta(estimated.coefficients.df1, S.df1)


prediction.interval.mvmeta.df1 =  X.p.df1 %*% coef(mv.fit.df1)

prediction.interval.mvmeta.lower.df1 =  X.p.df1 %*% coef(mv.fit.df1) - sqrt( rowSums(X.p.df1  * (X.p.df1  %*% vcov(mv.fit.df1))) ) * qt(0.025, dim(df1)[1] - 3)

prediction.interval.mvmeta.upper.df1 =  X.p.df1 %*% coef(mv.fit.df1) + sqrt( rowSums(X.p.df1  * (X.p.df1  %*% vcov(mv.fit.df1))) ) * qt(0.025, dim(df1)[1] - 3)




mvmeta.df.BS = cbind(df1[,c("Study","BMI","Treatment")],
                   fit =  expit(prediction.interval.mvmeta.df1), 
                   Lower= expit(prediction.interval.mvmeta.lower.df1),
                   Upper =expit(prediction.interval.mvmeta.upper.df1 ))



g.mvmeta.total.BS = ggplot(mvmeta.df.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
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
        legend.title =element_blank(),
        legend.position = "none")   + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " b", size= 12 )

g.mvmeta.total.BS


p1=  ggplot(mvmeta.df.BS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.15)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
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
        legend.title =element_blank(),
        legend.position = "bottom") + 
  annotate("text",x = 19.25,y=0.9, size = 10, label = "d") +ylim(c(0,1))

library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)


legend = gtable_filter(ggplotGrob(p1), "guide-box") 



library(grid)
MV_meta_plot_HT = grid.arrange(arrangeGrob(g.mvmeta.total.RCS,g.mvmeta.total.BS,
                                           ncol = 2, 
right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32))),
legend, heights=  c(10,1))



###########



## ----Absolute risk differences ---------------------------------------------------------------------------------------------------------
### MVmeta
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","mvmeta.df.RCS","mvmeta.df.BS")])

source("Functions/Create risk differences.R")


MV.meta_absolute_difference.RCS.HT =  risk.diff.creator(dataframe = mvmeta.df.RCS,
                                                 treatment = "Treatment",
                                                 matching.variables=  c("BMI"),
                              outcome= NULL, 
                              predicted.outcome = "fit", 
                              predicted.CI = c("Lower","Upper"))



MV.meta_absolute_difference.BS.HT =  risk.diff.creator(dataframe = mvmeta.df.BS,
                                                 treatment = "Treatment",
                                                 matching.variables=  c("BMI"),
                              outcome= NULL, 
                              predicted.outcome = "fit", 
                              predicted.CI = c("Lower","Upper"))




MV.meta_absolute_difference.RCS.HT.plot = ggplot(MV.meta_absolute_difference.RCS.HT,
                                                 aes(x = BMI, fit.diff)) + 
  geom_line(size=2)+ 
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ theme_minimal()+
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
  annotate("text",x = 19.25,y=0.2, size = 10, label = "a")


MV.meta_absolute_difference.BS.HT.plot = 
  ggplot(MV.meta_absolute_difference.BS.HT,aes(x = BMI, fit.diff)) + 
  geom_line(size=2)+ 
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ 
  geom_ribbon(mapping = aes(ymin=diff.lower, ymax=diff.upper),alpha=0.25)+
  geom_hline(yintercept = 0, linetype=2)+ theme_minimal()+
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
  annotate("text",x = 19.25,y=0.2, size = 10, label = "b")


MV_meta_plot_absolute_diff_HT = grid.arrange(MV.meta_absolute_difference.RCS.HT.plot,MV.meta_absolute_difference.BS.HT.plot,
                                           ncol = 2, 
right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)),
bottom= textGrob(label = expression(BMI (Kg/m^2)),vjust=0.25, hjust = 0.25,gp = gpar(fontsize=32)))







###########
##### Second scenario 
###########


### Clear enviroment 
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")])
########## Hereby the studies have different domains. 


df2%>%
  group_by(Study)%>%
  summarise(`Minimum BMI` =  min(BMI), `Maximum BMI` =  max(BMI))


### However in order to apply the splines described above we have to fit them in the same domain. 
### A work-around that has been proposed by Riley et al. (2020) is to generate pseudo data with 
### low weight near the boundaries of the effect modifier.
### These pseudo data will help us generate the beta coefficients we need to pool during the second stage. 
### In our case, the boundaries of BMI are 18.5 and 40. 

Lower.Boundary.Outcome.Control =  mean(df2[df2$BMI<19 & df2$Treatment=="Control",]$Y) ### ~27%
Lower.Boundary.Outcome.Treated =  mean(df2[df2$BMI<19 & df2$Treatment=="Treated",]$Y) ### ~13%

Upper.Boundary.Outcome.Control =  mean(df2[df2$BMI>39.5 & df2$Treatment=="Control",]$Y) ### ~71%
Upper.Boundary.Outcome.Treated =  mean(df2[df2$BMI>39.5 & df2$Treatment=="Treated",]$Y) ### ~43%


#### Now we generate pseudo data for all studies. Note that we don't need to be precise. 


tmp.lower =  do.call("rbind", replicate(4, df2[df2$BMI <19, ], simplify = FALSE))
tmp.lower$Study = rep(c("2nd Study", "3rd Study", "4th Study", "5th Study"), each =  dim(tmp.lower)[1]/4)

tmp.upper =  do.call("rbind", replicate(4, df2[df2$BMI >39.5, ], simplify = FALSE))
tmp.upper$Study = rep(c("2nd Study", "3rd Study", "4th Study", "5th Study"), each =  dim(tmp.upper)[1]/4)
tmp =  rbind(tmp.lower, tmp.upper)

rm(tmp.lower, tmp.upper, Lower.Boundary.Outcome.Control, Lower.Boundary.Outcome.Treated, Upper.Boundary.Outcome.Control, Upper.Boundary.Outcome.Treated)



########### RCS  indicator
###########

### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.df2 =   list(BMI=c(21, 25, 29, 31, 33.5, 37.8))

## The formula for all regions is the same so we save it 

formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = length(Knots.df2$BMI))

# Number of studies

nstudies.df2 = length(unique(df2$Study))

### Fit a stacked analysis in the whole data-set to get the model matrix
fit.df2 = gam( formula =formula,
               knots = Knots.df2,
               family = binomial("logit"), 
               data = df2)

### Get the model matrix from the stacked GAM.

X.p.df2 =  model.matrix(fit.df2)

### Create an empty matrix for the estimated coefficients and their variance covariance matrix

estimated.coefficients.df2 = matrix(NA,
                                    ncol = length(fit.df2$coefficients),nrow=nstudies.df2,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.df2)),1, paste, collapse=" "),
                                                     c(1:length(fit.df2$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.df2 = matrix(NA, ncol=sum(c(1:length(fit.df2$coefficients))), nrow = nstudies.df2 )

df2$wgt =  c(1)
tmp$wgt =  c(0.00001)
df2.plus =  rbind(df2, tmp)


k=3
j=1

for( i in unique(df2.plus$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2.plus%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,knots = Knots.df2, family = binomial("logit"),weights = wgt, data = minidf2)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.df2[j,] = fit$coefficients
  S.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
}
rm(k,j)




#### Perform for each region a multivariate meta-analysis
mv.fit.df2 = mvmeta(estimated.coefficients.df2, S.df2)


prediction.interval.mvmeta.df2 =  X.p.df2 %*% coef(mv.fit.df2)

prediction.interval.mvmeta.lower.df2 =  X.p.df2 %*% coef(mv.fit.df2) - sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.df2 =  X.p.df2 %*% coef(mv.fit.df2) + sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df.RCS = cbind(df2[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.df2 ))





g.mvmeta.total.RCS.df2 = ggplot(mvmeta.df.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
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
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS.df2
###########

########### BS  indicator
###########

### Define the knots positions
Knots.df2 =   list(BMI=c(21, 25, 29, 31, 33.5, 37.8))

## The formula for all regions is the same so we save it 

formula = Y~ BMI + Treatment+ s(BMI,bs ="cr",fx=T,by = Treatment,k = length(Knots.df2$BMI))

# Number of studies

nstudies.df2 = length(unique(df2$Study))

### Fit a stacked analysis in the whole data-set to get the model matrix
fit.df2 = gam( formula =formula,
               knots = Knots.df2,
               family = binomial("logit"), 
               data = df2)

### Get the model matrix from the stacked GAM.

X.p.df2 =  model.matrix(fit.df2)

### Create an empty matrix for the estimated coefficients and their variance covariance matrix

estimated.coefficients.df2 = matrix(NA,
                                    ncol = length(fit.df2$coefficients),nrow=nstudies.df2,
                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.df2)),1, paste, collapse=" "),
                                                     c(1:length(fit.df2$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.df2 = matrix(NA, ncol=sum(c(1:length(fit.df2$coefficients))), nrow = nstudies.df2 )


df2.plus =  rbind(df2, tmp.lower, tmp.upper)


k=3
j=1

for( i in unique(df2.plus$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2.plus%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,knots = Knots.df2, family = binomial("logit"), data = minidf2)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.df2[j,] = fit$coefficients
  S.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
}
rm(k,j)




#### Perform for each region a multivariate meta-analysis
mv.fit.df2 = mvmeta(estimated.coefficients.df2, S.df2)


prediction.interval.mvmeta.df2 =  X.p.df2 %*% coef(mv.fit.df2)

prediction.interval.mvmeta.lower.df2 =  X.p.df2 %*% coef(mv.fit.df2) - sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.df2 =  X.p.df2 %*% coef(mv.fit.df2) + sqrt( rowSums(X.p.df2  * (X.p.df2  %*% vcov(mv.fit.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df.RCS = cbind(df2[,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.df2 ))





g.mvmeta.total.RCS.df2 = ggplot(mvmeta.df.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
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
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS.df2
###########




## ----GAMMs----------------------------------------------------------------------------------------------------------------

########
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")]) ### To clear all environment besides the data-set
Knots= list (BMI = (quantile(df1$BMI , probs = c(0.05,0.275,0.5,0.725,0.95))))

fit.RCS.HT = gam(Y ~BMI+ Treatment+ 
                   s(BMI,by = Treatment,fx = T,bs="cr",k = 5) +  
                   s(Study,bs = "re") +  
                   s(Study,BMI,bs = "re")+  
                   s(Study,Treatment,bs = "re"),knots = Knots,
                 family = binomial("logit"),data = df1, nthreads = 8, method = "REML")

fit.BS.HT = gam(Y ~BMI+ Treatment+ 
                  s(BMI,by = Treatment,fx = T,bs="bs",k=5, m=c(2,0)) +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df1, nthreads = 8, method = "REML")


fit.PS.HT = gam(Y ~BMI+ Treatment+ 
                  s(BMI,by = Treatment,fx = F,bs="ps",k=17) +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df1, nthreads = 8, method = "REML")


fit.SS.HT = gam(Y ~BMI+ Treatment+ 
                  s(BMI,by = Treatment,bs="tp") +  
                  s(Study,bs = "re") +  
                  s(Study,BMI,bs = "re")+  
                  s(Study,Treatment,bs = "re"),
                family = binomial("logit"), data = df1, nthreads = 8, method = "REML")


new.data = data.frame(cbind(df1[,c("Study","BMI","Treatment")]))


preds.RCS.HT=  predict.gam(fit.RCS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.BS.HT=  predict.gam(fit.BS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.PS.HT=  predict.gam(fit.PS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)

preds.SS.HT=  predict.gam(fit.SS.HT, se.fit = T,newdata = new.data,newdata.guaranteed = T, exclude = c("s(Study)","s(Study,BMI)","s(Study,Treatment)"))%>%
  as.data.frame()%>%cbind(new.data)




preds.RCS.HT$lower = preds.RCS.HT$fit -1.96*preds.RCS.HT$se.fit
preds.BS.HT$lower  = preds.BS.HT$fit  -1.96*preds.BS.HT$se.fit
preds.PS.HT$lower  = preds.PS.HT$fit  -1.96*preds.PS.HT$se.fit
preds.SS.HT$lower  = preds.SS.HT$fit -1.96*preds.SS.HT$se.fit

preds.RCS.HT$upper = preds.RCS.HT$fit +1.96*preds.RCS.HT$se.fit
preds.BS.HT$upper  = preds.BS.HT$fit +1.96*preds.BS.HT$se.fit
preds.PS.HT$upper  = preds.PS.HT$fit +1.96*preds.PS.HT$se.fit
preds.SS.HT$upper  = preds.SS.HT$fit +1.96*preds.SS.HT$se.fit







g.GAMM.RCS.HT= ggplot(preds.RCS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "a") +ylim(c(0,1))


g.GAMM.BS.HT= ggplot(preds.BS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "b") +ylim(c(0,1))


g.GAMM.PS.HT= ggplot(preds.PS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19.25,y=0.8, size = 10,label = "c") +ylim(c(0,1))


g.GAMM.SS.HT= ggplot(preds.SS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="none") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "d") +ylim(c(0,1))


p1= ggplot(preds.SS.HT, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
        legend.position = "top") +theme(legend.position="top") + 
  annotate("text",x = 19.25,y=0.8, size = 10, label = "d") +ylim(c(0,1))


g.legend =  get_legend(p1)


g.GAMM.HT=grid.arrange(g.GAMM.RCS.HT,g.GAMM.BS.HT,g.GAMM.PS.HT,g.GAMM.SS.HT, ncol=4, right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)))






########




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


########


fit.RCS.Combined = gam(Y~  BMI+ Treatment+ 
                         s(BMI,by = Treatment,bs="cr",fx = T, k = 5) +  
                         s(Study,bs = "re") +  
                         s(Study,BMI,bs = "re")+  
                         s(Study,Treatment,bs = "re"),knots = Knots,
                       family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.BS.Combined = gam(Y~  BMI+ Treatment+ 
                        s(BMI,by = Treatment,fx = T,bs="bs",k=5, m=c(2,0)) +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.PS.Combined = gam(Y~  BMI+ Treatment+ 
                        s(BMI,by = Treatment,bs="ps", k = 19) +  
                        s(Study,bs = "re") +  
                        s(Study,BMI,bs = "re")+  
                        s(Study,Treatment,bs = "re"),
                      family = binomial("logit"), data = df3, nthreads = 8, method = "REML")


fit.SS.Combined = gam(Y~  BMI+ Treatment+ 
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


g.GAMM.BS.Combined=ggplot(preds.BS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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


g.GAMM.PS.Combined=ggplot(preds.PS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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
  annotate("text",x = 19.25,y=0.8, size = 10,label = "c") +ylim(c(0,1))


g.GAMM.SS.Combined=ggplot(preds.SS.Combined, aes(BMI, expit(fit), linetype= Treatment,color = Treatment)) + 
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



g.GAMM.Combined = grid.arrange(g.GAMM.RCS.Combined,g.GAMM.BS.Combined,g.GAMM.PS.Combined,g.GAMM.SS.Combined, ncol=4, right = textGrob(label = "", vjust = -1,gp = gpar(fontsize=32)))

grid.arrange(arrangeGrob(g.GAMM.HT,g.GAMM.DR,g.GAMM.Combined, ncol=1,
                         bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))), vjust=0.35,
                                          hjust = 0.35,gp = gpar(fontsize=32)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=32))), heights =c(12,1), g.legend)


## ----GAMMs Treatment effect plot------------------------------------------------------------------------------------------------------


preds.RCS.HT = preds.RCS.HT %>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))

preds.RCS.DR= preds.RCS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.RCS.Combined= preds.RCS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.BS.HT= preds.BS.HT%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.BS.DR = preds.BS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.BS.Combined = preds.BS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.PS.HT=preds.PS.HT%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.PS.DR=preds.PS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.PS.Combined=preds.PS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.SS.HT=preds.SS.HT%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.SS.DR=preds.SS.DR%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))


preds.SS.Combined=preds.SS.Combined%>%
  mutate(Lower =  fit - 1.96*se.fit, 
         Upper =  fit + 1.96*se.fit)%>%
  mutate(fit = expit(fit), Lower =  expit(Lower), Upper =  expit(Upper))



source("Functions/Create risk differences.R")


absolute_diff_RCS.HT = risk.diff.creator(dataframe = preds.RCS.HT,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", 
                  predicted.CI = c("Lower","Upper"))

absolute_diff_RCS.DR = risk.diff.creator(dataframe = preds.RCS.DR,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



absolute_diff_RCS.Comb = risk.diff.creator(dataframe = preds.RCS.Combined,
                                         treatment = "Treatment", 
                                         outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))



absolute_diff_BS.HT = risk.diff.creator(dataframe = preds.BS.HT,
                                        treatment = "Treatment",
                                        outcome = NULL, 
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_BS.DR = risk.diff.creator(dataframe = preds.BS.DR,
                                        treatment = "Treatment",outcome = NULL, 
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_BS.Comb = risk.diff.creator(dataframe = preds.BS.Combined,
                                          treatment = "Treatment",outcome = NULL, 
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", 
                  predicted.CI = c("Lower","Upper"))

absolute_diff_PS.HT = risk.diff.creator(dataframe = preds.PS.HT,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_PS.DR = risk.diff.creator(dataframe = preds.PS.DR,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_PS.Comb = risk.diff.creator(dataframe = preds.PS.Combined,
                                          treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_SS.HT = risk.diff.creator(dataframe = preds.SS.HT,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_SS.DR = risk.diff.creator(dataframe = preds.SS.DR,
                                        treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))

absolute_diff_SS.Comb = risk.diff.creator(dataframe = preds.SS.Combined,
                                          treatment = "Treatment", outcome = NULL,
                         matching.variables = c("BMI"),
                  predicted.outcome = "fit", predicted.CI = c("Lower","Upper"))
  

absolute_diff_RCS.HT=  absolute_diff_RCS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_RCS.DR=  absolute_diff_RCS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_RCS.Comb=  absolute_diff_RCS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_BS.HT=  absolute_diff_BS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_BS.DR=  absolute_diff_BS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_BS.Comb=  absolute_diff_BS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_PS.HT=  absolute_diff_PS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_PS.DR=  absolute_diff_PS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_PS.Comb=  absolute_diff_PS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.HT=  absolute_diff_SS.HT%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.DR=  absolute_diff_SS.DR%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


absolute_diff_SS.Comb=  absolute_diff_SS.Comb%>%
  select( BMI, fit.diff, diff.lower, diff.upper)


source("Functions/Point-wise meta-analysis.R")


#### Draw the plots
GAMM.DF.RCS.HT.diff.plot = absolute_diff_RCS.HT%>%
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


GAMM.DF.RCS.Comb.diff.plot = absolute_diff_RCS.Comb%>%
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



GAMM.DF.BS.HT.diff.plot=absolute_diff_BS.HT%>%
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

GAMM.DF.BS.Comb.diff.plot=absolute_diff_BS.Comb%>%
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

GAMM.DF.PS.HT.diff.plot=absolute_diff_PS.HT%>%
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


GAMM.DF.PS.Comb.diff.plot=absolute_diff_PS.Comb%>%
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


GAMM.DF.SS.HT.diff.plot=absolute_diff_SS.HT%>%
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

GAMM.DF.SS.Comb.diff.plot=absolute_diff_SS.Comb%>%
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


grid.arrange(arrangeGrob(GAMM.DF.RCS.HT.diff.plot, 
                         GAMM.DF.BS.HT.diff.plot, 
                         GAMM.DF.PS.HT.diff.plot, 
                         GAMM.DF.SS.HT.diff.plot, ncol=4),
             arrangeGrob(GAMM.DF.RCS.DR.diff.plot, 
                         GAMM.DF.BS.DR.diff.plot, 
                         GAMM.DF.PS.DR.diff.plot, 
                         GAMM.DF.SS.DR.diff.plot, ncol=4),
             arrangeGrob(GAMM.DF.RCS.Comb.diff.plot, 
                         GAMM.DF.BS.Comb.diff.plot, 
                         GAMM.DF.PS.Comb.diff.plot, 
                         GAMM.DF.SS.Comb.diff.plot, ncol=4),
             bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                              hjust = 0,gp = gpar(fontsize=32)), 
             left = textGrob(label = "Treatment effect (Absolute risk difference)", rot = 90, vjust = 1,gp = gpar(fontsize=32)))





