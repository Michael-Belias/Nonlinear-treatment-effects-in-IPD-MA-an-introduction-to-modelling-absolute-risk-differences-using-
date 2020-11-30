
## ----Load necessary libraries -----------------------------------------------------------------------
rm(list=ls()) ### To clear enviroment
# set.wd() ## use this function to define your personal workspace



# Libraries for loading and saving data
## Load haven for loading sas data-sets 
if(!require("haven")) install.packages("haven")
## Load haven for loading sas data-sets 
if(!require("sas7bdat")) install.packages("sas7bdat")



## ---- Libraries for plotting-----------------------------------------------------------------------
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


## ---- Libraries for data manipulation -----------------------------------------------------------------------
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



## ---- Libraries for the statistical analysis -----------------------------------------------------------------------

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



## ---- Simulation of the single study data-set------------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Single study dataset.R")

## ----Figure 1: Simulated association between mortality risk and BMI in a single study"---------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 1.R");simulated_plot_single_df

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 1.png",width = 1240, height = 1680) 
simulated_plot_single_df

dev.off()
## ----Simulation of IPD-sets with multiple studies --------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/First scenario data-set.R")

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")

#Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}

## ----Figure 2: Association between mortality risk and BMI per study in the heterogeneous IPD-set with equal BMI ranges"----
source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 2.R")
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 2.png",width = 1240, height = 1680) 
simulated_plot1
dev.off()

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

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 3.R");grid::grid.draw(simulated_plot2)

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 3.png",width = 1240, height = 1680) 
grid::grid.draw(simulated_plot2)
dev.off()

## ----Figure 4: Association between mortality risk and BMI per study in the combined IPD-set with different BMI ranges and between study differences in the mortality risks."----

source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 4.R");grid::grid.draw(simulated_plot3)
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 4.png",width = 1240, height = 1680) 
grid::grid.draw(simulated_plot3)
dev.off()
## ----Script for splines fitting in single study scenario----------------------------------------------------------------------------

#### Clear enviroment and keep only the datasets
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])


source("Code for Figures, Tables, Analysis and data-simulation/Analysis code/Single study analysis and figures.R")
## Figure 6 
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 6.png",width = 1240, height = 1680) 
grid.arrange(arrangeGrob(gRCS,gBS,gPS,gSS,
                         bottom= textGrob(label = expression(BMI (Kg/m^2)), hjust = 0,gp = gpar(fontsize=32)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=32))), 
             legend, heights= c(10,1))
dev.off()



## Figure 7
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 7.png",width = 1240, height = 1680) 
grid.arrange(gRCS_absolute_diff,gBS_absolute_diff,gPS_absolute_diff,gSS_absolute_diff,
             bottom= textGrob(label = expression(BMI (Kg/m^2)), hjust = 0.35,gp = gpar(fontsize=32)), 
             left = textGrob(label = "Treatment effect plot (absolute risk differences)", rot = 90, vjust = 1,gp = gpar(fontsize=32)))
dev.off()

## ----Pointwise meta-analysis---------------------------------------------------------------------------------
## Pointwise meta-analysis for the three scenarios

source("Pointwise meta-analysis/Pointwise meta-analysis (1st scenario).R")

source("Pointwise meta-analysis/Pointwise meta-analysis (2nd scenario).R")

source("Pointwise meta-analysis/Pointwise meta-analysis (3rd scenario).R")


#### Figure 8

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 8.png",width = 1240, height = 1680) 

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

dev.off()



#### Figure 9

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 9.png",width = 1240, height = 1680) 

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

dev.off()

## ----          MVmeta                  ------------------------------------------------------------------------------------------------------


source("Multivariate meta-analysis/Multivariate meta-analysis (1st scenario).R")

source("Multivariate meta-analysis/Multivariate meta-analysis (2nd scenario).R")

source("Multivariate meta-analysis/Multivariate meta-analysis (3rd scenario).R")


###### Figure 10


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 10.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(g.mvmeta.total.RCS, 
                         g.mvmeta.total.BS, 
                         g.mvmeta.total.RCS.DR, g.mvmeta.total.BS.DR,g.mvmeta.total.RCS.Comb, g.mvmeta.total.BS.Comb),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                               rot = 0, vjust = 0,hjust = 0.25,
                               gp = gpar(fontsize=32)),
             left = textGrob(label = "Mortality risk",
                             rot = 90, vjust = 1,
                             gp = gpar(fontsize=32)))

dev.off()


###### Figure 11

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 11.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(MV.meta_absolute_difference.RCS.HT.plot,MV.meta_absolute_difference.BS.HT.plot,
                         MV.meta_absolute_difference.RCS.DR.plot, MV.meta_absolute_difference.BS.DR.plot, 
                         MV.meta_absolute_difference.RCS.Comb.plot, MV.meta_absolute_difference.BS.Comb.plot),
             bottom = textGrob(label = expression(paste("BMI ", (Kg/m^2))), 
                               rot = 0, vjust = 0,hjust = 0.25,
                               gp = gpar(fontsize=32)),
             left = textGrob(label = "Treatment effect (Absolute risk difference)",
                             rot = 90, vjust = 1,
                             gp = gpar(fontsize=32)))
dev.off()

### Clear enviroment 
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit")])

## ----GAMMs----------------------------------------------------------------------------------------------------------------

source("GAMM/GAMM (1st scenario).R")

source("GAMM/GAMM (2nd scenario).R")

source("GAMM/GAMM (3rd scenario).R")

## Figure 12

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 12.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(g.GAMM.HT,g.GAMM.DR,g.GAMM.Combined, ncol=1,
                         bottom= textGrob(label = expression(paste("BMI ", (Kg/m^2))), vjust=0.35,
                                          hjust = 0.35,gp = gpar(fontsize=32)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=32))), heights =c(12,1), g.legend)


dev.off()


# Figure 13


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 13.png",width = 1240, height = 1680) 

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

dev.off()


## ----Empirical example------------------------------------------------------------------------------------------------------



rm(list=ls()[! ls() %in% c("expit")]) ### To clear all 
IPDMA <- read.sas7bdat("Data/IPDMA1.sas7bdat")
IPDMA[IPDMA == "NaN"] = NA 

names(IPDMA) <- tolower(names(IPDMA))
IPDMA$treat =  factor(IPDMA$treat  , labels = c("Placebo","Antibiotics") )
IPDMA$study = factor(IPDMA$study, labels = c("Damoiseaux","Burke","Appelman","Little","Saux","McCormick"))
IPDMA$bilat_0 =  factor(IPDMA$bilat_0  , labels = c("No","Yes") )

IPDMA =  IPDMA[which(IPDMA$age<=9),]


### Ommit Little study since they don't provide bilaterality information
miniIPD= IPDMA%>% filter((study != "Little"))

### Ommit Damoiseaux and Burke since they have only 4 and 7 levels in the age variable
a=miniIPD%>%
  group_by(study, bilat_0)%>%
  summarise(`Age mean` = mean(age),
            `Age range` = paste("[", round(min(age),3), ", ",round(max(age),3),"]", sep = ""),
            `Levels of Age` = length(unique(age)))%>%
  group_by(bilat_0)%>%
  group_split()

a =  merge(x = a[[1]],y = a[[2]], by= c("study"),suffixes = c(" Unilateral"," Bilateral") )%>%
  select(-contains("bilat_0"))
a
rm(a)

IPDMA[IPDMA$study == "Burke",]$age =  jitter(IPDMA[IPDMA$study == "Burke",]$age )

#### Check the number of events/ observations 


b=miniIPD%>%
  mutate(age_group=cut(age, breaks=c(-Inf, 0, 3, 6, Inf), labels=c("Baby","Toddler","Preschool","Gradeschooler")))%>%
  group_by(study, bilat_0, age_group, treat)%>%
  summarise(`Number of events` = sum(poutcome), 
            `Number of observations`= n())

b
rm(b)

c=miniIPD%>%
  group_by(study, bilat_0,  treat)%>%
  summarise(`Number of events` = sum(poutcome), 
            `Number of observations`= n())

c
rm(c)


## ----Pointwise meta-analysis on empirical example ------------------------------------------------------------------------------------------------------


### Gererate the prediction data.frames

source("Assisting functions/Point-wise meta-analysis.R")


### Ommit Damoiseaux and Burke since they have only 4 and 7 levels in the age variable
miniIPD.splines= miniIPD%>% filter(( study != "Burke" & study != "Appelman"))
miniIPD.linear= miniIPD%>% filter((  study == "Burke" | study == "Appelman"))


linear.Models =  miniIPD.linear%>%
  droplevels(miniIPD.linear$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = glm(formula = poutcome ~ treat + bilat_0 + treat*bilat_0  + age , 
                 family = binomial("logit"), data = .))



RCS = miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = gam(formula = poutcome ~   treat + bilat_0 + treat*bilat_0  + age +
                   s(age, by = bilat_0, bs="cr",fx=T,  k=3)+
                   s(age, by = treat, bs="cr",fx=T, k=3), 
                 knots = list(age=quantile(.$age, probs = c(0.1,0.5,0.9))), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))%>%
  rbind(., linear.Models)



BS = miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = gam(formula = poutcome ~ treat + bilat_0 + treat*bilat_0  + age +
                   s(age, by = treat,fx = T, bs="bs",k=3,m= c(1,0))+
                   s(age, by = bilat_0,fx = T, bs="bs",k=3,m= c(1,0)),
                 family = binomial("logit"),
                 data = ., 
                 method="REML" ))%>%
  rbind(., linear.Models)



PS = miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = gam(formula = poutcome ~ treat + bilat_0 + treat*bilat_0  + age +
                   s(age, by = treat,   bs="ps",fx = F,k=17)+
                   s(age, by = bilat_0, bs="ps",fx = F,k=17), 
                 family = binomial("logit"), data = ., 
                 method="REML" ))%>%
  rbind(., linear.Models)

SS= miniIPD.splines%>%
  droplevels(miniIPD.splines$study)%>% 
  arrange(desc(study))%>%
  group_by(study) %>%
  do(model = gam(formula = poutcome ~ treat + bilat_0 + treat*bilat_0  + age +
                   s(age, by = treat, bs="tp")+
                   s(age, by = bilat_0, bs="tp"), 
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
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "bottom")


predictions.BS= new.dat%>%
  droplevels(miniIPD$study)%>% 
  arrange(desc(study))%>%
  group_by(study)%>%
  nest()%>%
  full_join(BS, by = c("study")) %>% 
  group_by(study)%>% 
  do(augment(.$model[[1]], newdata = .$data[[1]],se_fit=T)) 



predictions.BS[predictions.BS$study != "Appelman",]%>%
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
        axis.text.y = element_text(face="bold",  size=18),
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
        axis.text.y = element_text(face="bold",  size=18),
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
        axis.text.y = element_text(face="bold",  size=18),
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


point.wise.DF.BS =  pointwise.ma(data = predictions.BS,
                                 clustering.variable = "study",
                                 combining.variables = c("treat","bilat_0","age"),
                                 predicted.outcome = ".fitted",
                                 predicted.outcome.se = ".se.fit",
                                 tau.method = "REML")




point.wise.DF.BS$RE.meta  =  expit(point.wise.DF.BS$RE.meta )
point.wise.DF.BS$RE.meta.upper  =  expit(point.wise.DF.BS$RE.meta.upper )
point.wise.DF.BS$RE.meta.lower  =  expit(point.wise.DF.BS$RE.meta.lower)



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
  geom_text(data= data.frame( label = c("", "a)"),bilat_0   = point.wise.DF.RCS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)



p1=  ggplot(point.wise.DF.RCS,aes(x = age, y = RE.meta, linetype =treat, color = treat))+ 
  facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("Risk of fever") + 
  xlab("Children's age")+  theme_minimal()+  theme_bw()+ 
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
        legend.title =element_text(size=28, hjust = 0.5))


legend = gtable_filter(ggplotGrob(p1), "guide-box") 


point.wise.DF.BS.plot = point.wise.DF.BS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
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
  geom_text(data= data.frame( label = c("", "b)"),bilat_0   = point.wise.DF.BS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


point.wise.DF.PS.plot = point.wise.DF.PS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
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
  geom_text(data= data.frame( label = c("", "c)"),bilat_0   = point.wise.DF.PS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)



point.wise.DF.SS.plot = point.wise.DF.SS%>%
  ggplot(aes(x = age, y = RE.meta,linetype =treat, color = treat))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment", guide = FALSE)+ 
  scale_linetype_discrete(name ="Treatment")+ylab("") + 
  xlab("")+  theme_minimal()+  theme_bw()+ 
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
        legend.position = "none")+ 
  geom_text(data= data.frame( label = c("", "d)"),bilat_0   = point.wise.DF.SS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)
  
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 14.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(point.wise.DF.RCS.plot, point.wise.DF.BS.plot, point.wise.DF.PS.plot, point.wise.DF.SS.plot, ncol=2,
                         top = textGrob("", vjust = 1, gp = gpar(fontface = "bold", fontsize = 50)),
                         left = textGrob("Risk of fever after 3-7 days", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold", fontsize = 32)),
                         bottom = textGrob("Children's Age", vjust = -1, 
                                           gp = gpar(fontface = "bold", fontsize = 32))),
             legend,widths=unit.c(unit(1, "npc") - legend$width, legend$width))

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

predictions.BS=predictions.BS%>%
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


predictions.BS.diff=risk.diff.creator(dataframe = predictions.BS, 
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

absolute_diff_BS.Empirical=  predictions.BS.diff%>%
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

point.wise.absolute_diff_BS.Empirical =  pointwise.ma(data = absolute_diff_BS.Empirical,
                                                       clustering.variable = "study",
                                                       combining.variables = c("age","bilat_0"),
                                                       predicted.outcome = "fit.diff",
                                                       predicted.outcome.se = NULL,
                                                       predicted.outcome.CI = c("diff.lower","diff.upper")
)
point.wise.absolute_diff_BS.Empirical =  point.wise.absolute_diff_BS.Empirical%>%
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
        axis.text.y = element_text(face="bold",  size=18),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "a)"),bilat_0   = point.wise.absolute_diff_RCS.Empirical$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)
point.wise.absolute_diff_RCS.Empirical.plot

point.wise.absolute_diff_BS.Empirical.plot = point.wise.absolute_diff_BS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
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
        axis.text.y = element_text(face="bold",  size=18),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "b)"),bilat_0   = point.wise.absolute_diff_BS.Empirical$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


point.wise.absolute_diff_PS.Empirical.plot = point.wise.absolute_diff_PS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
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
        axis.text.y = element_text(face="bold",  size=18),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "c)"),bilat_0   = point.wise.absolute_diff_PS.Empirical$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)



point.wise.absolute_diff_SS.Empirical.plot = point.wise.absolute_diff_SS.Empirical%>%
  ggplot(aes(x = age, y = RE.meta))+ facet_wrap(~bilat_0) +geom_line()+
  geom_ribbon(aes(ymin = RE.meta.lower,ymax=RE.meta.upper),alpha=0.2) +
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
        axis.text.y = element_text(face="bold",  size=18),
        axis.text.x=element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "d)"),bilat_0   = point.wise.absolute_diff_SS.Empirical$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 15.png",width = 1240, height = 1680) 

grid.arrange(point.wise.absolute_diff_RCS.Empirical.plot, 
             point.wise.absolute_diff_BS.Empirical.plot, 
             point.wise.absolute_diff_PS.Empirical.plot,
             point.wise.absolute_diff_SS.Empirical.plot, ncol=2,
             top = textGrob("", vjust = 1, gp = gpar(fontface = "bold", fontsize = 50)),
             left = textGrob("Antibiotics effect", rot = 90, vjust = 1, 
                             gp = gpar(fontface = "bold", fontsize = 32)),
             bottom = textGrob("Children's Age", vjust = -1, 
                               gp = gpar(fontface = "bold", fontsize = 32)))

dev.off()

## ----Multi-variate meta-analysis on empirical example  ------------------------------------------------------------------------------------------------------

rm(list=ls()[! ls() %in% c("expit","IPDMA")]) ### To clear all
### Ommit Little study since they don't provide bilaterality information
miniIPD= IPDMA%>% filter((study != "Little" & study != "Appelman"))

### check ranges of age 


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

### First we get the knots from each data-set 3 knot in 10%, 50% and 90% quantiles
Knots.miniIPD =   list(age=quantile(miniIPD$age, probs = c(0.1,0.5,0.9))) 
miniIPD =  rbind(miniIPD, rep.upper)

## The formula for all regions is the same so we save it 

formula = poutcome ~  treat + bilat_0 + treat*bilat_0  + age + 
  s(age, by = bilat_0, bs="cr",fx=T,  k=3)+
  s(age, by = treat, bs="cr",fx=T, k=3)

# Number of studies in each region

nstudies.miniIPD = length(unique(miniIPD$study))



### Fit a stacked analysis for each data-set to get the model matrix
fit.miniIPD = gam( formula =formula,
                   knots = Knots.miniIPD,
                   family = binomial("logit"), 
                   data = miniIPD)

### Get the model matrices for each data-set

X.p.miniIPD =  model.matrix(fit.miniIPD)

### Create empty matrices for the estimated splines coefficients

estimated.coefficients.miniIPD = matrix(NA,
                                        ncol = length(fit.miniIPD$coefficients),nrow=nstudies.miniIPD,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.miniIPD)),1, paste, collapse=" "),
                                                         c(1:length(fit.miniIPD$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.miniIPD = matrix(NA, ncol=sum(c(1:length(fit.miniIPD$coefficients))), nrow = nstudies.miniIPD )




k=3
j=1

for( i in unique(miniIPD$study)){
  
  ### Get a mini df1 with only one study
  minidf1 = miniIPD%>%
    filter(study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,
            knots = Knots.miniIPD, 
            weights= weight,
            family = binomial("logit"), 
            data = minidf1)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.miniIPD[j,] = fit$coefficients
  S.miniIPD[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
  #rm(i,minidf1,fit)
}
rm(k,j)




#### Perform for each region a multivariate meta-analysis
mv.fit.RCS.miniIPD= mvmeta(estimated.coefficients.miniIPD, S.miniIPD,control = list(maxiter=1000))

new.dat= data.frame(age= rep(seq(0,9,length.out = 50),each = 20),
                    treat = rep(unique(miniIPD$treat),500), 
                    bilat_0 =  rep(rep(unique(miniIPD$bilat_0),each=2),250),
                    study =  rep(rep(unique(miniIPD$study),each =4),50), 
                    poutcome = rep(c(0,1),500)
)


### Fit a stacked analysis for each data-set to get the model matrix
fit.miniIPD = gam( formula =formula,
                   knots = Knots.miniIPD,
                   family = binomial("logit"), 
                   data = new.dat)

### Get the model matrices for each data-set

X.p.miniIPD =  model.matrix(fit.miniIPD)




prediction.interval.mvmeta.miniIPD=  X.p.miniIPD%*% coef(mv.fit.RCS.miniIPD)

prediction.interval.mvmeta.lower.miniIPD=  X.p.miniIPD%*% coef(mv.fit.RCS.miniIPD) - sqrt( rowSums(X.p.miniIPD * (X.p.miniIPD  %*% vcov(mv.fit.RCS.miniIPD)))) * 1.96

prediction.interval.mvmeta.upper.miniIPD =  X.p.miniIPD %*% coef(mv.fit.RCS.miniIPD) + sqrt( rowSums(X.p.miniIPD  * (X.p.miniIPD  %*% vcov(mv.fit.RCS.miniIPD)))) * 1.96


mvmeta.df.RCS = cbind(new.dat[,c("study","age","treat","bilat_0")],
                      fit =  expit(prediction.interval.mvmeta.miniIPD), 
                      Lower= expit(prediction.interval.mvmeta.lower.miniIPD),
                      Upper =expit(prediction.interval.mvmeta.upper.miniIPD ))



g.mvmeta.total.RCS = ggplot(mvmeta.df.RCS,aes(x = age, fit, linetype= treat, color= treat)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ facet_wrap(~bilat_0)+
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
  geom_text(data= data.frame( label = c("a)", ""),bilat_0   = mvmeta.df.RCS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


g.mvmeta.total.RCS
###########



########### B-splines    indicator
###########


### First we get the knots from each data-set 3 inner equidistant knots and 6 outer (3 on both sides of the boundaries) 


## The formula for all regions is the same so we save it 

formula = poutcome ~  treat+ bilat_0 + treat*bilat_0  + 
  age+ 
  s(age, by = bilat_0, bs="bs",fx=T,  k=4, m= c(2,0))+
  s(age, by = treat, bs="bs",fx=T, k=4, m= c(2,0))


# Number of studies in each region

nstudies.miniIPD = length(unique(miniIPD$study))

Knots.BS.MV  = list (age = c(-8.86008333, -4.39283333,0, 2.5 , 9, 13.47616667, 17.94341667))

### Fit a stacked analysis for each data-set to get the model matrix
fit.miniIPD = gam( formula =formula,
                   family = binomial("logit"), knots = Knots.BS.MV,
                   data = miniIPD)

### Get the model matrices for each data-set

X.p.miniIPD =  model.matrix(fit.miniIPD)

### Create empty matrices for the estimated splines coefficients

estimated.coefficients.miniIPD = matrix(NA,
                                        ncol = length(fit.miniIPD$coefficients),nrow=nstudies.miniIPD,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.miniIPD)),1, paste, collapse=" "),
                                                         c(1:length(fit.miniIPD$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.miniIPD = matrix(NA, ncol=sum(c(1:length(fit.miniIPD$coefficients))), nrow = nstudies.miniIPD )




k=3
j=1

for( i in unique(miniIPD$study)){
  
  ### Get a mini df1 with only one study
  minidf1 = miniIPD%>%
    filter(study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,weights = weight,knots = Knots.BS.MV,
            family = binomial("logit"), data = minidf1)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.miniIPD[j,] = fit$coefficients
  S.miniIPD[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
  #rm(i,minidf1,fit)
}
rm(k,j)



new.dat= data.frame(age= rep(seq(0,9,length.out = 50),each = 20),
                    treat = rep(unique(miniIPD$treat),500), 
                    bilat_0 =  rep(rep(unique(miniIPD$bilat_0),each=2),250),
                    study =  rep(rep(unique(miniIPD$study),each =4),50), 
                    poutcome = rep(c(0,1),500)
)


### Fit a stacked analysis for each data-set to get the model matrix
fit.miniIPD = gam( formula =formula,
                   knots = Knots.BS.MV,
                   family = binomial("logit"), 
                   data = new.dat)

### Get the model matrices for each data-set

X.p.miniIPD =  model.matrix(fit.miniIPD)


#### Perform for each region a multivariate meta-analysis
mv.fit.BS.miniIPD= mvmeta(estimated.coefficients.miniIPD, S.miniIPD,control = list(maxiter=1000))


prediction.interval.mvmeta.miniIPD=  X.p.miniIPD%*% coef(mv.fit.BS.miniIPD)

prediction.interval.mvmeta.lower.miniIPD=  X.p.miniIPD%*% coef(mv.fit.BS.miniIPD) - sqrt( rowSums(X.p.miniIPD * (X.p.miniIPD  %*% vcov(mv.fit.BS.miniIPD)))) * 1.96

prediction.interval.mvmeta.upper.miniIPD =  X.p.miniIPD %*% coef(mv.fit.BS.miniIPD) + sqrt( rowSums(X.p.miniIPD  * (X.p.miniIPD  %*% vcov(mv.fit.BS.miniIPD)))) * 1.96



mvmeta.df.BS = cbind(new.dat[,c("study","age","treat","bilat_0")],
                     fit =  expit(prediction.interval.mvmeta.miniIPD), 
                     Lower= expit(prediction.interval.mvmeta.lower.miniIPD),
                     Upper =expit(prediction.interval.mvmeta.upper.miniIPD ))



g.mvmeta.total.BS = ggplot(mvmeta.df.BS,aes(x = age, fit, linetype= treat, color= treat)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ facet_wrap(~bilat_0)+
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
  geom_text(data= data.frame( label = c("b)", ""),bilat_0   = mvmeta.df.RCS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)

g.mvmeta.total.BS

##------------- Quadratic MVmeta  -----------------------------------------------------

rm(list=ls()[! ls() %in% c("expit","IPDMA")]) ### To clear all
### Ommit Little study since they don't provide bilaterality information
miniIPD= IPDMA%>% filter((study != "Little"))

### check ranges of age 


miniIPD%>%
  group_by(study,bilat_0)%>%
  summarise(Min = min(age), Max= max(age))


## The formula for all regions is the same so we save it 

formula = poutcome ~  treat + bilat_0 + age+ I(age^2) +  treat*bilat_0  + treat*age + treat*I(age^2)+ age*bilat_0  + I(age^2)*bilat_0   

# Number of studies in each region

nstudies.miniIPD = length(unique(miniIPD$study))



### Fit a stacked analysis for each data-set to get the model matrix
fit.miniIPD = glm( formula =formula,
                   family = binomial("logit"), 
                   data = miniIPD)

### Get the model matrices for each data-set

X.p.miniIPD =  model.matrix(fit.miniIPD)

### Create empty matrices for the estimated splines coefficients

estimated.coefficients.miniIPD = matrix(NA,
                                        ncol = length(fit.miniIPD$coefficients),nrow=nstudies.miniIPD,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.miniIPD)),1, paste, collapse=" "),
                                                         c(1:length(fit.miniIPD$coefficients))))


### Create empty matrices for the variance-covariance matrix of the coefficients

S.miniIPD = matrix(NA, ncol=sum(c(1:length(fit.miniIPD$coefficients))), nrow = nstudies.miniIPD )

k=3
j=1

for( i in unique(miniIPD$study)){
  print(i)
  ### Get a mini df1 with only one study
  minidf1 = miniIPD%>%
    filter(study == i)
  
  # Fit the GAM
  fit = gam(formula = formula ,
            #weights= weight,
            family = binomial("logit"), 
            data = minidf1)
  
  
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.miniIPD[j,] = fit$coefficients
  S.miniIPD[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  #mat1 <- predict.gam(fit, type = "lpmatrix")
  
  k=k+2
  j=j+1
  #rm(i,minidf1,fit)
  
}
rm(k,j)





#### Perform for each region a multivariate meta-analysis
mv.fit.Quadratic.miniIPD= mvmeta(estimated.coefficients.miniIPD, S.miniIPD)

prediction.interval.mvmeta.miniIPD       =  X.p.miniIPD%*% coef(mv.fit.Quadratic.miniIPD)

prediction.interval.mvmeta.lower.miniIPD =  X.p.miniIPD%*% coef(mv.fit.Quadratic.miniIPD) - sqrt( rowSums(X.p.miniIPD * (X.p.miniIPD  %*% vcov(mv.fit.Quadratic.miniIPD)))) * 1.96

prediction.interval.mvmeta.upper.miniIPD =  X.p.miniIPD %*% coef(mv.fit.Quadratic.miniIPD) + sqrt( rowSums(X.p.miniIPD  * (X.p.miniIPD  %*% vcov(mv.fit.Quadratic.miniIPD)))) * 1.96


mvmeta.df.Quadratic = cbind(miniIPD[,c("study","age","treat","bilat_0")],
                            fit =  expit(prediction.interval.mvmeta.miniIPD), 
                            Lower= expit(prediction.interval.mvmeta.lower.miniIPD),
                            Upper =expit(prediction.interval.mvmeta.upper.miniIPD ))



g.mvmeta.total.Quadratic = ggplot(mvmeta.df.Quadratic,aes(x = age, fit, linetype= treat, color= treat)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ facet_wrap(~bilat_0)+
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
  geom_text(data= data.frame( label = c("a)", ""),bilat_0   = mvmeta.df.Quadratic$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 16.png",width = 1240, height = 1680) 

g.mvmeta.total.Quadratic

dev.off()



## ----Absolute risk differences ---------------------------------------------------------------------------------------------------------
### MVmeta
source("Assisting functions/Create risk differences.R")


#### Create a nice data-set to make predictions and plots 
new.dat= data.frame(age= rep(seq(0,9,length.out = 50),each = 20),
                    treat = rep(unique(miniIPD$treat),500), 
                    bilat_0 =  rep(rep(unique(miniIPD$bilat_0),each=2),250),
                    study =  rep(rep(unique(miniIPD$study),each =4),50),
                    poutcome =  rep(0:1, 500)
)


### Fit a stacked analysis for each data-set to get the model matrix
fit.miniIPD.new.dat = glm( formula =formula,
                           family = binomial("logit"), 
                           data = new.dat)

### Get the model matrices for each data-set

X.p.miniIPD.new.dat =  model.matrix(fit.miniIPD.new.dat)



#### Perform for each region a multivariate meta-analysis
mv.fit.Quadratic.miniIPD = mvmeta(estimated.coefficients.miniIPD, S.miniIPD)

prediction.interval.mvmeta.miniIPD.new.dat       =  X.p.miniIPD.new.dat%*% coef(mv.fit.Quadratic.miniIPD)

prediction.interval.mvmeta.lower.miniIPD.new.dat =  X.p.miniIPD.new.dat%*% coef(mv.fit.Quadratic.miniIPD) - sqrt( rowSums(X.p.miniIPD.new.dat * (X.p.miniIPD.new.dat  %*% vcov(mv.fit.Quadratic.miniIPD)))) * 1.96

prediction.interval.mvmeta.upper.miniIPD.new.dat =  X.p.miniIPD.new.dat %*% coef(mv.fit.Quadratic.miniIPD) + sqrt( rowSums(X.p.miniIPD.new.dat  * (X.p.miniIPD.new.dat  %*% vcov(mv.fit.Quadratic.miniIPD)))) * 1.96


mvmeta.df.Quadratic.new.dat = cbind(new.dat[,c("study","age","treat","bilat_0")],
                            fit =  expit(prediction.interval.mvmeta.miniIPD.new.dat), 
                            Lower= expit(prediction.interval.mvmeta.lower.miniIPD.new.dat),
                            Upper =expit(prediction.interval.mvmeta.upper.miniIPD.new.dat ))



MV.meta_absolute_difference.Quadratic=  risk.diff.creator(dataframe = mvmeta.df.Quadratic.new.dat,
                                                        treatment = "treat",
                                                        matching.variables=  c("study","age","bilat_0"),
                                                        outcome= NULL, 
                                                        predicted.outcome = "fit", 
                                                        predicted.CI = c("Lower","Upper"))







MV.meta_absolute_difference.Quadratic.plot = ggplot(MV.meta_absolute_difference.Quadratic,aes(x = age, fit.diff)) +
  geom_line(size=2)+ facet_wrap(.~bilat_0)+
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
        legend.position = "none") 


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 17.png",width = 1240, height = 1680) 
MV.meta_absolute_difference.Quadratic.plot
dev.off()


## ----GAMM meta-analysis on empirical example  ------------------------------------------------------------------------------------------------------



### Ommit Little Study since they don't provide bilaterality information
miniIPD= IPDMA%>% filter((study != "Little"))

# make the bilat variable a factor for graphs. 
miniIPD$bilat_0 =  factor(miniIPD$bilat_0 , labels = c("Unilateral","Bilateral") )
Knots.RCS = list (age = quantile(IPDMA$age , probs = c(0.1,0.5,0.9)))
Knots.BS  = list (age = c(-12.65556,-6.32778,0, 6.32778 , 12.65556, 18.98334, 25.31112))


fit.RCS <- gam(formula = poutcome ~  treat * bilat_0  + 
                 treat *age+ 
                 s(age, by = bilat_0, bs="cr", k=3, fx=T )+
                 s(age, by = treat, bs="cr", k=3, fx=T ) + 
                 s(age,by =  as.numeric(study),  bs="re")+
                 s(treat,by = bilat_0, bs= "fs")+
                 s(treat,by =  as.numeric(study),  bs="re")+
                 s(bilat_0,by =  as.numeric(study),  bs="re"), 
               family = binomial("logit"),knots = Knots.RCS, data = miniIPD, 
               method="REML")


fit.BS <- gam(formula = poutcome ~  treat * bilat_0  + 
                treat *age+ 
                s(age, by = bilat_0, bs="bs", k=4, m= c(2,0), fx=T)+
                s(age, by = treat, bs="bs",k=4, m= c(2,0), fx=T) + 
                s(age,by =  as.numeric(study),  bs="re")+
                s(treat,by = bilat_0, bs= "fs")+
                s(treat,by =  as.numeric(study),  bs="re")+
                s(bilat_0,by =  as.numeric(study),  bs="re"), 
              family = binomial("logit"), data = miniIPD,
              method="REML")

fit.PS <- gam(formula = poutcome ~ treat * bilat_0  + 
                treat *age+ 
                s(age, by = bilat_0, bs="ps", k=17)+
                s(age, by = treat, bs="ps", k=17) + 
                s(treat,by = bilat_0, bs= "fs")+
                s(age,by =  as.numeric(study),  bs="re")+
                s(treat,by =  as.numeric(study),  bs="re")+
                s(bilat_0,by =  as.numeric(study),  bs="re"), 
              family = binomial("logit"), data = miniIPD, 
              method="REML")


fit.SS <-  gam(formula = poutcome ~  treat * bilat_0  + 
                 treat *age+ 
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



preds.BS=  predict.gam(fit.BS, se.fit = T,
                       newdata = new.dat,newdata.guaranteed = T, 
                       exclude = exclude.parameters)%>%
  as.data.frame()%>%cbind(new.dat)

preds.BS$lower =  preds.BS$fit - 1.96*preds.BS$se.fit;preds.BS$lower = expit(preds.BS$lower)
preds.BS$upper =  preds.BS$fit + 1.96*preds.BS$se.fit;preds.BS$upper = expit(preds.BS$upper)
preds.BS$fit   =  expit(preds.BS$fit)


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
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "a)"),bilat_0   = preds.RCS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


g.GAMM.BS = ggplot(preds.BS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.BS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "b)"),bilat_0   = preds.BS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


g.GAMM.PS =ggplot(preds.PS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.PS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "c)"),bilat_0   = preds.PS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


g.GAMM.SS = ggplot(preds.SS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.SS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "d)"),bilat_0   = preds.SS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)

p1= ggplot(preds.RCS, aes(age,fit, color= treat))+ facet_wrap(~bilat_0)+ geom_line()+
  geom_ribbon(data = preds.RCS, aes(ymin = lower,ymax=upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("Risk of developing fever afte 1 week") + 
  xlab("Children's age")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "bottom") + 
  geom_text(data= data.frame( label = c("", "a)"),bilat_0   = preds.RCS$bilat_0[c(1,3)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, hjust   = -0.1,vjust   = -1, size = 10)


g.legend =  get_legend(p1)

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 18.png",width = 1240, height = 1680) 
g.GAMM=grid.arrange(arrangeGrob(
  g.GAMM.RCS,
  g.GAMM.BS,
  g.GAMM.PS,
  g.GAMM.SS, ncol=2),
  g.legend ,ncol=1,heights = c(10,1),
  left = textGrob(label = "Risk of developing fever after 1 week",
                  rot = 90,vjust = 0.5,gp = gpar(fontsize=32)),
  bottom = textGrob(label = "Children's age", 
                    vjust = 0,gp = gpar(fontsize=32)))

dev.off()


source("Assisting functions/Create risk differences.R")



predictions.diff.RCS  = risk.diff.creator(dataframe = preds.RCS,treatment = "treat",
                                          matching.variables = c("age","bilat_0"),
                                          outcome= NULL, 
                                          predicted.outcome = "fit", 
                                          predicted.CI = c("lower","upper")
)

predictions.diff.BS  = risk.diff.creator(dataframe = preds.BS,treatment = "treat",
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
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "a)"),
                              bilat_0   = predictions.diff.RCS$bilat_0[c(1,5)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, size = 10)


g.GAMM.BS.diff = ggplot(predictions.diff.BS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.BS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "b)"),
                              bilat_0   = predictions.diff.RCS$bilat_0[c(1,5)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, size = 10)



g.GAMM.PS.diff = ggplot(predictions.diff.PS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.PS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "c)"),
                              bilat_0   = predictions.diff.RCS$bilat_0[c(1,5)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, size = 10)



g.GAMM.SS.diff = ggplot(predictions.diff.SS, aes(age,fit.diff))+ facet_wrap(~bilat_0)+ geom_line()+ geom_hline(yintercept = 0,linetype=2)+
  geom_ribbon(data = predictions.diff.SS, aes(ymin = diff.lower,ymax=diff.upper),alpha=0.2) +
  scale_color_jama(name= "Treatment")+ 
  scale_linetype_discrete(name ="Treatment")+
  ylab("") + 
  xlab("")+ theme_bw()+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),         panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_text(size=28, hjust = 0.5),
        legend.position = "none") + 
  geom_text(data= data.frame( label = c("", "d)"),
                              bilat_0   = predictions.diff.RCS$bilat_0[c(1,5)]),
            mapping = aes(x = 5, y = 0.4, label = label), 
            inherit.aes = F, size = 10)


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 19.png",width = 1240, height = 1680)
g.GAMM.diff =grid.arrange(
  g.GAMM.RCS.diff,
  g.GAMM.BS.diff,
  g.GAMM.PS.diff,
  g.GAMM.SS.diff, ncol=2,
  left = textGrob(label = "Antibiotics effect",
                  rot = 90,vjust = 0.5,gp = gpar(fontsize=32)),
  bottom = textGrob(label = "Children's age", 
                    vjust = 0,gp = gpar(fontsize=32)))

dev.off()


## ----Figure 20: Appendix "----

source("Code for Figures, Tables, Analysis and data-simulation/Appendix/Appendix.R")



