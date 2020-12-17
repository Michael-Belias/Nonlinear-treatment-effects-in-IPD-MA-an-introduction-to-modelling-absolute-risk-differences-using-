
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

newtheme <- theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 48,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 34,face = "italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=32),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=24, hjust = 0.5),
        axis.text.y = element_text(face="bold",  size=32),
        axis.title.y = element_text(size = 56),
        axis.title.x = element_text(size = 56),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.position = "bottom",
        legend.title=element_blank())



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
source("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 2.R");simulated_plot1


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
rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df","newtheme")])


source("Code for Figures, Tables, Analysis and data-simulation/Analysis code/Single study analysis and figures.R")
## Figure 6 
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 6.png",width = 1240, height = 1680) 
grid.arrange(arrangeGrob(gRCS,gBS,gPS,gSS,
                         bottom= textGrob(label = expression(BMI (kg/m^2)), hjust = 0,gp = gpar(fontsize=48)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=48))), 
             legend, heights= c(10,1))
dev.off()



## Figure 7
png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 7.png",width = 1240, height = 1680) 
grid.arrange(gRCS_absolute_diff,gBS_absolute_diff,gPS_absolute_diff,gSS_absolute_diff,
             bottom= textGrob(label = expression(BMI (kg/m^2)), hjust = 0.35,gp = gpar(fontsize=48)), 
             left = textGrob(label = "Treatment effect plot (absolute risk differences)", rot = 90, vjust = 1,gp = gpar(fontsize=48)))
dev.off()

## ----Pointwise meta-analysis---------------------------------------------------------------------------------
## Pointwise meta-analysis for the three scenarios

source("Pointwise meta-analysis/Pointwise meta-analysis (1st scenario).R")

source("Pointwise meta-analysis/Pointwise meta-analysis (2nd scenario).R")

source("Pointwise meta-analysis/Pointwise meta-analysis (3rd scenario).R")


#### Figure 8

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 8.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.plot, 
                         point.wise.DF.NS.HT.plot, 
                         point.wise.DF.PS.HT.plot, 
                         point.wise.DF.SS.HT.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = -1,gp = gpar(fontsize=48))),
             arrangeGrob(point.wise.DF.RCS.DR.plot, 
                         point.wise.DF.NS.DR.plot, 
                         point.wise.DF.PS.DR.plot, 
                         point.wise.DF.SS.DR.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = -1,gp = gpar(fontsize=48))),
             arrangeGrob(point.wise.DF.RCS.Comb.plot, 
                         point.wise.DF.NS.Comb.plot, 
                         point.wise.DF.PS.Comb.plot, 
                         point.wise.DF.SS.Comb.plot, ncol=4, 
                         right= textGrob(label = "", 
                                         vjust = 0,gp = gpar(fontsize=48)),
             bottom = textGrob(label = expression(paste("BMI ", (kg/m^2))), 
                               rot = 0, vjust = 0,hjust= 0.25,
                             gp = gpar(fontsize=48))),
             legend,ncol = 1, heights = c(4,4,5,1),
             left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,
                             gp = gpar(fontsize=48)))

dev.off()



#### Figure 9

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 9.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(point.wise.DF.RCS.HT.diff.plot, 
                         point.wise.DF.NS.HT.diff.plot, 
                         point.wise.DF.PS.HT.diff.plot, 
                         point.wise.DF.SS.HT.diff.plot, ncol=4),
             arrangeGrob(point.wise.DF.RCS.DR.diff.plot, 
                         point.wise.DF.NS.DR.diff.plot, 
                         point.wise.DF.PS.DR.diff.plot, 
                         point.wise.DF.SS.DR.diff.plot, ncol=4),
             arrangeGrob(point.wise.DF.RCS.Comb.diff.plot, 
                         point.wise.DF.NS.Comb.diff.plot, 
                         point.wise.DF.PS.Comb.diff.plot, 
                         point.wise.DF.SS.Comb.diff.plot, ncol=4),
             bottom = textGrob(label = expression(paste("BMI ", (kg/m^2))), 
                               rot = 0, vjust = 0,hjust = 0.25,
                             gp = gpar(fontsize=48)),
             left = textGrob(label = "Treatment effect (Absolute risk difference)",
                             rot = 90, vjust = 1,
                             gp = gpar(fontsize=48)))

dev.off()

## ----          MVmeta                  ------------------------------------------------------------------------------------------------------


source("Multivariate meta-analysis/Multivariate meta-analysis (1st scenario).R")

source("Multivariate meta-analysis/Multivariate meta-analysis (2nd scenario).R")

source("Multivariate meta-analysis/Multivariate meta-analysis (3rd scenario).R")


###### Figure 10


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 10.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(g.mvmeta.total.RCS, 
                         g.mvmeta.total.NS, 
                         g.mvmeta.total.RCS.DR, 
                         g.mvmeta.total.NS.DR,
                         g.mvmeta.total.RCS.Comb, 
                         g.mvmeta.total.NS.Comb,
                         bottom = textGrob(label = expression(paste("BMI ", (kg/m^2))), 
                                           rot = 0, vjust = 0,hjust = 0.25,
                                           gp = gpar(fontsize=48)),
                         left = textGrob(label = "Mortality risk",
                                         rot = 90, vjust = 1,
                                         gp = gpar(fontsize=48))),
             legend, heights= c(10,1))

dev.off()


###### Figure 11

png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 11.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(MV.meta_absolute_difference.RCS.HT.plot,MV.meta_absolute_difference.NS.HT.plot,
                         MV.meta_absolute_difference.RCS.DR.plot, MV.meta_absolute_difference.NS.DR.plot, 
                         MV.meta_absolute_difference.RCS.Comb.plot, MV.meta_absolute_difference.NS.Comb.plot,
                         bottom = textGrob(label = expression(paste("BMI ", (kg/m^2))), 
                                           rot = 0, vjust = 0,hjust = 0.25,
                                           gp = gpar(fontsize=48)),
                         left = textGrob(label = "Treatment effect (Absolute risk difference)",
                                         rot = 90, vjust = 1,
                                         gp = gpar(fontsize=48))))
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
                         bottom= textGrob(label = expression(paste("BMI ", (kg/m^2))), vjust=0.35,
                                          hjust = 0.35,gp = gpar(fontsize=48)), 
                         left = textGrob(label = "Mortality risk", rot = 90, vjust = 1,gp = gpar(fontsize=48))), heights =c(12,1), g.legend)


dev.off()


# Figure 13


png("Code for Figures, Tables, Analysis and data-simulation/Figures and Tables/Figure 13.png",width = 1240, height = 1680) 

grid.arrange(arrangeGrob(GAMM.DF.RCS.HT.diff.plot, 
                         GAMM.DF.NS.HT.diff.plot, 
                         GAMM.DF.PS.HT.diff.plot, 
                         GAMM.DF.SS.HT.diff.plot, ncol=4),
             arrangeGrob(GAMM.DF.RCS.DR.diff.plot, 
                         GAMM.DF.NS.DR.diff.plot, 
                         GAMM.DF.PS.DR.diff.plot, 
                         GAMM.DF.SS.DR.diff.plot, ncol=4),
             arrangeGrob(GAMM.DF.RCS.Comb.diff.plot, 
                         GAMM.DF.NS.Comb.diff.plot, 
                         GAMM.DF.PS.Comb.diff.plot, 
                         GAMM.DF.SS.Comb.diff.plot, ncol=4),
             bottom= textGrob(label = expression(paste("BMI ", (kg/m^2))), 
                              hjust = 0,gp = gpar(fontsize=48)), 
             left = textGrob(label = "Treatment effect (Absolute risk difference)", rot = 90, vjust = 1,gp = gpar(fontsize=48)))

dev.off()


## ----Empirical example------------------------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Analysis code/Empirical example.R")

## ----Figure 20: Appendix "----

source("Code for Figures, Tables, Analysis and data-simulation/Appendix/Appendix.R")



