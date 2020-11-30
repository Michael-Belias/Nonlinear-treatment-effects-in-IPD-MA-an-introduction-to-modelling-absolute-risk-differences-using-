## ----Assisting tables for Figure 3----
tab2 = df2%>%
  group_by(Study)%>%
  summarise("Minimum BMI" =round(min(BMI),2), `Maximum BMI`= round(max(BMI),2))



## ----Figure 3: Association between mortality risk and BMI per study in the non-heterogeneous IPD-set with different BMI ranges."----
simulated_plot2= df2 %>%
  ggplot(aes(x = BMI, `Mortality risk`, linetype= Treatment, color= Treatment)) + 
  geom_line(size=1.5) + ylab("Mortality risk") + 
  xlab(expression(paste("BMI ", (Kg/m^2))))  + scale_color_jama()+
  facet_wrap(fct_relevel(Study, "total", after = Inf)~., ncol = 2, dir="v")+ 
  geom_vline(data = tab2, aes(xintercept=`Minimum BMI`),linetype =2)+ 
  geom_vline(data = tab2, aes(xintercept=`Maximum BMI`),linetype =2) + 
  theme_bw()+ 
  theme(plot.title    = element_text(hjust = 0.5,size = 48,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 34,face = "italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=12),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=24, hjust = 0.5),
        axis.title.y = element_text(size = 48),
        axis.title.x = element_text(size = 48),
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=30, hjust = 0),
        legend.title=element_blank(),
        legend.position = "bottom")

gp <- ggplotGrob(simulated_plot2)



# get coordinates of empty panels to be blanked out
empty.area <- gtable_filter(gp, "panel", trim = F)
empty.area <- empty.area$layout[sapply(empty.area$grob,
                                       function(x){class(x)[[1]]=="zeroGrob"}),]

empty.area$t <- empty.area$t - 1 #extend up by 1 cell to cover facet header
empty.area$b <- empty.area$b + 1 #extend down by 1 cell to cover x-axis

simulated_plot2 <- gtable_add_grob(x = gp,
                       grobs = tableGrob(tab2,
                                         rows = NULL,
                                         theme = ttheme_minimal()),
                       t = min(empty.area$t),
                       l = min(empty.area$l),
                       b = max(empty.area$b),
                       r = max(empty.area$r),
                       name = "textbox")
grid::grid.draw(simulated_plot2)




