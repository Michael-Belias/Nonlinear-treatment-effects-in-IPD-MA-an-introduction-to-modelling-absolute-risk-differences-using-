## ----Assisting tables for Figure 3----
tab3 =  df3%>%
  group_by(Study)%>%
  summarise("Minimum BMI" =round(min(BMI),2), `Maximum BMI`= round(max(BMI),2))







simulated_plot3= df3 %>%
  ggplot(aes(x = BMI, `Mortality risk`, linetype= Treatment, color= Treatment)) + 
  geom_line(size=1.5) + ylab("Mortality risk") +xlab(expression(paste("BMI ", (Kg/m^2))))  +
  scale_color_jama()+
  facet_wrap(fct_relevel(Study, "total", after = Inf)~., ncol = 2, dir="v")+ 
  geom_vline(data = tab3, aes(xintercept=`Minimum BMI`),linetype =2)+ 
  geom_vline(data = tab3, aes(xintercept=`Maximum BMI`),linetype =2) + newtheme


gp <- ggplotGrob(simulated_plot3)


mytheme <- gridExtra::ttheme_minimal(
  core = list(fg_params=list(cex = 2.0)),
  colhead = list(fg_params=list(cex = 2.0)),
  rowhead = list(fg_params=list(cex = 2.0)))



# get coordinates of empty panels to be blanked out
empty.area <- gtable_filter(gp, "panel", trim = F)
empty.area <- empty.area$layout[sapply(empty.area$grob,
                                       function(x){class(x)[[1]]=="zeroGrob"}),]

empty.area$t <- empty.area$t - 1 #extend up by 1 cell to cover facet header
empty.area$b <- empty.area$b + 1 #extend down by 1 cell to cover x-axis

simulated_plot3 <- gtable_add_grob(x = gp,
                       grobs = tableGrob(tab3,
                                         rows = NULL,
                                         theme = mytheme),
                       t = min(empty.area$t),
                       l = min(empty.area$l),
                       b = max(empty.area$b),
                       r = max(empty.area$r),
                       name = "textbox")
grid::grid.draw(simulated_plot3)

