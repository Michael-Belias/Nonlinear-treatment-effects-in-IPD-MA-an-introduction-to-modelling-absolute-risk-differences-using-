simulated_plot1= df1%>%
  ggplot( aes(x = BMI, `Mortality risk`, fill=Study, linetype= Treatment, color= Treatment)) + 
  geom_line(size=1.5) + facet_wrap(.~Study, dir="v")+ ylab("Mortality risk") + 
  xlab(expression(paste("BMI ", (Kg/m^2)))) + 
  scale_color_jama()+
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


simulated_plot1
