simulated_plot1= df1%>%
  ggplot( aes(x = BMI, `Mortality risk`, fill=Study, linetype= Treatment, color= Treatment)) + 
  geom_line(size=1.5) + facet_wrap(.~Study, dir="h")+ ylab("Mortality risk") + 
  xlab(expression(paste("BMI ", (kg/m^2)))) + 
  scale_color_jama()+ newtheme


simulated_plot1
