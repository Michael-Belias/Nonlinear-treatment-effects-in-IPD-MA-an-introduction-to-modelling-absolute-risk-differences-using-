simulated_plot_single_df= ggplot(single.df, aes(x = BMI, `Mortality risk`,linetype= Treatment, color= Treatment)) + 
  geom_line(size=1.5) + ylab("Mortality risk") + xlab(expression(paste("BMI ", (kg/m^2))))+
  scale_color_jama()+ ylim(c(0,1))+ newtheme

simulated_plot_single_df
