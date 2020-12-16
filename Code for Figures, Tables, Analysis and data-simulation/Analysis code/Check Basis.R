
### Check the basis functions 

fit.RCS.model_matrix =  predict(fit.RCS, type = "lpmatrix")


colnames(fit.RCS.model_matrix) = c("Intercept", "BMI", "Treatment", "B_1C", "B_2C", "B_3C", "B_4C", "BMI:Treatment", 
                                   "B_1T", "B_2T", "B_3T", "B_4T")



auto.zoo=   zoo(fit.RCS.model_matrix[,-c(1,2,4,8,9)])
fit.RCS.plot.C = autoplot(auto.zoo[auto.zoo$Treatment==0,], facets = NULL)+
  geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("RCS Harrel basis functions (Control)")+ 
  theme(plot.title = element_text(hjust = 0.5))



fit.RCS.plot.T = autoplot(auto.zoo[auto.zoo$Treatment==1,], facets = NULL)+
  geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("RCS Harrel basis functions (Treated)")+ 
  theme(plot.title = element_text(hjust = 0.5))

fit.RCS.plot =  grid.arrange(fit.RCS.plot.C,fit.RCS.plot.T)


### Check the basis functions 

fit.BS.model_matrix =  predict(fit.BS, type = "lpmatrix")


colnames(fit.BS.model_matrix) = c("Intercept", "BMI", "Treatment", "B_1C", "B_2C", "B_3C", "B_4C", "BMI:Treatment", 
                                  "B_1T", "B_2T", "B_3T", "B_4T")

auto.zoo=   zoo(fit.BS.model_matrix[,-c(1,2,8)])
fit.BS.plot.C = autoplot(auto.zoo[auto.zoo$Treatment==0,], facets = NULL)+
  geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("BS basis functions (Control)")+ 
  theme(plot.title = element_text(hjust = 0.5))



fit.BS.plot.T = autoplot(auto.zoo[auto.zoo$Treatment==1,], facets = NULL)+
  geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("BS basis functions (Treated)")+ 
  theme(plot.title = element_text(hjust = 0.5))

fit.BS.plot =  grid.arrange(fit.BS.plot.C,fit.BS.plot.T)





