library(splines)
library(rms)
library(zoo)
library(mgcv)

x= seq(0,1,length.out = 100)

knots = seq( .05 , .95 , length =5)

degree= 3

my_ns =  function(x, knots, degree){
  
  
positive_part =  function(z){
    
    if (z>0){
      return(z)
    }else{
      return(0)
    }
}
  
  
  k = length(knots)
  
  x.model.matrix =  as.data.frame(matrix(NA, ncol = k, nrow = length(x)))
  
  colnames(x.model.matrix)= paste("B_", 0:(k-1), sep="")
  
  
  x.model.matrix$B_0 =  rep(1, length(x))
  x.model.matrix$B_1= x
  
  for( w in 1: (length(knots)-2)){
    
    first_part  = (sapply((x-knots[w]), positive_part))^degree  
    second_part = ((knots[k] - knots[w])/(knots[k] - knots[k-1])) *(sapply(x - knots[k-1], positive_part))^degree
    third_part =  ((knots[k-1]-knots[w])/(knots[k]-knots[k-1]))   *(sapply(x - knots[k], positive_part))^degree
  

    x.model.matrix[,w+2] = (first_part - second_part + third_part)/(knots[k] - knots[1])^2

    }
   return(x.model.matrix)
 
}


model_matrix  = my_ns(x, knots = knots, degree = 3)


my.gplot = autoplot(zoo(model_matrix[,-1]), facets = NULL)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("My function basis functions")+ 
  theme(plot.title = element_text(hjust = 0.5))

Harrel.RCS =  rcspline.eval(x,knots = knots , inclx = T)
colnames(Harrel.RCS) =  colnames(model_matrix[,-1])
Harrel.gplot = autoplot(zoo(Harrel.RCS), facets = NULL)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("Harrel's basis functions")+ 
  theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(my.gplot, Harrel.gplot,
                        top =  "Mine vs Harrel's restricted cubic (both with 5 knots)")


NS = ns(x, intercept = T,  knots = knots)


NS.plot = autoplot(zoo(NS), facets = NULL)+ ggtitle("ns() function basis functions")+
  scale_color_discrete(name = "Basis functions") + theme(plot.title = element_text(hjust = 0.5))


BS= bs(x, intercept = T,knots = knots,degree = 3)



BS.plot =autoplot(zoo(BS), facets = NULL)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("bs() function basis functions")+ theme(plot.title = element_text(hjust = 0.5))


gridExtra::grid.arrange(NS.plot, BS.plot,
                        top =  "Natural cubic vs cubic B-splines (both with 5 knots)")


y= rep(5:6,each=50)

GAM.RCS <- gam(y~x+ s(x,bs="cr",fx = T, k = 10))

GAM.RCS.model_matrix =  predict(GAM.RCS, type = "lpmatrix")
GAM.RCS.plot = autoplot(zoo(GAM.RCS.model_matrix[,-c(1,2)]), facets = NULL)+geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("gam function basis functions for Restricted cubic splines")+ 
  theme(plot.title = element_text(hjust = 0.5))

GAM.RCS.plot



gridExtra::grid.arrange(NS.plot, BS.plot,GAM.RCS.plot,Harrel.gplot,
                        top =  "Natural cubic vs cubic B-splines (both with 5 knots)")


GAM.BS <- gam(y~x+ s(x,bs="bs",fx = T, k = 7, m = c(3,1)))

GAM.BS.model_matrix =  predict(GAM.BS, type = "lpmatrix")
GAM.BS.plot = autoplot(zoo(GAM.BS.model_matrix[,-c(1,2)]), facets = NULL)+geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions ") + 
  ggtitle("gam function basis functions for B-splines")+ 
  theme(plot.title = element_text(hjust = 0.5))

GAM.BS.plot


BS= bs(x, intercept = T,knots = GAM.BS$smooth[[1]]$knots,degree = 3)



BS.plot =autoplot(zoo(BS), facets = NULL)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("bs() function basis functions")+ theme(plot.title = element_text(hjust = 0.5))


BS.plot


PS= survival::pspline(x = x, intercept = T,df = 10, degree = 3)

PS.plot =autoplot(zoo(PS), facets = NULL)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("PS() function basis functions")+ theme(plot.title = element_text(hjust = 0.5))


PS.plot


GAM.PS <- gam(y~x+ s(x,bs="bs",fx = T, k = 7, m = c(3,0)))

GAM.PS.model_matrix =  predict(GAM.PS, type = "lpmatrix")
GAM.PS.plot = autoplot(zoo(GAM.PS.model_matrix[,-c(1,2)]), facets = NULL)+geom_hline(yintercept = 0)+
  scale_color_discrete(name = "Basis functions") + 
  ggtitle("gam function basis functions")+ 
  theme(plot.title = element_text(hjust = 0.5))

GAM.PS.plot





gridExtra::grid.arrange(NS.plot, GAM.RCS.plot,Harrel.gplot,
                        top =  "Natural cubic splines (all with 5 knots)")

