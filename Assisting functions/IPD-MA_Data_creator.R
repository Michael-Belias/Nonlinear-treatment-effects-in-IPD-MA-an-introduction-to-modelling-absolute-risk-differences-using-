### IPD-MA data-creator


library(dplyr)



data.creator = function(name.of.Clustering,
                        name.of.Treatment ,
                        number.of.studies, 
                        values.of.clustering.variable,
                        step.size,
                        name.of.Continuous ,
                        Treatment,
                        ranges.of.Continuous 
                        ){
  
  
  min.max = as.data.frame(matrix(ranges.of.Continuous, ncol = 2, 
                                 nrow = number.of.studies,
                                 byrow = T, 
                                 dimnames = list(c(1:5),c("Min","Max"))))
  
  min.max = cbind(min.max,values.of.clustering.variable)
  
  colnames(min.max) =  c("Min","Max",name.of.Clustering)
  
  
  Cont = vector()
  study = vector()
  
  for(i in min.max[,name.of.Clustering]){
    
    
    mindata= min.max%>%
      filter(!!as.name(name.of.Clustering) == i)
    
    Cont.to.add = rep(seq( from  = mindata$Min,to =  mindata$Max,by =  step.size),each =2)
    Cont= c(Cont,Cont.to.add)
    l= length(rep(seq( from  = mindata$Min,to =  mindata$Max, by =  step.size),2))
    study = c(study,rep(i,l ))
    
  }
  
  new.dat = data.frame(study,Treatment = rep(x = Treatment,length(Cont)/2),Cont)
  new.dat =  new.dat[order(new.dat$Cont),]
  colnames(new.dat) =  c(name.of.Clustering, name.of.Treatment,name.of.Continuous)
  
  return(new.dat)
  
  
}





