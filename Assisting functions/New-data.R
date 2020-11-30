### Run this scrip to make a nice data-set to predict the outcomes of fever


create.new.data= function(data, step.size=0.01, continuous.variable ="age", clustering.variable = "study"){
  
 
min.max = data%>%
    group_by(!!as.name(clustering.variable))%>%
    select(!!as.name(continuous.variable))%>%
    na.omit() %>%
    summarise(Minimum =  floor(min(age)), Maximum =  ceiling(max(age)))
  
age =  vector()
study = vector()

for(i in min.max$study){
    
  
    mindata= min.max[min.max$study == i,]
    age.to.add = rep(seq( from  = mindata$Minimum,to =  mindata$Maximum, by = step.size),each =4)
    age= c(age,age.to.add)
    l= length(rep(seq( from  = mindata$Minimum,to =  mindata$Maximum, by = step.size),4))
    study = c(study,rep(i,l ))

}
  
  
  new.dat = data.frame(study   = study,
                       treat   = rep(x = unique(data$treat),length(age)/2),
                       bilat_0 = as.factor(rep(rep(x = unique(data$bilat_0),each =2)  ,length(age)/4)),
                       age = age)
  
  new.dat =  new.dat[order(new.dat$age),]
  
  
  
  rm(age, study,i,l)
  
  return(new.dat)
  
  
}
