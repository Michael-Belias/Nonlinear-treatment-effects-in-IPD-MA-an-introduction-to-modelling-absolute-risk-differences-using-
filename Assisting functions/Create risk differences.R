###### Create from a data-set the risk difference


risk.diff.creator =  function(dataframe = NULL, treatment = "treat", 
                              matching.variables=  c("age","bilat_0"),
                              outcome= NULL, 
                              predicted.outcome = "RE.meta", 
                              predicted.CI = c("RE.meta.lower","RE.meta.upper")){
  
  
 
  
dataframe = dataframe[,c(outcome,treatment,matching.variables, predicted.outcome, predicted.CI)]
  
  
  
split= dataframe%>%
    group_by(!!as.name(treatment) )%>%
    group_split()
  
  
split.df= full_join(x = split[[1]],y = split[[2]], by= matching.variables  )
  
  
  
  p1 =  as.matrix(split.df[, paste(predicted.outcome,"y",sep = ".")])
  p2 =  as.matrix(split.df[, paste(predicted.outcome,"x",sep = ".")])
  l1 =  as.matrix(split.df[, paste(predicted.CI[1],"y",sep = ".")])
  l2 =  as.matrix(split.df[, paste(predicted.CI[1],"x",sep = ".")])
  u1 =  as.matrix(split.df[, paste(predicted.CI[2],"y",sep = ".")])
  u2 =  as.matrix(split.df[, paste(predicted.CI[2],"x",sep = ".")])
  
  split.df$fit.diff = p1 - p2 
  
  split.df$diff.lower =  split.df$fit.diff - sqrt((p1-l1)^2 + (u2-p2)^2)
  
  split.df$diff.upper =  split.df$fit.diff +  sqrt((p2-l2)^2 + (u1-p1)^2)
  
  split.df= as.data.frame(split.df)
  return(split.df)
  
  
}
