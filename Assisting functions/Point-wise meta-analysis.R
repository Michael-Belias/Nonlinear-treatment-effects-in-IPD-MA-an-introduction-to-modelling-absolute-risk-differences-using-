#### code for running the point-wise meta-analysis

pointwise.ma = function(data, clustering.variable = "Study", 
                        combining.variables=  c("BMI","Treatment"), 
                        predicted.outcome =  "fit", 
                        predicted.outcome.se = NULL, 
                        predicted.outcome.CI = c("lower", "upper"), 
                        tau.method = "EB" ){
  
  
  split= data%>%
    group_by(!!as.name(clustering.variable) )%>%
    group_split()
  
  data <- merge(merge(merge(merge(
    split[[1L]],
    split[[2L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(1,2)),
    split[[3L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(2,3)),
    split[[4L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(3,4)),
    split[[5L]], by= combining.variables, all = TRUE, no.dups = T,suffixes = c(4,5))
  
  
  data$FE.meta =  NA
  data$FE.meta.upper =  NA
  data$FE.meta.lower =  NA
  data$FE.se =  NA
  
  
  data$RE.meta =  NA
  data$RE.meta.upper =  NA
  data$RE.meta.lower =  NA
  data$RE.se =  NA
  
  data$RE.meta.HKSJ =  NA
  data$RE.meta.HKSJ.upper =  NA
  data$RE.meta.HKSJ.lower =  NA
  data$RE.HKSJ.se =  NA
  
  
  data$Q =  NA
  data$H = NA
  
  data$H.HKSJ =  NA
  data$Q.HKSJ =  NA
  
  
  data$tau =  NA
  data$pval.Q =NA
  
  
  
  
  for(i in 1:dim(data)[1]){
    
    TE= data[i,]%>%
      select(contains(predicted.outcome))%>%
      t()%>%na.omit()%>%
      as.vector()
    
    if(!is.null(predicted.outcome.se)){
    seTE= data[i,]%>%
      select(contains(predicted.outcome.se))%>%
      t()%>%na.omit()%>%
      as.vector()
    
    meta1= metagen(TE= TE, seTE =seTE, method.tau = tau.method,hakn = T, adhoc.hakn = "ci", control=list(maxiter=1000)  )
    meta2= metagen(TE= TE, seTE =seTE, method.tau = tau.method,hakn = F, control = list(maxiter=1000) )
    
    }else if(!is.null(predicted.outcome.CI)){
        lower =  data[i,]%>%
          select(contains(predicted.outcome.CI[1]))%>%
          t()%>%na.omit()%>%
          as.vector()
        
        upper =  data[i,]%>%
          select(contains(predicted.outcome.CI[2]))%>%
          t()%>%na.omit()%>%
          as.vector()
        
        meta1= metagen(TE= TE, lower = lower, upper = upper, method.tau = tau.method,hakn = T, adhoc.hakn = "ci" )
        meta2= metagen(TE= TE, lower = lower, upper = upper, method.tau = tau.method,hakn = F )
        
      }
    
    ### Store the Heterogeneity measures
    data[i,]$Q =  meta2$Q
    data[i,]$Q.HKSJ =  meta1$Q
    data[i,]$H =  meta2$H
    data[i,]$H.HKSJ =  meta1$H
    
    ### Store the Fixed-effects model results
    data[i,]$FE.meta =  meta2$TE.fixed
    data[i,]$FE.meta.upper =  meta2$upper.fixed
    data[i,]$FE.meta.lower =  meta2$lower.fixed
    data[i,]$FE.se =  meta1$seTE.fixed
    
    data[i,]$tau =  meta2$tau2
    data[i,]$pval.Q =  meta2$pval.Q
    
    ### Store the Random-effects model results
    data[i,]$RE.meta =  meta2$TE.random
    data[i,]$RE.meta.upper =  meta2$upper.random
    data[i,]$RE.meta.lower =  meta2$lower.random
    data[i,]$RE.se =  meta2$seTE.random
    
    ### Store the Random-effects model with HKSJ results
    data[i,]$RE.meta.HKSJ =  meta1$TE.random
    data[i,]$RE.meta.HKSJ.upper =  meta1$upper.random
    data[i,]$RE.meta.HKSJ.lower =  meta1$lower.random
    data[i,]$RE.HKSJ.se =  meta1$seTE.random

    
    if(round(i*100/dim(data)[1]) != round((i- 1)*100/dim(data)[1] )){
      cat("\014") 
      print(paste(round(i*100/dim(data)[1]),"%",sep = ""))
      }
    
  }

  
  return(data)
}

