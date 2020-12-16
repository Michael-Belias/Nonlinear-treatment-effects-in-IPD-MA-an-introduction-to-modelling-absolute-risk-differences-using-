
source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Second scenario data-set.R")
#Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}

#rm(list=ls()[! ls() %in% c("df1","df2","df3","expit","single.df")])

df2%>%
  group_by(Study)%>%
  summarise(range(BMI))


## We follow White et al. and Riley et al. recommendations and we performed data augmentation
## In our simulated data-set the range of BMI in Study 1 is [18.5,27] while in Study 5 [31,40]
## To avoid errors we create pseudo-data on the boundaries and give them very small weight to 
## avoid biased regression curves. 

## We introduce the weight variable in our data-set 
## In the original data-set the weight will be 1, while in the augmented data-set 0.000000001
df2$weight =  1


## We save into objects the values near the overall boundaries 18.5 and 40 
## Note that how close we wish to be on the boundaries is arbitrary. 

lower=df2[df2$BMI<19,] ;   n.lower =  dim(lower)[1]; lower$BMI = 18.5
upper=df2[df2$BMI >39.5,]; n.upper =  dim(upper)[1]; upper$BMI = 40


## We produce 4 copies of the lower and upper data-sets and give them very small weights.

rep.lower=  do.call(rbind, replicate(4, lower, simplify = FALSE)) ;  rep.lower$weight= 0.0000001
rep.upper=  do.call(rbind, replicate(4, upper, simplify = FALSE)) ;  rep.upper$weight= 0.0000001

## As you can see the lower and upper data-sets use the original study information
head(rep.lower); head(rep.upper)
## So we change the names of the studies within the data-sets. 

rep.lower$Study = rep(unique(df2$Study)[-1], each= n.lower)
rep.upper$Study = rep(unique(df2$Study)[-5], each= n.upper)

## And we add them to the original data-set.


df2 =  rbind(df2, rep.lower, rep.upper)
## Now each study has values near the boundaries of BMI [18.5,40]

df2%>%
  group_by(Study)%>%
  summarise(range(BMI))


### remove the object we don't need

rm(lower,upper, rep.lower,rep.upper,n.lower, n.upper)

### NS B-splines
###


### Get the knots from the overall model to use the same per study
Knots.ns.df2 =  c(25.66667, 32.83333)


## The formula for all studies is the same so we save it 

formula.ns = formula = Y~ Treatment + ns(BMI,knots =  Knots.ns.df2)*Treatment

## Number of studies

nstudies.ns.df2 = length(unique(df2$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.ns.df2 = glm( formula =formula.ns,
                  family = binomial("logit"), 
                  data = df2[df2$weight==1,])

### Get the model matrices for each data-set

X.p.ns.df2 =  model.matrix(fit.ns.df2)

### In order for mvmeta to work we need the applied B-spline to be fitted on the same domain
### Create empty matrix for the estimated splines coefficients

estimated.coefficients.ns.df2 = matrix(NA,
                                       ncol = length(fit.ns.df2$coefficients),nrow=nstudies.ns.df2,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.ns.df2)),1, paste, collapse=" "),
                                                        c(1:length(fit.ns.df2$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.ns.df2 = matrix(NA, ncol=sum(c(1:length(fit.ns.df2$coefficients))), nrow = nstudies.ns.df2 )


j=1

for( i in unique(df2$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = glm(formula = formula.ns,
            weights = weight,
            family = binomial("logit"), 
            data = minidf2)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.ns.df2[j,] = fit$coefficients
  S.ns.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  j=j+1
  #rm(i,minidf2,fit)
}
rm(j)


library(mvmeta)
#### Perform a multivariate meta-analysis
mv.fit.ns.df2 = mvmeta(estimated.coefficients.ns.df2, 
                       S.ns.df2,
                       control = list(maxiter=1000))


prediction.interval.mvmeta.ns.df2 =  X.p.ns.df2 %*% coef(mv.fit.ns.df2)

prediction.interval.mvmeta.lower.ns.df2 =  X.p.ns.df2 %*% coef(mv.fit.ns.df2) - sqrt( rowSums(X.p.ns.df2  * (X.p.ns.df2  %*% vcov(mv.fit.ns.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.ns.df2 =  X.p.ns.df2 %*% coef(mv.fit.ns.df2) + sqrt( rowSums(X.p.ns.df2  * (X.p.ns.df2  %*% vcov(mv.fit.ns.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df2.ns = cbind(df2[df2$weight==1,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.ns.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.ns.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.ns.df2 ))





g.mvmeta.total.ns.DR = ggplot(mvmeta.df2.ns,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30), 
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 30,y = 0.8,label=  " ns() mvmeta scenario 2", size= 12 ) 


g.mvmeta.total.ns.DR



### B-splines
###

### Get the knots from the overall model to use the same per study
Knots.bs.df2 =  c(25.66667, 32.83333)

## The formula for all studies is the same so we save it 

formula.bs = formula = Y~ Treatment + bs(BMI,knots = Knots.bs.df2)*Treatment

## Number of studies

nstudies.bs.df2 = length(unique(df2$Study))



### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.bs.df2 = glm( formula =formula.bs,
                  family = binomial("logit"), 
                  data = df2[df2$weight==1,])

### Get the model matrices for each data-set

X.p.bs.df2 =  model.matrix(fit.bs.df2)





### In order for mvmeta to work we need the applied B-spline to be fitted on the same domain




### Create empty matrix for the estimated splines coefficients

estimated.coefficients.bs.df2 = matrix(NA,
                                       ncol = length(fit.bs.df2$coefficients),nrow=nstudies.bs.df2,
                                       dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.bs.df2)),1, paste, collapse=" "),
                                                        c(1:length(fit.bs.df2$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.bs.df2 = matrix(NA, ncol=sum(c(1:length(fit.bs.df2$coefficients))), nrow = nstudies.bs.df2 )


j=1

for( i in unique(df2$Study)){
  
  ### Get a mini df2 with only one study
  minidf2 = df2%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = glm(formula = formula.bs,
            weights = weight,
            family = binomial("logit"), 
            data = minidf2)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.bs.df2[j,] = fit$coefficients
  S.bs.df2[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  
  j=j+1
  
  #rm(i,minidf2,fit)
}
rm(j)


estimated.coefficients.bs.df2[is.na(estimated.coefficients.bs.df2)] = 0
S.bs.df2[is.na(S.bs.df2)] = 0



#### Perform a multivariate meta-analysis
mv.fit.bs.df2 = mvmeta(estimated.coefficients.bs.df2, S.bs.df2,control = list(maxiter=1000))


prediction.interval.mvmeta.bs.df2 =  X.p.bs.df2 %*% coef(mv.fit.bs.df2)

prediction.interval.mvmeta.lower.bs.df2 =  X.p.bs.df2 %*% coef(mv.fit.bs.df2) - sqrt( rowSums(X.p.bs.df2  * (X.p.bs.df2  %*% vcov(mv.fit.bs.df2)))) * qt(0.025, dim(df2)[1] - 3)

prediction.interval.mvmeta.upper.bs.df2 =  X.p.bs.df2 %*% coef(mv.fit.bs.df2) + sqrt( rowSums(X.p.bs.df2  * (X.p.bs.df2  %*% vcov(mv.fit.bs.df2)))) * qt(0.025, dim(df2)[1] - 3)


mvmeta.df2.bs = cbind(df2[df2$weight==1,c("Study","BMI","Treatment")],
                      fit =  expit(prediction.interval.mvmeta.bs.df2), 
                      Lower= expit(prediction.interval.mvmeta.lower.bs.df2),
                      Upper =expit(prediction.interval.mvmeta.upper.bs.df2 ))





g.mvmeta.total.bs.DR = ggplot(mvmeta.df2.bs,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
  geom_line(size=2)+ ylim(c(0,1))+
  ylab("")+ xlab("") + scale_color_jama()+ 
  theme_bw()+ geom_ribbon(mapping = aes(ymin=Lower, ymax=Upper),alpha=0.25)+
  theme(plot.title    = element_text(hjust = 0.5,size = 26,face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,size = 18,face = "bold.italic"),
        axis.text.x.bottom  = element_text(angle = 0, vjust = 0.5, size=24),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(2, "lines"),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text = element_text(face="bold", size=16, hjust = 0.5),
        axis.title.y = element_text(size = 30), 
        axis.title.x = element_text(size = 30),
        axis.text.y = element_text(face="bold",  size=24),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=42, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 25,y = 0.8,label=  " bs() mvmeta", size= 12 ) 


g.mvmeta.total.bs.DR



grid.arrange(g.mvmeta.total.bs.DR, g.mvmeta.total.ns.DR)

