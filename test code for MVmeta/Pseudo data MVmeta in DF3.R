
### MVmeta with pseudo-data

### Generate pseudo data with limited weight

## For instance, as you can see in our simulated data-set the range of BMI in Study 1 is [18.5,27] while in Study 5 [31,40]
## To avoid errors we may need to create pseudo-data on the boundaries and give them very small weight to avoid biased regression curves. 

## We introduce the weight variable 
## In the original data-set the weight will be 1
df3$weight =  1


## We save into objects the values near the overall boundaries 18.5 and 40 
## Note that how close we wish to be on the boundaries is arbitrary. 

lower=df3[df3$BMI<19,] ;   n.lower =  dim(lower)[1]
upper=df3[df3$BMI >39.5,]; n.upper =  dim(upper)[1]


## We produce 4 copies of the lower and upper data-sets and give them very small weights.

rep.lower=  do.call(rbind, replicate(4, lower, simplify = FALSE)) ;  rep.lower$weight= 0.00001
rep.upper=  do.call(rbind, replicate(4, upper, simplify = FALSE)) ;  rep.upper$weight= 0.00001

## As you can see the lower and upper data-sets use the original study information
head(rep.lower); head(rep.upper)
## So we change the names of the studies within the data-sets. 
rep.lower$Study = rep(unique(df3$Study)[-1], each= n.lower)
rep.upper$Study = rep(unique(df3$Study)[-5], each= n.upper)

## And we add them to the original data-set.


df3.with.pseudo =  rbind(df3, rep.lower, rep.upper)


### remove the object we don't need

rm(lower,upper, rep.lower,rep.upper)


### RCS  
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df3.with.pseudo =   list(BMI=quantile(df3$BMI, probs = c(0.05,0.275,0.5,0.725,0.95))) 

## The formula for all Studies is the same so we save it 

formula.RCS = Y~ I(BMI) + Treatment+ s(I(BMI),bs ="cr",fx=T,by = Treatment,k = 5)

## Number of studies

nstudies.RCS.df3.with.pseudo = length(unique(df3.with.pseudo$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df3.with.pseudo = gam( formula =formula.RCS,
                               knots = Knots.RCS.df3,
                               family = binomial("logit"), 
                               weight= weight,
                               data = df3.with.pseudo)

### Get the model matrices for each data-set

X.p.RCS.df3.with.pseudo =  model.matrix(fit.RCS.df3.with.pseudo)

### Create empty matrix for the estimated splines coefficients

estimated.coefficients.RCS.df3.with.pseudo = matrix(NA,
                                                    ncol = length(fit.RCS.df3.with.pseudo$coefficients),nrow=nstudies.RCS.df3.with.pseudo,
                                                    dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.RCS.df3.with.pseudo)),1, paste, collapse=" "),
                                                                     c(1:length(fit.RCS.df3.with.pseudo$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.RCS.df3.with.pseudo = matrix(NA, ncol=sum(c(1:length(fit.RCS.df3.with.pseudo$coefficients))), nrow = nstudies.RCS.df3.with.pseudo )







k=3
j=1

for( i in unique(df3.with.pseudo$Study)){
  
  ### Get a mini df3.with.pseudo with only one study
  minidf3.with.pseudo = df3.with.pseudo%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.RCS ,
            knots = Knots.RCS.df3.with.pseudo,
            weights = weight,
            family = binomial("logit"),
            data = minidf3.with.pseudo)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df3.with.pseudo[j,] = fit$coefficients
  S.RCS.df3.with.pseudo[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  #rm(i,minidf3.with.pseudo,fit)
}
rm(k,j)


preds=  as.data.frame(predict(fit,se.fit = T,df3))
check.pseudo  =  cbind(df3, preds)

ggplot(check.pseudo, mapping = aes(BMI, Y,color= Treatment)) + 
  geom_line( aes(BMI, expit(fit),color= Treatment)) +
  geom_ribbon(aes(ymin =  expit(fit-1.96*se.fit), ymax = expit(fit+1.96*se.fit)),alpha = 0.2)





#### Perform a multivariate meta-analysis
mv.fit.RCS.df3.with.pseudo = mvmeta(estimated.coefficients.RCS.df3.with.pseudo, S.RCS.df3.with.pseudo)


prediction.interval.mvmeta.RCS.df3.with.pseudo =  X.p.RCS.df3.with.pseudo %*% coef(mv.fit.RCS.df3.with.pseudo)

prediction.interval.mvmeta.lower.RCS.df3.with.pseudo =  X.p.RCS.df3.with.pseudo %*% coef(mv.fit.RCS.df3.with.pseudo) - sqrt( rowSums(X.p.RCS.df3.with.pseudo  * (X.p.RCS.df3.with.pseudo  %*% vcov(mv.fit.RCS.df3.with.pseudo)))) * qt(0.025, dim(df3.with.pseudo)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df3.with.pseudo =  X.p.RCS.df3.with.pseudo %*% coef(mv.fit.RCS.df3.with.pseudo) + sqrt( rowSums(X.p.RCS.df3.with.pseudo  * (X.p.RCS.df3.with.pseudo  %*% vcov(mv.fit.RCS.df3.with.pseudo)))) * qt(0.025, dim(df3.with.pseudo)[1] - 3)


mvmeta.df3.with.pseudo.RCS = cbind(df3.with.pseudo[,c("Study","BMI","Treatment")],
                                   fit =  expit(prediction.interval.mvmeta.RCS.df3.with.pseudo), 
                                   Lower= expit(prediction.interval.mvmeta.lower.RCS.df3.with.pseudo),
                                   Upper =expit(prediction.interval.mvmeta.upper.RCS.df3.with.pseudo ))





g.mvmeta.total.RCS.Comb = ggplot(mvmeta.df3.with.pseudo.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
        axis.text.y = element_text(face="bold",  size=18),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.text=element_text(size=20, hjust = 0), 
        legend.title =element_blank(),
        legend.position = "none")  + 
  annotate(geom = "text",x = 19.25,y = 0.8,label=  " a", size= 12 ) 


g.mvmeta.total.RCS.Comb

