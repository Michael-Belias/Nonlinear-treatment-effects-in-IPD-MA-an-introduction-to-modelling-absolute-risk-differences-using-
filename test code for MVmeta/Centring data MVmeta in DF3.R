## ----Simulation of third IPD-MA scenario --------------------------------------------------------------------------------------

source("Code for Figures, Tables, Analysis and data-simulation/Simulated datasets/Third scenario data-set.R")

### Introduce the expit function to back-transform from logit scale

expit<-function(rs) {1/(1+exp(-rs))}


### RCS
###

## The formula for all studies is the same so we save it 

df3$c.BMI =  df3$BMI - 27.5

### MVmeta without pseudo-data.

### RCS  
### Define the knots position in 5%, 27.5%,  50%, 72.5% and 95% quantiles of BMI
Knots.RCS.df3 =   list(BMI=quantile(df3$c.BMI, probs = c(0.05,0.275,0.5,0.725,0.95))) 

## The formula for all Studies is the same so we save it 

formula.RCS = Y~ I(c.BMI) + Treatment+ s(I(c.BMI),bs ="cr",fx=T,by = Treatment,k = 5)

## Number of studies

nstudies.RCS.df3 = length(unique(df3$Study))


### Fit a stacked analysis in the whole data-set to get the model matrix to get the predicted outcomes later on

fit.RCS.df3 = gam( formula =formula.RCS,
                   knots = Knots.RCS.df3,
                   family = binomial("logit"), 
                   data = df3)



plot.df3 =  cbind(df3, predict(fit.RCS.df3, se.fit = T))
ggplot(plot.df3, aes(BMI, Y, color= Treatment)) +geom_point() + geom_line(aes(BMI,expit(fit)))+ 
  geom_ribbon(aes(ymin= expit(fit-1.96*se.fit), ymax=expit(fit+1.96*se.fit), color= Treatment),alpha =  0.3)



### Get the model matrices for each data-set

X.p.RCS.df3 =  model.matrix(fit.RCS.df3)



### Create empty matrix for the estimated splines coefficients

estimated.coefficients.RCS.df3 = matrix(NA,
                                        ncol = length(fit.RCS.df3$coefficients),nrow=nstudies.RCS.df3,
                                        dimnames = list( apply(expand.grid(c("Study"),c(1:nstudies.RCS.df3)),1, paste, collapse=" "),
                                                         c(1:length(fit.RCS.df3$coefficients))))


### Create empty matrix for the variance-covariance matrix of the coefficients

S.RCS.df3 = matrix(NA, ncol=sum(c(1:length(fit.RCS.df3$coefficients))), nrow = nstudies.RCS.df3 )

k=3
j=1

for( i in unique(df3$Study)){
  
  ### Get a mini df3 with only one study
  minidf3 = df3%>%
    filter(Study == i)
  
  # Fit the GAM
  fit = gam(formula = formula.RCS ,
            knots = Knots.RCS.df3, 
            family = binomial("logit"), data = minidf3)
  ### Extract the coefficients and their standard errors for mvmeta
  estimated.coefficients.RCS.df3[j,] = fit$coefficients
  S.RCS.df3[j,] = vcov(fit)[lower.tri(vcov(fit), diag = T)]
  
  k=k+2
  j=j+1
  #rm(i,minidf3,fit)
}
rm(k,j)

preds.df3=  as.data.frame(predict(fit,se.fit = T, df3))
check  =  cbind(df3, preds.df3)

ggplot(check, mapping = aes(BMI, Y,color= Treatment)) + 
  geom_line( aes(BMI, expit(fit),color= Treatment)) +
  geom_ribbon(aes(ymin =  expit(fit-1.96*se.fit), ymax = expit(fit+1.96*se.fit)),alpha = 0.2)




#### Perform a multivariate meta-analysis
mv.fit.RCS.df3 = mvmeta(estimated.coefficients.RCS.df3, S.RCS.df3)


prediction.interval.mvmeta.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3)

prediction.interval.mvmeta.lower.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3) - sqrt( rowSums(X.p.RCS.df3  * (X.p.RCS.df3  %*% vcov(mv.fit.RCS.df3)))) * qt(0.025, dim(df3)[1] - 3)

prediction.interval.mvmeta.upper.RCS.df3 =  X.p.RCS.df3 %*% coef(mv.fit.RCS.df3) + sqrt( rowSums(X.p.RCS.df3  * (X.p.RCS.df3  %*% vcov(mv.fit.RCS.df3)))) * qt(0.025, dim(df3)[1] - 3)


mvmeta.df3.RCS = cbind(df3[,c("Study","BMI","Treatment")],
                       fit =  expit(prediction.interval.mvmeta.RCS.df3), 
                       Lower= expit(prediction.interval.mvmeta.lower.RCS.df3),
                       Upper =expit(prediction.interval.mvmeta.upper.RCS.df3 ))





g.mvmeta.total.RCS.Comb = ggplot(mvmeta.df3.RCS,aes(x = BMI, fit, linetype= Treatment, color= Treatment)) + 
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
