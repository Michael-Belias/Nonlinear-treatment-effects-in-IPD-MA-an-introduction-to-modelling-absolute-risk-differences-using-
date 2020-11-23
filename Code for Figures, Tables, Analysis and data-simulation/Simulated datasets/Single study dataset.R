### This is the code for the single study dataset


set.seed(831)
single.df =  data.frame(BMI= rep(seq(18.5,to = 40, length.out = 100),each= 5),
                        Treat =  rep(c(0,1),250))


## We create a pseudo-variable that will help us create the curves we described in the paper
single.df$BMI.standardised =  with(single.df, 2*(BMI-25)/40)
## And add a column to generate the true underlying mortality risk 
single.df$`Mortality risk` = NA

### Generate the mortality risk as function of BMI
single.df[single.df$Treat==0,]$`Mortality risk` =  with(single.df[single.df$Treat==0,], 0.2+  BMI.standardised^2)
single.df[single.df$Treat==1,]$`Mortality risk` =  with(single.df[single.df$Treat==1,], 0.2+  BMI.standardised^4)

### Create the binary outcome given the mortality risks calculated above
single.df$Y <- rbinom(dim(single.df)[1],1,single.df$`Mortality risk`) 


single.df$Treatment =  factor(single.df$Treat, levels = c(0,1), labels = c("Control","Treated"))
