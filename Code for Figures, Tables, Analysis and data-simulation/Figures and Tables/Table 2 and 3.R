## ----Create a table reporting the mean and ranges of BMI per study for second scenario IPD-set -------------------------------------------------------------------------------------
table2 = df2%>%
  group_by(Study)%>%
  summarise("Range of BMI" =  paste("[", round(min(BMI),1), ",", round(max(BMI),1),"]", sep = ""), "Average BMI" =  round(mean(BMI),1))

colnames(table2) =  c("Studies","BMI Range","Average BMI")

table2

## ----Create a table reporting the mean and ranges of BMI per study for third scenario IPD-set -------------------------------------------------------------------------------------

table3 = df3%>%
  group_by(Study)%>%
  summarise("Range of BMI" =  paste("[", round(min(BMI),1), ",", round(max(BMI),1),"]", sep = ""), "Average BMI" =  round(mean(BMI),1))

colnames(table3) =  c("Studies","BMI Range","Average BMI")

table3
