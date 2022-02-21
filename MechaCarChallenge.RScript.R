#Deliverable 1
#3 import libraries
library(dplyr)
library(tidyverse)

#4 import data into df
MechaCar_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#5 linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_df)

#6
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_df))

#bonus: eliminate the independent variables that have little impact on predicting mpg to see impact
lm(mpg ~ vehicle_length + ground_clearance, data=MechaCar_df)
summary(lm(mpg ~ vehicle_length + ground_clearance, data=MechaCar_df)) 

# Deliverable 2

#2 
coil_df <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F) 

#3
total_summary <- coil_df %>% summarize(Mean=mean(PSI),
                                      Median=median(PSI), 
                                      Variance=var(PSI),
                                      SD=sd(PSI), 
                                      .groups = 'keep') 

#4
lot_summary <- coil_df  %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),
                                                                         Median=median(PSI),
                                                                         Variance=var(PSI),
                                                                         SD=sd(PSI),
                                                                         .groups = 'keep')  


plt1 <- ggplot(coil_df,aes(y=PSI)) 
plt1 + geom_boxplot() 


plt2 <- ggplot(coil_df,aes(x=Manufacturing_Lot,y=PSI)) 
plt2 + geom_boxplot()

# Delieverable 3

# 1
t.test(coil_df$PSI,mu=1500)

# 2
lot1 <- subset(coil_df, Manufacturing_Lot=="Lot1")
lot2 <- subset(coil_df, Manufacturing_Lot=="Lot2")
lot3 <- subset(coil_df, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)






