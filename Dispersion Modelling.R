theta1 = c(6.581972142, 4.670444453, 0.485674906, 5.222007114, 0.372468859, 2.683919224)
plot(qnorm(0.5+(1:6-0.5)/20),sort(abs(theta1)),xlab = "Quantiles", ylab = "Absolute Effects" )
abline(lm(sort(abs(theta1))~qnorm(0.5+(1:6-0.5)/20)), col = "red", x)

library(readxl)
flight_data1 <- read_excel("C:/Users/manav/OneDrive/Documents/Spring2023/ISEN616/flight data 1.xlsx")
View(flight_data1)

#Stepwise regression 
library(olsrr)
library(car)
#Removed below model due to High Colliearity
mo_1<- lm(flight_data1$lns^2~l+w+W+L+F+d+l*w+l*W+l*L+l*F+l*d+w*W+w*L+w*F+w*d+W*L+W*F+W*d+L*F+L*d+F*d, data = flight_data1)
#ols_step_both_p(mo_1)

#Main effects Model
mo_2<- lm(flight_data1$`lns^2`~l+w+W+L+F+d,data = flight_data1)
ols_step_both_p(mo_2)
#after removing the most significant variable
mo_3<- lm(flight_data1$`lns^2`~w+W+L+F+d,data = flight_data1)
ols_step_both_p(mo_3)

#model with l interactions
mo_l1<- lm(flight_data1$`lns^2`~l+l:w+l:W+l:L+l:d+l:F, data = flight_data1)
ols_step_both_p(mo_l1)
#model with w interactions
mo_w1<- lm(flight_data1$`lns^2`~w+w:l+w:L+w:W+w:d+w:F, data = flight_data1)
ols_step_both_p(mo_w1)

#model with L interactions
mo_L1<- lm(flight_data1$`lns^2`~L+L:l+L:w+L:W+L:d+L:F, data = flight_data1)
ols_step_both_p(mo_L1)

#model with W interactions
mo_W1<-lm(flight_data1$`lns^2`~W+W:l+W:w+W:L+W:d+W:F, data = flight_data1)
ols_step_both_p(mo_W1)

#model with d interactions
mo_d1<-lm(flight_data1$`lns^2`~d+d:l+d:w+d:W+d:L+d:F, data = flight_data1)
ols_step_both_p(mo_d1)

#model with F interactions
mo_F1<-lm(flight_data1$`lns^2`~F+F:l+F:w+F:W+F:L+F:d, data = flight_data1)
ols_step_both_p(mo_F1)

##Step2 choosing significant two-factor interactions from above in the model eqn
mo_final1<- lm(flight_data1$`lns^2`~l+w+d+F+W+L+w:F+L:W+W:d+d:F, data = flight_data1)
ols_step_both_p(mo_final1)

#Step3 choosing the main effect from the step2 and running a equation with all its possible two factor interactions
mo_finally1<- lm(flight_data1$`lns^2`~L+w+l+l:w+l:W+l:L+l:d+l:F+w:W+w:d+L:w+w:F+L:W+L:d+L:F, data = flight_data1)
ols_step_both_p(mo_finally1)

#Full_mo_<- lm(flight_data1$`lns^2`~l+w+W+L+F+d+l*w+l*W+l*L+l*F+l*d+w*W+w*L+w*F+w*d+W*L+W*F+W*d+L*F+L*d+F*d, data = flight_data1)
#options(max.print = 5000)
#ols_step_all_possible(Full_mo_)


sm1<-lm(flight_data1$`lns^2`~l,data = flight_data1)
summary(sm1)
