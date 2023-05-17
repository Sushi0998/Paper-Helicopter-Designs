theta = c(0.601666667, 0.128333333, 0.207222222, 0.049444444, 0.050555556, 0.21611111)
plot(qnorm(0.5+(1:6-0.5)/20),sort(abs(theta)), xlab = "Half Normal Quantiles", ylab = "Absolute Effects")
abline(lm(sort(abs(theta))~qnorm(0.5+(1:6-0.5)/20)), col = "red")
library(readxl)
flight_data <- read_excel("C:/Users/manav/OneDrive/Documents/Spring2023/ISEN616/flight data.xlsx")
#View(flight_data)

#Stepwise regression 
library(olsrr)
library(car)
#Removed below model due to High Colliearity
#mo_1<- lm(Average~l+w+W+L+F+d+l*w+l*W+l*L+l*F+l*d+w*W+w*L+w*F+w*d+W*L+W*F+W*d+L*F+L*d+F*d, data = flight_data)
#ols_step_both_p(mo_1)

#Main effects Model
mo_2<- lm(Average~l+w+W+L+F+d,data = flight_data)
ols_step_both_p(mo_2)
#after removing the most significant variable
mo_3<- lm(Average~w+W+L+F+d,data = flight_data)
ols_step_both_p(mo_3)

#model with l interactions
mo_l<- lm(Average~l+l:w+l:W+l:L+l:d+l:F, data = flight_data)
ols_step_both_p(mo_l)
#model with w interactions
mo_w<- lm(Average~w+w:l+w:L+w:W+w:d+w:F, data = flight_data)
ols_step_both_p(mo_w)

#model with L interactions
mo_L<- lm(Average~L+L:l+L:w+L:W+L:d+L:F, data = flight_data)
ols_step_both_p(mo_L, pent = 0.1, prem = 0.3)

#model with W interactions
mo_W<-lm(Average~W+W:l+W:w+W:L+W:d+W:F, data = flight_data)
ols_step_both_p(mo_W)

#model with d interactions
mo_d<-lm(Average~d+d:l+d:w+d:W+d:L+d:F, data = flight_data)
ols_step_both_p(mo_d)

#model with F interactions
mo_F<-lm(Average~F+F:l+F:w+F:W+F:L+F:d, data = flight_data)
ols_step_both_p(mo_F)

##Step2 choosing significant two-factor interactions from above in the model eqn
mo_final<- lm(Average~l+w+d+F+L+w:L+d:L+F:W, data = flight_data)
ols_step_both_p(mo_final)

#Step3 choosing the main effect from the step2 and running a equation with all its possible two factor interactions
mo_finally<- lm(Average~l+l:w+W:l+L:l+d:l+F:l, data = flight_data)
ols_step_both_p(mo_finally)


sm<-lm(Average~l,data = flight_data)
summary(sm)

  
