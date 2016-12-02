#Scrap of my script analysis 


#ts_opec_basket <- ts(opec_basket, start=c(2003,01,02), 
#        end = c(2016,02,01), frequency = 1)
#summary(ts_opec_basket)

cycle(ts_opec_basket)
summary(opec_basket)
?lag
opec <- opec_basket
opec["lag1"] <-lag(opec_basket[,2], k=1)
na.action(lag(opec_basket[,2], k=1))
?na.action
