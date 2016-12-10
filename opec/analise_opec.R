options(max.print = 20)
require(zoo)

opec_basket <- read.csv2("data_05_11_16.csv", header = T, 
                         sep=",", dec=".", 
                         colClasses = c("Date", "numeric")) 

opec_basket <- zoo(opec_basket)

opec_basket <- merge(opec_basket,lag(opec_basket[,2], -(1:2)))

opec_basket[is.na(opec_basket)]<-0

opec_basket <- na.exclude(opec_basket, 0)
opec_basket <- as.data.frame(opec_basket[,(2:4)] , 
                             row.names=as.Date.factor(opec_basket[,1]))

opec_basket$Value <- as.numeric(as.character(opec_basket$Value))
opec_basket$"lag-1" <- as.numeric(as.character(opec_basket$"lag-1"))
opec_basket$"lag-2" <- as.numeric(as.character(opec_basket$"lag-2"))

opec_l <- lm(opec_basket[,1] ~ opec_basket[,2])
plot(opec_basket$Value)
#abline(opec_l)

#Histograma
#hist(as.numeric(levels(opec_basket[,2])))