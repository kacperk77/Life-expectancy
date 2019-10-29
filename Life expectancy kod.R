setwd('C:\\Users\\lenovo\\Desktop\\sgh\\ekonometria')
remove(list = ls())

library(Hmisc)
library(AER)
library(car)
library(lmtest)
library(stargazer)
library(tidyverse)
library(tseries)
library(corrplot)
options(scipen = 100)

remove(data,data2)
#wczytanie pierwszego data setu
data <- read.csv("lifeexpectancy1.csv", sep = ',')
summary(data)
data <- data %>% select(literacyrate, homicidiesper100k, electricity, Schooling, 
                        HIV.AIDS, wateraccess, tuberculosis, inflation, gdppercapita, healthexppercapita,
                        fertilityrate, lifeexp, internet, gdppercapita, CO2, forest, 
                        urbanpop, urbanpopgrowth, leastdeveloped)


#model1
model1 <- lm(lifeexp ~ . , data = data)
summary(model1)



#wczytanie drugiego data setu
data2 <- read.csv("lifeexpectancy2.csv")
summary(data2)
data2$Status <- ifelse(data2$Country %in% 
                         c('France', 'Canada'), 'Developed', as.character(data2$Status))
data2 <- data2 %>% select(Schooling, HIV.AIDS, tuberculosis, inflation, healthexppercapita, 
                          fertilityrate, lifeexp, CO2, urbanpopgrowth, wateraccess,
                           leastdeveloped)





#wizualizacja 
cor <- cor(data2[,
         c("Schooling","HIV.AIDS","tuberculosis","healthexppercapita","fertilityrate","CO2",
           "urbanpopgrowth", "wateraccess", "leastdeveloped")
         ])

corrplot.mixed(cor,  tl.col="black", tl.pos = "lt")



hist(data2$lifeexp[data2$leastdeveloped==0], xlab = "developing and developed", 
     ylab = "czestotliwosc",col = "blue", border = "black", main = "Histogram")

hist(data2$lifeexp[data2$leastdeveloped==1], xlab = "leastdeveloped", 
     ylab = "czestotliwosc",col = "red", border = "black", main = "Histogram", add = T)
legend("topleft", c("developing and developed", "least developed"), col=c("blue", "red"), lwd=10)

p <- ggplot(data = data2 , mapping = aes(x = Schooling, y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)

p + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))

p1 <- ggplot(data = data2 , mapping = aes(x = HIV.AIDS, y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)
  
p1 + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))


p2 <- ggplot(data = data2 , mapping = aes(x = tuberculosis, y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)

p2 + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))

p3 <- ggplot(data = data2 , mapping = aes(x = healthexppercapita, 
                                          y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)

p3 + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))

p4 <- ggplot(data = data2 , mapping = aes(x = fertilityrate, 
                                          y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)

p4 + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))

p5 <- ggplot(data = data2 , mapping = aes(x = CO2, 
                                          y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)

p5 + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))

p6 <- ggplot(data = data2 , mapping = aes(x = wateraccess, 
                                          y = lifeexp, fill = as.factor(leastdeveloped)))+
  geom_point(shape=21, color="blue",  size=3)

p6 + scale_fill_discrete(name = "Legend", labels = c("developing and developed", "least developed"))



#model2
model2 <- lm(lifeexp~Schooling+HIV.AIDS+tuberculosis+healthexppercapita+
               fertilityrate+urbanpopgrowth+
               CO2+wateraccess+
             leastdeveloped,
             data = data2)

summary(model2)

#Test RESET na poprawna postac funkcyjna
reset(model2)

#normalnosc skladnika losowego
jarque.bera.test(model2$residuals)
hist(model2$residuals,breaks=50,col="grey",xlab="reszty",ylab="czestotliwosc")
#wspoliniowosc
vif(model2)

#Diagnozowanie heteroskedastycznosci skladnika losowego
data2$ehat2		=model2$residuals^2
plot(data2$Schooling, data2$ehat2)
plot(data2$HIV.AIDS, data2$ehat2)
plot(data2$tuberculosis, data2$ehat2)
plot(data2$healthexppercapita, data2$ehat2)
plot(data2$fertilityrate, data2$ehat2)
plot(data2$urbanpopgrowth, data2$ehat2)
plot(data2$CO2, data2$ehat2)
plot(data2$wateraccess, data2$ehat2)
plot(data2$leastdeveloped, data2$ehat2)

bptest(model2)

#test white'a
white.model <- lm(ehat2~Schooling+HIV.AIDS+tuberculosis+healthexppercapita+fertilityrate+urbanpopgrowth+
                    CO2+wateraccess+
                    leastdeveloped+I(Schooling^2)+I(HIV.AIDS^2)+I(tuberculosis^2)+
                    I(healthexppercapita^2)+I(fertilityrate^2)+I(urbanpopgrowth^2)+
                    I(CO2^2)+I(wateraccess^2)
                    ,
                  data = data2)

N=nrow(data2)
summary(white.model)
LM=N*summary(white.model)$r.squared 
pchisq(LM,17, lower.tail=FALSE)



#model3 - log-linear
model3 <- lm(log(lifeexp)~Schooling+HIV.AIDS+tuberculosis+healthexppercapita+
               fertilityrate+urbanpopgrowth+
               CO2+wateraccess+
               leastdeveloped,
             data = data2)


summary(model3)
reset(model3)
bptest(model3)
vif(model3)

testy <- data.frame("model2" = c(0.01519, 0.000003413, 0.01313) , 
                    "model3" = c(0.07264, 0.00000000000000022, 0.01552), stringsAsFactors = FALSE) 
rownames(testy) <- c("pvaluetestRESET", "pvaluetestJB", "pvaluetestBP")
testy

#Odporny estymator wariancji-kowariancji White’a 
VCOVHC <- vcovHC(model3, type = "HC0")

#oszacowanie wls
ehat2 <- model3$residuals^2


auxilary.lm <- lm(log(ehat2)~Schooling+HIV.AIDS+tuberculosis+healthexppercapita+
                    fertilityrate+urbanpopgrowth+CO2+wateraccess+
                    leastdeveloped, data = data2)
data2$weights1 <- 1/(exp(auxilary.lm$fitted.values))


wls <- lm(log(lifeexp)~Schooling+HIV.AIDS+tuberculosis+healthexppercapita+
             fertilityrate+urbanpopgrowth+CO2+
             wateraccess+leastdeveloped, 
           data = data2, weights = data2$weights1)

summary(wls)

coeftest(model3)
coeftest(model3, vcov = VCOVHC)


#ostateczne oszacowanie
summary(wls)


    