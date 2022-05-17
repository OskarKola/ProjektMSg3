# modele regresji liniowej przedstawiające zależności wysokości
# zebranych plonów z hektara w tonach od ilości użytego nawozu

data <- read.delim("dane3.txt") #LOAD DATA
names(data) <- c('wyd', 'nawoz', 'kraj') #change column names

# Ilość zboża a ilość nawozu

### POLAND ###
poland = data[data$kraj ==1, ]

## wydajność~nawóz

model_poland<-lm(wyd~nawoz, data = poland)
summary(model_poland)
### HUNGARY ###
hungary = data[data$kraj ==0, ]

model_hungary<-lm(wyd~nawoz, data = hungary)
summary(model_hungary)

### POLAND & HUNGARY ###

model<-lm(wyd ~ nawoz, data = data)
summary(model)

#Z3 standardowy błąd szacunku
confint(model)
summary(model)
sigma(model)*100/mean(data$nawoz)
sigma(model)*100/mean(data$wyd)

confint(model_hungary)
summary(model_hungary)
sigma(model_hungary)*100/mean(hungary$nawoz)
sigma(model_hungary)*100/mean(hungary$wyd)

confint(model_poland)
summary(model_poland)
sigma(model_poland)*100/mean(poland$nawoz)
sigma(model_poland)*100/mean(poland$wyd)

#Z4 wykresy zależności
predict_poland = predict(model_poland)
plot(poland$nawoz, poland$wyd, main="wykres zaleznosci, Polska", xlab = "nawoz", ylab = "wydajnosc", col = "red")
abline(model_poland, col = "blue")
lines(predict_poland, col = "green")

predict_hungary = predict(model_hungary)
plot(hungary$nawoz, hungary$wyd, main="wykres zaleznosci, Wegry", xlab = "nawoz", ylab = "wydajnosc", col = "red")
abline(model_hungary, col = "blue")
lines(predict_hungary, col = "green")

predict_all = predict(model)
plot(data$nawoz, data$wyd, main="wykres zaleznosci, razem", xlab = "nawoz", ylab = "wydajnosc", col = "red")
abline(model, col = "blue")
lines(predict_all, col = "green")

#Z5 histogramy rezyduow

#poland_res<-resid(model_poland)
#hist(poland$nawoz, poland_res, ylab = "rezydua", xlab = "model regresji", 
#    main = "Histogram rezyduow, Polska")
#abline(0,0)

#hungary_res<-resid(model_hungary)
#hist(hungary$nawoz, hungary_res, ylab = "rezydua", xlab = "model regresji", 
#     main = "Histogram rezyduow, Wegry")
#abline(0,0)

#all_res<-resid(model)
#hist(data$nawoz, all_res, ylab = "rezydua", xlab = "model regresji", 
#     main = "Histogram rezyduow, Razem")
#abline(0,0)

respol <-residuals(model_poland)
hist(respol)
reshun <- residuals(model_hungary)
hist(reshun)
resall <-residuals(model)
hist(resall)

polstd.stdres = rstandard(model_poland)
hunstd.stdres = rstandard(model_hungary)
allstd.stdres = rstandard(model)

qqnorm(polstd.stdres, 
             ylab="Standardized Residuals", 
             xlab="Normal Scores", 
             main="Polska") 
qqline(polstd.stdres)

qqnorm(hunstd.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Wegry") 
qqline(hunstd.stdres)

qqnorm(allstd.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Razem") 
qqline(allstd.stdres)
