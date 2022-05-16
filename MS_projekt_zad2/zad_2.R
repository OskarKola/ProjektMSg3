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

#Z5 histogramy rezyduow

poland_res<-resid(model_poland)
hist(poland$nawoz, poland_res, ylab = "rezydua", xlab = "model regresji", 
     main = "Histogram rezyduow, Polska")
#abline(0,0)

hungary_res<-resid(model_hungary)
hist(hungary$nawoz, hungary_res, ylab = "rezydua", xlab = "model regresji", 
     main = "Histogram rezyduow, Wegry")
#abline(0,0)

polstd.stdres = rstandard(model_poland)
hunstd.stdres = rstandard(model_hungary)

qqnorm(polstd.stdres, 
             ylab="Standardized Residuals", 
             xlab="Normal Scores", 
             main="Poland") 
qqline(polstd.stdres)

qqnorm(hunstd.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Hungary") 
qqline(hunstd.stdres)
