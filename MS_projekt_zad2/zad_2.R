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