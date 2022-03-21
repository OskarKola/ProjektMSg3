# modele regresji liniowej przedstawiające zależności wysokości
# zebranych plonów z hektara w tonach od ilości użytego nawozu

setwd("~\MS_projekt_zad2") #set working directory
data <- read.delim("dane3.txt") #LOAD DATA
names(data) <- c('wyd', 'nawoz', 'kraj') #change column names

# Ilość zboża a ilość nawozu

### POLAND ###
poland = data[data$kraj ==1, ]
train_poland.idx <- sample(1:nrow(poland), 0.7*nrow(poland), replace = FALSE)
train_poland<-poland[train_poland.idx, ]
test_poland<-poland[-train_poland.idx, ]

nrow(train_poland)
nrow(test_poland)
## wydajność~nawóz

model_poland<-lm(wyd~nawoz, data = train_poland)
summary(model_poland)
### HUNGARY ###
hungary = data[data$kraj ==0, ]
train_hungary.idx <- sample(1:nrow(hungary), 0.7*nrow(hungary), replace = FALSE)
train_hungary<-hungary[train_hungary.idx, ]
test_hungary<-hungary[-train_hungary.idx, ]

nrow(train_hungary)
nrow(test_hungary)

model_hungary<-lm(wyd~nawoz, data = train_hungary)
summary(model_hungary)

### POLAND & HUNGARY ###
train.idx <- sample(1:nrow(data), 0.7*nrow(data), replace = FALSE)
train<-data[train.idx, ]
test<-data[-train.idx, ]

nrow(train)
nrow(test)

model<-lm(wyd ~ nawoz, data = data)
summary(model)

