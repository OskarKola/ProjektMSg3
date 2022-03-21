
setwd("~/Documents/MS_projekt") #set working directory
data <- read.delim("dane3.txt") #LOAD DATA
names(data) <- c('wyd', 'nawoz', 'kraj') #change column names
##########################################################

### POLAND #####
#subselect column with value "polska ==1"
polska = data[data$kraj ==1, ]
#mean value
pl_mean = mean(polska$wyd)

# quartiles
pl_quartiles = quantile(polska$wyd)# apply the quantile function 
#   0%   25%   50% (median)  75%  100% 
# 29.07 46.43    56.40       63.83 80.28 

#standard deviation
pl_sd = sd(polska$wyd)

### HUNGARY #####
#subselect column with value "hungary ==0"
hungary = data[data$kraj ==0, ]
#mean value
hu_mean = mean(hungary$wyd)

# quartiles
hu_quartiles = quantile(hungary$wyd)# apply the quantile function 
# 0%   25%   50%   75%  100% 
# 37.89 49.51 55.99 60.64 85.95 

#standard deviation
hu_sd = sd(hungary$wyd)





