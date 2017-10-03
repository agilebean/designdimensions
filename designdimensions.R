#################################################################################################
library(NeuralNetTools)

# create model
library(neuralnet)
AND <- c(rep(0, 7), 1)
OR <- c(0, rep(1, 7))
binary_data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0, 1)), AND, OR)
binary_data
mod <- neuralnet(AND + OR ~ Var1 + Var2 + Var3, binary_data,
                 hidden = c(6, 12, 8), rep = 10, err.fct = 'ce', linear.output = FALSE)

# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(mod, alpha = 0.6)

#################################################################################################

# EXAMPLE MASS:Boston data
# SOURCE https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
#
set.seed(500)
library(MASS)
data <- Boston

# check for missing data
apply(data,2,function(x) sum(is.na(x)))

# sample(x, size) := create training set randomly from 75% (size) of the data indices (1:nrow)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
# use the randomly drawn indices to define training set
train <- data[index,]
test <- data[-index,]

# linear regression
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)

# MSE = mean squared error to evaluate regression
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

# data prep
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]


library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

plot(nn)

# PREDICTION

pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

par("mar")
par(mfrow=c(1,1))
# more intuitive plot
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

###################################################################################################

library(neuralnet)
data(iris)

ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))


trainset$setosa = trainset$Species == "setosa"
trainset$virginica = trainset$Species == "virginica"
trainset$versicolor = trainset$Species == "versicolor"

trainset = iris[ind == 1,]> testset = iris[ind == 2,]


###################################################################################################
library(ISLR)
names(College)

data <- College[,-1]
maxs <- data %>% apply(., 2, max)
mins <- data %>% apply(., 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(data,center = mins, scale = maxs - mins))
scaled.data
# Check out results
print(head(scaled.data,2))

Private = as.numeric(College$Private)-1
data = cbind(Private,scaled.data)

library(caTools)
set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

names(train)

features <- names(scaled.data)

features

# Concatenate strings
f <- features %>% paste (., collapse=" + ") %>% paste("Private ~ ", .) %>% 
  as.formula(env= .GlobalEnv)
f

library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

# Compute Predictions off Test Set
predicted.values <- compute(nn,test[-1])

# Check out net.result
print(head(predicted.values$net.result))

# round
predicted.values$net.result <- sapply(predicted.values$net.result,round,digits=0)

test$Private
predicted.values$net.result

table(test$Private,predicted.values$net.result)

plot(nn)
