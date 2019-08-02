# Install and load packages
install.packages("Hmisc"); library(Hmisc) # for describing data
install.packages("ggplot2"); library(ggplot2) # for visualization
install.packages("reshape2"); library(reshape2) # for melting dataframes
install.packages("stats"); library(stats) # for logistic regression tasks
install.packages("caret"); library(caret)
install.packages("dominanceanalysis"); library(dominanceanalysis)
install.packages("randomFores"); library(randomForest)

### PRELIMINARY ANALYSIS
# Load and summarize data
data <- read.csv("data.csv")
head(data)
data <- data[, -c(1)]

describe(data) # No missing values, variables are on the same scale
data$class_label <- as.factor(data$class_label) 

# Inter-variable correlation chart
cor(data[, 2:11]) # No significant inter-variable correlation

# Boxplots of sensor values grouped by class labels
mm = melt(data, id=c("class_label"))
ggplot(mm)+geom_boxplot(aes(x=variable, y=value))+facet_grid(.~class_label)
# There are many outliers in sensor0, sensor4 and sensor8


### MODELING AND RANKING
# Generate balanced train and test sets
train <- data[c(1:150, 201:350),]
test <- data[c(151:200, 351:400),]

# Logistic regression model
model.1 <- glm(class_label~., data = train, family = binomial)
summary(model.1)

# Overall p-value of the model
anova(model.1, update(model.1, ~1), test="Chisq") # >0.05, indicates a good fit
# Goodness of fit test, Chi-square value for 289 dof and alpha=0.05
qchisq(df=289,p=0.95) # > residual deviance, indicates a good fit

# Plot of standardized residuals
plot(fitted(model.1), rstandard(model.1))

# Evaluate on the test set
preds <- ifelse(predict(model.1, test)> 0, 1, -1)
accuracy <- sum(test$class_label == preds)/nrow(test)
# 95.0% accuracy

# Ranking based on the absolute value of the t-statistic of each variable
ranked.sensors <- sort(t(varImp(model.1))[1,], decreasing=TRUE)
names(ranked.sensors)

# Ranking based on dominance analysis
da.logreg <- dominanceAnalysis(model.1)
summary(da.logreg)

# Print sensor contributions to the model
sensor.contributions <- da.logreg$contribution.average
sensor.contributions

# Sensor contributions based on different R2 metrics
sort(sensor.contributions$r2.m, decreasing=TRUE)
sort(sensor.contributions$r2.cs, decreasing=TRUE)
sort(sensor.contributions$r2.n, decreasing=TRUE)
sort(sensor.contributions$r2.e, decreasing=TRUE)

ranked.sensors.da <- sort(sensor.contributions$r2.m, decreasing=TRUE)
names(ranked.sensors.da)


# Random Forest model - ranking based on mean decrease in Gini index
set.seed(11)
model.2 <- randomForest(class_label~.,data=data, ntree=500)
model.2

# Variable importance plot based on mean decrease in Gini index
varImpPlot(model.2)
sensor.importance <- t(importance(model.2))[1,]
sort(sensor.importance, decreasing=TRUE)
