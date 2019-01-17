wine.ds <- read.csv2("winequality-white.csv", header=TRUE, sep = ";", dec =".")
str(wine.ds)
colMeans(wine.ds)
quants <- c(0.25,0.50,0.75)
apply(wine.ds , 2 , quantile , probs = quants , na.rm = TRUE)
apply(wine.ds, 2, FUN = IQR)
apply(wine.ds, 2, FUN = var)
apply(wine.ds, 2, FUN = sd)
# install.packages("e1071")
library(e1071)
apply(wine.ds, 2, FUN = skewness)
apply(wine.ds, 2, FUN = kurtosis)
wine.stats <- data.frame(matrix(ncol = 12, nrow = 9))
x <- c("Ustalona_kwasowosc", "Lotna_kwasowosc", "Kwas cytrynowy", "Cukier", "Chlorki", "Lotny_SO2", "Calkowity_SO2", "Gestosc", "pH","Siarczany", "Alkohol", "Jakosc")
colnames(wine.stats) <- x
y <- c("Srednia", "Q1", "Q2", "Q3", "IQR", "Wariancja", "Odchylenie standardowe", "Skosnosc", "Kurtoza")
rownames(wine.stats) <- y
wine.stats[1,] <- colMeans(wine.ds)
wine.stats[2,] <- apply(wine.ds , 2 , quantile , probs = 0.25 , na.rm = TRUE)
wine.stats[3,] <- apply(wine.ds , 2 , quantile , probs = 0.5 , na.rm = TRUE)
wine.stats[4,] <- apply(wine.ds , 2 , quantile , probs = 0.75 , na.rm = TRUE)
wine.stats[5,] <- apply(wine.ds, 2, FUN = IQR)
wine.stats[6,] <- apply(wine.ds, 2, FUN = var)
wine.stats[7,] <- apply(wine.ds, 2, FUN = sd)
wine.stats[8,] <- apply(wine.ds, 2, FUN = skewness)
wine.stats[9,] <- apply(wine.ds, 2, FUN = kurtosis)

summary(wine.ds)
head(wine.ds)
pairs(wine.ds, col = wine.ds$quality)

wine39 = subset(wine.ds, subset=(wine.ds$quality == "3" | wine.ds$quality == "8"| wine.ds$quality == "9") & wine.ds$chlorides < 0.15)
pairs(wine39, col = wine39$quality)

boxplot(wine.ds$fixed.acidity ~ wine.ds$quality)
boxplot(wine.ds$volatile.acidity ~ wine.ds$quality)
boxplot(wine.ds$citric.acid ~ wine.ds$quality)
boxplot(wine.ds$residual.sugar ~ wine.ds$quality)
boxplot(wine.ds$chlorides ~ wine.ds$quality)
boxplot(wine.ds$free.sulfur.dioxide ~ wine.ds$quality)
boxplot(wine.ds$total.sulfur.dioxide ~ wine.ds$quality)
boxplot(wine.ds$density ~ wine.ds$quality)
boxplot(wine.ds$pH ~ wine.ds$quality)
boxplot(wine.ds$sulphates ~ wine.ds$quality)
boxplot(wine.ds$alcohol ~ wine.ds$quality)

# install.packages("xlsx")
library("xlsx")
write.xlsx(wine.stats, "wine_stats.xlsx")

splitSample <- sample(1:3, size=nrow(wine.ds), prob=c(0.7,0.15,0.15), replace = TRUE)
train.wine <- wine.ds[splitSample==1,]
valid.wine <- wine.ds[splitSample==2,]
test.wine <- wine.ds[splitSample==3,]



train.wine$quality <- as.factor(train.wine$quality)
valid.wine$quality <- as.factor(valid.wine$quality)
test.wine$quality <- as.factor(test.wine$quality)

minmax.wine <- wine.ds
for (i in 1:nrow(wine.ds)){
  for(j in 1:ncol(wine.ds)){
    minmax.wine[i,j] <- (wine.ds[i,j] - min(wine.ds[,j]))/(max(wine.ds[,j]-min(wine.ds[,j]))) 
  }
}
minmax.wine$quality <- as.factor(wine.ds$quality)
splitSample.minmax <- sample(1:3, size=nrow(minmax.wine), prob=c(0.7,0.15,0.15), replace = TRUE)
train.minmax <- minmax.wine[splitSample==1,]
valid.minmax <- minmax.wine[splitSample==2,]
test.minmax <- minmax.wine[splitSample==3,]

tree1 <- rpart(formula = quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,
              data = train.wine, 
              method = "class"
)
summary(tree1)
plot(tree1)
text(tree1)

tablica <- table(predict(tree1, train.wine, type="class"), train.wine$quality)
tablica <- table(predict(tree2, train.minmax, type="class"), train.minmax$quality)
tablica <- table(predict(tree1, test.wine, type="class"), test.wine$quality)
tablica <- table(predict(tree1, test.minmax, type="class"), test.minmax$quality)
tablica
sum(tablica)-sum(diag(tablica))
sum(diag(tablica)) / sum(tablica)
tree2 <- rpart(formula = quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,
              data = train.minmax, 
              method = "class"
)
plot(tree2)
text(tree2)
#train.wine$quality <- as.numeric(train.wine$quality)
# wine.glm<-lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=train.wine)
# summary(wine.glm)
# plot(wine.glm)

wine9train = subset(train.wine, subset = train.wine$quality == 7)
ideal.wine <-  colMeans(wine9train)
ideal.wine

wine.difs <- sweep(wine9train, 0, ideal.wine)
# wine.difs <- apply(wine9train, FUN = function(x) x-ideal.wine, MARGIN = [,12])


library(rpart)
tree <- rpart(formula = quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,
              data = train.wine, 
              method = "class",
              na.action=na.pass, 
              parms = list(split=c("information","gini")), 
              # control = rpart.control(usersurrogate = 0, maxsurrogate = 0, cp = 0.00075)
              control = rpart.control(cp = 0.00075),
              maxdepth = 10
              )
# tree
summary(tree)

tablica <- table(predict(tree, train.wine, type="class"), train.wine$quality)
tablica
sum(tablica)-sum(diag(tablica))
sum(diag(tablica)) / sum(tablica)


tablica <- table(predict(tree, valid.wine, type="class"), valid.wine$quality)
tablica
# sum(tablica)-sum(diag(tablica))
sum(diag(tablica)) / sum(tablica)

# X11()
# plot(tree)

# library(maptree)
# draw.tree(tree)
