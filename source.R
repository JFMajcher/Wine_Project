wine.ds <- read.csv2("winequality-white.csv", header=TRUE, sep = ";", dec =".")
str(wine.ds)
colMeans(wine.ds)
quants <- c(0.25,0.50,0.75)
apply(wine.ds , 2 , quantile , probs = quants , na.rm = TRUE)
apply(wine.ds, 2, FUN = IQR)
apply(wine.ds, 2, FUN = var)
apply(wine.ds, 2, FUN = sd)
install.packages("e1071")
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

install.packages("xlsx")
library("xlsx")
write.xlsx(wine.stats, "wine_stats.xlsx")

splitSample <- sample(1:3, size=nrow(wine.ds), prob=c(0.7,0.15,0.15), replace = TRUE)
train.wine <- wine.ds[splitSample==1,]
valid.wine <- wine.ds[splitSample==2,]
test.wine <- wine.ds[splitSample==3,]
