library(dplyr)
library(ggplot2)

prod = read.csv2('Prod.csv')
trans = read.csv2('Trans.csv')

trans$weight <- gsub("\\.", "", trans[ , 4])
trans$weight <- as.numeric(trans$weight)

ggplot(trans, aes(x = reorder(Spediteur, weight, median), y = weight)) + geom_boxplot() + xlab('Spediteure')

ylim1 = boxplot.stats(trans$weight)$stats[c(1, 5)]
ggplot(trans, aes(x = reorder(Spediteur, weight, median), y = weight)) + geom_boxplot()  + coord_cartesian(ylim = ylim1*1.5)
ggplot(trans, aes(x = reorder(Spediteur, weight, median), y = weight, fill = Anlieferwerk)) + geom_boxplot()  + coord_cartesian(ylim = ylim1*1.5)

sped20 <- trans[trans$Spediteur == 'Sped20', 'weight']

# OUTLISER FOR SPED20
q <- quantile(sped20)
IQR <- q[3] - q[1]
outliers <- sped20 >= q[3] + IQR *1.5
trans[trans$Spediteur == 'Sped20', ][outliers, ]


boxplot(prod[, 2:4])

ggplot(trans, aes(x = reorder(Spediteur, weight, sum), y = weight, fill = Anlieferwerk)) + geom_col()#  + coord_cartesian(ylim = ylim1*1.5)


train <- read.csv('Train.csv')

boxplot(train[, 3:5])
boxplot(train[, 6:8])


ggplot(train, aes(x = Werk_1, y = Anlieferung_Werk_1)) + geom_point(size = 5, color = 'brown')
ggplot(train, aes(x = Werk_2, y = Anlieferung_Werk_2)) + geom_point(size = 5, color = 'brown')
ggplot(train, aes(x = Werk_3, y = Anlieferung_Werk_3)) + geom_point(size = 5, color = 'brown')
