install.packages('nortest')
carFullData = 
  read.csv("D:\\Statistics\\cars_dataset.csv", header=TRUE, sep=",")

carData = carFullData[,c(6,7,8)]
colnames(carData) = c("fuelType","tax","mpg") 

# making easier to read variables
fuelTypes=carData$fuelType
fuelTypes_without_electric <- fuelTypes[which(fuelTypes!="Electric", TRUE)]
tax=carData$tax
mpg=carData$mpg
mpg_without_electric<-mpg[which(fuelTypes!="Electric", TRUE)]

# 2. Изследване на променливите поотделно
# 2.1. разпределение на колите според вида двигател

par(mfrow = c(1,2))
table_fuels <- table(fuelTypes)
percents <- round(100*table_fuels / sum(table_fuels), 1)
colors <- c("#E77471", "#C2DFFF", "#FFA07A", "#9FE2BF","#DFFF00")

barplot(table_fuels, names.arg = c("Дизел", "Елекрически","Хибрид", "Други","Бензин"), col = "#F70D1A")

pie(x = table_fuels, main = "Видове двигатели", labels = percents, col = colors)
legend(x = "bottomright", legend = c("Дизел", "Елекрически","Хибрид", "Други","Бензин"), cex = 1, fill = colors)
par(mfrow = c(1, 1))

# 2.2. данък, числова непрекъсната
par(mfrow = c(1, 2))
# хистограма
hist(tax, main = "вероятностно разпределение", 
     xlab = "британски лири", ylab = "честота",
     col = "#C5908E", prob = TRUE, ylim = c(0, 0.02))

# boxplot
boxplot(tax, main = "данък", ylab = "британски лири", col = "#FFDB58")
par(mfrow = c(1, 1))

# qqplot
set.seed(9504)
tax_normal_distrib <- rnorm(n = 1000, mean = mean(tax), sd = sd(tax))

qqplot(tax, tax_normal_distrib, main = "данък", 
       xlab = "реални стойности", ylab = "теоретично нормално разпределение")
abline(a = 0, b = 1)

# ниво на съгласие
alpha <- 0.05

# тест за нормално разпределение
shapiro.test(tax) 

#Anderson-Darling normality test
library(nortest)
ad.test(tax)$p.value

# локация
round(mean(tax), 3)
# 116.953
# дисперсия
round(sd(tax), 3)
# 64.046

# 2.3. mpg, числова непрекъсната
par(mfrow = c(1, 2))
# хистограма
hist(mpg, main = "вероятностно разпределение", 
     xlab = "изминати мили за един галон", ylab = "честота",
     col = "#6AFB92", prob = TRUE, ylim = c(0, 0.05))

# boxplot
boxplot(mpg, main = "mpg", ylab = "литри", col = "#FFA62F")
par(mfrow = c(1, 1))

# qqplot
set.seed(734)
mpg_normal_distrib <- rnorm(n = 1000, mean = mean(mpg), sd = sd(mpg))

qqplot(mpg, mpg_normal_distrib, main = "mpg", 
       xlab = "реални стойности", ylab = "теоретичното нормално разпределение")
abline(a = 0, b = 1)

#Anderson-Darling normality test
library(nortest)
ad.test(mpg)$p.value

# локация
round(median(mpg), 3)
# 55.4
# дисперсия
round(mad(mpg), 3)
# 11.119

# 3. Изследване на взаимодействия между променливите
# 3.1. категорийни обясняващи и числови зависими
# 3.1.1. вид двигател и данък 
boxplot(tax ~ fuelTypes,  xlab = "вид двигател", ylab = "данък", col = "#8C001A")
Petrol_tax <- tax[which(fuelTypes == 'Petrol')]
Diesel_tax <- tax[which(fuelTypes == 'Diesel')]
Hybrid_tax <- tax[which(fuelTypes == 'Hybrid')]
Electric_tax <- tax[which(fuelTypes == 'Electric')]
Other_tax <- tax[which(fuelTypes == 'Other')]

par(mfrow = c(1, 2))
hist(Petrol_tax, main = "", xlab = "данък в лири за Бензин", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Diesel_tax, main = "", xlab = "данък в лири за Дизел", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Hybrid_tax, main = "", xlab = "данък в лири за Хибрид", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Electric_tax, main = "", xlab = "данък в лири за Елекрически", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Other_tax, main = "", xlab = "данък в лири за Други", ylab = "честота", col = "#E55451", prob = TRUE)

par(mfrow = c(1, 1))

#Anderson-Darling normality test
ad.test(Petrol_tax)
lillie.test(Petrol_tax) 

# 3.1.2.вид двигател и mpg
boxplot(mpg_without_electric ~ fuelTypes_without_electric, 
        xlab = "вид двигател", ylab = "mpg", col = "#728C00")
Petrol_mpg <- mpg[which(fuelTypes == 'Petrol')]
Diesel_mpg <- mpg[which(fuelTypes == 'Diesel')]
Hybrid_mpg <- mpg[which(fuelTypes == 'Hybrid')]
Other_mpg <- mpg[which(fuelTypes == 'Other')]

par(mfrow = c(1, 2))
hist(Petrol_mpg, main = "", xlab = "мили изминати с един галон Бензин", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Diesel_mpg, main = "", xlab = "мили изминати с един галон Дизел", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Hybrid_mpg, main = "", xlab = "мили изминати с един галон гориво при Хибрид", ylab = "честота", col = "#E55451", prob = TRUE)
hist(Other_mpg, main = "", xlab = "мили изминати с един галон гориво при Други", ylab = "честота", col = "#E55451", prob = TRUE)

par(mfrow = c(1, 1))

#Anderson-Darling normality test
ad.test(Petrol_mpg)

# 3.2. числови обясняващи и числови зависими
# 3.2.2 данък и mpg
Petrol_tax <- tax[which(fuelTypes == 'Petrol')]
Diesel_tax <- tax[which(fuelTypes == 'Diesel')]
Hybrid_tax <- tax[which(fuelTypes == 'Hybrid')]
Electric_tax <- tax[which(fuelTypes == 'Electric')]
Other_tax <- tax[which(fuelTypes == 'Other')]

Petrol_mpg <- mpg[which(fuelTypes == 'Petrol')]
Diesel_mpg <- mpg[which(fuelTypes == 'Diesel')]
Hybrid_mpg <- mpg[which(fuelTypes == 'Hybrid')]
Electric_mpg <- mpg[which(fuelTypes == 'Electric')]
Other_mpg <- mpg[which(fuelTypes == 'Other')]



plot(tax, mpg, xlab = "данък в лири", ylab = "мили изминати с един галон")
abline(a = 80, b = 5, lwd = 1)

plot(Petrol_tax, Petrol_mpg, xlab = "данък в лири", ylab = "мили изминати с един галон Бензин")
abline(a = 80, b = 5, lwd = 1)

plot(Electric_tax, Electric_mpg, xlab = "данък в лири", ylab = "мили изминати с Електричество с едно зареждане")
abline(a = 80, b = 5, lwd = 1)

plot(Diesel_tax, Diesel_mpg, xlab = "данък в лири", ylab = "мили изминати с един галон Дизел")
abline(a = 80, b = 5, lwd = 1)

plot(Other_tax, Other_mpg, xlab = "данък в лири", ylab = "мили изминати с един галон Други горива")
abline(a = 80, b = 5, lwd = 1)

plot(Hybrid_tax, Hybrid_mpg, xlab = "данък в лири", ylab = "мили изминати с един галон горива при Хибрид")
abline(a = 80, b = 5, lwd = 1)

# корелация
rho <- round(cor(tax, mpg, method = "spearman"), 3)
rho_Petrol <- round(cor(Petrol_tax, Petrol_mpg, method = "spearman"), digits = 3)
rho_Electric <- round(cor(Electric_tax, Electric_mpg, method = "spearman"), digits = 3)

abs(rho) 
abs(rho_Petrol) 
abs(rho_Electric) 
