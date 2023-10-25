library(readxl)
library(ggplot2)

# 1. uzd.

diabetes_data <- read_excel("homeworks\\data\\HW1\\diabetes.xlsx")

# Datus veidi/tipi
str(diabetes_data)

# Aprakstošā statistika
summary(diabetes_data)

# Kastu grafiks par Augumu
kastu_grafiks_augums <- ggplot(diabetes_data, aes(y=`Augums, m`)) + 
  geom_boxplot() + 
  ggtitle("Auguma kastu grafiks") + 
  ylab("Augums (m)")
print(kastu_grafiks_augums)

# Kastu grafiks par Svaru
kastu_grafiks_svars <- ggplot(diabetes_data, aes(y=`Svars, kg`)) + 
  geom_boxplot() + 
  ggtitle("Svara kastu grafiks") + 
  ylab("Svars (kg)")
print(kastu_grafiks_svars)

# Izkliedes grafiks starp Augumu un Svaru
scatter_augums_svars <- ggplot(diabetes_data, aes(x=`Augums, m`, y=`Svars, kg`)) + 
  geom_point() +
  ggtitle("Izkliedes grafiks: Augums") + 
  xlab("Augums (m)") + ylab("Svars (kg)")
print(scatter_augums_svars)

# Izkliedes grafiks starp KMI un Svaru ar regresijas līniju
scatter_KMI_svars <- ggplot(diabetes_data, aes(x=KMI, y=`Svars, kg`)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, color="blue") +
  ggtitle("Izkliedes grafiks: KMI vs. Svars ar regresijas līniju") + 
  xlab("KMI") + ylab("Svars (kg)")
print(scatter_KMI_svars)

# Stabiņu diogramma par dzimumu skaitu
barplot_dzimums <- ggplot(diabetes_data, aes(x=Dzimums)) + 
  geom_bar() + 
  ggtitle("Stabiņu diagramma: Dzimumu skaits") + 
  xlab("Dzimums") + ylab("Skaits")
print(barplot_dzimums)

# Histogramma par Augumu
hist_augums <- ggplot(diabetes_data, aes(x=`Augums, m`)) + 
  geom_histogram(aes(y=..density..), binwidth=0.05, fill="blue", color="black", alpha=0.7) + 
  ggtitle("Histogramma: Augums") + 
  xlab("Augums (m)") + ylab("Biežums")
print(hist_augums)

# Histogramma par Svaru ar sadalījuma līkni
hist_svars <- ggplot(diabetes_data, aes(x=`Svars, kg`)) + 
  geom_histogram(aes(y=..density..), binwidth=5, fill="blue", color="black", alpha=0.7) + 
  geom_density(color="red", size=1.5) +
  ggtitle("Histogramma: Svars ar sadalījuma līkni") + 
  xlab("Svars (kg)") + ylab("Biežums")
print(hist_svars)

# QQ grafiks par Augumu
p8 <- ggplot(diabetes_data, aes(sample=`Augums, m`)) + 
  geom_qq() + 
  ggtitle("QQ grafiks: Augums") + 
  xlab("Teorētiskie kvantiļi") + ylab("Parauga kvantiļi")
print(p8)

# QQ grafiks par Svaru
qq_svars <- ggplot(diabetes_data, aes(sample=`Svars, kg`)) + 
  geom_qq() + 
  ggtitle("QQ grafiks: Svars") + 
  xlab("Teorētiskie kvantiļi") + ylab("Parauga kvantiļi")
print(qq_svars)



# Histogramma Gurni ar normālo sadalījumu
hist_gurni_norm <- ggplot(diabetes_data, aes(x=`Gurni, cm`)) + 
  geom_histogram(aes(y=..density..), binwidth=2, fill="blue", color="black", alpha=0.7) + 
  stat_function(fun=dnorm, args=list(mean=mean(diabetes_data$`Gurni, cm`, na.rm=TRUE), 
                                     sd=sd(diabetes_data$`Gurni, cm`, na.rm=TRUE)), 
                color="red", size=1.5) +
  ggtitle("Histogramma: Gurni ar normālo sadalījumu") + 
  xlab("Gurni (cm)") + ylab("Biežums")
print(hist_gurni_norm)

# Histogramma Viduklis ar students-T sadalījumu
hist_viduklis_st <- ggplot(diabetes_data, aes(x=`Viduklis, cm`)) + 
  geom_histogram(aes(y=..density..), binwidth=2, fill="blue", color="black", alpha=0.7) + 
  stat_function(fun=function(x) dt((x - mean(diabetes_data$`Viduklis, cm`, na.rm=TRUE)) 
                / sd(diabetes_data$`Viduklis, cm`, na.rm=TRUE), df=107) 
                / sd(diabetes_data$`Viduklis, cm`, na.rm=TRUE), 
                color="red", size=1.5) +
  ggtitle("Histogramma: Viduklis ar students-T sadalījumu") + 
  xlab("Viduklis, cm") + ylab("Biežums")
print(hist_viduklis_st)

# Histogramma HbA1c ar eksponenciālo sadalījumu
hist_hba1c_exp <- ggplot(diabetes_data, aes(x=`HbA1c, %`)) + 
  geom_histogram(aes(y=..density..), fill="blue", color="black", alpha=0.7) + 
  stat_function(fun=dexp, args=list(rate=1/mean(diabetes_data$`HbA1c, %`, na.rm=TRUE)), 
                color="green", size=1.5) +
  ggtitle("Histogramma: HbA1c ar eksponenciālo sadalījumu") + 
  xlab("HbA1c, %") + ylab("Biežums")
print(hist_hba1c_exp)



# 2. uzd.
library(caret)
library(MASS)

# Ielādējam datus
exam_data <- read_excel("homeworks\\data\\HW1\\2021_visi_kopa_1.xlsx")

# Notīram rindiņas, kurās ir vārds "atbr."
exam_data <- exam_data[!apply(exam_data, 1, function(row) any(grepl("atbr.", row))), ]

set.seed(777)

# Sajaucam datus
exam_data <- exam_data[sample(nrow(exam_data)), ]

# Uzstādam eksāmenu tipu kā 1 vai 0 (1 - matemātikas eksāmens, 0 - citi eksāmeni)
exam_data$IsMath <- ifelse(exam_data$Parbaudijums == "mat", 1, 0)

# Pārveidojam kategoriju mainīgas par faktoriem
exam_data$dzimums <- as.factor(exam_data$dzimums)
exam_data$IsMath <- as.factor(exam_data$IsMath)

# Sadalam datus treniņu un testēšanas kopās
trainIndex <- createDataPartition(exam_data$IsMath, p = .8, list = FALSE, times = 1)

trainData <- exam_data[trainIndex, ]
testData  <- exam_data[-trainIndex, ]

# LDA
lda_model <- lda(IsMath ~ Procenti + dzimums, data=trainData)

# Veic prognozes izmantojot testēšanas datus
lda_pred <- predict(lda_model, testData)

# Nodrošinām, ka prognozēšanas un faktiskajiem testa datiem ir vienādi līmeņi
levels(lda_pred$class) <- levels(testData$IsMath)

# Izveido sajaukšanas matricu
confusion <- confusionMatrix(lda_pred$class, testData$IsMath)
print(confusion)

# Aprēķina precizitāti
accuracy <- sum(diag(confusion$table)) / sum(confusion$table)
print(paste("Precizitāte:", round(accuracy, 3)))



# Skaitīt cik reizes katra skola parādās datu kopā
skolu_skaititajs <- table(exam_data$IzglītībasIestāde)

# Izvēlēties lielākās skolas (piemēram, top 10)
lielakas_skolas <- names(skolu_skaititajs[order(skolu_skaititajs, decreasing = TRUE)][1:10])

# Izveidot jaunu mainīgo par skolu klasi
exam_data$SkolaKlase <- ifelse(exam_data$IzglītībasIestāde %in% lielakas_skolas, exam_data$IzglītībasIestāde, 'Citas')

# Pārveidojam kategoriju mainīgas par faktoriem
exam_data$SkolaKlase <- as.factor(exam_data$SkolaKlase)

# Sadalam datus treniņu un testēšanas kopās
set.seed(777)
trainIndex <- createDataPartition(exam_data$SkolaKlase, p = .8, list = FALSE, times = 1)
trainData <- exam_data[trainIndex, ]
testData  <- exam_data[-trainIndex, ]

# Veicam LDA izmantojot izvēlētās kolonnas kā ieejas datus
lda_model <- lda(SkolaKlase ~ IsMath + Procenti + dzimums, data=trainData)

# Nodrošinām, ka prognozes klasēm ir tādi paši līmeņi kā testa datiem
lda_pred$class <- factor(lda_pred$class, levels = levels(trainData$SkolaKlase))

# Veicam prognozes izmantojot testēšanas datus
lda_pred <- predict(lda_model, testData)

# Izveidojam sajaukšanas matricu un aprēķinājam precizitāti
confusion <- confusionMatrix(lda_pred$class, testData$SkolaKlase)
print(confusion)

accuracy <- sum(diag(confusion$table)) / sum(confusion$table)
print(paste("Precizitāte:", round(accuracy, 3)))


library(e1071)

# Sadalam datus treniņu un testēšanas kopās
trainIndex <- createDataPartition(exam_data$IsMath, p = .8, list = FALSE, times = 1)
trainData <- exam_data[trainIndex, ]
testData  <- exam_data[-trainIndex, ]

# Naive Bayes
nb_model <- naiveBayes(IsMath ~ dzimums + Punkti, data=trainData)
nb_pred <- predict(nb_model, testData)
confusion_nb <- confusionMatrix(nb_pred, testData$IsMath)
print(confusion_nb)



# 3. uzdevums
lambda_1 <- 3
lambda_2 <- 4
lambda <- lambda_1 + lambda_2

p_year <- sum(dpois(0:8, lambda))

lambda_half <- lambda / 2
p_half_year <- sum(dpois(0:8, lambda_half))

print(paste("Varbūtība gadā: ", p_year))
print(paste("Varbūtība pusgadā: ", p_half_year))


