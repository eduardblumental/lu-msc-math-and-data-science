library(readxl)
library(ggplot2)

# Izlasām datus no Excel faila
diabetes_data <- read_excel("homeworks\\data\\HW1\\diabetes.xlsx")

# Aprakstīt datus, kādi ir datu veidi/tipi
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





