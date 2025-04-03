#############################################################
# Examen de Analisis de Datos
############################################################

(5 > 3) & (4 == 2 + 2) | (7 < 6)


x <- c(2, 4, 6, 8, 10)
x[x%%4==0]

10%%2

# Pregunta N° 8


primas <- c(5, 7, 8, 12, 14, 18, 19, 21, 21, 22, 24, 25, 26, 30, 35, 50, 100)
boxplot(primas)

#Pregunta N° 9

primas <- c(100, 50, 75, 120, 85, 110, 60, 55, 130, 95, 135, 150, 200,
            165, 175, 220, 80, 90, 45, 160, 125, 140, 180, 210, 
            140, 170, 125, 55, 115, 95, 165, 190, 115, 75, 100,
            145, 160, 130, 150, 70, 120, 110, 130, 80, 200, 50, 
            160, 210, 180, 160)

#Criterio del 5% elimina 2.5% a cada lado
q2.5 <- quantile(primas,0.025)
q97.5 <- quantile(primas,0.975)
outl_p <- primas[c(primas<q2.5 | primas>q97.5)]
primas[c(19,16)]


#Pregunta N° 10

charges <- c(200, 250, 300, 50, 180, 220, 300, 280, 310, 400,
             450, 370, 330, 310, 360, 290, 250, 230, 270, 320, 
             410, 370, 340, 500, 550, 480, 600, 450, 300, 290, 
             430, 470, 380, 350, 410, 420, 460, 390, 320, 330, 
             350, 340, 380, 370, 420, 510, 500, 530, 600, 570, 
             910)

#Primero debemos normalizar la data

charges_no <- (charges - mean(charges))/sd(charges)
out_nor <- charges_no[charges_no> -3 & charges_no< 3]

#identificar el valor eliminado
eliminado <- which(charges_no<= -3 | charges_no>= 3)
charges[eliminado]


# Pregunta N° 11

library(openxlsx)
imput <- read.xlsx(file.choose())

summary(imput)
str(imput)


imput_complete= imput[complete.cases(imput),]
imput_incomplete= imput[!complete.cases(imput),]


lm_1 <- lm(RiskScore~ Claims+BMI +Age+ Income, data = imput_complete)
lm_2 <- lm(RiskScore~ Age+ Copay + Income, data = imput_complete)
lm_3 <- lm(RiskScore~ Claims+BMI +Age, data = imput_complete)
lm_4 <- lm(RiskScore~ BMI +Age+ Copay + Income, data = imput_complete)

# Comparar modelos

comparacion <- data.frame(
  Model = c("lm_1", "lm_2", "lm_3", "lm_4"),
  R2_Adjusted = c(summary(lm_1)$adj.r.squared, 
                  summary(lm_2)$adj.r.squared, 
                  summary(lm_3)$adj.r.squared, 
                  summary(lm_4)$adj.r.squared),
  AIC = c(AIC(lm_1), AIC(lm_2), AIC(lm_3), AIC(lm_4)),
  BIC = c(BIC(lm_1), BIC(lm_2), BIC(lm_3), BIC(lm_4)),
  Residual_Std_Error = c(summary(lm_1)$sigma, 
                         summary(lm_2)$sigma, 
                         summary(lm_3)$sigma, 
                         summary(lm_4)$sigma))
comparacion

#Pregunta N° 12

round(summary(lm_4)$adj.r.squared,3)


#Pregunta N° 13
lm_4
names(imput_incomplete)
datlm_4 <- imput_incomplete[,-c(1,6)]

imput_incomplete$RiskScore[which(is.na(imput_incomplete$RiskScore))
                           ] <- predict(lm_4,datlm_4)

imput2 <- rbind(imput_complete,imput_incomplete)

round(mean(imput$RiskScore, na.rm=TRUE),2) ;round(mean(imput2$RiskScore),2)

#Pregunta N° 15

imput <- read.xlsx(file.choose())

# summary(imput)
# str(imput)

imput$Gender <- as.factor(imput$Gender)
imput$Smoker <- as.factor(imput$Smoker)
imput$Region <- as.factor(imput$Region)
imput$PolicyType <- as.factor(imput$PolicyType)
imput$UsedInsurance <- as.factor(imput$UsedInsurance)
names(imput)
imput_cuant <- imput[,-c(2,4,7,8,9,13)]

library(ggplot2)
library(plotly)

# Matriz de correlaciones

cor(imput_cuant)

# Pregunta N°16
# modelo1

modelo1 <- lm(BasePremium ~ Age + Deductible + Copay 
              + CoverageAmount + Charges + RiskScore, data=imput_cuant)
 
summary(modelo1) # al parecer hay colinealidad perfecta
# entre la variable Monto de cobertura y otras variables

#Pregunta N°17

# Eliminamos de la base, BasePremium
# names(imput_cuant)
d_pca <- imput_cuant[,-c(14)]
d_pca_esc= scale(d_pca)
# se crea PCA
pca <- prcomp(d_pca_esc)
summary(pca)  
# Elegimos 5 componentes principales , por la varianza acumulada en PC5 (74.5%)


# Pregunta N° 18

# Analizamos los componentes de cada PCA

pca$rotation
#¿Que variables tiene las correlaciones mas altas dentro de cada PC?
#PC1  Copago, Deducible, Monto de cobertura, Prima (Seguros y Deducibles)
#PC2 BMI, Gastos Medicos,Reclamo Previo, Score de Riesgo (Riesgo Medico)
#PC3 Reclamos, Prima (Monto de Indemnizacion)

# Pregunta N° 19

pca_data <- as.data.frame(pca$x)
pca_data$BasePremium <- imput_cuant$BasePremium 

#Regresion con todos los componentes principales

# reg <- as.formula(paste("BasePremium ~",
#                         paste(paste0("PC",1:13),
#                               collapse= "+"))) 
reg_pca <- lm(BasePremium ~ ., data=pca_data)

resul_mod <- summary(reg_pca)
sig_pca <- resul_mod$coefficients[resul_mod$coefficients[,4]<0.05,] 

#Pregunta N° 20

#Regresion sobre los 5 pca

reg_pca2 <- lm(BasePremium ~ PC1 +PC2 +PC3 +PC4 +PC5, data=pca_data)
resul_mod2 <- summary(reg_pca2)
round(resul_mod2$adj.r.squared,4)

#Pregunta N°21

reg_pca3 <- lm(BasePremium ~ Copay+ CoverageAmount+ Deductible+ 
               BMI+ PreviousClaims+  MedicalExpenses+
               RiskScore+ Claims+ Charges+ 
               Children+ Income+ Age, data=imput_cuant )

summary(reg_pca3)
  
#Pregunta N° 22
#Correr regresion solo con variables significativas
resul_mod <- summary(reg_pca3)
sig_pca <- resul_mod$coefficients[resul_mod$coefficients[,4]<0.05,]
sig_pca <- row.names(sig_pca)

reg <- as.formula(paste("BasePremium ~",
                   paste(sig_pca,
                         collapse= "+"))) 

#regresion corregida
reg_pca4 <- lm(reg,data=imput_cuant)

summary(reg_pca4)
summary(modelo1)
resul_mod <- summary(modelo1)
resul_mod4 <- summary(reg_pca4)
round(resul_mod$adj.r.squared,4) ; round(resul_mod4$adj.r.squared,4) 

#Agregamos RiskScore al modelo 4, para ver si explica mejor

reg <- as.formula(paste("BasePremium ~",
                        paste(c(sig_pca, "RiskScore"),
                              collapse = " + ")))

reg_pca5 <- lm(reg,data=imput_cuant)
resul_mod5<- summary(reg_pca5) # RiskScore no es significativa para explicar

round(resul_mod$adj.r.squared,4) ; round(resul_mod4$adj.r.squared,4) ; round(resul_mod5$adj.r.squared,4)

#Pregunta N° 23

new_dat <- data.frame(Copay = 0.2,
                      CoverageAmount= 100000,
                      PreviousClaims= 4,
                      MedicalExpenses= 6365,
                      Claims= 0,
                      Charges= 3802,
                      Age=71)


predict(reg_pca4,new_dat)

###########################################################################
# Clusters
###########################################################################

rm(list=ls())
gc()               
cat("\014")      

dev.off() #por si no corre algo relacionado a graficas

library(openxlsx)
library(factoextra)

seguros= read.xlsx(file.choose())

seguros_scaled= scale(seguros)

# Usamos el método del codo
fviz_nbclust(seguros_scaled, kmeans, method = "wss") 

################USAR SEMILLA 80!! CORRER JUNTO
set.seed(80)
km_res <- kmeans(seguros_scaled, centers = 3, nstart = 50)

##################
# 4. Visualizar clusters
fviz_cluster(km_res, data = seguros_scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


# 5. Agregar clusters al dataframe original
km_res$cluster
seguros$Cluster= as.factor(km_res$cluster)



# Contar cuántos autos hay en cada cluster
table(seguros$Cluster)

# Calcular el promedio de cada variable por cluster
aggregate(seguros[, -which(names(seguros) == "Cluster")], 
          by = list(Cluster = seguros$Cluster), 
          FUN = mean)

# Comparar la dispersión dentro de cada cluster
aggregate(seguros[, -which(names(seguros) == "Cluster")], 
          by = list(Cluster = seguros$Cluster), 
          FUN = sd)

### Visualización de Clusters

library(tidyr)


as.factor(km_res$cluster)


seguro_scaled_dat=as.data.frame(seguros_scaled)
names(seguro_scaled_dat)
seguro_scaled_dat$Cluster= as.factor(km_res$cluster)

# Este código solo se cambia nombres de variables  desde:hasta (en este caso Age: BasePremium), dejando fuera la variable Clusters que incorporamos
data_long <- gather(seguro_scaled_dat, variable, valor, Age:BasePremium ,factor_key=TRUE)

ggplot(data_long, aes(as.factor(x = variable), y = valor,group=Cluster, colour = Cluster)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")+  guides(x = guide_axis(angle =45)) + labs(title = "Perfilamiento de Clusters",
                                                                       x = "Variables", y = "Nivel")


# Pregunta N° 24

# El primer codo se ve entre 3 y 4

#########################################################################
# Arbol de Decision
#########################################################################

rm(list=ls())
gc()               
dev.off()
cat("\014")


library(openxlsx)

seguros= read.xlsx(file.choose())

str(seguros)

library(rpart)
library(rpart.plot)  # Para visualizar el árbol


# Convertimos la variable objetivo en factor
seguros$Premium_type= as.factor(seguros$Premium_type)
# DEBEMOS ELIMINAR CLUSTERS!!!
seguros$Cluster= NULL

### CORRER JUNTO
set.seed(80)
index= sample(1:1000, size=800, replace= FALSE)

###

training= seguros[index,]
testing= seguros[-index,]
# Entrenamos el Árbol de Decisión
modelo_arbol <- rpart(Premium_type ~ ., data = training, method = "class")
# cuando ponemos variable~ . , el puntito implica que son TODAS las variables

# Visualizamos el árbol
rpart.plot(modelo_arbol, type = 3, extra = 101, box.palette = "RdYlGn")

# Hacemos predicciones
predicciones <- predict(modelo_arbol, testing, type = "class")

# Matriz de confusión
confusion_matrix <- table(Predicho = predicciones, Real = testing$Premium_type)
confusion_matrix

# Puede obtener estadísticos con la librería caret

# No puedo intalar caret, por que mi R es una version vieja
# y tengo que llamar a todo el BCP para cambiar (naaa)
# por eso, hago a mano


confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

p_o <- accuracy # precision observada
# p_e precision esperada
p_e <- sum((rowSums(confusion_matrix)*colSums(confusion_matrix))/(sum(confusion_matrix))^2) 

# a <- rowSums(confusion_matrix)
# b <- colSums(confusion_matrix)
# c <- sum(confusion_matrix)
# p_e <- sum((a*b)/c^2)

kappa <- (p_o-p_e)/(1-p_e)

# Pregunta N° 30
p_o  

#Pregunta N° 31
kappa

# Pregunta  N° 32

error <-  sum(confusion_matrix) - sum(diag(confusion_matrix)) 
error

#Pregunta N°32

#  Le corresponde la categoria Gold

########################################################################
# Reglas de Asociacion
#######################################################################

rm(list=ls())
gc()               
dev.off()
cat("\014")

# Instalar y cargar el paquete arules si no lo tienes instalado
library(arules)

library(openxlsx)
insurance_data= read.xlsx(file.choose())
str(insurance_data)
# Asegurarse de que las variables categóricas ya están como factores
insurance_data$Gender <- as.factor(insurance_data$Gender)
insurance_data$MaritalStatus <- as.factor(insurance_data$MaritalStatus)
insurance_data$PolicyType <- as.factor(insurance_data$PolicyType)
insurance_data$Region <- as.factor(insurance_data$Region)
insurance_data$UsedInsurance <- as.factor(insurance_data$UsedInsurance)

# Convertir el dataframe a un conjunto de transacciones (usando solo las variables categóricas)
trans_data <- as(insurance_data[, c("Gender", "MaritalStatus", "PolicyType", "Region", "UsedInsurance")], "transactions")

# Ver las primeras transacciones
summary(trans_data)


#Pregunta N° 33

#Generar reglas con apriori()

reglas <- apriori(trans_data,
        parameter= list(supp=0.01, conf=0.7,target="rules"))

# install.packages("arulesViz")
library(arulesViz)

inspect(reglas)
inspectDT(reglas) #Saca una tabla mas clara

###################################################################################
# Redes Neuronales
##################################################################################


library(openxlsx)
insurance_data= read.xlsx(file.choose())
str(insurance_data)
# Asegurarse de que las variables categóricas ya están como factores
insurance_data$Gender <- as.factor(insurance_data$Gender)
insurance_data$MaritalStatus <- as.factor(insurance_data$MaritalStatus)
insurance_data$PolicyType <- as.factor(insurance_data$PolicyType)
insurance_data$Region <- as.factor(insurance_data$Region)
insurance_data$UsedInsurance <- as.factor(insurance_data$UsedInsurance)

library(nnet)

str(insurance_data)

set.seed(123)
#SE ELIMINA CLAIMS PORQUE ESTÁ DIRECTAMENTE RELACIONADA AL USO DEL SEGURO (YA QUE SI ALGUIEN LO USÓ ES MAYOR A 0)
# Entrenamos el modelo
modelo_red_neuronal <- nnet(UsedInsurance ~ Age + Gender + BMI + Smoker + Children + Income + Region + 
                              MaritalStatus + PolicyType + PreviousClaims + HospitalDistance + 
                              MedicalExpenses + Deductible + Copay + CoverageAmount + RiskScore, 
                            data = insurance_data, 
                            size = 10,  # Número de neuronas en la capa oculta
                            maxit = 500,  # Máximas iteraciones
                            decay = 0.008)  # Regularización

predicciones_prob <- predict(modelo_red_neuronal, insurance_data, type = "raw")

# Convertir probabilidades en clases (umbral 0.5)
predicciones <- ifelse(predicciones_prob > 0.5, 1, 0)

# Convertir predicciones a factor
predicciones <- factor(predicciones, levels = c(0, 1))

confusion_matrix <- table(Predicho = predicciones, Real = insurance_data$UsedInsurance)
print(confusion_matrix)

## EL MODELO APRENDE MAL, PROBEMOS ESCALANDO LAS VARIABLES CUANTITATIVAS


# Pregunta N° 34

TP <- confusion_matrix[1,1]
FN <- confusion_matrix[1,2]
FP <- confusion_matrix[2,1]
TN <- confusion_matrix[2,2]


# calculamos la sencibilidad
sencibilidad <- TP/(TP+FN)

# Calculamos la precision

precision <- (TP+TN)/sum(confusion_matrix)

sencibilidad ; precision


#Pregunta N° 35

insurance_data2= insurance_data

insurance_data2$Age= scale(insurance_data2$Age)
insurance_data2$BMI= scale(insurance_data2$BMI)
insurance_data2$Children= scale(insurance_data2$Children)
insurance_data2$Income= scale(insurance_data2$Income)
insurance_data2$PreviousClaims= scale(insurance_data2$PreviousClaims)
insurance_data2$HospitalDistance= scale(insurance_data2$HospitalDistance)
insurance_data2$MedicalExpenses= scale(insurance_data2$MedicalExpenses)
insurance_data2$Deductible= scale(insurance_data2$Deductible)
insurance_data2$Copay= scale(insurance_data2$Copay)
insurance_data2$CoverageAmount= scale(insurance_data2$CoverageAmount)
insurance_data2$RiskScore= scale(insurance_data2$RiskScore)


set.seed(123)
modelo_red_neuronal <- nnet(UsedInsurance ~ Age + Gender + BMI + Smoker + Children + Income + Region + 
                              MaritalStatus + PolicyType + PreviousClaims + HospitalDistance + 
                              MedicalExpenses + Deductible + Copay + CoverageAmount + RiskScore, 
                            data = insurance_data2, 
                            size = 15,  # Número de neuronas en la capa oculta
                            maxit = 500,  # Máximas iteraciones
                            decay = 0.01)  # Regularización
predicciones_prob <- predict(modelo_red_neuronal, insurance_data2, type = "raw")

predicciones <- ifelse(predicciones_prob > 0.5, 1, 0)

# Convertir predicciones a factor
predicciones <- factor(predicciones, levels = c(0, 1))

confusion_matrix <- table(Predicho = predicciones, Real = insurance_data2$UsedInsurance)
print(confusion_matrix)


TP <- confusion_matrix[1,1]
FN <- confusion_matrix[1,2]
FP <- confusion_matrix[2,1]
TN <- confusion_matrix[2,2]


# calculamos la sencibilidad
sencibilidad <- TP/(TP+FN)

# Calculamos la precision

precision <- (TP+TN)/sum(confusion_matrix)


#Calculo de Kappa

p_o <- precision # precision observada
# p_e precision esperada
p_e <- sum((rowSums(confusion_matrix)*colSums(confusion_matrix))/(sum(confusion_matrix))^2) 
kappa <- (p_o-p_e)/(1-p_e)

ppv <- TP/(TP+FP)


precision; kappa;sencibilidad; ppv
