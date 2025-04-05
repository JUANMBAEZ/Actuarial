#Para la última sección se sugiere cargar la data "Seguros CRZ New" utilizada previamente. Se dejan algunos códigos de apoyo para iniciar este trabajo:
  
  
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
library(caret)

conf_matrix <- confusionMatrix(predicciones, insurance_data$UsedInsurance, positive = "1")

print(conf_matrix)

print(conf_matrix)

conf_matrix2 <- confusionMatrix(predicciones, insurance_data$UsedInsurance, positive ="1")

print(conf_matrix)


## EL MODELO APRENDE MAL, PROBEMOS ESCALANDO LAS VARIABLES CUANTITATIVAS

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
modelo_red_neuronal2 <- nnet(UsedInsurance ~ Age + Gender + BMI + Smoker + Children + Income + Region + 
                              MaritalStatus + PolicyType + PreviousClaims + HospitalDistance + 
                              MedicalExpenses + Deductible + Copay + CoverageAmount + RiskScore, 
                            data = insurance_data2, 
                            size = 15,  # Número de neuronas en la capa oculta
                            maxit = 500,  # Máximas iteraciones
                            decay = 0.01)  # Regularización

predicciones_prob <- predict(modelo_red_neuronal2, insurance_data2, type = "raw")
predicciones <- ifelse(predicciones_prob > 0.5, 1, 0)

# Convertir predicciones a factor
predicciones <- factor(predicciones, levels = c(0, 1))

confusion_matrix1 <- table(Predicho = predicciones, Real = insurance_data2$UsedInsurance)
print(confusion_matrix1)

conf_matrix2 <- confusionMatrix(predicciones, insurance_data2$UsedInsurance, positive =  "1")

print(conf_matrix2)
