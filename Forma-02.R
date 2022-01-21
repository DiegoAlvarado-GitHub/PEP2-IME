


if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}

if (!require(caret)){
  install.packages("caret", dependencies = TRUE )
  require (caret)
}

if (!require(leaps)){
  install.packages("leaps", dependencies = TRUE )
  require (leaps)
}

if (!require(dummies)){
  install.packages("dummies", dependencies = TRUE )
  require (dummies)
}

if (!require(devtools)){
  install.packages("devtools", dependencies = TRUE )
  require (devtools)
}

if (!require(pROC)){
  install.packages("pROC", dependencies = TRUE )
  require (pROC)
}


if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}


#apertura del archivo con la información
data <- read.csv(file.choose(), head = TRUE, sep=";")




#Pregunta 1



# Se leen los datos
datosP1 <- data

#Se establece un nivel de significancia del 0.05

alfaP1 <- 0.05

#Se formulan las hipótesis:
#Hipótesis nula (H0): No existen diferencias significativas entre el promedio de evaluación
# realizada por el capitán entre las distintas divisiones.

#Hipótesis alternativa (Ha): Existe al menos una división en las que se observe una diferencia significativa
# entre el promedio de evaluación realizada por el capitán.


#Se filtra por división para posteriormente convertir los datos en formato largo
datosP1Cavetrooper <- datosP1 %>% filter(division == "Cavetrooper") %>% select(eval_capitan)
datosP1Cavetrooper <- as.numeric(gsub(",", ".", datosP1Cavetrooper$eval_capitan))

datosP1Snowtrooper <- datosP1 %>% filter(division == "Snowtrooper") %>% select(eval_capitan)
datosP1Snowtrooper <- as.numeric(gsub(",", ".", datosP1Snowtrooper$eval_capitan))

datosP1Lavatrooper <- datosP1 %>% filter(division == "Lavatrooper") %>% select(eval_capitan)
datosP1Lavatrooper <- as.numeric(gsub(",", ".", datosP1Lavatrooper$eval_capitan))


datosP1Shoretrooper <- datosP1 %>% filter(division == "Shoretrooper") %>% select(eval_capitan)
datosP1Shoretrooper <- as.numeric(gsub(",", ".", datosP1Shoretrooper$eval_capitan))

datosP1Spacetrooper <- datosP1 %>% filter(division == "Spacetrooper") %>% select(eval_capitan)
datosP1Spacetrooper <- as.numeric(gsub(",", ".", datosP1Spacetrooper$eval_capitan))

datosP1Sandtrooper <- datosP1 %>% filter(division == "Sandtrooper") %>% select(eval_capitan)
datosP1Sandtrooper <- as.numeric(gsub(",", ".", datosP1Sandtrooper$eval_capitan))


datosP1Flametrooper <- datosP1 %>% filter(division == "Flametrooper") %>% select(eval_capitan)
datosP1Flametrooper <- as.numeric(gsub(",", ".", datosP1Flametrooper$eval_capitan))

datosP1Recontrooper <- datosP1 %>% filter(division == "Recontrooper") %>% select(eval_capitan)
datosP1Recontrooper <- as.numeric(gsub(",", ".", datosP1Recontrooper$eval_capitan))


#Puesto que son 100 observaciones para cada división, se toman en cuenta 100 instancias
instancia <- factor(1:100)


datosP1Filtrados <- data.frame(instancia,datosP1Cavetrooper,datosP1Snowtrooper,datosP1Lavatrooper,datosP1Shoretrooper,datosP1Spacetrooper,datosP1Sandtrooper,datosP1Flametrooper,datosP1Recontrooper)

colnames(datosP1Filtrados) <- c("Instancias","Cavetrooper","Snowtrooper", "Lavatrooper","Shoretrooper","Spacetrooper","Sandtrooper","Flametrooper","Recontrooper")

datosP1Largo <- datosP1Filtrados %>% pivot_longer(c("Cavetrooper","Snowtrooper", "Lavatrooper","Shoretrooper","Spacetrooper","Sandtrooper","Flametrooper","Recontrooper"),
                                                  names_to = "division",
                                                  values_to = "eval_capitan")



datosP1Largo[["division"]] <- factor(datosP1Largo[["division"]])

print(datosP1Largo$eval_capitan)

pruebaP1 <- shapiro.test(datosP1Largo$eval_capitan)
print(pruebaP1)
#no existe evidencia suficientemente fuerte para rechazar la normalidad


cat ("Procedimiento ANOVA usando ezANOVA")
pruebaAnova <- ezANOVA(data = datosP1Largo, dv = eval_capitan , within = division,
                       wid = Instancias, return_aov = TRUE)
print(summary(pruebaAnova$aov))

#como el p-value es menor a alfa, se rechaza la hipotesis nula sobre la alternativa, existiendo al menos una división en las que se observe una diferencia significativa
# entre el promedio de evaluación realizada por el capitán.











#Pregunta 2
#semilla 
set.seed(23)
#se obtiene la muestra de 400 datos 
muestras <- sample_n(data, size= 400)
muestras$es_clon <-factor(muestras$es_clon)
muestras$division <-factor(muestras$division)

as.numeric(gsub(",", ".", muestras$velocidad))
as.numeric(gsub(",", ".", muestras$agilidad))
as.numeric(gsub(",", ".", muestras$armadura))
as.numeric(gsub(",", ".", muestras$eficiencia))



# Separar conjuntos de entrenamiento (80%) y prueba (20%)
n <- nrow (muestras)
n_entrenamiento <- floor(0.8 * n)
aux <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- muestras[aux , ]
prueba <- muestras[-aux , ]

#se busca predecir si es clon o no, esto se encuentra en la columna es_clon

#Ajuste de modelo nulo
nulo <- glm( es_clon ~ 1 , family = binomial ( link = "logit") , data = entrenamiento )

#Ajuste de modelo completo
completo <- glm( es_clon ~ . , family = binomial ( link = "logit") ,
                 data = entrenamiento )

cat("\n Evaluación de modelos completo y nulo mediante el AIC:\n")
print(add1(nulo , scope = completo))
#Mediante la informacion mostrada, se determina pertinente escoger como predictor: velocidad, posee el menor AIC
#los otros posibles predictores son agilidad + armadura + eficiencia, pero entregan valores na

#Realizando otro tipo de selección, como regresión escalonada, podemos notar que nos entrega solo como predictor a velocidad
mejor <- step(nulo, scope = list(lower = nulo, upper = completo), direction = "both", trace = 0)
print (summary(mejor))



#Se procede a construir el modelo con un solo predicto (por el motivo mencionado previamente)
modelo_esClon <- glm(es_clon ~ velocidad, family = binomial (link = "logit"), data = entrenamiento)
print(summary(modelo_esClon))

#se calcula la distancia cook
observaciones <- data.frame (respuesta_predicha = fitted ( modelo_esClon ) ) 
observaciones [[" distancia_Cook "]] <- cooks.distance ( modelo_esClon ) 
# Observaciones con distancia de Cook mayor a uno. 
distanciaCook <- which ( observaciones [[" distancia_Cook "]] > 1) 
cat ("- Residuos con una distancia de Cook alta :",distanciaCook , "\n") 
#como se puede observar no existen observaciones con distancia mayor a 1 (no hay valores atipicos)


#Verificar la independencia del residuo
print(durbinWatsonTest(modelo_esClon, max.lag = 5))
#Como se puede ver cumple

#Verificar multicolinealidad entre los predictores: esto no lo podemos verificar debido a que tenemos un solo predictor
#Falto terminar











#Pregunta 3

# se realiza un estudio para determinar el sentir de los estudiantes de la universidad de santiago respecto al retorno presencial a la universidad, para 
# esto se seleccionaron 200 estudiantes al azar quienes deben evaluar 2 diferentes políticas de retorno, cada una con sus respectivos puntos que se deben
# cumplir tanto de higiene, protocolo docente, protocolo en caso de covid positivo, etc. Cada política de retorno se evalúa en una escala de Likert de 7
# puntos, donde 1 significa “muy malo” y 7, “muy bueno”.

#Variables involucradas: política de retorno 1, política de retorno 2

#Hipótesis a contrastar
# H0: las mismas personas no perciben diferencia en las políticas de retorno
# HA: las mismas personas consideran que la política de retorno 1 es mejor que la política de retorno 2






























