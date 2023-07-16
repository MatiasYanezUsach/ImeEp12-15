# Grupo 2
# Integrantes:
# Matías Yáñez Muñoz
# Pablo Villarreal Ortiz
# David Valero Croma

# Importación de librerías
if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE)
  require(tidyr)
}

if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}
if(!require(caret)){
  install.packages("caret", dependencies = TRUE)
  require(caret)
}
if(!require(leaps)){
  install.packages("leaps", dependencies = TRUE)
  require(leaps)
}

if(!require(car)){
  install.packages("car", dependencies = TRUE)
  require(car)
}

################################################################################
# contexto:

# Un estudio recolectó muestras de distintas botellas de vino de una importante 
# viña francesa. Estas mediciones están disponibles en el archivo EP13 Datos.csv
# que acompaña a este enunciado, el cual esta compuesto de la siguiente manera:

#Columna              |Descripción                   |Unidad
#---------------------|------------------------------|--------------------------
#clase                |Tipo de vino                  |Categórica (Tinto, Blanco)
#calidad              |Calidad del vino              |Entera [0; 10]
#acidez.fija          |Ácidos fijos [mg/L]           |Real [0; 20]
#acidez.volátil       |Ácidos volátiles [g/L]        |Real [0; 2]
#ácido.cítrico        |Ácidos cítrico [mg/L]         |Real [0; 2]
#azúcar.residual      |Azúcar residual [g/L]         |Real [0; 80]
#cloruros             |Cloruros [g/L]                |Real [0; 1]
#dióxido.azufre.libre |Dióxido de azufre libre [g/L] |Real [0; 300]
#dióxido.azufre.total |Dióxido de azufre total [g/L] |Real [0; 500]
#densidad             |Densidad total [g/L]          |Real [0; 1,5]
#ph                   |Acidez (pH)                   |Real [1; 5]
#sulfatos             |Sulfatos [g/L]                |Real [0; 3]
#alcohol              |Porcentaje de alcohol         |Real [0; 20]
#---------------------|------------------------------|--------------------------
# Como es posible observar, el estudio incluyó 12 mediciones de características
# del vino y la indicación de si cada vino es tinto o blanco.

# En consecuencia del estudio anterior, se nos se pide construir un modelo de 
# regresión lineal simple y otro de regresión lineal múltiple para predecir la
# variable calidad, de acuerdo con las siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro 
#    dígitos del RUN (sin considerar el dígito verificador) del integrante de 
#    menor edad del equipo.

# 2. Seleccionar una muestra de 100 vinos.

# 3. Seleccionar de forma aleatoria 6 posibles variables predictoras.

# 4. Seleccionar, entre las variables que no fueron escogidas en el punto 
#    anterior, una que el equipo considere que podría ser útil para predecir la 
#    variable calidad, justificando bien esta selección.

# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el 
#    predictor seleccionado en el paso anterior.

# 6. Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el 
#    punto 3.

# 7. Usando herramientas para la exploración de modelos del entorno R, escoger 
#    entre dos y cinco predictores de entre las variables presentes en el 
#    conjunto obtenido en el paso anterior para construir un modelo de regresión
#    lineal múltiple.

# 8. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema 
#    con las condiciones que deben cumplir.

# 9. Evaluar el poder predictivo del modelo en datos no utilizados para 
#    construirlo (o utilizando validación cruzada).
################################################################################

# Antes de resolver la problemática, en primer lugar debemos obtener los datos:
datos <- read.csv2(file.choose(), stringsAsFactors = TRUE)

# Definimos la semilla con los últimos 4 dígitos del RUT del participante 
# Matias Yañez (20.580.291-6), el cual es el menor de todos los integrantes.
set.seed(0291)

# Cogemos la muestra de los 100 vinos
muestra <- sample_n(datos, 100)
muestra[["clase"]] <- factor(muestra[["clase"]])

# Separamos la variable de respuesta
respuesta <- muestra[["calidad"]]
muestra[["calidad"]] <- NULL

# La variable de respuesta escogida es "calidad" debido a que en este caso, la 
# calidad del vino es una variable de interés y se espera que esté influenciada
# por las características medibles del vino, como la acidez, el contenido de 
# azúcar, el pH, etc.

# Seleccionamos de forma aleatoria las 6 variables predictoras, para ello 
# obtenemos, los nombres de cada variable
variables <- colnames(muestra)

# Luego tomamos 6 de forma aleatoria
predictoras <- sample(variables, 6, replace = FALSE)
cat("Las variables predictoras escogidas de forma aleatoria son:\n",predictoras)

# Como las variables predictoras obtenidas en el momento de ejecutar son: 
# densidad, acidez.fija, ácido.cítrico, clase, dióxido.azufre.libre y 
# azúcar.residual, como equipo hemos concluido que entre las variables 
# restantes, la mejor candidata para predecir la variable de calidad es el pH.

# La variable escogida fue el ph, ya que tras buscar información en diferentes
# sitios web, hemos encontrado que el ph del vino es una medida de la acidez o 
# alcalinidad del mismo, y la acidez del vino juega un papel importante en su 
# sabor, equilibrio y conservación, por lo que un ph adecuado puede contribuir
# a la calidad del vino y afectar su estabilidad y evolución química.

# Pero tenemos dos problemas, el primero es que al tomarse las variables de 
# forma aleatoria, puede que la variable escogida por nosotros sea elegida
# previamente como predictora, y el segundo problema es que pueda existir una
# mejor variable entre las restantes para determinar la calidad del vino.

# Para evitar los problemas anteriores, crearemos una matriz de correlación, 
# para evaluar la correlación de cada variable restante con la variable de 
# respuesta (calidad) anteriormente definida.

# Evaluar la correlación de las variables restantes con la respuesta.
sub_muestra <- muestra %>% select(!predictoras)
correlacion <- cor(sub_muestra, y = respuesta)
print(correlacion)

# Una vez creada la matriz de correlación, obtenemos la variable que tenga mayor
# correlación:
correlacion_max <- which(abs(correlacion) == max(abs(correlacion)))
predictor <- rownames(correlacion)[correlacion_max]
cat("La mejor variable es:",predictor)

# Una vez obtenida la mejor variable para determinar la calidad de los vinos
# daremos paso a construir el modelo de regresión lineal simple, para ello en
# primer lugar juntaremos la variable de respuestas con la variable predictora
datos_rls <- sub_muestra %>% select(predictor)
colnames(datos_rls)[1] <- "predictor_rls"
datos_rls <- cbind(datos_rls, respuesta)

# Si somos observadores, podemos observar que la variable predictora es de tipo
# categórica, por lo que deberemos realizar una regresión lineal con un 
# predictor categórico

# Antes de seguir avanzando con el problema, debemos verificar el cumplimiento 
# de las siguientes condiciones:

# 1. Los datos deben presentar una relación lineal.
# 2. La distribución de los residuos debe ser cercana a la normal.
# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados debe
#    ser aproximadamente constante.
# 4. Las observaciones deben ser independientes entre sí.

# Entonces, para poder verificar la primera condición, lo que haremos sera 
# evaluar si existe una relación lineal entre la variable predictora y la 
# variable de respuesta utilizando un gráfico de dispersión (scatter plot) con 
# la variable predictora en el eje x y la variable de respuesta en el eje y.
g1 <- ggscatter(datos_rls, x = "predictor_rls", y = "respuesta", color = "blue",
                fill = "blue", xlab = "Alcohol [grados]", 
                ylab = "Calidad [Puntuacion]")

g1 <- g1 + geom_smooth(method = lm, se = FALSE, colour = "red")
print(g1)

# Como podemos observar en el gráfico resultante, no se puede verificar la 
# primera condición, por lo que usaremos la validación cruzada de pliegues para
# solucionar dicho inconveniente 
rls <- train (respuesta ~ predictor_rls, data = datos_rls, method = "lm",
              trControl = trainControl(method = "cv", number = 5))
print(summary(rls))

# Una vez obtenido el modelo anterior, obtendremos los residuos relacionados en
# conjunto de  las estadísticas influyentes, para ello, construiremos una matriz
# donde se encontrara todo lo anterior mencionado.
evaluacion_rls <- data.frame(respuesta_predicha = fitted(rls[["finalModel"]]))
evaluacion_rls[["residuos_estandarizados"]] <- rstandard(rls[["finalModel"]])
evaluacion_rls[["residuos_estudiantizados"]] <-rstudent(rls[["finalModel"]])
evaluacion_rls[["distancia_Cook"]] <- cooks.distance(rls[["finalModel"]])
evaluacion_rls[["dfbeta"]] <- dfbeta(rls[["finalModel"]])
evaluacion_rls[["dffit"]] <- dffits(rls[["finalModel"]])
evaluacion_rls[["apalancamiento"]] <- hatvalues(rls[["finalModel"]])
evaluacion_rls[["covratio"]] <- covratio(rls[["finalModel"]])

cat("Identificación de valores atípicos:\n")
# Observaciones con residuos estandarizados fuera del 95% esperado.
residuos_stnd <- which(abs(evaluacion_rls[["residuos_estandarizados"]]) > 1.96)
cat("Residuos estandarizados fuera del 95% esperado:", residuos_stnd, "\n")

# Observaciones con distancia de Cook mayor a uno.
residuos_cook <- which(evaluacion_rls[["distancia_Cook"]] > 1)
cat("Residuos con una distancia de Cook alta:", residuos_cook, "\n")

# Como podemos observar, no se encuentran distancias de cook mayores a 1, lo que
# significa que no hay observaciones que tengan un impacto significativo en el 
# modelo de regresión lineal simple.  

# Observaciones con apalancamiento mayor igual al doble del apalancamiento 
# promedio.
apalancamiento <- (ncol(datos_rls) + 1) / nrow(datos_rls)
residuos_apal <- which(evaluacion_rls[["apalancamiento"]] > 2 * apalancamiento)
cat("Residuos con apalancamiento fuera de rango:", residuos_apal, "\n")

# Observaciones con DFBeta mayor o igual a 1.
residuos_DFBeta <- which(apply(evaluacion_rls[["dfbeta"]] >= 1, 1, any))
names(residuos_DFBeta) <- NULL
cat("Residuos con DFBeta >= 1:", residuos_DFBeta, "\n")

#  Como podemos observar, no se encuentran observaciones con DFBeta mayor o 
# igual a 1,ya que ninguna observación tiene una influencia significativa en los
# resultados de la estimación de los coeficientes.

# Observaciones con razón de covarianza fuera de rango.
inferior <- 1 - 3 * apalancamiento
superior <- 1 + 3 * apalancamiento
residuos_cov <- which(evaluacion_rls[["covratio"]] < inferior | 
                      evaluacion_rls[["covratio"]] > superior)
cat("Residuos con razón de covarianza fuera de rango:", residuos_cov, "\n")

# Resumen de los residuos obtenidos
residuos <- c(residuos_stnd, residuos_cook, residuos_apal, residuos_DFBeta, 
              residuos_cov)
residuos <- sort(unique(residuos))

cat("\nResumen de los residuos obtenidos:\n")
cat("Apalancamiento promedio:", apalancamiento, "\n")
cat("Intervalo razón de covarianza: [", inferior, "; ", superior, "]\n\n",
    sep = "")

print(round(evaluacion_rls[residuos, c("distancia_Cook", 
                                       "apalancamiento", 
                                       "covratio")], 3))

# Una vez observado todos los residuos anteriores, podemos concluir que se
# trabajan con algunas observaciones con características de un valor atípico, 
# pero como no se encontraron observaciones con una distancia de cook mayores a
# 1, es posible determinar que dichas observaciones no son preocupantes a la
# hora de trabajar con la muestra.

# Por ultimo, veamos gráficos para evaluar el modelo.
modelo_final <- lm(respuesta ~ predictor_rls, data = datos_rls)
print(summary(modelo_final))
plot(modelo_final)

# En conclusión, el modelo de regresión lineal simple muestra una relación 
# lineal fuerte entre el indice de alcohol y la calidad del vino, y no se 
# identificaron observaciones influyentes o problemas graves en los residuos,
# por lo que, el indice de alcohol es un predictor relevante para estimar la 
# calidad del vino.


###############################################################################
# Ahora realizaremos la regresión lineal múltiple, para ello, seleccionaremos
# las variables predictores escogidas al azar previamente.
datos_rlm <- muestra %>% select(predictoras)
datos_rlm <- cbind(respuesta, datos_rlm) # Trabajaremos con la misma respuesta

# Seleccionar mejores predictores para modelo de regresión lineal múltiple
# usando el método de todos los subconjuntos.
rlm_ini <- regsubsets(respuesta ~ ., data = datos_rlm, nbest = 1, nvmax = 5,
                      method = "exhaustive")
plot(rlm_ini)

# Como en el caso anterior tuvimos problemas para verificar las condiciones, en 
# este caso ocurrirá lo mismo, por lo que usaremos la validación cruzada de 
# pliegues para solucionar dicho inconveniente 

# Ajustar el modelo con los mejores predictores usando validación cruzada de 5
# pliegues, utilizando la azucar, debido a que en la vida real, es un componente
# vital para la calidad del vino
rlm <- train (respuesta ~  densidad + azucar.residual, data = datos_rlm, 
              method = "lm",
              trControl = trainControl(method = "cv", number = 5))

cat("\nModelo de regresión lineal múltiple\n")
print(summary(rlm))

# Evaluar modelo.
# Obtener residuos y estadísticas de influencia de los casos.
evaluacion_rlm <- data.frame(respuesta_predicha = fitted(rlm[["finalModel"]]))
evaluacion_rlm[["residuos_estandarizados"]] <- rstandard(rlm[["finalModel"]])
evaluacion_rlm[["residuos_estudiantizados"]] <-rstudent(rlm[["finalModel"]])
evaluacion_rlm[["distancia_Cook"]] <- cooks.distance(rlm[["finalModel"]])
evaluacion_rlm[["dfbeta"]] <- dfbeta(rlm[["finalModel"]])
evaluacion_rlm[["dffit"]] <- dffits(rlm[["finalModel"]])
evaluacion_rlm[["apalancamiento"]] <- hatvalues(rlm[["finalModel"]])
evaluacion_rlm[["covratio"]] <- covratio(rlm[["finalModel"]])

cat("Identificación de valores atípicos:\n")
# Observaciones con residuos estandarizados fuera del 95% esperado.
residuos_stnd2 <- which(abs(evaluacion_rlm[["residuos_estandarizados"]]) > 1.96)
cat("Residuos con una distancia de Cook alta \n")
print(residuos_stnd2)

# Observaciones con distancia de cook mayor a 1
residuos_cook2 <- which(evaluacion_rlm[["distancia_Cook"]] > 1)
cat("Residuos con distancia de Cook mayor que 1: \n")

# Observaciones con apalancamiento superior al doble del apalancamiento promedio
apalancamiento2 <- ncol(datos_rlm) / nrow(datos_rlm)
residuos_apal2 <- which(evaluacion_rlm[["apalancamiento"]] > 2 * apalancamiento2)
cat("Residuos con apalancamiento fuera de rango:", residuos_apal2, "\n")

# DFBeta debería ser < 1.
residuos_DFBeta2 <- which(apply(evaluacion_rlm[["dfbeta"]] >= 1, 1, any))
names(residuos_DFBeta2) <- NULL
cat("Residuos con DFBeta mayor que 1: ", residuos_DFBeta2, "\n")

# Los casos no deberían desviarse significativamente de los límites recomendados
# para la razón de covarianza:
inferior2 <- 1 - 3 * apalancamiento2
superior2 <- 1 + 3 * apalancamiento2
residuos_cov2 <- which(evaluacion_rlm[["covratio"]] < inferior2 |
                       evaluacion_rlm[["covratio"]] > superior2)
cat("Residuos con razón de covarianza fuera de rango:", residuos_cov2, "\n")

# Resumen de los residuos obtenidos
residuos2 <- c(residuos_stnd2, residuos_cook2, residuos_apal2, residuos_DFBeta2,
               residuos_cov2)
residuos2 <- sort(unique(residuos2))
cat("\nResumen de los residuos obtenidos:\n")
cat("Apalancamiento promedio:", apalancamiento2, "\n")
cat("Intervalo razón de covarianza: [", inferior2, "; ", superior2, "]\n\n",
    sep = "")
print(round(evaluacion_rlm[residuos2, c("distancia_Cook", 
                                       "apalancamiento", 
                                       "covratio")], 3))

# Una vez observado todos los residuos anteriores, podemos concluir que se
# trabajan con algunas observaciones con características de un valor atípico, 
# pero como no se encontraron observaciones con una distancia de cook mayores a
# 1, es posible determinar que dichas observaciones no son preocupantes a la
# hora de trabajar con la muestra.

# Ademas podemos observar que existe una fuerte relación entre las variables
# de azúcar y densidad para determinar la calidad del vino, ademas podemos
# concluir que este tipo de regresión nos entrego mejores resultados que la rls

#Realizamos una prueba de independencia para los residuos
cat("\nIndependencia de los residuos\n")
print(durbinWatsonTest(rlm[["finalModel"]]))

# Puesto que la prueba de Durbin-Watson entrega p = 0,174, podemos concluir que
# los residuos son independientes.