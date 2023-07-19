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
# regresión logística para predecir la variable clase, de acuerdo con las
# siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos
#    del RUN (sin considerar el dígito verificador) del integrante de mayor edad 
#    del equipo.

# 2. Seleccionar una muestra de 120 vinos, asegurando que la mitad sean blancos 
#    y la otra mitad, tintos. Dividir esta muestra en dos conjuntos: los datos 
#    de 80 vinos (40 con clase “Blanco”) para utilizar en la construcción de los
#    modelos y 40 vinos (20 con clase “Blanco”) para poder evaluarlos.

# 3. Seleccionar 6 variables predictoras de manera aleatoria (al igual que en el
#    ejercicio anterior).

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría
#    ser útil para predecir la clase, justificando bien esta selección.

# 5. Usando el entorno R y paquetes estándares, construir un modelo de regresión
#    logística con el predictor seleccionado en el paso anterior y utilizando de
#    la muestra obtenida.

# 6. Agregue la variable seleccionada en el paso 4 al conjunto obtenido en el 
#    punto 3.

# 7. Usando herramientas estándares1 para la exploración de modelos del entorno 
#    R, buscar entre dos y cinco predictores de entre las variables presentes en
#    el conjunto obtenido en el paso anterior para construir un modelo de 
#    regresión logística múltiple.

# 8. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de 
#    ajuste y son generalizables) y “arreglarlos” en caso de que tengan algún 
#    problema.

# 9. Usando herramientas del paquete caret, evaluar el poder predictivo de los 
#    modelos con los datos de los 40 vinos que no se incluyeron en su 
#   construcción en términos de sensibilidad y especificidad.
################################################################################

# Antes de resolver la problemática, en primer lugar debemos obtener los datos:
datos <- read.csv2("EP13 Datos.csv", stringsAsFactors = TRUE)
datos[["clase"]] <- factor(datos[["clase"]])

#ratio_azucar_alcohol <- datos[["azucar.residual"]] / datos[["alcohol"])
#EN <- rep("Sobrepeso", length(IMC))
#EN[IMC < 25] <- "No sobrepeso"
#EN <- factor(EN)
#datos <- cbind(EN, datos)


# Definimos la semilla con los últimos 4 dígitos del RUT del participante Pablo 
# Villarreal (19.894.097-6), el cual es el mayor de todos los integrantes.
set.seed(4097)

# Seleccionamos una muestra de 120 vinos, asegurando que la mitad sean vinos 
# blancos y la otra mitad vinos tintos
muestras_blancos <- datos %>% filter(clase == "Blanco") %>% 
  sample_n(60, replace = FALSE)

muestras_tintos <- datos %>% filter(clase == "Tinto") %>%
  sample_n(60, replace = FALSE)

# Juntamos ambas muestras
muestras_120 <- rbind(muestras_blancos, muestras_tintos)
# Separamos la variable de respuesta
respuesta <- muestras_120[["clase"]]
muestras_120[["clase"]] <- NULL

# Seleccionamos los datos de 80 vinos (40 con clase “Blanco”) para utilizar en 
# la construcción de los modelos y 40 vinos (20 con clase “Blanco”) para poder 
# evaluarlos.
vinos_model <- sample.int(nrow(x = muestras_blancos), 40, replace = FALSE)
vinos_eval <- sample.int(nrow(x = muestras_blancos), 40, replace = FALSE)

# Seleccionamos los datos de la misma forma en la que hicimos en el Ep anterior.
# Entonces:

# Seleccionamos 6 variables predictoras al azar.
variables <- colnames(muestras_120)
predictores <- sample(variables, 6, replace = FALSE)
cat("Predictores seleccionados al azar:\n")
print(predictores)

# como podemos observar, las variables predictoras escogidas al azar son:
# "azúcar.residual", "ácido.cítrico", "acidez.volátil", "densidad", "cloruros", 
# "alcohol"

# Al igual que en el trabajo anterior, tenemos dos problemas para escoger una 
# variable predictora, la primera es que al tomarse las variables de forma 
# aleatoria, puede que la variable escogida por nosotros sea elegida previamente
# como predictora, y la segunda es que pueda existir una mejor variable entre 
# las restantes para determinar la calidad del vino.

# Sin embargo en este trabajo, como tenemos como variable de respuesta la 
# "clase" del vino, y esta es categórica, no podemos usar la función "cor" para
# determinar la correlación entre la variable predictora escogida y la variable
# de respuesta, por lo que:

# datos para rlm / REVISAR
datos.rlm <- datos %>% select(predictores)
datos.rlm <- cbind(respuesta, datos.rlm)
