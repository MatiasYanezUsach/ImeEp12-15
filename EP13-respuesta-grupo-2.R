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

# Definimos la semilla con el los últimos 4 dígitos del rut del participante 
# Matias Yañez, el cual es el menor de todos los integrantes
set.seed(0291)

# Cogemos la muestra de los 100 vinos
muestra <- sample_n(datos, 100)
muestra[["clase"]] <- factor(muestra[["clase"]])




