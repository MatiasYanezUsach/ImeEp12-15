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
# sitios web, hemos encontrado que el ph vino es una medida de la acidez o 
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
muestra <- muestra %>% select(!predictoras)
correlacion <- cor(muestra, y = respuesta)
print(correlacion)

# Una vez creada la matriz de correlación, obtenemos la variable que tenga mayor
# correlación:
correlacion_max <- which(abs(correlacion) == max(abs(correlacion)))
predictor <- rownames(correlacion)[correlacion_max]
cat("La mejor variable es:",predictor)

# Una vez obtenida la mejor variable para determinar la calidad de los vinos
# daremos paso a construir el modelo de regresión lineal simple, para ello en
# primer lugar juntaremos la variable de respuestas con la variable predictora
datos_rls <- muestra %>% select(predictor)
datos_rls <- cbind(datos_rls, respuesta)






