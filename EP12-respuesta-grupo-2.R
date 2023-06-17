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

if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}

if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse)
}

# ------------------------------ ACTIVIDADES --------------------------------------

# 1. Copiar el enunciado de los problemas asignados como comentarios en un script R.
# 2. El equipo lee los enunciados entregados y revisa los datos gráficamente.
# 3. Transformación de datos:
#   a. El equipo discute las posibles transformaciones y decide cuál(es) aplicar.
#   b. El equipo aplica las transformaciones y realiza la prueba paramétrica de hipótesis correspondiente.
# 4. Métodos robustos:
#   a. El equipo discute qué prueba tradicional (paramétrica) correspondería en cada caso y, considerando
#      esto, elige una prueba no paramétrica que aplicar.
#   b. El equipo aplica la prueba de hipótesis correspondiente utilizando el entorno R.
#   c. El equipo identifica las hipótesis nula y alternativa en cada caso y entrega una conclusión,
#      interpretándola en el caso estudiado.

# ------------------------------- PREGUNTAS ----------------------------------------

# 1. Tras el éxito obtenido recientemente por un equipo de agrónomos en la mejora del cultivo de manzanas de
#    exportación, con la ayuda de algunos modelos estadísticos, un colega dedicado a la producción de peras
#    desea estudiar algunas características de sus productos. Para ello, ha registrado los pesos (en gramos) de
#    algunas unidades (cada una de un árbol diferente) de dos variedades distintas, Winter Nelly y Golden Bosc,
#    durante la semana 15 de crecimiento. Desea saber si el peso de ambas variedades en esta etapa de su
#    desarrollo es el mismo. ¿Qué se puede concluir?

#   Winter Nelly  |  Golden Bosc  |
# ----------------|---------------|
#    150.6948     |   130.2934    |
#    144.935      |   192.9736    |
#    321.2211     |   227.2896    |
#    153.4385     |   160.5874    |
#    122.8188     |   132.915     |
#    171.2131     |   118.7482    |
#    157.9422     |   149.1814    |
#    219.7431     |   236.0405    |
#    204.1157     |   191.3631    |
#                 |   133.928     |
#                 |   367.991     |
#                 |   144.3353    |

# Estudio: ¿El peso de la variedad de peras tipo Winter Nelly en la semana 15 es similar al peso 
#          de la variedad Golden Bosc en dicha semana?

# Carga de datos

winter_nelly <- c(150.6948, 144.935, 321.2211, 153.4385, 122.8188, 171.2131, 157.9422, 219.7431, 204.1157)
golden_bosc <- c(130.2934, 192.9736, 227.2896, 160.5874, 132.915, 118.7482, 149.1814, 236.0405, 191.3631, 133.928, 367.991, 144.3353)

# Se formulan las siguientes hipótesis:

# H0: No hay diferencia significativa entre las medias de ambas variedades de peras en la semana 15 de crecimiento.
# H1: Existe una diferencia significativa entre las medias de ambas variedades de peras en la semana 15 de crecimiento.

# H0: µA - µB = 0
# H1: µA - µB ≠ 0

# Definición de alfa

alfa <- 0.05

# Revisión de condiciones para prueba t de Student de comparación de medias
# Se utiliza Shapiro para verificar si tiene una distribución normal

print(shapiro.test(winter_nelly))
print(shapiro.test(golden_bosc))

# Si observamos los valores obtenidos para el p-valor, resultan menores que al nivel
# De significancia previamente definido, por lo que no se distribuye de forma normal
# lo que indica que no se cumple la normalidad para las variables observadas.

# Se realizan los gráficos de dispersión e histograma correspondientes
g1 <- gghistogram(winter_nelly, color = "blue", 
                  fill = "blue", bins = 10, 
                  xlab = "Peso (Gramos)", 
                  ylab = "Peras (Unidad)")

g2 <- gghistogram(golden_bosc, color = "red", 
                  fill = "red", bins = 10, 
                  xlab = "Peso (Gramos)",
                  ylab = "Peras (Unidad)")

print(g1)
print(g2)

# Se realizan los gráficos Q-Q para mostrar la distribución. 
g1_q_q <- ggqqplot(data = winter_nelly, 
                   color = "steelblue", 
                   xlab = "Teórico", 
                   ylab = "Muestra", 
                   title = "Gráfico Q-Q muestra Winter Nelly vs. distribución normal")
print(g1_q_q)

g2_q_q <- ggqqplot(data = golden_bosc, 
                   color = "steelblue", 
                   xlab = "Teórico",
                   ylab = "Muestra", 
                   title = "Gráfico Q-Q muestra Golden Bosc vs. distribución normal")
print(g2_q_q)
# Como podemos observar en ambos gráficos Q-Q, hay presencia marcada de valores atípicos.

# Se realiza una transformación logarítmica debido a la asimetría de los datos
log_winter_nelly <- log(winter_nelly)
log_golden_bosc <- log(golden_bosc)

# Se utiliza Shapiro para verificar si las transformaciones tienen una distribución normal
print(shapiro.test(log_winter_nelly))
print(shapiro.test(log_golden_bosc))

# Ahora si observamos el p-valor, podemos observar que se cumple una distribución normal,
# lo que indica que se cumple la normalidad para las variables observadas. 

# Se realizan los gráficos de dispersión e histograma correspondientes
log_g1 <- gghistogram(log_winter_nelly, 
                      color = "blue", 
                      fill = "blue",
                      bins = 10, 
                      xlab = "Peso (Gramos)", 
                      ylab = "Peras (Unidad)")

log_g2 <- gghistogram(log_golden_bosc, 
                      color = "red", 
                      fill = "red", 
                      bins = 10, 
                      xlab = "Peso (Gramos)", 
                      ylab = "Peras (Unidad)")

print(log_g1)
print(log_g2)

# Comprobación de similitud a distribución normal mediante gráfico Q-Q
log_g1_q_q <- ggqqplot(data = log_winter_nelly,
                       color = "steelblue", 
                       xlab = "Teórico", 
                       ylab = "Muestra", 
                       title = "Gráfico Q-Q muestra transformada Winter Nelly vs. distribución normal")
print(log_g1_q_q)

log_g2_q_q <- ggqqplot(data = log_golden_bosc, 
                       color = "steelblue",
                       xlab = "Teórico",
                       ylab = "Muestra",
                       title = "Gráfico Q-Q muestra transformada Golden Bosc vs. distribución normal")
print(log_g2_q_q)

#Ahora como podemos observar en ambos graficos Q-Q, no se observan valores atipicos.

# Prueba t de Student con condiciones cumplidas

prueba <- t.test(x = log_winter_nelly,
                 y = log_golden_bosc,
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1 - alfa)

#se imprime la prueba realizada
print(prueba)

# Como el valor de p obtenido es 0.8895 y este es mayor al alfa (0.05), entonces se falla en rechazar 
# la hipótesis nula en favor de la hipótesis alternativa. Por lo tanto, se concluye con un 95% de confianza 
# que no hay evidencia suficiente para afirmar que existe una diferencia significativa entre los pesos de 
# las variedades de Winter Nelly y Golden Bosc en la semana 15 de crecimiento. Por lo tanto, se concluye 
# que en esta etapa de desarrollo, el peso de ambas variedades es similar.

# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.
# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.