#Problema 1
#Teorema del Límite Central
#El Teorema del Límite Central es uno de los más importantes en la inferencia estadística y habla sobre la convergencia de los estimadores como la proporción muestral a la distribución normal. 
#Algunos autores afirman que esta aproximación es bastante buena a partir del umbral n>30

#A continuación se describen los siguientes pasos para su verificación:

#a. Realice una simulación en la cual genere una población de N=1000
#(Lote), donde el porcentaje de individuos (supongamos plantas) enfermas sea del 50%.

# Definir el tamaño de la población
N <- 1000
# Definir la probabilidad de individuos enfermos
p <- 0.5
# Generar la población
poblacion <- rbinom(n = N, size = 1, prob = p)

##Genere una función que permita:
#Obtener una muestra aleatoria de la población y
muestra <- sample(poblacion, size = 100, replace = FALSE)
#Calcule el estimador de la proporción muestral pˆ
#para un tamaño de muestra dado n
p <- mean(muestra)
print(p)

##Repita el escenario anterior (b) n=500 veces y analice los resultados en cuanto al comportamiento de los 500
#resultados del estimador pˆ

muestra2 <- sample(poblacion, size = 500, replace = FALSE)
p2 <- mean(muestra2)

#¿Qué tan simétricos o sesgados son los resultados obtenidos? 
#¿qué se puede observar en cuanto a la variabilidad?

##d 
# Definir los tamaños de muestra
n <- c(5, 10, 15, 20, 30, 50, 60, 100, 200, 500)

# Generar muestras aleatorias y calcular la media
means <- sapply(n, function(x) {
  set.seed(123) # fijar la semilla para reproducibilidad
  xbar <- mean(rnorm(x))
  return(xbar)
})

# Ver los resultados
means

# Realizar pruebas de normalidad y gráficos para cada muestra
for (i in seq_along(n)) {
  # Generar la muestra aleatoria
  set.seed(123) # fijar la semilla para reproducibilidad
  sample <- rnorm(n[i])
  
  # Realizar la prueba de Shapiro-Wilk
  shapiro <- shapiro.test(sample)
  
  # Graficar el QQ-plot
  qqnorm(sample)
  qqline(sample)
  
  # Imprimir los resultados
  cat("Tamaño de muestra:", n[i], "\n")
  cat("Media muestral:", round(means[i], 3), "\n")
  cat("Shapiro-Wilk p-value:", shapiro$p.value, "\n")
  if (shapiro$p.value < 0.05) {
    cat("La muestra NO se distribuye normalmente\n")
  } else {
    cat("La muestra se distribuye normalmente\n")
  }
  cat("\n")
}

#e. Repita toda la simulación (puntos a – d), pero ahora para lotes con 10% de plantas enfermas y de nuevo para lotes con un 90% de plantas enfermas. Concluya sobre los resultados del ejercicio.


