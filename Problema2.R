#a. 
# Definir los tamaños de las poblaciones
N1 <- 1000
N2 <- 1500

# Generar las poblaciones
set.seed(123) # fijar la semilla para reproducibilidad
p1 <- rbinom(N1, 1, 0.1)
p2 <- rbinom(N2, 1, 0.1)

# Verificar los resultados
table(p1)
table(p2)


# Definir función para obtener muestras aleatorias y estimadores de proporción
sample_props <- function(N1, N2, n) {
  # Obtener muestras aleatorias de ambos lotes
  sample1 <- sample(p1, n, replace = TRUE)
  sample2 <- sample(p2, n, replace = TRUE)
  
  # Calcular los estimadores de proporción muestral para cada lote
  p1_hat <- mean(sample1)
  p2_hat <- mean(sample2)
  
  # Calcular la diferencia entre los estimadores
  diff <- p1_hat - p2_hat
  
  # Retornar los resultados como un vector
  return(c(p1_hat, p2_hat, diff))
}

# Ejemplo de uso de la función para obtener una muestra de tamaño n=100
set.seed(123) # fijar la semilla para reproducibilidad
sample_props(N1, N2, n = 100)

## c. 
n <- 100
reps <- 500
sample_props_diff <- function(N1, N2, n) {
  p1 <- rbinom(N1, 1, 0.1)
  p2 <- rbinom(N2, 1, 0.1)
  s1 <- sample(p1, n)
  s2 <- sample(p2, n)
  diff <- mean(s1) - mean(s2)
  return(diff)
}
# Realizar simulación
diffs <- rep(NA, reps)
for (i in 1:reps) {
  diffs[i] <- sample_props_diff(N1, N2, n)
}

# Analizar resultados
hist(diffs, main = "Distribución de diferencias de estimadores", xlab = "Diferencia", col = "lightblue")
abline(v = 0, col = "red")
mean_diff <- mean(diffs)
sd_diff <- sd(diffs)
cat("Media de diferencias:", mean_diff, "\n")
cat("Desviación estándar de diferencias:", sd_diff, "\n")

## d. 

simulacion <- function(N1, N2, n) {
  # Generar las dos poblaciones
  poblacion1 <- rbinom(N1, 1, 0.1)
  poblacion2 <- rbinom(N2, 1, 0.1)
  
  # Obtener las muestras aleatorias
  muestra1 <- sample(poblacion1, n)
  muestra2 <- sample(poblacion2, n)
  
  # Calcular los estimadores de proporción muestral
  p1_hat <- sum(muestra1) / n
  p2_hat <- sum(muestra2) / n
  
  # Calcular la diferencia entre los estimadores
  diff_p_hat <- p1_hat - p2_hat
  
  # Devolver un vector con los resultados
  return(c(p1_hat, p2_hat, diff_p_hat))
}

# Definir los tamaños de muestra
nd <- c(5, 10, 15, 20, 30, 50, 60, 100, 200, 500)

# Repetir la simulación 500 veces para cada tamaño de muestra
resultados <- data.frame(matrix(NA, nrow = length(n), ncol = 3))
colnames(resultados) <- c("p1_hat", "p2_hat", "diff_p_hat")
for (i in 1:length(n)) {
  temp <- replicate(500, simulacion(N1 = 1000, N2 = 1500, n = n[i]))
  resultados[i, ] <- colMeans(temp)
}
# Mostrar los resultados
resultados

## Analisis 

# Prueba de Shapiro-Wilk
shapiro.test(resultados$diff_p_hat)

# Gráfico Q-Q plot
qqnorm(resultados$diff_p_hat)
qqline(resultados$diff_p_hat)

#e-
