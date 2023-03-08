# Definir el tamaño de la población
N <- 1000

# Definir la probabilidad de individuos enfermos
p <- 0.5

# Generar la población
poblacion <- rbinom(n = N, size = 1, prob = p)