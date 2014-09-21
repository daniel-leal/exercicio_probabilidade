# obtendo o banco
banco <- read.table(file = "/home/daniel/R_Script/exercicio_probabilidade/LungCapData.txt", header = T, sep=",")

# exibindo os dados do banco
summary(banco)

attach(banco)

# Exibindo os valores em grafico
plot(factor(Age))

# Quantidade de Linhas
tamanho <- length(factor(Age))

# Calculando a Esperança
esperanca <- function(x, px) {
  result <- numeric()
  n <- length(x)
  for (i in 1:n) {
    result[i] <- x[i] * px[i]
  }
  E <- sum(result)
  return(E)
}

# Calculando a Variância
variancia <- function(x, px) {
  esp <-  esperanca(x, px) ** 2
  result <- numeric()
  n <- length(x)
  for (i in 1:n) {
    result[i] <- (x[i]**2) * px[i]
  }
  E <- sum(result)
  return(E - esp)
}

###### Execução do Programa ######

# Obtendo os Valores da coluna Age.
x <- as.numeric(levels(factor(Age)))

# Obtendo a frequência dos valores da Coluna Age
freq <- as.numeric(matrix(summary(factor(Age))))

# Calculando a probabilidade
probabilidade.x <- function(freq) {
  n <- length(freq)
  prob <- numeric()
  for(i in 1:n) {
    prob[i] <- freq[i] / length(Age)
  }
  return(prob)
}

prob <- probabilidade.x(freq)

# Verificando a esperança.
esperanca(x, prob)

# Verificando a variância.
variancia(x, prob)
var(Age)

# Calculando o Desvio
sqrt(var(Age))
sd(Age)

