# Bibliotecas utilizadas
library(ggplot2) # para os gráficos

# --- EXPERIMENTO COM O LANÇAMENTO DE UMA MOEDA ---
# O experimento é feito por meio de uma simulação usando a função 'sample', que
# amostra valores atribuidos a ela

# Por exemplo, para simular 1 lançamento da moeda:
sample(c("Cara","Coroa"),size = 1)

# Para simular 5 lançamentos:
sample(c("Cara","Coroa"),size = 5, replace = TRUE)

# É possível mostrar as frequências obtidas com os lançamentos:
table(sample(c("Cara","Coroa"),size = 5, replace = TRUE))

# É possível também mostrar as probabilidades:
prop.table(table(sample(c("Cara","Coroa"),size = 5, replace = TRUE)))

# Aumentando o número de lançamentos, a probabilidade tende a 50%
n <- 1000 # <--- mude aqui!
prop.table(table(sample(c("Cara","Coroa"),size = n, replace = TRUE)))

# Agora usano um loop para registrar as probabilidades de "Cara" para um número
# crescente de lançamentos
nmax <- 1000 # limite do experimento
passo <- 1 # passos do experimento (de 1 em 1, 5 em 5, etc.). Múltiplo de "nmax"
resultado <- vector()
j <- 1
for (i in seq(1, nmax, passo)) {
  lancar <- prop.table(table(sample(c("Cara","Coroa"),size = i, replace = TRUE)))
  # Caso não ocorram "Caras" no lançamento, a probabilidade é igual a 0. É pre-
  # ciso corrigir o "NA" que o R associa automaticamente
  if (is.na(as.numeric(lancar["Cara"]))) { 
    resultado[j] <- 0
  # Caso contrário, a probabilidade de "Caras" é contabilizada
  } else {
    resultado[j] <- as.numeric(lancar["Cara"])
  }
  j <- j + 1
}

# Plotando os resultados
dado <- data.frame(lancamento = seq(1, nmax, passo), cara = resultado)
aux <- data.frame(lancamento = seq(1, nmax, passo), teorico = rep(0.5,nmax))
# tiff('lancamentoMoeda.tif', height=720, width = 1780, res=300)
ggplot() + 
  geom_line(data = dado, aes(x=lancamento, y=cara), color="blue3") +
  geom_line(data = aux, aes(x=lancamento, y=teorico), color="black", linetype = "longdash") +
  labs(x="Nº de lançamentos",y="Probabilidade de Cara") + 
  expand_limits(y=c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.25)) +
  theme_gray()
# dev.off()