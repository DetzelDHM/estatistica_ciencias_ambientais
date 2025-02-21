# Bibliotecas utilizadas
library(ggplot2) # para os gráficos

# --- PLOTANDO AS DISTRIBUIÇÕES ---

# DISTRIBUIÇÃO NORMAL
# Valores da variável aleatória
x <- seq(-10,10,0.01)
# Obtenção das probabilidades com a função 'dnorm'
probNormM1 <- dnorm(x, mean = 0, sd = 1)
probNormM2 <- dnorm(x, mean = 0, sd = 2)
probNormM3 <- dnorm(x, mean = 0, sd = 3)
# Gráfico
# tiff('distNormal.tif', height=720, width = 1780, res=300)
normalM1 <- data.frame(va = x, prob = probNormM1)
normalM2 <- data.frame(va = x, prob = probNormM2)
normalM3 <- data.frame(va = x, prob = probNormM3)
ggplot() +
  geom_polygon(data = normalM1, aes(x = va, y = prob),
               colour="darkgreen", fill = "darkgreen", alpha = 0.2) +
  geom_polygon(data = normalM2, aes(x = va, y = prob),
               colour="blue3", fill = "blue3", alpha = 0.2) +
  geom_polygon(data = normalM3, aes(x = va, y = prob),
               colour="gold4", fill = "gold4", alpha = 0.2) +
  labs(x="X",y="Densidade",subtitle = "Variando \u03c3") +
  theme_gray()
# dev.off()
# (unicodes mu: \u03bc sigma: \u03c3

# DISTRIBUIÇÃO LOG-NORMAL
# Valores da variável aleatória
x <- seq(0,10,0.01)
# Obtenção das probabilidades com a função 'dlnorm'
p <- dlnorm(x, meanlog = 0, sdlog = 1)
# Gráfico
# tiff('distLogNormal.tif', height = 1200, width = 1000, res=300)
xPlot <- data.frame(va = x, prob = p)
ggplot() +
  geom_polygon(data = xPlot, aes(x = va, y = prob),
               colour="darkgreen", fill = "darkgreen", alpha = 0.2) +
  labs(x="X",y="Densidade") +
  theme_gray()
# dev.off()

# DISTRIBUIÇÃO UNIFORME
# Valores da variável aleatória
x <- seq(0,1,0.01)
# Obtenção das probabilidades com a função 'dlnorm'
p <- dunif(x, min = 0, max = 1)
# Gráfico
# tiff('distLogUniforme.tif', height = 1200, width = 1000, res=300)
xPlot <- data.frame(va = x, prob = p)
ggplot() +
  geom_area(data = xPlot, aes(x = va, y = prob),
               colour="darkgreen", fill = "darkgreen", alpha = 0.2) +
  labs(x="X",y="Densidade") +
  theme_gray()
# dev.off()

# DISTRIBUIÇÃO EXPONENCIAL
# Valores da variável aleatória
x <- seq(-0.01,7,0.01)
# Obtenção das probabilidades com a função 'dlnorm'
p <- dexp(x, rate = 1)
# Gráfico
# tiff('distExp.tif', height = 1200, width = 1000, res=300)
xPlot <- data.frame(va = x, prob = p)
ggplot() +
  geom_polygon(data = xPlot, aes(x = va, y = prob),
               colour="darkgreen", fill = "darkgreen", alpha = 0.2) +
  labs(x="X",y="Densidade") +
  theme_gray()
# dev.off()

# DISTRIBUIÇÃO DE GUMBEL
library(ordinal) # pacote requerido par Gumbel
# detach("package:ordinal", unload=TRUE) - caso queira desinstalar o pacote
# Valores da variável aleatória
x <- seq(-5,15,0.01)
# Obtenção das probabilidades com a função 'dgumbel'
p <- dgumbel(x, location = 1, scale = 2)
# Gráfico
# tiff('distGumbel.tif', height = 1200, width = 1000, res=300)
xPlot <- data.frame(va = x, prob = p)
ggplot() +
  geom_polygon(data = xPlot, aes(x = va, y = prob),
               colour="darkgreen", fill = "darkgreen", alpha = 0.2) +
  labs(x="X",y="Densidade") +
  theme_gray()
# dev.off()

# DISTRIBUIÇÃO GEV
library(texmex)
# Valores da variável aleatória
x <- seq(0,15,0.01)
# Obtenção das probabilidades com a função 'dlnorm'
p <- dgev(x, mu = 2, sigma = 1, xi = 0.5)
# Gráfico
# tiff('distGEV.tif', height = 1200, width = 1000, res=300)
xPlot <- data.frame(va = x, prob = p)
ggplot() +
  geom_polygon(data = xPlot, aes(x = va, y = prob),
               colour="darkgreen", fill = "darkgreen", alpha = 0.2) +
  labs(x="X",y="Densidade") +
  theme_gray()
# dev.off()

# --- Teorema do Limite Central ---
# Exemplo com lançamento de dados

# Saídas possíveis do lançamento de um dado
dados <- c(1,2,3,4,5,6)

# Número de repetições
repeteco <- 5

# Número de simulações
n <- 1000

resultado <- c()
# Loop para o experimento
for (i in 1:n){
  resultado[i] <- mean(sample(dados,repeteco,replace = TRUE))
}

# Obtenção do histograma
res <- data.frame(x = resultado)
# tiff('histogramaLimiteCentral.tif', height=1200, width = 1000, res=300)
ggplot(data = res, aes(x=x)) +
  geom_histogram(aes(y = after_stat(density)), colour="darkgreen", fill="white",
                 bins=6, binwidth = 1) +
  geom_density(colour = "darkgreen", linetype = 2) +
  labs(x="Faces do dado",y="Frequência", subtitle = paste("Repetições =",repeteco)) +
  scale_x_continuous(breaks=seq(0,6,1)) +
  theme_gray()
# dev.off()

# --- DISTRIBUIÇÕES EMPÍRICAS ---
# Foco na FDA empírica

# Dados utilizados - vazões anuais em Foz do Areia (rio Iguaçu)
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])

# Traçado FDA empírica
# tiff('FDAEmpirica_FdA.tif', height=720, width = 1780, res=300)
p <- ggplot(Qmedia, aes(vazoes))
p <- p + stat_ecdf(geom = "point", pad = FALSE, colour = "darkgreen", fill = "white", shape = 21) +
  labs(x="Vazões (m³/s)",y="Frequências acumuladas") +
  theme_gray()
print(p)
# dev.off()


# Traçado FDA teórica Normal
# Estatísticas para plot
media <- mean(Qmedia$vazoes)
desvpad <- sd(Qmedia$vazoes)
# tiff('FDATeoricaN_FdA.tif', height=720, width = 1780, res=300)
p <- ggplot(Qmedia, aes(vazoes))
p <- p + geom_point(stat = "ecdf", colour = "darkgreen", fill = "white", shape = 21) +
  stat_function(fun = pnorm, color = "black", args = list(media, desvpad)) +
  labs(x="Vazões (m³/s)",y="Frequências acumuladas",
       subtitle = "Comparação com distribuição normal") +
  theme_gray()
print(p)
# dev.off()

# Traçado FDA teórica Log-Normal
# Estatísticas para plot
media <- mean(log(Qmedia$vazoes))
desvpad <- sd(log(Qmedia$vazoes))
# tiff('FDATeoricaLN_FdA.tif', height=720, width = 1780, res=300)
p <- ggplot(Qmedia, aes(vazoes))
p <- p + geom_point(stat = "ecdf", colour = "darkgreen", fill = "white", shape = 21) +
  stat_function(fun = plnorm, color = "black", args = list(media, desvpad)) +
  labs(x="Vazões (m³/s)",y="Frequências acumuladas",
       subtitle = "Comparação com distribuição log-normal") +
  theme_gray()
print(p)
# dev.off()
