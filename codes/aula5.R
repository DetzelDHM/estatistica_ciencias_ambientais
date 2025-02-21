# Bibliotecas utilizadas
library(ggplot2) # para os gráficos

# --- HISTOGRAMA PARA CÁLCULO DE PROBABILIDADES ---
# Série de dados utilizados: banco de dados de qualidade de água
qualIGU <- read.csv('chem_data.csv')

# A opção é pela série de Oxigênio Dissolvido no posto IG3 (bacia do Iguaçu)
OD1 <- subset(qualIGU, PONTO == "IG3", select = OD..mg.L.)
# Exclusão dos dados negativos (fisicamente impossíveis)
OD1[OD1 < 0] <- NA
# Exclusão dos dados com falhas da série
OD <- na.omit(OD1)


# Número de classes:
# Critério de Sturges (1926)
k <- ceiling(1+3.3*log10(length(NOrg$N.Org..mg.L.)))

# Obtenção do histograma
# tiff('histogramaOD.tif', height=720, width = 1780, res=300)
ggplot(OD, aes(x = 100*(OD..mg.L./sum(OD..mg.L.)))) +
  geom_histogram(colour="darkgreen", fill="white", bins=k, boundary = 1, binwidth = 1) +
  labs(x="Classes de OD (mg/L)",y="Frequência Relativa (%)") +
  scale_x_continuous(breaks=seq(0,7,1)) +
  theme_gray()
# dev.off()

# --- PLOTANDO AS DISTRIBUIÇÕES ---
# Distribuição geométrica

# Valores da variável aleatória
yGeom <- seq(1,10,1)
# Obtenção das probabilidades com a função 'dgeom', adotando p = prob
probGeom <- dgeom(yGeom, prob = 0.2)
# Gráfico
# tiff('distGeometrica.tif', height=1200, width = 1000, res=300)
geometrica <- data.frame(va = yGeom, prob = probGeom)
ggplot(geometrica, aes(x = va, y = prob)) +
  geom_col(colour="darkgreen", fill="white") +
  labs(x="Y",y="Probabilidade (%)",subtitle = "p = 0.7") +
  theme_gray()
# dev.off()

# Distribuição binomial

yBinom <- seq(1,10,1)
# Obtenção das probabilidades com a função 'dbinom', adotando p = prob
probBinom <- dbinom(x = yBinom, size = 10, prob = 0.7)
# Gráfico
# tiff('distBinomial.tif', height=1200, width = 1000, res=300)
binomial <- data.frame(va = yBinom, prob = probBinom)
ggplot(binomial, aes(x = va, y = prob)) +
  geom_col(colour="darkgreen", fill="white") +
  labs(x="Y",y="Probabilidade (%)",subtitle = "p = 0.7") +
  theme_gray()
# dev.off()