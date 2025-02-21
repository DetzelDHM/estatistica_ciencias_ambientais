# Pacote necessário
# install.packages("moments")

# Bibliotecas utilizadas
library(ggplot2) # para os gráficos
library(moments) # para cálculo do coeficiente de assimetria

# --- EXEMPLO GRÁFICO COM EIXO TRANSFORMADO ---
# Aqui utilizando a transformação logarítmica

# Carregamento dos dados: vazões máximas anuais em Foz do Areia e Salto Caxias
# A rotina para obtenção das vazões máximas está na aula2.R
dados <- read.csv("vazoesMaximasAnuaisFAreiaSCaxias.csv", header = F)
qMax <- data.frame(anos    = dados[,1],
                   FAreia  = dados[,2],
                   SCaxias = dados[,3])

# Elaboração do gráfico
# tiff('transformaLogEixos.tif', height=720, width = 1780, res=300)
ggplot(qMax, aes(x = FAreia, y = SCaxias)) +
  geom_point(color="blue3") +
  labs(x="Vazões em Foz do Areia (m³/s)",y="Vazões em Salto Caxias (m³/s)") + 
  # transformação da escala dos eixos
  coord_trans(x = "log10", y = "log10") +
  # ajuste manual dos limites dos eixos
  expand_limits(x=c(800,10000), y=c(2500, 30000)) +
  # ajustes manual das marcas de escalas (interpretação para o eixo y: as marcas
  # são mostradas de 0 a 30.000 m³/s, em escala de 7500 m³/s)
  scale_y_continuous(breaks=seq(0,30000,7500)) +
  scale_x_continuous(breaks=seq(0,10000,2000)) +
  theme_light() 
# dev.off()

# --- ESTATÍSTICAS DESCRITIVAS ---

# Série de dados utilizados: banco de dados de qualidade de água
qualIGU <- read.csv('chem_data.csv')

# A opção é pela série de Nitrogênio Orgânico no posto IG3 (bacia do Iguaçu)
NOrg1 <- subset(qualIGU, PONTO=="IG3", select=N.Org..mg.L.)

# Plot dos dados para visualização
# tiff('nitroOrg.tif', height=720, width = 1780, res=300)
ggplot(NOrg1, aes(x=1:58, y=N.Org..mg.L.)) +
  geom_point(color = "blue3") +
  labs(x="Campanhas",y="Nitrogênio Orgânico (mg/L)") + 
  theme_gray()
# dev.off()
  
# Caracterização dos dados
# Tamanho da série (incluindo falhas)
n1 <- nrow(NOrg1)
# Número de falhas (aqui representadas por NA)
nFalhas <- sum(is.na(NOrg1))
# Percentual de falhas
perFalhas <- 100*(nFalhas/n1)
# Exclusão dos dados com falhas da série
NOrg <- na.omit(NOrg1)
# Tamanho da série (sem falhas)
n <- nrow(NOrg)

# Medidas de tendência central
# Média
media <- mean(NOrg$N.Org..mg.L.)
# Mediana
mediana <- median(NOrg$N.Org..mg.L.)

# Medidas de variabilidade
# Variância
variancia <- var(NOrg$N.Org..mg.L.)
# Desvio Padrão
desvPad <- sd(NOrg$N.Org..mg.L.)
# Amplitude interquartil. Para essa métrica, o tipo 6 se refere às posições de
# plotagem de Weibull, utilizadas para a interpolação dos quartis da série
ampInterq <- IQR(NOrg$N.Org..mg.L., type = 6)
# Desvio absoluto da mediana (MAD)
desvAbsMediana <- mad(NOrg$N.Org..mg.L.)
# Coeficiente de variação
cv <- 100*(desvPad/media)

# Medidas de assimetria
# Coeficiente de assimetria. O R não possui função pronta para cálculo desse
# coeficiente. É preciso usar a biblioteca 'moments'
assimetria <- skewness(NOrg$N.Org..mg.L.)
# Assimetria quartílica. Aqui não há função pronta, então ela será calculada 
# usando a equação da métrica
percentis <- as.numeric(quantile(NOrg$N.Org..mg.L., probs = c(.25, .5, .75),
                               type = 6)) # percentis para 25%, 50% e 75%
qs <- ((percentis[3]-percentis[2]) - (percentis[2]-percentis[1])) / 
       (percentis[3]-percentis[1]) # aplicação da equação

# Impressão dos resultados no console
sprintf("Média: %.2f mg/L",media)
sprintf("Mediana: %.2f mg/L",mediana)
sprintf("Variância: %.2f (mg/L)²",variancia)
sprintf("Desvio Padrão: %.2f mg/L",desvPad)
sprintf("Amplitude interquartil: %.2f mg/L",ampInterq)
sprintf("Desvio absoluto da mediana: %.2f mg/L",desvAbsMediana)
sprintf("Coeficiente de variação: %.0f%%",cv)
sprintf("Coeficiente de assimetria: %.2f",assimetria)
sprintf("Assimetria quartílica: %.2f",qs)

# Análises rápidas podem ser feitas por meio da função 'summary', que mostra os
# valores: mínimo, máximo, mediana, média, 1º e 3º quantis (25% e 75%, resp.)
summary(NOrg$N.Org..mg.L.)