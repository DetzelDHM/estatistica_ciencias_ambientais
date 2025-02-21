# Instalação dos pacotes necessários para este código (requer conexão 
# com a internet)
# install.packages("ggplot2")
# install.packages("lubridate")

# Bibliotecas utilizadas
library(ggplot2) # para os gráficos
library(lubridate) # para conversão de texto em datas

# --- EXEMPLOS OUTLIERS ---

# 1. Gráficos vazões diárias Salto Caxias e Foz do Areia
vazoesDiariasSaltoCaxias <- read.csv('vazoesDiariasSaltoCaxias.csv', header=FALSE)
vazoesDiariasFozDoAreia <- read.csv('vazoesDiariasFozDoAreia.csv', header=FALSE)

# Montagem do data frame com os dias e as vazões (padronizadas pela média)
# Salto Caxias
QdiariaSaltoCaxias <- data.frame(
  dias = vazoesDiariasSaltoCaxias[,1],
  vazoes = vazoesDiariasSaltoCaxias[,2]/mean(vazoesDiariasSaltoCaxias[,2]))
# Foz do Areia
QdiariaFozDoAreia <- data.frame(
  dias = vazoesDiariasFozDoAreia[,1],
  vazoes = vazoesDiariasFozDoAreia[,2]/mean(vazoesDiariasFozDoAreia[,2]))

# Conversões das colunas das datas (lida como texto) para o formato apropriado
QdiariaSaltoCaxias$dias <- dmy(QdiariaSaltoCaxias$dias)
QdiariaFozDoAreia$dias <- dmy(QdiariaFozDoAreia$dias)

# *** Processo para determinação da vazão máxima anual ***

# Manipulação do vetor de datas para extração das vazões a cada ano. Essa infor-
# mação será usada como indexador do loop que fará os cálculos
# Foz do Areia
datasFozDoAreia <- data.frame(datas=QdiariaFozDoAreia$dias)
datasFozDoAreia$datas <- as.Date(datasFozDoAreia$datas)
datasFozDoAreia <- transform(datasFozDoAreia, ano=format(datas,"%Y"),
          mes=format(datas,"%m"),
          dia=format(datas,"%d"))
# Concatenação da data (agora como vetor) e vazões
QdFozDoAreia <- cbind(datasFozDoAreia,vazoes=QdiariaFozDoAreia$vazoes)

# Salto Caxias
datasSaltoCaxias <- data.frame(datas=QdiariaSaltoCaxias$dias)
datasSaltoCaxias$datas <- as.Date(datasSaltoCaxias$datas)
datasSaltoCaxias <- transform(datasSaltoCaxias, ano=format(datas,"%Y"),
                             mes=format(datas,"%m"),
                             dia=format(datas,"%d"))
# Concatenação da data (agora como vetor) e vazões
QdSaltoCaxias <- cbind(datasSaltoCaxias,vazoes=QdiariaSaltoCaxias$vazoes)

# Comando para extração dos anos do histórico, sem repetições
anosFozDoAreia <- as.numeric(unique(QdFozDoAreia$ano))
anosSaltoCaxias <- as.numeric(unique(QdSaltoCaxias$ano))

# Loops para determinação das vazões máximas
# Foz do Areia
QmaxAnualFozDoAreia <- vector()
# Determinação das vazões máximas anuais a partir do indexador acima
for (i in 1:length(anosFozDoAreia)) {
  QmaxAnualFozDoAreia[i] <- max(QdFozDoAreia[QdFozDoAreia$ano==anosFozDoAreia[i],5])
}
# Inserção dos anos
QmaxAnualFozDoAreia <- matrix(QmaxAnualFozDoAreia)
QmaxAnualFozDoAreia<- data.frame(anos=anosFozDoAreia,vazoes=QmaxAnualFozDoAreia)

# Salto Caxias
QmaxAnualSaltoCaxias <- vector()
# Determinação das vazões máximas anuais a partir do indexador acima
for (i in 1:length(anosSaltoCaxias)) {
  QmaxAnualSaltoCaxias[i] <- max(QdSaltoCaxias[QdSaltoCaxias$ano==anosSaltoCaxias[i],5])
}
# Inserção dos anos
QmaxAnualSaltoCaxias <- matrix(QmaxAnualSaltoCaxias)
QmaxAnualSaltoCaxias<- data.frame(anos=anosSaltoCaxias,vazoes=QmaxAnualSaltoCaxias)

# Plot da série de Foz do Areia com possível outlier
# tiff('vazoesMaximasFozDoAreia.tif', height=720, width = 1780, res=300)
ggplot(QmaxAnualFozDoAreia, aes(x=anos, y=vazoes, colour="Foz do Areia")) +
  geom_point() +
  labs(x="Ano",y="Vazão adm. (m³/s)") + 
  theme_light() +
  scale_colour_manual(values = c("blue3")) +
  theme_light() +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")
# dev.off()

# Comparação entre duas séries no mesmo rio (confirmação de que não é outlier)
# tiff('vazoesMaximasFDAeSC.tif', height=720, width = 1780, res=300)
ggplot() +
  geom_line(data = QmaxAnualFozDoAreia, aes(x=anos, y=vazoes, colour="Foz do Areia")) +
  geom_line(data = QmaxAnualSaltoCaxias, aes(x=anos, y=vazoes, colour="Salto Caxias")) +
  labs(x="Ano",y="Vazão adm. (m³/s)", colour="Séries") +
  scale_colour_manual(values = c("blue3", "red3")) +
  theme_light() +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom")
# dev.off()

# --- REPRESENTAÇÃO GRÁFICA DE DADOS ---

# Dados utilizados - vazões anuais em Foz do Areia (rio Iguaçu)
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])

# 1. Série temporal
# tiff('serieTemporal.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(x=anos, y=vazoes)) + 
  scale_y_continuous(breaks = seq(0, 1600, by = 400)) +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  geom_line(color="blue3") +
  labs(x="Ano",y="Vazão (m³/s)") + 
  theme_gray()
# dev.off()

# 2. Histograma

# Número de classes:
# Critério de Iman and Conover (1983) (2^k >= n)
condicao = FALSE
k1 <- 1
while (condicao == FALSE) {
  if (2^k1 < length(Qmedia$vazoes)) {
    condicao == FALSE
  } else {
    break
  }
k1 <- k1 + 1
}
# Critério de Sturges (1926)
k2 <- ceiling(1+3.3*log10(length(Qmedia$vazoes)))

# Obtenção do histograma
# tiff('histograma.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(x=vazoes)) +
  geom_histogram(colour="blue3", fill="white", bins=k2) +
  labs(x="Classes de vazões (m³/s)",y="Frequência") +
  theme_gray()
# dev.off()

# 3. Densidade
# tiff('densidade.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(x=vazoes)) +
  geom_density(colour="blue3", fill="blue3", alpha=0.3) +
  labs(x="Vazões (m³/s)",y="Densidade") +
  theme_gray()
# dev.off()

# 4. Frequência acumulada
# tiff('cumulativeFrequency.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(vazoes)) +
  stat_ecdf(geom = "point", pad = FALSE, colour = "blue3") +
  labs(x="Vazões (m³/s)",y="Frequências acumuladas") +
  theme_gray()
# dev.off()

# 5. Boxplot
# Simples
# tiff('boxplot.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(y=vazoes)) +
  geom_boxplot(fill="blue3", color="black", alpha=0.3) +
  expand_limits(y=c(200,1601)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
  coord_flip() +
  labs(y="Vazões (m³/s)") +
  theme_gray() +
  theme(axis.text.y=element_blank())
# dev.off()

# Com jitters
# tiff('boxplotjitter.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(x=1,y=vazoes)) +
  geom_boxplot(fill="blue3", color="black", alpha=0.3) +
  geom_jitter(color="red3", size=1.5, alpha=0.3) +
  expand_limits(y=c(200,1601)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
  coord_flip() +
  labs(y="Vazões (m³/s)") +
  theme_gray() +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
# dev.off()

# Violin plot
# tiff('boxplotviolin.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(x=1,y=vazoes)) +
  geom_violin(trim=FALSE,width=1.5,fill="blue3",alpha=0.3) +
  geom_jitter(color="red3", size=1.5, alpha=0.3) +
  geom_boxplot(color="gray", alpha=0.5) +
  expand_limits(y=c(200,1601)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
  coord_flip() +
  labs(y="Vazões (m³/s)") +
  theme_gray() +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
# dev.off()