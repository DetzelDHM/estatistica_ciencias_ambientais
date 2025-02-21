# Bibliotecas utilizadas
library(ggplot2) # para os gráficos
library(lubridate) # para conversão de texto em datas
library(MASS) # para transformação Box-Cox

# --- EXEMPLOS GRÁFICOS ---
# Exemplo gráfico 1

# Dados utilizados - vazões anuais em Foz do Areia (rio Iguaçu)
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])

# Divisão da amostra em duas, com base nos últimos 30 anos
amostra1 <- Qmedia[1:match(1990,Qmedia$anos),]
amostra2 <- Qmedia[(match(1990,Qmedia$anos)+1):nrow(Qmedia),]

# Médias das amostras
media1 <- mean(amostra1$vazoes)
media2 <- mean(amostra2$vazoes)

# Gráfico das séries com as médias
dado1 <- data.frame(ano = amostra1$anos, media = rep(media1,nrow(amostra1)))
dado2 <- data.frame(ano = amostra2$anos, media = rep(media2,nrow(amostra2)))
# tiff('diferencaMedia.tif', height=720, width = 1780, res=300)
ggplot() + 
  geom_line(data = Qmedia, aes(x=anos, y=vazoes, colour = "Histórico")) +
  geom_line(data = dado1, aes(x=ano, y=media, colour = "Média 1931-1990"), linetype = "longdash") +
  geom_line(data = dado2, aes(x=ano, y=media, colour = "Média 1991-2021"), linetype = "longdash") +
  labs(x="Anos",y="Vazões (m³/s)") + 
  scale_color_manual(name = "Legenda", values = c("Histórico" = "gray70", "Média 1931-1990" = "blue", "Média 1991-2021" = "darkgreen")) +
  theme_gray()
# dev.off()


# Exemplo gráfico 2

# Série de dados utilizados: banco de dados de qualidade de água
qualIGU <- read.csv('chem_data.csv')

# A opção é pela série de DBO nos postos IG3 e IG4 (já eliminando as falhas)
amostra1 <- na.omit(subset(qualIGU, PONTO=="IG3", select = c('DATA','DBO..mg.L.')))
amostra2 <- na.omit(subset(qualIGU, PONTO=="IG4", select = c('DATA','DBO..mg.L.')))
# Conversão da coluna das datas (lida como texto) para o formato apropriado
amostra1$DATA <- dmy(amostra1$DATA)
amostra2$DATA <- dmy(amostra2$DATA)

# Média das séries
media1 <- mean(amostra1$DBO..mg.L.)
media2 <- mean(amostra2$DBO..mg.L.)

# Gráfico das séries com as médias
dado1 <- data.frame(ano = amostra1$DATA, media = rep(media1,nrow(amostra1)))
dado2 <- data.frame(ano = amostra2$DATA, media = rep(media2,nrow(amostra2)))
# tiff('diferencaMediaDBO.tif', height=720, width = 1780, res=300)
ggplot() + 
  geom_point(data = amostra1, aes(x=DATA, y=DBO..mg.L., colour = "Posto IG3")) +
  geom_point(data = amostra2, aes(x=DATA, y=DBO..mg.L., colour = "Posto IG4")) +
  geom_line(data = dado1, aes(x=ano, y=media, colour = "Média IG3"), linetype = "longdash") +
  geom_line(data = dado2, aes(x=ano, y=media, colour = "Média IG4"), linetype = "longdash") +
  scale_x_date(breaks = "12 months", date_labels = "%d %b %y") +
  labs(x="Coletas",y="DBO (mg/L)") + 
  scale_color_manual(name = "Legenda",
                     values = c("Posto IG3" = "gray70",
                                "Posto IG4" = "gray50",
                                "Média IG3" = "blue",
                                "Média IG4" = "darkgreen"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","solid","blank","blank"),
                       shape = c(NA,NA,16,16)))) +
  theme_gray() +
  theme(axis.text.x=element_text(angle=30, hjust=1))
# dev.off()

# --- TRANSFORMAÇÃO BOX-COX ---
# Dados utilizados: vazões em Foz do Areia
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])

# Executando a transformação Box-Cox
# Estimativa do parâmetro lambda
bc <- boxcox(lm(Qmedia$vazoes ~ 1), plotit = FALSE) # determina diversos lambdas
lambda <- bc$x[which.max(bc$y)] # extrai o valor exato do lambda
QmediaTransf <- (Qmedia$vazoes ^ lambda - 1) / lambda # transforma a série

# Histograma antes da transformação
# tiff('boxcoxAntes.tif', height=720, width = 720, res=300)
ggplot(Qmedia, aes(x=vazoes)) +
  geom_histogram(colour="green4", fill="white", bins=7) +
  labs(x="Classes de vazões (m³/s)",y="Frequência") +
  theme_gray()
# dev.off()

# Histograma depois da transformação
QmediaTransf <- data.frame(anos = vazoesMediasFozDoAreia[,1],
                           vazoes = QmediaTransf)
# tiff('boxcoxDepois.tif', height=720, width = 720, res=300)
ggplot(QmediaTransf, aes(x=vazoes)) +
  geom_histogram(colour="green4", fill="white", bins=7) +
  labs(x="Classes de vazões (m³/s)",y="Frequência") +
  theme_gray()
# dev.off()

# --- EXECUÇÃO DOS TESTES DE HIPÓTESE PARA VAZÕES E DBOs ---
# São executados 4 testes:
# 1. Teste t para igualdade de médias
# 2. Teste de Wilcoxon para igualdade de medianas
# 3. Teste F para igualdade de variâncias
# 4. Teste de Fligner-Killeen para igualdade de variâncias

# Dados utilizados (bloco repetido das linhas anteriores, por facilidade)
# Amostra única: Vazões médias anuais em Foz do Areia
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])
# Divisão da amostra em duas, com base nos últimos 30 anos
amostra1Vazao <- Qmedia[1:match(1990,Qmedia$anos),]
amostra2Vazao <- Qmedia[(match(1990,Qmedia$anos)+1):nrow(Qmedia),]

# Duas amostras: DBOs nos postos IG3 e IG4 do rio Iguaçu
qualIGU <- read.csv('chem_data.csv')
amostra1DBO <- na.omit(subset(qualIGU, PONTO=="IG3", select = c('DATA','DBO..mg.L.')))
amostra2DBO <- na.omit(subset(qualIGU, PONTO=="IG4", select = c('DATA','DBO..mg.L.')))
# Conversão da coluna das datas (lida como texto) para o formato apropriado
amostra1DBO$DATA <- dmy(amostra1DBO$DATA)
amostra2DBO$DATA <- dmy(amostra2DBO$DATA)

# Uniformização dos dados para os testes. As variáveis genéricas 'amostra1' e 
# 'amostra2' guardarão os dados de vazão ou DBO, de acordo com a seleção abaixo:
# amostra1 <- amostra1Vazao$vazoes
# amostra2 <- amostra2Vazao$vazoes
amostra1 <- amostra1DBO$DBO..mg.L.
amostra2 <- amostra2DBO$DBO..mg.L.

# Teste 1: t para igualdade de médias
t.test(amostra1, amostra2, alternative = "two.sided", var.equal = FALSE)
t.test(amostra1, amostra2, alternative = "greater", var.equal = FALSE)
t.test(amostra1, amostra2, alternative = "less", var.equal = FALSE)

# Teste 2: Wilcoxon para igualdade de medianas
wilcox.test(amostra1, amostra2,exact = FALSE, alternative = "two.sided")
wilcox.test(amostra1, amostra2,exact = FALSE, alternative = "greater")
wilcox.test(amostra1, amostra2,exact = FALSE, alternative = "less")

# Teste 3: F para igualdade de variâncias
var.test(amostra1, amostra2, alternative = "two.sided")
var.test(amostra1, amostra2, alternative = "greater")
var.test(amostra1, amostra2, alternative = "less")

# Teste 3: Fligner-Killeen para igualdade de variâncias
# Esse teste precisa de uma organização específica dos dados, de maneira que 
# um só vetor é aceito como dado de entrada. É preciso também identificar quais
# são as amostras nesse vetor
# Identificando as amostras 1 e 2 por meio de vetores
idAmostra1 <- cbind(amostra1,rep(1,length(amostra1)))
idAmostra2 <- cbind(amostra2,rep(2,length(amostra2)))
# Unindo ambas as matrizes
amostra <- rbind(idAmostra1,idAmostra2)
# Montando o data frame
amostra <- data.frame(valores = amostra[,1],
                      grupos = amostra[,2])

# Executando o teste
fligner.test(valores ~ grupos, amostra)
