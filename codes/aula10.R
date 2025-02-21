# Bibliotecas utilizadas
library(ggplot2) # para os gráficos
library(corrplot) # para gráficos da matriz de correlações
library(RColorBrewer) # paleta de cores para gráficos

# Série de dados utilizados: banco de dados de qualidade de água
qualIG2 <- read.csv('correlacao_regressao.csv')

# Seleção das séries de DBO e OD para o gráfico de dispersão
dados <- data.frame(Q = qualIG2$Q,
                    DBO = qualIG2$DBO)

# Elaboração do gráfico
# tiff('dispersao.tif', height=720, width = 720, res=300)
ggplot(dados,aes(x = Q, y = DBO)) +
  geom_point(color = "blue3") +
  labs(x="Vazão (m³/s)",y="DBO (mg/L)") +
  theme_gray()
# dev.off()

# tiff('dispersaoLog.tif', height=720, width = 720, res=300)
ggplot(dados,aes(x = log(Q), y = log(DBO))) +
  geom_point(color = "blue3") +
  labs(x="Log. Vazão (m³/s)",y="Log. DBO (mg/L)") +
  theme_gray()
# dev.off()

# --- MEDIDAS DAS CORRELAÇÕES ---
# 1. Correlação linear de Pearson
# 2. Correlação de Spearman

# 1. Determinação da correlação linear de Pearson
# Cálculo do coeficiente
cor(dados$Q,dados$DBO,method = "pearson")

# Teste de significância para o coeficiente
# H0: o coeficiente é zero
# H1: o coeficiente é diferente de zero
cor.test(dados$Q,dados$DBO,method = "pearson")

# 2. Determinação da correlação de Spearman
# Cálculo do coeficiente
cor(dados$Q,dados$DBO,method = "spearman")

# Teste de significância para o coeficiente
# H0: o coeficiente é zero
# H1: o coeficiente é diferente de zero
cor.test(dados$Q,dados$DBO,method = "spearman",exact = FALSE, continuity = TRUE)

# --- REGRESSÃO LINEAR SIMPLES ---

# Somente dados numéricos (sem datas e identificador do posto)
dados <- qualIG2[,4:ncol(qualIG2)]

# Para o exemplo, a regressão é feita com as variáveis DBO (dependente) e 
# NT (dependente)
dadosReg <- data.frame(DBO = dados$DBO, NT = dados$NT)

# Dispersão (com linha, para análise inicial)
# tiff('dispersaoDBONT_CI.tif', height=720, width = 720, res=300)
ggplot(dadosReg, aes(x = NT, y = DBO)) + 
  geom_point(shape = 1, color="blue") +
  geom_smooth(method = lm, se = TRUE, linetype="dashed",
              color="black") +
  labs(y ="DBO (mg/L)",x ="NT (mg/L)") +
  theme_gray()
# dev.off()

# Criação do modelo de regressão
reg <- lm(DBO ~ NT, data = dadosReg)
# Exibição dos resultados do ajuste
summary(reg)
# Intervalo de confiança para o parâmetro ajustado
confint(reg)

# --- REGRESSÃO LINEAR MÚLTIPLA ---

# Somente dados numéricos (sem datas e identificador do posto)
dados <- qualIG2[,4:ncol(qualIG2)]

# Verificação da multicolinearidade
matrizCorrelacoes <- cor(dados)

# Gráfico da matriz de correlações
# tiff('matrizCorrelacoes.tif', height=1080, width = 1080, res=200)
corrplot(matrizCorrelacoes, method="number", type = "upper",
         col=brewer.pal(n=8, name="RdBu"),
         tl.col="black", tl.srt=45,diag=FALSE)
# dev.off()

# Inclusão das variáveis de interesse para a regressão
dadosReg <- data.frame(DBO = dados$DBO,     # variável dependente (fixa)
                       COD = dados$COD,     # variável explicativa 1
                       N.NH4 = dados$N.NH4, # variável explicativa 2
                       NOrg = dados$N.Org,  # variável explicativa 3
                       N.NO2 = dados$N.NO2, # variável explicativa 4
                       N.NO3 = dados$N.NO3, # variável explicativa 5
                       NT = dados$NT,       # variável explicativa 6  
                       PT = dados$PT,       # variável explicativa 7
                       OD = dados$OD,       # variável explicativa 8
                       Q = dados$Q)         # variável explicativa 9

# Criação do modelo de regressão (alterar as variáveis na chamada da função!!)
reg <- lm(DBO ~ OD + NOrg + N.NH4, data = dadosReg)
# Exibição dos resultados do ajuste
summary(reg)
