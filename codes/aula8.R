# --- APLICAÇÃO DOS TESTES DE HIPÓTESE ---
# São executados 6 testes:
# 1. Teste de Wald e Wolfowitz para independência
# 2. Teste de Mann-Kendall para tendência (não estacionariedade)
# 3. Teste de Pettitt para quebra na tendência central (não estacionariedade)
# 4. Teste Qui-Quadrado para adequação do ajuste
# 5. Teste de Kolmogorov-Smirnov para adequação do ajuste
# 6. Teste de Filliben para adequação do ajuste

# Bibliotecas utilizadas
library(DescTools) # para o teste de Wald e Wolfowitz e prewhitening
library(psd)       # para o prewhitening
library(trend)     # para os testes de Mann-Kendall e Pettitt
library(EnvStats)  # para o teste Qui-Quadrado
library(ppcc)      # para o teste de Filliben

# Dados utilizados - vazões anuais em Foz do Areia (rio Iguaçu)
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
# amostra <- data.frame(vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])
amostra <- vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)]

# Teste 1: Wald e Wolfowitz para independência
RunsTest(amostra > mean(amostra))

# Opcional: aplicação de prewhitening para séries com dependência
aux <- prewhiten(ts(amostra), AR.max=1, detrend = FALSE, demean = FALSE)
# aux <- prewhiten(ts(amostra), AR.max=1, zero.pad = FALSE)
amostraWhit <- aux[["prew_ar"]]

# Teste 2: Mann-Kendall para tendência (não estacionariedade)
mk.test(amostra, alternative = "two.sided", continuity=TRUE)
mk.test(amostra, alternative = "greater", continuity=TRUE)
mk.test(amostra, alternative = "less", continuity=TRUE)

# Teste 3: Pettitt para quebra na tendência central (não estacionariedade)
p <- pettitt.test(amostra) # guarda o resultado do teste na variável 'p'
p[["p.value"]] # p-valor do teste
1931 + p[["estimate"]] # ponto de quebra da série

# Teste 4: Qui-Quadrado para adequação do ajuste
gofTest(amostra, test = "chisq", distribution = "gevd", n.classes = 7)
# Outras distribuições que podem ser usadas como atributo da entrada
# distribution no lugar de "norm": exp, gamma, gevd, lnorm, lnorm3, weibull

# Teste 5: Kolmogorov-Smirnov para adequação do ajuste (gofTest também faz "ks)
ks.test(amostra, pnorm, mean = mean(amostra), sd = sd(amostra), alternative = "two.sided")
ks.test(amostra, pgevd, loc = 550.27, scale = 204.73, shape = 0.06, alternative = "two.sided")

# Teste 6: Filliben para adequação do ajuste
ppccTest(amostra, qfn = "qnorm")
# Para certas distribuições, é preciso informar o parâmetro de forma. Por exemplo,
# para GEV:
ppccTest(amostra, qfn = "qgev", shape = 0.06305066)
# O valor do parâmetro 'shape' foi obtido pela função 'gofTest' acima


# --- PLOT DA SÉRIE NÃO ESTACIONÁRIA COM TENDÊNCIA ---
# O gráfico é feito ajustando-se uma regressão linear sobre os dados

# Bibliotecas utilizadas
library(ggplot2) # para os gráficos

# Dados utilizados - vazões anuais em Foz do Areia (rio Iguaçu)
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')
# vazoesMediasFozDoAreia <- read.csv('vazoesMaximasAnuaisFAreiaSCaxias.csv')
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])

# Ajuste da regressão
regressao <- lm(formula = Qmedia$vazoes ~ Qmedia$anos, data = Qmedia)
# Obtendo-se os coeficintes ajustados
coef <- coefficients(regressao)
b <- coef[1]
a <- coef[2]

# Elaboração do gráfico
# tiff('tendencia.tif', height=720, width = 1780, res=300)
ggplot() +
  geom_point(data = Qmedia, aes(x = anos, y = vazoes, colour="Vazões"), shape=21) + 
  geom_abline(aes(intercept = b, slope = a, colour="Tendência")) +
  labs(x="Anos",y="Vazões (m³/s)") +
  # expand_limits(y=c(0,1600)) +
  # scale_y_continuous(breaks = seq(0, 1600, by = 400)) +
  scale_color_manual(name = "Legenda",
                     values = c("Vazões" = "gray50",
                                "Tendência" = "darkgreen"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA,21)))) +
  theme_gray() 
# dev.off()