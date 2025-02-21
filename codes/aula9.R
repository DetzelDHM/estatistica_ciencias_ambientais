# Bibliotecas utilizadas
library(ggplot2) # para os gráficos

# Série de dados utilizados: banco de dados de qualidade de água
qualIGU <- read.csv('chem_data.csv')
# A opção é pela série de OD no posto IG3 (bacia do Iguaçu)
OD1 <- na.omit(subset(qualIGU, PONTO == "IG1", select = OD..mg.L.))
# Média e desvio padrão da série
media <- mean(OD1$OD..mg.L.)
desvpad <- sd(OD1$OD..mg.L.)
# Erro padrão da média
erropadrao <- desvpad/sqrt(nrow(OD1))

# Subamostras da série (números gerados com distribuição normal)
sub1 <- rnorm(12, mean = media, sd = desvpad)
sub2 <- rnorm(12, mean = media, sd = desvpad)
sub3 <- rnorm(12, mean = media, sd = desvpad)
sub4 <- rnorm(12, mean = media, sd = desvpad)
sub5 <- rnorm(12, mean = media, sd = desvpad)
sub6 <- rnorm(12, mean = media, sd = desvpad)
sub7 <- rnorm(12, mean = media, sd = desvpad)
sub8 <- rnorm(12, mean = media, sd = desvpad)
sub9 <- rnorm(12, mean = media, sd = desvpad)
sub10 <- rnorm(12, mean = media, sd = desvpad)
# Cálculo dos intervalos de confiança (90%) para cada amostra
ci1 <- t.test(sub1,conf.level = 0.90)$"conf.int"
ci2 <- t.test(sub2,conf.level = 0.90)$"conf.int"
ci3 <- t.test(sub3,conf.level = 0.90)$"conf.int"
ci4 <- t.test(sub4,conf.level = 0.90)$"conf.int"
ci5 <- t.test(sub5,conf.level = 0.90)$"conf.int"
ci6 <- t.test(sub6,conf.level = 0.90)$"conf.int"
ci7 <- t.test(sub7,conf.level = 0.90)$"conf.int"
ci8 <- t.test(sub8,conf.level = 0.90)$"conf.int"
ci9 <- t.test(sub9,conf.level = 0.90)$"conf.int"
ci10 <- t.test(sub10,conf.level = 0.90)$"conf.int"
# Cálculo das médias de cada amostra
m1 <- t.test(sub1)$"estimate"
m2 <- t.test(sub2)$"estimate"
m3 <- t.test(sub3)$"estimate"
m4 <- t.test(sub4)$"estimate"
m5 <- t.test(sub5)$"estimate"
m6 <- t.test(sub6)$"estimate"
m7 <- t.test(sub7)$"estimate"
m8 <- t.test(sub8)$"estimate"
m9 <- t.test(sub9)$"estimate"
m10 <- t.test(sub10)$"estimate"
# Montagem do data frame para o gráfico
dados <- data.frame(medias = c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10),
                    mediapop = rep(10,x = media),
                    ciinf = c(ci1[1],ci2[1],ci3[1],ci4[1],ci5[1],ci6[1],ci7[1],ci8[1],ci9[1],ci10[1]),
                    cisup = c(ci1[2],ci2[2],ci3[2],ci4[2],ci5[2],ci6[2],ci7[2],ci8[2],ci9[2],ci10[2]),
                    amostras = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"))

# Gráfico com os intervalos de confiança
# tiff('intervaloConf.tif', height=720, width = 1780, res=300)
ggplot() +
  geom_point(data = dados,aes(x = amostras,y = medias, colour = "Médias amostrais")) +
  geom_line(data = dados, aes(x = amostras, y = mediapop, colour = "Média populacional", group = 1),linetype = "longdash") +
  geom_linerange(data = dados,aes(x = amostras, ymin = ciinf, ymax = cisup),color="blue3") +
  labs(x="Amostras",y="OD (mg/L)") +
  scale_color_manual(name = "Legenda",
                     values = c("Médias amostrais" = "blue3",
                                "Média populacional" = "black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA,16)))) +
  theme_gray()
# dev.off()

# Subamostras da série (números gerados com distribuição normal)
sub1 <- OD1[1:10,]
sub2 <- OD1[11:20,]
sub3 <- OD1[21:30,]
sub4 <- OD1[31:40,]
sub5 <- OD1[41:53,]
# Cálculo dos intervalos de confiança (95%) para cada amostra
ci1 <- t.test(sub1)$"conf.int"
ci2 <- t.test(sub2)$"conf.int"
ci3 <- t.test(sub3)$"conf.int"
ci4 <- t.test(sub4)$"conf.int"
ci5 <- t.test(sub5)$"conf.int"
# Cálculo das médias de cada amostra
m1 <- t.test(sub1)$"estimate"
m2 <- t.test(sub2)$"estimate"
m3 <- t.test(sub3)$"estimate"
m4 <- t.test(sub4)$"estimate"
m5 <- t.test(sub5)$"estimate"
# Montagem do data frame para o gráfico
dados <- data.frame(medias = c(m1,m2,m3,m4,m5),
                    mediapop = rep(5,x = media),
                    ciinf = c(ci1[1],ci2[1],ci3[1],ci4[1],ci5[1]),
                    cisup = c(ci1[2],ci2[2],ci3[2],ci4[2],ci5[2]),
                    amostras = c("A1","A2","A3","A4","A5"))

# Gráfico com os intervalos de confiança
ggplot() +
  geom_point(data = dados,aes(x = amostras,y = medias, colour = "Médias amostrais")) +
  geom_line(data = dados, aes(x = amostras, y = mediapop, colour = "Média populacional", group = 1),linetype = "longdash") +
  geom_linerange(data = dados,aes(x = amostras, ymin = ciinf, ymax = cisup),color="blue3") +
  scale_color_manual(name = "Legenda",
                     values = c("Médias amostrais" = "blue3",
                                "Média populacional" = "black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank"),
                       shape = c(NA,16)))) +
  theme_gray() 
