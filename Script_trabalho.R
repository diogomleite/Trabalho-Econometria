# Retirando nota??o cient?fica
options(scipen=999) #Retira
options(scipen=0) #Insere

# Carregar os pacotes que ser?o usados
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2)

# Descri??o das vari?veis
glimpse(dados_bh)


dados_bh$continente <- as.factor(dados_bh$continente)
dados_bh$via <- as.factor(dados_bh$via)
dados_bh$pacote <- as.factor(dados_bh$pacote)
dados_bh$primeira <- as.factor(dados_bh$primeira)
dados_bh$marca <- as.factor(dados_bh$marca)
dados_bh$nacionalidade <- as.factor(dados_bh$nacionalidade)

# Estabelecendo classes de referencia
table(dados_bh$continente)
dados_bh$continente <-relevel(dados_bh$continente, ref="AMÉRICA DO SUL")
table(dados_bh$via)
dados_bh$via <-relevel(dados_bh$via, ref="TERRESTRE")
table(dados_bh$pacote)
dados_bh$pacote <-relevel(dados_bh$pacote, ref="Não")
table(dados_bh$primeira)
dados_bh$primeira <-relevel(dados_bh$primeira, ref="Sim")
table(dados_bh$marca)
dados_bh$marca <-relevel(dados_bh$marca, ref="Não")
dados_bh$nacionalidade <- relevel(dados_bh$nacionalidade, ref = "BRASIL")

# Grupo 1: Gasto ~ caracter?sticas individuais
reg1 = lm(gasto ~ nacionalidade, data = dados_bh)
reg2 = lm(gasto ~ nacionalidade + sexo, data = dados_bh)
reg3 = lm(gasto ~ vezes + sexo + nacionalidade, data = dados_bh)
reg4 = lm(gasto ~ vezes + sexo + marca + nacionalidade, data = dados_bh)
summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)

# Grupo 2: Pernoite ~ caracter?sticas individuais
reg5 = lm(pernoite ~ idade, data = dados_bh)
reg6 = lm(pernoite ~ sexo + idade, data = dados_bh)
reg7 = lm(pernoite ~ sexo + idade + nacionalidade, data = dados_bh)
reg8 = lm(pernoite ~ sexo + idade + nacionalidade + motivo, data = dados_bh)
summary(reg5)

# Grupo 3: Gasto ~ caracter?sticas da viagem
reg9 =  lm(gasto ~ pessoas, data = dados_bh)
reg10 = lm(gasto ~ motivo + pessoas, data = dados_bh)
reg11 = lm(gasto ~ motivo + pessoas + hospedagem, data = dados_bh)
reg12 = lm(gasto ~ motivo + pessoas + hospedagem + continente, data = dados_bh)
summary(reg9)

# Grupo 4: Pernoite ~ caracter?sticas da viagem
reg13 = lm(pernoite ~ hospedagem, data = dados_bh)
reg14 = lm(pernoite ~ hospedagem + atividades, data = dados_bh)
reg15 = lm(pernoite ~ via + hospedagem + marca + atividades, data = dados_bh)
reg16 = lm(pernoite ~ motivo + via + pessoas + hospedagem + marca + atividades, data = dados_bh)

summary(reg15)

# An?lise
install.packages("stargazer")
require(stargazer)
stargazer(reg1, reg2, reg3, reg4, type="latex", font.size = 'small', column.sep.width = '2pt', title = "Gasto vesus características individuais") # Gasto + indiv?duos
stargazer(reg5, reg6, reg7, reg8, type="text") # Pernoite + indiv?duos
stargazer(reg9, reg10, reg11, reg12, type="text") # Gasto + Viagem
stargazer(reg13, reg14, reg15, reg16, type="text") # Pernoite + Viagem

#Exportando
getwd()
stargazer(reg1, reg2, reg3, reg4, type="html", title = "Gasto versus características individuais", out="Gasto_individual.html")
stargazer(reg5, reg6, reg7, reg8, type="latex", title = "Permanência versus características individuais", out="Pernoite_individual.html")
stargazer(reg9, reg10, reg11, reg12, type="latex", title = "Gasto versus características da viagem", out="Gasto_viagem.html")
stargazer(reg13, reg14, reg15, reg16, type="latex", title = "Permanência versus características da viagem", out="Pernoite_viagem.html")

library(dplyr)
library(cowplot)

if(!require(GGally)) install.packages("GGally")
dados_bh2 <- subset(dados_bh, select = c(pernoite, gasto, pessoas, vezes, idade, atividades))
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "pink", ...)
}
pairs(dados_bh2, diag.panel = panel.hist)
ggpairs(dados_bh2, lower = list(continuous = "smooth"))
ggcorr(dados_bh, label = T)

# Carregar os pacotes que ser?o usados
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2)

table(dados_bh$hospedagem)

### Multicolinearidade: VIF > 10
vif(reg1)
vif2 = vif(reg2)
vif3 = vif(reg3)
vif4 = vif(reg4)
vif(reg5)
vif6 = vif(reg6)
vif7 = vif(reg7)
vif8 = vif(reg8)
vif9 = vif(reg9)
vif10 = vif(reg10)
vif11 = vif(reg11)
vif12 = vif(reg12)
vif(reg13)
x = vif(reg14)
y = vif(reg15)
z = vif(reg16)

stargazer(vif2, vif3, vif4, vif6, vif7, vif8, vif10, vif11, vif12, type="html", out = "Vifs2.html")

# teste Breusch-Pagan
install.packages('lmtest') # Instalando pacote
library(lmtest) # Verificar disponibilidade

# Testes formais
# 1o Breusch-Pagan
bptest (reg1)
bptest (reg2)
bptest (reg3)
bptest (reg4)
bptest (reg5)
bptest (reg6)
bptest (reg7)
bptest (reg8)
bptest (reg9)
bptest (reg10)
bptest (reg11)
bptest (reg12)
bptest (reg13)
bptest (reg14)
bptest (reg15)
bptest (reg16)

# Esse e um teste de homocedasticidade
# Se o valor-p se tornar "pequeno", a hipotese nula sera rejeitada

# 2o Goldfeld-Quandt test
gqtest(reg1)
gqtest(reg2)
gqtest(reg3)
gqtest(reg4)
gqtest(reg5)
gqtest(reg6)
gqtest(reg7)
gqtest(reg8)
gqtest(reg9)
gqtest(reg10)
gqtest(reg11)
gqtest(reg12)
gqtest (reg13)
gqtest (reg14)
gqtest (reg15)
gqtest (reg16)

# Rejeitando H0 pode-se afirmar que ha diferenca entre variancias com probabilidade de erro igual ao p-valor.

# Grafico Heterocedasticidade
plot(reg1)
plot(reg1, residuals(reg1),xlab="Valores Ajustados", ylab="Residuos")
abline(h=0)
res1 = residuals(reg1)
hist(res1, main = "Histograma dos resíduos")

par(mfrow = c(4, 4))
plot(reg1, which = 1)

plot(reg2, which = 1)

plot(reg3, which = 1)

plot(reg4, which = 1)

plot(reg5, which = 1)

plot(reg6, which = 1)

plot(reg7, which = 1)

plot(reg8, which = 1)

plot(reg9, which = 1)

plot(reg10, which = 1)

plot(reg11, which = 1)

plot(reg12, which = 1)

plot(reg13, which = 1)

plot(reg14, which = 1)

plot(reg15, which = 1)

plot(reg16, which = 1)

library(ggplot2)
library(cowplot)
install.packages("gridGraphics")
library(gridGraphics)

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, ncol = 4)

table(dados_bh$pessoas)
