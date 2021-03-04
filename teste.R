rm(list = ls())

## Questão 1 
# Para ler os dados de outro arquivo:
#setwd("C:\\Users\\Natalia\\Google Drive\\Teste Cientista dados\\")
#dados <- read.csv("tabela.csv",header = TRUE,sep = ";")

# Para inserir os dados:
jogadores <- c("Fernando","Thales","Diogo","Ricardo","Aline","Joao","Gustavo","Albino","Wellinton")
notas <- c(9,8,3,7,8,10,6,5,3)
jogo <- data.frame(jogadores,notas)

# Uma forma de equilibrar as equipes é separar em 3 melhores, 
# 3 medianos e 3 piores, e combinar as pontuações de forma que
# o 1º classificado do melhor grupo esteja com o último classificado
# no pior grupo, depois distribuir os integrantes do grupo mediano
# conforme as pontuações gerais (dentro de cada grupo)

#Colocar as notas em ordem decrescente

max_notas <- jogo[order(jogo$notas,decreasing=TRUE),]

n <- 9 #numero de participantes
i <- 3 #quantidade de grupos
j <- 1
#grupos

g1 <- max_notas[c(j,j+4,n),]
g2 <- max_notas[c(j+1,j+3,n-1),] 
g3 <- max_notas[c(j+2,j+5,n-2),]

g1 #grupo 1
g2 #grupo 2
g3 #grupo 3

soma <- c(sum(g1$notas),sum(g2$notas),sum(g3$notas))

mean(g1$notas) #media grupo 1
mean(g2$notas) #media grupo 2
mean(g3$notas) #media grupo 3


## Apesar da pontuação (e média) serem parecidas, o grupo g3 tem
## a menor variabilidade entre seus integrantes, é o menos habilidoso
## em pontuação, porém o mais fácil de ser trabalhado visto que é mais
## homogeneo. 

var(g1$notas) #variancia = 12.33
var(g2$notas) #variancia = 10.33
var(g3$notas) #variancia = 2.33

des_pad_g1 <- var(g1$notas)^(0.5)  # desvio-padrao 3.51
des_pad_g2 <- var(g2$notas)^(0.5)  # desvio-padrao 3.21
des_pad_g3 <- var(g3$notas)^(0.5)  # desvio-padrao 1.53


## Questão 2

# Cada semente é um evento independente, com probabilidade 0.5 (germinar ou não germinar);
# Cada semente é um ensaio de Bernoulli, portanto 320 sementes (ensaios independentes)
# X~Bin(320,0.5)
# Menos de 25 germinaram, significa que não incluiremos a probabilidade de não ter
# germinado nenhuma, isto é, exceto P(X=0)
# P(X<25) = P(X=24)+P(X=23)+ ... +P(X=1)

prob_germinar <- sum(pbinom(1:24,320,0.5)) 
prob_germinar ## probabilidade do cereal infectado germinar

pbinom(25,320,0.5)

#b)

k <- seq(0, 320, by=13.5)
fx <- pbinom(1:24,320,0.5)
length(k)
length(fx)

x11()
plot(k,fx, 
     main = "Probabilidade de sementes de aveia infectadas germinarem",
     xlab = "Sementes infectadas",
     ylab = "Probabilidade de germinar")

