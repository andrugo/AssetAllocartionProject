## Quando começo um novo script é boa pratica 
# 1 eliminar todo o conteudo dados, variveis...
rm(list = ls())

# 2 setar a pasta onde queremos trabalhar (modificar o enderezo)
setwd("/Users/andreaugolini/Library/Mobile Documents/com~apple~CloudDocs/Documents/Work UERJ/Aulas Pos/Asset_Allocation/R")

## Baixo os pacotes que preciso no trabalho
# install.packages('quantmod')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages("ggthemes")

## Cargo as librarias que preciso no trabalho
library(quantmod)
library(dplyr)
library(ggplot2)
library(ggthemes)


###########################################################
## Baixar dados de series financeira desde yahoo finance  #
###########################################################

# No pacote Quantmod existe a função get symbol que me permite baixar 
# dados direttamente da yahoo Finance no R

start <- '2010-01-01' # Data de inicio 
end <- '2019-01-30'   # Data de fin 

# Petrobras
getSymbols(Symbols = "PETR4.SA",src = "yahoo", from = start, to = end)

# Vale
getSymbols(Symbols = "VALE3.SA",src = "yahoo", from = start, to = end)

# Os simbolos de las variaveis que queremos baixar a podemos encontrar em:
# https://finance.yahoo.com/


# È possivel que algunos dados não estejam então temos que sacar esse dados utilizando a função 
# na.omit()
PETR4.SA  <- na.omit(PETR4.SA)
VALE3.SA  <- na.omit(VALE3.SA)


# Preço

ggplot(PETR4.SA, aes(Index, PETR4.SA.Adjusted)) +
  geom_line()+
  #theme_economist()+
  #theme_wsj() +
  ggtitle("Preço Petrobras") +
  xlab("Date") + ylab("Petrobras") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")


# Profit and Loss diário
end <- NROW(PETR4.SA)
start <- 1
Preço <- PETR4.SA$PETR4.SA.Adjusted
P_L_d <- as.numeric(Preço[(start+1):end])-as.numeric(Preço[start:(end-1)])
P_L_d <- cbind.data.frame(Date=index(PETR4.SA)[start:(end-1)],P_L_d)

ggplot(P_L_d, aes(Date,P_L_d)) +
  geom_line()+
  #theme_economist()+
  #theme_wsj() +
  ggtitle("P&L Petrobras") +
  xlab("Date") + ylab("Petrobras") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

# Profit and Loss diário cumulativo

cum_P_L_d <- cumsum(P_L_d$P_L_d)
cum_P_L_d <- cbind.data.frame(Date=index(PETR4.SA)[start:(end-1)],cum_P_L_d)

ggplot(cum_P_L_d, aes(Date,cum_P_L_d)) +
  geom_line()+
  #theme_economist()+
  #theme_wsj() +
  ggtitle("P&L Petrobras Cumulativo") +
  xlab("Date") + ylab("Petrobras") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

# Rendimentos simple diário
I <- (as.numeric(Preço[2:end])-as.numeric(Preço[1:(end-1)]))/as.numeric(Preço[1:(end-1)])
I <- cbind.data.frame(Date=index(PETR4.SA)[2:end],I)

ggplot(I, aes(Date,I)) +
  geom_line()+
  #theme_economist()+
  #theme_wsj() +
  ggtitle("Rendimentos simple") +
  xlab("Date") + ylab("Petrobras") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

# Rendimentos logaritmico diário
R <- log(as.numeric(Preço[2:end])/as.numeric(Preço[1:(end-1)]))
R <- cbind.data.frame(Date=index(PETR4.SA)[2:end],R)

ggplot(R, aes(Date,R)) +
  geom_line()+
  #theme_economist()+
  #theme_wsj() +
  ggtitle("Rendimentos compostos") +
  xlab("Date") + ylab("Petrobras") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

###########################
## Como fazer uma função ##  
###########################
nomefunction <- function(x,y,z){
  a <- x+y+z
  b <- x*y*z
  c <- x/y+z
  
  return <- list(a=a,b=b,c=c)
  return
  }

pateta <- nomefunction(2,3,5) 
pateta

#########################################################
# Vantagem de trabalhar com os rendimentos logarítmicos #
#########################################################
ggplot(R, aes(x=R)) + 
  geom_density(color="blue") + 
  stat_function(fun = dnorm, args = c(mean = mean(R$R), sd = sd(R$R)), col = "tomato") +
  geom_histogram(alpha = 0.5,bins=900) +
  ggtitle("Histograma(x) vs densidade(x) vs Gaussian(x)")

ggplot(R, aes(sample=R))+
  stat_qq() + 
  stat_qq_line()

# Estimativa da função de autocorrelação de uma série temporal de rendimento logaritmico
# 
acf(R$R,type = "correlation")
# Estimativa da função de autocovariância de uma série temporal de rendimento logaritmico
acf(R$R,type = "covariance")


###
# Skewness e Kurtosis
###
library(moments)
skewness(R$R)
kurtosis(R$R)

######################
# Test Jarque-Bera

jarque.test(R$R)
# Recuso a Normalidade < 0.05

#################
#Test Ljung-Box

Box.test(R$R, 1,type = "Ljung")
# Aceito H_0: Não há correlação serial com lag 1 p-value > 0.05

Box.test(R$R, 10,type = "Ljung")
# Aceito H_0: Não há correlação serial com lag até 10 p-value > 0.05

Box.test(R$R, 20,type = "Ljung")
# Recuso H_0: há correlação serial com até lag 20 p-value < 0.05
# Lembrando que é um teste conjunto

#Exemplo: Variavel Aleatoria com distribução Normal 
y=rnorm(1000)
jarque.test(y)
acf(y,type = "correlation")
Box.test(y, 20,type = "Ljung")


# Ljung Box Text minha function

.LJ.test<-function(data=data,h=20,alpha=0.95){
  n <- NROW(data)
  
  r2sum <- 0
  rho <- NULL
  for(k in 1:h){
    rho <- cor(data[1:(n-k)],data[(k+1):n])
    r2sum <- r2sum+rho^2/(n-k)
  }
Q <- r2sum*(n*(n+2))
critical <- qchisq(alpha, df=h)
p.value <- 1-pchisq(Q,h)
H0 <- ifelse(Q>critical,paste0("há correlação serial com até lag ",h, "  p.value = ", round(p.value,3), "  Q.value = ", round(Q,3)),
              paste0("não há correlação serial com até lag ",h,"    p.value = ", round(p.value,3), "  Q.value = ", round(Q,3)))
print(H0)
}
    
.LJ.test(R$R,h=20)  
.LJ.test(R$R,h=10)  
.LJ.test(R$R,h=1)  




