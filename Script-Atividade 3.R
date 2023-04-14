## Questão 1

#Defina os evendos A (coroa 5xcara) e B (cara 5xcoroa)
#Probabilidade
A = 1/6
B = 5/6
theta = 1/2

# 1.1 probabilidade de cara no 1º lançamento
(P_cara_1_lancamento<-A*theta+B*theta) 
# 1.2 probabilidade de B dado Cara no 1º lançamento
(P_B_dado_cara_no_1_lancamento<-B*theta/P_cara_1_lancamento)
# ou 1.2 probabilidade de A dado Cara no 1º lançamento
(P_A_dado_cara_no_1_lancamento<-A*theta/P_cara_1_lancamento)
# 1.3 probabilidade de c2 dado c1
P_C2_e_C1<-A*A*theta+B*B*theta
(P_C2_dado_C1<-round(P_C2_e_C1/P_cara_1_lancamento,2))


## Questão 2

# 2.1
alfa <- 4
s <- 10
betaa <- 4
n <- 12

(esperado <- (alfa+s)/(betaa+n))

# 2.2
(desvio <- sqrt((alfa+s)/((betaa+n)^2)))

# 2.3 probabilidade de theta ser maior que x
x <- 1
1-pgamma(x, s+alfa, n+alfa)


## Questão 3

n1 = 10
sigma2 = 2.3^2
mu0 = 7
tau2 = 1^2
media = 10

valor1 = 7.99
valor2 = 9.93

# 3.1 valor esperado de mu a posteriori
(mu<-round((1/tau2*mu0+n1/sigma2*media)/(1/tau2+n1/sigma2),2))

# 3.2
var<-(1)/(1/tau2+n1/sigma2) 
(dp<-round(sqrt(var),2))

# 3.3
(distribuicao<-c(paste0("Normal~(",mu,",",dp,")")))
(round(pnorm(valor2,mu,dp)-pnorm(valor1,mu,dp),2))


## Questão 4

n2 = 35
t_barra = 3.9
s1 = t_barra*n2
beta1 = 0.01
alfa1 = 0.01

# 4.1
(beta1+s1)/(alfa1+n2-1)

# 4.2
var1 <- ((beta1+s1)^2)/(((alfa1+n2-1)^2)*(alfa1+n2-2))
sqrt(var1)

# 4.3
# paciente sobreviver pelo menos y anos
y <- 6
if (!require('Pareto')) install.packages('Pareto'); library('Pareto')

alfa2 <- (alfa1+n2) # shape
k <- (beta1+s1) # location

1-pPareto(k+y, k, alfa2)