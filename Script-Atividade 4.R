#Questão 1

n<- 10
sucesso<-5
fracasso<-5

alpha <- sucesso + 1
beta <- n - sucesso + 1

#1.1 
#média da beta
alpha/(alpha+beta)

#1.2
#mediana
qbeta(0.5,alpha,beta)


#1.3
#HPD

conf<-0.95
alfa<-alpha
beta<-beta

# Minimizando o comprimento do intervalo
comp<-function(a){qbeta(conf+a,alfa,beta)-qbeta(a,alfa,beta)}
ainf<-optimize(f=comp,lower=0,upper=1-conf)$minimum
ainf
theta.i<-qbeta(ainf,alfa,beta)
theta.s<-qbeta(conf+ainf,alfa,beta)

# igualando a densidade nos dois extremos
dif<-function(a){dbeta(qbeta(conf+a,alfa,beta),alfa,beta)-
    dbeta(qbeta(a,alfa,beta),alfa,beta)}
ainf<-uniroot(f=dif,lower=0,upper=1-conf)$root
theta.i<-qbeta(ainf,alfa,beta)
theta.s<-qbeta(conf+ainf,alfa,beta)


theta.i
theta.s


#Questão 2

install.packages("EnvStats")
library(EnvStats)

n <- 10
x <- 13.5
theta <- 1
alpha1 <- 1
alpha2 <- alpha1 + n

#2.1
#média pareto
(alpha2*x)/(alpha2-1)

#2.2
#mediana da pareto
x*(2^(1/alpha2))

round(14.90482,2)

#2.3
#HPD
#pareto(, escala, shape)

conf<-0.95
alfa<- x
beta<- alpha2

# Minimizando o comprimento do intervalo
comp<-function(a){qpareto(conf+a,alfa,beta)-qpareto(a,alfa,beta)}
ainf<-optimize(f=comp,lower=0,upper=1-conf)$minimum
ainf
theta.i<-qpareto(ainf,alfa,beta)
theta.s<-qpareto(conf+ainf,alfa,beta)

# igualando a densidade nos dois extremos
dif<-function(a){dpareto(qbeta(conf+a,alfa,beta),alfa,beta)-
    dpareto(qbeta(a,alfa,beta),alfa,beta)}
ainf<-uniroot(f=dif,lower=0,upper=1-conf)$root
theta.i<-qpareto(ainf,alfa,beta)
theta.s<-qpareto(conf+ainf,alfa,beta)


theta.i
theta.s


#Questão 3

alpha3 <-17
beta3 <- 22
#3.1
#média gama inversa

beta3/(alpha3-1)

#3.2
#Desvio padrão da gama inversa

sqrt((beta3^2)/(((alpha3-1)^2)*(alpha3-2)))


#3.3
#moda da gama inversa

beta3/(alpha3+1)


#Questão 4

n= 1000
xa <- 420
alpha_a <- 1
beta_a <- xa - alpha_a
xb <- 370
alpha_b <- 1
beta_b <- xb - alpha_b
xo <- 1000-xa-xb

#4.3
#HPD para theta a
conf<-0.95
alfa<- xa+1
beta <- (xb+1)+(xo+1)


#4.1 - média da beta marginal theta a
alfa/(alfa+beta)

# Minimizando o comprimento do intervalo
comp<-function(a){qbeta(conf+a,alfa,beta)-qbeta(a,alfa,beta)}
ainf<-optimize(f=comp,lower=0,upper=1-conf)$minimum
ainf
theta.i<-qbeta(ainf,alfa,beta)
theta.s<-qbeta(conf+ainf,alfa,beta)

# igualando a densidade nos dois extremos
dif<-function(a){dbeta(qbeta(conf+a,alfa,beta),alfa,beta)-
    dbeta(qbeta(a,alfa,beta),alfa,beta)}

ainf<-uniroot(f=dif,lower=0,upper=1-conf)$root
theta.i<-qbeta(ainf,alfa,beta)
theta.s<-qbeta(conf+ainf,alfa,beta)


theta.i
theta.s


#4.4
#HPD para theta b


conf<-0.95
alfa<- xb+1
beta<- xa+1+(xo+1)

#4.2 - média da beta marginal de thetha b
alfa/(alfa+beta)

# Minimizando o comprimento do intervalo
comp<-function(a){qbeta(conf+a,alfa,beta)-qbeta(a,alfa,beta)}
ainf<-optimize(f=comp,lower=0,upper=1-conf)$minimum
ainf
theta.i<-qbeta(ainf,alfa,beta)
theta.s<-qbeta(conf+ainf,alfa,beta)

# igualando a densidade nos dois extremos
dif<-function(a){dbeta(qbeta(conf+a,alfa,beta),alfa,beta)-
    dbeta(qbeta(a,alfa,beta),alfa,beta)}
ainf<-uniroot(f=dif,lower=0,upper=1-conf)$root
theta.i<-qbeta(ainf,alfa,beta)
theta.s<-qbeta(conf+ainf,alfa,beta)


theta.i
theta.s