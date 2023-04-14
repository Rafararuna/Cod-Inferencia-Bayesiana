### QUESTÃO 1 ###

n <- 10 #lançamentos
k <- 2 #numero de coroas
x <- n-k #numero de caras
theta <- 0.52 #theta da H_a) p > theta

# 1    
1-pbinom(x-1,n,theta)

# 2
1-pnbinom(x-1,k,1-theta)


### QUESTÃO 2 ###

sigma.a <- sqrt(0.01)
sigma.b <- sqrt(0.81)

# Calculo dos percentis da mistura
y <- 0.1*2.326 #valor que ele te da nos parenteses dentro da probabilidade

F.mistura<-function(nive){0.5*pnorm(y,0,sigma.a)+0.5*pnorm(y,0,sigma.b)-(1+nive)/2}
(mist.upper<-uniroot(f=F.mistura,interval=c(0,3))$root)


### QUESTÃO 3 ###

a <- 3 #etta (aquele n estranho) = theta^a
x <- 12 #valor de X que ele te da na questao
n <- 15 #primeiro valor que ele da na questao
theta <- 0.5 #valor dado na probabilidade de theta ser maior que isso

# 1
(alfa = x + a)
(beta = n - x + 1)

# 2
1-pbeta(theta,alfa,beta)