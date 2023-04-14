# Questão 1

-1
-1
0


# Questão 2

# X ~ NegBin (k,theta)
# Priori de Jeffreys - Beta (1/2,0)
# Posteriori - Beta (y+1/2, k-1)
y = 36
k = 6
n = y + k

# PQ - media
(y+1/2)/(y+1/2 + k-1)

# Desvio padrão
var <- ((y+1/2)*(k-1))/(((y+1/2 + k-1)^2)*(y+1/2 + k-1 + 1))
sqrt(var)


# Questão 3

# X ~ Multinomial (theta1, theta2, theta3)
# Priori de Jeffreys - Dirichlet (1/2 ,1/2, 1/2) - própria
# Posteriori - Dirichlet (x1+1/2 ,x2+1/2, x3+1/2) - própria
x1=16
x2=34
x3=8
alfa1.post = x1 + 1/2
alfa2.post = x2 + 1/2
alfa3.post = x3 + 1/2

# Letra a)
# PQ - media p1
# Marginal de p1 - Beta(alfa1.post, alfa2.post + alfa3.post)
alfa1 = alfa1.post
beta1 = alfa2.post + alfa3.post
(alfa1)/(alfa1 + beta1)
# Desvio-padrão
var1 = (alfa1*beta1)/(((alfa1+beta1)^2)*(alfa1+beta1+1))
sqrt(var1)

# Letra b)
# Marginal de p2 - Beta(alfa2.post,alfa1.post + alfa3.post)
alfa2 = alfa2.post
beta2 = alfa1.post + alfa3.post
# PQ - media p2-p1
(alfa2)/(alfa2 + beta2) - (alfa1)/(alfa1 + beta1)

# Letra c)
M<-50000 # tamanho do Monte Carlo
alfa<-c(alfa1.post,alfa2.post,alfa3.post)
post.sample<-NULL
for(m in 1:M){
  y<-rgamma(3,alfa,1)
  p<-y/sum(y)
  post.sample<-rbind(post.sample,p)
}
dif<-post.sample[,2]-post.sample[,1]
sd(dif)
