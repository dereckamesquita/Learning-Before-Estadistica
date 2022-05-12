###########################################
## CAPM tests
## Summer School of Economics and Finance
###########################################

# Data source:
# collected from Kenneth's French website
https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#
## Data description:
# 100 portfolios 
#
## Portfolio construction:
# The portfolios, which are constructed at the end of each June, 
# are the intersections of 10 portfolios formed on size (market equity, ME)
# and 10 portfolios formed on investment (Inv). 
# Investment is the change in total assets from the fiscal year ending in year t-2 
# to the fiscal year ending in t-1, divided by t-2 total assets.
#
##Additional considerations:
# The portfolios for July of year t to June of t+1 include all NYSE, AMEX, 
# and NASDAQ stocks for which we have market equity data for June of t and total assets data for t-2 and t-1.


#Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DescTools)
library(GRS.test)
options(scipen=999)

#Working directory

#Data
data<-read_xlsx("EAP/100_Portfolios.xlsx",sheet = "Data") %>% as.data.frame()
head(data)

#Portafolios
#Sample
n_begin <- 1
n_end <- 666
#From Jan 1964 to Jun 2019

#portfolio
port_begin <- 1
port_end <- 99
portfolios <- data[n_begin:n_end,(port_begin+1):(port_end+1)]
View(portfolios)

#Fama-French (1993) factors:
Re_market <- data[n_begin:n_end,102]
SMB	<- data[n_begin:n_end,103]
HML	<- data[n_begin:n_end,104]

View(Re_market)
#Risk-free interest rate
RF <- data[n_begin:n_end,105]


###############################
## Time-Series Regression (OLS)

# Econometric Model: Re_q = alpha + beta*Re_m + epsilon
# Re = retorno en exceso

n <- dim(portfolios)[2]

#,2 indica que queremos saber el numero de columnas. 
#Si fuese 1, seria el numero de filas

obs <- dim(portfolios)[1]
X <- data.frame(c=rep(1,obs),x=Re_market) %>% as.matrix()  

Nvar <- dim(X)[2]

coef <- list()
yhat<- list()


#[[ ]]nos permite acceder a colummnas dentro de lista
mylist <- list(num=42, greeting="Hello, world", hola = c(2,3,4))
mylist[[3]]





#99 regressiones, para cada portafolio, para estimar los parametros via OLS
for( j in 1:n){
  Y = portfolios[,j] - RF #Excess Return Portfolio j
  coef[[j]] = solve(t(X)%*%X)%*%t(X)%*%Y  # alpha and beta ((X'X)-1 X'Y)
  yhat[[j]] = X%*%(coef[[j]] %>% as.matrix())
}
yhatdf<- do.call("cbind",yhat) %>% as.data.frame
#do.call(rnorm, list(5,10,1)) # equivalente a rnorm(5, mean = 10, sd=1)

residuasl <- portfolios-yhatdf
residuasl
View(yhatdf)

# Unbiased estimator of variance of disturbances (in population model)
# variancia de paramatros estimados depende de variancia de residuos
s2 = list()
for( i in 1:n){
  s2[[i]] = (t(residuasl[,i])%*%residuasl[,i])/(obs-Nvar)
}
s2=matrix(do.call("c",s2),nrow=1)
View(s2)
# Variance of coef = [alpha beta]
variance_coef <- list()
for(k in 1:n){
  # 1 calcular var cov de k1los coeficientes  
  var_c = s2[k]*solve(t(X)%*%X)
  # 2 Extract the var(alpha) and var(beta)
  varc1 = c(var_c[1,1],var_c[2,2])
  # 3 Guardar el vector var(alpha) and var(beta)
  variance_coef[[k]] = varc1 # %1st row is var(alpha), 2nd row is var(beta)
}
variance_coef<-do.call("cbind",variance_coef)
sd = variance_coef^(0.5)
#alpha and beta
alpha = do.call("cbind",coef)[1,]
beta = do.call("cbind",coef)[2,]

# t-statistic 
t_alpha = alpha/sd[1,] #al indicar . hacemos una operacion elemento a elemento
t_beta = beta/sd[2,] 

#plots
par(mfrow=c(2,2))
plot(1:n,alpha,xlab = "portafolio", title = 'Estimated Alpha')
barplot(t_alpha,ylab="t_alpha")
plot(1:n,beta,xlab = "portafolio", title = 'Estimated Beta')
barplot(t_beta,ylab="t_beta")


## CAPM CS prediction: E(Re) = beta*E[Rem]

# estimamos el retorno en exceso de cada portfolio
Exc_return = portfolios - do.call(cbind, replicate(99, RF, simplify=FALSE)) # Excess returns for every portfolio
# n= No of portfolios

# Does CAPM fit the Exp return? 
# calculamos el retorno en exceso medio(promedio)
Exp_Exc_return = sapply(Exc_return,mean) #=E(Re): mean for every exc return port
Exp_Exc_market_return = mean(Re_market)  #=E[Rem]

# Does CAPM fit the Exp return? PLOT: E(Re)vs beta*E[Rem] 
# data observada vs modelo
Estimate_Exp_Exc_return = beta*Exp_Exc_market_return #=beta*E[Rem]


#-- Exp Exc Return: Data vs Estimation: E(Re) = beta*E[Rem]
# scatter(beta*E[Rem],E(Re))
plot(Estimate_Exp_Exc_return,Exp_Exc_return)
abline(coef = c(0,1))
#No se cumple que todos los puntos esten sobre la linea de 45 ya que hemos
#asumidos que el alfa = 0 cuando en realidad no lo es!!
  
#-- BETA vs Exp Return
plot(beta,Exp_Exc_return)

## Hypothesis Testing (Individually: alpha)
# Ho: B3 = 0, Ha: B3 <> 0
alpha_h0 = 0    # my hypothesis.
df = obs - Nvar # degree of freedom (obs disponibles para realizar las estimaciones)
sig = 0.05      # significant level
t_critic = qt(1-sig,df)

#alpha
p_value_alpha = 2*(1 - pnorm(abs(t_alpha),lower.tail = T));

#observar cuantos alphas tuvieron alpha <5% y son diferentes de 0
plot(alpha,100.*p_value_alpha)
#plot(alpha,rep(5,n))

#beta
p_value_beta = 2*(1 - pnorm(abs(t_beta),lower.tail = T));
# if p_value<5% then alpha is different to 0
plot(beta,100.*p_value_beta)
plot(beta,rep(5,n))

# GRS test
# H0: all the alphas are zero
ret.mat <- data[n_begin:n_end,(port_begin+1):(port_end-2)]
factor.mat <- data[n_begin:n_end,102]

head(ret.mat)
head(factor.mat)

GRS.test(ret.mat,factor.mat)$GRS.stat
GRS.test(ret.mat,factor.mat)$GRS.pval

#what if we include FF3 factors

factor.matff <- data[n_begin:n_end,102:104]
GRS.test(ret.mat,factor.matff)$GRS.stat
GRS.test(ret.mat,factor.matff)$GRS.pval

