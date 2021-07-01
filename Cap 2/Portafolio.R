# Instalamos y llamamos a los paquetes
desinsta
install.packages("fPortfolio")
install.packages("lpSolve")


remove.packages("fPortfolio")

library("fPortfolio")
library("lpSolve")

Data = SMALLCAP.RET
head(Data)

#De esta forma estamos solicitando todas las filas 
Data = Data[,c("BKE", "FCEL", "GG", "OII", "SEB")]
str(Data)

matcovData = covEstimator(Data)

matcovData
# Especificación de restriccion de media varianza

especificacion_corta = portfolioSpec() 

setSolver(especificacion_corta) = "solveRshortExact"

# Frontera eficiente
# Restricción (constraints="Short"): 
# Esto quiere decir que podemos vender los activos, esto se refleja en el signo negativo 
# de Portfolio Weights:

shortFrontier = portfolioFrontier(Data,spec=especificacion_corta,constraints="Short")

# Por default reporta los portafolio: 1,13,25,37,50

shortFrontier


frontera = shortFrontier

frontierPlot(frontera,frontier = "both",risk="Sigma",type="l",col = c("blue", "grey"))


# Frontera eficiente
frontierPlot(frontera,frontier = "both",risk="Sigma",type="l",col = c("blue", "grey"))

# Portafolio de mínima varianza
minvariancePoints(frontera,pch=19,col="red")

# Otros portafolios
singleAssetPoints(frontera,risk="Sigma",pch=19,cex=1.5,col=topo.colors(6))

