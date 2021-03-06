"
P207 Sexo
1. Hombre
2. Mujer


P300A <- ¿Cuál es el Idioma o lengua materna que aprendió en su niñez? 2 0 N
1. Quechua
2. Aimara
3. Otra lengua nativa
4. Castellano
6. Portugués
7. Otra lengua extranjera
8. No escucha/no habla
9. Lengua de señas peruanas
10. Ashaninka
11. Awajún/Aguarun
12. Shipibo – Konibo
13. Shawi / Chayahuita
14. Matsigenka / Machiguenga
15. Achuar
Rango 1-15


P301A <- ¿Cuál es el último año o grado de estudios y nivel que aprobó? 
1. Sin nivel
2. Educación inicial
3. Primaria incompleta
4. Primaria completa
5. Secundaria incompleta
6. Secundaria completa
7. Superior no universitaria Incompleta
8. Superior no universitaria completa
9. Superior universitaria incompleta
10. Superior universitaria completa
11. Maestria/Doctorado
12. Básica especial
Rango 1-12


P401H1 <- ¿Tiene Ud. limitaciones de forma permanente, para: Moverse o caminar, para 
1. Si
2. No
Rango 1-2

P401H2 ¿Tiene Ud. limitaciones de forma permanente, para: Ver, aun usando anteojos? 
  1. Si
2. No
Rango 1-2

P401H3 <- ¿Tiene Ud. limitaciones de forma permanente, para: Hablar o comunicarse, aún
usando el lenguaje de señas u otro? 
  1. Si
2. No
Rango 1-2

P401H4 ¿Tiene Ud. limitaciones de forma permanente, para: Oír, aún usando audífonos
?
  1. Si
2. No
Rango 1-2

P401H5 <- ¿Tiene Ud. limitaciones de forma permanente, para: Entender o aprender
(concentrarse y recordar)?
  1. Si
2. No
Rango 1-2

P507 <- Ud. se desempeño en su ocupación principal o negocio como:
1. Empleador o patrono
2. Trabajador Independiente
3. Empleado
4. Obrero
5. Trabajador Familiar No Remunerado
6. Trabajador del Hogar
7. Otro
Rango 1-7

P513T ¿Cuántas horas trabajó la semana pasada, en su ocupación principal, el día: Total
P524A1
¿Cuánto fue su ingreso total en el(la) ... anterior, incluyendo las horas extras,
bonificaciones, pago por concepto de refrigerio, movilidad, comisiones, etc.? - Monto
S/. - Ingreso Total
"
#Simulacion
set.seed(2021)
obs=15
x1 <- sample(1:4,obs, replace=T)
x2 <- sample(1:2,obs, replace=T)
y <- sample(1000:1500, obs, replace=T)

levelsx1 = c("obrero","empleado","gerente","dueño")

#Forma 1
for (a in (1:4)){
  x1[x1==a]=levelsx1[a]
}

x1d<- factor(x1,levels =levelsx1)
x1d
head(x1)
str(x1)
model1<- model.matrix(y~x2+x1d)
head(model1[,-1])

#forma 2

x1[x1=="1"]="obrero"
x1[x1=="2"]="empleado"
x1[x1=="3"]="gerente"
x1[x1=="4"]="dueño"

x1d<- factor(x1,levels =c("obrero","empleado","gerente","dueño" ))



x1<- ordered(x1)
levels(x1) = c("obrero","empleado","gerente","dueño")
dfprueba<- data.frame(y,x1,x2)

head(x1)
str(x1)
model1<- model.matrix(y~x2+x1d)

head(model1[,-1])

#Modelado
library(ggplot2)

dfprueba<- data.frame(y,x1d,x2)

ggplot(dfprueba, aes(x=x2, y=y, color=x1d)) + 
  geom_point() + theme_light()


##
Precio <- c(12, 15, 25, 11, 16, 7)
Area <- c(3, 4, 1, 6, 5, 3)
Pisci <- factor(x=c('Grande', 'Sin', 'Pequena', 'Pequena', 'Sin', 'Grande'),
                levels=c('Sin','Pequena','Grande'))

model.matrix(Precio ~ Area + Pisci)

##

dfprueba<- data.frame(y,x1,x2)
plot(dfprueba)

dfprueba$x1 <- factor(dfprueba$x1, levels = factores, ordered = T)

plot(dfprueba$x1)

table(x1)

res <- model.matrix(~x1, data = dfprueba)
head(res[, -1])


##Importar la data

ruta <-"E:/Github/Learning-Before-Estadistica/Enaho/data.csv"
data <- read.csv(ruta, header=TRUE)

data$P524A1=as.numeric(data$P524A1)
data$P524A1
hist(data$P524A1 )


#Descripcion de Data

#Conocer que tipos de variables tenemos
str(data)
head(data)

#Obtener datos estadisticos
summary(data)


d <- 5
for(i in 1:10) { 
  nam <- paste("A", i, sep = "")
  assign(nam, rnorm(3)+d)
}


name=numeric(2)
c <- c("Juan","Pedro")
for  (i in 1:2){
  name[i] <- paste("sub",c[i], sep="")
}

name


#Crear variables en bucles

list <- c(1,0,1,4,5,6)
assign(paste0("variable", list[5]), list[5])
