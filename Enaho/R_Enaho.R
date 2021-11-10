"
P300A <- ¿Cuál es el Idioma o lengua materna que aprendió en su niñez?
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


model <- lm(log(data$P524A1)~log(data$P301A)+log(data$P401H1)+log(data$P401H2)+log(data$P401H3)+log(data$P401H4)+log(data$P401H5))

summary(model)
