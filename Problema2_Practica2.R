#Codigo para problema 2
mis_dades <- iris

#Calcular la recta lineal entre Sepal.Lenght = m*Petal.Length + b
#Calculo de m:
x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
mean(x)
mean(y)
sd(x)
hist(x)

m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m

#Calculo de b:
b <- mean(y)-m*mean(x)
b

#Predición cuando Petal.Length = 1.5 (El valor esperado en ese punto):
m*1.5+b

#Forma rápida de encontrar la recra LINEAL:
mod <- lm(y~x)
summary(mod)
data.frame(x=1.5)
predict(mod, data.frame(x=1.5))

#Predecir el valor esperado para cada valor de x:
data.frame(x=x)
ypred <- predict(mod, data.frame(x=x))

#Graficar la recta en la gráfica y VS x:
plot(x,y, col="red",pch=16)
lines(x,ypred)

#Calcular R^2 (que tan buena es la capacidad predictiva del modelo):
Resc <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
Resc