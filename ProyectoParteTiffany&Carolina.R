#1.	Tabla inventario=Cant de Autos en inventario con modelos<2000 ( >Qty final y Marca)


#Creacion de Grafico
library(ggplot2)
 
ggplot(Inventario_Autos)+
geom_bar(aes(y=Brand,
fill=as.factor(Model_Year<2000)),
position = "stack")

#2.	Tabla inventario=Cuantos vehic hay por sucursal (Qty final y surcursal pa�s)
ggplot(Inventario_Autos)+
  geom_bar(mapping = aes(x=Sucursal_Pais=="China",y=Qty_Final,
                           size=4,
                           color=6))



#3.	Tabla inventario= cuales marcas de vehic ingresan por el periodo 2020
#4.	Tabla inventario= a los autos menos vendidos colocarles una etiqueta de descuento de 25% y reflejar el impacto de la p�rdida
#5.	Tabla clientes= representar por medio de un grafico de pie, la cantidad por g�nero
#6.	Tabla clientes= gr�fico de dispersi�n para averiguar las edades m�s representativas de los clientes 
#7.	Tabla clientes= representar la antig�edad de los clientes que existen en la compa��a
#8.	Tabla ventas= agregar una columna para obtener el precio total (Precio unit*Qty venta) y as� obtener un estudio de las ganancias por marca
#9.	Tabla ventas+ tabla clientes= obtener las preferencias de marca y a�o por cliente
#10.Tabla ventas: Comparativo de ventas por mes (Fecha venta y precio total)