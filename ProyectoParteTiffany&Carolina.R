#Proyecto Modulo V
#Integrantes Carolina Soto & Tiffany Serrano


#1. Cantidad de vehículos por marca, según año modelo menor a 2000  (Tabla inventario)

filterModel<-filter(Inventario_Autos,Model_Year<2000)

ggplot(data=filterModel)+
geom_bar(aes(y=Brand,
             fill=Model_Year))+ 
  ggtitle("Cantidad de vehículos por marca, según año modelo menor a 2000")+
  theme(plot.title = element_text(hjust = 0.5))

#2 Cantidad final de vehículos por Sucursal_país (Tabla Inventario)

plot_ly(data = Inventario_Autos, x = ~Sucursal_Pais,
        y= ~Qty_Final, type= "bar")


#3.Cantidad de Marcas de vehículos que ingresan en todo el año 2020 (Tabla Inventario) 


Fecha_I<-as.character(Inventario_Autos$Fecha_ingreso)
datosFecha= mutate(Inventario_Autos,
                   Año = year(Fecha_I), Cant_marca= 1)

datosFechaFiltro=filter(datosFecha,Año=="2020")
groupmarca= group_by(datosFechaFiltro,Brand)
                        
plot_ly(data = groupmarca, x = ~Brand, y=~Cant_marca, color = ~Brand, type = "bar" 
)


#4.	Marcas de Autos con más de 45 unidades en Inventario Final, brindar un descuento de 2mil$ y que el modelo sea menor al año 2000 (Tabla Inventario)

INV_menos_Vendidos <- Inventario_Autos %>%
  mutate(Prec_Total_conDesc = ifelse(Qty_Final > 45, Precio_Compra - 2000, NA)) %>%
  group_by(Brand) %>%
  na.omit(INV_menos_Vendidos)
filter_Año= filter(INV_menos_Vendidos,Model_Year > "1990") 


Prec_Total_conDesc <- plot_ly(
  type = "funnelarea",
  values = as.numeric(filter_Año$Prec_Total_conDesc),
  textinfo = "value",
  text = as.character(filter_Año$Brand),
  title = list(position = "top left", text = "Sale, Modelos menores a 1999"))

Prec_Total_conDesc


#5.	Cantidad de clientes según su género (Tabla Clientes)

#a) Barras
Clientes_sexo1 <- mutate(Clientes_Autos, sexo_factor1 = factor(Clientes_Autos$Género, 
                                                             labels = c("Femenino", "Masculino")))
ggplot(Clientes_sexo1, aes(x = sexo_factor1)) +
  geom_bar(width = 0.4, fill=rgb(0.1,0.3,0.5,0.7), aes(y = (..count..)/sum(..count..))) +
  scale_x_discrete("Sexo") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Porcentaje",labels=scales::percent) + #Configuración eje y
  labs(title = "Cantidad de Clientes reportados en las bases de Datos",
       subtitle = "Frecuencia relativa de la variable sexo")

#b) Pie
Clientes_sexo2 <- mutate(Clientes_Autos, sexo_factor2=1/100)

Sexo <- data.frame("Género"=rownames(Clientes_sexo2), Clientes_sexo2)
data <- Clientes_sexo2[,c('Género', 'sexo_factor2')]

plot_ly(data, labels =~Género , values = ~sexo_factor2, type = 'pie', 
                title = 'Porcentaje de Clientes reportados en las bases de Datos')
                


#6.	Edades más representativas de los clientes (Tabla Clientes)

hist(Clientes_Autos$Edad,
     main="Histograma por Edad de Clientes",
     xlab="Edad", ylab="Frecuencia",
     xlim=c(15, 95), ylim=c(0, 100),
     col="gray")
abline(v=median(Clientes_Autos$Edad), col="blue")
abline(v=mean(Clientes_Autos$Edad), col="red")

median(Clientes_Autos$Edad)  = 49.5
mean(Clientes_Autos$Edad) = 49.29



#7.	Representar la antigüedad de los clientes que existen en la compañía (Tabla Clientes)




Fecha_C<-as.character(Clientes_Autos$Fecha_creaciónID)
Clientes<-mutate(Clientes_Autos,AñoC= year(Fecha_C)
                 ,MesC= month(Fecha_C),arrange (unite(Clientes,Periodo_YYMM, c(11:12), sep= "/") ))

View(clientes2)


plot_ly(Clientes, x = ~Periodo_YYMM, color= ~AñoC,
              type = 'scatter')


#8.	Estudio de ganancias del año 2021 por nombre de marca que empieza con A, B y C (Tabla Ventas)

Fecha_V<-as.character(Ventas_Autos$Fecha_Venta)
Ventas_Autos<-mutate(Ventas_Autos,
                       Venta_Total = PrecioUnit_Venta* Qty_Venta, 
                     AñoV= year(Fecha_V), MesV= month(Fecha_V))

VentasFiltro=filter(Ventas_Autos,AñoV=="2021", str_detect(Brand,"^A|^B|^C")) 
                                                  
AgrupMarca= group_by(VentasFiltro,Brand)


plot_ly(data = AgrupMarca, x = ~Brand, y=~Venta_Total, color = ~Brand,
      mode= "line" 
)


#9.	Preferencias de clientes por color del vehículo para los años 2020 y 2021(Tabla Ventas+Clientes)

View(Ventas_Autos)
View(Join)

Join<-merge(x = Ventas_Autos, y = Clientes_Autos, by = "VentasID")

Join<-group_by(Join,Color)

Join= filter(Join,str_detect(Nombre_Completo,"^A|^B|^C"))
plot_ly(data = Join , x = ~Color, y=~Nombre_Completo, color=~Género,
        type = "bar",title = list(position = "top center", 
                        text = "Preferencias por color vehículo")
)

#10.Comparativo de ventas por mes del año 2021(Tabla Ventas)

Fecha_V<-as.character(Ventas_Autos$Fecha_Venta)
Ventas_Autos<-mutate(Ventas_Autos,
                     Venta_Total = PrecioUnit_Venta* Qty_Venta, 
                     AñoV= year(Fecha_V), MesV= month(Fecha_V))
VentasFiltro2=filter(Ventas_Autos,AñoV=="2021")

plot_ly(data = VentasFiltro2 , x = ~MesV, y=~Venta_Total, color=~MesV,
        type= "bar")

#Librerias y paquetes utilizados
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library("RColorBrewer")
library(wesanderson)
library(tidyr)
View(Inventario_Autos)
View(Clientes_Autos)
View(Ventas_Autos)

