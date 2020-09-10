---
title: "Untitled"
output:
   html_document:
      self_contained: false
      keep_md: true
  

---



Librerias utilizadas


```r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(stars))
suppressPackageStartupMessages(library(spData))
suppressPackageStartupMessages(library(spDataLarge))
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(leaflet)) 
suppressPackageStartupMessages(library(ggplot2) )
```

# Ejercicio capitulo 1 ----------------------------------------------------


**1-Leer el shapefile storms_xyz_feature del directorio en el sf paquete**

```r
system.file("shape/storms_xyz_feature.shp", package="sf") %>% read_sf() -> storms_xyz_feature
```
**2-Copie este archivo en otro directorio de su computadora y lealo desde alla (nota: un shapefile consta de mas de un archivo!)**


```r
storms_xyz_feature1=  st_zm(storms_xyz_feature)

st_write(storms_xyz_feature1, "C:/Users/User/Documents/Mayker/Maestria Profesional/Estadistica Espacial/SP1649-1120-NI/storms_xyz_feature1.shp")

storms_xyz_feature1 = st_read("C:/Users/User/Documents/Mayker/Maestria Profesional/Estadistica Espacial/SP1649-1120-NI/storms_xyz_feature1.shp")
```
**3-¿Cuántas características contiene este conjunto de datos?**

Destaca por ser un archivo con figuras tipo "Lenetring", el bbox es de xmin:-102.2, ymin:8.3, xmax:0 ymax:59. Además posee una variable adicional llmada Z


```r
storms_xyz_feature
```

```
## Simple feature collection with 71 features and 1 field
## geometry type:  LINESTRING
## dimension:      XYZ
## bbox:           xmin: -102.2 ymin: 8.3 xmax: 0 ymax: 59.5
## z_range:        zmin: 924 zmax: 1017
## CRS:            NA
## # A tibble: 71 x 2
##    Track                                                                geometry
##    <chr>                                                            <LINESTRING>
##  1 TONY    Z (-50.8 20.1 1011, -51.2 20.4 1011, -51.5 20.8 1010, -51.7 21.3 100~
##  2 SANDY   Z (-77.4 14.3 1006, -77.8 13.9 1005, -78.2 13.5 1003, -78.6 13.1 100~
##  3 RAFAEL  Z (-62.7 14.7 1006, -63.2 15 1007, -63.7 15.3 1006, -63.8 15.6 1006,~
##  4 PATTY   Z (-72.5 25.5 1012, -72.4 25.4 1010, -72.2 25.4 1009, -72.2 25.5 100~
##  5 OSCAR   Z (-38 12.4 1008, -38.7 13.6 1008, -39.6 15 1007, -40.5 16.3 1006, -~
##  6 NADINE  Z (-38 15.5 1008, -39.4 15.6 1007, -40.8 15.8 1007, -41.9 16 1007, -~
##  7 MICHAEL Z (-36.7 28.9 1015, -37.9 28.1 1015, -39.2 27.2 1015, -40.1 26.4 101~
##  8 LESLIE  Z (-27.4 12.9 1010, -29.6 13 1010, -31.7 13.1 1009, -33.7 13.2 1009,~
##  9 KIRK    Z (-43.4 23.9 1007, -44.5 24 1004, -45.5 24.3 1003, -46.3 24.6 1001,~
## 10 JOYCE   Z (-31.7 10.7 1011, -33.1 11.1 1011, -34.5 11.5 1011, -35.8 12.1 100~
## # ... with 61 more rows
```


 **4-Grafique el conjunto de datos con axes = TRUE(pista: antes de graficar, canalice st_zm para soltar las coordenadas Z y M; mas sobre esto en el capitulo 3 ).**


```r
d=st_zm(storms_xyz_feature)

d %>%  plot(graticule = TRUE, axes = TRUE)
```

![](Lab1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
 
 
 **5-Antes de graficar, canalice el conjunto de datos st_set_crs(4326). A que se debe lo diferente en la trama obtenida?**


```r
d=st_zm(storms_xyz_feature)

   d %>% 
  st_set_crs(4326) %>%
  plot(graticule = TRUE, axes = TRUE)
```

![](Lab1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

con el comando st_zm lo que se hizo fue eliminar la variable z y con st_set_crs(4326) se hace proyecta y se observa  la altitud y latitud*



# Ejercicio capitulo 2 ----------------------------------------------------


**convertir el (x,y) puntos (10,2), (-10,-2),(10,-2) y (0,10) a coordenadas polares (10,2)**   

```r
x=10
y=2

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi)
r1
```

```
## [1] 10.19804
```

```r
a
```

```
## [1] 11.30993
```
(-10,-2)       

```r
x=-10
y=-2

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi) 
r1
```

```
## [1] 10.19804
```

```r
a
```

```
## [1] 11.30993
```


(10,-2) 

```r
x=10
y=-2

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi)  
r1
```

```
## [1] 10.19804
```

```r
a
```

```
## [1] -11.30993
```
(0,10) 

```r
x=0
y=10

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi)
r1
```

```
## [1] 10
```

```r
a
```

```
## [1] 90
```
**convertir los puntos polares (r,a) (10,45),(0,100),(5,359) as coordenadas cortecianas**

x=r cos(a)
y=r sin(a)

(10,45)

```r
r=10
a=45

ar=(a*pi)/180

x=r*cos(ar)
y=r*sin(ar)
x
```

```
## [1] 7.071068
```

```r
y 
```

```
## [1] 7.071068
```


(0,100)


```r
r=0
a=100

ar=(a*pi)/180

x=r*cos(ar)
y=r*sin(ar)
x
```

```
## [1] 0
```

```r
y 
```

```
## [1] 0
```

(5,359)


```r
r=5
a=359

ar=(a*pi)/180

x=r*cos(ar)
y=r*sin(ar)
x
```

```
## [1] 4.999238
```

```r
y 
```

```
## [1] -0.08726203
```

**•	suponiendo que la Tierra es una esfera con un radio de 6371 km, calcule para (λ,ϕ)(λ,ϕ) señala la distancia del gran círculo entre (10,10)(10,10) y (11,10)(11,10), Entre (10,80)(10,80) y (11,80)(11,80), Entre (10,10)(10,10) y (10,11)(10,11) y entre (10,80)(10,80) y (10,81)(10,81)(unidades: grado). ¿Cuáles son las unidades de distancia?**


```r
r=6371

l1=10
a1=10

l2=11
a2=10

a1r=(a1*pi)/180
l1r=(l1*pi)/180

a2r=(a2*pi)/180
l2r=(l2*pi)/180

a12=acos( (sin(a1r)*sin(a2r))+((cos(a1r)*cos(a2r))*cos(l1r-l2r)))
#a12
a12=((a12*180)/pi) 
d=(a12*r)/1000
d
```

```
## [1] 6.274208
```

(10,80) y (11,80)


```r
l1=10
a1=80

l2=11
a2=80

a1r=(a1*pi)/180
l1r=(l1*pi)/180

a2r=(a2*pi)/180
l2r=(l2*pi)/180



a12=acos( (sin(a1r)*sin(a2r))+((cos(a1r)*cos(a2r))*cos(l1r-l2r)))
a12=((a12*180)/pi) 
a12
```

```
## [1] 0.173646
```

```r
d=(a12*r)/1000
d
```

```
## [1] 1.106299
```
(10,10) y (10,11) 


```r
l1=10
a1=10

l2=10
a2=11

a1r=(a1*pi)/180
l1r=(l1*pi)/180

a2r=(a2*pi)/180
l2r=(l2*pi)/180



a12=acos( (sin(a1r)*sin(a2r))+((cos(a1r)*cos(a2r))*cos(l1r-l2r)))
a12=((a12*180)/pi) 
a12
```

```
## [1] 1
```

```r
d=(a12*r)/1000
d
```

```
## [1] 6.371
```
(10,80) y (10,81)

```r
l1=10
a1=80

l2=10
a2=81

a1r=(a1*pi)/180
l1r=(l1*pi)/180

a2r=(a2*pi)/180
l2r=(l2*pi)/180



a12=acos( (sin(a1r)*sin(a2r))+((cos(a1r)*cos(a2r))*cos(l1r-l2r)))
a12=((a12*180)/pi) 
a12
```

```
## [1] 1
```

```r
d=(a12*r)/1000
d
```

```
## [1] 6.371
```


# Ejercicio 4.4 -----------------------------------------------------------

**NDVI, indice de vegetacion diferenciado normalizado, se calcula como (NIR-R) / (NIR + R), siendo NIR el infrarrojo cercano y R la banda roja. Lea el L7_ETMs.tif archivo en el objeto xy distribuya las dimensiones de la banda sobre los atributos por split(x, "band"). Luego, calcule el NDVI usando una expresiÃ³n que use los atributos NIR (banda 4) y R (banda 3) directamente.**


```r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
x
```

```
## stars object with 3 dimensions and 1 attribute
## attribute(s):
##   L7_ETMs.tif    
##  Min.   :  1.00  
##  1st Qu.: 54.00  
##  Median : 69.00  
##  Mean   : 68.91  
##  3rd Qu.: 86.00  
##  Max.   :255.00  
## dimension(s):
##      from  to  offset delta                       refsys point values    
## x       1 349  288776  28.5 UTM Zone 25, Southern Hem... FALSE   NULL [x]
## y       1 352 9120761 -28.5 UTM Zone 25, Southern Hem... FALSE   NULL [y]
## band    1   6      NA    NA                           NA    NA   NULL
```

```r
plot(x)
```

![](Lab1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
xx=split(x, "band")
resultado_ndvi= (xx[4]-xx[3])/(xx[4]+xx[3])
resultado_ndvi
```

```
## stars object with 2 dimensions and 1 attribute
## attribute(s):
##       X4           
##  Min.   :-0.75342  
##  1st Qu.:-0.20301  
##  Median :-0.06870  
##  Mean   :-0.06432  
##  3rd Qu.: 0.18667  
##  Max.   : 0.58667  
## dimension(s):
##   from  to  offset delta                       refsys point values    
## x    1 349  288776  28.5 UTM Zone 25, Southern Hem... FALSE   NULL [x]
## y    1 352 9120761 -28.5 UTM Zone 25, Southern Hem... FALSE   NULL [y]
```

```r
plot(resultado_ndvi)
```

![](Lab1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

**Calcule NDVI para la imagen S2, usando st_applyy una funcion ndvi = function(x) (x[4]-x[3])/(x[4]+x[3]). Trace el resultado y escriba el resultado en un GeoTIFF. Explique la diferencia en tiempo de ejecuciÃ³n entre trazar y escribir.**


```r
ndvi = function(x) (x[4]-x[3])/(x[4]+x[3])
resultado_dos=st_apply(x, c("x", "y"), ndvi)
resultado_dos
```

```
## stars object with 2 dimensions and 1 attribute
## attribute(s):
##      ndvi          
##  Min.   :-0.75342  
##  1st Qu.:-0.20301  
##  Median :-0.06870  
##  Mean   :-0.06432  
##  3rd Qu.: 0.18667  
##  Max.   : 0.58667  
## dimension(s):
##   from  to  offset delta                       refsys point values    
## x    1 349  288776  28.5 UTM Zone 25, Southern Hem... FALSE   NULL [x]
## y    1 352 9120761 -28.5 UTM Zone 25, Southern Hem... FALSE   NULL [y]
```

```r
plot(resultado_dos)
```

![](Lab1_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
se guarda el resultado

```r
write_stars(resultado_dos, "C:/Users/User/Documents/Mayker/Maestria Profesional/Estadistica Espacial/resultado_dos.tif")
```


**1.	Utilice st_transform para transformar el starsobjeto leido L7_ETMs.tifen EPSG 4326. Imprima el objeto. ¿Es esta una cuadricula regular? Grafique la primera banda usando argumentos axes=TRUEy border=NA, y explique por quÃ© esto lleva tanto tiempo.**


```r
new = st_crs(4326)
y = st_transform(x, new)
y
```

```
## stars object with 3 dimensions and 1 attribute
## attribute(s):
##   L7_ETMs.tif    
##  Min.   :  1.00  
##  1st Qu.: 54.00  
##  Median : 69.00  
##  Mean   : 68.91  
##  3rd Qu.: 86.00  
##  Max.   :255.00  
## dimension(s):
##      from  to offset delta refsys point                          values    
## x       1 349     NA    NA WGS 84 FALSE [349x352] -34.9165,...,-34.8261 [x]
## y       1 352     NA    NA WGS 84 FALSE  [349x352] -8.0408,...,-7.94995 [y]
## band    1   6     NA    NA     NA    NA                            NULL    
## curvilinear grid
```

-despues de hacer la transformacion se tiene como resultado una figura curvilinea.
-cuando convertimos o transformamos datos tipo raster regular en otro sistema de referencia de coordenadas, se volverá curvilineo a menos que volvamos a muestrear; el remuestreo siempre se realiza a costa de alguna pérrdida de datos y no es reversible


Grafico de la primer banda

```r
#plot(y)
yy=split(y, "band")
plot(yy[1], axes=TRUE, border=NA)
```

![](Lab1_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

Toma mucho tiempo porque los datos de tif/L7_ETMs.tif son tipo raster y al aplicarle una transformación le estamos cambiando a otro sistema de coordenadas lo que genera perdida de datos.


-tilice st_warppara deformar el L7_ETMs.tif a un objeto a EPSG 4326 y graficar el objeto resultante con axes=TRUE. ¿Por que el grafico se crea mucho mas rapido que con st_transform?




```r
new = st_crs(4326)
y2 = st_warp(x, crs = new)

plot(y2, axes=TRUE, border=NA)
```

![](Lab1_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

**La expresionn st_warp  realiza el metodo de remuestreo del vecino mas cercano (metodo = "cerca"), que es el predeterminado,con st_warp vuelvo a una figura regular, pero hay perdida de informacion **



# Ejercicio capitulo 6 ----------------------------------------------------


Agregue una variable al ncconjunto de datos mediante nc$State = "North Carolina". ¿Que valor deberia adjuntar a esta variable para la relacion atributo-geometrica (agr)?


```r
nc <- system.file("gpkg/nc.gpkg", package="sf") %>%
   read_sf() %>%
   st_transform(32119)

plot(nc)
```

```
## Warning: plotting the first 10 out of 14 attributes; use max.plot = 14 to plot
## all
```

![](Lab1_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
nc$State = "North Carolina"
```

La variable STATE se debe agregar de manera con identity, ya que es una variables que identifica a la geometria como un todo 

Cree un nuevo sf objeto a partir de la geometria obtenida por st_union(nc) y asigne "North Carolina"a la variable State. Â¿que AGR puede asignar ahora a esta variable de atributo?



```r
# nc10 <- nc %>% select(BIR74, SID74, State) %>%
#    st_set_agr(c(BIR74 = "aggregate", SID74 = "aggregate", State = "identity"))
# 
# plot(nc10)
# 
```

**Cree un nuevo sf objeto a partir de la geometria obtenida por st_union(nc) y asigne "North Carolina"a la variable State. Â¿que AGR puede asignar ahora a esta variable de atributo?**


```r
vv=st_union(nc)
vvv=vv %>% st_sf()
vvvv=vvv%>%st_transform(32119)
vvvv$State = "North Carolina"
plot(vvvv)
```

![](Lab1_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
vv1=st_union(nc, by_feature=TRUE)
vvv1=vv1 %>% st_sf()
vvvv1=vvv1%>%st_transform(32119)
vvvv1$State = "North Carolina"
plot(vvvv1)
```

```
## Warning: plotting the first 10 out of 15 attributes; use max.plot = 15 to plot
## all
```

![](Lab1_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

 Esa variable se puede agregar como identity, ya que no cambia para las geometrias

**use st_area para agregar una variable con nombre area a nc. Compare las variables area y AREA en el conjunto de datos nc. ¿Cuáles son las unidades de AREA? ¿Están relacionados linealmente las dos variables? Si hay discrepancias, ¿cuál podría ser la causa?**

```r
d=st_area(nc)

nc$area_cal=d

nc1 <- head(cbind(nc$AREA, nc$area_cal, round ((nc$area_cal/10000000000),3) ) )
nc1
```

```
##       [,1]       [,2]  [,3]
## [1,] 0.114 1137590142 0.114
## [2,] 0.061  611196991 0.061
## [3,] 0.143 1423728763 0.142
## [4,] 0.070  694661115 0.069
## [5,] 0.153 1520991764 0.152
## [6,] 0.097  967855261 0.097
```
La variable AREA parece estar dividida entre 10 000 000 000, ya que al dividirla entra esa cifra da muy similar a la variable de la base de datos


**¿La areavariable es intensiva o extensiva? ¿Es su agr igual a constanta, identidad aggregate?**

la variable area es extensica 
Ademas se puede agragar con un tipo agregado 

   
**Busque el nombre del condado que contiene POINT(-78.34046 35.017)**


```r
nc_4326 <- system.file("gpkg/nc.gpkg", package="sf") %>%
   read_sf() %>%
   st_transform(4326)

punto<- st_point( c( -78.34046, 35.017 ) )
res <- st_contains(nc_4326, punto)
```

```
## although coordinates are longitude/latitude, st_contains assumes that they are planar
```

```r
res <- summary(res) 
res_1 <- as.numeric( res[,1])
pos <- which( res_1==1 )
nc_4326$NAME[pos]
```

```
## [1] "Sampson"
```

**Busque los nombres de todos los condados con limites que tocan el Sampson**


```r
sampson_geo <- nc_4326$geom[pos]
#res_2 <- st_intersection(nc_4326, sampson_geo)
res_2 <- st_intersection(nc_4326, sampson_geo)
```

```
## although coordinates are longitude/latitude, st_intersection assumes that they are planar
```

```
## Warning: attribute variables are assumed to be spatially constant throughout all
## geometries
```

```r
res_2 <- res_2[ res_2$NAME!='Sampson',    ]; res_2$NAME
```

```
## [1] "Johnston"   "Wayne"      "Harnett"    "Cumberland" "Duplin"    
## [6] "Bladen"     "Pender"
```
**Enumere los nombres de todos los condados que se encuentran a menos de 50 km del condado Sampson.**


```r
res_3 <- st_is_within_distance(nc_4326, sampson_geo, dist = 50000)
res_3 <- summary(res_3) 
res_3 <- as.numeric( res_3[,1])
pos <- which( res_3==1 )
nc_4326$NAME[pos]
```

```
##  [1] "Wake"        "Chatham"     "Wilson"      "Johnston"    "Greene"     
##  [6] "Lee"         "Wayne"       "Harnett"     "Moore"       "Lenoir"     
## [11] "Sampson"     "Cumberland"  "Jones"       "Hoke"        "Duplin"     
## [16] "Onslow"      "Robeson"     "Bladen"      "Pender"      "Columbus"   
## [21] "New Hanover" "Brunswick"
```

# Ejercicio 8 -----------------------------------------------------------


```r
africa = world %>% 
   filter(continent == "Africa", !is.na(iso_a2)) %>% 
   left_join(worldbank_df, by = "iso_a2") %>% 
   dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
   st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

zion = st_read((system.file("vector/zion.gpkg", package = "spDataLarge")))
```

```
## Reading layer `zion' from data source `C:\Users\User\Documents\R\win-library\4.0\spDataLarge\vector\zion.gpkg' using driver `GPKG'
## Simple feature collection with 1 feature and 11 fields
## geometry type:  POLYGON
## dimension:      XY
## bbox:           xmin: 302903.1 ymin: 4112244 xmax: 334735.5 ymax: 4153087
## projected CRS:  UTM Zone 12, Northern Hemisphere
```

```r
data(nlcd, package = "spDataLarge")
```

**Cree un mapa que muestre la distribución geográfica del Índice de Desarrollo Humano ( HDI) en África con gráficos base (pista: uso plot()) y paquetes tmap (pista: uso tm_shape(africa)  ...).**


```r
# Uso de plot
#plot(africa)
plot(africa[4])
```

![](Lab1_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

```r
#plot(st_geometry(africa) , col = africa$HDI)
```
Uso de tm_shape

```r
tm_shape(africa)+ tm_fill(col = "HDI")
```

![](Lab1_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

**Extienda el tmap creado para el ejercicio anterior para que la leyenda tenga tres bins: “Alto” (por HDIencima de 0,7), “Medio” ( HDIentre 0,55 y 0,7) y “Bajo” ( HDIpor debajo de 0,55).**
**•	Bonificación: mejore la estética del mapa, por ejemplo, cambiando el título de la leyenda, las etiquetas de clase y la paleta de colores.**


```r
africa = africa %>% mutate(rango_HDI=case_when(HDI<0.55~'bajo',
                                               HDI>=0.55 & HDI<0.7  ~'medio',
                                               HDI>0.7 ~'alto',
                                               TRUE~'nd'))


map1=tm_shape(africa)+ tm_fill(col = "rango_HDI")+tm_style("classic") + tm_layout(title = "Africa por nivel de HDI") +tm_layout(scale = 2)
map1
```

![](Lab1_files/figure-html/unnamed-chunk-36-1.png)<!-- -->
**Represente africa en las subregiones en el mapa. Cambie la paleta de colores predeterminada y el título de la leyenda. A continuación, combine este mapa y el mapa creado en el ejercicio anterior en un solo gráfico.**



```r
map2=tm_shape(africa)+ tm_fill(col = "subregion", palette = "BuGn") + tm_layout(title = "Africa por subregiones")
map2                   
```

![](Lab1_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

```r
tmap_arrange(map1, map2)
```

![](Lab1_files/figure-html/unnamed-chunk-38-1.png)<!-- -->
#4-Crea un mapa de cobertura terrestre del Parque Nacional Zion.


```r
plot(zion)
```

```
## Warning: plotting the first 10 out of 11 attributes; use max.plot = 11 to plot
## all
```

![](Lab1_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

```r
#tm_shape(zion)+tm_fill()+tm_fill()
```

**Cree mapas de facetas de países de África oriental:**
**•	Con una faceta que muestra el IDH y la otra que representa el crecimiento de la población (pista: usando variables HDIy pop_growth, respectivamente)**
**•	Con un 'múltiplo pequeño' por país**





```r
africa_Oriental=africa %>% filter(subregion=='Eastern Africa')

africa_Oriental_HDI=africa_Oriental
africa_Oriental_HDI$faceta='HDI'
africa_Oriental_HDI$indicador=africa_Oriental_HDI$HDI

africa_Oriental_popg=africa_Oriental
africa_Oriental_popg$faceta='pop_growth'
africa_Oriental_popg$indicador=africa_Oriental_popg$pop_growth


d=rbind(africa_Oriental_HDI,africa_Oriental_popg)



tm_shape(africa_Oriental) +
   tm_polygons()+
   tm_shape(d)+
   tm_symbols(col = "black", border.col = "white", size =c("indicador") )+
   tm_facets(by = "faceta", nrow = 2, free.coords = TRUE)
```

![](Lab1_files/figure-html/unnamed-chunk-40-1.png)<!-- -->
**Sobre la base de los ejemplos de mapas de facetas anteriores, cree mapas animados de África Oriental:**
**•	Mostrando primero la distribución espacial de las puntuaciones del IDH y luego el crecimiento de la población**
**•	Mostrando cada país en orden**


**Crea un mapa interactivo de África:**
**•	Con tmap**
**•	Con mapview**
**•	Con folleto**
**•	Bonificación: para cada enfoque, agregue una leyenda (si no se proporciona automáticamente) y una barra de escala**

Con tmap

```r
map0=tm_shape(africa)+ tm_fill()

tmap_mode("view")
```

```
## tmap mode set to interactive viewing
```

```r
map2
```

<!--html_preserve--><div id="htmlwidget-30eb55850a77cc8b9620" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-30eb55850a77cc8b9620">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"createMapPane","args":["tmap401",401]},{"method":"addProviderTiles","args":["Esri.WorldGrayCanvas",null,"Esri.WorldGrayCanvas",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["OpenStreetMap",null,"OpenStreetMap",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,"Esri.WorldTopoMap",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"pane":"tilePane"}]},{"method":"addPolygons","args":[[[[{"lng":[33.9037111971045,34.07262,37.6986899999999,37.7669,39.20222,38.74054,38.7997700000001,39.44,39.4700000000001,39.19469,39.25203,39.1865200000001,39.5357400000001,39.9496,40.3165862291108,40.31659,39.521,38.4275565935878,37.82764,37.47129,36.7751509946228,36.5140816586843,35.312397902169,34.5599890479994,34.28,33.9408377240965,33.7397200000001,32.7593754412213,32.1918648617919,31.5563480974665,31.1577513369501,30.7400097314221,30.7400154965518,30.1999967791017,29.62003217949,29.4199927100882,29.5199866065729,29.3399975929003,29.7535124040999,30.11632,30.50554,30.7522400000001,30.74301,30.52766,30.4696736457612,30.46967,30.7583089535831,30.8161348813177,30.4191048520192,30.7698600000001,31.8661700000001,33.9037111971045],"lat":[-0.94999999999952,-1.05981999999978,-3.0969899999995,-3.67711999999945,-4.67677000000032,-5.90894999999988,-6.47566,-6.83999999999994,-7.09999999999996,-7.70390000000006,-8.00781000000007,-8.48550999999984,-9.11237000000013,-10.0984000000006,-10.3170977528174,-10.3170999999999,-10.8968800000001,-11.2852023250816,-11.2687899999998,-11.5687600000001,-11.5945374487813,-11.7209380021671,-11.4391464168795,-11.5200200334156,-10.1599999999998,-9.69367384198043,-9.41714999999955,-9.23059905358901,-8.93035898197308,-8.76204884199826,-8.59457874731718,-8.34000593035405,-8.34000741947069,-7.07998097089848,-6.52001515058319,-5.93999887453942,-5.41997893638628,-4.49998341229434,-4.45238941815333,-4.09012000000008,-3.56858000000006,-3.35931000000007,-3.03430999999938,-2.80761999999986,-2.41385475710109,-2.41383,-2.28725025798838,-1.69891407634556,-1.1346591121508,-1.01455000000005,-1.02736000000001,-0.94999999999952]}]],[[{"lng":[-8.66558956545481,-8.66512447756418,-8.68439978680905,-8.6872936670174,-11.9694189111712,-11.9372244938533,-12.8742215641696,-13.1187544417747,-12.9291019352635,-16.845193650774,-17.0634232243426,-17.0204284326758,-17.0029617985611,-14.7509545557135,-14.6308326888511,-14.2211677718573,-13.8911103988091,-12.5009626937254,-12.0307588363016,-11.7182197738003,-11.392554897497,-10.5512625797853,-10.1894242008776,-9.73534339032888,-9.41303748212448,-8.79488399904907,-8.81782833498668,-8.66558956545481],"lat":[27.6564258895926,27.5894790715585,27.3957441268962,25.8810562199892,25.9333527694684,23.374594224536,23.284832261645,22.7712202010962,21.3270706242674,21.3333234725747,20.9997521021309,21.4223102889818,21.4207341577966,21.5006000839032,21.8609398462749,22.3101630721881,23.691009019459,24.7701162785785,26.0308661972029,26.1040917017608,26.8834239771544,26.990807603457,26.860944729108,26.8609447291077,27.0884760604885,27.1206963160227,27.6564258895926,27.6564258895926]}]],[[{"lng":[29.3399975929003,29.5199866065729,29.4199927100882,29.62003217949,30.1999967791017,30.7400154965518,30.7400097314221,30.3460860531908,29.0029122250605,28.7348665707625,28.4498710466728,28.6736816749289,28.4960697771418,28.3722530453704,28.6424174333924,29.3415478858691,29.6160014177712,29.6996138852195,28.9342859229768,28.523561639121,28.15510867688,27.3887988624238,27.1644197934125,26.5530875993996,25.7523096046047,25.4181181169732,24.783169793403,24.314516228948,24.257155389104,23.9122152035557,23.4567908057674,22.8373454118847,22.4027982927424,22.1552681820643,22.2087532894864,21.8751819190423,21.8018013851879,21.949130893652,21.7464559262033,21.7281107927397,20.5147481625265,20.6018229509383,20.0916215349206,20.0377230160402,19.4175024756732,19.1666133968961,19.0167517432497,18.4641756527527,18.1342216325691,17.4729700049622,17.0899959652472,16.8601908708452,16.5731799658961,16.326528354567,13.3755973649719,13.024869419007,12.7351713395787,12.3224316748635,12.1823368669203,12.4366882666609,12.4680041846297,12.6316117692658,12.9955172054652,13.258240187237,13.6002348161447,14.1449560889333,14.2090348649752,14.5826037940132,15.1709916520884,15.7535400733148,16.0062895036543,15.9728031755292,16.4070919125101,16.8653068376421,17.5237162614729,17.63864464689,17.6635526872547,17.8265401547033,17.7741919287916,17.8988354834796,18.0942757504074,18.3937923519711,18.4530652198099,18.5429822119978,18.9323124528848,19.4677836442931,20.2906791521089,20.9275911801063,21.65912275563,22.4051237321955,22.7041235694363,22.8414795264681,23.2972139828501,24.4105310401463,24.8050289242624,25.1288334490033,25.2787984555143,25.6504553565575,26.4027608578625,27.0440653826047,27.3742261085175,27.9799772478428,28.4289937680269,28.6966776872988,29.1590784034465,29.715995314256,29.9535001970695,30.8338524217154,30.8338598975938,30.77334679538,31.1741492042358,30.8526701189481,30.4685075212903,30.0861535987627,29.8757788429024,29.8195032081366,29.5878377621722,29.5794661801409,29.2918868344366,29.2548348324833,29.1174788754516,29.0249263852168,29.2763839047491,29.3399975929003],"lat":[-4.49998341229434,-5.41997893638628,-5.93999887453942,-6.52001515058319,-7.07998097089848,-8.34000741947069,-8.34000593035405,-8.23825652428861,-8.40703175215326,-8.52655934004487,-9.16491830814593,-9.60592498132493,-10.7898837215639,-11.7936467424016,-11.9715686987823,-12.3607439103724,-12.1788945451371,-13.2572266577718,-13.2489584286049,-12.6986044246971,-12.2724805640176,-12.1327474911006,-11.6087484676612,-11.9244397925321,-11.7849651017761,-11.3309359676598,-11.2386935360188,-11.2628264298988,-10.9519926896635,-10.9268262671378,-10.8678634578923,-11.0176217586746,-10.9930754533357,-11.0848011206539,-9.89479623783669,-9.52370777754828,-8.90870655684256,-8.3059009741584,-7.92008473066765,-7.29087249108115,-7.29960580813844,-6.93931772220009,-6.94309010175717,-7.11636117923104,-7.15542856204412,-7.73818368899972,-7.98824594486014,-7.8470142554066,-7.98767750410492,-8.068551120642,-7.54568897871243,-7.22229786543002,-6.62264454511463,-5.87747039146587,-5.86424122479947,-5.98438892987823,-5.96568206138816,-6.10009246177961,-5.78993051516391,-5.68430388755946,-5.24836150474518,-4.9912712540926,-4.78110320396197,-4.88295745200897,-4.50013844159121,-4.51000864015864,-4.79309213625347,-4.97023894614997,-4.34350717531459,-3.85516489015611,-3.53513274497209,-2.71239226645344,-1.7409270157983,-1.22581633871332,-0.743830254727142,-0.424831638188876,-0.0580839982135096,0.288923244626053,0.855658677571411,1.74183197672868,2.36572154378752,2.9004434269285,3.50438589112323,4.20178518311852,4.70950613038598,5.03152781821265,4.69167776124537,4.32278554933008,4.22434194581388,4.02916006104729,4.63305084881001,4.71012624757326,4.60969310141435,5.10878408448895,4.89724660890256,4.92724477784785,5.17040822999717,5.25608775473677,5.15087453859131,5.12785268800519,5.23394440350009,4.4084133976378,4.28715464926459,4.4550772159971,4.38926727947331,4.60080475506008,4.17369904216787,3.50917160422244,3.50916596111063,2.33988332764186,2.20446523682122,1.84939647054376,1.58380544677967,1.06231273030602,0.597379868976472,-0.205310153813445,-0.587405694179271,-1.34131316488571,-1.62005584066834,-2.21510995850913,-2.29221119548857,-2.83925790773017,-3.29390715903389,-4.49998341229434]}]],[[{"lng":[41.58513,40.993,40.98105,41.855083092644,42.12861,42.76967,43.66087,44.9636,47.78942,48.4867358742269,48.9381295102964,48.938232863161,48.9384912453225,48.9420052427183,48.9482047585097,48.9482047585099,49.26776,49.72862,50.25878,50.73202,51.1112,51.13387,51.04153,51.04531,50.83418,50.55239,50.07092,49.4527,48.59455,47.74079,46.56476,45.56399,44.06815,43.13597,42.04157,41.81095,41.58513],"lat":[-1.6832500000002,-0.858290000000163,2.78452000000011,3.91891192048376,4.23412999999991,4.25259000000017,4.95754999999995,5.00162000000004,8.00300000000016,8.83762624758973,9.45174896894627,9.97350006758193,10.9823273787832,11.3942660587987,11.410617281698,11.410617281698,11.43033,11.5789,11.67957,12.0219000000002,12.0246399999998,11.7481500000002,11.1665100000001,10.6409000000002,10.2797199999994,9.19873999999999,8.08173000000019,6.80466000000015,5.33910999999971,4.21940000000015,2.85528999999983,2.04575999999946,1.05282999999993,0.292200000000048,-0.919160000000231,-1.44647000000012,-1.6832500000002]}]],[[{"lng":[39.20222,37.7669,37.6986899999999,34.07262,33.9037111971045,33.8935689696669,34.18,34.6721,35.03599,34.59607,34.47913,34.005,34.6201962678539,35.298007118233,35.8174476623535,35.8174476623535,36.1590786328556,36.8550932380081,38.120915,38.43697,38.67114,38.89251,39.5593842587659,39.85494,40.76848,41.1718,41.855083092644,40.98105,40.993,41.58513,40.88477,40.63785,40.26304,40.12119,39.80006,39.60489,39.20222],"lat":[-4.67677000000032,-3.67711999999945,-3.0969899999995,-1.05981999999978,-0.94999999999952,0.109813537861806,0.515000000000006,1.17694,1.90583999999997,3.05374000000034,3.55560000000022,4.24988494736234,4.84712274208211,5.5060000000001,5.33823208279071,4.77696566346184,4.4478641276728,4.4478641276728,3.59860500000003,3.58850999999996,3.61606999999994,3.50073999999981,3.42206000000053,3.83879000000044,4.25701999999984,3.91909000000011,3.91891192048376,2.78452000000011,-0.858290000000163,-1.6832500000002,-2.08254999999989,-2.49979000000018,-2.57309000000051,-3.2776800000002,-3.68116000000003,-4.34653000000029,-4.67677000000032]}]],[[{"lng":[24.5673690121521,23.8058134294668,23.459012892356,23.3947790870172,23.5572497901428,23.5543042335022,22.9775435726926,22.8641654802442,22.8762199999999,22.50869,22.49762,22.28801,21.93681,22.03759,22.29658,22.18329,22.51202,22.30351,22.5679500000001,23.0245900000001,23.8868900000001,23.8376600000001,23.8500000000001,25.0000000000001,25,29.02,32.9,36.86623,37.1887200000001,36.9694099999999,37.1147000000001,37.4817900000001,37.8627600000001,38.4100899594732,37.9040000000001,37.1674700000001,36.8525300000001,36.7538900000001,36.32322,36.4295100000001,36.2702200000001,35.86363,35.2604900000001,34.8316300000001,34.7311500000001,34.2574500000001,33.9616200000001,33.9749800000001,33.9633927949712,33.8249634809075,33.8421308530282,33.7219592481831,33.2069380845618,33.0867664797167,33.2069380845618,32.7434190373025,32.6747495488196,32.0738915245948,32.3142347342848,32.4000715948883,31.8507156870255,31.3528618955249,30.8378407319034,29.9966394979886,29.6189573113328,29.5159530786086,29.0009319149872,28.9665971707458,27.9708895877444,27.8335506107788,27.1125209817089,26.7520061671738,26.4773282132425,25.962307049621,25.7906333284139,25.069603699344,24.7949257454127,24.537415163602,24.1940677211877,23.8869795808607,24.5673690121521],"lat":[8.2291879337856,8.66631887454243,8.95428579348854,9.2650678572925,9.68121816653863,10.089255275915,10.7144625919981,11.1423951278074,11.3846100000003,11.6793600000003,12.2602400000002,12.64605,12.5881800000002,12.9554599999995,13.3723199999999,13.7864800000002,14.0931799999997,14.32682,14.9442900000004,15.6807200000003,15.6108399999994,19.5804699999999,20.0000000000002,20.0030400000001,21.9999999999997,21.9999999999995,21.9999999999997,21.9999999999997,21.0188500000002,20.83744,19.8079599999998,18.6140900000005,18.3678599999999,17.99830739997,17.4275399999998,17.2631400000001,16.9565500000006,16.2918599999997,14.8224899999998,14.4221100000004,13.5633300000002,12.5782799999996,12.0828600000006,11.3189599999999,10.9101699999998,10.6300899999998,9.58358000000019,8.68456000000007,9.46428522942106,9.48406084571541,9.98191463721596,10.3252620796304,10.7201116384068,11.4411412674762,12.1793382686664,12.2480077571497,12.0248319195808,11.9733298032187,11.681484477166,11.0806264529416,10.5312705450785,9.81024091600827,9.70723668328428,10.2909273353888,10.0849188699402,9.79307354388752,9.60423245056005,9.39822398511191,9.39822398511191,9.60423245056017,9.63856719480163,9.46689347359502,9.55273033419781,10.1364209863024,10.4110989402338,10.2737599632678,9.81024091600904,8.91753756573175,8.72869647240424,8.61972971293243,8.2291879337856]}]],[[{"lng":[23.8376600000001,23.8868900000001,23.0245900000001,22.5679500000001,22.30351,22.51202,22.18329,22.29658,22.03759,21.93681,22.28801,22.49762,22.50869,22.8762199999999,22.8641654802442,22.2311291846688,21.7238216488595,21.0008683610962,20.0596854997643,19.094008009526,18.8120097185093,18.9110217627805,18.3895548845232,17.9649296403809,16.7059883968863,16.4561845231873,16.2905615576919,16.1062317237067,15.2794604834691,15.4360917497457,15.1208655127653,14.9799955583377,14.5444665869818,13.954218377344,14.171466098699,14.6272005550811,14.9093538753947,15.4678727556052,14.923564894275,14.9601518083376,14.89336,14.4957873877628,14.5957812842476,13.9544767595056,13.9566988460941,13.5403935075508,13.97217,15.2477311540418,15.3004411149797,15.6857405941478,15.9032466976643,15.4871480648501,15.4710600000001,15.0968876481819,14.8513,15.8608500000001,19.8492600000001,23.8376600000001],"lat":[19.5804699999999,15.6108399999994,15.6807200000003,14.9442900000004,14.32682,14.0931799999997,13.7864800000002,13.3723199999999,12.9554599999995,12.5881800000002,12.64605,12.2602400000002,11.6793600000003,11.3846100000003,11.1423951278074,10.9718887394609,10.5670555688856,9.47598521569197,9.01270600019456,9.0748469100255,8.98291453697861,8.63089468020632,8.28130361575171,7.89091400800287,7.5083275415301,7.73477366783289,7.75430735923892,7.49708791750611,7.42192454673781,7.69281240481132,8.38215017336905,8.79610423424365,8.96586131432249,9.54949494062671,10.0213782821,9.92091929772449,9.99212942142262,9.98233673750288,10.8913251815173,11.555574042197,12.2190500000002,12.8593962671375,13.3304269474777,13.3534487980636,13.9966911890171,14.3671336939009,15.6843700000003,16.6273058130506,17.9279499374048,19.9571800806427,20.3876189234176,20.7304145370256,21.04845,21.3085187850748,22.8629499999999,23.4097199999996,21.4950900000001,19.5804699999999]}]],[[{"lng":[16.3449768408952,16.8240173682409,17.2189286638154,17.3874971859515,17.8361519711095,18.4648991228048,19.0021273129111,19.8947343278886,19.8957678565344,20.1657255388272,20.7586092465118,20.6664701677354,20.8896090023717,21.6058960303694,22.1059688656579,22.5795316911806,22.8242712745149,23.3120967953502,23.7335697771227,24.2112667172288,25.0251705258258,25.6646663754377,25.7658488298652,25.9416520525222,26.4857532081233,26.7864066911974,27.1194096208862,28.0172359555253,29.432188348109,29.839036899543,30.3228833350918,30.6598653500671,31.1914091326213,31.6703979835347,31.9305888201242,31.7524084815819,31.8377779477281,31.3331575863979,31.0440796241572,30.9496667823599,30.6766085141296,30.6859619483745,31.2827730649133,31.8680603370511,32.0716654802811,32.8301204770289,32.5802649268977,32.4621326026785,32.203388706193,31.5210014177789,31.325561150851,30.9017627296253,30.6228133481138,30.0557161801428,28.9255526059195,28.2197558936771,27.464608188596,26.4194523454928,25.9096643409335,25.7806282895007,25.172861769316,24.6778532243921,23.5940434099346,22.9881889177447,22.5741573422222,21.542799106541,20.689052768647,20.0712610205976,19.6164050635646,19.1932784359587,18.8553145687699,18.4246431820494,18.3774109229346,18.2444991390799,18.2500801937674,17.9251904639484,18.2479097836112,18.2217615088715,17.5669177588689,17.0644161312627,17.0629175147262,16.3449768408952],"lat":[-28.5767050106981,-28.0821615536646,-28.3559432919466,-28.7835140927302,-28.8563778622614,-29.0454619280171,-28.9724431291885,-28.4611048316604,-24.7677902157609,-24.9179619280012,-25.8681364885517,-26.4774533017051,-26.828542982696,-26.7265337053516,-26.2802560360792,-25.979447523708,-25.5004586727945,-25.2686898739657,-25.3901294898517,-25.6702157528737,-25.7196700985768,-25.4868160946699,-25.1748454729236,-24.6963733863336,-24.616326592713,-24.2406906063831,-23.5743230119796,-22.8277535946593,-22.0913127580673,-22.1022164852806,-22.2716118303337,-22.1515674781197,-22.2515096981722,-23.6589690080741,-24.3694165992224,-25.4842839494874,-25.8433318010512,-25.6601905250085,-25.7314523251396,-26.0226490211041,-26.3980783017048,-26.7438453101693,-27.2858794084787,-27.1779273414213,-26.7338200823048,-26.7421916643359,-27.4701575660313,-28.301011244421,-28.75240488049,-29.2573869768463,-29.4019776343988,-29.9099569638278,-30.4237757301061,-31.1402694638331,-32.1720411109725,-32.7719528134491,-33.226963799779,-33.6149504534261,-33.6670402971763,-33.9446460914488,-33.7968514950936,-33.9871757952242,-33.7944743792078,-33.9164307594169,-33.8640825335054,-34.2588387997831,-34.4171753883249,-34.7951368141079,-34.8191663551235,-34.4625989723097,-34.4443055152787,-33.997872816709,-34.1365206845483,-33.8677515601984,-33.2814307594148,-32.611290785454,-32.4291313616245,-31.6616329892251,-30.7257211239874,-29.8786410458593,-29.8759538713797,-28.5767050106981]},{"lng":[28.9782625668572,28.5417000668555,28.0743384132078,27.5325110206275,26.9992619158076,27.7493970069565,28.1072046241454,28.2910693702399,28.8483996925077,29.018415154748,29.3251664568326,28.9782625668572],"lat":[-28.9555966122615,-28.6475017229379,-28.8514686011935,-29.2427108700756,-29.8759538713797,-30.6451058896119,-30.545732110315,-30.226216729454,-30.0700505510682,-29.7437655575775,-29.2573869768463,-28.9555966122615]}]],[[{"lng":[28.9782625668572,29.3251664568326,29.018415154748,28.8483996925077,28.2910693702399,28.1072046241454,27.7493970069565,26.9992619158076,27.5325110206275,28.0743384132078,28.5417000668555,28.9782625668572],"lat":[-28.9555966122615,-29.2573869768463,-29.7437655575775,-30.0700505510682,-30.226216729454,-30.545732110315,-30.6451058896119,-29.8759538713797,-29.2427108700756,-28.8514686011935,-28.6475017229379,-28.9555966122615]}]],[[{"lng":[31.1914091326213,30.6598653500671,30.3228833350918,29.839036899543,29.432188348109,28.7946562029242,28.0213700701086,27.7272278175033,27.7247473487533,27.2965047543505,26.1647908871585,25.8503914730947,25.6491634457502,25.264225701608,26.3819352556489,26.7067733090356,27.0444271176307,27.5982434425028,28.4679061215427,28.8258687680285,28.9474634132113,29.5168343442031,30.2742558123051,30.3389547055345,31.1730639991577,31.6364982439512,31.8520406430406,32.3282389666102,32.8476387875758,32.8498608741644,32.6548856951271,32.6119942563249,32.7727079607526,32.6597432797626,32.5086930681734,32.244988234188,31.1914091326213],"lat":[-22.2515096981722,-22.1515674781197,-22.2716118303337,-22.1022164852806,-22.0913127580673,-21.6394540341074,-21.4859750302009,-20.8518018531154,-20.4990585262905,-20.3915198706908,-19.2930856258949,-18.7144129370907,-18.5360258928189,-17.7365398088312,-17.8460421688585,-17.9612289364363,-17.9380262183375,-17.2908305803141,-16.468400160389,-16.3897486304404,-16.0430514461939,-15.6446778296568,-15.5077869605151,-15.88083912523,-15.8609436987981,-16.0719902482778,-16.319417006091,-16.3920740698937,-16.7133981258845,-17.9790573055772,-18.6720899390439,-19.4193828264162,-19.7155921363131,-20.3042900529826,-20.3952922502481,-21.1164885393136,-22.2515096981722]}]],[[{"lng":[29.432188348109,28.0172359555253,27.1194096208862,26.7864066911974,26.4857532081233,25.9416520525222,25.7658488298652,25.6646663754377,25.0251705258258,24.2112667172288,23.7335697771227,23.3120967953502,22.8242712745149,22.5795316911806,22.1059688656579,21.6058960303694,20.8896090023717,20.6664701677354,20.7586092465118,20.1657255388272,19.8957678565344,19.8954577979407,20.8811340674759,20.9106413103145,21.655040317479,23.1968583513393,23.5790055681377,24.2173645362392,24.5207051937925,25.0844433936646,25.264225701608,25.6491634457502,25.8503914730947,26.1647908871585,27.2965047543505,27.7247473487533,27.7272278175033,28.0213700701086,28.7946562029242,29.432188348109],"lat":[-22.0913127580673,-22.8277535946593,-23.5743230119796,-24.2406906063831,-24.616326592713,-24.6963733863336,-25.1748454729236,-25.4868160946699,-25.7196700985768,-25.6702157528737,-25.3901294898517,-25.2686898739657,-25.5004586727945,-25.979447523708,-26.2802560360792,-26.7265337053516,-26.828542982696,-26.4774533017051,-25.8681364885517,-24.9179619280012,-24.7677902157609,-21.8491569963483,-21.8143270809829,-18.2522189266724,-18.2191460100054,-17.8690381812276,-18.2812610816199,-17.8893470191185,-17.8871249325299,-17.6618156877379,-17.7365398088312,-18.5360258928189,-18.7144129370907,-19.2930856258949,-20.3915198706908,-20.4990585262905,-20.8518018531154,-21.4859750302009,-21.6394540341074,-22.0913127580673]}]],[[{"lng":[19.8957678565344,19.8947343278886,19.0021273129111,18.4648991228048,17.8361519711095,17.3874971859515,17.2189286638154,16.8240173682409,16.3449768408952,15.6018180681058,15.2104724463595,14.9897107276086,14.7432141455763,14.4081441585958,14.3857165869812,14.2577140641942,13.8686422054687,13.3524979997374,12.8268453304645,12.6085640804636,11.7949186540281,11.7341988460851,12.2154614600194,12.8140812516884,13.46236209479,14.058501417709,14.209706658595,18.2633093604342,18.9561869646036,21.3771761410456,23.2150484555061,24.0338615251708,24.6823490740015,25.0769503109823,25.0844433936646,24.5207051937925,24.2173645362392,23.5790055681377,23.1968583513393,21.655040317479,20.9106413103145,20.8811340674759,19.8954577979407,19.8957678565344],"lat":[-24.7677902157609,-28.4611048316604,-28.9724431291885,-29.0454619280171,-28.8563778622614,-28.7835140927302,-28.3559432919466,-28.0821615536646,-28.5767050106981,-27.8212472470228,-27.0909559058735,-26.1173719214953,-25.3929200171958,-23.8530140113298,-22.6566529273407,-22.1112081845005,-21.6990369605397,-20.8728341610577,-19.6731657854017,-19.0453488094878,-18.069129327062,-17.3018893368245,-17.1116683895583,-16.9413428687242,-16.9712118465892,-17.4233806291425,-17.3531006812256,-17.3099508602621,-17.7890947404723,-17.9306364885197,-17.5231161434662,-17.2958431942464,-17.3534107398196,-17.5788233374766,-17.6618156877379,-17.8871249325299,-17.8893470191185,-18.2812610816199,-17.8690381812276,-18.2191460100054,-18.2522189266724,-21.8143270809829,-21.8491569963483,-24.7677902157609]}]],[[{"lng":[-16.7137288070235,-17.1261067367126,-17.6250426904907,-17.1851728988222,-16.7007063460859,-16.4630981104079,-16.1206900700419,-15.6236661442587,-15.1357372705588,-14.577347581429,-14.0995214502422,-13.4357376774531,-12.8306583317475,-12.1707502913803,-12.1248874577213,-11.9277160303116,-11.5533977930054,-11.4678991357785,-11.5139428369506,-11.6583009505579,-12.2035648258856,-12.2785990055734,-12.4990506657306,-13.2178181624782,-13.7004760400843,-15.548476935274,-15.8165742660043,-16.1477168441306,-16.6774519515546,-16.8415246240813,-15.9312959456922,-15.691000535535,-15.5118125065629,-15.1411632959495,-14.7121972314946,-14.2777017887846,-13.8449633447724,-14.0469923568175,-14.3767138330558,-14.6870308089685,-15.0817353988138,-15.3987703109245,-15.6245963200399,-16.7137288070235],"lat":[13.5949586043797,14.3735157332894,14.7295405135639,14.9194772404531,15.6215274113542,16.1350361190388,16.4556625431933,16.3693370630502,16.5872824162406,16.5982636581027,16.3043022730107,16.0393830428663,15.303691514543,14.6168342147354,13.9947274845899,13.4220751001472,13.141213690641,12.7545189478008,12.4429875757289,12.3865827498828,12.4656476912894,12.3544400089971,12.3320899520311,12.5758735213678,12.5861829696099,12.6281700708476,12.5155671248833,12.5477615422012,12.3848515894009,13.1513939478027,13.1302841252111,13.2703530949385,13.278569647673,13.5095116235847,13.2982066919441,13.2805850285325,13.5050416121921,13.7940678980003,13.6256802433775,13.6303569604995,13.8764918075055,13.8603687606311,13.6235873478696,13.5949586043797]}]],[[{"lng":[-11.5139428369506,-11.4678991357785,-11.5533977930054,-11.9277160303116,-12.1248874577213,-12.1707502913803,-11.8342075260795,-11.6660782536178,-11.3490950179395,-10.6507913883794,-10.0868464827782,-9.7002550928027,-9.55023840985939,-5.53774430990845,-5.31527726889193,-5.48852250815044,-5.97112870932424,-6.45378658693033,-4.92333736817423,-1.55005489745761,1.82322757325903,2.06099083823393,2.68358849448643,3.14666100425391,3.15813317222271,4.26741946780004,4.2702099951438,3.72342166506349,3.63825890464648,2.74999270998149,1.38552819174686,1.01578331869849,0.374892205414688,-0.266257290030585,-0.51585445800034,-1.06636349120566,-2.00103512206877,-2.19182451009038,-2.96769446452058,-3.10370683431276,-3.52280270019986,-4.00639075358723,-4.28040503581488,-4.42716610352379,-5.22094194174312,-5.19784257650864,-5.470564947929,-5.40434159994697,-5.81692623536528,-6.05045203289226,-6.20522294760643,-6.49396501303727,-6.66646094402754,-6.85050655763505,-7.6227591618048,-7.89958980959238,-8.02994361004861,-8.33537716310974,-8.28235714357828,-8.40731075686002,-8.62032101076713,-8.58130530438677,-8.37630489748491,-8.78609900555947,-8.90526485842453,-9.12747351727958,-9.327616339546,-9.56791174970322,-9.89099280439201,-10.1652137923488,-10.5932238428063,-10.8708296370782,-11.0365559554383,-11.2975736149445,-11.4561685856483,-11.5139428369506],"lat":[12.4429875757289,12.7545189478008,13.141213690641,13.4220751001472,13.9947274845899,14.6168342147354,14.799096991429,15.3882083195563,15.4112560083587,15.1327458765213,15.3304857446856,15.2641073674075,15.4864968937753,15.5016897648692,16.201853745992,16.3251020370082,20.6408334416477,24.9565906845035,24.9745740829413,22.7926659204978,20.6108094344862,20.1422333846798,19.8562301701604,19.6935785995213,19.0573642033599,19.1552652043368,16.8522274846013,16.1842837590123,15.5681198185804,15.4095248478768,15.3235611027592,14.9681822778879,14.9289081893461,14.9243089868723,15.1161577417556,14.9738150090078,14.5590082870009,14.2464175480675,13.7981503361512,13.5412667912286,13.3376616479984,13.472485459848,13.2284435083501,12.5426455754048,11.7138589543074,11.3751457788504,10.9512698429755,10.3707368026091,10.2225546330122,10.0963607853555,10.5240607772194,10.4113028019583,10.4308106551487,10.1389938419959,10.1472362329467,10.2973821069711,10.2065349390013,10.4948119165421,10.7925973576238,10.9092569035227,10.8108908146553,11.1362456323647,11.3936459416104,11.8125609399846,12.0883580591265,12.3080604110153,12.3342862004036,12.1942430688928,12.0604786239055,11.844083563683,11.923975328006,12.1778874780721,12.2112446151167,12.0779710962358,12.0768342147255,12.4429875757289]}]],[[{"lng":[-17.0634232243426,-16.845193650774,-12.9291019352635,-13.1187544417747,-12.8742215641696,-11.9372244938533,-11.9694189111712,-8.6872936670174,-8.68439978680905,-4.92333736817423,-6.45378658693033,-5.97112870932424,-5.48852250815044,-5.31527726889193,-5.53774430990845,-9.55023840985939,-9.7002550928027,-10.0868464827782,-10.6507913883794,-11.3490950179395,-11.6660782536178,-11.8342075260795,-12.1707502913803,-12.8306583317475,-13.4357376774531,-14.0995214502422,-14.577347581429,-15.1357372705588,-15.6236661442587,-16.1206900700419,-16.4630981104079,-16.5497078109291,-16.2705517236883,-16.1463474186749,-16.2568833073472,-16.3776511296133,-16.2778381006415,-16.5363236149655,-17.0634232243426],"lat":[20.9997521021309,21.3333234725747,21.3270706242674,22.7712202010962,23.284832261645,23.374594224536,25.9333527694684,25.8810562199892,27.3957441268962,24.9745740829413,24.9565906845035,20.6408334416477,16.3251020370082,16.201853745992,15.5016897648692,15.4864968937753,15.2641073674075,15.3304857446856,15.1327458765213,15.4112560083587,15.3882083195563,14.799096991429,14.6168342147354,15.303691514543,16.0393830428663,16.3043022730107,16.5982636581027,16.5872824162406,16.3693370630502,16.4556625431933,16.1350361190388,16.6738921167617,17.1669627954748,18.1084815536168,19.0967158065505,19.5938172469821,20.0925206568144,20.5678663192514,20.9997521021309]}]],[[{"lng":[2.69170169435626,1.86524051271232,1.61895063640924,1.66447757325838,1.46304284018467,1.42506066245014,1.07779503744874,0.772335646171485,0.89956302247407,1.24346967937649,1.44717817547107,1.93598554851989,2.15447350424993,2.49016360841793,2.84864301922659,3.61118045412556,3.57221642417747,3.79711225751172,3.6000700211828,3.70543826662592,3.2203515967021,2.91230838381026,2.72379275880951,2.74906253420022,2.69170169435626],"lat":[6.25881724692864,6.14215770102923,6.83203807212611,9.12859039960936,9.33462433515725,9.82539541263282,10.1756065942754,10.4708082137427,10.9973393823644,11.1105107690836,11.547719224489,11.6411502140724,11.9401500513132,12.2330520695431,12.2356358911582,11.6601671411559,11.3279393579515,10.7347455916731,10.3321861841194,10.0632103540405,9.44415253339926,9.13760793704429,8.50684540448963,7.87073436119333,6.25881724692864]}]],[[{"lng":[14.8513,15.0968876481819,15.4710600000001,15.4871480648501,15.9032466976643,15.6857405941478,15.3004411149797,15.2477311540418,13.97217,13.5403935075508,13.9566988460941,13.9544767595056,14.5957812842476,14.4957873877628,14.2135307145846,14.1813362972668,13.9953528174483,13.3187016130186,13.0839872575488,12.3020711605405,11.5278031755114,10.9895931331915,10.7010319352737,10.1148144873547,9.52492801274295,9.01493330245444,7.80467125817879,7.33074669763002,6.82044192874775,6.44542605960564,5.44305830244014,4.36834354006601,4.10794599774732,3.96728274904885,3.68063357912581,3.61118045412556,2.84864301922659,2.49016360841793,2.15447350424993,2.17710778159378,1.02410322429748,0.993045688490067,0.429927605805517,0.295646396495101,0.374892205414688,1.01578331869849,1.38552819174686,2.74999270998149,3.63825890464648,3.72342166506349,4.2702099951438,4.26741946780004,5.67756595218068,8.57289310062979,11.9995056494716,13.5814245947905,14.1438708838552,14.8513],"lat":[22.8629499999999,21.3085187850748,21.04845,20.7304145370256,20.3876189234176,19.9571800806427,17.9279499374048,16.6273058130506,15.6843700000003,14.3671336939009,13.9966911890171,13.3534487980636,13.3304269474777,12.8593962671375,12.8020354272935,12.483656927943,12.4615652531384,13.5563563094576,13.5961471623224,13.0371890324368,13.3289800073739,13.3873226994312,13.2469178328937,13.2772518986494,12.8511021997546,12.82665924728,13.3435269230639,13.0980380314611,13.1150912541175,13.4927684595226,13.8659239771019,13.7474815942892,13.5312157251483,12.9561087101719,12.5529033472139,11.6601671411559,12.2356358911582,12.2330520695431,11.9401500513132,12.6250178084776,12.8518256698066,13.3357496200037,13.9887330184439,14.4442349308806,14.9289081893461,14.9681822778879,15.3235611027592,15.4095248478768,15.5681198185804,16.1842837590123,16.8522274846013,19.1552652043368,19.6012069767994,21.5656607121591,23.4716684025964,23.0405060897689,22.4912889673715,22.8629499999999]}]],[[{"lng":[2.69170169435626,2.74906253420022,2.72379275880951,2.91230838381026,3.2203515967021,3.70543826662592,3.6000700211828,3.79711225751172,3.57221642417747,3.61118045412556,3.68063357912581,3.96728274904885,4.10794599774732,4.36834354006601,5.44305830244014,6.44542605960564,6.82044192874775,7.33074669763002,7.80467125817879,9.01493330245444,9.52492801274295,10.1148144873547,10.7010319352737,10.9895931331915,11.5278031755114,12.3020711605405,13.0839872575488,13.3187016130186,13.9953528174483,14.1813362972668,14.5771777686225,14.468192172919,14.4153788591167,13.5729496598946,13.3086763851539,13.1675997249971,12.955467970439,12.7536715023392,12.2188721045506,12.0639461605396,11.8393087093668,11.7457743669185,11.0587878760304,10.4973751156114,10.1182768083183,9.5227059261544,9.23316287602305,8.75753299320863,8.5002877132597,7.46210818851594,7.08259646976444,6.6980721370806,5.89817264163469,5.36280480309088,5.03357425295937,4.32560713056069,3.57418012860455,2.69170169435626],"lat":[6.25881724692864,7.87073436119333,8.50684540448963,9.13760793704429,9.44415253339926,10.0632103540405,10.3321861841194,10.7347455916731,11.3279393579515,11.6601671411559,12.5529033472139,12.9561087101719,13.5312157251483,13.7474815942892,13.8659239771019,13.4927684595226,13.1150912541175,13.0980380314611,13.3435269230639,12.82665924728,12.8511021997546,13.2772518986494,13.2469178328937,13.3873226994312,13.3289800073739,13.0371890324368,13.5961471623224,13.5563563094576,12.4615652531384,12.483656927943,12.0853608260535,11.9047516951931,11.5723688826916,10.7985659855533,10.1603620467491,9.64062632897336,9.41777171471503,8.71776276288872,8.30582408287447,7.79980845787238,7.39704234458945,6.98138296144978,6.64442678469077,7.05535777427539,7.03876963950935,6.45348236737186,6.44449066815327,5.47966583904808,4.7719829370269,4.4121082625464,4.46468903240337,4.24059418376939,4.26245331462881,4.88797068930631,5.61180247641815,6.27065114992348,6.25830048260573,6.25881724692864]}]],[[{"lng":[14.4957873877628,14.89336,14.9601518083376,14.923564894275,15.4678727556052,14.9093538753947,14.6272005550811,14.171466098699,13.954218377344,14.5444665869818,14.9799955583377,15.1208655127653,15.4360917497457,15.2794604834691,14.7765454444046,14.5365600928411,14.4594071794294,14.5589359880235,14.4783724300805,14.9509534033897,15.0362195166713,15.4053959489644,15.8627323747475,15.9073808122477,16.0128524105554,15.9409188168051,15.1463419938852,14.3378125342466,13.0758223812468,12.9513338558556,12.3593803239522,11.7516654801998,11.2764490088437,9.64915815597263,9.79519575362946,9.404366896206,8.94811567550107,8.74492394372942,8.48881554529089,8.5002877132597,8.75753299320863,9.23316287602305,9.5227059261544,10.1182768083183,10.4973751156114,11.0587878760304,11.7457743669185,11.8393087093668,12.0639461605396,12.2188721045506,12.7536715023392,12.955467970439,13.1675997249971,13.3086763851539,13.5729496598946,14.4153788591167,14.468192172919,14.5771777686225,14.1813362972668,14.2135307145846,14.4957873877628],"lat":[12.8593962671375,12.2190500000002,11.555574042197,10.8913251815173,9.98233673750288,9.99212942142262,9.92091929772449,10.0213782821,9.54949494062671,8.96586131432249,8.79610423424365,8.38215017336905,7.69281240481132,7.42192454673781,6.40849803306205,6.22695872642099,5.45176056561061,5.03059764243164,4.73260549562053,4.2103893090947,3.85136729574712,3.33530060466475,3.01353729899898,2.55738943115883,2.26763967529809,1.72767263428021,1.96401479736723,2.22787466064943,2.26709707275924,2.32161570882723,2.19281220133951,2.32675751383988,2.26105093018092,2.2838660750377,3.07340444580914,3.73452688233535,3.90412893311718,4.35221527751989,4.49561737712977,4.7719829370269,5.47966583904808,6.44449066815327,6.45348236737186,7.03876963950935,7.05535777427539,6.64442678469077,6.98138296144978,7.39704234458945,7.79980845787238,8.30582408287447,8.71776276288872,9.41777171471503,9.64062632897336,10.1603620467491,10.7985659855533,11.5723688826916,11.9047516951931,12.0853608260535,12.483656927943,12.8020354272935,12.8593962671375]}]],[[{"lng":[0.89956302247407,0.772335646171485,1.07779503744874,1.42506066245014,1.46304284018467,1.66447757325838,1.61895063640924,1.86524051271232,1.06012169760493,0.83693118653633,0.570384148774851,0.490957472342239,0.71202924968688,0.461191847342118,0.365900506195884,0.367579990245386,-0.0497847151599382,0.0238025244237014,0.89956302247407],"lat":[10.9973393823644,10.4708082137427,10.1756065942754,9.82539541263282,9.33462433515725,9.12859039960936,6.83203807212611,6.14215770102923,5.92883738852882,6.27997874595197,6.91435862876772,7.41174428957615,8.31246450442415,8.67722260175639,9.46500397382965,10.1912128768272,10.706917832884,11.0186817489009,10.9973393823644]}]],[[{"lng":[0.0238025244237014,-0.0497847151599382,0.367579990245386,0.365900506195884,0.461191847342118,0.71202924968688,0.490957472342239,0.570384148774851,0.83693118653633,1.06012169760493,-0.50763790526594,-1.06362464029419,-1.96470659016759,-2.8561250472024,-2.81070146321783,-3.24437008301126,-2.98358496745032,-2.56218950032624,-2.82749630371271,-2.96389624674711,-2.94040930827046,-1.20335771321143,-0.761575893548178,-0.438701544588578,0.0238025244237014],"lat":[11.0186817489009,10.706917832884,10.1912128768272,9.46500397382965,8.67722260175639,8.31246450442415,7.41174428957615,6.91435862876772,6.27997874595197,5.92883738852882,5.34347260174272,5.00054779705399,4.71046214438339,4.99447581625953,5.3890512150244,6.25047150311335,7.37970490155554,8.21962779381178,9.64246084231957,10.3953347843801,10.9626903345127,11.0098192407626,10.936929633015,11.0983409692791,11.0186817489009]}]],[[{"lng":[-8.02994361004861,-7.89958980959238,-7.6227591618048,-6.85050655763505,-6.66646094402754,-6.49396501303727,-6.20522294760643,-6.05045203289226,-5.81692623536528,-5.40434159994697,-4.9546532861431,-4.77988359213197,-4.33024695476039,-3.98044918457668,-3.51189897298628,-2.82749630371271,-2.56218950032624,-2.98358496745032,-3.24437008301126,-2.81070146321783,-2.8561250472024,-3.31108435710007,-4.00881954590494,-4.6499173649179,-5.83449622234451,-6.52876909018584,-7.51894120933044,-7.71215938966976,-7.63536821128404,-7.53971513511176,-7.57015255373169,-7.99369259279588,-8.31134762209403,-8.60288021486863,-8.38545162600058,-8.48544552248534,-8.4392984684487,-8.28070349774492,-8.22179236493219,-8.29904863120856,-8.20349890790089,-7.83210038901919,-8.07911373537436,-8.30961646161225,-8.22933712404682,-8.02994361004861],"lat":[10.2065349390013,10.2973821069711,10.1472362329467,10.1389938419959,10.4308106551487,10.4113028019583,10.5240607772194,10.0963607853555,10.2225546330122,10.3707368026091,10.15271393477,9.82198476810148,9.61083486575713,9.86234406172175,9.90032623945599,9.64246084231957,8.21962779381178,7.37970490155554,6.25047150311335,5.3890512150244,4.99447581625953,4.98429555909781,5.17981334067418,5.16826365805684,4.99370066977559,4.70508779542513,4.33828847901699,4.3645659448378,5.18815908448971,5.31334524171641,5.70735219972564,6.12618968345162,6.19303314862119,6.46756419517186,6.91180064536847,7.3952078312428,7.68604279218161,7.6871796736924,8.12332876223603,8.31644358971039,8.45545319257527,8.57570425051862,9.37622386315151,9.78953196862261,10.129020290564,10.2065349390013]}]],[[{"lng":[-13.7004760400843,-13.2178181624782,-12.4990506657306,-12.2785990055734,-12.2035648258856,-11.6583009505579,-11.5139428369506,-11.4561685856483,-11.2975736149445,-11.0365559554383,-10.8708296370782,-10.5932238428063,-10.1652137923488,-9.89099280439201,-9.56791174970322,-9.327616339546,-9.12747351727958,-8.90526485842453,-8.78609900555947,-8.37630489748491,-8.58130530438677,-8.62032101076713,-8.40731075686002,-8.28235714357828,-8.33537716310974,-8.02994361004861,-8.22933712404682,-8.30961646161225,-8.07911373537436,-7.83210038901919,-8.20349890790089,-8.29904863120856,-8.22179236493219,-8.28070349774492,-8.4392984684487,-8.72212358238212,-8.926064622422,-9.20878638349084,-9.40334815106974,-9.33727983238459,-9.75534216962584,-10.0165665348612,-10.2300935530913,-10.5054772607747,-10.4943151513996,-10.6547704736659,-10.622395188835,-10.8391519840833,-11.1174812484073,-11.9172773909886,-12.150338100625,-12.4259285140376,-12.5967191227622,-12.7119575667731,-13.2465502588325,-13.6851539779098,-14.0740449691223,-14.3300758529124,-14.5796988590983,-14.6932319808435,-14.8395537988779,-15.1303112451682,-14.6856872217289,-14.3821915348787,-14.1214064193178,-13.9007997298638,-13.7431607731574,-13.8282718571421,-13.7187436588995,-13.7004760400843],"lat":[12.5861829696099,12.5758735213678,12.3320899520311,12.3544400089971,12.4656476912894,12.3865827498828,12.4429875757289,12.0768342147255,12.0779710962358,12.2112446151167,12.1778874780721,11.923975328006,11.844083563683,12.0604786239055,12.1942430688928,12.3342862004036,12.3080604110153,12.0883580591265,11.8125609399846,11.3936459416104,11.1362456323647,10.8108908146553,10.9092569035227,10.7925973576238,10.4948119165421,10.2065349390013,10.129020290564,9.78953196862261,9.37622386315151,8.57570425051862,8.45545319257527,8.31644358971039,8.12332876223603,7.6871796736924,7.68604279218161,7.71167430259852,7.30903738039653,7.31392080324782,7.5269052189386,7.92853445071135,8.5410552026667,8.42850393313548,8.40620555260126,8.34889638918976,8.71554067630046,8.97717845299426,9.26791006106859,9.68824616133001,10.0458729110064,10.0469839543007,9.85857168216416,9.83583405195622,9.62018830000199,9.34271169681078,8.903048610871,9.49474376061352,9.88616689700821,10.015719712764,10.2144672713586,10.656300767454,10.8765715600984,11.0404116886795,11.5278237980565,11.5092719588634,11.6771170109477,11.6787189803483,11.8112690291777,12.1426441512491,12.2471855737754,12.5861829696099]}]],[[{"lng":[-16.6774519515546,-16.1477168441306,-15.8165742660043,-15.548476935274,-13.7004760400843,-13.7187436588995,-13.8282718571421,-13.7431607731574,-13.9007997298638,-14.1214064193178,-14.3821915348787,-14.6856872217289,-15.1303112451682,-15.6641804671755,-16.0852141992736,-16.3147867497302,-16.3089473128812,-16.6138382634033,-16.6774519515546],"lat":[12.3848515894009,12.5477615422012,12.5155671248833,12.6281700708476,12.5861829696099,12.2471855737754,12.1426441512491,11.8112690291777,11.6787189803483,11.6771170109477,11.5092719588634,11.5278237980565,11.0404116886795,11.4584740259209,11.5245940210387,11.8065147974065,11.9587018905062,12.1709111597125,12.3848515894009]}]],[[{"lng":[-8.4392984684487,-8.48544552248534,-8.38545162600058,-8.60288021486863,-8.31134762209403,-7.99369259279588,-7.57015255373169,-7.53971513511176,-7.63536821128404,-7.71215938966976,-7.97410722495725,-9.00479366701868,-9.91342037600668,-10.7653838769866,-11.4387794661821,-11.1998018050483,-11.1467042708684,-10.6955948551765,-10.2300935530913,-10.0165665348612,-9.75534216962584,-9.33727983238459,-9.40334815106974,-9.20878638349084,-8.926064622422,-8.72212358238212,-8.4392984684487],"lat":[7.68604279218161,7.3952078312428,6.91180064536847,6.46756419517186,6.19303314862119,6.12618968345162,5.70735219972564,5.31334524171641,5.18815908448971,4.3645659448378,4.35575511313217,4.83241852459203,5.59356069581944,6.14071076092569,6.78591685630603,7.10584564862463,7.39670644777945,7.93946401614144,8.40620555260126,8.42850393313548,8.5410552026667,7.92853445071135,7.5269052189386,7.31392080324782,7.30903738039653,7.71167430259852,7.68604279218161]}]],[[{"lng":[-13.2465502588325,-12.7119575667731,-12.5967191227622,-12.4259285140376,-12.150338100625,-11.9172773909886,-11.1174812484073,-10.8391519840833,-10.622395188835,-10.6547704736659,-10.4943151513996,-10.5054772607747,-10.2300935530913,-10.6955948551765,-11.1467042708684,-11.1998018050483,-11.4387794661821,-11.7081945459357,-12.4280989241938,-12.9490490381282,-13.1240254378685,-13.2465502588325],"lat":[8.903048610871,9.34271169681078,9.62018830000199,9.83583405195622,9.85857168216416,10.0469839543007,10.0458729110064,9.68824616133001,9.26791006106859,8.97717845299426,8.71554067630046,8.34889638918976,8.40620555260126,7.93946401614144,7.39670644777945,7.10584564862463,6.78591685630603,6.86009837486082,7.26294200279159,7.79864573814537,8.16394643801696,8.903048610871]}]],[[{"lng":[-5.40434159994697,-5.470564947929,-5.19784257650864,-5.22094194174312,-4.42716610352379,-4.28040503581488,-4.00639075358723,-3.52280270019986,-3.10370683431276,-2.96769446452058,-2.19182451009038,-2.00103512206877,-1.06636349120566,-0.51585445800034,-0.266257290030585,0.374892205414688,0.295646396495101,0.429927605805517,0.993045688490067,1.02410322429748,2.17710778159378,2.15447350424993,1.93598554851989,1.44717817547107,1.24346967937649,0.89956302247407,0.0238025244237014,-0.438701544588578,-0.761575893548178,-1.20335771321143,-2.94040930827046,-2.96389624674711,-2.82749630371271,-3.51189897298628,-3.98044918457668,-4.33024695476039,-4.77988359213197,-4.9546532861431,-5.40434159994697],"lat":[10.3707368026091,10.9512698429755,11.3751457788504,11.7138589543074,12.5426455754048,13.2284435083501,13.472485459848,13.3376616479984,13.5412667912286,13.7981503361512,14.2464175480675,14.5590082870009,14.9738150090078,15.1161577417556,14.9243089868723,14.9289081893461,14.4442349308806,13.9887330184439,13.3357496200037,12.8518256698066,12.6250178084776,11.9401500513132,11.6411502140724,11.547719224489,11.1105107690836,10.9973393823644,11.0186817489009,11.0983409692791,10.936929633015,11.0098192407626,10.9626903345127,10.3953347843801,9.64246084231957,9.90032623945599,9.86234406172175,9.61083486575713,9.82198476810148,10.15271393477,10.3707368026091]}]],[[{"lng":[27.3742261085175,27.0440653826047,26.4027608578625,25.6504553565575,25.2787984555143,25.1288334490033,24.8050289242624,24.4105310401463,23.2972139828501,22.8414795264681,22.7041235694363,22.4051237321955,21.65912275563,20.9275911801063,20.2906791521089,19.4677836442931,18.9323124528848,18.5429822119978,18.4530652198099,17.8099003435053,17.1330424333463,16.5370581397241,16.0128524105554,15.9073808122477,15.8627323747475,15.4053959489644,15.0362195166713,14.9509534033897,14.4783724300805,14.5589359880235,14.4594071794294,14.5365600928411,14.7765454444046,15.2794604834691,16.1062317237067,16.2905615576919,16.4561845231873,16.7059883968863,17.9649296403809,18.3895548845232,18.9110217627805,18.8120097185093,19.094008009526,20.0596854997643,21.0008683610962,21.7238216488595,22.2311291846688,22.8641654802442,22.9775435726926,23.5543042335022,23.5572497901428,23.3947790870172,23.459012892356,23.8058134294668,24.5673690121521,25.1149324887168,25.1241308936647,25.7966479835112,26.2134184099451,26.4659094581232,27.2134090512252,27.3742261085175],"lat":[5.23394440350009,5.12785268800519,5.15087453859131,5.25608775473677,5.17040822999717,4.92724477784785,4.89724660890256,5.10878408448895,4.60969310141435,4.71012624757326,4.63305084881001,4.02916006104729,4.22434194581388,4.32278554933008,4.69167776124537,5.03152781821265,4.70950613038598,4.20178518311852,3.50438589112323,3.56019643799852,3.72819651937931,3.19825470622646,2.26763967529809,2.55738943115883,3.01353729899898,3.33530060466475,3.85136729574712,4.2103893090947,4.73260549562053,5.03059764243164,5.45176056561061,6.22695872642099,6.40849803306205,7.42192454673781,7.49708791750611,7.75430735923892,7.73477366783289,7.5083275415301,7.89091400800287,8.28130361575171,8.63089468020632,8.98291453697861,9.0748469100255,9.01270600019456,9.47598521569197,10.5670555688856,10.9718887394609,11.1423951278074,10.7144625919981,10.089255275915,9.68121816653863,9.2650678572925,8.95428579348854,8.66631887454243,8.2291879337856,7.82510407147925,7.50008515057971,6.97931590415836,6.54660329836178,5.94671743410217,5.55095347739434,5.23394440350009]}]],[[{"lng":[18.4530652198099,18.3937923519711,18.0942757504074,17.8988354834796,17.7741919287916,17.8265401547033,17.6635526872547,17.63864464689,17.5237162614729,16.8653068376421,16.4070919125101,15.9728031755292,16.0062895036543,15.7535400733148,15.1709916520884,14.5826037940132,14.2090348649752,14.1449560889333,13.6002348161447,13.258240187237,12.9955172054652,12.6207597184845,12.3186076188739,11.9149630062421,11.0937728206919,11.8551216976481,11.4780387712143,11.8209635759032,12.4957027523382,12.5752844580676,13.1096187679656,13.9924072608077,14.2992102393246,14.4254557634136,14.3164184912777,13.8433207536457,14.276265903387,14.0266687354172,13.2826314632788,13.0031136410121,13.0758223812468,14.3378125342466,15.1463419938852,15.9409188168051,16.0128524105554,16.5370581397241,17.1330424333463,17.8099003435053,18.4530652198099],"lat":[3.50438589112323,2.9004434269285,2.36572154378752,1.74183197672868,0.855658677571411,0.288923244626053,-0.0580839982135096,-0.424831638188876,-0.743830254727142,-1.22581633871332,-1.7409270157983,-2.71239226645344,-3.53513274497209,-3.85516489015611,-4.34350717531459,-4.97023894614997,-4.79309213625347,-4.51000864015864,-4.50013844159121,-4.88295745200897,-4.78110320396197,-4.43802336997619,-4.6062301570859,-5.03798674888455,-3.97882659263111,-3.42687061932054,-2.76561899171394,-2.51416147218194,-2.39168832764991,-1.94851124431523,-2.42874032960364,-2.47080494548906,-1.99827564861222,-1.33340667074499,-0.552627455246837,0.0387576359013736,1.19692983642672,1.39567739502107,1.31418366129697,1.83089630778284,2.26709707275924,2.22787466064943,1.96401479736723,1.72767263428021,2.26763967529809,3.19825470622646,3.72819651937931,3.56019643799852,3.50438589112323]}]],[[{"lng":[11.2764490088437,11.7516654801998,12.3593803239522,12.9513338558556,13.0758223812468,13.0031136410121,13.2826314632788,14.0266687354172,14.276265903387,13.8433207536457,14.3164184912777,14.4254557634136,14.2992102393246,13.9924072608077,13.1096187679656,12.5752844580676,12.4957027523382,11.8209635759032,11.4780387712143,11.8551216976481,11.0937728206919,10.0661352881357,9.40524539555497,8.79799563969317,8.83008670414643,9.04841963057959,9.29135053878369,9.49288862472199,9.83028405115564,11.2850789730365,11.2764490088437],"lat":[2.26105093018092,2.32675751383988,2.19281220133951,2.32161570882723,2.26709707275924,1.83089630778284,1.31418366129697,1.39567739502107,1.19692983642672,0.0387576359013736,-0.552627455246837,-1.33340667074499,-1.99827564861222,-2.47080494548906,-2.42874032960364,-1.94851124431523,-2.39168832764991,-2.51416147218194,-2.76561899171394,-3.42687061932054,-3.97882659263111,-2.96948251710521,-2.1443132462693,-1.11130136475447,-0.779073581549632,-0.459351494960059,0.268666083167643,1.01011953369146,1.0678937849939,1.05766185140021,2.26105093018092]}]],[[{"lng":[9.64915815597263,11.2764490088437,11.2850789730365,9.83028405115564,9.49288862472199,9.30561323409626,9.64915815597263],"lat":[2.2838660750377,2.26105093018092,1.05766185140021,1.0678937849939,1.01011953369146,1.16091136311896,2.2838660750377]}]],[[{"lng":[30.7400097314221,31.1577513369501,31.5563480974665,32.1918648617919,32.7593754412213,33.2313879737753,33.4856876970836,33.3153104998173,33.1142891782019,33.3064221534631,32.9917643572379,32.6881653175231,33.2140246925252,30.1794812354818,30.2742558123051,29.5168343442031,28.9474634132113,28.8258687680285,28.4679061215427,27.5982434425028,27.0444271176307,26.7067733090356,26.3819352556489,25.264225701608,25.0844433936646,25.0769503109823,24.6823490740015,24.0338615251708,23.2150484555061,22.5624784685243,21.8878426449539,21.9338863461259,24.0161365088947,23.9309220720454,24.0799052263428,23.9041536801182,24.0178935075926,23.9122152035557,24.257155389104,24.314516228948,24.783169793403,25.4181181169732,25.7523096046047,26.5530875993996,27.1644197934125,27.3887988624238,28.15510867688,28.523561639121,28.9342859229768,29.6996138852195,29.6160014177712,29.3415478858691,28.6424174333924,28.3722530453704,28.4960697771418,28.6736816749289,28.4498710466728,28.7348665707625,29.0029122250605,30.3460860531908,30.7400097314221],"lat":[-8.34000593035405,-8.59457874731718,-8.76204884199826,-8.93035898197308,-9.23059905358901,-9.67672169356442,-10.5255587703907,-10.7965499813296,-11.6071981746928,-12.4357780900607,-12.7838705379785,-13.7128577612893,-13.9718600399363,-14.7960991349914,-15.5077869605151,-15.6446778296568,-16.0430514461939,-16.3897486304404,-16.468400160389,-17.2908305803141,-17.9380262183375,-17.9612289364363,-17.8460421688585,-17.7365398088312,-17.6618156877379,-17.5788233374766,-17.3534107398196,-17.2958431942464,-17.5231161434662,-16.8984514299222,-16.0803101538769,-12.8984371883689,-12.9110462378484,-12.5658476701392,-12.1912968888866,-11.7222815894063,-11.2372982723473,-10.9268262671378,-10.9519926896635,-11.2628264298988,-11.2386935360188,-11.3309359676598,-11.7849651017761,-11.9244397925321,-11.6087484676612,-12.1327474911006,-12.2724805640176,-12.6986044246971,-13.2489584286049,-13.2572266577718,-12.1788945451371,-12.3607439103724,-11.9715686987823,-11.7936467424016,-10.7898837215639,-9.60592498132493,-9.16491830814593,-8.52655934004487,-8.40703175215326,-8.23825652428861,-8.34000593035405]}]],[[{"lng":[32.7593754412213,33.7397200000001,33.9408377240965,34.28,34.5599890479994,34.280006137842,34.5599890479994,34.9071513201362,35.267956170398,35.6868453305559,35.7719047381084,35.3390629412316,35.0338102556835,34.3812919451341,34.3072912940921,34.5176660499523,34.4596334164885,34.0648254737786,33.7897001482567,33.2140246925252,32.6881653175231,32.9917643572379,33.3064221534631,33.1142891782019,33.3153104998173,33.4856876970836,33.2313879737753,32.7593754412213],"lat":[-9.23059905358901,-9.41714999999955,-9.69367384198043,-10.1599999999998,-11.5200200334156,-12.2800253231325,-13.5799976538667,-13.5654248999604,-13.8878341610299,-14.6110458309544,-15.8968588192406,-16.1074402808302,-16.8012997372127,-16.1835596655961,-15.4786414527026,-15.0137085913727,-14.6130095353814,-14.3599500464486,-14.4518307430629,-13.9718600399363,-13.7128577612893,-12.7838705379785,-12.4357780900607,-11.6071981746928,-10.7965499813296,-10.5255587703907,-9.67672169356442,-9.23059905358901]}]],[[{"lng":[34.5599890479994,35.312397902169,36.5140816586843,36.7751509946228,37.47129,37.82764,38.4275565935878,39.521,40.31659,40.3165862291108,40.3165885760172,40.478387485523,40.4372530454187,40.5608113950286,40.5996203956798,40.775475294769,40.4772506040126,40.0892639503652,39.4525586280971,38.5383508644215,37.4111328468389,36.2812793312094,35.8964966163641,35.1983996925331,34.78638349787,34.7018925310728,35.1761271502154,35.3734277687057,35.3858482537054,35.5625455363691,35.5339347674043,35.3717741228724,35.6074703305556,35.4587455584196,35.0407348976107,34.2158240089355,33.013210076639,32.5746321957779,32.6603633969501,32.9159550310657,32.8301204770289,32.0716654802811,31.985779249812,31.8377779477281,31.7524084815819,31.9305888201242,31.6703979835347,31.1914091326213,32.244988234188,32.5086930681734,32.6597432797626,32.7727079607526,32.6119942563249,32.6548856951271,32.8498608741644,32.8476387875758,32.3282389666102,31.8520406430406,31.6364982439512,31.1730639991577,30.3389547055345,30.2742558123051,30.1794812354818,33.2140246925252,33.7897001482567,34.0648254737786,34.4596334164885,34.5176660499523,34.3072912940921,34.3812919451341,35.0338102556835,35.3390629412316,35.7719047381084,35.6868453305559,35.267956170398,34.9071513201362,34.5599890479994,34.280006137842,34.5599890479994],"lat":[-11.5200200334156,-11.4391464168795,-11.7209380021671,-11.5945374487813,-11.5687600000001,-11.2687899999998,-11.2852023250816,-10.8968800000001,-10.3170999999999,-10.3170977528174,-10.3170960425258,-10.7654407690899,-11.7617107072449,-12.6391765275609,-14.2019751929318,-14.6917644181943,-15.406294447494,-16.1007740210645,-16.7208912085672,-17.1010230445064,-17.586368096591,-18.6596875952932,-18.8422604305813,-19.5528113745937,-19.7840117326676,-20.497043145431,-21.2543612606687,-21.8408370907486,-22.14,-22.0900000000002,-23.0707878557272,-23.5353589820318,-23.7065630022146,-24.1226099585968,-24.4783505184933,-24.8163143856823,-25.3575733375077,-25.7273182105557,-26.1485844865995,-26.2158672014436,-26.7421916643359,-26.7338200823048,-26.29177988048,-25.8433318010512,-25.4842839494874,-24.3694165992224,-23.6589690080741,-22.2515096981722,-21.1164885393136,-20.3952922502481,-20.3042900529826,-19.7155921363131,-19.4193828264162,-18.6720899390439,-17.9790573055772,-16.7133981258845,-16.3920740698937,-16.319417006091,-16.0719902482778,-15.8609436987981,-15.88083912523,-15.5077869605151,-14.7960991349914,-13.9718600399363,-14.4518307430629,-14.3599500464486,-14.6130095353814,-15.0137085913727,-15.4786414527026,-16.1835596655961,-16.8012997372127,-16.1074402808302,-15.8968588192406,-14.6110458309544,-13.8878341610299,-13.5654248999604,-13.5799976538667,-12.2800253231325,-11.5200200334156]}]],[[{"lng":[32.0716654802811,31.8680603370511,31.2827730649133,30.6859619483745,30.6766085141296,30.9496667823599,31.0440796241572,31.3331575863979,31.8377779477281,31.985779249812,32.0716654802811],"lat":[-26.7338200823048,-27.1779273414213,-27.2858794084787,-26.7438453101693,-26.3980783017048,-26.0226490211041,-25.7314523251396,-25.6601905250085,-25.8433318010512,-26.29177988048,-26.7338200823048]}]],[[{"lng":[12.9955172054652,12.6316117692658,12.4680041846297,12.4366882666609,12.1823368669203,11.9149630062421,12.3186076188739,12.6207597184845,12.9955172054652],"lat":[-4.78110320396197,-4.9912712540926,-5.24836150474518,-5.68430388755946,-5.78993051516391,-5.03798674888455,-4.6062301570859,-4.43802336997619,-4.78110320396197]}],[{"lng":[12.3224316748635,12.7351713395787,13.024869419007,13.3755973649719,16.326528354567,16.5731799658961,16.8601908708452,17.0899959652472,17.4729700049622,18.1342216325691,18.4641756527527,19.0167517432497,19.1666133968961,19.4175024756732,20.0377230160402,20.0916215349206,20.6018229509383,20.5147481625265,21.7281107927397,21.7464559262033,21.949130893652,21.8018013851879,21.8751819190423,22.2087532894864,22.1552681820643,22.4027982927424,22.8373454118847,23.4567908057674,23.9122152035557,24.0178935075926,23.9041536801182,24.0799052263428,23.9309220720454,24.0161365088947,21.9338863461259,21.8878426449539,22.5624784685243,23.2150484555061,21.3771761410456,18.9561869646036,18.2633093604342,14.209706658595,14.058501417709,13.46236209479,12.8140812516884,12.2154614600194,11.7341988460851,11.6400960628816,11.7785372249915,12.1235807634044,12.1756189307223,12.500095249083,12.7384786312454,13.3129138526019,13.6337211442698,13.7387276546869,13.6863794287752,13.3873279151022,13.1209875830698,12.8753695003866,12.9290613135378,13.2364327328099,12.9330403988243,12.7282983740839,12.2273470394465,12.3224316748635],"lat":[-6.10009246177961,-5.96568206138816,-5.98438892987823,-5.86424122479947,-5.87747039146587,-6.62264454511463,-7.22229786543002,-7.54568897871243,-8.068551120642,-7.98767750410492,-7.8470142554066,-7.98824594486014,-7.73818368899972,-7.15542856204412,-7.11636117923104,-6.94309010175717,-6.93931772220009,-7.29960580813844,-7.29087249108115,-7.92008473066765,-8.3059009741584,-8.90870655684256,-9.52370777754828,-9.89479623783669,-11.0848011206539,-10.9930754533357,-11.0176217586746,-10.8678634578923,-10.9268262671378,-11.2372982723473,-11.7222815894063,-12.1912968888866,-12.5658476701392,-12.9110462378484,-12.8984371883689,-16.0803101538769,-16.8984514299222,-17.5231161434662,-17.9306364885197,-17.7890947404723,-17.3099508602621,-17.3531006812256,-17.4233806291425,-16.9712118465892,-16.9413428687242,-17.1116683895583,-17.3018893368245,-16.6731421851294,-15.7938160132512,-14.8783163387678,-14.449143568584,-13.5476998836846,-13.1379057756101,-12.4836304663627,-12.0386447078972,-11.2978630509928,-10.7310759416164,-10.3735783830207,-9.76689706791404,-9.16693368900503,-8.95909107832754,-8.56262948978441,-7.59653858808796,-6.9271220841787,-6.29444752362922,-6.10009246177961]}]],[[{"lng":[30.4696736457612,30.52766,30.74301,30.7522400000001,30.50554,30.11632,29.7535124040999,29.3399975929003,29.2763839047491,29.0249263852168,29.6321761410786,29.9383590024079,30.4696736457612],"lat":[-2.41385475710109,-2.80761999999986,-3.03430999999938,-3.35931000000007,-3.56858000000006,-4.09012000000008,-4.45238941815333,-4.49998341229434,-3.29390715903389,-2.83925790773017,-2.91785776124591,-2.3484868302543,-2.41385475710109]}]],[[{"lng":[49.5435189145958,49.8089807472791,50.0565108579572,50.2174312681141,50.4765368996255,50.377111443896,50.2002746925932,49.8606055031387,49.6726066424609,49.8633443540502,49.7745642433727,49.4986120949341,49.4356185239703,49.0417924334739,48.548540887248,47.9307491391987,47.5477234230513,47.0957613462266,46.2824776548171,45.4095076841105,44.8335738462176,44.0397204933498,43.7637683449112,43.6977775408745,43.3456543312376,43.254187046081,43.4332975604046,43.8936828956929,43.8963700701721,44.3743253924397,44.4643974139244,44.2324219093662,44.0429761085842,43.9630843442609,44.3124687029863,44.4465173683514,44.9449365578065,45.502731967965,45.8729936053363,46.3122432798172,46.8821826515643,47.7051298358124,48.0052148781313,47.8690474790422,48.2938277524814,48.8450602557388,48.863508742067,49.1946513201933,49.5435189145958],"lat":[-12.4698328589409,-12.8952849259995,-13.5557614071217,-14.7587887508766,-15.2265121395507,-15.706069431219,-16.0002633602564,-15.4142526180666,-15.7102035458026,-16.4510368791384,-16.8750420060934,-17.1060356584383,-17.9530640601346,-19.1187810197741,-20.4968881161339,-22.3915011532511,-23.781958916929,-24.9416297339904,-25.1784628231843,-25.6014344214937,-25.3461011695388,-24.988345228782,-24.4606771786495,-23.5741163062508,-22.7769039852835,-22.0574130184838,-21.3364751115805,-21.1633073869703,-20.8304594865783,-20.0723662248568,-19.4354541968591,-18.961994724201,-18.3313872209431,-17.4099447567469,-16.850495700755,-16.2162191708051,-16.1793738745803,-15.9743734676785,-15.7934542782249,-15.7800184058294,-15.2101823869466,-14.5943026668915,-14.0912325985307,-13.6638685034763,-13.7840678849877,-13.0891748999587,-12.4878679338105,-12.0405567358924,-12.4698328589409]}]],[[{"lng":[-16.7137288070235,-15.6245963200399,-15.3987703109245,-15.0817353988138,-14.6870308089685,-14.3767138330558,-14.0469923568175,-13.8449633447724,-14.2777017887846,-14.7121972314946,-15.1411632959495,-15.5118125065629,-15.691000535535,-15.9312959456922,-16.8415246240813,-16.7137288070235],"lat":[13.5949586043797,13.6235873478696,13.8603687606311,13.8764918075055,13.6303569604995,13.6256802433775,13.7940678980003,13.5050416121921,13.2805850285325,13.2982066919441,13.5095116235847,13.278569647673,13.2703530949385,13.1302841252111,13.1513939478027,13.5949586043797]}]],[[{"lng":[9.48213992680528,9.05560265466815,8.43910281742612,8.43047285323338,7.61264163578218,7.52448164229224,8.1409814795343,8.37636762862377,8.21782433435231,8.42096438969168,9.50999352381061,10.2100024756363,10.1806502620945,11.0288672217334,11.1000256689993,10.6000045101431,10.5932865739451,10.9395186703007,10.807847120821,10.1495927262871,10.3396586442566,10.8568363786337,11.1085006038951,11.488787469131,11.4322534522037,10.9447896663945,10.6369014827995,9.95022505050508,10.0565751481617,9.97001712407285,9.48213992680528],"lat":[30.3075560572464,32.1026919622013,32.5062848984005,32.7483373072564,33.3441148951489,34.0973764104519,34.6551459823936,35.4798760035558,36.4331769882604,36.9464273137836,37.3499944117664,37.2300017359847,36.724037787415,37.092103176414,36.8999960393689,36.4100001083773,35.9474443629332,35.6989840764735,34.8335071884491,34.3307730168976,33.7857416855149,33.7687401392913,33.2933428004222,33.1369957545236,32.3689031031528,32.0818146835558,31.7614208033458,31.3760696477452,30.9618313664928,30.5393248560752,30.3075560572464]}]],[[{"lng":[-8.68439978680905,-8.66512447756418,-8.66558956545481,-8.67411617678297,-7.05922766766196,-6.06063229005377,-5.24212927898278,-4.85964616537447,-3.69044104655472,-3.64749793132015,-3.06898027181265,-2.61660478352956,-1.30789913573787,-1.12455115396631,-1.3880492822226,-1.73345455566147,-1.79298580566171,-2.16991370279862,-1.20860287108905,-0.127454392894598,0.503876580415215,1.46691857260655,3.16169884605082,4.81575809084913,5.3201200700178,6.26181969567261,7.33038496260397,7.73707848474101,8.42096438969168,8.21782433435231,8.37636762862377,8.1409814795343,7.52448164229224,7.61264163578218,8.43047285323338,8.43910281742612,9.05560265466815,9.48213992680528,9.80563439295236,9.85999799972345,9.68388471847276,9.75612837081678,9.62905602381108,9.71628584151967,9.31941084151816,9.91069257980178,9.94826134607797,10.3038468766784,10.7713635596229,11.560669386449,11.9995056494716,8.57289310062979,5.67756595218068,4.26741946780004,3.15813317222271,3.14666100425391,2.68358849448643,2.06099083823393,1.82322757325903,-1.55005489745761,-4.92333736817423,-8.68439978680905],"lat":[27.3957441268962,27.5894790715585,27.6564258895926,28.8412889673969,29.5792284205245,29.7316997340017,30.0004430201355,30.5011876490433,30.896951605751,31.6372940129808,31.7244979924732,32.0943462183863,32.2628889023057,32.6515215113578,32.8640150009415,33.919712836232,34.5279186060916,35.1683963079168,35.7148487411871,35.8886624212012,36.301272894835,36.6056470810343,36.7839049342251,36.8650369329235,36.7165188665171,37.1106550156067,37.1183806422342,36.8857075058401,36.9464273137836,36.4331769882604,35.4798760035558,34.6551459823936,34.0973764104519,33.3441148951489,32.7483373072564,32.5062848984005,32.1026919622013,30.3075560572464,29.4246383733235,28.9599897323703,28.1441738957789,27.6882585718846,27.140953477481,26.5122063257858,26.0943248560575,25.3654546167968,24.9369536402324,24.3793132593707,24.5625320500616,24.0979092473254,23.4716684025964,21.5656607121591,19.6012069767994,19.1552652043368,19.0573642033599,19.6935785995213,19.8562301701604,20.1422333846798,20.6108094344862,22.7926659204978,24.9745740829413,27.3957441268962]}]],[[{"lng":[36.4295100000001,36.32322,36.7538900000001,36.8525300000001,37.1674700000001,37.9040000000001,38.4100899594732,38.99062299984,39.266110060388,39.8142936541402,41.1792749366977,41.7349516131324,42.2768306821449,42.5895764503753,43.0812260272002,42.7796423683448,42.3515600000001,42.00975,41.59856,41.1552,40.8966,40.0262500000001,39.3406100000001,39.0994,38.51295,37.9060700000001,37.5937700000001,36.4295100000001],"lat":[14.4221100000004,14.8224899999998,16.2918599999997,16.9565500000006,17.2631400000001,17.4275399999998,17.99830739997,16.8406261255522,15.9227234969672,15.4356472844008,14.4910796167526,13.9210368921417,13.3439920109544,13.0004212508619,12.6996385767068,12.4554157576958,12.5422299999999,12.8658199999999,13.45209,13.7733299999999,14.1186399999999,14.5195899999999,14.5315500000002,14.7406400000002,14.50547,14.9594300000003,14.2131000000002,14.4221100000004]}]],[[{"lng":[-2.16991370279862,-1.79298580566171,-1.73345455566147,-1.3880492822226,-1.12455115396631,-1.30789913573787,-2.61660478352956,-3.06898027181265,-3.64749793132015,-3.69044104655472,-4.85964616537447,-5.24212927898278,-6.06063229005377,-7.05922766766196,-8.67411617678297,-8.66558956545481,-8.81782833498668,-8.79488399904907,-9.41303748212448,-9.73534339032888,-10.1894242008776,-10.5512625797853,-11.392554897497,-11.7182197738003,-12.0307588363016,-12.5009626937254,-13.8911103988091,-14.2211677718573,-14.6308326888511,-14.7509545557135,-17.0029617985611,-17.0204284326758,-16.9732478499932,-16.5891369287677,-16.2619217594956,-16.3264139469959,-15.9826106429581,-15.4260037907422,-15.0893318343607,-14.8246451481616,-14.8009256657397,-14.4399399479648,-13.7738048975065,-13.1399417790143,-13.1216133699148,-12.6188366357831,-11.6889192366908,-10.9009569971044,-10.3995922510086,-9.56481116376568,-9.81471839032917,-9.43479326011935,-9.30069291832189,-8.65747636558501,-7.65417843263821,-6.91254411460142,-6.24434200685142,-5.92999426921989,-5.19386349122203,-4.59100623210514,-3.64005652507007,-2.60430579264409,-2.16991370279862],"lat":[35.1683963079168,34.5279186060916,33.919712836232,32.8640150009415,32.6515215113578,32.2628889023057,32.0943462183863,31.7244979924732,31.6372940129808,30.896951605751,30.5011876490433,30.0004430201355,29.7316997340017,29.5792284205245,28.8412889673969,27.6564258895926,27.6564258895926,27.1206963160227,27.0884760604885,26.8609447291077,26.860944729108,26.990807603457,26.8834239771544,26.1040917017608,26.0308661972029,24.7701162785785,23.691009019459,22.3101630721881,21.8609398462749,21.5006000839032,21.4207341577966,21.4223102889818,21.8857445337752,22.1582343612497,22.6793395044817,23.0177684595608,23.7233584660738,24.3591336125611,24.5202607284472,25.1035326197255,25.6362649602221,26.2544184432972,26.6188923202522,27.6401478134204,27.6541476717193,28.0381855331489,28.148643907172,28.8321422388804,29.0985859237779,29.9335737167503,31.177735500609,32.0380964218362,32.5646792668912,33.2402452662425,33.6970649277026,34.1104763860375,35.1458653834375,35.7599881047945,35.7551821965916,35.3307119817456,35.3998550481521,35.1790933294014,35.1683963079168]}]],[[{"lng":[36.86623,32.9,29.02,25,25,25,24.70007,24.95762,24.80287,25.16482,26.49533,27.45762,28.45048,28.91353,29.68342,30.09503,30.97693,31.68796,31.96041,32.19247,32.99392,33.7734,34.2654347446462,34.26544,34.8232432887838,34.9226,34.64174,34.42655,34.15451,33.92136,33.58811,33.13676,32.42323,32.32046,32.73482,33.34876,34.10455,34.47387,34.79507,35.69241,35.49372,35.52598,36.69069,36.86623],"lat":[21.9999999999997,21.9999999999997,21.9999999999995,21.9999999999997,25.6824999963612,29.2386545295335,30.0441900000002,30.6615999999997,31.0892900000003,31.5691499999998,31.5856800000004,31.3212599999994,31.0257699999995,30.87005,31.18686,31.4733999999999,31.5558599999999,31.4296,30.9336000000001,31.26034,31.0240699999996,30.9674599999998,31.2193573095204,31.2193600000002,29.7610807617183,29.5013299999998,29.0994199999999,28.3439899999999,27.8233,27.6486999999997,27.9713599999999,28.4176500000003,29.8510799999998,29.7604299999996,28.7052300000003,27.6998899999997,26.14227,25.5985600000007,25.03375,23.9267100000002,23.75237,23.1024400000005,22.2048499999997,21.9999999999997]}]],[[{"lng":[25,25.0000000000001,23.8500000000001,23.8376600000001,19.8492600000001,15.8608500000001,14.8513,14.1438708838552,13.5814245947905,11.9995056494716,11.560669386449,10.7713635596229,10.3038468766784,9.94826134607797,9.91069257980178,9.31941084151816,9.71628584151967,9.62905602381108,9.75612837081678,9.68388471847276,9.85999799972345,9.80563439295236,9.48213992680528,9.97001712407285,10.0565751481617,9.95022505050508,10.6369014827995,10.9447896663945,11.4322534522037,11.488787469131,12.66331,13.0832600000001,13.91868,15.24563,15.7139399999999,16.61162,18.02109,19.08641,19.5740400000001,20.05335,19.8203300000001,20.1339699999999,20.8545200000001,21.54298,22.8957600000001,23.2368,23.6091300000001,23.9275,24.9211399999999,25.16482,24.80287,24.95762,24.70007,25,25,25],"lat":[21.9999999999997,20.0030400000001,20.0000000000002,19.5804699999999,21.4950900000001,23.4097199999996,22.8629499999999,22.4912889673715,23.0405060897689,23.4716684025964,24.0979092473254,24.5625320500616,24.3793132593707,24.9369536402324,25.3654546167968,26.0943248560575,26.5122063257858,27.140953477481,27.6882585718846,28.1441738957789,28.9599897323703,29.4246383733235,30.3075560572464,30.5393248560752,30.9618313664928,31.3760696477452,31.7614208033458,32.0818146835558,32.3689031031528,33.1369957545236,32.7927799999998,32.8788200000003,32.7119600000002,32.26508,31.3762599999997,31.1821800000001,30.76357,30.2663900000001,30.5258199999996,30.9857599999993,31.7517900000002,32.2382000000003,32.7068,32.8431999999995,32.6385799999998,32.19149,32.1872600000003,32.0166700000007,31.89936,31.5691499999998,31.0892900000003,30.6615999999997,30.0441900000002,29.2386545295335,25.6824999963612,21.9999999999997]}]],[[{"lng":[47.78942,44.9636,43.66087,42.76967,42.12861,41.855083092644,41.1718,40.76848,39.85494,39.5593842587659,38.89251,38.67114,38.43697,38.120915,36.8550932380081,36.1590786328556,35.8174476623535,35.8174476623535,35.298007118233,34.70702,34.25032,34.0751,33.5682900000001,32.9541800000001,33.2948000000001,33.8255000000001,33.9749800000001,33.9616200000001,34.2574500000001,34.7311500000001,34.8316300000001,35.2604900000001,35.86363,36.2702200000001,36.4295100000001,37.5937700000001,37.9060700000001,38.51295,39.0994,39.3406100000001,40.0262500000001,40.8966,41.1552,41.59856,42.00975,42.3515600000001,42.0000000000001,41.6617600000001,41.7395900000002,41.7555700000002,42.3141400000001,42.5549300000001,42.776851841001,42.5587599999999,42.9281200000001,43.2969900000001,43.67875,46.9483400000001,47.78942],"lat":[8.00300000000016,5.00162000000004,4.95754999999995,4.25259000000017,4.23412999999991,3.91891192048376,3.91909000000011,4.25701999999984,3.83879000000044,3.42206000000053,3.50073999999981,3.61606999999994,3.58850999999996,3.59860500000003,4.4478641276728,4.4478641276728,4.77696566346184,5.33823208279071,5.5060000000001,6.59422,6.82607000000012,7.22595000000006,7.71333999999999,7.78497000000021,8.35458000000017,8.37916000000014,8.68456000000007,9.58358000000019,10.6300899999998,10.9101699999998,11.3189599999999,12.0828600000006,12.5782799999996,13.5633300000002,14.4221100000004,14.2131000000002,14.9594300000003,14.50547,14.7406400000002,14.5315500000002,14.5195899999999,14.1186399999999,13.7733299999999,13.45209,12.8658199999999,12.5422299999999,12.1000000000003,11.6311999999999,11.3551100000002,11.05091,11.0342000000002,11.1051100000002,10.9268785669341,10.5725800000003,10.0219400000006,9.54048000000011,9.18357999999981,7.99687999999993,8.00300000000016]}]],[[{"lng":[42.3515600000001,42.7796423683448,43.0812260272002,43.3178524106647,43.2863814633989,42.7158736508965,43.1453048032421,42.776851841001,42.5549300000001,42.3141400000001,41.7555700000002,41.7395900000002,41.6617600000001,42.0000000000001,42.3515600000001],"lat":[12.5422299999999,12.4554157576958,12.6996385767068,12.3901484237111,11.9749282902458,11.7356405705188,11.4620396997489,10.9268785669341,11.1051100000002,11.0342000000002,11.05091,11.3551100000002,11.6311999999999,12.1000000000003,12.5422299999999]}]],[[{"lng":[33.9037111971045,31.8661700000001,30.7698600000001,30.4191048520192,29.821518588996,29.5794661801409,29.5878377621722,29.8195032081366,29.8757788429024,30.0861535987627,30.4685075212903,30.8526701189481,31.1741492042358,30.77334679538,30.8338598975938,30.8338524217154,31.2455600000001,31.88145,32.6864200000001,33.3900000000001,34.005,34.47913,34.59607,35.03599,34.6721,34.18,33.8935689696669,33.9037111971045],"lat":[-0.94999999999952,-1.02736000000001,-1.01455000000005,-1.1346591121508,-1.44332244222969,-1.34131316488571,-0.587405694179271,-0.205310153813445,0.597379868976472,1.06231273030602,1.58380544677967,1.84939647054376,2.20446523682122,2.33988332764186,3.50916596111063,3.50917160422244,3.78190000000014,3.55826999999995,3.79231999999979,3.78999999999963,4.24988494736234,3.55560000000022,3.05374000000034,1.90583999999997,1.17694,0.515000000000006,0.109813537861806,-0.94999999999952]}]],[[{"lng":[30.4191048520192,30.8161348813177,30.7583089535831,30.46967,30.4696736457612,29.9383590024079,29.6321761410786,29.0249263852168,29.1174788754516,29.2548348324833,29.2918868344366,29.5794661801409,29.821518588996,30.4191048520192],"lat":[-1.1346591121508,-1.69891407634556,-2.28725025798838,-2.41383,-2.41385475710109,-2.3484868302543,-2.91785776124591,-2.83925790773017,-2.29221119548857,-2.21510995850913,-1.62005584066834,-1.34131316488571,-1.44332244222969,-1.1346591121508]}]],[[{"lng":[30.8338524217154,29.9535001970695,29.715995314256,29.1590784034465,28.6966776872988,28.4289937680269,27.9799772478428,27.3742261085175,27.2134090512252,26.4659094581232,26.2134184099451,25.7966479835112,25.1241308936647,25.1149324887168,24.5673690121521,23.8869795808607,24.1940677211877,24.537415163602,24.7949257454127,25.069603699344,25.7906333284139,25.962307049621,26.4773282132425,26.7520061671738,27.1125209817089,27.8335506107788,27.9708895877444,28.9665971707458,29.0009319149872,29.5159530786086,29.6189573113328,29.9966394979886,30.8378407319034,31.3528618955249,31.8507156870255,32.4000715948883,32.3142347342848,32.0738915245948,32.6747495488196,32.7434190373025,33.2069380845618,33.0867664797167,33.2069380845618,33.7219592481831,33.8421308530282,33.8249634809075,33.9633927949712,33.9749800000001,33.8255000000001,33.2948000000001,32.9541800000001,33.5682900000001,34.0751,34.25032,34.70702,35.298007118233,34.6201962678539,34.005,33.3900000000001,32.6864200000001,31.88145,31.2455600000001,30.8338524217154],"lat":[3.50917160422244,4.17369904216787,4.60080475506008,4.38926727947331,4.4550772159971,4.28715464926459,4.4084133976378,5.23394440350009,5.55095347739434,5.94671743410217,6.54660329836178,6.97931590415836,7.50008515057971,7.82510407147925,8.2291879337856,8.61972971293243,8.72869647240424,8.91753756573175,9.81024091600904,10.2737599632678,10.4110989402338,10.1364209863024,9.55273033419781,9.46689347359502,9.63856719480163,9.60423245056017,9.39822398511191,9.39822398511191,9.60423245056005,9.79307354388752,10.0849188699402,10.2909273353888,9.70723668328428,9.81024091600827,10.5312705450785,11.0806264529416,11.681484477166,11.9733298032187,12.0248319195808,12.2480077571497,12.1793382686664,11.4411412674762,10.7201116384068,10.3252620796304,9.98191463721596,9.48406084571541,9.46428522942106,8.68456000000007,8.37916000000014,8.35458000000017,7.78497000000021,7.71333999999999,7.22595000000006,6.82607000000012,6.59422,5.5060000000001,4.84712274208211,4.24988494736234,3.78999999999963,3.79231999999979,3.55826999999995,3.78190000000014,3.50917160422244]}]]],["Tanzania","Western.Sahara","Democratic.Republic.of.the.Congo","Somalia","Kenya","Sudan","Chad","South.Africa","Lesotho","Zimbabwe","Botswana","Namibia","Senegal","Mali","Mauritania","Benin","Niger","Nigeria","Cameroon","Togo","Ghana","Cote.d.Ivoire","Guinea","Guinea.Bissau","Liberia","Sierra.Leone","Burkina.Faso","Central.African.Republic","Republic.of.Congo","Gabon","Equatorial.Guinea","Zambia","Malawi","Mozambique","Swaziland","Angola","Burundi","Madagascar","The.Gambia","Tunisia","Algeria","Eritrea","Morocco","Egypt","Libya","Ethiopia","Djibouti","Uganda","Rwanda","South.Sudan"],"africa",{"interactive":true,"className":"","pane":"tmap401","stroke":false,"color":"#FFFFFF","weight":1,"opacity":0,"fill":true,"fillColor":["#E8F6F9","#7ACAB2","#BFE7DF","#E8F6F9","#E8F6F9","#7ACAB2","#BFE7DF","#42AE77","#42AE77","#E8F6F9","#42AE77","#42AE77","#147E3A","#147E3A","#147E3A","#147E3A","#147E3A","#147E3A","#BFE7DF","#147E3A","#147E3A","#147E3A","#147E3A","#147E3A","#147E3A","#147E3A","#147E3A","#BFE7DF","#BFE7DF","#BFE7DF","#BFE7DF","#E8F6F9","#E8F6F9","#E8F6F9","#42AE77","#BFE7DF","#E8F6F9","#E8F6F9","#147E3A","#7ACAB2","#7ACAB2","#E8F6F9","#7ACAB2","#7ACAB2","#7ACAB2","#E8F6F9","#E8F6F9","#E8F6F9","#E8F6F9","#E8F6F9"],"fillOpacity":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"dashArray":"none","smoothFactor":1,"noClip":false},["<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Tanzania<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Western Sahara<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Democratic Republic of the Congo<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Somalia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Kenya<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Sudan<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Chad<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>South Africa<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Southern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Lesotho<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Southern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Zimbabwe<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Botswana<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Southern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Namibia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Southern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Senegal<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Mali<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Mauritania<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Benin<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Niger<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Nigeria<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Cameroon<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Togo<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Ghana<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Cote d'Ivoire<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Guinea<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Guinea-Bissau<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Liberia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Sierra Leone<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Burkina Faso<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Central African Republic<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Republic of Congo<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Gabon<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Equatorial Guinea<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Zambia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Malawi<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Mozambique<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Swaziland<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Southern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Angola<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Middle Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Burundi<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Madagascar<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>The Gambia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Western Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Tunisia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Algeria<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Eritrea<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Morocco<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Egypt<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Libya<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Northern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Ethiopia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Djibouti<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Uganda<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Rwanda<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>South Sudan<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>subregion<\/nobr><\/td><td align=\"right\"><nobr>Eastern Africa<\/nobr><\/td><\/tr><\/table><\/div>"],null,["Tanzania","Western Sahara","Democratic Republic of the Congo","Somalia","Kenya","Sudan","Chad","South Africa","Lesotho","Zimbabwe","Botswana","Namibia","Senegal","Mali","Mauritania","Benin","Niger","Nigeria","Cameroon","Togo","Ghana","Cote d'Ivoire","Guinea","Guinea-Bissau","Liberia","Sierra Leone","Burkina Faso","Central African Republic","Republic of Congo","Gabon","Equatorial Guinea","Zambia","Malawi","Mozambique","Swaziland","Angola","Burundi","Madagascar","The Gambia","Tunisia","Algeria","Eritrea","Morocco","Egypt","Libya","Ethiopia","Djibouti","Uganda","Rwanda","South Sudan"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#E8F6F9","#BFE7DF","#7ACAB2","#42AE77","#147E3A"],"labels":["Eastern Africa","Middle Africa","Northern Africa","Southern Africa","Western Africa"],"na_color":null,"na_label":"NA","opacity":1,"position":"topright","type":"unknown","title":"subregion","extra":null,"layerId":"legend401","className":"info legend","group":"africa"}]},{"method":"addLayersControl","args":[["Esri.WorldGrayCanvas","OpenStreetMap","Esri.WorldTopoMap"],"africa",{"collapsed":true,"autoZIndex":true,"position":"topleft"}]}],"limits":{"lat":[-34.8191663551235,37.3499944117664],"lng":[-17.6250426904907,51.13387]},"fitBounds":[-34.8191663551235,-17.6250426904907,37.3499944117664,51.13387,[]]},"evals":[],"jsHooks":{"render":[{"code":"function(el, x, data) {\n  return (\n\t\t\t\t\t\tfunction(el, x) {\n\t\t\t\t\t\t\tvar tldiv = document.getElementsByClassName(\"leaflet-top leaflet-left\")[0];\n\t\t\t\t\t\t\tvar titlediv = document.createElement('div');\n\t\t\t\t\t\t\ttitlediv.className = \"info legend leaflet-control\";\n\t\t\t\t\t\t\ttitlediv.innerHTML = \"<b>Africa por subregiones<\/b>\";\n\t\t\t\t\t\t\ttldiv.insertBefore(titlediv, tldiv.childNodes[0]);\n\t\t\t\t\t\t}).call(this.getMap(), el, x, data);\n}","data":null}]}}</script><!--/html_preserve-->
Con mapview


```r
# mapview::mapview(africa)
```







