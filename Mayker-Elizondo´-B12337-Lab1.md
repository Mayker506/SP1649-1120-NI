---
title: "Mayker Elizondo-b12337-Lab1"
output:
  html_document:
    self_contained: no
    keep_md: yes

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
library(rmarkdown)
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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
 
 
 **5-Antes de graficar, canalice el conjunto de datos st_set_crs(4326). A que se debe lo diferente en la trama obtenida?**


```r
d=st_zm(storms_xyz_feature)

   d %>% 
  st_set_crs(4326) %>%
  plot(graticule = TRUE, axes = TRUE)
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

con el comando st_zm lo que se hizo fue eliminar la variable z y con st_set_crs(4326) se hace proyecta y se observa  la altitud y latitud*



# Ejercicio capitulo 2 ----------------------------------------------------


**convertir el (x,y) puntos (10,2), (-10,-2),(10,-2) y (0,10) a coordenadas polares (10,2)**   

```r
x=10
y=2

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi)

cor <- cbind(r1,a);cor
```

```
##            r1        a
## [1,] 10.19804 11.30993
```
(-10,-2)       

```r
x=-10
y=-2

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi) 
cor <- cbind(r1,a);cor
```

```
##            r1        a
## [1,] 10.19804 11.30993
```


(10,-2) 

```r
x=10
y=-2

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi)  
cor <- cbind(r1,a);cor
```

```
##            r1         a
## [1,] 10.19804 -11.30993
```
(0,10) 

```r
x=0
y=10

r1=sqrt((x^2)+(y^2) )
a=atan((y/x))
a=((a*180)/pi)
cor <- cbind(r1,a);cor
```

```
##      r1  a
## [1,] 10 90
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
corde <- cbind(x,y);corde
```

```
##             x        y
## [1,] 7.071068 7.071068
```


(0,100)


```r
r=0
a=100

ar=(a*pi)/180

x=r*cos(ar)
y=r*sin(ar)
corde <- cbind(x,y);corde
```

```
##      x y
## [1,] 0 0
```

(5,359)


```r
r=5
a=359

ar=(a*pi)/180

x=r*cos(ar)
y=r*sin(ar)
corde <- cbind(x,y);corde 
```

```
##             x           y
## [1,] 4.999238 -0.08726203
```

**•	suponiendo que la Tierra es una esfera con un radio de 6371 km, calcule para (λ,ϕ)(λ,ϕ) señala la distancia del gran círculo entre (10,10)(10,10) y (11,10)(11,10), Entre (10,80)(10,80) y (11,80)(11,80), Entre (10,10)(10,10) y (10,11)(10,11) y entre (10,80)(10,80) y (10,81)(10,81)(unidades: grado). ¿Cuáles son las unidades de distancia?**

la distancia es está en milles de radianes

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
d=(a12*r)/1000
d
```

```
## [1] 0.01930856
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
d=(a12*r)/1000
d
```

```
## [1] 0.1111949
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
d=(a12*r)/1000
d
```

```
## [1] 0.1111949
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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

Toma mucho tiempo porque los datos de tif/L7_ETMs.tif son tipo raster y al aplicarle una transformación le estamos cambiando a otro sistema de coordenadas lo que genera perdida de datos.


-tilice st_warppara deformar el L7_ETMs.tif a un objeto a EPSG 4326 y graficar el objeto resultante con axes=TRUE. ¿Por que el grafico se crea mucho mas rapido que con st_transform?




```r
new = st_crs(4326)
y2 = st_warp(x, crs = new)

plot(y2, axes=TRUE, border=NA)
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

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




Algunos otros mapas paquetes que se puede usar son:

ggplot2:2	
googleway

tmap:	Thematic Maps

rasterVis:Visualization Methods for Raster Data


```r
# Uso de plot
#plot(africa)
plot(africa[4])
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

```r
#plot(st_geometry(africa) , col = africa$HDI)
```
Uso de tm_shape

```r
tm_shape(africa)+ tm_fill(col = "HDI")
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-36-1.png)<!-- -->
**Represente africa en las subregiones en el mapa. Cambie la paleta de colores predeterminada y el título de la leyenda. A continuación, combine este mapa y el mapa creado en el ejercicio anterior en un solo gráfico.**



```r
map2=tm_shape(africa)+ tm_fill(col = "subregion", palette = "BuGn") + tm_layout(title = "Africa por subregiones")
map2                   
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

```r
tmap_arrange(map1, map2)
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-38-1.png)<!-- -->
#4-Crea un mapa de cobertura terrestre del Parque Nacional Zion.


```r
plot(zion)
```

```
## Warning: plotting the first 10 out of 11 attributes; use max.plot = 11 to plot
## all
```

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

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

![](Mayker-Elizondo´-B12337-Lab1_files/figure-html/unnamed-chunk-40-1.png)<!-- -->



**Crea un mapa interactivo de África:**
**•	Con tmap**
**•	Con mapview**
**•	Con folleto**
**•	Bonificación: para cada enfoque, agregue una leyenda (si no se proporciona automáticamente) y una barra de escala**

Con tmap

```r
map0=tm_shape(africa)+ tm_fill(col = "subregion")

tmap_mode("view")
map0
```
Con mapview


```r
mapview::mapview(africa)
```




con leaflet

```r
pal = colorNumeric("RdYlBu", domain = africa$HDI)

leaflet(data = africa)%>% 
   addProviderTiles(providers$CartoDB.Positron)%>%
   addPolygons(data = lnd, fill = FALSE) %>% 
   addLegend(pal = pal, values = ~HDI) %>% 
   setView(lng = -0.1, 51.5, zoom = 12) %>% 
   addMiniMap()
```

