#El siguiente script está diseñado para la tesis: Importancia de las áreas verdes de la ciudad de puebla para la conservación de la  avifauna.
#Viene agrupado en dos archivos el archivo ave1
#y el archivo ave2, el primero una lista diseñada como una libreta de campo, el segundo una matriz más estilizada
#listado1 con el archivo ave1
#read.csv("/home/juan/Documentos/Fer/datos tesis/listadouno.csv",header=TRUE)->ave1
#read.table("/home/juan/Documentos/Fer/datos tesis/listadodos1.csv",header=TRUE,sep=",")->ave2
read.csv("~/Documentos/Fer/datos tesis/listadouno.csv",header=TRUE)->ave1
read.table("~/Documentos/Fer/datos tesis/listadodos1.csv",header=TRUE,sep=",")->ave2
attach(ave1)
names(ave1)
levels(especie)
attach(ave2)
names(ave2)
ave3<-ave2[,5:81]
taves<-colSums(ave3)

#llamamos las librerías necesarias en este caso vegan, si no las tenemos las instalamos con el install.packages(paquete,dep=TRUE)
library(vegan)
library(BiodiversityR)
library(FD) #opcional
library(vegetarian)
library(rareNMtests) #opcional
library(entropart)
library(iNEXT)
library(ggplot2)
#Estos comandos son exploratorios, no conviene hacer mucho caso
subset(especie,sitio=="Paseo.bravo")
especie[sitio=="Parque.atoyac"]
#análisis serios jeje
#aquí se va a calcular la diversidad verdadera por visita es decir los 9 parques por las 4 visitas en total 72 índices de diversidad
#diversidad 
#parques total y calles total
#primero la formula de la diversidad verdadera
Hverd<- function(x){
  x<-x[x>0]
  P<-x/sum(x)
  -sum(P*log(P))->la
  exp(la)->la2
  print(la2)
}
Hverd2<- function(x){
  x<-x[x>0]
  P<-x/sum(x)
  (P^2)->P1
sum(P1)->la
  1-la->la2
1/la2->la3
  print(la3)
}

#después unos pequeños comandos exploratorios más
tapply(conteo.total,list(tipo), Hverd)->conteo1;conteo1
#para cada parque y por muestreo
#la sumatoria de las aves en abundancia por visita y por sitio
tapply(conteo.total,list(sitio,visita), sum)->conteo;conteo
##Ahora la  diversidad verdadera
tapply(conteo.total,list(sitio,visita), Hverd)->conteo2;conteo2
tapply(conteo.total,list(sitio,visita,especie), sum)->conteoesp
#convertir los na a ceros
conteoesp[is.na(conteoesp)] <- 0
conteoesp
tapply(conteo.total,list(sitio), Hverd)->conteo4;conteo4
tapply(conteo.total,list(sitio), sum)->conteon;conteon
c(conteon)
d(conteon1,lev="alpha",q=1)
#abundancias por gremios
#quitasmos las columnas que no están con especie
subset(ave1,  ! especie %in% c("COLIBRI", "Icterus", "Empidonax","NOIDEN","VERMIVORA","VIREO","RAPAZ") )->newav1
levels(newav1$especie)
newav1$especie = factor(newav1$especie)
levels(newav1$especie)
attach(newav1)
names(newav1)
levels(newav1$especie)
tapply(conteo.total,list(Gremio,tipo), sum)->conteog;conteog
sum(conteo.total)
#una pequeña grafiquilla
x11()
boxplot(conteo2,xlab="visitas",ylab="riqueza (exp shannon")
data.frame(conteo2)
c(conteo2)->conteo3
shapiro.test(conteo3)
#sitio para los ancovas
#como no quiero hacer una base de datos extra para las áreas entonces
#delimitaremos una en este script haciendo repeticionesa
#primero una matriz para poder ver la áreas
area<-matrix(c(44.4,29.8,34.5,76.8,54.2,45.3,86.1,73.7,85.5),9,1)
colnames(area)<-c("porcentaje")
rownames(area)<-c("Chapulco","cu","Arte","Paseo bravo","Los Fuertes","Ecológico","Panteón","Atoyac","Juarez")
area
callesarea<-c(16.11,4.76,5.54,6.21,6.89,7.38,9.774,7.58,4.02)
#creamos las 72 repeticiones 
area2<-rep(c(16.1,4.8,5.5,6.21,6.9,7.38,9.77,7.58,4,44.4,29.8,54.2,85.5,86.1,34.5,73.7,45.3,76.8),each=4)
sum(area2)
area2
tipos<-rep(c("calles","parques"),each=36)
as.factor(tipos)->tipos1
data.frame(conteo3,area2,tipos)
data.frame(conteo3,area2,tipos)
ancova1<-glm(conteo3~area2*tipos1, poisson);ancova1
summary(ancova1)
anova(ancova1,test="Chisq")
#y nada es significativo
boxplot(conteo3~area2)
plot(conteo3~area2)

#un anova normal
aov(conteo3~tipos1*area2)->anova1
summary(anova1)
#y nada me lleva la que me trajo
#es hora de hacer el segundo ancova
distancias<-matrix(c(3855,3681,5428,3923,1887,664,4400,5506,3370),9,1)
colnames(distancias)<-c("distancia")
rownames(distancias)<-c("Chapulco","cu","Arte","Paseo bravo","Los Fuertes","Ecológico","Panteón","Atoyac","Juarez")
distancias
rep(c(3855,3681,5428,3923,1887,664,4400,5506,3370,3855,3681,5428,3923,1887,664,4400,5506,3370),each=4)->disti1
disti1
tipos<-rep(c("calles","parques"),each=36)
as.factor(tipos)->tipos1
data.frame(conteo3,disti1,tipos)
ancova2<-glm(conteo3~disti1*tipos1, poisson);ancova2
summary(ancova2)
anova(ancova2,test="Chisq")
boxplot(conteo3~disti1)
boxplot(conteo3~disti1+tipos)


#nuevos modelos lineales pero ahora con los nueve datos
area3<-rep(c(16.1,4.8,5.5,6.21,6.9,7.38,9.77,7.58,4,44.4,29.8,54.2,85.5,86.1,34.5,73.7,45.3,76.8),each=1)
sum(area3)
area3
tipos1<-rep(c("calles","parques"),each=9)
as.factor(tipos1)->tipos2
data.frame(conteo5,area3,tipos2)
ancova3<-glm(conteo5~area3*tipos2, poisson);ancova3
summary(ancova3)
anova(ancova3,test="Chisq")

#y nada me lleva la que me trajo
#pollos por especie pero también está calculado para otros sitios
tapply(conteo.total,list(especie), sum)->conteo3;conteo3
#este comando servirá para contar las aves por sitio usando el listado1
tapply(conteo.total,list(sitio,especie),sum)->conteo4;conteo4
tapply(conteo.total,list(sitio,especie, visita),sum)->conteo5;conteo5
tapply(especie, sort(tipo),levels)
data.frame(sort(tipo), sort(especie))->tabla1
table(especie,tipo)->tabla2

levels(tabla1)
as.numeric(especie)->especie1
plot(conteo4)
plot(conteo2)
points(conteo1[1:9,1],pch=12)
points(conteo1[10:18,1],pch=16)
plot(conteo1[10:18,1],pch=16)
data.frame(especie,visita)
ave[-which(ave1$visita=="1" & ave1$especie)]
tapply(especie,list(visita),length)
tapply(conteo.total,list(visita,especie),sum)
tapply(especie(sitio,tipo,visita))
ave1 [-which(ave1$sitio=="Panteon" & ave1$tipo=="par" & ave1$especie=="COIN"), ]
ave1$sitio=="Panteon"
sitio
subset(sitio=="Panteon",tipo=="par", especie)
ave1$sitio
list(subset(especie,sitio=="atoyac" & tipo=="par" ))
levels(sitio$especie)
tapply(sitio,especie)
sitio

#####modelos lineales generalizados con el listado dos
ave2
names(ave2)
ave2[,-82:-88]->ave2m;ave2m
names(ave2m)
edit(ave2m)
names(ave3)
options(width=300)
#función poderosa alternativa a tapply
by(ave2m[,5:81],ave2m$Sitio,FUN=data.frame)->abu;abu
by(ave2m[,5:81],list(ave2m$Sitio,ave2m$Tipo),FUN=Hverd)->verdl2
by(ave2m[,5:81],list(ave2m$Sitio,ave2m$Tipo,ave2m$Visita),FUN=Hverd)
by(ave2m[,5:81],list(ave2m$Sitio,ave2m$Tipo),FUN=colSums)->sumaave;sumaave
by(ave2m[,5:81],list(ave2m$Sitio,ave2m$Tipo),FUN=Hverd2)->verdls2;verdls2

unlist(sumaave)->sumaave2;sumaave2
#paste(sumaave2)->sumaave2
matrix(sumaave2,77,18)->matrizsuma
t(matrizsuma)->matrizsuma
colnames(matrizsuma)<-names(ave3)
rownames(matrizsuma)<-c("artc","atoc","chac","cuc","ecoc","fuec","juac","pabc","panc","artp","atocp","chap","cup","ecop","fuep","juap","pabp","panp")
#edit(matrizsuma)
data.frame(matrizsuma)->matrizsuma1
c(Hverd(matrizsuma1[1,]),Hverd(matrizsuma1[2,]),Hverd(matrizsuma1[3,]),Hverd(matrizsuma1[4,]),Hverd(matrizsuma1[5,]),Hverd(matrizsuma1[6,]),Hverd(matrizsuma1[7,]),Hverd(matrizsuma1[8,]),Hverd(matrizsuma1[9,]),Hverd(matrizsuma1[10,]),Hverd(matrizsuma1[11,]),Hverd(matrizsuma1[12,]),Hverd(matrizsuma1[13,]),Hverd(matrizsuma1[14,]),Hverd(matrizsuma1[15,]),Hverd(matrizsuma1[16,]),Hverd(matrizsuma1[17,]),Hverd(matrizsuma1[18,]))->verdadera1
#write.table(matrizsuma1,"/home/fertimjim/Documentos/Fer/datos tesis/abunporpar.csv",sep=",")
c(verdl2)->verdl3
shapiro.test(verdadera1)
area3<-rep(c(16.1,4.8,5.5,6.21,6.9,7.38,9.77,7.58,4,44.4,29.8,54.2,85.5,86.1,34.5,73.7,45.3,76.8),each=1)
sum(area3)
area3
tipos1<-rep(c("calles","parques"),each=9)
as.factor(tipos1)->tipos2
data.frame(verdadera1,area3,tipos2)
anova(glm(verdl3~area3*tipos2,poisson),test="Chisq")
ancova3<-glm(verdadera1~area3*tipos2, poisson);ancova3
summary(ancova3)
anova(ancova3,test="Chisq")

distancias<-rep(c(3855,3681,5428,3923,1887,664,4400,5506,3370),2)
tipos<-rep(c("calles","parques"),each=9)
as.factor(tipos)->tipos1
data.frame(verdadera1,tipos,distancias)
ancova2<-glm(verdadera1~distancias*tipos, poisson);ancova2
summary(ancova2)
anova(ancova2,test="Chisq")

#Ancovas no generalizados
area3<-rep(c(16.1,4.8,5.5,6.21,6.9,7.38,9.77,7.58,4,44.4,29.8,54.2,85.5,86.1,34.5,73.7,45.3,76.8),each=1)
sum(area3)
area3
tipos1<-rep(c("calles","parques"),each=9)
as.factor(tipos1)->tipos2
data.frame(verdadera1,area3,tipos2)
ancova4<-lm(verdadera1~area3*tipos2);ancova4
ancova4<-lm(verdadera1~tipos2*area3);ancova4

anova(lm(verdadera1~1))

summary(ancova4)
anova(ancova4)

distancias<-rep(c(3855,3681,5428,3923,1887,664,4400,5506,3370),2)
tipos<-rep(c("calles","parques"),each=9)
as.factor(tipos)->tipos1
data.frame(verdadera1,tipos,distancias)
ancova5<-lm(verdadera1~tipos*distancias);ancova5
summary(ancova5)
anova(ancova5,test="Chisq")


#paso a paso y a manopla
names(ave2)
subset(ave2,Sitio=="pan"&Tipo=="cal")->pac
colSums(pac[,c(-1:-4,-82:-88)])->pac2
as.numeric(pac2)
matrix(as.numeric(pac2),1,77)->mapac
Hverd(mapac)
#ignoremos lo de a manopla si se pudo antes
#listado2
read.table("/home/fertimjim/Documentos/Fer/datos tesis/listadodos.csv",header=TRUE)->ave2
attach(ave2)
names(ave2)
ave3<-ave2[,5:81]
sp1<-specaccum(ave3)
sp2<-specaccum(ave3,"random")
plot(sp2,col=2,lwd=2)
methods(plot)
summary(sp2)
colSums(ave3)
#aves por temporada y número de individuos por temporada
subset(ave2,Visita==1)->contma
subset(ave2,Visita==2)->contmb
contma$Visita
subset(ave2,Visita==3)->contra
subset(ave2,Visita==4)->contrb
#migratoria
contma[,5:81]->contma1
colSums(contma1)->contma2
sort(contma2)
sum(contma2)
rev(sort(contma2))->contma3;contma3
data.frame(contma3,seq(1:77))->contma4;contma4
contma4[-47:-77,]->contma5
rownames(contma5)

contmb[,5:81]->contmb1
colSums(contmb1)->contmb2
sort(contmb2)
sum(contmb2)
sum(contmb2)+sum(contma2)
rev(sort(contmb2))->contmb3;contmb3
data.frame(contmb3,seq(1:77))->contmb4;contmb4
contmb4[-55:-77,]->contmb5
rownames(contmb5)
levels(as.factor(c(rownames(contmb5),rownames(contma5))))
contma

#reproductiva
contra[,5:81]->contra1
colSums(contra1)->contra2
sort(contra2)
sum(contra2)
rev(sort(contra2))->contra3;contma3
data.frame(contra3,seq(1:77))->contra4;contra4
contra4[-49:-77,]->contra5
rownames(contra5)

contrb[,5:81]->contrb1
colSums(contrb1)->contrb2
sort(contrb2)
sum(contrb2)
sum(contrb2)+sum(contra2)
rev(sort(contrb2))->contrb3;contrb3
data.frame(contrb3,seq(1:77))->contrb4;contrb4
contrb4[-51:-77,]->contrb5
rownames(contrb5)
levels(as.factor(c(rownames(contra5),rownames(contrb5))))

#ahora la curva de rarefacción
ave2
ave3<-ave2[,5:81]
taves<-colSums(ave3)
t(taves)->tavesa
write.table(tavesa,"/home/fertimjim/Documentos/datos tesis/tavesosa.csv",sep=",")
sort(taves)

rarecurve(ave3)
poolaccum(ave3,permutations=100)
rarefa1<-c(seq(100,300, by=50),sum=(taves))
rarefa2<-rarefy(taves, sample=rarefa1, se=T, MARG=2)
#rarefa2<-rarefy(taves, sample=16022, se=T, MARG=2)
#rarefa2
max(rarefa2[1,])
plot(rarefa2)
plot(rarefa1, rarefa2[1,],ylab="Especies",xlab="Individuos",type="n",ylim=c(0,70),xlim=c(0,5000),main="Curva de rarefacción general" )
points(rarefa1,rarefa2[1,]+rarefa2[2,],lty=1,col=12)
sample<-min(colSums(taves))
rarecurve(ave2, step = 20, sample = 16022, col = "blue", cex = 0.6)

##las curvas de rarefacción para parques y calles
#primero calles
names(ave2)
attach(ave2)
subset(ave2,Tipo=="cal")->calle
calle[,5:88]->calle1
colSums(calle1)->tavesc
rarefac<-c(seq(100,300, by=50),sum=(tavesc))
rarefa2c<-rarefy(tavesc, sample=rarefac, se=T, MARG=2)
plot(rarefac, rarefa2c[1,],ylab="Especies",xlab="Individuos",type="n",ylim=c(0,70),xlim=c(0,5000))
points(rarefac,rarefa2c[1,]+rarefa2[2,],lty=1,col=10)
#ahora parques
subset(ave2,Tipo=="par")->parq
parq[,5:88]->parq1
colSums(parq1)->tavesp
rarefap<-c(seq(100,300, by=50),sum=(tavesp))
rarefa2p<-rarefy(tavesp, sample=rarefap, se=T, MARG=2)
plot(rarefap, rarefa2p[1,],ylab="Especies",xlab="Individuos",type="n",ylim=c(0,70),xlim=c(0,5000))
points(rarefap,rarefa2p[1,]+rarefa2[2,],lty=1,col=11)
#las tres curvas
x11()
plot(rarefap, rarefa2p[1,],ylab="Especies",xlab="Individuos",type="n",ylim=c(0,90),xlim=c(0,5000))
points(rarefa1,rarefa2[1,]+rarefa2[2,],lty=1,col=12,pch=10)
points(rarefac,rarefa2c[1,]+rarefa2[2,],lty=1,col=10,pch=12)
points(rarefap,rarefa2p[1,]+rarefa2[2,],lty=1,col=11,pch=13)
legend(locator(1),c("Curva general","calles","parques"),pch=c(10,12,13),col=c(12,10,11))
#sin la curva de rarefacción general
x11()
plot(rarefap, rarefa2p[1,],ylab="Especies",xlab="Individuos",type="n",ylim=c(0,90),xlim=c(0,3000),main="Curvas de rarefacción")
points(rarefac,rarefa2c[1,]+rarefa2[2,],lty=1,col=10,pch=12)
points(rarefap,rarefa2p[1,]+rarefa2[2,],lty=1,col=11,pch=14)
legend(locator(1),c("calles","parques"),pch=c(12,14),col=c(10,11))

#aquí las curvas de rango abundancia
x11()
barplot(rev(sort(taves)),col=rainbow(20),space=1.5, cex.names=0.5,las=2)
x11
plot(rev(sort(taves)),col=rainbow(20),type="p",ylim=c(0,4000),xlim=c(0,90))
points(rev(sort(taves)),col=rainbow(20),space=1.5, cex.names=0.5)
lines(rev(sort(taves)),col=rainbow(20))
ave3[1,]
?radfit
plot(taves)
lines(taves)
taves
sort(taves)->taves2
mediat<-mean(taves2)
desvia<-sd(taves2)
longi<-length(taves2)
hist(log(taves2))
curve(dnorm(x,mediat,desvia)*longi,0,8,add=TRUE)
plot(log(taves2))
rang<-longi*(1-pnorm(log(taves2),mediat,desvia))
lines(rang,log(taves2))
rankabundance(ave3)->rango1
x11()
par(ylog=TRUE)
plot(rango1,log="y",ylab="Hola Mundo")
rankabunplot(rango1,scale="abundance")
rankabunplot(rango1,scale="proportion")
plot(rango1,xlab="Rango de especies",ylab="Abundancia")
x11()
log(rango1)->rango2
plot(rango2)
x11()
png(file="rangogeneral.png",width=600, height=450)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangogn.png",width=800,height=400,bg="transparent")
par(family = "serif")
plot(rango1,xlab="Rango de especies",ylab="Abundancia")
lines(rango1)
text(12,3500, "Quiscalus mexicanus", font=3)
text(12,2900, "Passer Domesticus", font=3)
text(12,2200,"Columba livia", font=3)
text(14,1800, "Haemorrhous mexicanus", font=3)
text(14,800, "Columbina inca", font=3)
dev.off()
lines(rango1)
log10(rango1)->rango2
plot(rango2)
#por parques y calles
#por calles primero
subset(ave2,Tipo=="cal")->calle
calle[,5:81]->calle1
rankabundance(calle1)->rangoc
rankabunplot(rangoc,scale="abundance",pch=4,main="Abundancia calles")
x11()
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangocn.png",width=800,height=400,bg="transparent")
par(family="serif")
plot(rangoc,xlab="Rango de especies",ylab="Abundancia", pch=4)
lines(rangoc)
text(12,2100, "Passer domesticus", font=3)
text(13,1700, "Quiscalus mexicanus", font=3)
text(13,1400,"Columba livia", font=3)
text(14,800, "Haemorrhous mexicanus", font=3)
text(14,500, "Columbina inca", font=3)
dev.off()
#por parques después
subset(ave2,Tipo=="par")->parq
parq[,5:81]->parq1
colSums(parq1)->tavesp
tavesp
rankabundance(parq1)->rangop
rankabunplot(rangop,scale="abundance",pch=5,main="Abundancia parques")
x11()
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangopn.png",width=800,height=400,bg="transparent")
par(family = "serif")
plot(rangop,xlab="Rango de especies",ylab="Abundancia", pch=5)
lines(rangop)
text(10,1800, "Quiscalus mexicanus", font=3)
text(13,1200, "Haemorrhous mexicanus", font=3)
text(13,900,"Passer domesticus", font=3)
text(13,800, "Columba livia ", font=3)
text(14,500, "Setophaga coronata", font=3)
dev.off()
log(rangoc)->rangoc1
log(rangop)->rangop1
x11()
par(mfrow=c(1,3))
plot(rangoc1,col=2)
points(rangop1,col=3)
legend(locator(1),c("calles","parques"),col=c(2,3),pch=1)
legend(4,6,c("calles","parques"),col=c(2,3),pch=1)

plot(rangoc,xlab="Rango de especies",ylab="Abundancia", main="Curva de rango abundancia calles", pch=4)
lines(rangoc)
text(12,2100, "Passer domesticus", font=3)
text(13,1700, "Quiscalus mexicanus", font=3)
text(13,1400,"Columba livia", font=3)
text(14,800, "Haemorrhous mexicanus", font=3)
text(14,500, "Columbina inca", font=3)
plot(rangop,xlab="Rango de especies",ylab="Abundancia", main="Curva de rango abundancia parques", pch=5)
lines(rangop)
text(10,1800, "Quiscalus mexicanus", font=3)
text(13,1200, "Haemorrhous mexicanus", font=3)
text(13,900,"Passer domesticus", font=3)
text(13,800, "Columba livia ", font=3)
text(14,500, "Setophaga coronata", font=3)

#juntos
rankabunplot(rangoc,scale="abundance",pch=4,col="red",main="Todos los parques")
rankabunplot(rangop,scale="abundance",pch=5, addit=TRUE,col="blue")

rankabunplot(rangoc,scale="abundance",pch=11)
points(rangoc)
x11()
par(mfrow=c(1,2))
plot(rangop,xlab="Rango de especies",ylab="Abundancia", main="Curva de rango abundancia parques", pch=5)
lines(rangop)
text(15,1900, "Quiscalus mexicanus", font=3,cex=0.7)
text(17,1200, "Haemorrhous mexicanus", font=3, cex=0.7)
text(15,900,"Passer domesticus", font=3, cex=0.7)
text(15,800, "Columba livia ", font=3,cex=0.7)
text(18,500, "Setophaga coronata", font=3,cex=0.7)

plot(rangoc,xlab="Rango de especies",ylab="Abundancia", main="Curva de rango abundancia calles", pch=4)
lines(rangoc)
text(13,2100, "Passer domesticus", font=3,cex=0.7)
text(15,1850, "Quiscalus mexicanus", font=3,cex=0.7)
text(13,1400,"Columba livia", font=3,cex=0.7)
text(19,850, "Haemorrhous mexicanus", font=3,cex=0.7)
text(15,500, "Columbina inca", font=3,cex=0.7)

log(rango1)->logig
log(rangop)->logip;logip
log(rangoc)->logic;logic
1:84->sequi
logig[,2]->logig2
logip[,2]->logip2
logic[,2]->logic2
x11()
plot(sequi,logig2, ylim=c(0,9),xlim=c(0,85),xlab="Rango",ylab="Abundancias log(escala)",pch=5)
points(sequi,logic2,col="blue",pch=6)
points(sequi,logip2,col="red",pch=7)

rep(seq(1:77),2)->repeti;repeti
rep(c("Parques","Calles"),each=77)->Sitios
c(logip2,logic2)->logics
data.frame(logics,Sitios,repeti)->pararango
x11()
ggplot(pararango,aes(x=repeti,y=logics,shape=Sitios,colour=Sitios))+geom_point()->plotigra;plotigra
plotigra+ggtitle("Rango abundancia escala logarítmica") + xlab("Rango") +ylab("Número de especies")+scale_fill_discrete(name="Sitios")+theme(panel.background=element_blank(),axis.title.x=element_text(family="serif"))
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangologi.png",width=600, height=450)
par(mar=c(5,3,2,2)+0.1,family="serif")
ggplot(pararango,aes(x=repeti,y=logics,shape=Sitios,colour=Sitios))+geom_point()->plotigra;plotigra
plotigra + xlab("Rango") +ylab("Número de especies")+scale_fill_discrete(name="Sitios")+theme(panel.background=element_blank(),axis.title.x=element_text(family="serif"))
dev.off()

#la gráfica que quiere el doc
ave3
colSums(ave3)->ran1
as.numeric(ran1)->ran2
rev(sort(ran2))->ran3;ran3
sum(ran1)->ransum
(ran3/ransum)*100->ranprop
plot(ranprop)
log10(ran3)->ran4
plot(ran4)
log(ran3)->ran5
plot(ran5)

subset(ave2,Tipo=="par")->parq
parq[,5:81]->parq1
colSums(parq1)->tavesp
tavesp
rev(sort(tavesp))->tavesn;tavesn
tavesn[-75:-77]->tavesn
names(tavesn)->tavesnamep
as.numeric(tavesp)->ranp1
rev(sort(ranp1))->ranp2;ranp2
ranp2[-75:-77]->ranp2;ranp2
sum(ranp2)->ransump
(ranp2/ransump)*100->ranpropp
plot(ranpropp)
log10(ranp2)->ranp4
plot(ranp4)
log(ranp2)->ranp5
plot(ranp5)

subset(ave2,Tipo=="cal")->calle
calle[,5:81]->calle1
colSums(calle1)->tavesc
tavesc
rev(sort(tavesc))->tavesnc;tavesnc
tavesnc[-41:-77]->tavesnc
names(tavesnc)->tavesnamec
as.numeric(tavesc)->ranc1
rev(sort(ranc1))->ranc2;ranc2
ranc2[-41:-77]->ranc2;ranc2
sum(ranc2)->ransumc
(ranc2/ransumc)*100->ranpropc
plot(ranpropc)
log10(ranc2)->ranc4
plot(ranc4)
log(ranc2)->ranc5
plot(ranc5)

seq(1:74)->repeti2;repeti2
data.frame(repeti2,ranp4,tavesnamep)->paralogp
x11()
ggplot(paralogp,aes(x=repeti2,y=ranp4,label=tavesnamep))+xlab("Rango")+ylab("Abundancias (Log 10)")+geom_point(colour="gray60",size=8,pch="*")+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"))+ggtitle("Áreas verdes")+geom_text(size=2.5,family="serif",fontface=3,angle=65,hjust=-0.5,vjust=-0.5)+scale_x_discrete(limits=c(0,77))+scale_y_discrete(limits=c(-0.5,4))+scale_x_continuous(limits=c(0,77))+scale_y_continuous(limits=c(-0.5,3.75))->grafpa;grafpa
grafpa
seq(1:40)->repeti3;repeti3
ranc4[1:40]->ranca
data.frame(repeti3,ranca,tavesnamec)->paralogc
x11()
ggplot(paralogc,aes(x=repeti3,y=ranca,label=tavesnamec))+xlab("")+ylab("")+geom_point(colour="gray60",size=6,pch="°")+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),axis.text.y = element_blank())+ggtitle("Calles")+geom_text(size=2.5,family="serif",fontface=3,angle=65,hjust=-0.5,vjust=-0.5)+scale_x_discrete(limits=c(0,40))+scale_y_discrete(limits=c(-0.5,40))+scale_x_continuous(limits=c(0,40))+scale_y_continuous(limits=c(-0.5,3.75))->grafca;grafca
grafca 
require(gridExtra)
grid.arrange(grafpa,grafca,ncol=2)
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangologaap.png", width=4000,height=2000,units="px",res=350)
grid.arrange(grafpa,grafca,ncol=2)
dev.off()
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangologaap1.png", width=1500,height=750)
grid.arrange(grafpa,grafca,ncol=2)
dev.off()
#rango abundancia juntas
c(tavesnamep,tavesnamec)->tavesj
c(ranp4,ranca)->ranj
seq(1:114)->secuj
c(rep("Áreas verdes",74),rep("Áreas grises",40))->Sitij
data.frame(tavesj,ranj,secuj,Sitij)->juntj
x11()
ggplot(juntj,aes(x=secuj,y=ranj,label=tavesj,shape=Sitij))+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=8)+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.position="top",legend.title=element_blank())+ggtitle("")+geom_text(size=2.5,family="serif",fontface=3,angle=65,hjust=-0.5,vjust=-0.5)+scale_x_continuous(breaks=c(1,74,78,117),labels=c("1","74","1","40"),limits=c(0,120))+scale_shape_manual(values=c("*","°"))+scale_color_manual(values=c("Gray10","Black"))+scale_y_continuous(limits=c(-0.5,4))->grafj;grafj	
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangologaap2.png", width=2000,height=1000,res=175)
grafj
dev.off()
#otra modificación
c(tavesnamep,tavesnamec)->tavesj
c(ranp4,ranca)->ranj
c(1:74,1:40)->secuj1
c(rep("Áreas verdes",74),rep("Áreas grises",40))->Sitij
data.frame(tavesj,ranj,secuj1,Sitij)->juntj
x11()
ggplot(juntj,aes(x=secuj1,y=ranj,label=tavesj,shape=Sitij))+facet_grid(.~Sitij,scales="free_x")+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=5)+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.title=element_blank(),legend.position="none",strip.background = element_rect(fill = 'white'))+ggtitle("")+geom_text(size=2.5,family="serif",fontface=3,angle=75,hjust=-0.5,vjust=-0.5)+scale_shape_manual(values=c("*","°"))+scale_color_manual(values=c("Gray10","Black"))+scale_y_continuous(limits=c(-1,3.6))+scale_x_continuous()->grafj1;grafj1	
ggplot(juntj,aes(x=secuj1,y=ranj,label=tavesj,shape=Sitij))+facet_grid(Sitij~.,space="free_x",scales="free_y")+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=5)+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.title=element_blank(),legend.position="none",strip.background = element_rect(fill = 'white'))+ggtitle("")+geom_text(size=2.5,family="serif",fontface=3,angle=65,hjust=-0.5,vjust=-0.5)+scale_shape_manual(values=c("*","°"))+scale_color_manual(values=c("Gray10","Black"))+scale_y_continuous(limits=c(-0.5,4))+scale_x_continuous()->grafj2;grafj2	
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangologaap4.png", width=2000,height=2500,res=300)
grafj2
dev.off()
#siguen las modificaciones
subset(ave2,Tipo=="cal")->calle
calle[,5:81]->calle1
subset(ave2,Tipo=="par")->parq
parq[,5:81]->parq1

ta.pa <- rev(sort(colSums(parq1)))
x.pa <- 1:length(ta.pa)
ta.ca <- rev(sort(colSums(calle1)))
x.ca <- 1:length(ta.ca)
plot(x.pa,log(rev(sort(ta.pa)),10),type = "n",xlim = c(0,100),ylim = c(0,3.5))
points(x.pa+15,log(rev(sort(ta.pa)),10),cex = 0.5)
points(x.pa,log(rev(sort(ta.ca)),10),cex = 0.5)
text(x.pa+15,log(rev(sort(ta.pa)),10),names(ta.pa),pos = 4,cex = 0.5,family = "serif",offset = 1,srt=45)
text(x.pa,log(rev(sort(ta.ca)),10),names(ta.ca),pos = 4,cex = 0.5,family = "serif",offset = 1,srt=45)

c(tavesnamep,tavesnamec)->tavesj
c(ranp4,ranca)->ranj
c((12:85),(1:40))->secuj
c(rep("Áreas verdes",74),rep("Áreas grises",40))->Sitij
data.frame(tavesj,ranj,secuj,Sitij)->juntj
x11()
ggplot(juntj,aes(x=secuj,y=ranj,label=tavesj,shape=Sitij))+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=8)+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.position="top",legend.title=element_blank(),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),axis.text=element_text(colour="black"))+ggtitle("")+geom_text(size=2.5,family="serif",fontface=3,angle=65,hjust=-0.5,vjust=-0.5)+scale_x_continuous(breaks=c(1,40,12,85),labels=c("1","40","1","74"),limits=c(0,85))+scale_shape_manual(values=c("°","*"))+scale_color_manual(values=c("Gray10","Black"))+scale_y_continuous(limits=c(-0.5,4))->grafj3;grafj3	
png(file="~/Documentos/Fer/datos tesis/imagenes/rangologaap5.png", width=2500,height=2000,res=300)
grafj3
dev.off()

##para la presentación
c(tavesnamep,tavesnamec)->tavesj
c(ranp4,ranca)->ranj
c(1:74,1:40)->secuj1
c(rep("Áreas verdes",74),rep("Áreas grises",40))->Sitij
data.frame(tavesj,ranj,secuj1,Sitij)->juntj
x11()
ggplot(juntj,aes(x=secuj1,y=ranj,shape=Sitij,colour=Sitij))+facet_grid(Sitij~.,space="free_x",scales="free_y")+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=8)+geom_line(size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.title=element_blank(),legend.position="top",strip.background = element_rect(fill = 'white'),axis.text=element_text(colour="black",size=12),strip.text.y = element_blank())+ggtitle("")+scale_shape_manual(values=c("°","*"))+scale_color_manual(values=c("gray20","darkgreen"))+scale_y_continuous(limits=c(-0.5,4))+scale_x_continuous()+scale_fill_manual(values=c("Gray10","Green"))->grafj2;grafj2	
png(file="~/Documentos/Fer/datos tesis/imagenes/rangopresen.png", width=2000,height=2500,res=300)
grafj2
dev.off()

ggplot(juntj,aes(x=secuj1,y=ranj,shape=Sitij,colour=Sitij))+facet_grid(Sitij~.,space="free_x",scales="free_y")+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=8)+geom_line(size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.title=element_blank(),legend.position="top",strip.background = element_rect(fill = 'white'),axis.text=element_text(colour="black",size=12),strip.text.y = element_blank())+ggtitle("")+scale_shape_manual(values=c("°","*"))+scale_color_manual(values=c("gray20","darkgreen"))+scale_y_continuous(limits=c(-0.5,4))+scale_x_continuous()+scale_fill_manual(values=c("Gray10","Green"))->grafj2;grafj2	
png(file="~/Documentos/Fer/datos tesis/imagenes/rangopresen1.png", width=4000,height=2500,res=400)
grafj2
dev.off()

#ahora a saber cuantas aves tengo por sitio
#utilizando las dos bases de datos a ver cual queda
ave1
ave2
Sitio
subset(ave2,Tipo=="par")->parq
parq[,5:81]->parq2
colSums(parq2)->parq3
rev(sort(parq3))

#por parques
#parque del arte
subset(parq,Sitio=="art")->artap1
artap1[,5:81]->artap2
colSums(artap2)->artap3
rev(sort(artap3))->artap4
data.frame(artap4,seq(1:77))
#atoyac
subset(parq,Sitio=="ato")->atoy1
atoy1[,5:81]->atoy2
colSums(atoy2)->atoy3
rev(sort(atoy3))->atoy4
data.frame(atoy4,seq(1:77))
#chapulco
subset(parq,Sitio=="cha")->chap1
chap1[,5:81]->chap2
colSums(chap2)->chap3
rev(sort(chap3))->chap4
data.frame(chap4,seq(1:77))
#CU
subset(parq,Sitio=="cu")->cun1
cun1[,5:81]->cun2
colSums(cun2)->cun3
rev(sort(cun3))->cun4
data.frame(cun4,seq(1:77))
#ecologico
subset(parq,Sitio=="eco")->ecol1
ecol1[,5:81]->ecol2
colSums(ecol2)->ecol3
rev(sort(ecol3))->ecol4
data.frame(ecol4,seq(1:77))
#fuertes
subset(parq,Sitio=="fue")->fuer1
fuer1[,5:81]->fuer2
colSums(fuer2)->fuer3
rev(sort(fuer3))->fuer4
data.frame(fuer4,seq(1:77))
#juarez
subset(parq,Sitio=="jua")->juar1
juar1[,5:81]->juar2
colSums(juar2)->juar3
rev(sort(juar3))->juar4
data.frame(juar4,seq(1:77))
#paseo
subset(parq,Sitio=="pab")->pabr1
pabr1[,5:81]->pabr2
colSums(pabr2)->pabr3
rev(sort(pabr3))->pabr4
data.frame(pabr4,seq(1:77))
#panteon
subset(parq,Sitio=="pan")->pant1
pant1[,5:81]->pant2
colSums(pant2)->pant3
rev(sort(pant3))->pant4
data.frame(pant4,seq(1:77))
#ahora para calles
subset(ave2,Tipo=="cal")->parc
parc[,5:81]->parc2
colSums(parc2)->parc3
sort(parc3)
#por cada calle
#parque del arte calle
subset(parc,Sitio=="art")->artac1
artac1[,5:81]->artac2
colSums(artac2)->artac3
rev(sort(artac3))->artac4
data.frame(artac4,seq(1:77))
#atoyac
subset(parc,Sitio=="ato")->atoyc1
atoyc1[,5:81]->atoyc2
colSums(atoyc2)->atoyc3
rev(sort(atoyc3))->atoyc4
data.frame(atoyc4,seq(1:77))
#chapulco
subset(parc,Sitio=="cha")->chapc1
chapc1[,5:81]->chapc2
colSums(chapc2)->chapc3
rev(sort(chapc3))->chapc4
data.frame(chapc4,seq(1:77))
#CU
subset(parc,Sitio=="cu")->cunc1
cunc1[,5:81]->cunc2
colSums(cunc2)->cunc3
rev(sort(cunc3))->cunc4
data.frame(cunc4,seq(1:77))
#ecologico
subset(parc,Sitio=="eco")->ecolc1
ecolc1[,5:81]->ecolc2
colSums(ecolc2)->ecolc3
rev(sort(ecolc3))->ecolc4
data.frame(ecolc4,seq(1:77))
#fuertes
subset(parc,Sitio=="fue")->fuerc1
fuerc1[,5:81]->fuerc2
colSums(fuerc2)->fuerc3
rev(sort(fuerc3))->fuerc4
data.frame(fuerc4,seq(1:77))
#juarez
subset(parc,Sitio=="jua")->juarc1
juarc1[,5:81]->juarc2
colSums(juarc2)->juarc3
rev(sort(juarc3))->juarc4
data.frame(juarc4,seq(1:77))
#paseo
subset(parc,Sitio=="pab")->pabrc1
pabrc1[,5:81]->pabrc2
colSums(pabrc2)->pabrc3
rev(sort(pabrc3))->pabrc4
data.frame(pabrc4,seq(1:77))
#panteon
subset(parc,Sitio=="pan")->pantc1
pantc1[,5:81]->pantc2
colSums(pantc2)->pantc3
rev(sort(pantc3))->pantc4
data.frame(pantc4,seq(1:77))

subset(ave2,Tipo=="cal"&Sitio=="pan")->pan4
pan4[,5:88]->pan5
colSums(pan5)->pan6
sort(pan6)
names(ave2)
Sitio
subset(parq,Si)
result<-matrix(c(15,18,17,24,21,29,14,9,17,27,44,34,46,33,44,22,20,37),9,2)
colnames(result)<-c("Calles","Parques")
rownames(result)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo bravo","Panteón")
result
x11()
barplot(result,beside=T,col=c("royalblue","snow","gray50","royalblue4","tomato1","firebrick","seagreen","snow4","wheat1"),ylab="N?mero de especies", xlab="Tipos",ylim=c(0,80),axisnames=TRUE)
text(1.5,17,"15",cex=0.8)
text(2.5,20,"18",cex=0.8)
text(3.5,19,"17",cex=0.8)
text(4.5,26,"24",cex=0.8)
text(5.5,23,"21",cex=0.8)
text(6.5,31,"29",cex=0.8)
text(7.5,16,"14",cex=0.8)
text(8.5,11,"9",cex=0.8)
text(9.5,19,"17",cex=0.8)
text(11.5,29,"27",cex=0.8)
text(12.5,46,"44",cex=0.8)
text(13.5,36,"34",cex=0.8)
text(14.5,48,"46",cex=0.8)
text(15.5,35,"33",cex=0.8)
text(16.5,46,"44",cex=0.8)
text(17.5,24,"22",cex=0.8)
text(18.5,22,"20",cex=0.8)
text(19.5,39,"37",cex=0.8)

legend(locator(1),rownames(result),fill=c("royalblue","snow","gray50","royalblue4","tomato1","firebrick","seagreen","snow4","wheat2"),cex=0.7)
#la misma gr?fica anterior pero en blanco y negro
x11()
barplot(result,beside=T,col=c("black","snow","gray50","gray67","grey21","gray80","gray90","gray45","wheat3"),ylab="Número de especies", xlab="Tipos",ylim=c(0,80),axisnames=TRUE)
text(1.5,17,"15",cex=0.8)
text(2.5,20,"18",cex=0.8)
text(3.5,19,"17",cex=0.8)
text(4.5,26,"24",cex=0.8)
text(5.5,23,"21",cex=0.8)
text(6.5,31,"29",cex=0.8)
text(7.5,16,"14",cex=0.8)
text(8.5,11,"9",cex=0.8)
text(9.5,19,"17",cex=0.8)
text(11.5,29,"27",cex=0.8)
text(12.5,46,"44",cex=0.8)
text(13.5,36,"34",cex=0.8)
text(14.5,48,"46",cex=0.8)
text(15.5,35,"33",cex=0.8)
text(16.5,46,"44",cex=0.8)
text(17.5,24,"22",cex=0.8)
text(18.5,22,"20",cex=0.8)
text(19.5,39,"37",cex=0.8)

legend(1,75,rownames(result),fill=c("black","snow","gray50","gray67","grey21","gray80","gray90","gray45","wheat3"),cex=0.7)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/divp.png",width=700,height=500,bg="transparent")
par(family="serif")
barplot(result,beside=T,col=c("black","snow","gray50","gray67","grey21","gray80","gray90","gray45","wheat3"),ylab="Número de especies", xlab="Tipos",ylim=c(0,80),axisnames=TRUE)
text(1.5,17,"15",cex=0.8)
text(2.5,20,"18",cex=0.8)
text(3.5,19,"17",cex=0.8)
text(4.5,26,"24",cex=0.8)
text(5.5,23,"21",cex=0.8)
text(6.5,31,"29",cex=0.8)
text(7.5,16,"14",cex=0.8)
text(8.5,11,"9",cex=0.8)
text(9.5,19,"17",cex=0.8)
text(11.5,29,"27",cex=0.8)
text(12.5,46,"44",cex=0.8)
text(13.5,36,"34",cex=0.8)
text(14.5,48,"46",cex=0.8)
text(15.5,35,"33",cex=0.8)
text(16.5,46,"44",cex=0.8)
text(17.5,24,"22",cex=0.8)
text(18.5,22,"20",cex=0.8)
text(19.5,39,"37",cex=0.8)

legend(1,75,rownames(result),fill=c("black","snow","gray50","gray67","grey21","gray80","gray90","gray45","wheat3"),cex=0.7)
dev.off()
#ahora con ave1
subset(ave1,sitio=="Panteon")->pante
pante[,2]
subset(ave1,sitio=="Juarez")->jua
jua[,2]
edit(jua)
write.table(jua,"/home/fertimjim/Documentos/datos tesis/jua.csv",sep="\t")
#es hora de calcular los índices de sorensen  y de bray curtis
dist(ave3)

#cálculos extras el buen Chao y el buen Jackniffe
#el método re rarefacción me supone una gran cantidad de problemas puesto
#que no es posible calcular el número de especies esperadas
ave3
poolaccum(ave3,permutations=300)->dip
dip
x11()
plot(dip$chao[,2],xlab="muestras",ylab="especies",type="n")
lines(dip$chao[,2],col="blue")
lines(dip$jack1[,2],col="red")
lines(dip$jack2[,2],col="green")
lines(dip$boot[,2],col="red")

poolaccum(BCI,permutations=999)->dip2
dip
x11()
plot(dip2$chao[,2],xlab="muestras",ylab="especies",type="n")
lines(dip2$chao[,2],col="blue")
lines(dip2$jack1[,2],col="red")
lines(dip2$jack2[,2],col="green")
lines(dip2$boot[,2],col="red")
?specnumber(BCI)
pollo1<-ave2[,4:88]
pollo1
specnumber(pollo1)
vegdist(ave3)
#ahora el cálculo de las curvas de rarefacción/extrapolación
#usando datos que no son de la tesis
library(rareNMtests)
data(Chiapas)
Chiapas
subset(Chiapas, Region=="El Triunfo")->chia1
str(chia1)
#individuos
rarefaction.sample(chia1[,-1])->chia2
rarefaction.sample(chia1[,-1],q=1)->chia3
rarefaction.sample(chia1[,-1],q=2)->chia4
#muestras
rarefaction.sample(chia1[,-1],method="coverage")->chia5
rarefaction.sample(chia1[,-1], q=1,method="coverage")->chia6
rarefaction.sample(chia1[,-1], q=2,method="coverage")->chia7
plot(chia2[,1],chia2[,2],lwd=2,xlab="Unidades de muestreo",ylab="Números de Hill")
lines(chia3[,1],chia3[,2],lwd=2,lty=2)
lines(chia4[,1],chia4[,2],lwd=2,lty=2)
plot(chia5[,1],chia5[,2],lwd=2,xlab="cobertura",ylab="Números de Hill")
lines(chia6[,1],chia6[,2],lwd=2,lty=2)
lines(chia7[,1],chia7[,2],lwd=2,lty=2)
#los análisis de mi tesis
#la diversidad beta va aquí.
#primero el análisis de cluster
beta1<-matrix(c(15,18,17,24,21,29,14,9,6,27,46,34,46,33,44,22,20,37),9,2)
colnames(beta1)<-c("parque","calles")
rownames(beta1)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo bravo","Panteón")
beta1
vegdist(beta1,method="bray")->beta2
hclust(beta2,method="complete")->beta3
x11()
plot(beta3)
vegdist(beta1,method="jaccard")->beta4;beta4
write.table(beta4,"/home/fertimjim/Documentos/datos tesis/beta1.csv")

hclust(beta4,method="complete")->beta5;beta5
plot(beta5)
1-beta4->beta6
hclust(beta6,method="complete")->beta7
hclust(beta6,method="average")->beta8

plot(beta5,hang=-1)
plot(beta7,hang=-1)
x11()
par(mfrow=c(2,1))
plot(beta7,hang=-1)
plot(beta8,hang=-1)
names(ave2)
attach(ave2)
#Sitio
subset(ave2,Tipo=="par")->betapa
betapa[,1:81]->betapa1;betapa1
betapa1[,-2:-4]->betapa2;betapa2
subset(betapa2,Sitio=="art")->bartp
bartp[,-1]->bartp1
colSums(bartp1)->bartp2
subset(betapa2,Sitio=="ato")->batop
batop[,-1]->batop1
colSums(batop1)->batop2
subset(betapa2,Sitio=="cha")->bchap
bchap[,-1]->bchap1
colSums(bchap1)->bchap2
subset(betapa2,Sitio=="cu")->bcup
bcup[,-1]->bcup1
colSums(bcup1)->bcup2
subset(betapa2,Sitio=="eco")->becop
becop[,-1]->becop1
colSums(becop1)->becop2
subset(betapa2,Sitio=="fue")->bfuep
bfuep[,-1]->bfuep1
colSums(bfuep1)->bfuep2
subset(betapa2,Sitio=="jua")->bjuap
bjuap[,-1]->bjuap1
colSums(bjuap1)->bjuap2
subset(betapa2,Sitio=="pab")->bpabp
bpabp[,-1]->bpab1
colSums(bpab1)->bpab2
subset(betapa2,Sitio=="pan")->bpanp
bpanp[,-1]->bpan1
colSums(bpan1)->bpan2
as.character(names(bpan2))->nombres
betap<-matrix(c(bartp2,batop2,bchap2,bcup2,becop2,bfuep2,bjuap2,bpab2,bpan2),77,9)
t(betap)->betap;betap
colnames(betap)<-nombres
rownames(betap)<-c("PAR","PAT","CHA","CUB","PAE","FUE","PAJ","PAB","PAM")
rownames(betap)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo bravo","Panteón")

betap
#t(betap)->betestp
#write.table(betestp,"~/Documentos/Fer/datos tesis/parquesest.csv",sep=",")
vegdist(betap,method="horn")->betap1
hclust(betap1,method="single")->betap2
plot(betap2,main="Distancias de Horn para las áreas verdes, q=1")
ifelse(betap>0,1,0)->betapsim
t(betapsim)->betapsim2
write.table(betapsim2,"~/Documentos/Fer/datos tesis/parquesestpa.csv",sep=",")
data.frame(betapsim)->betapsim;betapsim
betapsim2<- betadiver(betapsim,"sim");betapsim2
hclust(betapsim2,method="complete")->betapsim3
plot(betapsim3)

1-betapsim2->betapsim4;betapsim4
hclust(betapsim4,method="complete")->betapsim5
plot(betapsim5)
#hclust(betapsim4,method="single")->betapsim5
plot(betapsim5)

#as.matrix(betapsim)

library(cluster)
x11()
pltree(agnes(betap1,method="single"))
x11()
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betap.png",width=800,height=400,bg="transparent")
par(family="serif")
plot(betap2,main="Distancias de Horn para las áreas verdes, q=1")
dev.off()
1-betap1->betap3;betap3
hclust(betap3,method="median")->betap4
x11()
plot(betap4,hang=-1)
x11()
pltree(agnes(betap3,method="complete"))
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betap2.png",width=800,height=400,bg="transparent")
par(family="serif")
plot(betap4,main="Distancias de Horn para las áreas verdes, q=1")
dev.off()
metaMDS(betap,distance="horn")->betapm
plot(betapm)
#ahora calles
subset(ave2,Tipo=="cal")->betaca
betaca[,1:81]->betaca1;betaca1
betaca1[,-2:-4]->betaca2;betaca2
subset(betaca2,Sitio=="art")->bartc
bartc[,-1]->bartc1
colSums(bartc1)->bartc2
subset(betaca2,Sitio=="ato")->batoc
batoc[,-1]->batoc1
colSums(batoc1)->batoc2
subset(betaca2,Sitio=="cha")->bchac
bchac[,-1]->bchac1
colSums(bchac1)->bchac2
subset(betaca2,Sitio=="cu")->bcuc
bcuc[,-1]->bcuc1
colSums(bcuc1)->bcuc2
subset(betaca2,Sitio=="eco")->becoc
becoc[,-1]->becoc1
colSums(becoc1)->becoc2
subset(betaca2,Sitio=="fue")->bfuec
bfuec[,-1]->bfuec1
colSums(bfuec1)->bfuec2
subset(betaca2,Sitio=="jua")->bjuac
bjuac[,-1]->bjuac1
colSums(bjuac1)->bjuac2
subset(betaca2,Sitio=="pab")->bpabc
bpabc[,-1]->bpabc1
colSums(bpabc1)->bpabc2
subset(betaca2,Sitio=="pan")->bpanc
bpanc[,-1]->bpanc1
colSums(bpanc1)->bpanc2
as.character(names(bpanc2))->nombresc
betac<-matrix(c(bartc2,batoc2,bchac2,bcuc2,becoc2,bfuec2,bjuac2,bpabc2,bpanc2),77,9)
t(betac)->betac
colnames(betac)<-nombresc
rownames(betac)<-c("PAR","PAT","CHA","CUB","PAE","FUE","PAJ","PAB","PAM")
rownames(betac)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo bravo","Panteón")
betac
#t(betac)->betestc
#write.table(betestc,"~/Documentos/Fer/datos tesis/callesest.csv",sep=",")

vegdist(betac,method="horn")->betac1
hclust(betac1,method="single")->betac2
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betac.png",width=800,height=400,bg="transparent")
par(family="serif")
plot(betac2,main="Distancias de Horn para las regiones de calles q=1")
dev.off()
1-betac1->betac3;betac3
hclust(betac3)->betac4
x11()
plot(betac4)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betac2.png",width=800,height=400,bg="transparent")
par(family="serif")
plot(betac4,main="Distancias de Horn para las regiones de calles q=1")
dev.off()
ifelse(betac>0,1,0)->betacsim
t(betacsim)->betacsim2
write.table(betacsim2,"~/Documentos/Fer/datos tesis/callesestpa.csv",sep=",")
data.frame(betacsim)->betacsim;betacsim
betacsim2<- betadiver(betacsim,"sim");betacsim2
hclust(betacsim2,method="complete")->betacsim3
plot(betacsim3)
1-betacsim2->betacsim4
hclust(betacsim4,method="complete")->betacsim5
plot(betacsim5)

metaMDS(betac,distance="horn")->betacm
plot(betapm,display="sites")
points(betacm,display="sites",pch=2)

#
x11
plot(betapm,type="n")
points(betapm$species[,1],betapm$species[,2],pch=1)
points(betacm$species[,1],betacm$species[,2],pch=2)

polygon(betapm$points[,1],betapm$points[,2])
lines(betacm)
lines(betapm)
betapa2
#unión de matrices
betapc<-matrix(c(bartp2,batop2,bchap2,bcup2,becop2,bfuep2,bjuap2,bpab2,bpan2,bartc2,batoc2,bchac2,bcuc2,becoc2,bfuec2,bjuac2,bpabc2,bpanc2),77,18)
t(betapc)->betapc
colnames(betapc)<-nombres
rownames(betapc)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo bravo","Panteón","C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Los Fuertes","C.Juarez","C.Paseo bravo","C.Panteón")
betapc
vegdist(betapc,method="horn")->betapc1
hclust(betapc1,method="complete")->betapc2
x11()
plot(betapc2)
1-betapc1->betapc3
hclust(betapc3,method="complete")->betapc4
x11()
plot(betapc4)
metaMDS(betapc,distance="horn")->betapcm
betapc
ifelse(betapc>0,1,0)->betapcsim
data.frame(betapcsim)->betapcsim;betapcsim
#metaMDS(betapcsim,distance="sim")->betapcsim2
metaMDS (betapcsim, k=2, distfun = betadiver, distance ="sim",trymax=100)
x11()
ordiplot(betapcm,type="n")
orditorp(betapcm,display="sites",col="blue",air=0.01)
#ahora con poligonos
tratamientin=c(rep("Parques",9),rep("Calles",9))
x11()
ordiplot(betapcm,type="n")
ordihull(betapcm,groups=tratamientin,draw="polygon",col=c("grey90","red"),label=F,border="red")
orditorp(betapcm,display="sites",col="blue",air=0.01)
orditorp(betapcm,display="species",col="red",air=0.01)
color=c(rep("white",5),rep("black",5))

x11()
png(file="NMDS1.png",width=600, height=450)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/NMDS1.png",width=800,height=400,bg="transparent")
par(family="serif")
ordiplot(betapcm,type="n",xlab="coordenada1",ylab="coordenada2")
ordihull(betapcm,groups=tratamientin,draw="polygon",col=color,label=F,border="red")
orditorp(betapcm,display="sites",col="blue",air=0.01)
text(0.0,0.5,"Calles",cex=1.2,font=2)
text(0.0,-0.6,"Parques",cex=1.2,font=2)
dev.off()
1-betapcm
#usando ggplot2
install.packages("ggdendro",dep=TRUE)
library(ggplot2)
library(ggfortify)
library(devtools)
devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
install.packages("ggdendro",dep=TRUE)
library(ggdendro)
#similitud
ggdendrogram(betap4)
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betap3.png",width=21, height=15,units="cm")
ggdendrogram(betac4)
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betac3.png",width=21, height=15,units="cm")
#disimilitud pero asociado a matrices de similitud
ggdendrogram(betap2)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betap4.png",width=21, height=15,units="cm")
ggdendrogram(betac2)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betac4.png",width=21, height=15,units="cm")

#gráfica con el método de Ian
ggdendrogram(betapsim5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=11),axis.text.y=element_text(colour="black",size=11),text=element_text(family="serif"))
ggsave("~/Documentos/Fer/datos tesis/imagenes/betapsim.png",width=21, height=15,units="cm")
ggdendrogram(betacsim5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=11),axis.text.y=element_text(colour="black",size=11),text=element_text(family="serif"))
ggsave("~/Documentos/Fer/datos tesis/imagenes/betacsim.png",width=21, height=15,units="cm")


#gráfica con matrices de disimilitud
ggdendrogram(betapsim3)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=11),axis.text.y=element_text(colour="black",size=11),text=element_text(family="serif"))
ggsave("~/Documentos/Fer/datos tesis/imagenes/betapsimpres.png",width=21, height=15,units="cm")

ggsave("~/Documentos/Fer/datos tesis/imagenes/betapsim1.png",width=21, height=15,units="cm")

ggdendrogram(betacsim3)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=11),axis.text.y=element_text(colour="black",size=11),text=element_text(family="serif"))
ggsave("~/Documentos/Fer/datos tesis/imagenes/betacsimpres.png",width=21, height=15,units="cm")
ggsave("~/Documentos/Fer/datos tesis/imagenes/betacsim1.png",width=21, height=15,units="cm")

#el NMDS en ggplot2
head(fortify(betapcm))->pcm
#el paquete no sirve de mucho 
autoplot(pcm2,layers="sites",frame=TRUE,frame.type="norm")+stat_ellipse()
??frame.type
#asi que será a manopla
betapcm
fortify(betapcm)->pcm2
names(pcm2)
attach(pcm2)
c("Arte","Atoyac","Chapulco","CU","Ecológico","Fuertes","Juarez","Paseo Bravo","Panteón","C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Fuertes","C.Juarez","C.Paseo", "C.Panteón")->ques
rep(c("Área verde","Calle"),each=9)->repques
data.frame(Dim1[Score=="sites"],Dim2[Score=="sites"],ques,repques)->pcm3
colnames(pcm3)<-c("Dimension1","Dimension2","Parques","Tipos")
#para el recuerdo ggplot(pcm2,aes(x=Dim1[Score=="sites"],y=Dim2[Score=="sites"]))+geom_point()
#la verdadera
ggplot(pcm3,aes(x=Dimension1,y=Dimension2,color=Tipos))+geom_point()+stat_ellipse(geom="polygon",level=0.95,fill="gray",alpha=.01)

ggplot(pcm3,aes(x=Dimension1,y=Dimension2))+geom_point(colour="gray")+stat_ellipse(data=pcm3, aes(group=Tipos),geom="polygon",level=0.95,fill="gray",alpha=.3,color="gray90")+theme(panel.background=element_blank(),axis.text.x = element_text(colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))+annotate("text",x=0.5,y=0.5,label="Áreas verdes",family="serif")+annotate("text",x=-0.5,y=-0.2,label="Calles",family="serif")
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/NMDS2.png",width=21, height=15,units="cm")
edit(betap)
#el nuevo nmds modificado con bsim
metaMDS (betapcsim, k=2, distfun = betadiver, distance ="sim",trymax=100)->betacsim3
fortify(betacsim3)->pcmsim
names(pcmsim)
attach(pcmsim)
c("Arte","Atoyac","Chapulco","CU","Ecológico","Fuertes","Juarez","Paseo Bravo","Panteón","C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Fuertes","C.Juarez","C.Paseo", "C.Panteón")->ques1
rep(c("Área verde","Calle"),each=9)->repques1
data.frame(betacsim3=Dim1[Score=="sites"],betacsim3=Dim2[Score=="sites"],ques,repques)->pcm3
colnames(pcm3)<-c("Dimension1","Dimension2","Parques","Tipos")
metaMDS (betapcsim, k=2, distfun = betadiver, distance ="sim",trymax=100)->betacsim3
datillos <- as.data.frame(scores(betacsim3)) 
data.frame(datillos,ques1,repques1)->pcmsim;pcmsim
colnames(pcmsim)<-c("Dimension1","Dimension2","Parques","Tipos")
ggplot(pcmsim,aes(x=Dimension1,y=Dimension2,fill = Tipos,color=Tipos))+geom_point()+stat_ellipse(data=pcmsim, aes(group=Tipos),geom="polygon",level=0.95,alpha=.3)+theme(panel.background=element_blank(),axis.text.x = element_text(colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))+annotate("text",x=0.5,y=0.5,label="Áreas verdes",family="serif")+annotate("text",x=-0.5,y=-0.2,label="Calles",family="serif")->grafpol
grafpol+scale_fill_manual(values=c("dodgerblue","#FF9999"))+scale_colour_manual(values = c("red","blue"))

ggplot(pcmsim,aes(x=Dimension1,y=Dimension2,fill = Tipos,color=Tipos,shape=Tipos))+geom_point(size=12)+stat_ellipse(data=pcmsim, aes(group=Tipos),geom="polygon",level=0.95,alpha=.3)+theme(panel.background=element_blank(),axis.text.x = element_text(colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"),legend.position="top")->grafpol
grafpol+scale_fill_manual(values=c("#336666","#330000"))+scale_colour_manual(values = c("#336666","#330000"))+scale_shape_manual(values=c("*","°"))


ggplot(pcmsim,aes(x=Dimension1,y=Dimension2,fill = Tipos,color=Tipos,shape=Tipos))+geom_point(size=10)+stat_ellipse(data=pcmsim, aes(group=Tipos),geom="polygon",level=0.95,alpha=.3)+theme(panel.background=element_blank(),axis.text.x = element_text(colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"),legend.position="top")->grafpol
grafpol+scale_fill_manual(values=c("#336666","#330000"))+scale_colour_manual(values = c("#336666","#330000"))+scale_shape_manual(values=c("*","°"))
ggsave("~/Documentos/Fer/datos tesis/imagenes/NMDSsim.png",width=21, height=15,units="cm")

#plots normales
tratamientin=c(rep("Parques",9),rep("Calles",9))
x11()
ordiplot(betacsim3,type="n")
ordihull(betacsim3,groups=tratamientin,draw="polygon",col=c("grey90","red"),label=F,border="red")
orditorp(betacsim3,display="sites",col="blue",air=0.01)
orditorp(betacsim3,display="species",col="red",air=0.001)
color=c(rep("white",5),rep("black",5))

#usando vegetarian
library(vegetarian)
similarity(betap,betac,q=1)
sim.table(betap,half=T,diag=T,q=1)->betips;betips
plot(betipsa)
sim.table(betap,half=F,diag=F)->betips2;betips2
as.hclust(betips)
sim.table(betac,half=T,diag=T,q=1)->betics;betics

betips[!is.na(betips)]<-0;betips
betips[is.na(betips)]<-0;betips
betips[lower.tri(betips)]
betips
#funcionó
rownames(betips)<-rownames(betap)
colnames(betips)<-rownames(betap)
betips
dist(betips)
hclust(dist(betips),method="single")->betaps
plot(betaps)
hclust(dist(betics))->betacs
plot(betacs)

hclust(betips)
hclustfunc <- function(x, method = "complete") {    
  hclust(dist(x, method = method), method = method)
}
hclustfunc(betips)
plot(betips)
plot(hclustfunc(betips))
betips1<-is.na(betips)
betips[!betips1]

hclust(betips)

similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[1,],betap[9,])
similarity(betap[2,],betap[3,])

similarity(betac,betac)
library(scatterplot3d)

scatterplot3d(beta1)
ave2
parques<-c(27,44,34,46,33,44,22,20,37)
calles<-c(15,18,17,24,21,29,14,9,17)
cor.test(parques,calles,estimate="cor")     
plot(parques,calles)
x11()
cor.test(parques,calles,estimate="cor")
calles <- c(15,18,17,24,21,29,14,9,17)
parqes <- c(27,44,34,46,33,44,22,20,37)

png("~/Documentos/Fer/datos tesis/imagenes/Correlacion.png",width=800,height=400,bg="transparent")
par(family = "serif",mar = c(5.1,4.5,4.1,2.1))
plot(calles,parqes,pch = "*",cex = 2,xlim = c(5,35),ylim = c(20,50),xlab = "Áreas grises (Número de especies)",ylab = "Áreas verdes (Número de especies)",cex.lab = 1.5,las = 1)
text(calles,parqes + 1.5,c("Arte","Parque del Rio Atoyac","Chapulco","Ciudad Universitaria","Parque Ecológico","Los Fuertes","Juarez","Paseo Bravo","Panteón"))
dev.off()
cor.test(calles,parqes)

#gráfica de correlación para la presentación
calles <- c(15,18,17,24,21,29,14,9,17)
parqes <- c(27,44,34,46,33,44,22,20,37)
areaspr<-c("Arte","Parque del Rio Atoyac","Chapulco","Ciudad Universitaria","Parque Ecológico","Los Fuertes","Juarez","Paseo Bravo","Panteón")
data.frame(calles,parqes,areaspr)->areaspre
ggplot(areaspre,aes(x=calles,y=parqes,label=areaspr))+xlab("Áreas grises (Número de especies)")+ylab("Áreas verdes (Número de especies)")+geom_point(size=12,shape="*")+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.title=element_blank(),legend.position="top",strip.background = element_rect(fill = 'white'),axis.text=element_text(colour="black",size=15),strip.text.y = element_blank(),axis.text.y=element_text(colour="black",size=15),axis.text.x=element_text(colour="black",size=15))+ggtitle("")+geom_text(size=5,family="serif",fontface=3,hjust=0.3,vjust=-1)+scale_x_continuous(breaks=9:32,limits=c(9, 30))+scale_y_continuous(breaks=20:48,limits=c(20, 48))+annotate("text",x=27,y=22,label="R=0.82, P=0.007",family="serif",size=8)->corpre1;corpre1
png(file="~/Documentos/Fer/datos tesis/imagenes/corpre.png", width=1000,height=750,res=100)
corpre1
dev.off()

#ahora el permanova
shapiro.test(betapc)
# no son normales
sitios<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo bravo","Panteón","C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Los Fuertes","C.Juarez","C.Paseo bravo","C.Panteón")
area<-c(34.5,73.7,44.4,29.8,45.3,54.2,85.5,86.1,76.8,44.4,29.8,54.2,85.5,86.1,34.5,73.7,45.3,76.8)
data.frame(sitios,area)->padi
#cbin
#edit(betapc)
adonis(betapc~sitios,data=padi,permutations=99)->sip
summary(sip)
#permanova con los datos en bruto
ave2[,4]->visita
as.factor(visita)->visita
paste(ave2[,1],ave2[,2],sep="_")->peg
adonis(ave3~peg+visita,permutations=99)->park2
summary(park2)
park2
#anosim
attach(padi)
anosim(betapc1,sitios)
#la diversidad verdadera va aquí
#como no se de que rayos habla claudia Moreno los haré a ver que pasa con sus datos
library(FD)
read.table("/home/fertimjim/Documentos/datos tesis/carpetah/comunidadesFD.txt",header=TRUE)->comuni
read.table("/home/fertimjim/Documentos/datos tesis/carpetah/rasgosFD.txt",header=TRUE)->rasgos
as.matrix(comuni)->comuni1
dbFD(rasgos,comuni1)
ex1 <- gowdis(dummy$trait)
ex1
#diversidad funcional
library(vegan)
library(cluster)
library(lattice)
source("/home/fertimjim/Descargas/FD_Calculators.R")
read.csv("~/Documentos/Fer/datos tesis/funcional.csv",header=TRUE)->funcionalt
attach(funcionalt)
names(funcionalt)
funcionalt[,3:13]->funcionalt1
rownames(funcionalt1)<-funcionalt[,2]
funcionalt1
distancias<-daisy(funcionalt1,metric="gower")
arbi<-hclust(distancias)
as.dendrogram(arbi)->arbi2
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbi2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(funcionalt1)[order.dendrogram(arbi2)]), 4,
      las = 1, at = 1:length(funcionalt[,2]),cex=0.6,font=3)
#parques
read.csv("~/Documentos/Fer/datos tesis/funcional.csv",header=TRUE)->funcionalt
attach(funcionalt)
names(funcionalt)
subset(funcionalt,Presencia1=="parque")->parquef
parquef
parquef[,3:13]->parquef1;parquef1
rownames(parquef1)<-parquef[,2]
parquef1
distanciasp<-daisy(parquef1,metric="gower")
arbip<-hclust(distanciasp)
as.dendrogram(arbip)->arbip2
x11()
par(mar = c(5, 4, 4, 18))
plot(arbip2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquef1)[order.dendrogram(arbip2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.4,font=3)
#calles
attach(funcionalt)
names(funcionalt)
subset(funcionalt,Presencia2=="calle")->parquec
parquec
parquec[,3:13]->parquec1;parquec1
rownames(parquec1)<-parquec[,2]
parquec1
distanciasc<-daisy(parquec1,metric="gower")
arbic<-hclust(distanciasc)
as.dendrogram(arbic)->arbic2
x11()
par(mar = c(5, 4, 4, 18))
plot(arbic2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquec1)[order.dendrogram(arbic2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)

#parques solo gremios
subset(funcionalt,Presencia1=="parque")->parquef
parquef
parquef[,c(7:9,13)]->parquefg;parquefg
rownames(parquefg)<-parquef[,2]
parquefg
distanciaspg<-daisy(parquefg,metric="gower")
arbipg<-hclust(distanciaspg)
as.dendrogram(arbipg)->arbipg2
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbipg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquefg)[order.dendrogram(arbipg2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/gremimp1.png",height=980)
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbipg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquefg)[order.dendrogram(arbipg2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=1,font=3)
dev.off()
#calles solo gremios
subset(funcionalt,Presencia2=="calle")->parquec
parquec
parquec[,c(7:9,13)]->parquecg;parquecg
rownames(parquecg)<-parquec[,2]
parquecg
distanciascg<-daisy(parquecg,metric="gower")
arbicg<-hclust(distanciascg)
as.dendrogram(arbicg)->arbicg2
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbicg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquecg)[order.dendrogram(arbicg2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/gremimc1.png",height=480)
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbicg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquecg)[order.dendrogram(arbicg2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=1,font=3)
dev.off()
#modelo neutral con parques y gremios más abundancias
function(x){
for (i in seq_along(x)){
print(x[1])
}
}->nulo
for (i in seq_along(Familia)){
print(Familia[i])
}->nul1
sample(Familia,77,replace=TRUE)->nul2
replicate(1000,nul2)->nul3
sample(nul3,77,replace=F)
length(nul2)
seq_along(Familia)
as.numeric(Familia)->factori
as.factor(factori)
rep(sample(Familia,77,rep=TRUE),2)->famn
parquef[,c(7:9,13)]->parquefn1;parquefn1
data.frame(parquefn1)->parquefn2
sample(parquefn2,77,replace=TRUE)->parquefn2;parquefn2
replicate(2,parquefn2)->parquefn3;parquefn3
sample(parquefn3,replace=T,1)->parquefn4;parquefn4

#length(parquefn4)
#rownames(parquefn4)<-parquef[,2]

parquef[,6]->gp;gp
sample(gp,75,replace=TRUE)->gp1
replicate(1000,gp1)->gp2
sample(gp2,75,replace=F)->gp3
parquef[,8]->g1p;g1p
sample(g1p,75,replace=TRUE)->g1p1
replicate(1000,g1p1)->g1p2
sample(g1p2,75,replace=F)->g1p3
parquef[,9]->g2p;g2p
sample(g2p,75,replace=TRUE)->g2p1
replicate(1000,g2p1)->g2p2
sample(g2p2,75,replace=F)->g2p3
parquef[,13]->ap;ap
sample(ap,75,replace=TRUE)->ap1
replicate(1000,ap1)->ap2
sample(ap2,75,replace=F)->ap3
data.frame(as.factor(gp3),as.factor(g1p3),as.factor(g2p3),as.numeric(ap3))->nul1
list(parquef[,2])
matrix(c(gp3,g1p3,g2p3,as.numeric(ap3)),75,4)->nul2
data.frame(nul2)->nul3
colnames(nul3)<-c("g1","g2","g3","ab")
rownames(nul3)<-parquef[,2]
#modelo nulo
data.frame(gp3,ap3)->nul4
colnames(nul4)<-c("g1","ab")
rownames(nul4)<-parquef[,2]
nul4
distanciasnp<-daisy(nul4,metric="gower")
arbicnp<-hclust(distanciasnp)
as.dendrogram(arbicnp)->arbicnp2
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbicnp2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(nul4)[order.dendrogram(arbicnp2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/gremnp1.png",height=1600,width=800,res=200)
par(mar = c(5, 2, 4, 11),family="serif")
plot(arbicnp2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(nul4)[order.dendrogram(arbicnp2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
dev.off()
##el segundo modelo neutral para calles
parquec[6]->gc;gc
sample(gc,40,replace=TRUE)->gc1
replicate(1000,gc1)->gc2
sample(gc2,40,replace=F)->gc3
parquec[,8]->g1c;g1c
sample(g1c,40,replace=TRUE)->g1c1
replicate(1000,g1c1)->g1c2
sample(g1c2,40,replace=F)->g1c3
parquec[,9]->g2c;g2c
sample(g2c,40,replace=TRUE)->g2c1
replicate(1000,g2c1)->g2c2
sample(g2c2,40,replace=F)->g2c3
parquec[,13]->ac;ac
sample(ac,40,replace=TRUE)->ac1
replicate(1000,ac1)->ac2
sample(ac2,40,replace=T)->ac3
#data.frame(as.factor(gc3),as.factor(g1c3),as.factor(g2c3),as.numeric(ac3))->nulc1
#list(parquef[,2])
matrix(c(gc3,g1c3,g2c3,as.numeric(ac3)),40,4)->nulc2
data.frame(gc3,ac3)->nulc3
colnames(nulc3)<-c("g1","ab")
rownames(nulc3)<-parquec[,2]
colnames(nulc2)<-c("g1","g2","g3","ab")
rownames(nulc2)<-parquec[,2]

distanciasnc<-daisy(nulc3,metric="gower")
arbicnc<-hclust(distanciasnc)
as.dendrogram(arbicnc)->arbicnc2
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbicnc2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(nulc3)[order.dendrogram(arbicnc2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/gremnc1.png",height=1060,width=800,res=200)
par(mar = c(5, 2, 4, 11),family="serif")
plot(arbicnc2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(nulc3)[order.dendrogram(arbicnc2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
dev.off()


?seq_along
#Ahora solo gremios y abundancias sin tomar en cuenta las demás opciones
#parques
subset(funcionalt,Presencia1=="parque")->parquef
parquef
parquef[,c(6,13)]->parquefsg
rownames(parquefsg)<-parquef[,2]
parquefsg
distanciasfsg<-daisy(parquefsg,metric="gower")
arbisg<-hclust(distanciasfsg)
as.dendrogram(arbisg)->arbisg2
plot(arbisg2)
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbisg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquefsg)[order.dendrogram(arbisg2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/gremiodefp.png",height=1600,width=800,res=200)
par(mar = c(5, 2, 4, 11),family="serif")
plot(arbisg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquefsg)[order.dendrogram(arbisg2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
dev.off()
#calles
subset(funcionalt,Presencia2=="calle")->parquec
parquec
parquec[,c(6,13)]->parquecsg
rownames(parquecsg)<-parquec[,2]
parquecsg
distanciascsg<-daisy(parquecsg,metric="gower")
arbiscg<-hclust(distanciascsg)
as.dendrogram(arbiscg)->arbiscg2
#plot(arbiscg2)
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbiscg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquecsg)[order.dendrogram(arbiscg2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/gremiodefc.png",height=1060,width=800,res=200)
par(mar = c(5, 2, 4, 11),family="serif")
plot(arbiscg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquecsg)[order.dendrogram(arbiscg2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
dev.off()

#dendrogramas para la presentación
#areas verdes
x11()
par(mar = c(5, 4, 4, 18),family="serif")
plot(arbisg2,horiz=T,cex=0.05,leaflab="none")
mtext(as.character(row.names(parquefsg)[order.dendrogram(arbisg2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
png("~/Documentos/Fer/datos tesis/imagenes/gremiodefpres.png",height=2120,width=1600,res=250)
par(mar = c(5, 11, 4, 12),family="serif")
plot(arbisg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquefsg)[order.dendrogram(arbisg2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
dev.off()
#calles
x11()
par(mar = c(5, 18, 4, 4),family="serif")
plot(arbiscg2,horiz=T,cex=0.05,leaflab="none",xlim=c(0,max(distanciascsg)))  
mtext(as.character(row.names(parquecsg)[order.dendrogram(arbiscg2)]), 2,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
png("~/Documentos/Fer/datos tesis/imagenes/gremiodefcres.png",height=2120,width=1600,res=250)
par(mar = c(5, 11, 4, 12),family="serif")
plot(arbiscg2,horiz=T,cex=0.05,leaflab="none",xlim=c(0,max(distanciascsg)))  
mtext(as.character(row.names(parquecsg)[order.dendrogram(arbiscg2)]), 2,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
dev.off()
#sin voltear calles
png("~/Documentos/Fer/datos tesis/imagenes/gremiodefcres1.png",height=2120,width=1600,res=250)
par(mar = c(5, 12, 4, 11),family="serif")
plot(arbiscg2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(parquecsg)[order.dendrogram(arbiscg2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
dev.off()

#nulos
png("~/Documentos/Fer/datos tesis/imagenes/gremnulprep.png",height=2120,width=1600,res=250)
par(mar = c(5, 2, 4, 11),family="serif")
plot(arbicnp2,horiz=T,cex=0.05,leaflab="none")  
mtext(as.character(row.names(nul4)[order.dendrogram(arbicnp2)]), 4,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
dev.off()
png("~/Documentos/Fer/datos tesis/imagenes/gremnulprec.png",height=2120,width=1600,res=250)
par(mar = c(5, 2, 4, 11),family="serif")
plot(arbiscg2,horiz=T,cex=0.05,leaflab="none",xlim=c(0,max(distanciasnp)))  
mtext(as.character(row.names(parquecsg)[order.dendrogram(arbiscg2)]), 4,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
dev.off()
##nulos
png("~/Documentos/Fer/datos tesis/imagenes/gremnulprep1.png",height=2120,width=1600,res=250)
par(mar = c(5, 12, 4, 11),family="serif")
plot(arbicnp2,horiz=T,cex=0.05,leaflab="none",xlim=c(0,max(distanciasnp)))  
mtext(as.character(row.names(nul4)[order.dendrogram(arbicnp2)]), 2,
      las = 1, at = 1:length(parquef[,2]),cex=0.6,font=3)
dev.off()
png("~/Documentos/Fer/datos tesis/imagenes/gremnulprec1.png",height=2120,width=1600,res=250)
par(mar = c(5, 12, 4, 11),family="serif")
plot(arbicnc2,horiz=T,cex=0.05,leaflab="none",xlim=c(0,max(distanciasnc)))  
mtext(as.character(row.names(nulc3)[order.dendrogram(arbicnc2)]), 2,
      las = 1, at = 1:length(parquec[,2]),cex=0.6,font=3)
dev.off()




#prueba de ji cuadrado
#parques
subset(funcionalt,Presencia1=="parque")->parquef
#Renombramos los campos de esta tabla
names(parquef) <- c("par.Clave","par.Especie","par.Familia","par.Orden","par.Residencia","par.Gremio","par.Alimentacion1","par.Alimentacion2","par.Alimentacion3","par.Selección_sexual","par.Habitat","par.Nido","par.Abundancia","par.Estatus","par.Presencia1","par.Presencia2","par.sumac")
#Lo adjuntamos si no no jala
attach(parquef)

#calles
subset(funcionalt,Presencia2=="calle")->parquec
#Renombramos los campos de esta tabla
names(parquec) <- c("cal.Clave","cal.Especie","cal.Familia","cal.Orden","cal.Residencia","cal.Gremio","cal.Alimentacion1","cal.Alimentacion2","cal.Alimentacion3","cal.Selección_sexual","cal.Habitat","cal.Nido","cal.Abundancia","cal.Estatus","cal.Presencia1","cal.Presencia2","cal.sumac")
attach(parquec)

#Ahora la gráfica de la frecuencia de especies de cada gremio para áreas verdes y calles.
frec.par <- table(par.Gremio)
frec.cal <- table(cal.Gremio)
frec.tot <- numeric(10)
frec.tot[1:10 %%2 != 0] <- frec.par
frec.tot[1:10 %%2 == 0] <- frec.cal
barplot(frec.tot,col = c("green","gray50"),density = 25)
chisq.test(t(matrix(frec.tot,byrow = T,nrow = 5)))

#la ji cuadrad que quiere federico
c(6,4,45,13,6,5,3,19,7,6)->cuantasg
matrix(cuantasg,ncol=2,nrow=5)->matrixcg
t(matrixcg)->matrixcg1
rownames(matrixcg1)<-c("Areas verdes","Areas grises")
chisq.test(matrixcg1)->chimat
chimat
chimat$expected

#la ji cuadrada con abundancias
c(86,12,2383,2802,3068,43,3,437,3104,4084)->cuantasg1
matrix(cuantasg1,ncol=2,nrow=5)->matrixcga
t(matrixcga)->matrixcg1a
rownames(matrixcg1a)<-c("Areas verdes","Areas grises")
chisq.test(matrixcg1a)->chimat1
chimat1
chimat1$expected

#especies por familia
rev(c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,5,6,6,8,9,11))->fam2
rev(c("Aegithalidae","Cuculidae","Falconidae","Laniidae","Mimidae","Passeridae","Psittacidae","Ptiliogonatidae",  "Sturnidae","Thraupidae","Cardinalidae","Corvidae","Fringilidae","Hirundinidae","Sylviidae","Accipitridae","Picidae","Troglodytidae","Turdidae","Columbidae","Emberizidae", "Trochilidae","Icteridae","Parulidae","Tyrannidae"))->fam1
familia<-matrix(fam2,1,25)
colnames(familia)<-(fam1)
box()
barplot(familia,beside=F,space=1,las=2,col="snow",cex.names=0.6, ylim=c(0,12), density=-3,border="black")
text(1.5,11.5,"11",cex=0.8)
text(3.5,10,"9",cex=0.8)
text(5.5,9,"8",cex=0.8)
text(7.5,7,"6",cex=0.8)
text(9.5,7,"6",cex=0.8)
text(11.5,6,"5",cex=0.8)
text(13.5,4,"3",cex=0.8)
text(15.5,4,"3",cex=0.8)
text(17.5,4,"3",cex=0.8)
text(19.5,4,"3",cex=0.8)
text(21.5,3,"2",cex=0.8)
text(23.5,3,"2",cex=0.8)
text(25.5,3,"2",cex=0.8)
text(27.5,3,"2",cex=0.8)
text(29.5,3,"2",cex=0.8)
text(31.5,2,"1",cex=0.8)
text(33.5,2,"1",cex=0.8)
text(35.5,2,"1",cex=0.8)
text(37.5,2,"1",cex=0.8)
text(39.5,2,"1",cex=0.8)
text(41.5,2,"1",cex=0.8)
text(43.5,2,"1",cex=0.8)
text(45.5,2,"1",cex=0.8)
text(47.5,2,"1",cex=0.8)
text(49.5,2,"1",cex=0.8)
data.frame(fam1,fam2)->fam3
library(ggplot2)
library(grid)
#la que usé
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/familias.png",width=2000,bg="transparent",res=200,height=1000)
print(ggplot(fam3,aes(x=reorder(fam1,fam2),y=fam2))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))+theme(panel.background=element_blank())+labs(x="Familias",y="Número de especies"))
dev.off()
ggplot(fam3,aes(x=reorder(fam1,fam2),y=fam2))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+theme(panel.background=element_blank())+labs(x="Familias",y="Número de especies")->famil
famil
ggsave("/home/fertimjim/Documentos/datos tesis/imagenes/familias.png",width=21, height=15,units="cm")
## plot
##al reves volteada
png("~/Documentos/Fer/datos tesis/imagenes/familias2.png",width=2000,bg="transparent",res=200,height=1000)
(ggplot(fam3,aes(x=reorder(fam1,order(fam2, decreasing = TRUE)),y=fam2,label=fam2))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))+theme(panel.background=element_blank())+labs(x="Familias",y="Número de especies"))+geom_text(size=5,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)
dev.off()

png("~/Documentos/Fer/datos tesis/imagenes/familiaspres.png",width=2000,bg="transparent",res=200,height=1000)
(ggplot(fam3,aes(x=reorder(fam1,order(fam2, decreasing = TRUE)),y=fam2,label=fam2))+geom_bar(stat="identity",fill="#336669")+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif",size=18))+theme(panel.background=element_blank())+labs(x="Familias",y="Número de especies"))+geom_text(size=5,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)
dev.off()


#Porcentaje de cobertura de la muestra
library("iNEXT")
taves
t(rev(sort(taves)))->avesillas;avesillas
as.numeric(avesillas)->avesillas2
c(avesillas2)->avesillas3;avesillas3
length(avesillas3)
iNEXT(avesillas3, q=0, datatype="abundance")->estimador1
estimador1
write.table(estimador1,"estimador1.csv",sep=",")
edit(estimador1)
77/98*100
iNEXT(avesillas3, q=c(0,1,2))->estimador2
iNEXT(avesillas3, q=1, datatype="abundance")->estimador3
estimador3
iNEXT(avesillas3, q=2, datatype="abundance")->estimador4
estimador4
#tengo 78% de completitud de especies
x11()
ggiNEXT(estimador1,type=1,color.var="none",facet.var="none")->grafi1;grafi1
grafi1 + ggtitle("Estimador de especies") + xlab("N?mero de individuos") +ylab("N?mero de especies")+geom_point(colour="gray50")
estimador1$iNextEst$m->xa
estimador1$iNextEst$qD->ya
estimador1$iNextEst$qD.95.LCL->yame
estimador1$iNextEst$qD.95.UCL->yam
estimador1$iNextEst$SC.95.LCL->yame2;yame2
estimador1$iNextEst$SC.95.UCL->yam2
estimador1$iNextEst$SC->za
estimador1$iNextEst$method->metodo;metodo
rep("Intrapolacion",19)->intra;intra
rep("extrapolacion",20)->extra;extra
c(intra,"Observada",extra)->Metodo;Metodo
data.frame(Metodo,metodo,xa,ya,yame,yam,yame2,yam2,za)->paragraf;paragraf
ggplot(paragraf,aes(x=xa,y=ya)) + geom_point(data = paragraf, aes(xa[21:40], ya[21:40],fill=Metodo[21:40])) + geom_line(data = paragraf, aes(xa[1:20],ya [1:20],linetype="intrapolaci?n"))+geom_ribbon(aes(ymin=yame,ymax=yam),alpha=0.2)->plotig;plotig
plotig+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("Numero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")

#sin linea
ggplot(paragraf,aes(x=xa,y=ya,shape=Metodo,colour=Metodo)) + geom_point(data = paragraf, aes(xa[21:40], ya[21:40],fill=Metodo[20:40])) + geom_point(data = paragraf, aes(xa[1:20],ya [1:20],fill=Metodo[1:20],shape=1))+geom_ribbon(aes(ymin=yame,ymax=yam),alpha=0.2)->plotig;plotig
plotig+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="Metodo")+labs(fill="Metodo")
x11()
ggplot(paragraf,aes(x=za,y=ya,shape=Metodo,colour=Metodo))+geom_point()+geom_ribbon(aes(ymin=yame2,ymax=yam2),alpha=0.2)+geom_line()->plotig3
plotig3+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="Metodo")+labs(fill="Metodo")
ggplot(paragraf,aes(x=xa,y=za,shape=Metodo,colour=Metodo))+geom_point()+geom_ribbon(aes(ymin=yame,ymax=yam),alpha=0.2)+geom_line()->plotig4
plotig4+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="Metodo")+labs(fill="Metodo")


#la guardamos
png(file="estimadorg.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(paragraf,aes(x=xa,y=ya,shape=Metodo,colour=Metodo))+geom_point()+geom_ribbon(aes(ymin=yame,ymax=yam),alpha=0.2)+geom_line()->plotig2
plotig2+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
dev.off()

plot(xa,ya)
#ahora juntamos las 3
iNEXT(avesillas3, q=c(0,1,2), datatype="abundance")->estimador5

x11()
ggiNEXT(estimador5, type=1)->grafi2
grafi2 + ggtitle("Estimador de las especies") + xlab("N?mero de individuos") +ylab("N?mero de especies")
X11()
ggiNEXT(estimador5, type=2)->grafi3;grafi3
grafi3 + ggtitle("Porcentaje de cobertura de la muestra") + xlab("N?mero de individuos") +ylab("Cobertura de la muestra")
x11()
ggiNEXT(estimador5, type=3)->grafi4;grafi4
grafi4 + ggtitle("Cobertura y especies") + xlab("Cobertura") +ylab("N?mero de especies")
#ahora las gr?ficas por parques y calles
#primero por parques
read.table("listadodos.csv",header=TRUE)->ave2
attach(ave2)
names(ave2)
subset(ave2,Tipo=="par")->avepa;avepa
names(avepa)
avepa1<-avepa[,5:81];avepa1
avepa2<-colSums(avepa1);avepa2
rev(sort(avepa2))->avepa3;avepa3
data.frame(avepa3,seq(c(1:77)))
avepa3[c(-75,-76,-77)]->avepa4
t(avepa4)->avepa5;avepa5
as.numeric(avepa5)->avepa6
length(avepa6)
as.numeric(avepa2)->avepa2m
rev(sort(avepa2m))
iNEXT(avepa6, q=0, datatype="abundance")->estimadorpa;estimadorpa
estimadorpa
iNEXT(avepa6, q=0, datatype="incidence")->estimadorpa1;estimadorpa1
estimadorpa1

ChaoSpecies(avepa6, datatype = "abundance", conf = 0.84)
ChaoSpecies(avepa6, datatype = "incidence", conf = 0.84)

specaccum(avepa1,"random",ci=2)->pruep
plot(pruep)
specnumber(avepa1)->S
(raremax <- min(rowSums(avepa1)))
Srare <- rarefy(avepa1, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
rarecurve(avepa1, step = 20, sample = raremax, col = "blue", cex = 0.6)
x11()
ggiNEXT(estimadorpa, type=1)->grafip;grafip
grafi1 + ggtitle("Estimador de especies:parques") + xlab("N?mero de individuos") +ylab("N?mero de especies")
#calles
specaccum(aveca1,"random",ci=2)->pruec
pruec
specaccum(aveca1,"rarefaction",ci=2,conditioned =TRUE,sites=9)->pruec
pruec
#Por partes
estimadorpa$iNextEst$m->xap
estimadorpa$iNextEst$qD->yap
estimadorpa$iNextEst$qD.95.LCL->yamep
estimadorpa$iNextEst$qD.95.UCL->yamp
estimadorpa$iNextEst$SC.95.LCL->yamep2;yamep2
estimadorpa$iNextEst$SC.95.UCL->yamp2
estimadorpa$iNextEst$SC->zap
estimadorpa$iNextEst$method->metodop;metodop
rep("Intrapolacion",19)->intrap;intrap
rep("extrapolacion",20)->extrap;extrap
c(intrap,"Observada",extrap)->Metodo;Metodo
data.frame(Metodo,metodop,xap,yap,yamep,yamp,yamep2,yamp2)->paragrafp;paragrafp
x11()
ggplot(paragrafp,aes(x=xap,y=yap,shape=Metodo,colour=Metodo))+geom_point()+geom_ribbon(aes(ymin=yamep,ymax=yamp),alpha=0.2)+geom_line()->plotigp2
plotigp2+ggtitle("Estimador de especies por parques") + xlab("N?mero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
#la guardamos
png(file="estimadorp.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(paragrafp,aes(x=xap,y=yap,shape=Metodo,colour=Metodo))+geom_point()+geom_ribbon(aes(ymin=yamep,ymax=yamp),alpha=0.2)+geom_line()->plotigp2
plotigp2+ggtitle("Estimador de especies por parques") + xlab("N?mero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
dev.off()
#para calles
read.table("listadodos.csv",header=TRUE)->ave2
attach(ave2)
names(ave2)
subset(ave2,Tipo=="cal")->aveca;aveca
names(aveca)
aveca1<-aveca[,5:81];aveca1
aveca2<-colSums(aveca1);aveca2
rev(sort(aveca2))->aveca3;aveca3
data.frame(aveca3,seq(c(1:77)))
t(aveca3)->aveca4;aveca4
aveca4[,c(-41:-77)]->aveca5;aveca5
as.numeric(aveca5)->aveca6
length(aveca6)
data.frame(aveca6,seq(c(1:41)))
as.numeric(aveca2)->aveca2m
t(avepa2m)->avepa2m1
t(aveca2m)->aveca2m1
matrix(c(avepa2m1,aveca2m1),77,2)->modificada;modificada
write.table(modificada,"/home/fertimjim/Documentos/Fer/datos tesis/modificada.csv",sep=",")
iNEXT(aveca6, q=0, datatype="abundance")->estimadorca;estimadorca
ChaoSpecies(aveca6, datatype = "abundance", conf = 0.84)

x11()
ggiNEXT(estimadorca, type=1)->grafic;grafic
grafic + ggtitle("Estimador de especies:calles") + xlab("N?mero de individuos") +ylab("N?mero de especies")
#por partes
estimadorca$iNextEst$m->xac
estimadorca$iNextEst$qD->yac
estimadorca$iNextEst$qD.95.LCL->yamec
estimadorca$iNextEst$qD.95.UCL->yamc
estimadorca$iNextEst$SC.95.LCL->yamec2;yamec2
estimadorca$iNextEst$SC.95.UCL->yamc2
estimadorca$iNextEst$method->metodoc;metodoc
estimadorca$iNextEst$SC->zac
rep("Intrapolacion",19)->intrac;intrac
rep("extrapolacion",20)->extrac;extrac
c(intrac,"Observada",extrac)->Metodo;Metodo
data.frame(Metodo,metodoc,xac,yac,yamec,yamc,yamec2,yamc2)->paragrafc;paragrafc
x11()
ggplot(paragrafc,aes(x=xac,y=yac,shape=M?todo,colour=M?todo))+geom_point()+geom_ribbon(aes(ymin=yamec,ymax=yamc),alpha=0.2)+geom_line()->plotigc2
plotigc2+ggtitle("Estimador de especies por calles") + xlab("N?mero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
#la guardamos
png(file="estimadorca.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(paragrafc,aes(x=xac,y=yac,shape=M?todo,colour=M?todo))+geom_point()+geom_ribbon(aes(ymin=yamec,ymax=yamc),alpha=0.2)+geom_line()->plotigc2
plotigc2+ggtitle("Estimador de especies por calles") + xlab("N?mero de individuos") +ylab("N?mero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
dev.off()
#todos
list(avesillas3,avepa6,aveca6)->avejun
iNEXT(avejun, q=0, datatype="abundance")->estimadorjun;estimadorjun
#gráficas unidas
names(paragraf)
names(paragrafp)
names(paragrafc)
rep(Metodo,3)->Metodot
rep(metodo,3)->metodot
c(xa,xap,xac)->xat
c(ya,yap,yac)->yat
c(yame,yamep,yamec)->yamet
c(yam,yamp,yamc)->yamt
c(yame2,yamep2,yamec2)->yamet2
c(yam2,yamep2,yamec2)->yamt2
c(za,zap,zac)->zat
rep(c("General","Parques","Calles"),each=40)->tipos
data.frame(Metodot,metodot,xat,yat,zat,yamet,yamt,yamet2,yamt2,tipos)->generali
ggplot(generali,aes(x=xat,y=yat,shape=tipos,colour=Metodot))+geom_point(colour="gray")+geom_ribbon(aes(ymin=yamet,ymax=yamt),alpha=0.1)+geom_line()+theme(panel.background=element_blank())->plotigt
plotigt+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("Numero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
ggplot(generali,aes(x=xat,y=yat))+geom_line(data=generali,x=xat[1:20],y=xat[1:20])

ggplot(generali,aes(x=xat,y=yat,fill=tipos))+geom_line(aes(linetype=factor(tipos)))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank())+geom_line(aes(y=yamet),linetype="dotted",colour="grey50")+geom_line(aes(y=yamt),linetype="dotted",colour="grey50")+labs(fill="Tipos",linetype="Tipos",shape="Método")->plotag;plotag
plotag+xlab("Número de individuos")+ylab("Número de especies")+annotate("text",x=12000,y=26,label="Calles",family="serif")+annotate("text",x=30000,y=60,label="General",family="serif")+annotate("text",x=1000,y=90,label="Parques",family="serif")
ggplot(generali,aes(x=xat,y=yat,shape=tipos))+geom_point()+geom_ribbon(aes(ymin=yamet,ymax=yamt),alpha=0.1)+geom_line()+theme(panel.background=element_blank())->plotigt
plotigt+ggtitle("Estimador de especies") + xlab("Numero de individuos") +ylab("Numero de especies")+theme(legend.text=element_text(colour=c("blue","red")))+scale_fill_discrete(name="M?todo")+labs(fill="Metodo")
#general que salió bien
ggplot(generali,aes(x=xat,y=yat,fill=tipos))+geom_line(aes(linetype=factor(tipos)))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank())+geom_line(aes(y=yamet),linetype="dotted",colour="grey50")+geom_line(aes(y=yamt),linetype="dotted",colour="grey50")+labs(fill="Tipos",linetype="Tipos",shape="Método")->plotag;plotag
plotag+xlab("Número de individuos")+ylab("Número de especies")+annotate("text",x=12000,y=26,label="Calles",family="serif")+annotate("text",x=30000,y=60,label="General",family="serif")+annotate("text",x=1000,y=90,label="Parques",family="serif")
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/estimbw.png",width=21, height=15,units="cm")
#general modificada
ggplot(generali,aes(x=xat,y=yat,fill=tipos,linetype=tipos))+geom_line()+scale_linetype_manual(values=c("solid","solid","solid"))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank(),legend.position="none",text=element_text(family="serif"))+geom_line(aes(y=yamet),,colour="grey50",linetype="dashed")+geom_line(aes(y=yamt),colour="grey50",linetype="dashed")+labs(fill="Tipos",linetype="Tipos",shape="Método")+guides(fill=FALSE)+xlab("Número de individuos")+ylab("Número de especies")->plotag1;plotag1
plotag1+annotate("text",x=19000,y=46,label="Calles",family="serif")+annotate("text",x=20000,y=95,label="General",family="serif")+annotate("text",x=11000,y=90,label="Áreas verdes",family="serif")
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/estimbw1.png",width=21, height=15,units="cm")

#general con sombras
ggplot(generali,aes(x=xat,y=yat,fill=tipos,linetype=tipos))+geom_line()+scale_linetype_manual(values=c("solid","solid","solid"))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank(),legend.position="none",text=element_text(family="serif"),axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black"))+labs(fill="Tipos",linetype="Tipos",shape="Método")+guides(fill=FALSE)+geom_ribbon(aes(ymin=yamet,ymax=yamt),alpha=0.2)+scale_fill_manual(values=c("gray50","gray50","gray50"))+xlab("Número de individuos")+ylab("Número de especies")->plotag2;plotag2
plotag2+annotate("text",x=19000,y=46,label="Áreas grises",family="serif")+annotate("text",x=20000,y=95,label="General",family="serif")+annotate("text",x=11000,y=90,label="Áreas verdes",family="serif")
ggsave("~/Documentos/Fer/datos tesis/imagenes/estimbw2.png",width=21, height=15,units="cm")

#general presentación
ggplot(generali,aes(x=xat,y=yat,fill=tipos,linetype=tipos))+geom_line()+scale_linetype_manual(values=c("solid","solid","solid"))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank(),legend.position="none",text=element_text(family="serif"),axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black"))+labs(fill="Tipos",linetype="Tipos",shape="Método")+guides(fill=FALSE)+geom_ribbon(aes(ymin=yamet,ymax=yamt),alpha=0.3)+scale_fill_manual(values=c("gray50","#D55E00","#339900"))+xlab("Número de individuos")+ylab("Número de especies")->plotag3;plotag3
plotag3+annotate("text",x=19000,y=46,label="Áreas grises",family="serif")+annotate("text",x=20000,y=95,label="General",family="serif")+annotate("text",x=11000,y=90,label="Áreas verdes",family="serif")
ggsave("~/Documentos/Fer/datos tesis/imagenes/estipermapres.png",width=21, height=15,units="cm")

#ahora el de cobertura de la muestra
ggplot(generali,aes(x=xat,y=zat,fill=tipos))+geom_line(aes(linetype=factor(tipos)))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank())+geom_line(aes(y=yamet2),linetype="dotted",colour="grey50")+geom_line(aes(y=yamt2),linetype="dotted",colour="grey50")+labs(fill="Tipos",linetype="Tipos",shape="Método")->plotagco;plotagco
plotagco+xlab("Número de individuos")+ylab("Cobertura de la muestra")
ggsave("/home/fertimjim/Documentos/datos tesis/imagenes/estimcob.png",width=21, height=15,units="cm")

ggplot(generali,aes(x=xat,y=zat,fill=tipos))+geom_line(aes(linetype=factor(tipos)))
ggplot(generali,aes(x=zat,y=yat,fill=tipos))+geom_line(aes(linetype=factor(tipos)))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank())+geom_line(aes(y=yamet2),linetype="dotted",colour="grey50")+geom_line(aes(yamt2),linetype="dotted",colour="grey50")+labs(fill="Tipos",linetype="Tipos",shape="Método")->plotagca;plotagca
plotagca+xlab("Cobertura de la muestra")+ylab("Número de especies")
ggsave("/home/fertimjim/Documentos/datos tesis/imagenes/estimcab.png",width=21, height=15,units="cm")


#perfil de diversidad
#datos sacados de los valores del análisis anterior
c(77,12,7)->general1
c(1,2,3)->cosas
c(40,7,5)->calles1
c(75,16,9)->parques10
c("q0","q1","q2")->Orden
rep(c("q0","q1","q2"),2)->Orden
rep(c("Calles","Parques"),each=3)->Sitio;Sitio
intpam<-c(126.313,16.783,10.234)
intpame<-c(78.864,16.236,9.919)
intcam<-c(156.227,7.636,5.580)
intcame<-c(47.883,7.404,5.580)
intgem<-c(158.334,12.507,7.921)
intgeme<-c(82.486,12.224,7.767)
errorsing<-c(16.431,0.131,0.074)
errorsinp<-c(10.399,0.220,0.171)
errorsinc<-c(23.165,0.103,0.060)

abs(general1-intgeme)->intgeme1
abs(general1-intgem)->intgem1
abs(calles1-intcame)->intcame1
abs(calles1-intcam)->intcam1
abs(parques10-intpame)->intpame1
abs(parques10-intpam)->intpam1
c(calles1,parques10)->junti
c(intcam,intpam)->junti2
c(intcame,intpame)->junti3
c(errorsinc,errorsinp)->junti4
data.frame(junti,Orden, Sitio,junti2,junti3,junti4)->framip;framip
x11()
par(bg = "gray")
x11()
plot(cosas, general1, xaxt="n",ylim=c(0,200),pch=1,col="green",ylab="N?mero de especies",xlab="Orden",cex=1.5,main="Perfil de diversidad")
segments(cosas,general1,cosas,general1+intgem1,col="green")
segments(cosas,general1,cosas,general1-intgeme1,col="green")

lines(cosas,general1,col="darkgreen")
points(cosas,parques10,col="blue",pch=4,cex=1.5)
segments(cosas,parques10,cosas,parques10+intpam1,col="blue")
segments(cosas,parques10,cosas,parques10-intpame1,col="blue")
lines(cosas,parques10,col="blue")
points(cosas,calles1,col="red",pch=5,cex=1.5)
segments(cosas,calles1,cosas,calles1+intcam1,col="red")
segments(cosas,calles1,cosas,calles1-intcame1,col="red")

lines(cosas,calles1,col="red")
mtext(c("q0","q1","q2"),side=1,line=1,adj=c(0,0.5,1))
legend(locator(1),c("general","parques","calles"),pch=c(3,4,5),col=c("green","blue","red"))
mtext(c("q0","q1","q2"),side=1,line="n")
grafi3
estimador5
x11()
plot(cosas, general1, xaxt="n",ylim=c(0,100),pch=1,col="green",ylab="N?mero de especies",xlab="Orden",cex=1.5,main="Perfil de diversidad")
lines(cosas,general1,col="darkgreen")
points(cosas,parques10,col="blue",pch=4,cex=1.5)
lines(cosas,parques10,col="blue")
points(cosas,calles1,col="red",pch=5,cex=1.5)
lines(cosas,calles1,col="red")
mtext(c("q0","q1","q2"),side=1,line=1,adj=c(0,0.5,1))
legend(locator(1),c("general","parques","calles"),pch=c(3,4,5),col=c("green","blue","red"))
mtext(c("q0","q1","q2"),side=1,line="n")
#ahora con ggplot2
x11()
ggplot(framip,aes(x=Orden,y=junti,shape=Sitio,color=Sitio))+geom_point(size=8)+geom_line(aes(group=Sitio))+geom_errorbar(aes(ymin=junti-junti4,ymax=junti+junti4),linetype="dotted")+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"),legend.position="top",legend.title=element_blank())+scale_color_manual(values=c("Gray40","Black"))+scale_shape_manual(values=c("°","*"))->perfili
perfili+ggtitle("Perfil de diversidad") + xlab("Orden") +ylab("Número de especies")+theme(legend.text=element_text(colour=c("blue","red")))
#la guardamos
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/perfili.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(framip,aes(x=Orden,y=junti,shape=Sitio,color=Sitio))+geom_point(size=8)+geom_line(aes(group=Sitio))+geom_errorbar(aes(ymin=junti-junti4,ymax=junti+junti4),linetype="dotted")+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"),legend.position="top",legend.title=element_blank())+scale_color_manual(values=c("Gray30","Black"))+scale_shape_manual(values=c("°","*"))->perfili
perfili + xlab("Orden") +ylab("Número de especies")
dev.off()


#la gráfica que quiere Ian
#Estos datos salieron del comando ChaoSpecies
c(86,63)->generalian
c(1,2)->ordensin
c("Áreas verdes","Matriz urbana")->citys
interu<-c(105.12,107)
interl<-c(78.6,48.58)
interu1<-c(interu-generalian)
interl1<-c((abs(interl-generalian)))
x11()
plot(ordensin, generalian, xaxt="n",ylim=c(0,200),pch=1,col="green",type="n",ylab="Número de especies",xlab="Sitios",cex=1.5)
points(ordensin,generalian,col="blue",pch="*",cex=1.5)
segments(ordensin,generalian,ordensin,generalian+(interu-generalian),col="black")
segments(ordensin,generalian,ordensin,generalian-(abs(interl-generalian)),col="black")
library(ggplot2)
data.frame(generalian,citys,interu1,interl1)->framip2
ggplot(framip2,aes(x=citys,y=generalian,shape=citys,color=citys))+geom_point(size=12)+geom_line(aes(group=citys))+geom_errorbar(aes(ymin=generalian-interl1,ymax=generalian+interu1),linetype="dotted")+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"),legend.position="top",legend.title=element_blank())+scale_color_manual(values=c("Gray30","Black"))+scale_shape_manual(values=c("*","°"))->estima
estima + ylab("Número de especies (84% Intervalo de confianza)") +xlab("Sitios")


#Estos datos salieron con Estimates
c(74,40)->generalian1
c(1,2)->ordensin1
c("Áreas verdes","Áreas grises")->citys1
interu1<-c(5,4.7)
interu2<-c(generalian1+interu1)
interl2<-c((abs(interu1-generalian1)))
data.frame(generalian1,citys1,interu2,interl2)->framip3
library(ggplot2)
ggplot(framip3,aes(x=citys1,y=generalian1,shape=citys1,color=citys1))+geom_point(size=12)+geom_line(aes(group=citys1))+geom_errorbar(aes(ymin=interl2,ymax=interu2),linetype="dotted",size=0.8)+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black",size=13),text=element_text(family="serif",size=13),axis.text.y=element_text(colour="black",size=13),legend.position="null",legend.title=element_blank())+scale_color_manual(values=c("Grey10","Black"))+scale_shape_manual(values=c("*","°"))->estima1
estima1 + ylab("Número de especies (84% Intervalo de confianza)") +xlab("Sitios")
png(file="~/Documentos/Fer/datos tesis/imagenes/raref84.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(framip3,aes(x=citys1,y=generalian1,shape=citys1,color=citys1))+geom_point(size=12)+geom_line(aes(group=citys1))+geom_errorbar(aes(ymin=interl2,ymax=interu2),linetype="dotted",size=0.8)+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black",size=13),text=element_text(family="serif",size=13),axis.text.y=element_text(colour="black",size=13),legend.position="null",legend.title=element_blank())+scale_color_manual(values=c("Gray10","Black"))+scale_shape_manual(values=c("*","°"))->estima1
estima1 + ylab("Número de especies (84% Intervalo de confianza)") +xlab("Sitios")
dev.off()
#para la presentación
png(file="~/Documentos/Fer/datos tesis/imagenes/raref84pres.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(framip3,aes(x=citys1,y=generalian1,shape=citys1,color=citys1))+geom_point(size=12)+geom_line(aes(group=citys1))+geom_errorbar(aes(ymin=interl2,ymax=interu2),linetype="dotted",size=0.8)+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black",size=13),text=element_text(family="serif",size=13),axis.text.y=element_text(colour="black",size=13),legend.position="null",legend.title=element_blank())+scale_color_manual(values=c("Gray10","forestgreen"))+scale_shape_manual(values=c("°","*"))+annotate("text",x=2.2,y=74,label="74±5",family="serif",size=7)+annotate("text",x=1.2,y=40,label="40±4.7",family="serif",size=7)->estima2
estima2+ ylab("Número de especies (84% Intervalo de confianza)") +xlab("Sitios")
dev.off()
library(entropart)
data("Paracou618")->para
edit(para)
library(vegan)
data(dune)        
para
summary(Paracou618.MC)
edit(Paracou618.MC)
p <- DivPart(q = 2, MC = Paracou618.MC, Biased = FALSE)
plot
edit(p)
summary(p)
plot(p)
ave3
taves
subset(ave2,Tipo=="par")->avepa;avepa
names(avepa)
avepa1<-avepa[,5:81];avepa1
avepa2<-colSums(avepa1);avepa2
as.numeric(avepa2)->avepam
subset(ave2,Tipo=="cal")->aveca;aveca
aveca1<-aveca[,5:81];aveca1
aveca2<-colSums(aveca1);aveca2
as.numeric(aveca2)->avecam
data.frame(avepa2,seq(c(1:77)))->avema;avema
rownames(avema)->names
Calles<-avecam
Parques<-avepam
Especies<-names
data.frame(avecam,avepam)->base
rownames(base)<-names
base
c("calles","parques")->sitios
colnames(base)<-sitios
#write.table(base,"/home/fertimjim/Documentos/Fer/datos tesis/modificada.csv",sep=",")
#?entropart
comunidades<-c("calles","parques")
pesos<-c(1,1)
data.frame(comunidades,pesos)->base2
MetaCommunity(base,pesos)->aveMC
DivPart(q = 0, MC = aveMC, Biased = FALSE)->aveMC2
plot(summary(aveMC2))
summary(aveMC2)->aveMC3
summary(aveMC2)
x11()
png("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/divalfbe.png",width=800,height=400,bg="transparent")
par(family="serif")
plot(aveMC2,ann=FALSE)
title(xlab=expression("Diversidad"~alpha~"y"~gamma),ylab=expression("Diversidad"~beta))
rect(0,0,65,1,col="gray")
dev.off()
polygon(0:65,c(0,1.0,rep(1,63),0),col="gray",border="gray",lwd=3,lty="solid")

Paracou618.MC
p <- DivPart(q = 0, MC = Paracou618.MC, Biased = FALSE)
summary(p)
plot(aveMC)
perfi <- DivProfile(seq(0, 2, 0.2), aveMC, Biased = FALSE)
summary(perfi)
x11()
perfi
plot(perfi)
library(ggplot2)
ggplot(perfi)
c(1.33,1.09,1.09)->beta
c("q0","q1","q2")->Orden2
c(1,2,3)->Orden3
data.frame(beta,Orden2,Orden3)->betaj
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/perfilib.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(betaj,aes(x=Orden2,y=beta))+geom_point()+geom_line(aes(x=Orden3))+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"))->perfilib
perfilib+ xlab("Orden") +ylab("Número de comunidades")
dev.off()
x11()
ggplot(betaj,aes(x=Orden2,y=beta))+geom_point()+geom_line(aes(x=Orden3))+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"))->perfilib
perfilib
#perfil beta presentación
png(file="~/Documentos/Fer/datos tesis/imagenes/perfilib1.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(betaj,aes(x=Orden2,y=beta))+geom_point(color="darkgreen",shape="*",size=12)+geom_line(aes(x=Orden3),color="darkgreen")+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"))->perfilib1
perfilib1+ xlab("Orden") +ylab("Número de comunidades")
dev.off()

#Perfil gamma
c(86.3,12.11,7.69)->gamma
c("q0","q1","q2")->Ordeng2
c(1,2,3)->Ordeng3
data.frame(gamma,Ordeng2,Ordeng3)->gammaj
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/perfilig.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(gammaj,aes(x=Ordeng2,y=gamma))+geom_point()+geom_line(aes(x=Ordeng3))+theme(panel.background=element_blank())->perfilig
perfilig+ xlab("Orden") +ylab("Diversidad gamma")
dev.off()
x11()
ggplot(gammaj,aes(x=Ordeng2,y=gamma))+geom_point()+geom_line(aes(x=Ordeng3))+theme(panel.background=element_blank())->perfilig
perfilig
library(vegetarian)
H(avecam,lev="alpha",q=2)
H(avepam,lev="alpha",q=2)
matrix(c(avecam,avepam),2,77)->matbetav
H(matbetav,lev="beta",q=2)

#usando SpadeR
library(SpadeR)
Diversity(Calles,datatype="abundance")
Diversity(Parques,datatype="abundance")


###graficas de barras para la presentacion
#ejemplos para resultados
library(ggplot2)
tempo<-c("Total","Migratoria","Reproductiva")
cuantos<-c(77,64,63)
data.frame(tempo,cuantos)->tempora
ggplot(tempora,aes(x=tempo,y=cuantos,fill=tempo))+geom_bar(stat="identity")
png("~/Documentos/Fer/datos tesis/imagenes/cuantos.png",width=1200,bg="transparent",res=100,height=600)
ggplot(tempora,aes(x=reorder(tempo,order(tempo, decreasing = TRUE)),y=cuantos,fill=tempo,label=cuantos))+geom_bar(stat="identity",width=.5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=15),axis.text.y=element_text(colour="black",size=15),text=element_text(family="serif",size=15),legend.position="none")+theme(panel.background=element_blank())+labs(x="Temporadas",y="Número de especies")+geom_text(size=6,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)+  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
dev.off()

tipos<-c("Residentes","Migratorios","Introducidos")
cuantos1<-c(49,24,4)
data.frame(tipos,cuantos1)->tempora1
ggplot(tempora1,aes(x=tipos,y=cuantos1,fill=tipos))+geom_bar(stat="identity")
png("~/Documentos/Fer/datos tesis/imagenes/tipos.png",width=1000,bg="transparent",res=100,height=500)
ggplot(tempora1,aes(x=reorder(tipos,order(tipos, decreasing = TRUE)),y=cuantos1,fill=tipos,label=cuantos1))+geom_bar(stat="identity",width=.5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=15),axis.text.y=element_text(colour="black",size=15),text=element_text(family="serif",size=15),legend.position="none")+theme(panel.background=element_blank())+labs(x="Tipos",y="Número de especies")+geom_text(size=5,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)
dev.off()

png("~/Documentos/Fer/datos tesis/imagenes/tipos1.png",width=1000,bg="transparent",res=100,height=500)
ggplot(tempora1,aes(x=reorder(tipos,order(tipos, decreasing = TRUE)),y=cuantos1,fill=tipos,label=cuantos1))+geom_bar(stat="identity",width=.5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=15),axis.text.y=element_text(colour="black",size=15),text=element_text(family="serif",size=15),legend.position="none")+theme(panel.background=element_blank())+labs(x="Tipos",y="Número de especies")+geom_text(size=5,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)+annotate("text",x=2,y=40,label="25 familias y 8 órdenes",family="serif",size=7)
dev.off()

dupli<-c("Únicas","Duplicadas")
cuantos2<-c(13,4)
data.frame(dupli,cuantos2)->duplia
ggplot(duplia,aes(x=dupli,y=cuantos2,fill=dupli))+geom_bar(stat="identity")
png("~/Documentos/Fer/datos tesis/imagenes/unidup.png",width=1000,bg="transparent",res=100,height=500)
ggplot(duplia,aes(x=reorder(dupli,order(dupli, decreasing = TRUE)),y=cuantos2,fill=dupli,label=cuantos2))+geom_bar(stat="identity",width=.5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=15),axis.text.y=element_text(colour="black",size=15),text=element_text(family="serif",size=15),legend.position="none")+theme(panel.background=element_blank())+labs(x="Tipos",y="Número de especies")+geom_text(size=5,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)+scale_fill_manual(values=c("#CC6666", "#9999CC"))
dev.off()

sitias<-c("Áreas verdes","Áreas Grises")
cuantos3<-c(74,40)
data.frame(sitias,cuantos3)->sitias1
ggplot(sitias1,aes(x=sitias,y=cuantos3,fill=sitias))+geom_bar(stat="identity")
png("~/Documentos/Fer/datos tesis/imagenes/sitiospres.png",width=1000,bg="transparent",res=100,height=500)
ggplot(sitias1,aes(x=reorder(sitias,order(sitias, decreasing = TRUE)),y=cuantos3,fill=sitias,label=cuantos3))+geom_bar(stat="identity",width=.5)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=15),axis.text.y=element_text(colour="black",size=15),text=element_text(family="serif",size=15),legend.position="none")+theme(panel.background=element_blank())+labs(x="Sitios",y="Número de especies")+geom_text(size=5,family="serif",fontface=3,colour="black",hjust=0.5,vjust=-0.2)+scale_fill_manual(values=c("gray70", "darkgreen"))
dev.off()

gremiin<-rep(c("Nectarívoros","Carnívoros","Insectívoros","Granívoros","Omnívoros"),2)
cuantosg<-c(6,4,45,13,6,5,3,19,7,6)
areagr<-rep(c("Áreas verdes","Áreas grises"),each=5)
data.frame(gremiin,cuantosg,areagr)->grembar
expression( Chi^2 ==2.697, "gl=4,P=0.61" )->expresion
as.character(expresion)->expresion1
ggplot(grembar,aes(x=gremiin,y=cuantosg,fill=areagr))+geom_bar(stat="identity",position="dodge")
png("~/Documentos/Fer/datos tesis/imagenes/grems.png",width=1000,bg="transparent",res=100,height=500)
ggplot(grembar,aes(x=gremiin,y=cuantosg,fill=areagr,label=cuantosg))+geom_bar(stat="identity",width=.5,position="dodge")+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black",size=15),axis.text.y=element_text(colour="black",size=15),text=element_text(family="serif",size=15),legend.position="none")+theme(panel.background=element_blank())+labs(x="Gremios",y="Número de especies")+geom_text(size=5,family="serif",fontface=2,colour="black",position = position_dodge(width=.5),vjust=-0.1)+scale_fill_manual(values=c("gray70", "darkgreen"))
dev.off()
##ejemplos de materiales y métodos
c(15,30,24,12,25,12.5)->barra1
matrix(barra1,ncol=3,nrow=2)->barra2
colnames(barra2)<-c("Biodiversidad","Ruido","Polución")
rownames(barra2)<-c("Urbano","Rural")
barplot(barra2,beside=T,legend.text=T,col = c("gray30","darkgreen"))
png(file="~/Documentos/Fer/datos tesis/imagenes/barra1.png",width=800, height=500,res=100)
par(mar=c(5,3,3,3)+0.1)
barplot(barra2,beside=T,legend.text=T,col = c("gray30","darkgreen"))
dev.off()

c(15,24,25)->barrau
matrix(barrau,ncol=3,nrow=1)->barrau2;barrau2
colnames(barrau2)<-c("Biodiversidad","Ruido","Polución")

c(30,12,12.5)->barrar
matrix(barrar,ncol=3,nrow=1)->barrar2;barrar2
colnames(barrar2)<-c("Biodiversidad","Ruido","Polución")

png(file="~/Documentos/Fer/datos tesis/imagenes/barrau.png",width=800, height=500,res=100)
par(mar=c(5,3,3,3)+0.1)
barplot(barrau2,beside=T,col = c("gray30"),axisnames = TRUE,cex.names=2,cex=2)
dev.off()

png(file="~/Documentos/Fer/datos tesis/imagenes/barrar.png",width=800, height=500,res=100)
par(mar=c(5,3,3,3)+0.1)
barplot(barrar2,beside=T,col = c("darkgreen"),axisnames = TRUE,cex.names=2,cex=2)
dev.off()
png(file="~/Documentos/Fer/datos tesis/imagenes/coseno1.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,3,3,3)+0.1)
plot(cos(seq(1,20,by=0.09)),type="n",axes=F,xlab="",ylab="")
lines(cos(seq(1,20,by=0.09)),col="darkred",lwd=3)
dev.off()
png(file="~/Documentos/Fer/datos tesis/imagenes/coseno2.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,3,3,3)+0.1)

plot(cos(seq(1,20,by=0.09)),type="n",axes=F,xlab="",ylab="")
lines(cos(seq(1,20,by=0.09)),col="darkblue",lwd=3)
dev.off()

c(15,30,24,12,25,25)->barra1a
matrix(barra1a,ncol=3,nrow=2)->barra2a
colnames(barra2a)<-c("Caso1","Caso2","Caso3")
rownames(barra2a)<-c("Gris","Verde")
barplot(barra2a,beside=T,legend.text=T,col = c("gray30","darkgreen"))
png(file="~/Documentos/Fer/datos tesis/imagenes/barra1a.png",width=800, height=500,res=100)
par(mar=c(6,3,3,3)+0.1)
barplot(barra2a,beside=T,col = c("gray30","darkgreen"),cex.names=2,cex=2)
legend(3,27,main=rownames(barra2a))
dev.off()
	
c(seq(1,20,by=0.1))->rarefin
seq(1:100)->x
plot(log((rarefin)^2))
c(seq(1:100))->rarefin
log((rarefin)^2)->y

plot(x,y,type="n",xlab="Número de especies",ylab="Número de individuos",main="Intrapolación/extrapolación",family="serif",cex=2,cex.axis=2)
lines(x[1:50],y[1:50])
points(x[51:101],y[51:101])
text(105,0.5,"Chao et al. 2015",cex=2,lwd=3,family="serif")

png(file="~/Documentos/Fer/datos tesis/imagenes/interprue.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(x,y,type="n",xlab="Número de individuos",ylab="Número de especies",main="Intrapolación/extrapolación",family="serif",cex.names=2,cex.axis=1.5,cex.lab=1.5)
lines(x[1:50],y[1:50])
points(x[51:100],y[51:100])
text(50,0.5,"Chao et al. 2015",cex=2,lwd=3,family="serif")
dev.off()

library(BiodiversityR)
data(dune.env)
data(dune)
RankAbun.1 <- rankabundance(dune)
RankAbun.1
png(file="~/Documentos/Fer/datos tesis/imagenes/rankprue.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)

plot(RankAbun.1,scale='abundance', addit=FALSE, main="Rango-Abundancia",xlab="Rango",ylab="Abundancia",family="serif",cex.axis=1.5,cex.lab=1.5)
lines(RankAbun.1)
text(15,2,"Magurran 2004",cex=2,family="serif")
dev.off()
#correlación
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
png(file="~/Documentos/Fer/datos tesis/imagenes/corprue.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(x,y,main="Correlación de Spearman",xlab="Riqueza de áreas grises ",ylab="Riqueza de áreas verdes ",family="serif",cex.axis=1.5,cex.lab=1.5)
text(55,2.7,"Zar 2010",cex=2,family="serif")
dev.off()

#ejemplo intervalos
c(77,80,42,45)->ejemi
c(1,2,3,4)->ordeneje
intere<-c(97,100,62,65)
intere1<-c(57,60,22,25)
intere2<-c(intere-ejemi)
intere3<-c((abs(intere1-ejemi)))
png(file="~/Documentos/Fer/datos tesis/imagenes/intereje.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(ordeneje, ejemi,ylim=c(0,105),pch=1,col="green",type="n",ylab="Intervalos de confianza 84%",xlab="Sitios",cex=1.5,cex.lab=1.5,xaxt="n",cex.axis=1.5)
points(ordeneje, ejemi,cex=1.5)
segments(ordeneje,ejemi,ordeneje,ejemi+(intere-ejemi),col="black")
segments(ordeneje,ejemi,ordeneje,ejemi-(abs(intere1-ejemi)),col="black")
text(2.5,5,"MacGregor-Fors y Payton (2013)",cex=2,family="serif")

dev.off()

#Ancova
arbolae<-seq(10,100,by=2)
rnorm(46,4,1)->ej1
rnorm(46,7,4)->ej2
png(file="~/Documentos/Fer/datos tesis/imagenes/ancoeje.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(arbolae,ej1,type="n",xlab="Área arbolada",ylab="Diversidad q=1",cex=1.5,cex.lab=1.5,cex.axis=1.5,ylim=c(0,20))
points(arbolae,ej1,pch=1)
points(arbolae,ej2,pch=3)
legend(80,19,c("Área verde","Área gris"),pch=c(1,3))
dev.off()

png(file="~/Documentos/Fer/datos tesis/imagenes/ancoeje1.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(arbolae,ej1,type="n",xlab="Distancia",ylab="Diversidad q=1",cex=1.5,cex.lab=1.5,cex.axis=1.5,ylim=c(0,20))
points(arbolae,ej1,pch=1)
points(arbolae,ej2,pch=3)
legend(80,19,c("Área verde","Área gris"),pch=c(1,3))
dev.off()

#simil
data(varespec)
vare.dist <- vegdist(varespec)
hclust(vare.dist)->ejesim
plot(ejesim,hang=-1,ylab="",main="Índices de similitud")
png(file="~/Documentos/Fer/datos tesis/imagenes/indiceeje.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(ejesim,hang=-1,ylab="",main="Índices de similitud",labels=F,sub="",xlab="")
dev.off()

plot(ejesim,hang=-1,ylab="",main="Índices de similitud",horiz=F)
png(file="~/Documentos/Fer/datos tesis/imagenes/gremioeje.png",width=800, height=500,res=100,bg="transparent")
par(mar=c(5,4,3,3)+0.1)
plot(as.dendrogram(ejesim),hang=-1,ylab="",main="",horiz=T,sub="",xlab="",leaflab="n",)
dev.off()

#Mapas 
library(maps)
library(maptools)
library(rgdal)
library(mapdata)
#paqueteria maps
map_data("world",region="Mexico")->mexi
x11()
ggplot(mexi, aes(x=long, y=lat, group=group)) + geom_path() #este mapa se ve de baja calidad
#paqueteria maptools
readShapePoly("/home/fertimjim/Documentos/Fer/datos tesis/Quantum GIS tesis2/Mapas de Puebla para tesis/Vectoriales/Puebla1/e14b43_area_urb50_a_utm.shp")->pueb
fortify(pueb)->puebmap
x11()
ggplot(puebmap, aes(x=long, y=lat, group=group)) + geom_path() 
x11()
map("worldHires","Mexico",col="gray90",fill=T)

##paqueteria leaflet
library(raster)
library(leaflet)

