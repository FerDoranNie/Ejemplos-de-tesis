#Este script corresponde  a los análisis de la tesis con título Importancia de las áreas verdes de la ciudad de Puebla para la conservación de la avifauna
#Riqueza y abundancia
#Realizar la curva de rango abundancia 
#Antes que nada es necesario realizar una matriz de datos 
#Utilizaremos una base de datos de las paqueterías disponibles. En este caso vegan
install.packages("vegan",dep=TRUE)
install.packages("devtools",dep=TRUE)
#llamar al paquete
library(devtools)
install_github("JohnsonHsieh/iNEXT")
library(iNEXT)
read.table("/home/juan/Documentos/Fer/datos tesis/ejemplotesis.txt",header=TRUE,sep=",")->datos1
attach(datos1)
names(datos1)
(datos1[,5:81])->datos2
by(datos2,list(datos1$Sitio,datos1$Tipo),FUN=colSums)->sumadatos1;sumadatos1
unlist(sumadatos1)->sumadatos2;sumadatos2
#paste(sumaave2)->sumaave2
matrix(sumadatos2,77,18)->matriz1
t(matriz1)->matriz1
colnames(matriz1)<-names(datos2)
rownames(matriz1)<-c("artc","atoc","chac","cuc","ecoc","fuec","juac","pabc","panc","artp","atocp","chap","cup","ecop","fuep","juap","pabp","panp")
#edit(matrizsuma)
data.frame(matriz1)->matriz2
Hverd<- function(x){
  x<-x[x>0]
  P<-x/sum(x)
  -sum(P*log(P))->la
  exp(la)->la2
  print(la2)
}
c(Hverd(matriz2[1,]),Hverd(matriz2[2,]),Hverd(matriz2[3,]),Hverd(matriz2[4,]),Hverd(matriz2[5,]),Hverd(matriz2[6,]),Hverd(matriz2[7,]),Hverd(matriz2[8,]),Hverd(matriz2[9,]),Hverd(matriz2[10,]),Hverd(matriz2[11,]),Hverd(matriz2[12,]),Hverd(matriz2[13,]),Hverd(matriz2[14,]),Hverd(matriz2[15,]),Hverd(matriz2[16,]),Hverd(matriz2[17,]),Hverd(matriz2[18,]))->verdadera1
area3<-rep(c(16.1,4.8,5.5,6.21,6.9,7.38,9.77,7.58,4,44.4,29.8,54.2,85.5,86.1,34.5,73.7,45.3,76.8),each=1)
sum(area3)
area3
tipos1<-rep(c("calles","parques"),each=9)
as.factor(tipos1)->tipos2
data.frame(verdadera1,area3,tipos2)
ancova4<-lm(verdadera1~area3*tipos2);ancova4
ancova4<-lm(verdadera1~tipos2*area3);ancova4
summary(ancova4)
anova(ancova4)
distancias<-rep(c(3855,3681,5428,3923,1887,664,4400,5506,3370),2)
tipos<-rep(c("calles","parques"),each=9)
as.factor(tipos)->tipos1
data.frame(verdadera1,tipos,distancias)
ancova5<-lm(verdadera1~tipos*distancias);ancova5
summary(ancova5)
anova(ancova5,test="Chisq")

#curva de rango abundancia
subset(datos1,Tipo=="par")->parq
parq[,5:81]->parq1
colSums(parq1)->tavesp
tavesp
rev(sort(tavesp))->tavesn;tavesn
tavesn[-75:-77]->tavesn
names(tavesn)->tavesnamep
as.numeric(tavesp)->ranp1
rev(sort(ranp1))->ranp2;ranp2
ranp2[-75:-77]->ranp2;ranp2
log10(ranp2)->ranp4
subset(datos1,Tipo=="cal")->calle
calle[,5:81]->calle1
colSums(calle1)->tavesc
tavesc
rev(sort(tavesc))->tavesnc;tavesnc
tavesnc[-41:-77]->tavesnc
names(tavesnc)->tavesnamec
as.numeric(tavesc)->ranc1
rev(sort(ranc1))->ranc2;ranc2
ranc2[-41:-77]->ranc2;ranc2
log10(ranc2)->ranc4
ranc4[1:40]->ranca
library(ggplot2)
c(tavesnamep,tavesnamec)->tavesj
c(ranp4,ranca)->ranj
c((12:85),(1:40))->secuj
c(rep("Áreas verdes",74),rep("Calles",40))->Sitij
data.frame(tavesj,ranj,secuj,Sitij)->juntj
x11()
ggplot(juntj,aes(x=secuj,y=ranj,label=tavesj,shape=Sitij))+xlab("Rango")+ylab("Abundancias  Log10")+geom_point(size=8)+geom_line(colour="black",size=0.5)+theme(panel.background=element_blank(),text=element_text(family="serif"),legend.position="top",legend.title=element_blank(),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),axis.text=element_text(colour="black"))+ggtitle("")+geom_text(size=2.5,family="serif",fontface=3,angle=65,hjust=-0.5,vjust=-0.5)+scale_x_continuous(breaks=c(1,40,12,85),labels=c("1","40","1","74"),limits=c(0,85))+scale_shape_manual(values=c("*","°"))+scale_color_manual(values=c("Gray10","Black"))+scale_y_continuous(limits=c(-0.5,4))->grafj3;grafj3
ggplotly(grafj3)->grafj4
grafj4
#png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/rangologaap5.png", width=2500,height=2000,res=300)
#grafj3
#dev.off()
(datos1[,5:81])->datos2
taves<-colSums(datos2)
library("iNEXT")
taves
t(rev(sort(taves)))->avesillas;avesillas
as.numeric(avesillas)->avesillas2
c(avesillas2)->avesillas3;avesillas3
length(avesillas3)
iNEXT(avesillas3, q=0, datatype="abundance")->estimador1
estimador1
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
subset(datos1,Tipo=="par")->avepa;avepa
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
subset(datos1,Tipo=="cal")->aveca;aveca
names(aveca)
aveca1<-aveca[,5:81];aveca1
aveca2<-colSums(aveca1);aveca2
rev(sort(aveca2))->aveca3;aveca3
data.frame(aveca3,seq(c(1:77)))
t(aveca3)->aveca4;aveca4
aveca4[,c(-41:-77)]->aveca5;aveca5
as.numeric(aveca5)->aveca6
iNEXT(aveca6, q=0, datatype="abundance")->estimadorca;estimadorca
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
library(ggplot2)
ggplot(generali,aes(x=xat,y=yat,fill=tipos,linetype=tipos))+geom_line()+scale_linetype_manual(values=c("solid","solid","solid"))+geom_point(aes(shape=factor(Metodot,"extrapolacion")))+theme(panel.background=element_blank(),legend.position="none",text=element_text(family="serif"),axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black"))+labs(fill="Tipos",linetype="Tipos",shape="Método")+guides(fill=FALSE)+geom_ribbon(aes(ymin=yamet,ymax=yamt),alpha=0.2)+scale_fill_manual(values=c("gray50","gray50","gray50"))+xlab("Número de individuos")+ylab("Número de especies")->plotag2;plotag2
#plotag2+annotate("text",x=19000,y=46,label="Calles",family="serif")+annotate("text",x=20000,y=95,label="General",family="serif")+annotate("text",x=11000,y=90,label="Áreas verdes",family="serif")
#ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/estimbw2.png",width=21, height=15,units="cm")

##### Ahora el perfil de diversidad.
####Opcional

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
errorsinp<-c(10.399,0.220,0.171)
errorsinc<-c(23.165,0.103,0.060)

c(calles1,parques10)->junti
c(intcam,intpam)->junti2
c(intcame,intpame)->junti3
c(errorsinc,errorsinp)->junti4
data.frame(junti,Orden, Sitio,junti2,junti3,junti4)->framip;framip
#png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/perfili.png",width=700, height=450,res=100)
#par(mar=c(5,3,2,2)+0.1)
ggplot(framip,aes(x=Orden,y=junti,shape=Sitio,color=Sitio))+geom_point(size=8)+geom_line(aes(group=Sitio))+geom_errorbar(aes(ymin=junti-junti4,ymax=junti+junti4),linetype="dotted")+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"),legend.position="top",legend.title=element_blank())+scale_color_manual(values=c("Gray30","Black"))+scale_shape_manual(values=c("°","*"))->perfili
perfili + xlab("Orden") +ylab("Número de especies")
ggplotly(perfili)->perfilia
perfilia

#dev.off()
#ahora el análisis de cluster
library(vegan)
subset(datos1,Tipo=="par")->betapa
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
rownames(betap)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los Fuertes","Juarez","Paseo Bravo","Panteón")
betap
vegdist(betap,method="horn")->betap1
hclust(betap1,method="single")->betap2
#para calcular el índice betaSim
ifelse(betap>0,1,0)->betapsim
t(betapsim)->betapsim2
data.frame(betapsim)->betapsim;betapsim
betapsim2<- betadiver(betapsim,"sim");betapsim2
hclust(betapsim2,method="complete")->betapsim3
1-betapsim2->betapsim4;betapsim4
hclust(betapsim4,method="complete")->betapsim5

subset(datos1,Tipo=="cal")->betaca
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
rownames(betac)<-c("C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Los fuertes","C.Juarez","C.Paseo bravo","C.Panteón")
betac
vegdist(betac,method="horn")->betac1
hclust(betac1,method="single")->betac2
ifelse(betac>0,1,0)->betacsim
t(betacsim)->betacsim2
data.frame(betacsim)->betacsim;betacsim
betacsim2<- betadiver(betacsim,"sim");betacsim2
hclust(betacsim2,method="complete")->betacsim3
plot(betacsim3)
1-betacsim2->betacsim4
hclust(betacsim4,method="complete")->betacsim5

install.packages("ggdendro",dep=TRUE)
library(ggplot2)
library(ggdendro)
ggdendrogram(betap2)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betap4.png",width=21, height=15,units="cm")
ggdendrogram(betac2)+theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/betac4.png",width=21, height=15,units="cm")
betapc<-matrix(c(bartp2,batop2,bchap2,bcup2,becop2,bfuep2,bjuap2,bpab2,bpan2,bartc2,batoc2,bchac2,bcuc2,becoc2,bfuec2,bjuac2,bpabc2,bpanc2),77,18)
t(betapc)->betapc
colnames(betapc)<-nombres
rownames(betapc)<-c("Arte","Atoyac","Chapulco","CU","Ecológico","Los fuertes","Juarez","Paseo bravo","Panteón","C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Los fuertes","C.Juarez","C.Paseo bravo","C.Panteón")
metaMDS(betapc,distance="horn")->betapcm
fortify(betapcm)->pcm2
names(pcm2)
attach(pcm2)
c("Arte","Atoyac","Chapulco","CU","Ecológico","Fuertes","Juarez","Paseo Bravo","Panteón","C.Arte","C.Atoyac","C.Chapulco","C.CU","C.Ecológico","C.Fuertes","C.Juarez","C.Paseo", "C.Panteón")->ques
rep(c("Área verde","Calle"),each=9)->repques
data.frame(Dim1[Score=="sites"],Dim2[Score=="sites"],ques,repques)->pcm3
colnames(pcm3)<-c("Dimension1","Dimension2","Parques","Tipos")
ggplot(pcm3,aes(x=Dimension1,y=Dimension2))+geom_point(colour="gray")+stat_ellipse(data=pcm3, aes(group=Tipos),geom="polygon",level=0.95,fill="gray",alpha=.3,color="gray90")+theme(panel.background=element_blank(),axis.text.x = element_text(colour="black"),axis.text.y=element_text(colour="black"),text=element_text(family="serif"))+annotate("text",x=0.5,y=0.5,label="Áreas verdes",family="serif")+annotate("text",x=-0.5,y=-0.2,label="Calles",family="serif")
ggsave("/home/fertimjim/Documentos/Fer/datos tesis/imagenes/NMDS2.png",width=21, height=15,units="cm")
#permanova
datos1[,4]->visita
as.factor(visita)->visita
paste(datos1[,1],datos1[,2],sep="_")->peg
adonis(datos2~peg+visita,permutations=99)->park2
summary(park2)
#perfil de diversidad beta
library(entropart)
subset(datos1,Tipo=="par")->avepa;avepa
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
pesos<-c(1,1)
MetaCommunity(base,pesos)->aveMC
perfi <- DivProfile(seq(0, 2, 0.2), aveMC, Biased = FALSE)
library(ggplot2)
c(1.33,1.09,1.09)->beta
c("q0","q1","q2")->Orden2
c(1,2,3)->Orden3
data.frame(beta,Orden2,Orden3)->betaj
png(file="/home/fertimjim/Documentos/Fer/datos tesis/imagenes/perfilib.png",width=700, height=450,res=100)
par(mar=c(5,3,2,2)+0.1)
ggplot(betaj,aes(x=Orden2,y=beta))+geom_point()+geom_line(aes(x=Orden3))+theme(panel.background=element_blank(),axis.text.x=element_text(colour="black"),text=element_text(family="serif"),axis.text.y=element_text(colour="black"))->perfilib
perfilib+ xlab("Orden") +ylab("Número de comunidades")
dev.off()

## los intervalos de confianza al 84%
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


