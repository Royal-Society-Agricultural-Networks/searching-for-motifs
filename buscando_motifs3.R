setwd("C:\\Users\\User\\Dropbox\\posdoc RS\\planillas\\")
planilla<-read.table("tabla_total28f.csv", sep=",",header=T)

#by Field selection
by.field<-planilla[planilla$Field=="543",]###voy cambiando numero aca

#Select herbivoty + parasitoidism interations togheter
INT<-by.field[by.field$interaction_type=="herbivory" |  by.field$interaction_type=="parasitoidism",]
head(INT)

 
####Detecting soybean (pl1)-herbivore interactions
herbi.soja<-function(INT){
	level.superior<-NULL
	por.fila<-length(INT[,3])
	for(i in 1:por.fila){
		if (INT[i,"lower.taxon"]=="pl1"){
      	up.tax<- INT[i,"upper.taxon"]
		level.superior[i]<-as.character(up.tax)
		}else{
			level.superior[i]<-"no"
			}
	}
	return(level.superior)
}

herb_en_motif<-data.frame(herbi.soja(INT))
herb_en_motif2<-cbind(INT$lower.taxon, herb_en_motif,INT$weight)
herb_en_motif1<-data.frame(herb_en_motif2[herb_en_motif2$herbi.soja.INT != "no",])#me devuelve un subset de datos con las int
colnames(herb_en_motif1)<-c("low.level","high.level","weight")
herb_en_motif1 ##LEPI 1 ASOCIADOS A SOJA


####  
###herbiv soja (L1)-parasitoid
parasitoides<-function(INT,k){
	level.3<-NULL
	por.fila<-length(INT[,1])
	for(i in 1:por.fila){
		if (INT[i,"lower.taxon"]==as.character(k)){ 
    		up.tax<- INT[i,"upper.taxon"]
		level.3[i]<-as.character(up.tax)
			}else{
			level.3[i]<-"no"
			}
	}
	return(level.3)
}

#insects associated to soybean crop
amigos<-as.character(herb_en_motif1[,2])#insects participando de motifs

#function including parasitoides function (for each partner insect in soybea crop...)
total1<-function(INT,amigos){
	filas<-nrow(INT)
	columnas<-length(amigos)
	final<-matrix(,filas,columnas)
	uno.a.uno<-length(amigos)
	for (j in 1:uno.a.uno){ #para cada companero de soja
		k<-amigos[j]
		final[,j]<-parasitoides(INT,k)#busco sus plantas compa;eras de borde
	}
	return(final)
}

matriz.compas<-total1(INT,amigos)####PARASITOIDES ASOCIADOS A LEPI1
colnames(matriz.compas)=amigos
matriz.compas ###

amigos2<-as.vector(matriz.compas)
amigos2.1<-unique(amigos2[amigos2!="no"])

##para cada columna de matri.compas pegarle columna low y weght

L1.parasitoid<-NULL
aa<-ncol(matriz.compas)
for (i in 1:aa){
bb<-cbind(as.character(INT[,"lower.taxon"]),matriz.compas[,i],INT[,"weight"])
L1.para<-data.frame(bb)
colnames(L1.para)<-c("low.level","high.level","weight")
L1.para1<-data.frame(L1.para[L1.para[,2] != "no",])#me devuelve un subset de datos con las int
L1.parasitoid[[i]]<-L1.para1
}
L1.parasitoid

###lepidopterans-parasitoid
herb.borde<-function(INT,k){
	level.2borde<-NULL
	por.fila<-length(INT[,1])
	for(i in 1:por.fila){
		if (INT[i,"upper.taxon"]==as.character(k)){ 
    		low.tax<- INT[i,"lower.taxon"]
		level.2borde[i]<-as.character(low.tax)
			}else{
			level.2borde[i]<-"no"
			}
	}
	return(level.2borde)
}



#function including planta.edge function (for each partner insect in soybea crop...)
total1<-function(INT,amigos2.1){
	filas<-nrow(INT)
	columnas<-length(amigos2.1)
	final<-matrix(,filas,columnas)
	uno.a.uno<-length(amigos2.1)
	for (j in 1:uno.a.uno){ #para cada companero de soja
		k<-amigos2.1[j]
		final[,j]<-herb.borde(INT,k)#busco sus plantas compa;eras de borde
	}
	return(final)
}

matriz.compas2<-total1(INT,amigos2.1)
colnames(matriz.compas2)=amigos2.1
matriz.compas2####LEPI BORDE ASOCIADOS A PARASITOIDES

amigos3<-as.vector(matriz.compas2)
amigos3.1<-unique(amigos3[amigos3!="no"])


Larvas.parasitoids<-NULL
bb<-ncol(matriz.compas2)
for (i in 1:bb){
l.p<-cbind(matriz.compas2[,i],as.character(INT[,"upper.taxon"]),INT[,"weight"])
Ls.paras<-data.frame(l.p)
colnames(Ls.paras)<-c("low.level","high.level","weight")
Ls.paras1<-data.frame(Ls.paras[Ls.paras[,1] != "no",])#me devuelve un subset de datos con las int
Larvas.parasitoids[[i]]<-Ls.paras1
}

Larvas.parasitoids

Larvas.parasitoids.final<-NULL
elementos<-length(Larvas.parasitoids)
	for (i in 1:elementos){	
		if (nrow(Larvas.parasitoids[[i]])>1){
		Larvas.parasitoids.final[[i]]=Larvas.parasitoids[[i]]
		}
	}

Larvas.parasitoids.final


#########################

###plants-lepidopterans
planta.borde<-function(INT,k){
	level.1borde<-NULL
	por.fila<-length(INT[,1])
	for(i in 1:por.fila){
		if (INT[i,"upper.taxon"]==as.character(k)){ 
    		low.tax<- INT[i,"lower.taxon"]
		level.1borde[i]<-as.character(low.tax)
			}else{
			level.1borde[i]<-"no"
			}
	}
	return(level.1borde)
}


#function including planta.edge function (for each partner insect in soybea crop...)
total1<-function(INT,amigos3.1){
	filas<-nrow(INT)
	columnas<-length(amigos3.1)
	final<-matrix(,filas,columnas)
	uno.a.uno<-length(amigos3.1)
	for (j in 1:uno.a.uno){ #para cada companero de soja
		k<-amigos3.1[j]
		final[,j]<-planta.borde(INT,k)#busco sus plantas compa;eras de borde
	}
	return(final)
}

matriz.compas3<-total1(INT,amigos3.1)###PLANTAS BORDES ASOCIADAS A LEPI2
colnames(matriz.compas3)=amigos3.1

matriz.compas3

###HELP HERE!!! I need to take each column of "matriz.compas3" and search for "pl1". If pl1=T take this column again
#and change all cells differents to "pl1" by "no". This works but stop in the first "pl1"

columnas<-ncol(matriz.compas3) 
filas<-nrow(matriz.compas3)
matriz.compas4<-matrix(0,filas,columnas)
for (i in 1:columnas){
	for(j in 1:filas){
		if (as.character(matriz.compas3[j,i])=="pl1"){ #if "pl1" is true enter here
			for(k in 1:filas){ ####I think that the problem is here. I need take a column with "pl1" and change !="pl1" by "no"
				if (as.character(matriz.compas3[k,i])=="pl1"){ 
				matriz.compas4[k,i]<-"soja"  #in the final version="pl1", I change names to check
				}else{
				matriz.compas4[k,i]<-"no es"#in the final version="no"
				}
			}
		}else{
			matriz.compas4[j,i]<-"idem" #in the final version=as.character(matriz.compas3[j,i])		
		}
	}
}


pl_borde.larvas<-NULL
dd<-ncol(matriz.compas4)
for (i in 1:dd){
pl.larv<-cbind(matriz.compas4[,i],as.character(INT[,"upper.taxon"]),INT[,"weight"])
pl.larv1<-data.frame(pl.larv)
colnames(pl.larv1)<-c("low.level","high.level","weight")
pl.larv2<-data.frame(pl.larv1[pl.larv1[,1] != "no",])#me devuelve un subset de datos con las int
pl_borde.larvas[[i]]<-pl.larv2
}
pl_borde.larvas

final_motifs<-c(pl_borde.larvas,Larvas.parasitoids.final)

############################################
###NO NO NO
####Filtered list of interactions participating of motifs
#ultimo1<-function(matriz.compas2,INT){
#	tabla.final<-NULL ###si en lugar de list me largaria una tablaaaaaa
#	n.columnas<-ncol(matriz.compas2)
#	for (i in 1:n.columnas){
#		seleccionada<-data.frame(matriz.compas2[,i])
#		subgrupo<-cbind(seleccionada,as.character(INT$upper.taxon),INT$weight)	
#		subgrupo1<-subgrupo[subgrupo[,1] != "no",]
#		colnames(subgrupo1)<-c("low.level","high.level","weight")
#		tabla.final[[i]]<-subgrupo1
#	}
#	return(tabla.final)
#}
#listado_motifs1<-ultimo1(matriz.compas2,INT)

#listado_motifs2<-NULL
#elementos<-length(listado_motifs1)
#	for (i in 1:elementos){	
#		if (nrow(listado_motifs1[[i]])>1){
#		listado_motifs2[[i]]=listado_motifs1[[i]]
#		}
#	}

#listado_motifs2



####Filtered list of interactions participating of motifs
#ultimo2<-function(matriz.compas3,INT){
#	tabla.final<-NULL ###si en lugar de list me largaria una tablaaaaaa
#	n.columnas<-ncol(matriz.compas3)
#	for (i in 1:n.columnas){
#		seleccionada<-data.frame(matriz.compas3[,i])
#		subgrupo<-cbind(seleccionada,as.character(INT$upper.taxon),INT$weight)	
#		subgrupo1<-subgrupo[subgrupo[,1] != "no",]
#		colnames(subgrupo1)<-c("low.level","high.level","weight")
#		tabla.final[[i]]<-subgrupo1
#	}
#	return(tabla.final)
#}
#listado_motifs3<-ultimo2(matriz.compas3,INT)

#final_motifs<-c(listado_motifs2,listado_motifs3)
#final_motifs
#####tengo q descartar pl2 lepi1... osea lepi que se asocian a soja pero no a para




