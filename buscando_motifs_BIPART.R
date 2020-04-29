####***MOTIFS IN BIPARTITE NETWORKS***####

setwd("C:\\Users\\User\\Dropbox\\posdoc RS\\planillas\\")
planilla<-read.table("tabla_total28f.csv", sep=",",header=T)

#Selection by Field
by.field<-planilla[planilla$Field=="536",]###voy cambiando numero aca

#Selection of interaction type   #herbivory or pollination
INT<-by.field<-by.field[by.field$interaction_type=="pollination",]
head(INT)


####Interactions soybean-insects
polin<-function(INT){
			level.superior<-NULL
			for(i in 1:length(INT[,3])){
					if (INT[i,"lower.taxon"]=="pl1"){
      				up.tax<- INT[i,"upper.taxon"]
					level.superior[i]<-as.character(up.tax)
					}else{
						level.superior[i]<-"no"
						}
			}
			return(level.superior)
}

poli_en_motif<-data.frame(polin(INT))
poli_en_motif2<-cbind(INT$lower.taxon, poli_en_motif,INT$weight)
poli_en_motif1<-data.frame(poli_en_motif2[poli_en_motif2$polin.INT != "no",])#me devuelve un subset de datos con las int
colnames(poli_en_motif1)<-c("low.level","high.level","weight")
poli_en_motif1


####Detecting plants from the edges that are participating on motif.  
###Insect-plant from the edge
planta.edge<-function(INT,k){
	level.inferior<-NULL
	for(i in 1:length(INT[,1])){
		if (INT[i,"upper.taxon"]==as.character(k)){ 
    		low.tax<- INT[i,"lower.taxon"]
		level.inferior[i]<-as.character(low.tax)
			}else{
			level.inferior[i]<-"no"
			}
	}
	return(level.inferior)
}

#insects associated to soybean crop
amigos<-as.character(poli_en_motif1[,2])#insects participando de motifs

#function including planta.edge function (for each partner insect in soybea crop...)
total1<-function(INT,amigos){
	final<-matrix(NA,nrow(INT),length(amigos))
	for (j in 1:length(amigos)){ #para cada companero de soja
		k<-amigos[j]
		final[,j]<-planta.edge(INT,k)#busco sus plantas compa;eras de borde
	}
	return(final)
}

matriz.compas<-total1(INT,amigos)
colnames(matriz.compas)=amigos

####Filtered list of interactions participating of motifs
ultimo<-function(matriz.compas,INT){
	tabla.final<-NULL ###si en lugar de list me largaria una tablaaaaaa
	n.columnas<-ncol(matriz.compas)
	for (i in 1:n.columnas){
		seleccionada<-data.frame(matriz.compas[,i])
		subgrupo<-cbind(seleccionada,as.character(INT$upper.taxon),INT$weight)	
		subgrupo1<-subgrupo[subgrupo[,1] != "no",]
		colnames(subgrupo1)<-c("low.level","high.level","weight")
		tabla.final[[i]]<-subgrupo1
	}
	return(tabla.final)
}
listado_motifs<-ultimo(matriz.compas,INT)

listado_motifs2<-NULL
elementos<-length(listado_motifs)
	for (i in 1:elementos){	
		if (nrow(listado_motifs[[i]])>1){
		listado_motifs2[[i]]=listado_motifs[[i]]
		}
	}

listado_motifs2_test <- as.matrix(do.call(rbind, listado_motifs2)) # this merges the separate edgelists into one

####"listado_motifs2" is a list of interactions participating in motifs 
####if output is "NULL", NO MOTIF FOUND

####**HELP HERE!!**####
#Ideally, I need a table or matrix as output, but I'm obtainig a list. How can I do this?
#I tried a few options, but couldn't find a solution

##para contar num de motifs tengo q contar las pl != de pl1
#################################################################




