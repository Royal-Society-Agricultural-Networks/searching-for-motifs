'''
Using motifs in ecological networks to identify the role of plants in crop margins for multiple ecosystem service provision
Julia Tavella*, Fredric M. Windsor, Débora C. Rother, Darren M. Evans, Paulo R. Guimarães Jr., Tania P. Palacios, Marcelo Lois,  Mariano Devoto
*juliatavella@gmail.com

New version for tripartite motifs October 2022

'''
#Preparing data
#Key species in motifs
#INPUT
planilla<-read.table("definitiva_ordenada.csv", sep=",",header=T)
'''
Pre: list of interactions. indicate lower level species in column 'From' and higher level in 'to'
Example:
Field	from	to	guild1	guild2	interaction_type	weight
536	Carduus_acanthoides	Apis_mellifera	plant	pollinator	pollination	2
536	Glycine_max	Palpada_34	plant	pollinator	pollination	2
536	Glycine_max	Rachiplusia_nu	plant	lepidoptera	herbivory	5
536	H1_H5_H11	Achyra_bifidalis	plant	lepidoptera	herbivory	15
536	H1_H5_H11	Lepi_18	plant	lepidoptera	herbivory	2
536	H1_H5_H11	Tatochila_sp	plant	lepidoptera	herbivory	1

Pos: a list of plant-insect interactions contained in motifs
'''

###########################################
####***MOTIFS IN TRIPARTITE NETWORKS***####
###########################################
#to study parasitism motifs


## A function to detect the apparent competition (ac) between herbivores of in- and off-crop plants
trip.motif.detector_ac <- function(edgelist,crop.plant){
	#"""
	# A function to detect the apparent competition (ac) between herbivores of in- and off-crop plants
	#input=edgelist containing interactions, and the crop plant/
	#output= a list of species and interactions that participate in motifs
	#"""
	## 1. Detect the herbivores interacting with the crop plants of interest
	herb_en_motif <- NULL
	  for (i in 1:length(crop.plant)){
		herb_en_motif[[i]] <- edgelist[edgelist$lower.taxon == crop.plant[i],] # Hebivores interacting with the crop plants of interest
	  }
	  herbivores_mat <- as.data.frame(do.call(rbind, herb_en_motif)) # Bind the lists of interactions together
	  herbivores <- unique(herbivores_mat[,3]) # Names of the upper taxon (the crop plant herbivores)
	  if (length(herbivores)==0){
		return("no herbivores in crop")
		}else{
	  
	## 2. Detect the parasitoids of the herbivores on the crop plants of interest
    para_en_motif <- NULL
    for (j in 1:length(herbivores)){ 
    para_en_motif[[j]] <- edgelist[edgelist$lower.taxon == herbivores[j],]
    }
	}
	parasitoids_mat <- as.data.frame(do.call(rbind, para_en_motif)) # Bind the lists of interactions together
	parasitoids <- unique(parasitoids_mat[,3]) # Names of the upper taxon (the parasitoids of the herbivores)
	herbivoros.fram<- unique(parasitoids_mat[,2])
	
	if (length(parasitoids)==0){
		return("no natural enemies in the crop")
		}else{
		
		## 3. Detect the shared herbivores of the parasitoids of the herbivores feeding on crop plants
		shared_herb_en_motif <- NULL
		for (k in 1:length(parasitoids)){ 
		shared_herb_en_motif[[k]] <- edgelist[edgelist$upper.taxon == parasitoids[k],]
		}
		}
		shared.herbivores_mat <- as.data.frame(do.call(rbind, shared_herb_en_motif)) # Bind the lists of interactions together
		shared.herbivores <- unique(shared.herbivores_mat[,2]) # Names of the lower taxon (the shared herbivores of the parasitoids of the crop plant herbivores)
		alternative.herbivores_mat<-shared.herbivores_mat[!shared.herbivores_mat[,2] %in% c(herbivoros.fram), ]
		alternative.herbivores<-unique(alternative.herbivores_mat[,2])
		
		if(length(alternative.herbivores)==0){
		return ("no herbivores sharing natural enemies with crop pests")
			} else {  
			
			## 4. Detect the shared plants of herbivores of the parasitoids of the herbivores feeding on crop plants
			shared.host.plants_en_motif <- NULL
			for (l in 1:length(alternative.herbivores)){ 
			shared.host.plants_en_motif[[l]] <- edgelist[edgelist$upper.taxon == alternative.herbivores[l],]# BUG aca--falta la excepcion
			} 
			}
			shared.host.plants_mat <- as.data.frame(do.call(rbind, shared.host.plants_en_motif)) # Bind the lists of interactions together
			shared.host.plants <- unique(shared.host.plants_mat[,2]) # Names of the lower taxon (the shared host plants of the shared herbivores of the parasitoids of the crop plant herbivores)
		
			
			if(length(shared.host.plants)==0){
			return ("no alternative host plants")
			}else{
				
				 ## 5. Collate all of the interactions taking part in the motifs
				 motif_interactions_part2<-rbind(shared.host.plants_mat,alternative.herbivores_mat)
				 shared.parasitoids=unique(shared.herbivores_mat[shared.herbivores_mat[,2]%in% c(alternative.herbivores),][,3])
				 herbivore.paras.int=parasitoids_mat[parasitoids_mat[,3]%in% c(shared.parasitoids),]
				 motif_interactions_part1<-rbind(herbivore.paras.int,herbivores_mat[herbivores_mat[,3] %in% herbivore.paras.int[,2],])
				 #report results 
				 return( rbind(motif_interactions_part1,motif_interactions_part2))
				 }
	}
				 
				 
##searching motifs at each site				 

motifs_trip=list()
field=unique(planilla$Field)
for (z in 1:length(field)){
	##Select by field
	by.field<-planilla[planilla$Field== field[z],]###voy cambiando numero aca537 543
	#Select herbivoty + parasitoidism interations togheter
	INT<-by.field[by.field$interaction_type=="herbivory" |  by.field$interaction_type=="parasitoidism",]
	colnames(INT)=c("Field","lower.taxon","upper.taxon","guild1","guild2","interaction_type","weight")
	motifs_trip[[z]]=trip.motif.detector_ac(edgelist=INT,crop.plant="Glycine_max")
	}
					 
					 
