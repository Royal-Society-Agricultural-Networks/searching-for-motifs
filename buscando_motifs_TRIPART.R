## A function to detect the apparent competition (ac) between herbivores of in- and off-crop plants
motif.detector_ac <- function(edgelist,crop.plant){
  
  ## 1. Detect the herbivores interacting with the crop plants of interest
  herb_en_motif <- NULL
  for (i in 1:length(crop.plant)){
    herb_en_motif[[i]] <- edgelist[edgelist$lower.taxon == crop.plant[i],] # Hebivores interacting with the crop plants of interest
  }
  herbivores_mat <- as.matrix(do.call(rbind, herb_en_motif)) # Bind the lists of interactions together
  herbivores <- unique(herbivores_mat[,3]) # Names of the upper taxon (the crop plant herbivores)
  
  
  ## 2. Detect the parasitoids of the herbivores on the crop plants of interest
  para_en_motif <- NULL
  for (j in 1:length(herbivores)){ 
    para_en_motif[[j]] <- edgelist[edgelist$lower.taxon == herbivores[j],]
  }
  parasitoids_mat <- as.matrix(do.call(rbind, para_en_motif)) # Bind the lists of interactions together
  parasitoids <- unique(parasitoids_mat[,3]) # Names of the upper taxon (the parasitoids of the herbivores)
  
  
  ## 3. Detect the shared herbivores of the parasitoids of the herbivores feeding on crop plants
  shared_herb_en_motif <- NULL
  for (k in 1:length(parasitoids)){ 
    shared_herb_en_motif[[k]] <- edgelist[edgelist$upper.taxon == parasitoids[k],]
  }
  shared.herbivores_mat <- as.matrix(do.call(rbind, shared_herb_en_motif)) # Bind the lists of interactions together
  shared.herbivores <- unique(shared.herbivores_mat[,2]) # Names of the lower taxon (the shared herbivores of the parasitoids of the crop plant herbivores)
  
  
  ## 4. Detect the shared plants of herbivores of the parasitoids of the herbivores feeding on crop plants
  shared.host.plants_en_motif <- NULL
  for (l in 1:length(shared.herbivores)){ 
    shared.host.plants_en_motif[[l]] <- edgelist[edgelist$upper.taxon == shared.herbivores[l],]
  }
  shared.host.plants_mat <- as.matrix(do.call(rbind, shared.host.plants_en_motif)) # Bind the lists of interactions together
  shared.host.plants <- unique(shared.host.plants_mat[,2]) # Names of the lower taxon (the shared host plants of the shared herbivores of the parasitoids of the crop plant herbivores)
  
  ## 5. Collate all of the interactions taking part in the motifs
  motif_interactions <- rbind(herbivores_mat, parasitoids_mat, shared.herbivores_mat, shared.host.plants_mat)
  motif_interactions <- distinct(data.frame(motif_interactions))
  
  ## 6. Remove non-motif interactions with shared herbivore parasitoids
  # Remove all parasitoids that only occur once in the shared herbivores matrix.... 
  
  ## 7. Report the results of the search
  return(motif_interactions)
  
}
