setwd()

#Read in the data
planilla<-read.table("tabla_total28f.csv", sep=",",header=T)

#Selection by Field
by.field <- planilla %>% filter(Field == "536")

#Selection of interaction type 'herbivory' or 'pollination'
INT <- by.field %>% filter(interaction_type == "pollination")

#Select the pollinators interacting with soy plants (pl1)
poli_en_motif <- INT %>% filter(lower.taxon == "pl1")
poli_en_motif <- poli_en_motif[c(2,3,7)]
colnames(poli_en_motif)<-c("low.level","high.level","weight")
poli_amigos <- poli_en_motif[,2]

#Select the other plants interacting with the same pollinators interacting with soy ('pl1')
listado_motif <- INT %>% filter(upper.taxon == poli_amigos[1] | upper.taxon == poli_amigos[2] | upper.taxon == poli_amigos[3])