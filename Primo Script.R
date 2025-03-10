#-------------------------------------------IMPOSTAZIONE DIRECTORY & CARICAMENTO LIBRERIE-------------------------------------------------#

path<-'C:/Users/EXT2061529/OneDrive - Eni/Desktop/Attività di tirocinio-tesi/Codice R'											#il percorso della directory
setwd(path)																								#setta la directory

library(readxl)																							# Libreria per leggere i file excel
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)
#-------------------------------------------IMPOSTAZIONE DIRECTORY & CARICAMENTO LIBRERIE-------------------------------------------------#



#----------------------------------------------------INIZIALIZZAZIONE DATASET-------------------------------------------------------------#
		   
FileName <- 'Database Molten Salts corrosion tests.xlsx'																# Il nome del file excel da caricare
SheetNr <- 5																							# Il numero della pagina da caricare del file excel
CorrData <- as.data.frame(read_excel(FileName, sheet = 5))																		# Carica il file excel
CorrDataRowNames <- CorrData[,1]
CorrData <- CorrData[-c(1)]																					# La prima colonna del dataset, contenente i nomi delle righe, viene ora eliminata in quanto � una ripetizione
rownames(CorrData) <- CorrDataRowNames																			# Imposta il nome delle righe del file caricato, dandogli lo stesso nome del file excel

#----------------------------------------------------INIZIALIZZAZIONE DATASET-------------------------------------------------------------#



#--------------------------------------------------SCELTA VARIABILI DISCRIMINANTI---------------------------------------------------------#

Molten_Salt_Types <- table(CorrData[13])																			# Crea una tabella in cui sono riportate le numerosit� di tutti i sali fusi del dataset

Exp_Velocities <- table(CorrData[1])																			# Crea una tabella in cui sono riportate le numerosit� di tutte le velocit� sperimentali del database

Exp_Temperatures <- table(CorrData[2])																			# Crea una tabella in cui sono riportate le numerosit� di tutte le temperature sperimentali del database

Exp_Dive_Times <- table(CorrData[3])																			# Crea una tabella in cui sono riportate le numerosit� di tutti i dive time sperimentali del database

Exp_Radiations <- table(CorrData[37])																			# Crea una tabella in cui sono riportate le numerosit� di tutte le temperature sperimentali del database

Material_Types <- table(CorrData[5])																			# Crea una tabella in cui sono riportate le numerosit� di tutti i materiali del dataset

Crucible_Types <- table(CorrData[4])																			# Crea una tabella in cui sono riportate le numerosit� di tutti i materiali dei crucible del database

for (x in 1:length(CorrData[,14])){																				# Nel dataset originale la feature "is purified?" � carattterizzata da valori "yes/no", preferisco che siano "Purified/Unpurified"
	if(CorrData[x,14] == "yes"){
		CorrData[x,14] <- "Purified"
	} else {
		CorrData[x,14] <- "Unpurified"	
	}
}
Purity <- table(CorrData[14])																					# Crea una tabella in cui sono riportate le numerosit� di tutti i sali se purificati o non purificati

#--------------------------------------------------SCELTA VARIABILI DISCRIMINANTI---------------------------------------------------------#



#---------------------------------------------------------ESTRAZIONE DATI-----------------------------------------------------------------#

		#------------ESTRAZIONE DATI TEMPERATURA----------------

Temperature <- data.frame(CorrData[2])																			# Estrazione dati sulle Temperature per un dato materiale a una data temperatura
names(Temperature) <- "Temperature"																				# Ribattezzo il nome della colonna del vettore
rownames(Temperature) <- rownames(CorrData)																		# Ribatezzo i nomi delle righe del vettore per essere uguali ai nomi delle righe del dataset di partenza

				#------VS MATERIAL
Temperature_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))								# Inizializza una matrice in cui le colonne sono le varie temperature per ogni materiale

for (x in 1:length(Material_Types)){																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	Temperature_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),2]			# la colonna x di Temperature_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x),
}																									# � uguale ai valori presenti nel dataset nella colonna delle temperature, corrispondenti al materiale x
colnames(Temperature_VS_Material) <- rownames(Material_Types)															# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
Temperature_VS_Material[Temperature_VS_Material==0] <- NA																# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Temperature_VS_Material <- Temperature_VS_Material[,order(colSums(!is.na(Temperature_VS_Material)), decreasing=TRUE)]						# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Temperature_VS_Material <- Temperature_VS_Material[,1:sum(!is.na(Temperature_VS_Material[1,]))]										# elimino le colonne che contengono solo "NA"
				#------VS MATERIAL

				#------VS MOLTEN SALT
Temperature_VS_Molten_Salt <- matrix(0,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types))							# Inizializza una matrice in cui le colonne sono le varie temperature per ogni sale fuso

for (x in 1:length(Molten_Salt_Types)){																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	Temperature_VS_Molten_Salt[c(1:as.numeric(Molten_Salt_Types[x])),x] <- CorrData[c(which(CorrData[,13]==rownames(Molten_Salt_Types)[x])),2]	# la colonna x di Temperature_VS_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x),
}																									# � uguale ai valori presenti nel dataset nella colonna delle temperature, corrispondenti al sale fuso x
colnames(Temperature_VS_Molten_Salt) <- rownames(Molten_Salt_Types)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso
Temperature_VS_Molten_Salt[Temperature_VS_Molten_Salt==0] <- NA															# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Temperature_VS_Molten_Salt <- Temperature_VS_Molten_Salt[,order(colSums(!is.na(Temperature_VS_Molten_Salt)), decreasing=TRUE)]				# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Temperature_VS_Molten_Salt <- Temperature_VS_Molten_Salt[,1:sum(!is.na(Temperature_VS_Molten_Salt[1,]))]								# elimino le colonne che contengono solo "NA"
				#------VS MOLTEN SALT

				#------VS (UN)PURIFIED MOLTEN SALT
Temperature_VS_Purified_Molten_Salt <- matrix(0,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types)*length(Purity))			# Inizializza una matrice in cui le colonne sono le varie temperature per ogni sale fuso purificato E non
Purified_Molten_Salts <- c(1:length(Molten_Salt_Types)*length(Purity))														# Inizializza un vettore di stringhe che specificheranno il tipo di sale purificato o non

z <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Temperature_VS_Purified_Molten_Salt
for (x in 1:length(Molten_Salt_Types)) {																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	for (y in 1:length(Purity)) {																				# Per y che va da 1 al 2 (sale purificato o no)
		z <- z+1																						# Incremento di z (z = 1:6 (6=3x2 (3= n�Sali, 2= purificato/non)))
		Purified_Molten_Salts[z] <- paste(rownames(Purity)[y],rownames(Molten_Salt_Types)[x])									# Inserisce nel vettore Purified_Molten_Salts la stringa che descrive il tipo di sale e la sua purificazione
		if (length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]==rownames(Molten_Salt_Types)[x])))>0){						# Se sono presenti dati per il sale fuso x, nelle condizioni di purificazione y
			Temperature_VS_Purified_Molten_Salt[1:length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]						# la colonna z di Temperature_VS_Purified_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x nello stato di purificazione y),
			==rownames(Molten_Salt_Types)[x]))),z] <- CorrData[c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]					# � uguale ai valori presenti nel dataset nella colonna delle temperature, corrispondenti al sale fuso x nelle condizioni di purificazione y
			==rownames(Molten_Salt_Types)[x])),2]
			
		}
	}
}
colnames(Temperature_VS_Purified_Molten_Salt) <- Purified_Molten_Salts														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso ed uno stato di purificazione
Temperature_VS_Purified_Molten_Salt <- Temperature_VS_Purified_Molten_Salt[1:max(colSums(matrix(Temperature_VS_Purified_Molten_Salt>0,			# Elimina le righe della matrice Temperature_VS_Purified_Molten_Salt contenenti solo 0 (Creo una matrice di valori logici (1 se valore della matrice>0, 0 altrimenti), poi
nrow=nrow(Temperature_VS_Purified_Molten_Salt),ncol=ncol(Temperature_VS_Purified_Molten_Salt)))),]									# faccio la somma delle colonne, cos� so quale colonna contiene pi� valori diversi da 0. scegliendo il valore massimo tra le somme cancello tutte le righe sottostanti)
Temperature_VS_Purified_Molten_Salt[Temperature_VS_Purified_Molten_Salt==0] <- NA												# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Temperature_VS_Purified_Molten_Salt <- Temperature_VS_Purified_Molten_Salt[,order(colSums(!is.na(Temperature_VS_Purified_Molten_Salt)),			# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
 decreasing=TRUE)]				
Temperature_VS_Purified_Molten_Salt <- Temperature_VS_Purified_Molten_Salt[,1:sum(!is.na(Temperature_VS_Purified_Molten_Salt[1,]))]				# elimino le colonne che contengono solo "NA"


rm(z)																									# Rimozione variabile inutile
rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile
				#------VS (UN)PURIFIED MOLTEN SALT

				#------VS DIVE TIME
Temperature_VS_Exp_Dive_Times <- matrix(0,nrow=max(as.numeric(Exp_Dive_Times)), ncol=length(Exp_Dive_Times))							# Inizializza una matrice in cui le colonne sono le varie temperature per ogni dive time

for (x in 1:length(Exp_Dive_Times)){																			# Per x che va da 1 al numero di dive time diversi presenti nel dataset
	Temperature_VS_Exp_Dive_Times[c(1:as.numeric(Exp_Dive_Times[x])),x] <- CorrData[c(which(CorrData[,3]==rownames(Exp_Dive_Times)[x])),2]		# la colonna x di Temperature_VS_Exp_Dive_Time (dalla prima riga alla riga corrispondente la numerosit� del dive time x),
}																									# � uguale ai valori presenti nel dataset nella colonna delle temperature, corrispondenti al dive time x
colnames(Temperature_VS_Exp_Dive_Times) <- rownames(Exp_Dive_Times)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un dive time
Temperature_VS_Exp_Dive_Times[Temperature_VS_Exp_Dive_Times==0] <- NA														# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Temperature_VS_Exp_Dive_Times <- Temperature_VS_Exp_Dive_Times[,order(colSums(!is.na(Temperature_VS_Exp_Dive_Times)), decreasing=TRUE)]			# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
				#------VS DIVE TIME

	#----------------------QUARTILI MEDIA E MODA------------------------------------------------
Temperature_Quartiles <- quantile(Temperature[,])
Temperature_Mean <- mean(Temperature[,])
Temperature_Mode <- as.numeric(names(sort(-Exp_Temperatures))[1])

Temperature_Stats <- t(c(sum(!is.na(Temperature)),Temperature_Quartiles,Temperature_Mean,Temperature_Mode))
names(Temperature_Stats) <- c("N� obs.",paste(names(Temperature_Quartiles),"quantile"),"Temperature Mean","Temperature Mode")

				#------VS MATERIAL
Temperature_VS_Material_N_Data <-colSums(!is.na(Temperature_VS_Material))
Temperature_VS_Material_Quartiles<-matrix(NA,nrow=ncol(Temperature_VS_Material),ncol=length(seq(0, 1, 0.25)))
Temperature_VS_Material_Mean<-matrix(NA,nrow=ncol(Temperature_VS_Material),ncol=1)
Temperature_VS_Material_Mode<-matrix(NA,nrow=ncol(Temperature_VS_Material),ncol=1)
for(x in 1:ncol(Temperature_VS_Material[,])){
	Temperature_VS_Material_Quartiles[x,]<-quantile(Temperature_VS_Material[,x],probs = seq(0, 1, 0.25), na.rm = TRUE)
	Temperature_VS_Material_Mean[x,] <- mean(Temperature_VS_Material[!is.na(Temperature_VS_Material[,x]),x])
	Temperature_VS_Material_Mode[x,] <- as.numeric(names(sort(-table(Temperature_VS_Material[,x])))[1])
}
Temperature_VS_Material_Stats<-cbind(Temperature_VS_Material_N_Data,Temperature_VS_Material_Quartiles,Temperature_VS_Material_Mean,Temperature_VS_Material_Mode)
colnames(Temperature_VS_Material_Stats) <- c("N� obs.",paste(as.character(seq(0, 1, 0.25)*100),"% quantile"),"Mean","Mode")
rownames(Temperature_VS_Material_Stats) <- colnames(Temperature_VS_Material)
				#------VS MATERIAL

				#------VS  (UN) PURIFIED MOLTEN SALTS
Temperature_VS_Purified_Molten_Salt_N_Data <-colSums(!is.na(Temperature_VS_Purified_Molten_Salt))
Temperature_VS_Purified_Molten_Salt_Quartiles<-matrix(NA,nrow=ncol(Temperature_VS_Purified_Molten_Salt),ncol=length(seq(0, 1, 0.25)))
Temperature_VS_Purified_Molten_Salt_Mean<-matrix(NA,nrow=ncol(Temperature_VS_Purified_Molten_Salt),ncol=1)
Temperature_VS_Purified_Molten_Salt_Mode<-matrix(NA,nrow=ncol(Temperature_VS_Purified_Molten_Salt),ncol=1)
for(x in 1:ncol(Temperature_VS_Purified_Molten_Salt[,])){
	Temperature_VS_Purified_Molten_Salt_Quartiles[x,]<-quantile(Temperature_VS_Purified_Molten_Salt[,x],probs = seq(0, 1, 0.25), na.rm = TRUE)
	Temperature_VS_Purified_Molten_Salt_Mean[x,] <- mean(Temperature_VS_Purified_Molten_Salt[!is.na(Temperature_VS_Purified_Molten_Salt[,x]),x])
	Temperature_VS_Purified_Molten_Salt_Mode[x,] <- as.numeric(names(sort(-table(Temperature_VS_Purified_Molten_Salt[,x])))[1])
}
Temperature_VS_Purified_Molten_Salt_Stats<-cbind(Temperature_VS_Purified_Molten_Salt_N_Data,Temperature_VS_Purified_Molten_Salt_Quartiles,Temperature_VS_Purified_Molten_Salt_Mean,Temperature_VS_Purified_Molten_Salt_Mode)
colnames(Temperature_VS_Purified_Molten_Salt_Stats) <- c("N° obs.",paste(as.character(seq(0, 1, 0.25)*100),"% quantile"),"Mean","Mode")
rownames(Temperature_VS_Purified_Molten_Salt_Stats) <- colnames(Temperature_VS_Purified_Molten_Salt)
				#------VS (UN) PURIFIED MOLTEN SALTS

				#------VS DIVE TIME
Temperature_VS_Exp_Dive_Times_N_Data <-colSums(!is.na(Temperature_VS_Exp_Dive_Times))
Temperature_VS_Exp_Dive_Times_Quartiles<-matrix(NA,nrow=ncol(Temperature_VS_Exp_Dive_Times),ncol=length(seq(0, 1, 0.25)))
Temperature_VS_Exp_Dive_Times_Mean<-matrix(NA,nrow=ncol(Temperature_VS_Exp_Dive_Times),ncol=1)
Temperature_VS_Exp_Dive_Times_Mode<-matrix(NA,nrow=ncol(Temperature_VS_Exp_Dive_Times),ncol=1)
for(x in 1:ncol(Temperature_VS_Exp_Dive_Times[,])){
	Temperature_VS_Exp_Dive_Times_Quartiles[x,]<-quantile(Temperature_VS_Exp_Dive_Times[,x],probs = seq(0, 1, 0.25), na.rm = TRUE)
	Temperature_VS_Exp_Dive_Times_Mean[x,] <- mean(Temperature_VS_Exp_Dive_Times[!is.na(Temperature_VS_Exp_Dive_Times[,x]),x])
	Temperature_VS_Exp_Dive_Times_Mode[x,] <- as.numeric(names(sort(-table(Temperature_VS_Exp_Dive_Times[,x])))[1])
}
Temperature_VS_Exp_Dive_Times_Stats<-cbind(Temperature_VS_Exp_Dive_Times_N_Data,Temperature_VS_Exp_Dive_Times_Quartiles,Temperature_VS_Exp_Dive_Times_Mean,Temperature_VS_Exp_Dive_Times_Mode)
colnames(Temperature_VS_Exp_Dive_Times_Stats) <- c("N° obs.",paste(as.character(seq(0, 1, 0.25)*100),"% quantile"),"Mean","Mode")
rownames(Temperature_VS_Exp_Dive_Times_Stats) <- colnames(Temperature_VS_Exp_Dive_Times)
				#------VS DIVE TIME
	#----------------------QUARTILI MEDIA E MODA------------------------------------------------

		#------------ESTRAZIONE DATI TEMPERATURA----------------

		#-------------ESTRAZIONE DATI DIVE TIME-----------------

Dive_Time <- data.frame(CorrData[3])
rownames(Dive_Time) <- rownames(CorrData)																			# Ribatezzo i nomi delle righe del vettore per essere uguali ai nomi delle righe del dataset di partenza


				#------VS MATERIAL
Dive_Time_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))									# Inizializza una matrice in cui le colonne sono i vari dive time per ogni materiale

for (x in 1:length(Material_Types)){																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	Dive_Time_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),3]			# la colonna x di Dive_Time_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x),
}																									# � uguale ai valori presenti nel dataset nella colonna dei dive time, corrispondenti al materiale x
colnames(Dive_Time_VS_Material) <- rownames(Material_Types)																# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
Dive_Time_VS_Material[Dive_Time_VS_Material==0] <- NA																	# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Dive_Time_VS_Material <- Dive_Time_VS_Material[,order(colSums(!is.na(Dive_Time_VS_Material)), decreasing=TRUE)]							# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
				#------VS MATERIAL

				#------VS MOLTEN SALT
Dive_Time_VS_Molten_Salt <- matrix(0,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types))							# Inizializza una matrice in cui le colonne sono i vari dive time per ogni sale fuso

for (x in 1:length(Molten_Salt_Types)){																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	Dive_Time_VS_Molten_Salt[c(1:as.numeric(Molten_Salt_Types[x])),x] <- CorrData[c(which(CorrData[,13]==rownames(Molten_Salt_Types)[x])),3]		# la colonna x di Dive_Time_VS_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x),
}																									# � uguale ai valori presenti nel dataset nella colonna dei dive time, corrispondenti al sale fuso x
colnames(Dive_Time_VS_Molten_Salt) <- rownames(Molten_Salt_Types)															# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso
Dive_Time_VS_Molten_Salt[Dive_Time_VS_Molten_Salt==0] <- NA																# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Dive_Time_VS_Molten_Salt <- Dive_Time_VS_Molten_Salt[,order(colSums(!is.na(Dive_Time_VS_Molten_Salt)), decreasing=TRUE)]					# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
				#------VS MOLTEN SALT

				#------VS (UN)PURIFIED MOLTEN SALT
Dive_Time_VS_Purified_Molten_Salt <- matrix(0,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types)*length(Purity))			# Inizializza una matrice in cui le colonne sono i vari dive time per ogni sale fuso purificato E non
Purified_Molten_Salts <- c(1:length(Molten_Salt_Types)*length(Purity))														# Inizializza un vettore di stringhe che specificheranno il tipo di sale purificato o non

z <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Dive_Time_VS_Purified_Molten_Salt
for (x in 1:length(Molten_Salt_Types)) {																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	for (y in 1:length(Purity)) {																				# Per y che va da 1 al 2 (sale purificato o no)
		z <- z+1																						# Incremento di z (z = 1:6 (6=3x2 (3= n°Sali, 2= purificato/non)))
		Purified_Molten_Salts[z] <- paste(rownames(Purity)[y],rownames(Molten_Salt_Types)[x])									# Inserisce nel vettore Purified_Molten_Salts la stringa che descrive il tipo di sale e la sua purificazione
		if (length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]==rownames(Molten_Salt_Types)[x])))>0){						# Se sono presenti dati per il sale fuso x, nelle condizioni di purificazione y
			Dive_Time_VS_Purified_Molten_Salt[1:length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]						# la colonna z di Dive_Time_VS_Purified_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x nello stato di purificazione y),
			==rownames(Molten_Salt_Types)[x]))),z] <- CorrData[c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]					# � uguale ai valori presenti nel dataset nella colonna dei dive time, corrispondenti al sale fuso x nelle condizioni di purificazione y
			==rownames(Molten_Salt_Types)[x])),3]
			
		}
	}
}
colnames(Dive_Time_VS_Purified_Molten_Salt) <- Purified_Molten_Salts														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso ed uno stato di purificazione
Dive_Time_VS_Purified_Molten_Salt <- Dive_Time_VS_Purified_Molten_Salt[1:max(colSums(matrix(Dive_Time_VS_Purified_Molten_Salt>0,				# Elimina le righe della matrice Temperature_VS_Purified_Molten_Salt contenenti solo 0 (Creo una matrice di valori logici (1 se valore della matrice>0, 0 altrimenti), poi
nrow=nrow(Dive_Time_VS_Purified_Molten_Salt),ncol=ncol(Dive_Time_VS_Purified_Molten_Salt)))),]										# faccio la somma delle colonne, cos� so quale colonna contiene pi� valori diversi da 0. scegliendo il valore massimo tra le somme cancello tutte le righe sottostanti)
Dive_Time_VS_Purified_Molten_Salt[Dive_Time_VS_Purified_Molten_Salt==0] <- NA													# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Dive_Time_VS_Purified_Molten_Salt <- Dive_Time_VS_Purified_Molten_Salt[,order(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt)), decreasing=TRUE)]	# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente

rm(z)																									# Rimozione variabile inutile
rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile	
				#------VS (UN)PURIFIED MOLTEN SALT
				
				#------VS TEMPERATURES
Dive_Time_VS_Exp_Temperatures <- matrix(0,nrow=max(as.numeric(Exp_Temperatures)), ncol=length(Exp_Temperatures))							# Inizializza una matrice in cui le colonne sono i vari dive time per ogni temperatura sperimentale

for (x in 1:length(Exp_Temperatures)){																			# Per x che va da 1 al numero di temperature sperimentali diverse presenti nel dataset
	Dive_Time_VS_Exp_Temperatures[c(1:as.numeric(Exp_Temperatures[x])),x] <- CorrData[c(which(CorrData[,2]==rownames(Exp_Temperatures)[x])),3]	# la colonna x di Dive_Time_VS_Exp_Temperatures (dalla prima riga alla riga corrispondente la numerosit� della temperatura sperimentale x),
}																									# � uguale ai valori presenti nel dataset nella colonna dei dive time, corrispondenti alla temperatura sperimentale x
colnames(Dive_Time_VS_Exp_Temperatures) <- rownames(Exp_Temperatures)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad una temperatura sperimentale
Dive_Time_VS_Exp_Temperatures[Dive_Time_VS_Exp_Temperatures==0] <- NA														# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Dive_Time_VS_Exp_Temperatures <- Dive_Time_VS_Exp_Temperatures[,order(colSums(!is.na(Dive_Time_VS_Exp_Temperatures)), decreasing=TRUE)]			# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
				#------VS TEMPERATURES

	#----------------------QUARTILI MEDIA E MODA------------------------------------------------
Dive_Time_Quartiles <- quantile(Dive_Time[,])
Dive_Time_Mean <- mean(Dive_Time[,])
Dive_Time_Mode <- as.numeric(names(sort(-Exp_Dive_Times))[1])

Dive_Time_Stats <- t(c(sum(!is.na(Dive_Time)),Dive_Time_Quartiles,Dive_Time_Mean,Dive_Time_Mode))
names(Dive_Time_Stats) <- c("N° obs.",paste(names(Dive_Time_Quartiles),"quantile"),"Dive Time Mean","Dive Time Mode")

				#------VS MATERIAL
Dive_Time_VS_Material_N_Data <-colSums(!is.na(Dive_Time_VS_Material))
Dive_Time_VS_Material_Quartiles<-matrix(NA,nrow=ncol(Dive_Time_VS_Material),ncol=length(seq(0, 1, 0.25)))
Dive_Time_VS_Material_Mean<-matrix(NA,nrow=ncol(Dive_Time_VS_Material),ncol=1)
Dive_Time_VS_Material_Mode<-matrix(NA,nrow=ncol(Dive_Time_VS_Material),ncol=1)
for(x in 1:ncol(Dive_Time_VS_Material[,])){
	Dive_Time_VS_Material_Quartiles[x,]<-quantile(Dive_Time_VS_Material[,x],probs = seq(0, 1, 0.25), na.rm = TRUE)
	Dive_Time_VS_Material_Mean[x,] <- mean(Dive_Time_VS_Material[!is.na(Dive_Time_VS_Material[,x]),x])
	Dive_Time_VS_Material_Mode[x,] <- as.numeric(names(sort(-table(Dive_Time_VS_Material[,x])))[1])
}
Dive_Time_VS_Material_Stats<-cbind(Dive_Time_VS_Material_N_Data,Dive_Time_VS_Material_Quartiles,Dive_Time_VS_Material_Mean,Dive_Time_VS_Material_Mode)
colnames(Dive_Time_VS_Material_Stats) <- c("N° obs.",paste(as.character(seq(0, 1, 0.25)*100),"% quantile"),"Mean","Mode")
rownames(Dive_Time_VS_Material_Stats) <- colnames(Dive_Time_VS_Material)
				#------VS MATERIAL

				#------VS  (UN) PURIFIED MOLTEN SALTS
Dive_Time_VS_Purified_Molten_Salt_N_Data <-colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt))
Dive_Time_VS_Purified_Molten_Salt_Quartiles<-matrix(NA,nrow=ncol(Dive_Time_VS_Purified_Molten_Salt),ncol=length(seq(0, 1, 0.25)))
Dive_Time_VS_Purified_Molten_Salt_Mean<-matrix(NA,nrow=ncol(Dive_Time_VS_Purified_Molten_Salt),ncol=1)
Dive_Time_VS_Purified_Molten_Salt_Mode<-matrix(NA,nrow=ncol(Dive_Time_VS_Purified_Molten_Salt),ncol=1)
for(x in 1:ncol(Temperature_VS_Purified_Molten_Salt[,])){
	Dive_Time_VS_Purified_Molten_Salt_Quartiles[x,]<-quantile(Dive_Time_VS_Purified_Molten_Salt[,x],probs = seq(0, 1, 0.25), na.rm = TRUE)
	Dive_Time_VS_Purified_Molten_Salt_Mean[x,] <- mean(Dive_Time_VS_Purified_Molten_Salt[!is.na(Dive_Time_VS_Purified_Molten_Salt[,x]),x])
	Dive_Time_VS_Purified_Molten_Salt_Mode[x,] <- as.numeric(names(sort(-table(Dive_Time_VS_Purified_Molten_Salt[,x])))[1])
}
Dive_Time_VS_Purified_Molten_Salt_Stats<-cbind(Dive_Time_VS_Purified_Molten_Salt_N_Data,Dive_Time_VS_Purified_Molten_Salt_Quartiles,Dive_Time_VS_Purified_Molten_Salt_Mean,Dive_Time_VS_Purified_Molten_Salt_Mode)
colnames(Dive_Time_VS_Purified_Molten_Salt_Stats) <- c("N° obs.",paste(as.character(seq(0, 1, 0.25)*100),"% quantile"),"Mean","Mode")
rownames(Dive_Time_VS_Purified_Molten_Salt_Stats) <- colnames(Dive_Time_VS_Purified_Molten_Salt)
				#------VS (UN) PURIFIED MOLTEN SALTS

				#------VS TEMPERATURE
Dive_Time_VS_Exp_Temperatures_N_Data <-colSums(!is.na(Dive_Time_VS_Exp_Temperatures))
Dive_Time_VS_Exp_Temperatures_Quartiles<-matrix(NA,nrow=ncol(Dive_Time_VS_Exp_Temperatures),ncol=length(seq(0, 1, 0.25)))
Dive_Time_VS_Exp_Temperatures_Mean<-matrix(NA,nrow=ncol(Dive_Time_VS_Exp_Temperatures),ncol=1)
Dive_Time_VS_Exp_Temperatures_Mode<-matrix(NA,nrow=ncol(Dive_Time_VS_Exp_Temperatures),ncol=1)
for(x in 1:ncol(Dive_Time_VS_Exp_Temperatures[,])){
	Dive_Time_VS_Exp_Temperatures_Quartiles[x,]<-quantile(Dive_Time_VS_Exp_Temperatures[,x],probs = seq(0, 1, 0.25), na.rm = TRUE)
	Dive_Time_VS_Exp_Temperatures_Mean[x,] <- mean(Dive_Time_VS_Exp_Temperatures[!is.na(Dive_Time_VS_Exp_Temperatures[,x]),x])
	Dive_Time_VS_Exp_Temperatures_Mode[x,] <- as.numeric(names(sort(-table(Dive_Time_VS_Exp_Temperatures[,x])))[1])
}
Dive_Time_VS_Exp_Temperatures_Stats<-cbind(Dive_Time_VS_Exp_Temperatures_N_Data,Dive_Time_VS_Exp_Temperatures_Quartiles,Dive_Time_VS_Exp_Temperatures_Mean,Dive_Time_VS_Exp_Temperatures_Mode)
colnames(Dive_Time_VS_Exp_Temperatures_Stats) <- c("N° obs.",paste(as.character(seq(0, 1, 0.25)*100),"% quantile"),"Mean","Mode")
rownames(Dive_Time_VS_Exp_Temperatures_Stats) <- colnames(Dive_Time_VS_Exp_Temperatures)
				#------VS TEMPERATURE
	#----------------------QUARTILI MEDIA E MODA------------------------------------------------


		#-------------ESTRAZIONE DATI DIVE TIME-----------------

		#-------------ESTRAZIONE DATI IMPURITA'-----------------

CorrDataMod <- data.frame(CorrData)																				# Creazione dataset locale che verr� cancellato
CorrDataMod[is.na(CorrDataMod)] <- 0																			# Per calcolare le impurit� totali c'� bisogno di eliminare i "NA" dal datase
CorrDataMod1 <- cbind(CorrDataMod[,1:15],list(rowSums(CorrDataMod[,16:36])),CorrDataMod[37:ncol(CorrData)])								# Creazione variabile buffer
CorrDataMod <- CorrDataMod1																					# Si � creato con queste istruzioni una copia del dataset originale, in cui al posto dei dati sulle singole impurit� c'� una sola colonna con la somma di queste
CorrDataMod[CorrDataMod[,16]==0,16]<-NA
colnames(CorrDataMod)[16] <- "Total Impurities Pre"																	# Si assegna il nome "Total Impurities Pre" a tale colonna
rm(CorrDataMod1)																							# La variabile buffer pu� essere eliminata

Total_Impurities <- data.frame(CorrDataMod[c(which(!is.na(CorrDataMod[,16]))),16:17])											# Estrazione dati sulle Total_Impurities per un dato materiale a una data temperatura (estraggo anche una seconda colonna per preservare i nomi delle righe successivamente...)
names(Total_Impurities) <- "Total_Impurities"																		# Ribattezzo il nome della colonna del vettore
rownames(Total_Impurities) <- rownames(CorrData)[c(which(!is.na(CorrDataMod[,16])))]											# Ribatezzo i nomi delle righe del vettore per essere uguali ai nomi delle righe del dataset di partenza
Total_Impurities <- Total_Impurities[order(Total_Impurities[,1], decreasing = TRUE),]											# Ordino in ordine decrescente i valori delle total impurities
Total_Impurities <- Total_Impurities[,1,drop=FALSE]																	# elimino la seconda colonna, inutile, preservando i nomi delle righe, che corrispondono agli id degli esperimenti

				#------VS MOLTEN SALT
Total_Impurities_VS_Molten_Salt <- matrix(NA,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types))						# Inizializza una matrice in cui le colonne sono le varie impurit� per ogni sale fuso

for (x in 1:length(Molten_Salt_Types)){																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	Total_Impurities_VS_Molten_Salt[c(1:as.numeric(Molten_Salt_Types[x])),x] <- CorrDataMod[c(which(CorrDataMod[,13]==					# la colonna x di Total_Impurities_VS_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x),
	rownames(Molten_Salt_Types)[x])),16]																		# � uguale ai valori presenti nel dataset nella colonna delle total impurities, corrispondenti al sale fuso x
}
colnames(Total_Impurities_VS_Molten_Salt) <- rownames(Molten_Salt_Types)													# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso
Total_Impurities_VS_Molten_Salt <-  apply(Total_Impurities_VS_Molten_Salt, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))		# Ordina in ordine decrescente le singole colonne della matrice
Total_Impurities_VS_Molten_Salt <- Total_Impurities_VS_Molten_Salt[,order(colSums(!is.na(Total_Impurities_VS_Molten_Salt)), decreasing=TRUE)]		# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Total_Impurities_VS_Molten_Salt <- Total_Impurities_VS_Molten_Salt[1:sum(!is.na(Total_Impurities_VS_Molten_Salt[,1])),]						# elimino le righe che contengono solo "NA"
				#------VS MOLTEN SALT

				#------VS PURIFIED MOLTEN SALT
Total_Impurities_VS_Purified_Molten_Salt <- matrix(NA,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types)*length(Purity))		# Inizializza una matrice in cui le colonne sono le varie impurit� totali per ogni sale fuso purificato E non
Purified_Molten_Salts <- c(1:length(Molten_Salt_Types)*length(Purity))														# Inizializza un vettore di stringhe che specificheranno il tipo di sale purificato o non

z <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Total_Impurities_VS_Purified_Molten_Salt
max <- 0																								# Variabile ausiliaria, che servir� per tenere traccia di quale tipo di sale ha maggiore numerosit� di dati di impurit� totali
for (x in 1:length(Molten_Salt_Types)) {																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	for (y in 1:length(Purity)) {																				# Per y che va da 1 al 2 (sale purificato o no)
		z <- z+1																						# Incremento di z (z = 1:6 (6=3x2 (3= n°Sali, 2= purificato/non)))
		Purified_Molten_Salts[z] <- paste(rownames(Purity)[y],rownames(Molten_Salt_Types)[x])									# Inserisce nel vettore Purified_Molten_Salts la stringa che descrive il tipo di sale e la sua purificazione
		if (length(c(which(CorrDataMod[,14]==rownames(Purity)[y]&CorrDataMod[,13]==rownames(Molten_Salt_Types)[x])))>0){					# Se sono presenti dati per il sale fuso x, nelle condizioni di purificazione y
			Total_Impurities_VS_Purified_Molten_Salt[1:length(c(which(CorrDataMod[,14]==rownames(Purity)[y]&CorrDataMod[,13]				# la colonna z di Total_Impurities_VS_Purified_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x nello stato di purificazione y),
			==rownames(Molten_Salt_Types)[x]))),z] <- CorrDataMod[c(which(CorrDataMod[,14]==rownames(Purity)[y]&CorrDataMod[,13]			# � uguale ai valori presenti nel dataset nella colonna delle impurit� totali, corrispondenti al sale fuso x nelle condizioni di purificazione y
			==rownames(Molten_Salt_Types)[x])),16]
		}
		loc_max <-max(which(Total_Impurities_VS_Purified_Molten_Salt[,z]>0),0.1)											# La numerosit� dei dati sulle impurit� totali per il sale fuso x nelle condizioni di purificazione y viene registrata
		if (loc_max>max){																					# Se la numerosit� dei dati sulle impurit� totali per il sale fuso x nelle condizioni di purificazione y � maggiore di max,
			max<-loc_max																				# si aggiorna tale valore	
		}
	}
}
colnames(Total_Impurities_VS_Purified_Molten_Salt) <- Purified_Molten_Salts													# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso ed uno stato di purificazione
Total_Impurities_VS_Purified_Molten_Salt <-  apply(Total_Impurities_VS_Purified_Molten_Salt, MARGIN = 2,								# Ordina in ordine decrescente le singole colonne della matrice
 function(x) sort(x, decreasing = TRUE, na.last = TRUE))																
Total_Impurities_VS_Purified_Molten_Salt <- Total_Impurities_VS_Purified_Molten_Salt[,order(colSums(!is.na(Total_Impurities_VS_Purified_Molten_Salt)),# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
 decreasing=TRUE)]																						
Total_Impurities_VS_Purified_Molten_Salt <- Total_Impurities_VS_Purified_Molten_Salt[1:sum(!is.na(Total_Impurities_VS_Purified_Molten_Salt[,1])),]	# elimino le righe che contengono solo "NA"

rm(z)																									# Rimozione variabile inutile
rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile
rm(max)																								# Rimozione variabile inutile

				#------VS PURIFIED MOLTEN SALT
		#-------------ESTRAZIONE DATI IMPURITA'-----------------

		#-----ESTRAZIONE DATI PRINCIPALI ELEMENTI DI LEGA-------

Ni_Cr_Mo_Fe_Comp <- data.frame(CorrData[7:10])
Ni_Cr_Mo_Fe_Comp[Ni_Cr_Mo_Fe_Comp==0] <- NA

				#------VS MATERIAL
Ni_Comp_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))									# Inizializza una matrice in cui le colonne sono i vari contenuti di Ni per ogni materiale
Cr_Comp_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))									# Inizializza una matrice in cui le colonne sono i vari contenuti di Cr per ogni materiale
Mo_Comp_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))									# Inizializza una matrice in cui le colonne sono i vari contenuti di Mo per ogni materiale
Fe_Comp_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))									# Inizializza una matrice in cui le colonne sono i vari contenuti di Fe per ogni materiale

for (x in 1:length(Material_Types)){																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	Ni_Comp_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),7]				# la colonna x di Ni_Comp_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x), � uguale ai valori presenti nel dataset nella colonna dei contenuti di Ni, corrispondenti al materiale x
	Cr_Comp_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),8]				# la colonna x di Cr_Comp_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x), � uguale ai valori presenti nel dataset nella colonna dei contenuti di Cr, corrispondenti al materiale x
	Mo_Comp_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),9]				# la colonna x di Mo_Comp_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x), � uguale ai valori presenti nel dataset nella colonna dei contenuti di Mo, corrispondenti al materiale x
	Fe_Comp_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),10]				# la colonna x di Fe_Comp_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x), � uguale ai valori presenti nel dataset nella colonna dei contenuti di Fe, corrispondenti al materiale x
}
colnames(Ni_Comp_VS_Material) <- paste(rep("Ni [%mol] in", times = length(rownames(Material_Types))),rownames(Material_Types))				# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
colnames(Cr_Comp_VS_Material) <- paste(rep("Cr [%mol] in", times = length(rownames(Material_Types))),rownames(Material_Types))				# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
colnames(Mo_Comp_VS_Material) <- paste(rep("Mo [%mol] in", times = length(rownames(Material_Types))),rownames(Material_Types))				# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
colnames(Fe_Comp_VS_Material) <- paste(rep("Fe [%mol] in", times = length(rownames(Material_Types))),rownames(Material_Types))				# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
Ni_Comp_VS_Material[Ni_Comp_VS_Material==0] <- NA																	# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Cr_Comp_VS_Material[Cr_Comp_VS_Material==0] <- NA																	# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Mo_Comp_VS_Material[Mo_Comp_VS_Material==0] <- NA																	# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Fe_Comp_VS_Material[Fe_Comp_VS_Material==0] <- NA																	# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi

Ni_Cr_Mo_Fe_Comp_VS_Material<-data.frame(Ni_Comp_VS_Material,Cr_Comp_VS_Material,Mo_Comp_VS_Material,Fe_Comp_VS_Material)					# Inizializzo un data frame che dovr� contenere in ordine le composizioni di Ni/Cr/Mo/Fe di ogni materiale
y<-1																									# Variabile ausiliaria					
for (x in seq(from=1, to=ncol(Ni_Cr_Mo_Fe_Comp_VS_Material), by=4)){														# Per x che va da 1 alla lunghezza di Ni_Cr_Mo_Fe_Comp_VS_Material(124) a step di 4 (da 1 a 121)  
	Ni_Cr_Mo_Fe_Comp_VS_Material[,x] <- Ni_Comp_VS_Material[,y]															# La colonna x di Ni_Cr_Mo_Fe_Comp_VS_Material � uguale alla colonna y di Ni_Comp_VS_Material
	colnames(Ni_Cr_Mo_Fe_Comp_VS_Material)[x] <- colnames(Ni_Comp_VS_Material)[y] 											# E si chiamano nello stesso modo
	Ni_Cr_Mo_Fe_Comp_VS_Material[,x+1] <- Cr_Comp_VS_Material[,y]														# La colonna x+1 di Ni_Cr_Mo_Fe_Comp_VS_Material � uguale alla colonna y di Cr_Comp_VS_Material
	colnames(Ni_Cr_Mo_Fe_Comp_VS_Material)[x+1] <- colnames(Cr_Comp_VS_Material)[y] 											# E si chiamano nello stesso modo
	Ni_Cr_Mo_Fe_Comp_VS_Material[,x+2] <- Mo_Comp_VS_Material[,y]														# La colonna x+2 di Ni_Cr_Mo_Fe_Comp_VS_Material � uguale alla colonna y di Mo_Comp_VS_Material
	colnames(Ni_Cr_Mo_Fe_Comp_VS_Material)[x+2] <- colnames(Mo_Comp_VS_Material)[y] 											# E si chiamano nello stesso modo
	Ni_Cr_Mo_Fe_Comp_VS_Material[,x+3] <- Fe_Comp_VS_Material[,y]														# La colonna x+3 di Ni_Cr_Mo_Fe_Comp_VS_Material � uguale alla colonna y di Fe_Comp_VS_Material
	colnames(Ni_Cr_Mo_Fe_Comp_VS_Material)[x+3] <- colnames(Fe_Comp_VS_Material)[y] 											# E si chiamano nello stesso modo
	y<-y+1																							# Ed incremento y per il prossimo ciclo
}																									# in questo modo le colonne delle quattro composizioni vengono ordinatamente caricate nel data frame

Ni_Cr_Mo_Fe_Comp_VS_Material <- Ni_Cr_Mo_Fe_Comp_VS_Material[1:sum(!is.na(Ni_Cr_Mo_Fe_Comp_VS_Material[,1])),]						# elimino le righe che contengono solo "NA"

Ni_Cr_Mo_Fe_Comp_VS_Material <- Ni_Cr_Mo_Fe_Comp_VS_Material[,c(!is.na(Ni_Cr_Mo_Fe_Comp_VS_Material[1,]))]								# elimino le colonne che contengono solo "NA"

rm(y)																									# E rimuovo y, che non serve pi�
				#------VS MATERIAL
		#-----ESTRAZIONE DATI PRINCIPALI ELEMENTI DI LEGA-------

		#--ESTRAZIONE DATI ENERGIE LIBERE DI GIBBS (ELLINGHAM)--

Ellingham_G <- data.frame(CorrData[11])																			# Estrazione dati sulle energie libere di gibbs prese dai diagrammi di ellingham per un dato materiale a una data temperatura
names(Ellingham_G) <- "Ellingham_G [kcal/gfw]"																		# Ribattezzo il nome della colonna del vettore
rownames(Ellingham_G) <- rownames(CorrData)																		# Ribatezzo i nomi delle righe del vettore per essere uguali ai nomi delle righe del dataset di partenza

				#------VS MATERIAL
Ellingham_G_VS_Material <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))								# Inizializza una matrice in cui le colonne sono le varie D_G di Ellingham per ogni materiale

for (x in 1:length(Material_Types)){																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	Ellingham_G_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==rownames(Material_Types)[x])),11]			# la colonna x di Ellingham_G_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x),
}																									# � uguale ai valori presenti nel dataset nella colonna delle varie D_G di Ellingham, corrispondenti al materiale x
colnames(Ellingham_G_VS_Material) <- rownames(Material_Types)															# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale
Ellingham_G_VS_Material[Ellingham_G_VS_Material==0] <- NA																# Gli zeri vengono eliminati dalla matrice e sostituiti con NA, in questo modo non influiranno sulle future analisi
Ellingham_G_VS_Material <- Ellingham_G_VS_Material[,order(colSums(!is.na(Ellingham_G_VS_Material)), decreasing=TRUE)]						# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
				#------VS MATERIAL


		#--ESTRAZIONE DATI ENERGIE LIBERE DI GIBBS (ELLINGHAM)--

		#-----------ESTRAZIONE DATI DI WEIGHT LOSS--------------

Weight_Loss <- data.frame(CorrData[c(which(CorrData[,39]!=0)),38:39])														# Estrazione dati sulle Weight_Loss per un dato materiale a una data temperatura (estraggo anche una seconda colonna per preservare i nomi delle righe successivamente...)
names(Weight_Loss) <- "Weight Loss"																				# Ribattezzo il nome della colonna del vettore
rownames(Weight_Loss) <- rownames(CorrData)[c(which(CorrData[,39]!=0))]														# assegno gli stessi nomi alle righe di Weight_Loss dei nomi delle righe di CorrData 
Weight_Loss <- Weight_Loss[order(Weight_Loss[,2], decreasing = TRUE),]														# Ordino in ordine decrescente i valori delle Weight Loss
Weight_Loss <- Weight_Loss[,2,drop=FALSE]																			# elimino la seconda colonna, inutile, preservando i nomi delle righe, che corrispondono agli id degli esperimenti

				#------VS MATERIAL
Weight_Loss_VS_Material <- matrix(NA,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))								# Inizializza una matrice in cui le colonne sono le varie weight loss per ogni materiale

for (x in 1:length(Material_Types)){																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	Weight_Loss_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==								# la colonna x di Weight_Loss_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x),
	rownames(Material_Types)[x])),39]																			# � uguale ai valori presenti nel dataset nella colonna dei weight loss, corrispondenti al materiale x
}
colnames(Weight_Loss_VS_Material) <- rownames(Material_Types)															# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale

Weight_Loss_VS_Material <- apply(Weight_Loss_VS_Material, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))					# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Weight_Loss_VS_Material <- Weight_Loss_VS_Material[,order(colSums(!is.na(Weight_Loss_VS_Material)), decreasing=TRUE)]						# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Weight_Loss_VS_Material <- Weight_Loss_VS_Material[1:sum(!is.na(Weight_Loss_VS_Material[,1])),]										# elimino le righe che contengono solo "NA"
Weight_Loss_VS_Material <- Weight_Loss_VS_Material[,1:sum(!is.na(Weight_Loss_VS_Material[1,]))]										# elimino le colonne che contengono solo "NA"

				#------VS MATERIAL

				#------VS MOLTEN SALT
Weight_Loss_VS_Molten_Salt <- matrix(NA,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types))							# Inizializza una matrice in cui le colonne sono le varie impurit� per ogni sale fuso

for (x in 1:length(Molten_Salt_Types)){																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	Weight_Loss_VS_Molten_Salt[c(1:as.numeric(Molten_Salt_Types[x])),x] <- CorrData[c(which(CorrData[,13]==							# la colonna x di Weight_Loss_VS_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x),
	rownames(Molten_Salt_Types)[x])),39]																		# � uguale ai valori presenti nel dataset nella colonna dei weigth loss, corrispondenti al sale fuso x
}
colnames(Weight_Loss_VS_Molten_Salt) <- rownames(Molten_Salt_Types)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso

Weight_Loss_VS_Molten_Salt <- apply(Weight_Loss_VS_Molten_Salt, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))				# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Weight_Loss_VS_Molten_Salt <- Weight_Loss_VS_Molten_Salt[,order(colSums(!is.na(Weight_Loss_VS_Molten_Salt)), decreasing=TRUE)]				# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Weight_Loss_VS_Molten_Salt <- Weight_Loss_VS_Molten_Salt[1:sum(!is.na(Weight_Loss_VS_Molten_Salt[,1])),]								# elimino le righe che contengono solo "NA"
Weight_Loss_VS_Molten_Salt <- Weight_Loss_VS_Molten_Salt[,1:sum(!is.na(Weight_Loss_VS_Molten_Salt[1,]))]								# elimino le colonne che contengono solo "NA"

				#------VS MOLTEN SALT

				#------VS PURIFIED MOLTEN SALT
Weight_Loss_VS_Purified_Molten_Salt <- matrix(NA,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types)*length(Purity))			# Inizializza una matrice in cui le colonne sono i vari weight loss per ogni sale fuso purificato E non
Purified_Molten_Salts <- c(1:length(Molten_Salt_Types)*length(Purity))														# Inizializza un vettore di stringhe che specificheranno il tipo di sale purificato o non

z <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Weight_Loss_VS_Purified_Molten_Salt
max <- 0																								# Variabile ausiliaria, che servir� per tenere traccia di quale tipo di sale ha maggiore numerosit� di dati di impurit� totali
for (x in 1:length(Molten_Salt_Types)) {																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	for (y in 1:length(Purity)) {																				# Per y che va da 1 al 2 (sale purificato o no)
		z <- z+1																						# Incremento di z (z = 1:6 (6=3x2 (3= n�Sali, 2= purificato/non)))
		Purified_Molten_Salts[z] <- paste(rownames(Purity)[y],rownames(Molten_Salt_Types)[x])									# Inserisce nel vettore Purified_Molten_Salts la stringa che descrive il tipo di sale e la sua purificazione
		if (length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]==rownames(Molten_Salt_Types)[x])))>0){						# Se sono presenti dati per il sale fuso x, nelle condizioni di purificazione y
			Weight_Loss_VS_Purified_Molten_Salt[1:length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]						# la colonna z di Weight_Loss_VS_Purified_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x nello stato di purificazione y),
			==rownames(Molten_Salt_Types)[x]))),z] <- CorrData[c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]					# � uguale ai valori presenti nel dataset nella colonna dei weight loss, corrispondenti al sale fuso x nelle condizioni di purificazione y
			==rownames(Molten_Salt_Types)[x])),39]
		}
		loc_max <-max(which(Weight_Loss_VS_Purified_Molten_Salt[,z]>0 & !is.na(Weight_Loss_VS_Purified_Molten_Salt)[,z]),0.1)				# La numerosit� dei dati sui weight loss per il sale fuso x nelle condizioni di purificazione y viene registrata
		if (loc_max>max){																					# Se la numerosit� dei dati sui weight loss per il sale fuso x nelle condizioni di purificazione y � maggiore di max,
			max<-loc_max																				# si aggiorna tale valore	
		}
	}
}
colnames(Weight_Loss_VS_Purified_Molten_Salt) <- Purified_Molten_Salts														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso ed uno stato di purificazione
Weight_Loss_VS_Purified_Molten_Salt <- apply(Weight_Loss_VS_Purified_Molten_Salt, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))	# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Weight_Loss_VS_Purified_Molten_Salt <- Weight_Loss_VS_Purified_Molten_Salt[,order(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt)),
 decreasing=TRUE)]																						# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Weight_Loss_VS_Purified_Molten_Salt <- Weight_Loss_VS_Purified_Molten_Salt[1:sum(!is.na(Weight_Loss_VS_Purified_Molten_Salt[,1])),]				# elimino le righe che contengono solo "NA"
Weight_Loss_VS_Purified_Molten_Salt <- Weight_Loss_VS_Purified_Molten_Salt[,1:sum(!is.na(Weight_Loss_VS_Purified_Molten_Salt[1,]))]				# elimino le colonne che contengono solo "NA"


rm(z)																									# Rimozione variabile inutile
rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile
rm(max)																								# Rimozione variabile inutile
				#------VS PURIFIED MOLTEN SALT

				#------VS TEMPERATURES
Weight_Loss_VS_Exp_Temperatures <- matrix(NA,nrow=max(as.numeric(Exp_Temperatures)), ncol=length(Exp_Temperatures))						# Inizializza una matrice in cui le colonne sono i vari weigth loss per ogni temperatura sperimentale

for (x in 1:length(Exp_Temperatures)){																			# Per x che va da 1 al numero di temperature sperimentali diverse presenti nel dataset
	Weight_Loss_VS_Exp_Temperatures[c(1:as.numeric(Exp_Temperatures[x])),x] <- CorrData[c(which(CorrData[,2]==rownames(Exp_Temperatures)[x])),39]	# la colonna x di Weight_Loss_VS_Exp_Temperatures (dalla prima riga alla riga corrispondente la numerosit� della temperatura sperimentale x),
}																									# � uguale ai valori presenti nel dataset nella colonna dei weigth loss, corrispondenti alla temperatura sperimentale x
colnames(Weight_Loss_VS_Exp_Temperatures) <- rownames(Exp_Temperatures)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad una temperatura sperimentale
Weight_Loss_VS_Exp_Temperatures <- apply(Weight_Loss_VS_Exp_Temperatures, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))		# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Weight_Loss_VS_Exp_Temperatures <- Weight_Loss_VS_Exp_Temperatures[,order(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures)), decreasing=TRUE)]		# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente 																						
Weight_Loss_VS_Exp_Temperatures <- Weight_Loss_VS_Exp_Temperatures[1:sum(!is.na(Weight_Loss_VS_Exp_Temperatures[,1])),]						# elimino le righe che contengono solo "NA"
Weight_Loss_VS_Exp_Temperatures <- Weight_Loss_VS_Exp_Temperatures[,1:sum(!is.na(Weight_Loss_VS_Exp_Temperatures[1,]))]						# elimino le colonne che contengono solo "NA"
				#------VS TEMPERATURES

				#------VS DIVE TIME
Weight_Loss_VS_Exp_Dive_Times <- matrix(NA,nrow=max(as.numeric(Exp_Dive_Times)), ncol=length(Exp_Dive_Times))							# Inizializza una matrice in cui le colonne sono i vari weight loss per ogni dive time

for (x in 1:length(Exp_Dive_Times)){																			# Per x che va da 1 al numero di dive time diversi presenti nel dataset
	Weight_Loss_VS_Exp_Dive_Times[c(1:as.numeric(Exp_Dive_Times[x])),x] <- CorrData[c(which(CorrData[,3]==rownames(Exp_Dive_Times)[x])),39]		# la colonna x di Weight_Loss_VS_Exp_Dive_Time (dalla prima riga alla riga corrispondente la numerosit� del dive time x),
}																									# � uguale ai valori presenti nel dataset nella colonna dei weight loss, corrispondenti al dive time x
colnames(Weight_Loss_VS_Exp_Dive_Times) <- rownames(Exp_Dive_Times)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un dive time

Weight_Loss_VS_Exp_Dive_Times <- apply(Weight_Loss_VS_Exp_Dive_Times, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))			# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Weight_Loss_VS_Exp_Dive_Times <- Weight_Loss_VS_Exp_Dive_Times[,order(colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times)), decreasing=TRUE)]			# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente 																						
Weight_Loss_VS_Exp_Dive_Times <- Weight_Loss_VS_Exp_Dive_Times[1:sum(!is.na(Weight_Loss_VS_Exp_Dive_Times[,1])),]							# elimino le righe che contengono solo "NA"
Weight_Loss_VS_Exp_Dive_Times <- Weight_Loss_VS_Exp_Dive_Times[,1:sum(!is.na(Weight_Loss_VS_Exp_Dive_Times[1,]))]							# elimino le colonne che contengono solo "NA"
				#------VS DIVE TIME

				#------RANGES
Dim_ranges <- 25																							# Setto quanto voglio che siano ampi i miei range di weight loss
ranges <- seq(from=Dim_ranges*floor(min(sort(Weight_Loss[,]))/Dim_ranges),to=Dim_ranges*ceiling(max(sort(Weight_Loss[,]))/Dim_ranges),by=Dim_ranges)	# Esplicito tali ranges
Weight_Loss_ranges <- data.frame(matrix(rep(0,times=length(ranges)-1), nrow=1, ncol=length(ranges)-1))								# Inizializza una matrice in cui le colonne rappresentano la numerosit� per ogni weight loss range
rownames(Weight_Loss_ranges)<-"Weight Loss"																		# assegno il nome alla riga della matrice
z <-1																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Weight_Loss_ranges
colnames(Weight_Loss_ranges)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")													# Si setta il nome della colonna per rappresentare tale range z
for (x in 1:length(sort(Weight_Loss[,]))) {																		# Per x che va da 1 al numero di weight loss registrati nel dataset
	if(sort(Weight_Loss[,])[x]>=ranges[z] & sort(Weight_Loss[,])[x]<ranges[z+1]){												# Se il weight loss x � compreso nell'intervallo di range z specificato
		Weight_Loss_ranges[1,z]<-Weight_Loss_ranges[1,z]+1															# Incrementa di 1 il valore della colonna z di Weight_Loss_ranges
		
	} else {																							
		z <- z+1																						# Altrimenti si passa al prossimo range z,							
		Weight_Loss_ranges[1,z]<-Weight_Loss_ranges[1,z]+1															# Si incrementa di 1 il valore della nuova colonna z (z+1) di Weight_Loss_ranges 		
		colnames(Weight_Loss_ranges)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")											# E si setta il nome della colonna per rappresentare tale range z			
	}
}
rm(x)																									# Rimozione variabile inutile
rm(z)																									# Rimozione variabile inutile																								
				#------RANGES

				#------RANGES VS DEPLETION
Max_Depletion <- data.frame(CorrData[38])																			# Carico i dati di depletion depth
Dim_ranges <- 25																							# Setto quanto voglio che siano ampi i miei range di weight loss
ranges <- seq(from=Dim_ranges*floor(min(sort(Weight_Loss[,]))/Dim_ranges),to=Dim_ranges*ceiling(max(sort(Weight_Loss[,]))/Dim_ranges),by=Dim_ranges)	# Esplicito tali ranges
Weight_Loss_ranges_VS_Depletion <- data.frame(matrix(rep(0,times=2*(length(ranges)-1)), nrow=2, ncol=length(ranges)-1))						# Inizializza una matrice in cui le colonne rappresentano la numerosit� per ogni weight loss range, se con depletion depth o no
rownames(Weight_Loss_ranges_VS_Depletion)[1]<-"With Depletion Depth data"													# assegno il nome alla prima riga della matrice
rownames(Weight_Loss_ranges_VS_Depletion)[2]<-"Without Depletion Depth data"													# assegno il nome alla seconda riga della matrice

z <-1																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Weight_Loss_ranges_VS_Depletion
colnames(Weight_Loss_ranges_VS_Depletion)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")											# Si setta il nome della prima colonna per rappresentare tale range z

for (x in 1:length(sort(Weight_Loss[,]))) {																		# Per x che va da 1 al numero di weight loss registrati nel dataset
	if(sort(Weight_Loss[,])[x]>=ranges[z] & sort(Weight_Loss[,])[x]<ranges[z+1]){												# Se il weight loss x � compreso nell'intervallo di range z specificato
		if(is.na(Max_Depletion[sort.list(Weight_Loss[,],na.last=NA)[x],])){												# e la depletion depth non � stata registrata per tale weigh loss
			Weight_Loss_ranges_VS_Depletion[2,z]<-Weight_Loss_ranges_VS_Depletion[2,z]+1										# Incrementa di 1 il valore della colonna z (che rappresenta il range) e della riga 2 (che rappresenta l'assenza di dati sulla depletion depth) di Weight_Loss_ranges
		}else{																						# Altrimenti
			Weight_Loss_ranges_VS_Depletion[1,z]<-Weight_Loss_ranges_VS_Depletion[1,z]+1										# Incrementa di 1 il valore della colonna z (che rappresenta il range) e della riga 1 (che rappresenta la presenza di dati sulla depletion depth) di Weight_Loss_ranges
		}
	} else {																							# Altrimenti, se il weight loss x non � compreso nell'intervallo di range z specificato,
			z <- z+1																					# si passa al prossimo range z,
		if(is.na(Max_Depletion[sort.list(Weight_Loss[,],na.last=NA)[x],])){												# e se la depletion depth non � stata registrata per tale weigh loss																																														# Altrimenti si passa al prossimo range z,							
			Weight_Loss_ranges_VS_Depletion[2,z]<-Weight_Loss_ranges_VS_Depletion[2,z]+1										# Si incrementa di 1 il valore della nuova colonna z(z+1) (che rappresenta il range) e della riga 2 (che rappresenta l'assenza di dati sulla depletion depth) di Weight_Loss_ranges 		
		}else{																						# Altrimenti																								
			Weight_Loss_ranges_VS_Depletion[1,z]<-Weight_Loss_ranges_VS_Depletion[1,z]+1										# Si incrementa di 1 il valore della nuova colonna z (z+1) (che rappresenta il range) e della riga 1 (che rappresenta la presenza di dati sulla depletion depth) di Weight_Loss_ranges 		
		}
			colnames(Weight_Loss_ranges_VS_Depletion)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")								# Infine, si setta il nome della colonna per rappresentare tale range z						
	}
}
rm(x)																									# Rimozione variabile inutile
rm(z)																									# Rimozione variabile inutile																								
				#------RANGES VS DEPLETION

		#-----------ESTRAZIONE DATI DI WEIGHT LOSS--------------

		#----------ESTRAZIONE DATI DI MAX DEPLETION-------------
Max_Depletion <- data.frame(CorrData[c(which(CorrData[,38]!=0)),38:39])														# Estrazione dati sulle Max_Depletion per un dato materiale a una data temperatura (estraggo anche una seconda colonna per preservare i nomi delle righe successivamente...)
names(Max_Depletion) <- "Max Depletion"																			# Ribattezzo il nome della colonna del vettore
rownames(Max_Depletion) <- rownames(CorrData)[c(which(CorrData[,38]!=0))]													# assegno gli stessi nomi alle righe di Max_Depletion dei nomi delle righe di CorrData 
Max_Depletion <- Max_Depletion[order(Max_Depletion[,1], decreasing = TRUE),]													# Ordino in ordine decrescente i valori delle Max Depletion
Max_Depletion <- Max_Depletion[,1,drop=FALSE]																		# elimino la seconda colonna, inutile, preservando i nomi delle righe, che corrispondono agli id degli esperimenti

				#------VS MATERIAL
Max_Depletion_VS_Material <- matrix(NA,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types))								# Inizializza una matrice in cui le colonne sono le varie max depletion per ogni materiale

for (x in 1:length(Material_Types)){																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	Max_Depletion_VS_Material[c(1:as.numeric(Material_Types[x])),x] <- CorrData[c(which(CorrData[,5]==								# la colonna x di Max_Depletion_VS_Material (dalla prima riga alla riga corrispondente la numerosit� del materiale x),
	rownames(Material_Types)[x])),38]																			# � uguale ai valori presenti nel dataset nella colonna delle max depletion, corrispondenti al materiale x
}
colnames(Max_Depletion_VS_Material) <- rownames(Material_Types)															# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un materiale

Max_Depletion_VS_Material <- apply(Max_Depletion_VS_Material, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))				# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Max_Depletion_VS_Material <- Max_Depletion_VS_Material[,order(colSums(!is.na(Max_Depletion_VS_Material)), decreasing=TRUE)]					# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Max_Depletion_VS_Material <- Max_Depletion_VS_Material[1:sum(!is.na(Max_Depletion_VS_Material[,1])),]									# elimino le righe che contengono solo "NA"
Max_Depletion_VS_Material <- Max_Depletion_VS_Material[,1:sum(!is.na(Max_Depletion_VS_Material[1,]))]									# elimino le colonne che contengono solo "NA"
				#------VS MATERIAL

				#------VS MOLTEN SALT
Max_Depletion_VS_Molten_Salt <- matrix(NA,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types))						# Inizializza una matrice in cui le colonne sono le varie impurit� per ogni sale fuso

for (x in 1:length(Molten_Salt_Types)){																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	Max_Depletion_VS_Molten_Salt[c(1:as.numeric(Molten_Salt_Types[x])),x] <- CorrData[c(which(CorrData[,13]==							# la colonna x di Max_Depletion_VS_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x),
	rownames(Molten_Salt_Types)[x])),38]																		# � uguale ai valori presenti nel dataset nella colonna delle max depletion, corrispondenti al sale fuso x
}
colnames(Max_Depletion_VS_Molten_Salt) <- rownames(Molten_Salt_Types)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso

Max_Depletion_VS_Molten_Salt <- apply(Max_Depletion_VS_Molten_Salt, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))			# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Max_Depletion_VS_Molten_Salt <- Max_Depletion_VS_Molten_Salt[,order(colSums(!is.na(Max_Depletion_VS_Molten_Salt)), decreasing=TRUE)]			# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Max_Depletion_VS_Molten_Salt <- Max_Depletion_VS_Molten_Salt[1:sum(!is.na(Max_Depletion_VS_Molten_Salt[,1])),]							# elimino le righe che contengono solo "NA"
Max_Depletion_VS_Molten_Salt <- Max_Depletion_VS_Molten_Salt[,1:sum(!is.na(Max_Depletion_VS_Molten_Salt[1,]))]							# elimino le colonne che contengono solo "NA"
				#------VS MOLTEN SALT

				#------VS PURIFIED MOLTEN SALT
Max_Depletion_VS_Purified_Molten_Salt <- matrix(NA,nrow=max(as.numeric(Molten_Salt_Types)), ncol=length(Molten_Salt_Types)*length(Purity))			# Inizializza una matrice in cui le colonne sono le varie max depletion per ogni sale fuso purificato E non
Purified_Molten_Salts <- c(1:length(Molten_Salt_Types)*length(Purity))														# Inizializza un vettore di stringhe che specificheranno il tipo di sale purificato o non

z <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Max_Depletion_VS_Purified_Molten_Salt
max <- 0																								# Variabile ausiliaria, che servir� per tenere traccia di quale tipo di sale ha maggiore numerosit� di dati di impurit� totali
for (x in 1:length(Molten_Salt_Types)) {																			# Per x che va da 1 al numero di sali fusi diversi presenti nel dataset
	for (y in 1:length(Purity)) {																				# Per y che va da 1 al 2 (sale purificato o no)
		z <- z+1																						# Incremento di z (z = 1:6 (6=3x2 (3= n�Sali, 2= purificato/non)))
		Purified_Molten_Salts[z] <- paste(rownames(Purity)[y],rownames(Molten_Salt_Types)[x])									# Inserisce nel vettore Purified_Molten_Salts la stringa che descrive il tipo di sale e la sua purificazione
		if (length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]==rownames(Molten_Salt_Types)[x])))>0){						# Se sono presenti dati per il sale fuso x, nelle condizioni di purificazione y
			Max_Depletion_VS_Purified_Molten_Salt[1:length(c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]					# la colonna z di Max_Depletion_VS_Purified_Molten_Salt (dalla prima riga alla riga corrispondente la numerosit� del sale fuso x nello stato di purificazione y),
			==rownames(Molten_Salt_Types)[x]))),z] <- CorrData[c(which(CorrData[,14]==rownames(Purity)[y]&CorrData[,13]					# � uguale ai valori presenti nel dataset nella colonna delle max depletion, corrispondenti al sale fuso x nelle condizioni di purificazione y
			==rownames(Molten_Salt_Types)[x])),38]
		}
		loc_max <-max(which(Max_Depletion_VS_Purified_Molten_Salt[,z]<0),0.1)												# La numerosit� dei dati sulle max depletion per il sale fuso x nelle condizioni di purificazione y viene registrata
		if (loc_max>max){																					# Se la numerosit� dei dati sulle max depletion per il sale fuso x nelle condizioni di purificazione y � maggiore di max,
			max<-loc_max																				# si aggiorna tale valore	
		}
	}
}
colnames(Max_Depletion_VS_Purified_Molten_Salt) <- Purified_Molten_Salts													# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un sale fuso ed uno stato di purificazione

Max_Depletion_VS_Purified_Molten_Salt <- apply(Max_Depletion_VS_Purified_Molten_Salt, MARGIN = 2, function(x) sort(x, decreasing = TRUE,			# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
 na.last = TRUE))	
Max_Depletion_VS_Purified_Molten_Salt <- Max_Depletion_VS_Purified_Molten_Salt[,order(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt)),
 decreasing=TRUE)]																						# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Max_Depletion_VS_Purified_Molten_Salt <- Max_Depletion_VS_Purified_Molten_Salt[1:sum(!is.na(Max_Depletion_VS_Purified_Molten_Salt[,1])),]			# elimino le righe che contengono solo "NA"
Max_Depletion_VS_Purified_Molten_Salt <- Max_Depletion_VS_Purified_Molten_Salt[,1:sum(!is.na(Max_Depletion_VS_Purified_Molten_Salt[1,]))]			# elimino le colonne che contengono solo "NA"

rm(z)																									# Rimozione variabile inutile
rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile
rm(max)																								# Rimozione variabile inutile
				#------VS PURIFIED MOLTEN SALT

				#------VS TEMPERATURES
Max_Depletion_VS_Exp_Temperatures <- matrix(NA,nrow=max(as.numeric(Exp_Temperatures)), ncol=length(Exp_Temperatures))						# Inizializza una matrice in cui le colonne sono le varie max depletion per ogni temperatura sperimentale

for (x in 1:length(Exp_Temperatures)){																			# Per x che va da 1 al numero di temperature sperimentali diverse presenti nel dataset
	Max_Depletion_VS_Exp_Temperatures[c(1:as.numeric(Exp_Temperatures[x])),x] <- CorrData[c(which(CorrData[,2]==rownames(Exp_Temperatures)[x])),38]	# la colonna x di Max_Depletion_VS_Exp_Temperatures (dalla prima riga alla riga corrispondente la numerosit� della temperatura sperimentale x),
}																									# � uguale ai valori presenti nel dataset nella colonna delle max depletion, corrispondenti alla temperatura sperimentale x
colnames(Max_Depletion_VS_Exp_Temperatures) <- rownames(Exp_Temperatures)													# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad una temperatura sperimentale

Max_Depletion_VS_Exp_Temperatures <- apply(Max_Depletion_VS_Exp_Temperatures, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))	# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Max_Depletion_VS_Exp_Temperatures <- Max_Depletion_VS_Exp_Temperatures[,order(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures)), decreasing=TRUE)]	# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente 																						
Max_Depletion_VS_Exp_Temperatures <- Max_Depletion_VS_Exp_Temperatures[1:sum(!is.na(Max_Depletion_VS_Exp_Temperatures[,1])),]					# elimino le righe che contengono solo "NA"
Max_Depletion_VS_Exp_Temperatures <- Max_Depletion_VS_Exp_Temperatures[,1:sum(!is.na(Max_Depletion_VS_Exp_Temperatures[1,]))]					# elimino le colonne che contengono solo "NA"
				#------VS TEMPERATURES

				#------VS DIVE TIME
Max_Depletion_VS_Exp_Dive_Times <- matrix(NA,nrow=max(as.numeric(Exp_Dive_Times)), ncol=length(Exp_Dive_Times))							# Inizializza una matrice in cui le colonne sono le varie max depletion per ogni dive time

for (x in 1:length(Exp_Dive_Times)){																			# Per x che va da 1 al numero di dive time diversi presenti nel dataset
	Max_Depletion_VS_Exp_Dive_Times[c(1:as.numeric(Exp_Dive_Times[x])),x] <- CorrData[c(which(CorrData[,3]==rownames(Exp_Dive_Times)[x])),38]		# la colonna x di Max_Depletion_VS_Exp_Dive_Time (dalla prima riga alla riga corrispondente la numerosit� del dive time x),
}																									# � uguale ai valori presenti nel dataset nella colonna delle max depletion, corrispondenti al dive time x
colnames(Max_Depletion_VS_Exp_Dive_Times) <- rownames(Exp_Dive_Times)														# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad un dive time

Max_Depletion_VS_Exp_Dive_Times <- apply(Max_Depletion_VS_Exp_Dive_Times, MARGIN = 2, function(x) sort(x, decreasing = TRUE, na.last = TRUE))		# Ordino, in ordine decrescente, le singole colonne di Weight_Loss_VS_Molten_Salt
Max_Depletion_VS_Exp_Dive_Times <- Max_Depletion_VS_Exp_Dive_Times[,order(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times)), decreasing=TRUE)]		# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente 																						
Max_Depletion_VS_Exp_Dive_Times <- Max_Depletion_VS_Exp_Dive_Times[1:sum(!is.na(Max_Depletion_VS_Exp_Dive_Times[,1])),]						# elimino le righe che contengono solo "NA"
Max_Depletion_VS_Exp_Dive_Times <- Max_Depletion_VS_Exp_Dive_Times[,1:sum(!is.na(Max_Depletion_VS_Exp_Dive_Times[1,]))]						# elimino le colonne che contengono solo "NA"
				#------VS DIVE TIME

				#------RANGES
Dim_ranges <- 25																							# Setto quanto voglio che siano ampi i miei range di max depletion
ranges <- seq(from=Dim_ranges*floor(min(sort(Max_Depletion[,]))/Dim_ranges),
		to=Dim_ranges*ceiling(max(sort(Max_Depletion[,]))/Dim_ranges),by=Dim_ranges)											# Esplicito tali ranges
Max_Depletion_ranges <- data.frame(matrix(rep(0,times=length(ranges)-1), nrow=1, ncol=length(ranges)-1))								# Inizializza una matrice in cui le colonne rappresentano la numerosit� per ogni max depletion range
rownames(Max_Depletion_ranges)<-"Max Depletion"																		# assegno il nome alla riga della matrice
z <-1																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Max_Depletion_ranges
colnames(Max_Depletion_ranges)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")													# Si setta il nome della colonna per rappresentare tale range z
for (x in 1:length(sort(Max_Depletion[,]))) {																		# Per x che va da 1 al numero di max depletion registrati nel dataset
	if(sort(Max_Depletion[,])[x]>=ranges[z] & sort(Max_Depletion[,])[x]<ranges[z+1]){											# Se il max depletion x � compreso nell'intervallo di range z specificato
		Max_Depletion_ranges[1,z]<-Max_Depletion_ranges[1,z]+1														# Incrementa di 1 il valore della colonna z di Max_Depletion_ranges
		
	} else {																							
		z <- z+1																						# Altrimenti si passa al prossimo range z,							
		Max_Depletion_ranges[1,z]<-Max_Depletion_ranges[1,z]+1														# Si incrementa di 1 il valore della nuova colonna z (z+1) di Max_Depletion_ranges 		
		colnames(Max_Depletion_ranges)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")											# E si setta il nome della colonna per rappresentare tale range z			
	}
}
rm(x)																									# Rimozione variabile inutile
rm(z)																									# Rimozione variabile inutile																								
				#------RANGES

				#------RANGES VS WEIGHT LOSS
Weight_Loss <- data.frame(CorrData[39])																			# Carico i dati di weight loss
Dim_ranges <- 25																							# Setto quanto voglio che siano ampi i miei range di depletion depth
ranges <- seq(from=Dim_ranges*floor(min(sort(Max_Depletion[,]))/Dim_ranges),
		to=Dim_ranges*ceiling(max(sort(Max_Depletion[,]))/Dim_ranges),by=Dim_ranges)											# Esplicito tali ranges
Max_Depletion_ranges_VS_Weight_Loss <- data.frame(matrix(rep(0,times=2*(length(ranges)-1)), nrow=2, ncol=length(ranges)-1))					# Inizializza una matrice in cui le colonne rappresentano la numerosit� per ogni depletion depth range, se con weight loss o no
rownames(Max_Depletion_ranges_VS_Weight_Loss)[1]<-"With Weight Loss"														# assegno il nome alla prima riga della matrice
rownames(Max_Depletion_ranges_VS_Weight_Loss)[2]<-"Without Weight Loss"														# assegno il nome alla seconda riga della matrice

z <-1																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Max_Depletion_ranges_VS_Weight_Loss
colnames(Max_Depletion_ranges_VS_Weight_Loss)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")										# Si setta il nome della prima colonna per rappresentare tale range z

for (x in 1:length(sort(Max_Depletion[,]))) {																		# Per x che va da 1 al numero di depletion depth registrati nel dataset
	if(sort(Max_Depletion[,])[x]>=ranges[z] & sort(Max_Depletion[,])[x]<ranges[z+1]){											# Se il depletion depth x � compreso nell'intervallo di range z specificato
		if(is.na(Weight_Loss[sort.list(Max_Depletion[,],na.last=NA)[x],])){												# e la weight loss non � stata registrata per tale weigh loss
			Max_Depletion_ranges_VS_Weight_Loss[2,z]<-Max_Depletion_ranges_VS_Weight_Loss[2,z]+1								# Incrementa di 1 il valore della colonna z (che rappresenta il range) e della riga 2 (che rappresenta l'assenza di dati sulla depletion depth) di Max_Depletion_ranges_VS_Weight_Loss
		}else{																						# Altrimenti
			Max_Depletion_ranges_VS_Weight_Loss[1,z]<-Max_Depletion_ranges_VS_Weight_Loss[1,z]+1								# Incrementa di 1 il valore della colonna z (che rappresenta il range) e della riga 1 (che rappresenta la presenza di dati sulla depletion depth) di Max_Depletion_ranges_VS_Weight_Loss
		}
	} else {																							# Altrimenti, se il depletion depth x non � compreso nell'intervallo di range z specificato,
			z <- z+1																					# si passa al prossimo range z,
		if(is.na(Weight_Loss[sort.list(Max_Depletion[,],na.last=NA)[x],])){												# e se la weight loss non � stata registrata per tale weigh loss																																														# Altrimenti si passa al prossimo range z,							
			Max_Depletion_ranges_VS_Weight_Loss[2,z]<-Max_Depletion_ranges_VS_Weight_Loss[2,z]+1								# Si incrementa di 1 il valore della nuova colonna z(z+1) (che rappresenta il range) e della riga 2 (che rappresenta l'assenza di dati sulla depletion depth) di Max_Depletion_ranges_VS_Weight_Loss 		
		}else{																						# Altrimenti																								
			Max_Depletion_ranges_VS_Weight_Loss[1,z]<-Max_Depletion_ranges_VS_Weight_Loss[1,z]+1								# Si incrementa di 1 il valore della nuova colonna z (z+1) (che rappresenta il range) e della riga 1 (che rappresenta la presenza di dati sulla depletion depth) di Max_Depletion_ranges_VS_Weight_Loss 		
		}
			colnames(Max_Depletion_ranges_VS_Weight_Loss)[z] <- paste("[",ranges[z],"/",ranges[z+1],"]")							# Infine, si setta il nome della colonna per rappresentare tale range z						
	}
}
rm(x)																									# Rimozione variabile inutile
rm(z)																									# Rimozione variabile inutile																								
				#------RANGES VS DEPLETION

		#----------ESTRAZIONE DATI DI MAX DEPLETION---------------

		#--------------ESTRAZIONE DATI MATERIALI------------------

				#------VS CRUCIBLE TYPE
Material_Types_VS_Crucible_Types <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types)*length(Crucible_Types))			# Inizializza una matrice in cui le colonne rappresentano la numerosit� per ogni coppia di materiale e crucible
Material_and_Crucible_Types <- c(1:length(Material_Types)*length(Crucible_Types))												# Inizializza un vettore di stringhe che specificheranno la coppia di materiale e crucible

	#	rownames(Material_Types)[x] <- gsub(" ","-",rownames(Material_Types))
	#	rownames(Crucible_Types)[y] <- gsub(" ","-",rownames(Crucible_Types))

z <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Material_Types_VS_Crucible_Types
max <- 0																								# Variabile ausiliaria, che servir� per tenere traccia di quale coppia materiale/crucible ha maggiore numerosit� 
for (x in 1:length(Material_Types)) {																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	for (y in 1:length(Crucible_Types)) {																		# Per y che va da 1 al numero di crucible diversi presenti nel dataset
		z <- z+1																						# Incremento di z
		Material_and_Crucible_Types[z] <- paste(gsub(" ","-",rownames(Material_Types)[x]),"with",gsub(" ","-",rownames(Crucible_Types)[y]),		# Inserisce nel vettore Material_and_Crucible_Types la stringa che descrive la coppia materiale/crucible
		"Crucible")	
		if (length(c(which(CorrData[,4]==rownames(Crucible_Types)[y]&CorrData[,5]==rownames(Material_Types)[x])))>0){					# Se sono presenti dati per il meteriale x, con il crucible y
			Material_Types_VS_Crucible_Types[1:length(c(which(CorrData[,4]==rownames(Crucible_Types)[y]&CorrData[,5]					# la colonna z di Material_Types_VS_Crucible_Types (dalla prima riga alla riga corrispondente la numerosit� della coppia materiale x/crucible y),
			==rownames(Material_Types)[x]))),z] <- rep(1, length(c(which(CorrData[,4]==rownames(Crucible_Types)[y]&CorrData[,5]			# � uguale ad un vettore di 1
			==rownames(Material_Types)[x]))))
		}
		loc_max <-max(which(Material_Types_VS_Crucible_Types[,z]>0),0.1)													# La numerosit� dei dati sulla coppia materiale x/crucible y viene registrata
		if (loc_max>max){																					# Se la numerosit� dei dati sulla coppia materiale x/crucible y � maggiore di max,
			max<-loc_max																				# si aggiorna tale valore	
		}
	}
}
colnames(Material_Types_VS_Crucible_Types) <- Material_and_Crucible_Types													# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad una sulla coppia materiale/crucible
Material_Types_VS_Crucible_Types <- Material_Types_VS_Crucible_Types[1:max,]													# Elimina le righe di Material_Types_VS_Crucible_Types contenenti solo 0
Material_and_Crucible_Types <- Material_and_Crucible_Types[c(which(colSums(Material_Types_VS_Crucible_Types)>0))]							# Molte combinazione matriale/crucible non hanno dati, vengono quindi eliminati i nomi
Material_Types_VS_Crucible_Types <- Material_Types_VS_Crucible_Types[,c(which(colSums(Material_Types_VS_Crucible_Types)>0))]					# Molte combinazione matriale/crucible non hanno dati, vengono quindi eliminate
# 	Material_Types_VS_Crucible_Types <- Material_Types_VS_Crucible_Types[,order(colSums((Material_Types_VS_Crucible_Types)>0), decreasing=TRUE)]	# Ordino le righe della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
Material_Types_VS_Crucible_Types <- colSums(Material_Types_VS_Crucible_Types)													# Sommando le colonne restanti ottengo un unico vettore in cui ogni colonna corrisponde alla numerosit� della coppia materiale/crucible

rm(z)																									# Rimozione variabile inutile
rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile
rm(max)																								# Rimozione variabile inutile
				#------VS CRUCIBLE TYPE

				#------VS PURIFIED MOLTEN SALTS
Purified_Molten_Salts <- rep(1,length(Molten_Salt_Types)*length(Purity))													# Inizializza un vettore di stringhe che specificheranno il numero di combinazioni di sale fuso e stato di purificazione
Material_Types_VS_Purified_Molten_Salts <- matrix(0,nrow=max(as.numeric(Material_Types)), ncol=length(Material_Types)*length(Purified_Molten_Salts))	# Inizializza una matrice in cui le colonne rappresentano la numerosit� per ogni coppia di materiale e sale fuso (non)purificato
Material_and_Purified_Molten_Salts <- c(1:length(Material_Types)*length(Purified_Molten_Salts))										# Inizializza un vettore di stringhe che specificheranno la coppia di materiale e e sale fuso (non)purificato

k <-0																									# Variabile ausiliaria, per tenere traccia della colonna della matrice Material_Types_VS_Purified_Molten_Salts
max <- 0																								# Variabile ausiliaria, che servir� per tenere traccia di quale coppia materiale/sale fuso (non)purificato ha maggiore numerosit� 
for (x in 1:length(Material_Types)) {																			# Per x che va da 1 al numero di materiali diversi presenti nel dataset
	for (y in 1:length(Molten_Salt_Types)) {																		# Per y che va da 1 al numero di sali fusi diversi presenti nel dataset
		for (z in 1:length(Purity)){																			# Per y che va da 1 al 2 (sale purificato o no)
			k <- k+1																					# Incremento di k
			Material_and_Purified_Molten_Salts[k] <- paste(gsub(" ","-",rownames(Material_Types)[x]),"with",gsub(" ","-",rownames(Purity)[z]),
			gsub(" ","-",rownames(Molten_Salt_Types)[y]))															# Inserisce nel vettore Material_and_Purified_Molten_Salts la stringa che descrive la coppia materiale/sale fuso (non)purificato
			if (length(c(which(CorrData[,5]==rownames(Material_Types)[x] & CorrData[,13]==rownames(Molten_Salt_Types)[y] &				# Se sono presenti dati per il meteriale x, con il sale fuso y,
			    CorrData[,14]==rownames(Purity)[z])))>0){															# nello stato di purificazione z
				Material_Types_VS_Purified_Molten_Salts[1:length(c(which(CorrData[,5]==rownames(Material_Types)[x] & CorrData[,13]		# la colonna k di Material_Types_VS_Purified_Molten_Salts (dalla prima riga alla riga corrispondente la numerosit�
				==rownames(Molten_Salt_Types)[y] & CorrData[,14]==rownames(Purity)[z]))),k] <- 								# della coppia materiale x/sale fuso y/(non)purificato z),
				rep(1, length(c(which(CorrData[,5]==rownames(Material_Types)[x]&CorrData[,13]==rownames(Molten_Salt_Types)[y] & 
				CorrData[,14]==rownames(Purity)[z]))))															# � uguale ad un vettore di 1
				
			}
			loc_max <-max(which(Material_Types_VS_Purified_Molten_Salts[,k]>0),0.1)											# La numerosit� dei dati sulla coppia materiale x/sale fuso y/(non)purificato z viene registrata
			if (loc_max>max){																				# Se la numerosit� dei dati sulla coppia materiale x/sale fuso y/(non)purificato z � maggiore di max,
				max<-loc_max																			# si aggiorna tale valore	
			}
		}
	}
}
colnames(Material_Types_VS_Purified_Molten_Salts) <- Material_and_Purified_Molten_Salts											# Imposta i nomi delle colonne della matrice, ogni colonna corrisponde ad una sulla coppia materiale/sale fuso (non)purificato
Material_Types_VS_Purified_Molten_Salts <- Material_Types_VS_Purified_Molten_Salts[1:max,]										# Elimina le righe di Material_Types_VS_Purified_Molten_Salts contenenti solo 0
Material_and_Purified_Molten_Salts <- Material_and_Purified_Molten_Salts[c(which(colSums(Material_Types_VS_Purified_Molten_Salts)>0))]			# Molte combinazione matriale/ sale fuso/ (non) purificato non hanno dati, vengono quindi eliminati
Material_Types_VS_Purified_Molten_Salts <- Material_Types_VS_Purified_Molten_Salts[,c(which(colSums(Material_Types_VS_Purified_Molten_Salts)>0))]	# Molte combinazione matriale/ sale fuso/ (non) purificato non hanno dati, vengono quindi eliminati
#  Material_Types_VS_Purified_Molten_Salts <- Material_Types_VS_Purified_Molten_Salts[,order(colSums((Material_Types_VS_Purified_Molten_Salts)>0),	# Ordino le colonne della matrice dato il numero di elementi per ogni colonna, in ordine decrescente
#   decreasing=TRUE)]
Material_Types_VS_Purified_Molten_Salts <- colSums(Material_Types_VS_Purified_Molten_Salts)										# Sommando le colonne restanti ottengo un unico vettore in cui ogni colonna corrisponde alla numerosit� della coppia materiale/sale fuso (non) purificato
	

rm(x)																									# Rimozione variabile inutile
rm(y)																									# Rimozione variabile inutile
rm(z)																									# Rimozione variabile inutile
rm(k)																									# Rimozione variabile inutile
rm(max)																								# Rimozione variabile inutile
				#------VS PURIFIED MOLTEN SALTS

		#--------------ESTRAZIONE DATI MATERIALI------------------

		#-------------ESTRAZIONE DATI SALI FUSI-----------------
Molten_Salt_Count <- table(1:6)
names(Molten_Salt_Count) <- paste(rep(names(Purity), times= length(Molten_Salt_Types)),rep(names(Molten_Salt_Types), each=length(Purity)))

z <- 0
for(x in 1:length(Molten_Salt_Types)){
	for(y in 1:length(Purity)){
		z <- z+1
		Molten_Salt_Count[z] <- sum(!is.na(which(CorrData[,13]==rownames(Molten_Salt_Types)[x]&CorrData[,14]==rownames(Purity)[y])))
	}
}
rm(x)
rm(y)
rm(z)
		#-------------ESTRAZIONE DATI SALI FUSI-----------------

#---------------------------------------------------------ESTRAZIONE DATI-----------------------------------------------------------------# 

#---------------------------------------------------GRAFICI (DEI DATI ESTRATTI)-----------------------------------------------------------# 

	#---------------------------BOX PLOTS------------------------------------------------------

		#-----------------GRAFICI TEMPERATURA----------------------
boxplot(Temperature, main = "Temperature Boxplot", xlab = "Temperature [C°]" , col = "red", border = "black", horizontal = TRUE, notch = TRUE)

				#------VS MATERIAL
proportion <- colSums(!is.na(Temperature_VS_Material[,7:1]))/sum(colSums(!is.na(Temperature_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Material[,7:1],main = "Temperature VS Material Boxplot - First Part",xlab = "Temperature [C�]",names = paste(colnames(Temperature_VS_Material[,7:1]),
"\n N�=",as.character(colSums(!is.na(Temperature_VS_Material[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Material[,14:8]))/sum(colSums(!is.na(Temperature_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Material[,14:8],main = "Temperature VS Material Boxplot - Second Part",xlab = "Temperature [C�]",names = paste(colnames(Temperature_VS_Material[,14:8]),
"\n N°=",as.character(colSums(!is.na(Temperature_VS_Material[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Material[,21:15]))/sum(colSums(!is.na(Temperature_VS_Material)))	
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Material[,21:15],main = "Temperature VS Material Boxplot - Third Part",xlab = "Temperature [C�]",names = paste(colnames(Temperature_VS_Material[,21:15]),
"\n N°=",as.character(colSums(!is.na(Temperature_VS_Material[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Material[,28:22]))/sum(colSums(!is.na(Temperature_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Material[,28:22],main = "Temperature VS Material Boxplot - Fourth Part",xlab = "Temperature [C�]",names = paste(colnames(Temperature_VS_Material[,28:22]),
"\n N°=",as.character(colSums(!is.na(Temperature_VS_Material[,28:22])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Material[,31:29]))/sum(colSums(!is.na(Temperature_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Material[,31:29],main = "Temperature VS Material Boxplot - Fifth Part",xlab = "Temperature [C�]",names = paste(colnames(Temperature_VS_Material[,31:29]),
"\n N°=",as.character(colSums(!is.na(Temperature_VS_Material[,31:29])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MATERIAL

				#------VS MOLTEN SALT
proportion <- colSums(!is.na(Temperature_VS_Molten_Salt[,3:1]))/sum(colSums(!is.na(Temperature_VS_Molten_Salt)))

par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Molten_Salt[,3:1],main = "Temperature VS Molten Salt Boxplot",xlab = "Temperature [C°]",names = paste(colnames(Temperature_VS_Molten_Salt[,3:1]),
"\n N°=",as.character(colSums(!is.na(Temperature_VS_Molten_Salt[,3:1])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MOLTEN SALT

				#------VS (UN)PURIFIED MOLTEN SALT
proportion <- colSums(!is.na(Temperature_VS_Purified_Molten_Salt[,4:1]))/sum(colSums(!is.na(Temperature_VS_Purified_Molten_Salt)))

par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Temperature_VS_Purified_Molten_Salt))!=0)))],
main = "Temperature VS (Un)Purified Molten Salt Boxplot",xlab = "Temperature [C°]",
names = paste(colnames(Temperature_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Temperature_VS_Purified_Molten_Salt))!=0)))]),
"\n N°=",
as.character(colSums(!is.na(Temperature_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Temperature_VS_Purified_Molten_Salt))!=0)))])))
[order(which(colSums(!is.na(Temperature_VS_Purified_Molten_Salt))!=0))]),
width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)
				#------VS (UN)PURIFIED MOLTEN SALT

				#------VS DIVE TIME
proportion <- colSums(!is.na(Temperature_VS_Exp_Dive_Times[,7:1]))/sum(colSums(!is.na(Temperature_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Exp_Dive_Times[,7:1],main = "Temperature VS Dive Time Boxplot - Firt Part",xlab = "Temperature [C°]",names = paste(colnames(Temperature_VS_Exp_Dive_Times[,7:1]),
"h \n N°=",as.character(colSums(!is.na(Temperature_VS_Exp_Dive_Times[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Exp_Dive_Times[,14:8]))/sum(colSums(!is.na(Temperature_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Exp_Dive_Times[,14:8],main = "Temperature VS Dive Time Boxplot - Second Part",xlab = "Temperature [C°]",names = paste(colnames(Temperature_VS_Exp_Dive_Times[,14:8]),
"h \n N°=",as.character(colSums(!is.na(Temperature_VS_Exp_Dive_Times[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Exp_Dive_Times[,21:15]))/sum(colSums(!is.na(Temperature_VS_Exp_Dive_Times)))	
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Exp_Dive_Times[,21:15],main = "Temperature VS Dive Time Boxplot - Third Part",xlab = "Temperature [C°]",names = paste(colnames(Temperature_VS_Exp_Dive_Times[,21:15]),
"h \n N°=",as.character(colSums(!is.na(Temperature_VS_Exp_Dive_Times[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Temperature_VS_Exp_Dive_Times[,29:22]))/sum(colSums(!is.na(Temperature_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Temperature_VS_Exp_Dive_Times[,29:22],main = "Temperature VS Dive Time Boxplot - Fourth Part",xlab = "Temperature [C°]",names = paste(colnames(Temperature_VS_Exp_Dive_Times[,29:22]),
"h \n N°=",as.character(colSums(!is.na(Temperature_VS_Exp_Dive_Times[,29:22])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS DIVE TIME

		#-----------------GRAFICI TEMPERATURA----------------------

		#-----------------GRAFICI DIVE TIME------------------------

boxplot(Dive_Time, main = "Dive Time Boxplot", xlab = "Dive Time [h]" , col = "red", border = "black", horizontal = TRUE, notch = TRUE)

				#------VS MATERIAL
proportion <- colSums(!is.na(Dive_Time_VS_Material[,7:1]))/sum(colSums(!is.na(Dive_Time_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Material[,7:1],main = "Dive Time Boxplot VS Material Boxplot - First Part",xlab = "Dive_Time [h]",names = paste(colnames(Dive_Time_VS_Material[,7:1]),
"\n N°=",as.character(colSums(!is.na(Dive_Time_VS_Material[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Material[,14:8]))/sum(colSums(!is.na(Dive_Time_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Material[,14:8],main = "Dive Time Boxplot VS Material Boxplot - Second Part",xlab = "Dive Time [h]",names = paste(colnames(Dive_Time_VS_Material[,14:8]),
"\n N°=",as.character(colSums(!is.na(Dive_Time_VS_Material[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Material[,21:15]))/sum(colSums(!is.na(Dive_Time_VS_Material)))	
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Material[,21:15],main = "Dive Time Boxplot VS Material Boxplot - Third Part",xlab = "Dive_Time [h]",names = paste(colnames(Dive_Time_VS_Material[,21:15]),
"\n N°=",as.character(colSums(!is.na(Dive_Time_VS_Material[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Material[,28:22]))/sum(colSums(!is.na(Dive_Time_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Material[,28:22],main = "Dive Time Boxplot VS Material Boxplot - Fourth Part",xlab = "Dive_Time [h]",names = paste(colnames(Dive_Time_VS_Material[,28:22]),
"\n N°=",as.character(colSums(!is.na(Dive_Time_VS_Material[,28:22])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Material[,31:29]))/sum(colSums(!is.na(Dive_Time_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Material[,31:29],main = "Dive Time Boxplot VS Material Boxplot - Fifth Part",xlab = "Dive_Time [h]",names = paste(colnames(Dive_Time_VS_Material[,31:29]),
"\n N°=",as.character(colSums(!is.na(Dive_Time_VS_Material[,31:29])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MATERIAL

				#------VS MOLTEN SALT
proportion <- colSums(!is.na(Dive_Time_VS_Molten_Salt[,3:1]))/sum(colSums(!is.na(Dive_Time_VS_Molten_Salt)))
par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Molten_Salt[,3:1],main = "Dive Time Boxplot VS Molten Salt",xlab = "Dive_Time [h]",names = paste(colnames(Dive_Time_VS_Molten_Salt[,3:1]),
"\n N°=",as.character(colSums(!is.na(Dive_Time_VS_Molten_Salt[,3:1])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MOLTEN SALT

				#------VS (UN)PURIFIED MOLTEN SALT
proportion <- colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt[,4:1]))/sum(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt)))
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt))!=0)))],
main = "Dive_Time Boxplot VS (Un)Purified Molten Salt",xlab = "Dive_Time [h]",
names = paste(colnames(Dive_Time_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt))!=0)))]),
"\n N°=",
as.character(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt))!=0)))])))
[order(which(colSums(!is.na(Dive_Time_VS_Purified_Molten_Salt))!=0))]),
width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)
				#------VS (UN)PURIFIED MOLTEN SALT

				#------VS TEMPERATURE
proportion <- colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,7:1]))/sum(colSums(!is.na(Dive_Time_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Exp_Temperatures[,7:1],main = "Dive Time VS Temperature Boxplot - First Part",xlab = "Dive Time [h]",names = paste(colnames(Dive_Time_VS_Exp_Temperatures[,7:1]),
"°C \n N°=",as.character(colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,14:8]))/sum(colSums(!is.na(Dive_Time_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Exp_Temperatures[,14:8],main = "Dive Time VS Temperature Boxplot - Second Part",xlab = "Dive Time [h]",names = paste(colnames(Dive_Time_VS_Exp_Temperatures[,14:8]),
"°C \n N°=",as.character(colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,21:15]))/sum(colSums(!is.na(Dive_Time_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Exp_Temperatures[,21:15],main = "Dive Time VS Temperature Boxplot - Third Part",xlab = "Dive Time [h]",names = paste(colnames(Dive_Time_VS_Exp_Temperatures[,21:15]),
"°C \n N°=",as.character(colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,24:22]))/sum(colSums(!is.na(Dive_Time_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Dive_Time_VS_Exp_Temperatures[,24:22],main = "Dive Time VS Temperature Boxplot - Fourth Part",xlab = "Dive Time [h]",names = paste(colnames(Dive_Time_VS_Exp_Temperatures[,24:22]),
"°C \n N°=",as.character(colSums(!is.na(Dive_Time_VS_Exp_Temperatures[,24:22])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS TEMPERATURE

		#---------------GRAFICI DIVE TIME--------------------

		#-------------GRAFICI DATI IMPURITA'-----------------
Total_Impurities 																	
boxplot(Total_Impurities,main = "Total Impurities Boxplot",xlab = paste("Total_Impurities [ppmw] data N°=",as.character(length(Total_Impurities[,]))),
col = "red",border = "black",horizontal = TRUE,notch = FALSE)

boxplot(Total_Impurities[c(which(Total_Impurities[,1]<2000)),1],main = "Total Impurities Boxplot",
xlab = paste("Total_Impurities [ppmw] - data points for ppmw < 2000 N°=",as.character(length(Total_Impurities[c(which(Total_Impurities[,1]<2000)),]))),
col = "red",border = "black",horizontal = TRUE,notch = FALSE)

				#------VS MOLTEN SALT
proportion <- colSums(!is.na(Total_Impurities_VS_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Molten_Salt[1,])))]))/sum(colSums(!is.na(Total_Impurities_VS_Molten_Salt)))
par(cex.axis=1)																													 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Total_Impurities_VS_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Molten_Salt[1,])))],
main = "Total Impurities VS Molten Salt Boxplot",
xlab = "Total_Impurities [ppmw]",
names = paste(colnames(Total_Impurities_VS_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Molten_Salt[1,])))]),
"\n N°=",
as.character(colSums(!is.na(Total_Impurities_VS_Molten_Salt))[which(Total_Impurities_VS_Molten_Salt[1,]>0)])),
width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)


boxplot(Total_Impurities_VS_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Molten_Salt[1,])))],
main = "Total Impurities VS Molten Salt Boxplot",
xlab = "Total_Impurities [ppmw]",
names = paste(colnames(Total_Impurities_VS_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Molten_Salt[1,])))]),
"\n N°=",
as.character(colSums(!is.na(Total_Impurities_VS_Molten_Salt[,]))[which(Total_Impurities_VS_Molten_Salt[1,]>0)])),
width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)
				#------VS MOLTEN SALT

				#------VS PURIFIED MOLTEN SALT
proportion <- colSums(!is.na(Total_Impurities_VS_Purified_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Purified_Molten_Salt[1,])))]))/sum(colSums(!is.na(Total_Impurities_VS_Purified_Molten_Salt)))
par(cex.axis=1)																													 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Total_Impurities_VS_Purified_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Purified_Molten_Salt[1,])))],
main = "Total Impurities VS Purified Molten Salt Boxplot",
xlab = "Total_Impurities [ppmw]",
names = paste(colnames(Total_Impurities_VS_Purified_Molten_Salt[,rev(which(!is.na(Total_Impurities_VS_Purified_Molten_Salt[1,])))]),
"\n N°=",
as.character(colSums(!is.na(Total_Impurities_VS_Purified_Molten_Salt))[which(Total_Impurities_VS_Purified_Molten_Salt[1,]>0)])),
width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)
				#------VS PURIFIED MOLTEN SALT

		#-------------GRAFICI DATI IMPURITA'-----------------

		#-----GRAFICI DATI PRINCIPALI ELEMENTI DI LEGA-------

boxplot(Ni_Cr_Mo_Fe_Comp, main = "Ni_Cr_Mo_Fe_Comp Boxplot", xlab = "comp [%mol]" , col = rep("red", time= ncol(Ni_Cr_Mo_Fe_Comp)), border = "black", horizontal = TRUE, notch = FALSE)


				#------VS MATERIAL
Ni_Cr_Mo_Fe_Comp_VS_Material
				#------VS MATERIAL
		#-----GRAFICI DATI PRINCIPALI ELEMENTI DI LEGA-------

		#----GRAFICI ENERGIE LIBERE DI GIBBS (ELLINGHAM)-----
 
boxplot(Ellingham_G, main = "Gibbs free EN. Ellingham Boxplot", xlab = "[kcal/gfw]" , col = "red", border = "black", horizontal = TRUE, notch = FALSE)

				#------VS MATERIAL
proportion <- colSums(!is.na(Ellingham_G_VS_Material[,7:1]))/sum(colSums(!is.na(Ellingham_G_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Ellingham_G_VS_Material[,7:1],main = "Gibbs free En. Ellingham VS Material Boxplot - First Part", xlab = "[kcal/gfw]",names = paste(colnames(Ellingham_G_VS_Material[,7:1]),
"\n N°=",as.character(colSums(!is.na(Ellingham_G_VS_Material[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Ellingham_G_VS_Material[,14:8]))/sum(colSums(!is.na(Ellingham_G_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Ellingham_G_VS_Material[,14:8],main = "Gibbs free En. Ellingham VS Material Boxplot - Second Part", xlab = "[kcal/gfw]",names = paste(colnames(Ellingham_G_VS_Material[,14:8]),
"\n N°=",as.character(colSums(!is.na(Ellingham_G_VS_Material[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Ellingham_G_VS_Material[,21:15]))/sum(colSums(!is.na(Ellingham_G_VS_Material)))	
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Ellingham_G_VS_Material[,21:15],main = "Gibbs free En. Ellingham VS Material Boxplot - Third Part", xlab = "[kcal/gfw]",names = paste(colnames(Ellingham_G_VS_Material[,21:15]),
"\n N°=",as.character(colSums(!is.na(Ellingham_G_VS_Material[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Ellingham_G_VS_Material[,28:22]))/sum(colSums(!is.na(Ellingham_G_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Ellingham_G_VS_Material[,28:22],main = "Gibbs free En. Ellingham VS Material Boxplot - Fourth Part", xlab = "[kcal/gfw]",names = paste(colnames(Ellingham_G_VS_Material[,28:22]),
"\n N°=",as.character(colSums(!is.na(Ellingham_G_VS_Material[,28:22])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Ellingham_G_VS_Material[,31:29]))/sum(colSums(!is.na(Ellingham_G_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Ellingham_G_VS_Material[,31:29],main = "Gibbs free En. Ellingham VS Material Boxplot - Fifth Part", xlab = "[kcal/gfw]",names = paste(colnames(Ellingham_G_VS_Material[,31:29]),
"\n N°=",as.character(colSums(!is.na(Ellingham_G_VS_Material[,31:29])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MATERIAL


		#----GRAFICI ENERGIE LIBERE DI GIBBS (ELLINGHAM)-----

		#--------------GRAFICI DI WEIGHT LOSS----------------
boxplot(Weight_Loss, main = "Weight Loss  Boxplot", xlab = "weight loss [mg/cm^2]" , col = "red", border = "black", horizontal = TRUE, notch = FALSE)

boxplot(Weight_Loss[Weight_Loss> -10 & Weight_Loss< 10],
 main = "Weight Loss Boxplot", xlab = "weight loss (between -10/10) [mg/cm^2]" , col = "red", border = "black", horizontal = TRUE, notch = FALSE)

				#------VS MATERIAL
proportion <- colSums(!is.na(Weight_Loss_VS_Material[,7:1]))/sum(colSums(!is.na(Weight_Loss_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Material[,7:1],main = "Weight Loss VS Material Boxplot - First Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Material[,7:1]),
"\n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Material[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Material[,14:8]))/sum(colSums(!is.na(Weight_Loss_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Material[,14:8],main = "Weight Loss VS Material Boxplot - Second Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Material[,14:8]),
"\n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Material[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Material[,21:15]))/sum(colSums(!is.na(Weight_Loss_VS_Material)))	
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Material[,21:15],main = "Weight Loss VS Material Boxplot - Third Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Material[,21:15]),
"\n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Material[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Material[,29:22]))/sum(colSums(!is.na(Weight_Loss_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Material[,29:22],main = "Weight Loss VS Material Boxplot - Fourth Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Material[,29:22]),
"\n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Material[,29:22])))[1:8]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MATERIAL

				#------VS MOLTEN SALT
proportion <- colSums(!is.na(Weight_Loss_VS_Molten_Salt[,3:1]))/sum(colSums(!is.na(Weight_Loss_VS_Molten_Salt)))

par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Molten_Salt[,3:1],main = "Weight Loss VS Molten Salt Boxplot",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Molten_Salt[,3:1]),
"\n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Molten_Salt[,3:1])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MOLTEN SALT

				#------VS PURIFIED MOLTEN SALT
proportion <- colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt[,4:1]))/sum(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt)))

par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt))!=0)))],
main = "Weight Loss VS (Un)Purified Molten Salt Boxplot",xlab = "Weight Loss [mg/cm^2]",
names = paste(colnames(Weight_Loss_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt))!=0)))]),
"\n N°=",
as.character(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt))!=0)))])))
[order(which(colSums(!is.na(Weight_Loss_VS_Purified_Molten_Salt))!=0))]),
width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)
																								# Rimozione variabile inutile
				#------VS PURIFIED MOLTEN SALT

				#------VS TEMPERATURES
proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,7:1]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Temperatures[,7:1],main = "Weight Loss VS Temperature Boxplot - First Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Temperatures[,7:1]),
"°C \n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,14:8]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Temperatures[,14:8],main = "Weight Loss VS Temperature Boxplot - Second Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Temperatures[,14:8]),
"°C \n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,21:15]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Temperatures[,21:15],main = "Weight Loss VS Temperature Boxplot - Third Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Temperatures[,21:15]),
"°C \n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,24:22]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Temperatures[,24:22],main = "Weight Loss VS Temperature Boxplot - Fourth Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Temperatures[,24:22]),
"°C\n N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,24:22])))[1:3]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)																									# ° uguale ai valori presenti nel dataset nella colonna dei weigth loss, corrispondenti alla temperatura sperimentale x
				#------VS TEMPERATURES

				#------VS DIVE TIME
proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times[,7:1]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Dive_Times[,7:1],main = "Weight Loss VS Dive Times Boxplot - First Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Dive_Times[,7:1]),
"h N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times[,14:8]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Dive_Times[,14:8],main = "Weight Loss VS Dive Times Boxplot - Second Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Dive_Times[,14:8]),
"h N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Temperatures[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times[,19:15]))/sum(colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Weight_Loss_VS_Exp_Dive_Times[,19:15],main = "Weight Loss VS Dive Times Boxplot - Third Part",xlab = "Weight Loss [mg/cm^2]",names = paste(colnames(Weight_Loss_VS_Exp_Dive_Times[,19:15]),
"h N°=",as.character(colSums(!is.na(Weight_Loss_VS_Exp_Dive_Times[,19:15])))[1:5]),width=proportion ,col = rep("red", times=5),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS DIVE TIME

		#--------------GRAFICI DI WEIGHT LOSS----------------

		#------------GRAFICI DI DEPLETION DEPTH--------------
boxplot(Max_Depletion, main = "Max Depletion  Boxplot", xlab = "Max Depletion [um]" , col = "red", border = "black", horizontal = TRUE, notch = FALSE)

boxplot(Max_Depletion[Max_Depletion> -50 & Max_Depletion< 10],
 main = "Max Depletion Boxplot", xlab = "Max Depletion (between -50/10) [um]" , col = "red", border = "black", horizontal = TRUE, notch = FALSE)

				#------VS MATERIAL
proportion <- colSums(!is.na(Max_Depletion_VS_Material[,7:1]))/sum(colSums(!is.na(Max_Depletion_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Material[,7:1],main = "Max Depletion VS Material Boxplot - First Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Material[,7:1]),
"\n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Material[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Material[,14:8]))/sum(colSums(!is.na(Max_Depletion_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Material[,14:8],main = "Max Depletion VS Material Boxplot - Second Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Material[,14:8]),
"\n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Material[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Material[,21:15]))/sum(colSums(!is.na(Max_Depletion_VS_Material)))	
par(cex.axis=0.6)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Material[,21:15],main = "Max Depletion VS Material Boxplot - Third Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Material[,21:15]),
"\n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Material[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Material[,23:22]))/sum(colSums(!is.na(Max_Depletion_VS_Material)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Material[,23:22],main = "Max Depletion VS Material Boxplot - Fourth Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Material[,23:22]),
"\n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Material[,23:22])))[1:2]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS MATERIAL

				#------VS MOLTEN SALT
proportion <- colSums(!is.na(Max_Depletion_VS_Molten_Salt[,ncol(Max_Depletion_VS_Molten_Salt):1]))/sum(colSums(!is.na(Max_Depletion_VS_Molten_Salt)))

par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Molten_Salt[,ncol(Max_Depletion_VS_Molten_Salt):1],
	  main = "Max Depletion VS Molten Salt Boxplot",xlab = "Max Depletion [um]",
	  names = paste(colnames(Max_Depletion_VS_Molten_Salt[,ncol(Max_Depletion_VS_Molten_Salt):1]),
			    "\n N°=",
			    as.character(colSums(!is.na(Max_Depletion_VS_Molten_Salt[,ncol(Max_Depletion_VS_Molten_Salt):1])))[1:ncol(Max_Depletion_VS_Molten_Salt)]),
	  width=proportion ,col = rep("red", times=7),border = "black",horizontal = TRUE,notch = FALSE)
				#------VS MOLTEN SALT

				#------VS PURIFIED MOLTEN SALT
proportion <- colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt[,ncol(Max_Depletion_VS_Molten_Salt):1]))/sum(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt)))

par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt))!=0)))],
main = "Max Depletion VS (Un)Purified Molten Salt Boxplot",xlab = "Max Depletion [um]",
	  names = paste(colnames(Max_Depletion_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt))!=0)))]),
			    "\n N°=",
			    as.character(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt[,rev(order(which(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt))!=0)))])))
					     [order(which(colSums(!is.na(Max_Depletion_VS_Purified_Molten_Salt))!=0))]),
idth=proportion ,col = rep("red", times=ncol(Max_Depletion_VS_Purified_Molten_Salt)),border = "black",horizontal = TRUE,notch = FALSE)
				#------VS PURIFIED MOLTEN SALT

				#------VS TEMPERATURES
proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,7:1]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Temperatures[,7:1],main = "Max Depletion VS Temperature Boxplot - First Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Temperatures[,7:1]),
"°C \n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,14:8]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Temperatures[,14:8],main = "Max Depletion VS Temperature Boxplot - Scond Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Temperatures[,14:8]),
"°C \n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,21:15]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Temperatures[,21:15],main = "Max Depletion VS Temperature Boxplot - Third Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Temperatures[,21:15]),
"°C \n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,23:22]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Temperatures[,23:22],main = "Max Depletion VS Temperature Boxplot - Fourth Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Temperatures[,23:22]),
"°C\n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,23:22])))[1:2]),width=proportion ,col = rep("red", times=2),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS TEMPERATURES

				#------VS DIVE TIME
proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times[,7:1]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Dive_Times[,7:1],main = "Max Depletion VS Dive Time Boxplot - First Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Dive_Times[,7:1]),
"h N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times[,7:1])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times[,14:8]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Dive_Times[,14:8],main = "Max Depletion VS Dive Time Boxplot - Second Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Dive_Times[,14:8]),
"h N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,14:8])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times[,21:15]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Dive_Times[,21:15],main = "Max Depletion VS Dive Time Boxplot - Third Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Dive_Times[,21:15]),
"h N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times[,21:15])))[1:7]),width=proportion ,col = rep("red", times=7),border = "black",
horizontal = TRUE,notch = FALSE)

proportion <- colSums(!is.na(Max_Depletion_VS_Exp_Temperatures[,23:22]))/sum(colSums(!is.na(Max_Depletion_VS_Exp_Temperatures)))	
par(cex.axis=0.7)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(Max_Depletion_VS_Exp_Dive_Times[,23:22],main = "Max Depletion VS Dive Time Boxplot - Fourth Part",xlab = "Max Depletion [um]",names = paste(colnames(Max_Depletion_VS_Exp_Dive_Times[,23:22]),
"h\n N°=",as.character(colSums(!is.na(Max_Depletion_VS_Exp_Dive_Times[,23:22])))[1:2]),width=proportion ,col = rep("red", times=2),border = "black",
horizontal = TRUE,notch = FALSE)
				#------VS DIVE TIME

		#------------GRAFICI DI DEPLETION DEPTH--------------

	#---------------------------ISTOGRAMMI------------------------------------------------------

		#------------------GRAFICI SALI FUSI-----------------------
par(cex.axis=0.7)
			# proportion <- Molten_Salt_Count[!is.na(Molten_Salt_Count)]/sum(Molten_Salt_Count[!is.na(Molten_Salt_Count)])
barplot(as.numeric(Molten_Salt_Count),
names.arg = paste(names(Molten_Salt_Count),"\n N°=",as.numeric(Molten_Salt_Count)),
# width=proportion,
col=rainbow(2*length(Molten_Salt_Count)),
ylim=range(pretty(c(0, as.numeric(Molten_Salt_Count)))),
main = "Molten Salt Count")
		#------------------GRAFICI SALI FUSI-----------------------

		#----------------GRAFICI EXP VELOCITIES--------------------
par(cex.axis=0.7)
			# proportion <- Exp_Velocities[!is.na(Exp_Velocities)]/sum(Exp_Velocities[!is.na(Exp_Velocities)])
barplot(as.numeric(Exp_Velocities),
names.arg = paste("v=",names(Exp_Velocities),"m/s \n N°=",as.numeric(Exp_Velocities)),
# width=proportion,
col=rainbow(2*length(Exp_Velocities)),
ylim=range(pretty(c(0, as.numeric(Exp_Velocities)))),
main = "Experimental Velocities Count")
		#----------------GRAFICI EXP VELOCITIES--------------------

		#-----------------GRAFICI TEMPERATURE----------------------
par(cex.axis=1)
			# proportion <- Exp_Temperatures[!is.na(Exp_Velocities[1:12])]/sum(Exp_Temperatures[!is.na(Exp_Temperatures)])
barplot(as.numeric(Exp_Temperatures)[1:12],
names.arg = paste("T=",names(Exp_Temperatures)[1:12],"°C \n N°=",as.numeric(Exp_Temperatures)[1:12]),
# width=proportion,
col=rainbow(2*length(Exp_Temperatures)[1:12]),
ylim=range(pretty(c(0, as.numeric(Exp_Temperatures)[1:12]))),
main = "Experimental Temperatures Count - First Half")

par(cex.axis=1)
			# proportion <- Exp_Temperatures[!is.na(Exp_Velocities[13:24])]/sum(Exp_Temperatures[!is.na(Exp_Temperatures)])
barplot(as.numeric(Exp_Temperatures)[13:24],
names.arg = paste("T=",names(Exp_Temperatures)[13:24],"°C \n N°=",as.numeric(Exp_Temperatures)[13:24]),
# width=proportion,
col=rainbow(2*length(Exp_Temperatures[13:24])),
ylim=range(pretty(c(0, as.numeric(Exp_Temperatures)[13:24]))),
main = "Experimental Temperatures Count - Second Half")
		#-----------------GRAFICI TEMPERATURE---------------------

		#-----------------GRAFICI DIVE TIMES----------------------
par(cex.axis=1)
			# proportion <- Exp_Dive_Times[!is.na(Exp_Velocities[1:12])]/sum(Exp_Dive_Times[!is.na(Exp_Dive_Times)])
barplot(as.numeric(Exp_Dive_Times)[1:15],
names.arg = paste("t=",names(Exp_Dive_Times)[1:15],"h \n N°=",as.numeric(Exp_Dive_Times)[1:15]),
# width=proportion,
col=rainbow(2*length(Exp_Dive_Times)[1:15]),
ylim=range(pretty(c(0, as.numeric(Exp_Dive_Times)[1:15]))),
main = "Experimental Dive Times Count - First Half")

par(cex.axis=1)
			# proportion <- Exp_Dive_Times[!is.na(Exp_Velocities[16:29])]/sum(Exp_Dive_Times[!is.na(Exp_Dive_Times)])
barplot(as.numeric(Exp_Dive_Times)[16:29],
names.arg = paste("T=",names(Exp_Dive_Times)[16:29],"h \n N°=",as.numeric(Exp_Dive_Times)[16:29]),
# width=proportion,
col=rainbow(2*length(Exp_Dive_Times[16:29])),
ylim=range(pretty(c(0, as.numeric(Exp_Dive_Times)[16:29]))),
main = "Experimental Dive Times Count - Second Half")
		#-----------------GRAFICI DIVE TIMES---------------------

		#--------------GRAFICI RADIATION EXPOSURE----------------
par(cex.axis=0.7)
			# proportion <- Exp_Radiations[!is.na(Exp_Velocities)]/sum(Exp_Radiations[!is.na(Exp_Radiations)])
barplot(as.numeric(Exp_Radiations),
names.arg = paste("rad=",names(Exp_Radiations)," \n N°=",as.numeric(Exp_Radiations)),
# width=proportion,
col=rainbow(2*length(Exp_Radiations)),
ylim=range(pretty(c(0, as.numeric(Exp_Radiations)))),
main = "Experimental Radiations Count")
		#--------------GRAFICI RADIATION EXPOSURE---------------

		#----------------GRAFICI MATERIAL TYPE------------------
par(cex.axis=0.7)
			# proportion <- Material_Types[!is.na(Material_Types[1:16])]/sum(Material_Types[!is.na(Material_Types)])
barplot(as.numeric(Material_Types)[1:13],
names.arg = paste(names(Material_Types)[1:13],"\n N�=",as.numeric(Material_Types)[1:13]),
# width=proportion,
col=rainbow(2*length(Material_Types[1:13])),
ylim=range(pretty(c(0, as.numeric(Material_Types)[1:13]))),
main = "Material Types Count - First Half")

par(cex.axis=0.7)
			# proportion <- Material_Types[!is.na(Material_Types[17:31])]/sum(Material_Types[!is.na(Material_Types)])
barplot(as.numeric(Material_Types)[14:25],
names.arg = paste(names(Material_Types)[14:25],"\n N�=",as.numeric(Material_Types)[14:25]),
# width=proportion,
col=rainbow(2*length(Material_Types[14:25])),
ylim=range(pretty(c(0, as.numeric(Material_Types)[14:25]))),
main = "Material Types Count - Second Half")

				#------VS CRUCIBLE TYPE
XX <- t(matrix(unlist(strsplit(Material_and_Crucible_Types," +")),  nrow=4, ncol=length(Material_Types_VS_Crucible_Types)))

par(cex.axis=0.5)
			# proportion <- Material_Types_VS_Crucible_Types[1:sum(!is.na(Material_Types_VS_Crucible_Types[1:16,])),1]/sum(Material_Types_VS_Crucible_Types[!is.na(Material_Types_VS_Crucible_Types)])
barplot(Material_Types_VS_Crucible_Types[1:16],
names.arg = paste(XX[1:16,1]," with","\n",XX[1:16,3]," Crucible","\n N�=",Material_Types_VS_Crucible_Types[1:16]),
# width=proportion,
col=rainbow(2*length(Material_Types_VS_Crucible_Types[1:16])),
ylim=range(pretty(c(0, Material_Types_VS_Crucible_Types[1:16]))),
main = "Material Types VS Crucible Types Count - First part")

par(cex.axis=0.5)
			# proportion <- Material_Types_VS_Crucible_Types[1:sum(!is.na(Material_Types_VS_Crucible_Types[17:32,])),1]/sum(Material_Types_VS_Crucible_Types[!is.na(Material_Types_VS_Crucible_Types)])
barplot(Material_Types_VS_Crucible_Types[17:32,1],
names.arg = paste(XX[17:32,1],"\n with","\n",XX[17:32,3],"\n Crucible","\n N°=",Material_Types_VS_Crucible_Types[17:32,1]),
# width=proportion,
col=rainbow(2*length(Material_Types_VS_Crucible_Types[17:32,])),
ylim=range(pretty(c(0, Material_Types_VS_Crucible_Types[17:32,]))),
main = "Material Types VS Crucible Types Count - Second part")

par(cex.axis=0.5)
			# proportion <- Material_Types_VS_Crucible_Types[1:sum(!is.na(Material_Types_VS_Crucible_Types[33:48,])),1]/sum(Material_Types_VS_Crucible_Types[!is.na(Material_Types_VS_Crucible_Types)])
barplot(Material_Types_VS_Crucible_Types[33:48,1],
names.arg = paste(XX[33:48,1],"\n with","\n",XX[33:48,3],"\n Crucible","\n N°=",Material_Types_VS_Crucible_Types[33:48,1]),
# width=proportion,
col=rainbow(2*length(Material_Types_VS_Crucible_Types[33:48,])),
ylim=range(pretty(c(0, Material_Types_VS_Crucible_Types[33:48,]))),
main = "Material Types VS Crucible Types Count - Third part")

par(cex.axis=0.5)
			# proportion <- Material_Types_VS_Crucible_Types[1:sum(!is.na(Material_Types_VS_Crucible_Types[49:39,])),1]/sum(Material_Types_VS_Crucible_Types[!is.na(Material_Types_VS_Crucible_Types)])
barplot(Material_Types_VS_Crucible_Types[49:39,1],
names.arg = paste(XX[49:39,1],"\n with","\n",XX[49:39,3],"\n Crucible","\n N°=",Material_Types_VS_Crucible_Types[49:39,1]),
# width=proportion,
col=rainbow(2*length(Material_Types_VS_Crucible_Types[49:39,])),
ylim=range(pretty(c(0, Material_Types_VS_Crucible_Types[49:39,]))),
main = "Material Types VS Crucible Types Count - Third part")
				#------VS CRUCIBLE TYPE

				#------VS PURIFIED MOLTEN SALT
XX <- t(matrix(unlist(strsplit(Material_and_Purified_Molten_Salts," +")),  nrow=4, ncol=length(Material_Types_VS_Purified_Molten_Salts)))

par(cex.axis=0.4)
			# proportion <- Material_Types_VS_Purified_Molten_Salts[1:sum(!is.na(Material_Types_VS_Purified_Molten_Salts[1:16,])),1]/sum(Material_Types_VS_Purified_Molten_Salts[!is.na(Material_Types_VS_Purified_Molten_Salts)])
barplot(Material_Types_VS_Purified_Molten_Salts[1:16],
names.arg = paste(XX[1:16,1],"\n with","\n",XX[1:16,3],"\n Crucible","\n N°=",Material_Types_VS_Purified_Molten_Salts[1:16]),
# width=proportion,
col=rainbow(length(Material_Types_VS_Purified_Molten_Salts[1:16])),
ylim=range(pretty(c(0, Material_Types_VS_Purified_Molten_Salts[1:16]))),
main = "Material Types VS Purified Molten Salts - First part")

par(cex.axis=0.4)
			# proportion <- Material_Types_VS_Purified_Molten_Salts[1:sum(!is.na(Material_Types_VS_Purified_Molten_Salts[17:32,])),1]/sum(Material_Types_VS_Purified_Molten_Salts[!is.na(Material_Types_VS_Purified_Molten_Salts)])
barplot(Material_Types_VS_Purified_Molten_Salts[17:32,1],
names.arg = paste(XX[17:32,1],"\n with","\n",XX[17:32,3],"\n Crucible","\n N°=",Material_Types_VS_Purified_Molten_Salts[17:32,1]),
# width=proportion,
col=rainbow(2*length(Material_Types_VS_Purified_Molten_Salts[17:32,])),
ylim=range(pretty(c(0, Material_Types_VS_Purified_Molten_Salts[17:32,]))),
main = "Material Types VS Purified Molten Salts - Second part")

par(cex.axis=0.5)
			# proportion <- Material_Types_VS_Purified_Molten_Salts[1:sum(!is.na(Material_Types_VS_Purified_Molten_Salts[33:48,])),1]/sum(Material_Types_VS_Purified_Molten_Salts[!is.na(Material_Types_VS_Purified_Molten_Salts)])
barplot(Material_Types_VS_Purified_Molten_Salts[33:47,1],
names.arg = paste(XX[33:47,1],"\n with","\n",XX[33:47,3],"\n Crucible","\n N°=",Material_Types_VS_Purified_Molten_Salts[33:47,1]),
# width=proportion,
col=rainbow(2*length(Material_Types_VS_Purified_Molten_Salts[33:47,])),
ylim=range(pretty(c(0, Material_Types_VS_Purified_Molten_Salts[33:47,]))),
main = "Material Types VS Purified Molten Salts Count - Third part")
				#------VS PURIFIED MOLTEN SALT

		#----------------GRAFICI MATERIAL TYPE------------------


		#--------------GRAFICI DI WEIGHT LOSS----------------
				#------RANGES
par(cex.axis=0.7)
		#	proportion <- Weight_Loss_ranges[!is.na(Weight_Loss_ranges)]/sum(Weight_Loss_ranges[!is.na(Weight_Loss_ranges)])
barplot(as.numeric(Weight_Loss_ranges),
names.arg = paste("range:",colnames(Weight_Loss_ranges),"mg/cm^2 \n N°=",as.numeric(Weight_Loss_ranges)),
col=rainbow(2*ncol(Weight_Loss_ranges)),
ylim=range(pretty(c(0, as.numeric(Weight_Loss_ranges)))),
main = "Weight Loss ranges Count")																								# Rimozione variabile inutile																								
				#------RANGES

				#------RANGES VS DEPLETION
data <-c(as.matrix(Weight_Loss_ranges_VS_Depletion))
labels <- paste("range (mg/cm^2) \n",rep(colnames(Weight_Loss_ranges_VS_Depletion), each=2),
		    " \n",
		    rep(rownames(Weight_Loss_ranges_VS_Depletion), times=ncol(Weight_Loss_ranges_VS_Depletion)),
		    "\n N°=",
		    c(as.matrix(Weight_Loss_ranges_VS_Depletion)))
colors <- rainbow(2*ncol(Weight_Loss_ranges_VS_Depletion))

data_1<-data[1:ceiling(length(data)/2)]
labels_1 <- labels[1:ceiling(length(data)/2)]
colors_1 <-colors[1:ceiling(length(data)/2)]
y_axis_1 <- range(pretty(c(0, max(data_1))))

par(cex.axis=0.5)
barplot(data_1, names.arg=labels_1, col=colors_1, ylim=y_axis_1, main = "Weight Loss ranges Count - first half")

data_2<-data[1+ceiling(length(data)/2):length(data)-2]
labels_2 <- labels[1+ceiling(length(data)/2):length(data)-2]
colors_2 <- colors[1+ceiling(length(data)/2):length(data)-2]
y_axis_2 <- range(pretty(c(0, max(data_2))))

par(cex.axis=0.4)
barplot(data_2, names.arg=labels_2, col=colors_2, ylim=y_axis_2, main = "Weight Loss ranges Count - second half")
				#------RANGES VS DEPLETION

		#--------------GRAFICI DI WEIGHT LOSS----------------

		#------------GRAFICI DI DEPLETION DEPTH--------------
				#------RANGES
par(cex.axis=0.7)
		#	proportion <- Max_Depletion_ranges[!is.na(Max_Depletion_ranges)]/sum(Max_Depletion_ranges[!is.na(Max_Depletion_ranges)])
barplot(as.numeric(Max_Depletion_ranges),
names.arg = paste("\n range (um) : \n",colnames(Max_Depletion_ranges),"\n N°=",as.numeric(Max_Depletion_ranges)),
col=rainbow(2*ncol(Max_Depletion_ranges)),
ylim=range(pretty(c(0, as.numeric(Max_Depletion_ranges)))),
main = "Max Depletion Depth ranges Count")	
				#------RANGES

				#------RANGES VS WEIGHT LOSS
data <-c(as.matrix(Max_Depletion_ranges_VS_Weight_Loss))
labels <- paste("range um \n",rep(colnames(Max_Depletion_ranges_VS_Weight_Loss), each=2),
		    " \n",
		    rep(rownames(Max_Depletion_ranges_VS_Weight_Loss), times=ncol(Max_Depletion_ranges_VS_Weight_Loss)),
		    "\n N°=",
		    c(as.matrix(Max_Depletion_ranges_VS_Weight_Loss)))
colors <- rainbow(2*ncol(Max_Depletion_ranges_VS_Weight_Loss))

data_1<-data[1:ceiling(length(data)/2)]
labels_1 <- labels[1:ceiling(length(data)/2)]
colors_1 <-colors[1:ceiling(length(data)/2)]
y_axis_1 <- range(pretty(c(0, max(data_1))))

par(cex.axis=0.5)
barplot(data_1, names.arg=labels_1, col=colors_1, ylim=y_axis_1, main = "Max Depletion Depth ranges Count - First half")

data_2<-data[1+ceiling(length(data)/2):length(data)-2]
labels_2 <- labels[1+ceiling(length(data)/2):length(data)-2]
colors_2 <- colors[1+ceiling(length(data)/2):length(data)-2]
y_axis_2 <- range(pretty(c(0, max(data_2))))

par(cex.axis=0.5)
barplot(data_2, names.arg=labels_2, col=colors_2, ylim=y_axis_2, main = "Max Depletion Depth ranges Count - Second Half")
				#------RANGES VS WEIGHT LOSS
		#------------GRAFICI DI DEPLETION DEPTH--------------
	#---------------------------ISTOGRAMMI------------------------------------------------------

	#-----------------------CORRELATION MATRIX--------------------------------------------------
#use : temperature(2) - dive time(3) - density(6)[as proxy for material] - Ni/Cr/Mo/Fe %mol (7-8-9-10) 
#	  - Ellingham mat/crucible (11-12[devi convertirlo in numeric]) -	max depth(38) - weigth loss(39) 
# CorrData[,c(2,3,6,7,8,9,10,11,12,38,39)]

	#-----------------------CORRELATION MATRIX--------------------------------------------------

#---------------------------------------------------GRAFICI (DEI DATI ESTRATTI)-----------------------------------------------------------# 

#------------------------------------------------CREAZIONE MACHINE LEARNING MODELS--------------------------------------------------------#



#------------------------------------------------CREAZIONE MACHINE LEARNING MODELS--------------------------------------------------------#


both <- CorrData[c(which( ( CorrData[,38]!=0 & !is.na(CorrData[,38]) ) & ( CorrData[,39]!=0 & !is.na(CorrData[,39]) ) ) ),38:39]

ordered_both <- both[order(both[,1]),]

cleaned_ordered_both <- ordered_both[-c(which(ordered_both[,2] < -50)),]

data <- 

plot(cleaned_ordered_both[,1],cleaned_ordered_both[,2], type="l")






legend("bottomright", "Temperature [C°]" , fill = "red")
Weight_Loss_and_Max_Depletion_Depth<- data.frame(CorrData[c(which((!(is.na(CorrData[,38])))&(!(is.na(CorrData[,39]))))),38:39])


# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())

# Plot
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=value, fill=name)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("A Violin wrapping a boxplot") +
    xlab("")



as.character(colSums(!is.na(Temperature_VS_Material[,7:1])))[1:7]
colnames(Temperature_VS_Material[,7:1])
paste(colnames(Temperature_VS_Material[,14:8])," N°=",as.character(colSums(!is.na(Temperature_VS_Material[,14:8])))[1:7])



XX <- matrix(c(1,2,3,4), nrow=2, ncol=2)

repcol(XX,2)


Temperature_VS_Material

factors_Temperature_VS_Material <- matrix(0,nrow=nrow(, ncol=length(levels(factor(Temperature[,1]))))

for (x in 4:25){
		print(table(Temperature_VS_Material[,x]))
		print(colnames(Temperature_VS_Material)[x])
}



for (x in 1:25){
		print(table(Dive_Time_VS_Material[,x]))
		print(colnames(Dive_Time_VS_Material)[x])
}



data("penguins", package = "palmerpenguins")

penguins <- drop_na(penguins)

plt <- ggbetweenstats(
  data = penguins,
  x = species,
  y = bill_length_mm
)

plt <- plt + 
  # Add labels and title
  labs(
    x = "Penguins Species",
    y = "Bill Length",
    title = "Distribution of bill length across penguins species"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )

plt <- plt  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())


plot_function <- function(DATA){

data <- data.frame(
		name=c(rep(colnames(DATA)[1],sum(!is.na(DATA[,1]))),
			 rep(colnames(DATA)[2],sum(!is.na(DATA[,2]))),
			 rep(colnames(DATA)[3],sum(!is.na(DATA[,3]))),
			 rep(colnames(DATA)[4],sum(!is.na(DATA[,4]))),
			 rep(colnames(DATA)[5],sum(!is.na(DATA[,5]))),
			 rep(colnames(DATA)[6],sum(!is.na(DATA[,6]))),
			 rep(colnames(DATA)[7],sum(!is.na(DATA[,7])))
			),
		value=c(DATA[!is.na(DATA[,1]),1],
			  DATA[!is.na(DATA[,2]),2],
			  DATA[!is.na(DATA[,3]),3],
			  DATA[!is.na(DATA[,4]),4],
			  DATA[!is.na(DATA[,5]),5],
			  DATA[!is.na(DATA[,6]),6],
			  DATA[!is.na(DATA[,7]),7]
			)
)

sample_size = data %>% group_by(name) %>% summarize(num=n())

# Plot
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=value, fill=name)) +
    geom_violin(width=1) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("A Violin wrapping a boxplot") +
    xlab("")

}

plot_function(Temperature_VS_Material)
plot_function(Dive_Time_VS_Material)
plot_function(Weight_Loss_VS_Material)
plot_function(Max_Depletion_VS_Material)

