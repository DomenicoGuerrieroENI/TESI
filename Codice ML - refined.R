#-------------------------------------------IMPOSTAZIONE DIRECTORY & CARICAMENTO LIBRERIE-------------------------------------------------#

path<-'C:/Users/EXT2061529/OneDrive - Eni/Desktop/Attività di tirocinio-tesi/Codice R'											#il percorso della directory
setwd(path)																								#setta la directory

library(readxl)																							# Libreria per leggere i file excel
library(ggplot2)
#library(dplyr)
library(hrbrthemes)
library(viridisLite)
library(viridis)

library(rpart)
library(randomForest)
library(lattice)
library(caret)
library(DALEX)
library(rpart.plot)
library(ingredients)
library(e1071)
library(bestNormalize)
library(h2o)

library(dplyr)
library(Metrics)

#-------------------------------------------IMPOSTAZIONE DIRECTORY & CARICAMENTO LIBRERIE-------------------------------------------------#

#-------------------------------------------------------INIZIALIZZAZIONE DATASET----------------------------------------------------------#
		   
FileName <- 'Database Molten Salts corrosion tests.xlsx'																# Il nome del file excel da caricare
SheetNr <- 5																							# Il numero della pagina da caricare del file excel
CorrData <- as.data.frame(read_excel(FileName, sheet = 5))																# Carica il file excel
CorrDataRowNames <- CorrData[,1]
CorrData <- CorrData[-c(1)]																					# La prima colonna del dataset, contenente i nomi delle righe, viene ora eliminata in quanto � una ripetizione
rownames(CorrData) <- CorrDataRowNames																			# Imposta il nome delle righe del file caricato, dandogli lo stesso nome del file excel

Corr_Dataset <- data.frame(CorrData)																			# Creazione dataset locale che verr� cancellato
Corr_Dataset[is.na(Corr_Dataset)] <- 0																			# Per calcolare le impurit� totali c'� bisogno di eliminare i "NA" dal datase
Corr_Data1 <- cbind(Corr_Dataset[,1:16],list(rowSums(Corr_Dataset[,17:36])),Corr_Dataset[37:ncol(CorrData)])							# Creazione variabile buffer
Corr_Dataset <- Corr_Data1																					# Si è creato con queste istruzioni una copia del dataset originale, in cui al posto dei dati sulle singole impurit� c'� una sola colonna con la somma di queste
Corr_Dataset[Corr_Dataset[,17]==0,17]<-NA
colnames(Corr_Dataset)[17] <- "Total Impurities Pre"																	# Si assegna il nome "Total Impurities Pre" a tale colonna
rm(Corr_Data1)

Corr_Dataset[,5] <- ifelse(Corr_Dataset[,4]==Corr_Dataset[,5],1,0)
colnames(Corr_Dataset)[5] <- "Mat_Cru"
Corr_Dataset$Mat_Cru <- as.factor(Corr_Dataset$Mat_Cru)
Corr_Dataset$molten_salt <- as.factor(Corr_Dataset$molten_salt)
Corr_Dataset$ms_purified <- as.factor(Corr_Dataset$ms_purified)

colnames(Corr_Dataset)[19] <- "MD"
colnames(Corr_Dataset)[20] <- "WL"
colnames(Corr_Dataset)[11] <- "G_m"
colnames(Corr_Dataset)[12] <- "G_c"
colnames(Corr_Dataset)[15] <- "res.moisture"
Corr_Dataset <- Corr_Dataset[,-c(1,4,17,18)]

Corr_Dataset <- data.frame(cbind(Corr_Dataset[,1:10],Corr_Dataset[,9]-Corr_Dataset[,10],Corr_Dataset[,11:16]))
colnames(Corr_Dataset)[11] <- "G_m_minus_G_c"
Corr_Dataset <- Corr_Dataset[c(which(Corr_Dataset[,17]<=0.5)),]

#-------------------------------------------------------INIZIALIZZAZIONE DATASET----------------------------------------------------------#

#-----------------------------------------------------------MACHINE LEARNING--------------------------------------------------------------#
	#--------------------------------------------------MACHINE LEARNING PER WL----------------------------------------------------------#
		#---------------------------------------------PREPROCESSAMENTO DATI-----------------------------------------------------------#
Corr_Dataset_WL <- Corr_Dataset[,c(1:14,17)]
Corr_Dataset_WL <- Corr_Dataset_WL[c(which(Corr_Dataset_WL[,15]!=0 & !is.na(Corr_Dataset_WL[,15]))),]

Corr_Dataset_WL <- Corr_Dataset_WL[,-c(3)]
		#---------------------------------------------PREPROCESSAMENTO DATI-----------------------------------------------------------#
		#-----------------------------------------------SVILUPPO MODELLI--------------------------------------------------------------#
			#---------------------------------------DEEP NEURAL NETWORK-------------------------------------------------------------#
set.seed(123)
h2o.init(nthreads = -1, max_mem_size = "4G")
				#---------------------------------PREPROCESSING DATA--------------------------------------------------------------#
Corr_Dataset_WL_norm <- Corr_Dataset_WL

#apply(Filter(is.numeric, Corr_Dataset_WL_norm), MARGIN = 2, function(x) skewness(x))

Corr_Dataset_WL_norm[,2] <- sign(Corr_Dataset_WL_norm[,2]) *abs(Corr_Dataset_WL_norm[,2])^ (1/3)

Corr_Dataset_WL_norm[,4] <-  sign(Corr_Dataset_WL_norm[,4]) *abs(Corr_Dataset_WL_norm[,4])^ (1/3)
Corr_Dataset_WL_norm[,5] <-  sign(Corr_Dataset_WL_norm[,5]) *abs(Corr_Dataset_WL_norm[,5])^ (1/3)
Corr_Dataset_WL_norm[,6] <-  sign(Corr_Dataset_WL_norm[,6]) *abs(Corr_Dataset_WL_norm[,6])^ (1/3)
Corr_Dataset_WL_norm[,7] <-  sign(Corr_Dataset_WL_norm[,7]) *abs(Corr_Dataset_WL_norm[,7])^ (1/3)

Corr_Dataset_WL_norm[,8]  <-  sign(Corr_Dataset_WL_norm[,8]) *abs(Corr_Dataset_WL_norm[,8]) ^ (1/3)
Corr_Dataset_WL_norm[,9]  <-  sign(Corr_Dataset_WL_norm[,9]) *abs(Corr_Dataset_WL_norm[,9]) ^ (1/3)
Corr_Dataset_WL_norm[,10] <-  sign(Corr_Dataset_WL_norm[,10])*abs(Corr_Dataset_WL_norm[,10])^ (1/3)

Corr_Dataset_WL_norm[,13] <- log1p(predict(yeojohnson(Corr_Dataset_WL_norm[,13])))

Corr_Dataset_WL_norm[,14] <- log(2-Corr_Dataset_WL_norm[,14])^(1/3)

#apply(Filter(is.numeric, Corr_Dataset_WL_norm), MARGIN = 2, function(x) skewness(x))
#apply(Filter(is.numeric, Corr_Dataset_WL_norm), MARGIN = 2, function(x) shapiro.test(x))    #if p<0.05 distribuzione NON normale

maxs <- matrix(rep(apply(Filter(is.numeric, Corr_Dataset_WL_norm), MARGIN = 2, function(x) max(x)),nrow(Corr_Dataset_WL_norm)),
		    nrow= nrow(Corr_Dataset_WL_norm), ncol=ncol(Filter(is.numeric, Corr_Dataset_WL_norm)), byrow = TRUE)
colnames(maxs) <- colnames(Filter(is.numeric, Corr_Dataset_WL_norm))

mins <- matrix(rep(apply(Filter(is.numeric, Corr_Dataset_WL_norm), MARGIN = 2, function(x) min(x)),nrow(Corr_Dataset_WL_norm)),
		    nrow= nrow(Corr_Dataset_WL_norm), ncol=ncol(Filter(is.numeric, Corr_Dataset_WL_norm)), byrow = TRUE)
colnames(mins) <- colnames(Filter(is.numeric, Corr_Dataset_WL_norm))

Corr_Dataset_WL_norm[,which(colnames(Corr_Dataset_WL_norm) %in% colnames(Filter(is.numeric, Corr_Dataset_WL_norm)))] <- 
( Filter(is.numeric, Corr_Dataset_WL_norm) - mins ) / ( maxs-mins )

Corr_Dataset_WL_norm <- predict(dummyVars(~ ., data = Corr_Dataset_WL_norm), newdata = Corr_Dataset_WL_norm) 

Corr_Dataset_WL_norm_h2o <- as.h2o(Corr_Dataset_WL_norm)

splits <- h2o.splitFrame(Corr_Dataset_WL_norm_h2o, ratios = 0.8, seed = 123)
train <- splits[[1]]
test  <- splits[[2]]
				#---------------------------------PREPROCESSING DATA--------------------------------------------------------------#

				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#

hyper_params <- list(
  			   hidden = list(
    						c(32, 32), c(64, 64), c(128, 128), c(256, 256), 
    						c(64, 32), c(128, 64), c(256, 128, 64), c(512, 256, 128),
    						c(64, 32, 16), c(128, 64, 32), c(256, 128, 64, 32)
  						),
  			   activation = c("Rectifier", "Tanh"),
  			   l1 = c(0, 1e-5, 1e-3),
  			   l2 = c(0, 1e-5, 1e-3)
			   )

search_criteria <- list(strategy = "RandomDiscrete", max_models = 50, seed = 123)

target <- "WL"
predictors <- setdiff(colnames(Corr_Dataset_WL_norm), target)

Corr_Dataset_WL_norm_grid <- h2o.grid(
					   algorithm = "deeplearning",
					   grid_id = "Corr_Dataset_WL_norm_grid",
					   x = predictors,
					   y = target,
					   training_frame = train,
					   validation_frame = test,
					   epochs = 200,
					   stopping_rounds = 10,
					   stopping_metric = "MSE",
					   stopping_tolerance = 0.001,
					   seed = 123,
					   hyper_params = hyper_params,
					   search_criteria = search_criteria
					   )

best_model <- h2o.getModel(h2o.getGrid("Corr_Dataset_WL_norm_grid", sort_by = "mse", decreasing = FALSE)@model_ids[[1]])
print(best_model)
				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#

perf <- h2o.performance(best_model, test)
print(perf)

dnn_WL_varimp<-h2o.varimp(best_model)
h2o.varimp_plot(best_model)

preds <- h2o.predict(best_model,test)
model_prediction_WL <- as.data.frame(preds)
model_prediction_WL <- model_prediction_WL*(maxs[1,ncol(maxs)]-mins[1,ncol(mins)])+mins[1,ncol(mins)]
model_prediction_WL <- 2-exp((model_prediction_WL)^3)

test_WL <- as.data.frame(test$WL)
test_WL <- test_WL*(maxs[1,ncol(maxs)]-mins[1,ncol(mins)])+mins[1,ncol(mins)]
test_WL <- 2-exp((test_WL)^3)

compare <- cbind(test_WL,model_prediction_WL)

mse_dnn_model_WL <- mse(test_WL[,1],model_prediction_WL[,1])

plot(test_WL[,1],model_prediction_WL[,1])
lines(c(1,-180),c(1,-180), type="l", col = "red")

plot(test_WL[which(test_WL[,1]>-20),1],model_prediction_WL[which(test_WL[,1]>-20),1])
lines(c(1,-20),c(1,-20), type="l", col = "red")

h2o.shutdown(prompt = FALSE)
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#
			#---------------------------------------DEEP NEURAL NETWORK-------------------------------------------------------------#

			#------------------------------------------RANDOM FOREST----------------------------------------------------------------#
				#-------------------------------TRAINING E TEST DATASET-----------------------------------------------------------#
set.seed(123)

train_idx_WL  <- createDataPartition(Corr_Dataset_WL$WL, p = 0.7, list = FALSE)
train_data_WL <- Corr_Dataset_WL[ train_idx_WL,]
test_data_WL  <- Corr_Dataset_WL[-train_idx_WL,]

#apply(Filter(is.numeric, train_data_WL), MARGIN = 2, function(x) skewness(x))

train_data_WL[,2] <- log(train_data_WL[,2])

train_data_WL[,4] <-  sign(train_data_WL[,4]) *abs(train_data_WL[,4])^ (1/3)
train_data_WL[,5] <-  sign(train_data_WL[,5]) *abs(train_data_WL[,5])^ (1/3)
train_data_WL[,6] <-  sign(train_data_WL[,6]) *abs(train_data_WL[,6])^ (1/3)
train_data_WL[,7] <-  sign(train_data_WL[,7]) *abs(train_data_WL[,7])^ (1/3)

train_data_WL[,8]  <-  sign(train_data_WL[,8]) *abs(train_data_WL[,8]) ^ (1/3)
train_data_WL[,9]  <-  sign(train_data_WL[,9]) *abs(train_data_WL[,9]) ^ (1/3)
train_data_WL[,10] <-  sign(train_data_WL[,10])*abs(train_data_WL[,10])^ (1/3)

train_data_WL[,13] <- log(10+predict(yeojohnson(train_data_WL[,13])))

train_data_WL[,14] <- log(2-train_data_WL[,14])^(1/3)

#apply(Filter(is.numeric, train_data_WL), MARGIN = 2, function(x) skewness(x))
#apply(Filter(is.numeric, train_data_WL), MARGIN = 2, function(x) shapiro.test(x))    #if p<0.05 distribuzione NON normale
				#-------------------------------TRAINING E TEST DATASET-----------------------------------------------------------#
				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#
set.seed(123)

ntree_vec <- seq(100,1000, by = 50)
mtry_vec <- seq(2,floor(sqrt(ncol(train_data_WL)-1)+2), by=1)
nodesize_vec <- seq(1,15, by = 1)
k<-0

mse_WL  <- matrix(rep(1,4*(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec))),
		   	nrow=4,
		   	ncol=(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec)))
rownames(mse_WL) <- c("ntree","mtry","nodesize","mse")

mape_WL <- matrix(rep(1,4*(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec))),
		  	nrow=4,
		   	ncol=(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec)))
rownames(mape_WL) <- c("ntree","mtry","nodesize","mape")

rf_model_WL <- list()

for(x in 1:length(ntree_vec)){
	for(y in 1:length(mtry_vec)){
		for(z in 1:length(nodesize_vec)){
			k<-k+1
			rf_model_WL[[k]] <- randomForest(WL ~ ., data = train_data_WL,
								   ntree = ntree_vec[x], 
								   mtry = mtry_vec[y],
								   nodesize=nodesize_vec[z])
			pred_rf_WL <- 2-exp(predict(rf_model_WL[[k]], newdata = test_data_WL)^3)
			mse_rf_WL  <- mse(test_data_WL$WL, pred_rf_WL)
			mse_WL[1,k]  <- ntree_vec[x]
			mse_WL[2,k]  <- mtry_vec[y]
			mse_WL[3,k]  <- nodesize_vec[z]
			mse_WL[4,k]  <- mse_rf_WL
			mape_rf_WL <- mape(test_data_WL$WL, pred_rf_WL)
			mape_WL[1,k]  <- ntree_vec[x]
			mape_WL[2,k]  <- mtry_vec[y]
			mape_WL[3,k]  <- nodesize_vec[z]
			mape_WL[4,k] <- mape_rf_WL			
		}
	}
}

rf_model_WL_opt_mape <- rf_model_WL[[which(mape_WL[4,]==min(mape_WL[4,]))]]
#rf_model_WL_opt_mape <- rf_model_WL[[max(which(mse_WL[4,]<=min(mape_WL[4,])+0.07*(max(mape_WL[4,])-min(mape_WL[4,]))))]]

rf_model_WL_opt_mse <- rf_model_WL[[which(mse_WL[4,]==min(mse_WL[4,]))]]
#rf_model_WL_opt_mse <- rf_model_WL[[max(which(mse_WL[4,]<=min(mse_WL[4,])+0.07*(max(mse_WL[4,])-min(mse_WL[4,]))))]]
				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#

pred_rf_WL_opt_mape <- 2-exp(predict(rf_model_WL_opt_mape, newdata = test_data_WL)^3)
mse_rf_WL_opt_mape  <- mse(test_data_WL$WL, pred_rf_WL_opt_mape)
mape_rf_WL_opt_mape <- mape(test_data_WL$WL, pred_rf_WL_opt_mape)

pred_rf_WL_opt_mse <- 2-exp(predict(rf_model_WL_opt_mse, newdata = test_data_WL)^3)
mse_rf_WL_opt_mse  <- mse(test_data_WL$WL, pred_rf_WL_opt_mse)
mape_rf_WL_opt_mse <- mape(test_data_WL$WL, pred_rf_WL_opt_mse)

cat("MSE Random Forest per WL (opt MAPE):", mse_rf_WL_opt_mape, "\n")
cat("MAPE Random Forest per WL (opt MAPE):", mape_rf_WL_opt_mape, "\n")

cat("MSE Random Forest per WL (opt MSE):", mse_rf_WL_opt_mse, "\n")
cat("MAPE Random Forest per WL (opt MSE):", mape_rf_WL_opt_mse, "\n")

risultati_WL <- data.frame(
  Osservato = test_data_WL$WL,
  Predizione_RF_opt_mse = pred_rf_WL_opt_mse,
  Predizione_RF_opt_mape = pred_rf_WL_opt_mape
)
head(risultati_WL)

varImpPlot(rf_model_WL_opt_mse)
plot(test_data_WL$WL, pred_rf_WL_opt_mse)
lines(c(1,-180),c(1,-180), type="l", col = "red")

plot(test_data_WL[which(test_data_WL[,14]>-20),14],pred_rf_WL_opt_mse[which(test_data_WL[,14]>-20)])
lines(c(1,-20),c(1,-20), type="l", col = "red")
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#
			#------------------------------------------RANDOM FOREST----------------------------------------------------------------#
		#-----------------------------------------------SVILUPPO MODELLI--------------------------------------------------------------#
	#--------------------------------------------------MACHINE LEARNING PER WL----------------------------------------------------------#

	#--------------------------------------------------MACHINE LEARNING PER MD----------------------------------------------------------#
		#---------------------------------------------PREPROCESSAMENTO DATI-----------------------------------------------------------#
Corr_Dataset_MD <- Corr_Dataset[,c(1:13,16)]
Corr_Dataset_MD <- Corr_Dataset_MD[c(which(Corr_Dataset_MD[,14]!=0 & !is.na(Corr_Dataset_MD[,14]))),]

Corr_Dataset_MD <- Corr_Dataset_MD[,-c(3)]
		#---------------------------------------------PREPROCESSAMENTO DATI-----------------------------------------------------------#
		#-----------------------------------------------SVILUPPO MODELLI--------------------------------------------------------------#

			#---------------------------------------DEEP NEURAL NETWORK-------------------------------------------------------------#
seed(123)
h2o.init(nthreads = -1, max_mem_size = "4G")
				#---------------------------------PREPROCESSING DATA--------------------------------------------------------------#
Corr_Dataset_MD_norm <- Corr_Dataset_MD

#apply(Filter(is.numeric, Corr_Dataset_MD_norm), MARGIN = 2, function(x) skewness(x))

#Corr_Dataset_MD_norm[,1] <- predict(yeojohnson(Corr_Dataset_MD_norm[,1]))

Corr_Dataset_MD_norm[,2] <- sign(Corr_Dataset_MD_norm[,2]) *abs(Corr_Dataset_MD_norm[,2])^ (1/3)

Corr_Dataset_MD_norm[,4] <- predict(yeojohnson(Corr_Dataset_MD_norm[,4]))
Corr_Dataset_MD_norm[,5] <- predict(yeojohnson(Corr_Dataset_MD_norm[,5]))
Corr_Dataset_MD_norm[,6] <- predict(yeojohnson(Corr_Dataset_MD_norm[,6]))
Corr_Dataset_MD_norm[,7] <- predict(yeojohnson(Corr_Dataset_MD_norm[,7]))

Corr_Dataset_MD_norm[,8]  <- predict(yeojohnson(Corr_Dataset_MD_norm[,8]))
Corr_Dataset_MD_norm[,9]  <- predict(yeojohnson(Corr_Dataset_MD_norm[,9]))
Corr_Dataset_MD_norm[,10] <- predict(yeojohnson(Corr_Dataset_MD_norm[,10]))

Corr_Dataset_MD_norm[,13] <- log(2-Corr_Dataset_MD_norm[,13])^(1/3)

#apply(Filter(is.numeric, Corr_Dataset_MD_norm), MARGIN = 2, function(x) skewness(x))


maxs <- matrix(rep(apply(Filter(is.numeric, Corr_Dataset_MD_norm), MARGIN = 2, function(x) max(x)),nrow(Corr_Dataset_MD_norm)),
		    nrow= nrow(Corr_Dataset_MD_norm), ncol=ncol(Filter(is.numeric, Corr_Dataset_MD_norm)), byrow = TRUE)
colnames(maxs) <- colnames(Filter(is.numeric, Corr_Dataset_MD_norm))

mins <- matrix(rep(apply(Filter(is.numeric, Corr_Dataset_MD_norm), MARGIN = 2, function(x) min(x)),nrow(Corr_Dataset_MD_norm)),
		    nrow= nrow(Corr_Dataset_MD_norm), ncol=ncol(Filter(is.numeric, Corr_Dataset_MD_norm)), byrow = TRUE)
colnames(mins) <- colnames(Filter(is.numeric, Corr_Dataset_MD_norm))


Corr_Dataset_MD_norm[,which(colnames(Corr_Dataset_MD_norm) %in% colnames(Filter(is.numeric, Corr_Dataset_MD_norm)))] <- 
( Filter(is.numeric, Corr_Dataset_MD_norm) - mins ) / ( maxs-mins )

Corr_Dataset_MD_norm <- predict(dummyVars(~ ., data = Corr_Dataset_MD_norm), newdata = Corr_Dataset_MD_norm) 

Corr_Dataset_MD_norm_h2o <- as.h2o(Corr_Dataset_MD_norm)

splits <- h2o.splitFrame(Corr_Dataset_MD_norm_h2o, ratios = 0.8, seed = 123)
train <- splits[[1]]
test <- splits [[2]]
				#---------------------------------PREPROCESSING DATA--------------------------------------------------------------#

				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#

hyper_params <- list(
  			   hidden = list(
    						c(32, 32), c(64, 64), c(128, 128), c(256, 256), 
    						c(64, 32), c(128, 64), c(256, 128, 64), c(512, 256, 128),
    						c(64, 32, 16), c(128, 64, 32), c(256, 128, 64, 32)
  						),
  			   activation = c("Rectifier", "Tanh"),
  			   l1 = c(0, 1e-5, 1e-3),
  			   l2 = c(0, 1e-5, 1e-3)
			   )

search_criteria <- list(strategy = "RandomDiscrete", max_models = 50, seed = 123)

target <- "MD"
predictors <- setdiff(colnames(Corr_Dataset_MD_norm), target)

Corr_Dataset_MD_norm_grid <- h2o.grid(
					   algorithm = "deeplearning",
					   grid_id = "Corr_Dataset_MD_norm_grid",
					   x = predictors,
					   y = target,
					   training_frame = train,
					   validation_frame = test,
					   epochs = 200,
					   stopping_rounds = 10,
					   stopping_metric = "MSE",
					   stopping_tolerance = 0.001,
					   seed = 123,
					   hyper_params = hyper_params,
					   search_criteria = search_criteria
					   )

best_model <- h2o.getModel(h2o.getGrid("Corr_Dataset_MD_norm_grid", sort_by = "mse", decreasing = FALSE)@model_ids[[1]])
print(best_model)
				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#
preds <- h2o.predict(best_model,test)
perf <- h2o.performance(best_model, test)
print(perf)

dnn_MD_varimp<-h2o.varimp(best_model)
h2o.varimp_plot(best_model)

model_prediction_MD <- as.data.frame(preds)
model_prediction_MD <- model_prediction_MD*(maxs[1,ncol(maxs)]-mins[1,ncol(mins)])+mins[1,ncol(mins)]
model_prediction_MD <- 2-exp((model_prediction_MD)^3)

test_MD <- as.data.frame(test$MD)
test_MD <- test_MD*(maxs[1,ncol(maxs)]-mins[1,ncol(mins)])+mins[1,ncol(mins)]
test_MD <- 2-exp((test_MD)^3)

compare <- cbind(test_MD,model_prediction_MD)

mse_dnn_model_MD <- mse(test_MD[,1],model_prediction_MD[,1])

plot(test_MD[,1],model_prediction_MD[,1])
lines(c(1,-180),c(1,-180), type="l", col = "red")

plot(test_MD[which(test_MD[,1]>-20),1],model_prediction_MD[which(test_MD[,1]>-20),1])
lines(c(1,-20),c(1,-20), type="l", col = "red")

h2o.shutdown(prompt = FALSE)
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#
			#---------------------------------------DEEP NEURAL NETWORK-------------------------------------------------------------#

			#------------------------------------------RANDOM FOREST----------------------------------------------------------------#
				#--------------------------------TRAINING E TEST DATASET----------------------------------------------------------#
set.seed(123)

train_idx_MD  <- createDataPartition(Corr_Dataset_MD$MD, p = 0.7, list = FALSE)
train_data_MD <- Corr_Dataset_MD[ train_idx_MD, ]
test_data_MD  <- Corr_Dataset_MD[-train_idx_MD, ]

#apply(Filter(is.numeric, train_data_MD), MARGIN = 2, function(x) skewness(x))

train_data_MD[,1] <- predict(yeojohnson(train_data_MD[,1]))

train_data_MD[,2] <- sign(train_data_MD[,2]) *abs(train_data_MD[,2])^ (1/3)

train_data_MD[,8]  <- predict(yeojohnson(train_data_MD[,8]))
train_data_MD[,9]  <- predict(yeojohnson(train_data_MD[,9]))
train_data_MD[,10] <- predict(yeojohnson(train_data_MD[,10]))

train_data_MD[,13] <- log(2-train_data_MD[,13])^(1/3)

#apply(Filter(is.numeric, train_data_MD), MARGIN = 2, function(x) skewness(x))
				#--------------------------------TRAINING E TEST DATASET----------------------------------------------------------#
				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#
set.seed(123)

ntree_vec <- seq(100,1000, by = 50)
mtry_vec <- seq(2,floor(sqrt(ncol(train_data_MD)-1)+2), by=1)
nodesize_vec <- seq(1,15, by = 1)
k<-0

mse_MD  <- matrix(rep(1,4*(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec))),
		   nrow=4,
		   ncol=(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec)))
rownames(mse_MD) <- c("ntree","mtry","nodesize","mse")

mape_MD <- matrix(rep(1,4*(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec))),
		   nrow=4,
		   ncol=(length(ntree_vec)*length(mtry_vec)*length(nodesize_vec)))
rownames(mape_MD) <- c("ntree","mtry","nodesize","mape")

rf_model_MD <- list()

for(x in 1:length(ntree_vec)){
	for(y in 1:length(mtry_vec)){
		for(z in 1:length(nodesize_vec)){
			k<-k+1
			rf_model_MD[[k]] <- randomForest(MD ~ ., data = train_data_MD,
								   ntree = ntree_vec[x], 
								   mtry = mtry_vec[y],
								   nodesize=nodesize_vec[z])
			pred_rf_MD <- 2-exp(predict(rf_model_MD[[k]], newdata = test_data_MD)^3)
			mse_rf_MD  <- mse(test_data_MD$MD, pred_rf_MD)
			mse_MD[1,k]  <- ntree_vec[x]
			mse_MD[2,k]  <- mtry_vec[y]
			mse_MD[3,k]  <- nodesize_vec[z]
			mse_MD[4,k]  <- mse_rf_MD
			mape_rf_MD <- mape(test_data_MD$MD, pred_rf_MD)
			mape_MD[1,k]  <- ntree_vec[x]
			mape_MD[2,k]  <- mtry_vec[y]
			mape_MD[3,k]  <- nodesize_vec[z]
			mape_MD[4,k] <- mape_rf_MD			
		}
	}
}

rf_model_MD_opt_mape <- rf_model_MD[[which(mape_MD[4,]==min(mape_MD[4,]))]]

rf_model_MD_opt_mse <- rf_model_MD[[which(mse_MD[4,]==min(mse_MD[4,]))]]
				#---------------------MODEL TRAINING & HYPERPARAMETER OPTIMIZATION------------------------------------------------#
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#

pred_rf_MD_opt_mape <- 2-exp(predict(rf_model_MD_opt_mape, newdata = test_data_MD)^3)
mse_rf_MD_opt_mape  <- mse(test_data_MD$MD, pred_rf_MD_opt_mape)
mape_rf_MD_opt_mape <- mape(test_data_MD$MD, pred_rf_MD_opt_mape)

pred_rf_MD_opt_mse <- 2-exp(predict(rf_model_MD_opt_mse, newdata = test_data_MD)^3)
mse_rf_MD_opt_mse  <- mse(test_data_MD$MD, pred_rf_MD_opt_mse)
mape_rf_MD_opt_mse <- mape(test_data_MD$MD, pred_rf_MD_opt_mse)


cat("MSE Random Forest per MD (opt MAPE):", mse_rf_MD_opt_mape, "\n")
cat("MAPE Random Forest per MD (opt MAPE):", mape_rf_MD_opt_mape, "\n")

cat("MSE Random Forest per MD (opt MSE):", mse_rf_MD_opt_mse, "\n")
cat("MAPE Random Forest per MD (opt MSE):", mape_rf_MD_opt_mse, "\n")

risultati_MD <- data.frame(
  Osservato = test_data_MD$MD,
  Predizione_RF_opt_mse = pred_rf_MD_opt_mse,
  Predizione_RF_opt_mape = pred_rf_MD_opt_mape
)
head(risultati_MD)

varImpPlot(rf_model_MD_opt_mse)
plot(test_data_MD$MD, pred_rf_MD_opt_mse)
lines(c(1,-180),c(1,-180), type="l", col = "red")

plot(test_data_MD[which(test_data_MD[,13]>-20),13],pred_rf_MD_opt_mse[which(test_data_MD[,13]>-20)])
lines(c(1,-20),c(1,-20), type="l", col = "red")
				#------------------------------------MODEL RESULTS----------------------------------------------------------------#
			#------------------------------------------RANDOM FOREST----------------------------------------------------------------#
		#-----------------------------------------------SVILUPPO MODELLI--------------------------------------------------------------#
	#--------------------------------------------------MACHINE LEARNING PER MD----------------------------------------------------------#
#-----------------------------------------------------------MACHINE LEARNING--------------------------------------------------------------#








