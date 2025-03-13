plot_function <- function(DATA, name){

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
  ggplot(aes(x=myaxis, y=value, fill=name)) +
    geom_violin(width=1) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
   # theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=20, face="bold"),  # Ingrandisce il titolo
      axis.text = element_text(size=20),  # Ingrandisce i valori sugli assi
      axis.title = element_text(size=20),  # Ingrandisce i nomi degli assi
    ) +
    ggtitle(paste("Experimental", name, "distribution VS material")) +
    xlab("")

}

plot_function(Temperature_VS_Material, "temperatures")
plot_function(Dive_Time_VS_Material,"dive times")


HastelloyN_crucibles  <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[1]),4]))

	  #width=rep(0.1,times=length(HastelloyN_crucibles)),
par(cex.axis=1.7)
barplot(as.numeric(HastelloyN_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
	  names.arg = paste(names(HastelloyN_crucibles),
	  " N°=",as.numeric(HastelloyN_crucibles)),
	  col=rainbow(2*length(HastelloyN_crucibles)),
	  ylim=range(pretty(c(0, as.numeric(HastelloyN_crucibles)))),
	  main = "Hastelloy N crucibles"
	  #,horiz = TRUE
	  )

SS_crucibles          <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[2]),4]))
par(cex.axis=0.7)
barplot(as.numeric(SS_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(SS_crucibles)," N°=",as.numeric(SS_crucibles)),
col=rainbow(2*length(SS_crucibles)),
ylim=range(pretty(c(0, as.numeric(SS_crucibles)))),
main = "SS crucibles")

Cr_crucibles          <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[3]),4]))
par(cex.axis=0.7)
barplot(as.numeric(Cr_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Cr_crucibles)," N°=",as.numeric(Cr_crucibles)),
col=rainbow(2*length(Cr_crucibles)),
ylim=range(pretty(c(0, as.numeric(Cr_crucibles)))),
main = "Cr crucibles")

Incoloy800H_crucibles <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[4]),4]))
par(cex.axis=0.7)
barplot(as.numeric(Incoloy800H_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Incoloy800H_crucibles)," N°=",as.numeric(Incoloy800H_crucibles)),
col=rainbow(2*length(Incoloy800H_crucibles)),
ylim=range(pretty(c(0, as.numeric(Incoloy800H_crucibles)))),
main = "Incoloy 800 H crucibles")

Fe_crucibles          <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[5]),4]))
par(cex.axis=0.7)
barplot(as.numeric(Fe_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Fe_crucibles)," N°=",as.numeric(Fe_crucibles)),
col=rainbow(2*length(Fe_crucibles)),
ylim=range(pretty(c(0, as.numeric(Fe_crucibles)))),
main = "Fe crucibles")

HastelloyB3_crucibles <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[6]),4]))
par(cex.axis=0.7)
barplot(as.numeric(HastelloyB3_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(HastelloyB3_crucibles)," N°=",as.numeric(HastelloyB3_crucibles)),
col=rainbow(2*length(HastelloyB3_crucibles)),
ylim=range(pretty(c(0, as.numeric(HastelloyB3_crucibles)))),
main = "Hastelloy B-3 crucibles")

Inconel625_crucibles  <- -1*sort(-table(CorrData[which(CorrData[5]==names(sort(-table(CorrData[5])))[7]),4]))
par(cex.axis=0.7)
barplot(as.numeric(Inconel625_crucibles),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Inconel625_crucibles)," N°=",as.numeric(Inconel625_crucibles)),
col=rainbow(2*length(Inconel625_crucibles)),
ylim=range(pretty(c(0, as.numeric(Inconel625_crucibles)))),
main = "Inconel 625 crucibles")



ModCorrData <- cbind(CorrData[,1:12],paste(CorrData[,14],CorrData[,13]),CorrData[15:39])
colnames(ModCorrData)[13] <- "Pur. Molt. Salt"

table(ModCorrData[13])

HastelloyN_MoltenSalt  <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[1]),13]))
par(cex.axis=0.7)
barplot(as.numeric(HastelloyN_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(HastelloyN_MoltenSalt)," N°=",as.numeric(HastelloyN_MoltenSalt)),
col=rainbow(2*length(HastelloyN_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(HastelloyN_MoltenSalt)))),
main = "Hastelloy N Molten Salts")


SS_MoltenSalt          <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[2]),13]))
par(cex.axis=0.7)
barplot(as.numeric(SS_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(SS_MoltenSalt)," N°=",as.numeric(SS_MoltenSalt)),
col=rainbow(2*length(SS_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(SS_MoltenSalt)))),
main = "SS Molten Salts")

Cr_MoltenSalt          <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[3]),13]))
par(cex.axis=0.7)
barplot(as.numeric(Cr_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Cr_MoltenSalt)," N°=",as.numeric(Cr_MoltenSalt)),
col=rainbow(2*length(Cr_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(Cr_MoltenSalt)))),
main = "Cr Molten Salts")

Incoloy800H_MoltenSalt <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[4]),13]))
par(cex.axis=0.7)
barplot(as.numeric(Incoloy800H_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Incoloy800H_MoltenSalt)," N°=",as.numeric(Incoloy800H_MoltenSalt)),
col=rainbow(2*length(Incoloy800H_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(Incoloy800H_MoltenSalt)))),
main = "Incoloy 800 H Molten Salts")

Fe_MoltenSalt          <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[5]),13]))
par(cex.axis=0.7)
barplot(as.numeric(Fe_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Fe_MoltenSalt)," N°=",as.numeric(Fe_MoltenSalt)),
col=rainbow(2*length(Fe_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(Fe_MoltenSalt)))),
main = "Fe Molten Salts")

HastelloyB3_MoltenSalt <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[6]),13]))
par(cex.axis=0.7)
barplot(as.numeric(HastelloyB3_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(HastelloyB3_MoltenSalt)," N°=",as.numeric(HastelloyB3_MoltenSalt)),
col=rainbow(2*length(HastelloyB3_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(HastelloyB3_MoltenSalt)))),
main = "Hastelloy B-3 Molten Salts")

Inconel625_MoltenSalt  <- -1*sort(-table(ModCorrData[which(ModCorrData[5]==names(sort(-table(ModCorrData[5])))[7]),13]))
par(cex.axis=0.7)
barplot(as.numeric(Inconel625_MoltenSalt),
	  space = 1,
	  cex.axis = 1.7,
	  cex.names=1.7,
	  cex.main=1.7,
names.arg = paste(names(Inconel625_MoltenSalt)," N°=",as.numeric(Inconel625_MoltenSalt)),
col=rainbow(2*length(Inconel625_MoltenSalt)),
ylim=range(pretty(c(0, as.numeric(Inconel625_MoltenSalt)))),
main = "Inconel 625 Molten Salts")


WL_VS_mat <- Weight_Loss_VS_Material[,c(    which( colnames(Weight_Loss_VS_Material)=="Hastelloy N" ),
				  			 +which( colnames(Weight_Loss_VS_Material)=="SS" ),
				  			 +which( colnames(Weight_Loss_VS_Material)=="Cr" ),
				  			 +which( colnames(Weight_Loss_VS_Material)=="Incoloy 800 H" ),
				   			 +which( colnames(Weight_Loss_VS_Material)=="Fe" ),
				  			 +which( colnames(Weight_Loss_VS_Material)=="Hastelloy B-3" ),
				  			 +which( colnames(Weight_Loss_VS_Material)=="Inconel 600" ) ) ]

proportion <- colSums(!is.na(WL_VS_mat[,7:1]))/sum(colSums(!is.na(WL_VS_mat)))	
par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(WL_VS_mat[,7:1],
		main = "Weight Loss VS Material Boxplot",
		xlab = "Weight Loss [mg/cm^2]",
		names = paste(colnames(WL_VS_mat[,7:1]),"\n N°=",as.character(colSums(!is.na(WL_VS_mat[,7:1])))[1:7]),
		width=proportion ,
		col = rep("red", times=7),
		border = "black",
#		horizontal = TRUE,
		notch = FALSE)


MD_VS_mat <- Max_Depletion_VS_Material[,c(  which( colnames(Max_Depletion_VS_Material)=="Hastelloy N" ),
				  			 +which( colnames(Max_Depletion_VS_Material)=="SS" ),
				  			 +which( colnames(Max_Depletion_VS_Material)=="Cr" ),
				  			 +which( colnames(Max_Depletion_VS_Material)=="Incoloy 800 H" ),
				   			 +which( colnames(Max_Depletion_VS_Material)=="Fe" ),
				  			 +which( colnames(Max_Depletion_VS_Material)=="Hastelloy B-3" ),
				  			 +which( colnames(Max_Depletion_VS_Material)=="Inconel 600" ) ) ]

proportion <- colSums(!is.na(MD_VS_mat[,5:1]))/sum(colSums(!is.na(MD_VS_mat)))	
par(cex.axis=1)																								 # is for x-axis	par(cex.lab=0.5) is for y-axis
boxplot(WL_VS_mat[,5:1],
		main = "Max Depletion VS Material Boxplot",
		xlab = "Max Depletion [um]",
		names = paste(colnames(WL_VS_mat[,5:1]),"\n N°=",as.character(colSums(!is.na(WL_VS_mat[,5:1])))[1:5]),
		width=proportion,
		col = rep("red", times=5),
		border = "black")

	#-----------------------CORRELATION MATRIX--------------------------------------------------
#use : temperature(2) - dive time(3) - density(6)[as proxy for material] - Ni/Cr/Mo/Fe %mol (7-8-9-10) 
#	  - Ellingham mat/crucible (11-12[devi convertirlo in numeric]) -	max depth(38) - weigth loss(39) 
# CorrData[,c(2,3,6,7,8,9,10,11,12,38,39)]

dataforplot <- cbind(CorrData[,c(2,3,6,7,8,9,10,11,12)],,CorrData[,38:39])
colnames(dataforplot)<-c("T[°C]","Dive_t[h]","density","Ni_mol","Cr_mol","Mo_mol","Fe_mol","∆G_m","∆G_c","Max_dep","W_Loss")

cleandatafroplotMD<-na.omit(dataforplot[,-11])
cleandatafroplotWL<-na.omit(dataforplot[,-10])


corrplot(cor(cleandatafroplotMD), method="number")
corrplot(cor(cleandatafroplotWL), method="number")

	#-----------------------CORRELATION MATRIX--------------------------------------------------







