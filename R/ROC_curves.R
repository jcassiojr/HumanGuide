#load ROCR
library(ROCR);

#load ligands and decoys
lig <- unique(read.table("ligands.txt")[,1]);
dec <- unique(read.table("decoys.txt")[,1]);

#load data file from docking
uniqRes <- read.table("dataforR_uq.txt",header=T);

#change colnames
colnames(uniqRes)[1]="LigandName";

#add column with ligand/decoy info
uniqRes$IsActive <- as.numeric(uniqRes$LigandName %in% lig)

#define ROC parameters 
#here INTER is selected to compare between ligands using rDock SCORE.INTER
#this could be changed for also running with other programs
predINTERuq <- prediction(uniqRes$INTER*-1, uniqRes$IsActive)
perfINTERuq <- performance(predINTERuq, 'tpr','fpr')

#plot in jpg format with a grey line with theoretical random results
jpeg("hivpr_Rinter_ROC.jpg")
plot(perfINTERuq,main="hivpr - ROC Curves",col="blue")
abline(0,1,col="grey")
dev.off()

#AUC (area under the curve)
auc_rdock <- performance(predINTERuq, "auc")
auc.area_rdock <- slot(auc_rdock, "y.values")[[1]]
cat("AUC: \n")
cat(auc.area_rdock)
cat("\n\n")

#Enrichment Factors
EF_rdock <- perfINTERuq@y.values[[1]]/perfINTERuq@x.values[[1]]
EF_rdock_1 <- EF_rdock[which(perfINTERuq@x.values[[1]] > 0.01)[1]]
EF_rdock_20 <- EF_rdock[which(perfINTERuq@x.values[[1]] > 0.2)[1]]
cat("Enrichment Factor top1%:\n")
cat(EF_rdock_1)
cat("\n\n")
cat("Enrichment Factor top20%:\n")
cat(EF_rdock_20)
cat("\n\n")

#Semilog plot
jpeg("hivpr_semilog_ROC.jpg")
rdockforsemilog=perfINTERuq@x.values[[1]]
rdockforsemilog[rdockforsemilog < 0.0005]=0.0005
plot(rdockforsemilog,perfINTERuq@y.values[[1]],type="l",xlab="False Positive Rate", ylab="True Positive Rate",xaxt="n", log="x", col="blue",main="hivpr - Semilog ROC Curves")
axis(1, c(0,0.001,0.01,0.1,1))
x<-seq(0,1,0.001)
points(x,x,col="gray",type="l")
dev.off()
