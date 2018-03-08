##############################################################################################################################
#                   This scripts calculate the main performance measures                                                     #
#         it also writes 2 tabeles in /results that will be used by other scripts to produce the assessment figures          #
##############################################################################################################################

StatisticalValidation <- function(Experimental.data, path.prediction, AUC.list){
  #get the real data
  realValue <- Experimental.data[, 2]
  # set threshold for AUC
  classThreshold <- AUC.list
  #create empty results matrices
  resultMatrix <- matrix(ncol = 3, nrow = length(path.prediction))
  colnames(resultMatrix) <- c("RMSE", "PCC", "KCC")
  rownames(resultMatrix) <- rep("empty",length(path.prediction))
  
  if (length(AUC.list) > 0){ # compute only if a threshold for the AUC is provided
    performanceMatrix <- matrix(ncol = length(classThreshold)*4 , nrow = length(path.prediction)) # compute for all threshold the 4 scoring index (Sensitivity, spec, bal. Accuracy)
    perfParameters <- c("Sens", "Spec","B. Acc","AUC" ) # B. Acc = Balanced Accuracy
    
    listnames <-c()
    for (l in classThreshold){
      for(m in perfParameters){
        listnames <-c(listnames, paste(m,l))
      }
    }
    colnames(performanceMatrix) <-listnames 
    rownames(performanceMatrix) <-rep("empty", length(path.prediction))
  }
  #############################################################
  #     Calculate performance measures: CC, AUC     ###########
  #############################################################
  #load the prediction
  
  for(i in 1:length(path.prediction)) { 
    #select prediction file i
    pat <- paste('_',i,'.txt', sep='') 
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction <- read.table(file=CurrentSubmission, sep="\t", quote="", na.strings='*', header=TRUE)
    predictedValue <- prediction[, 2]
    predictedSTD <- prediction[, 3] 
    #get the correlation coefficients
    pearsonCor.coeff <- cor(realValue , predictedValue , method = "pearson")
    kendallCor.coeff <- cor(realValue , predictedValue , method = "kendall", use="pairwise")
    rmsd <- sqrt(sum((predictedValue-realValue)^2)/length(predictedValue))
    rownames(resultMatrix)[i] <- paste("Submission", i) 
    resultMatrix[i,] <-c(rmsd,pearsonCor.coeff, kendallCor.coeff)
    
    #############################################
    #### get AUC and other performance indexes ##
    #############################################
    
    if (length(classThreshold) > 0) { #  if no threshold is provided, AUC will not be computed
      performanceList <- c()
      for(j in 1:length(classThreshold)) {
        realClass <- realValue
        realClass[ which(realValue[] > classThreshold[j]) ] <- 1
        realClass[ which(realValue[] <= classThreshold[j]) ] <- 0
        #get predicted classes
        predClass <- predictedValue
        predClass[ which(predictedValue[] > classThreshold[j]) ] <- 1
        predClass[ which(predictedValue[] <= classThreshold[j]) ] <- 0
        #get pathogenic mutations
        positive <- which(realClass[] == 1)
        TP <- length( which(predClass[positive] == realClass[positive] ) )
        FN <- length( which(predClass[positive] != realClass[positive] ) )
        #get neutral mutations
        neutral <- which(realClass[] == 0)
        TN <- length( which(predClass[neutral] == realClass[neutral] ) )
        FP <- length( which(predClass[neutral] != realClass[neutral] ) )
        #get specificity and sensitivity
        sens <- TP / (TP + FN)
        spec <- TN / (TN + FP)
        bAccu <- (sens + spec) /2
        performanceList <- c(performanceList, sens, spec, bAccu)
        pred <- prediction( predictions= predictedValue, labels= realClass)
        perf <- performance(pred, "tpr", "fpr")
        auc <- performance(pred, "auc")
        auc <- round(unlist(slot(auc, "y.values")), digits = 6)
        performanceList <- c(performanceList, auc)
      }
      ################################
      rownames(performanceMatrix)[i] <- paste("Submission", i) 
      performanceMatrix[i, ] <- performanceList
    }
    if (length(classThreshold) > 0) {   
      finalMatrix <- cbind (resultMatrix, performanceMatrix)
    } else {
      finalMatrix <- resultMatrix
    }
    write.table(round(finalMatrix, d=2), file='./results/general_statistics_tab.txt', sep = "\t" , quote = F, col.names = NA )
  }  
  
  
  #######################################################################################
  #    this script calculate CC among performance indices and produce an heatmap figure #
  #######################################################################################
  
  
  
  predictions <- as.data.frame(finalMatrix) 

  ##### make same column order plot paper
  submission.names <- predictions$X # save submission names
  predictions <- cbind(predictions$PCC,predictions$KCC, predictions$RMSE, predictions[which(grepl('AUC', colnames(predictions)))] )
  colnames(predictions)[1:3]=c('PCC','KCC','RMSE')
  predictions.table <- predictions # prediction table for later use
  predictions <- as.matrix(predictions[,1:ncol(predictions)])
  
  
  
  #calculate the FULL correlation matrix
  matr <- matrix(0, ncol(predictions), ncol(predictions))
  for(i in 1:ncol(predictions)) {
    for(j in 1:ncol(predictions)) {
      matr[i, j] <- cor(as.numeric(predictions[,i]) , as.numeric(predictions[,j]) , method = "pearson")
      #matr[i, j] <- cor(as.numeric(predictions[,i]) , as.numeric(predictions[,j]) , method = "kendall", use="pairwise")
    }
  }
  #plot the correlation matrix
  png("./results/Scoring_index_correlation.png", width = 10, height = 10, units = 'in', res = 300)
  cellcolors<-matrix(NA, ncol(matr), ncol(matr))
  cellcolors[matr >= 0]<-
    color.scale(matr[matr >= 0],
                cs1=0,cs2=c(0.4,0.7),cs3=0) # green scale for positive numbers
  cellcolors[matr < 0]<-
    color.scale(matr[matr < 0],
                cs1=c(0.6,0.9),cs2=0,cs3=0) # red scale for negative numbers
  matr2=abs(matr) # make all numbers positive
  color2D.matplot(matr, show.values=TRUE, axes=FALSE, cellcolors=cellcolors, xlab="", ylab="", vcex = 1.6)
  axis(3, at=0.5:ncol(predictions),  labels=colnames(predictions),  cex.axis = 1.2, font = 2 ) 
  axis(2, at=0.5:ncol(predictions),  labels=rev(colnames(predictions)) , cex.axis = 1.2, font = 2 ) 
  #
  ppi = 600
  dev.off()
  ##############################################
  ##### Make table with submission ranking #####
  ##############################################
  predictions <- predictions.table
  predictions$RMSE=rank(-predictions$RMSE, ties.method = "min") # this way RMSE values will be ranked in inverse order (first greatest, last smallest)
  predictions <- apply(predictions[2:ncol(predictions)],2, function(x) rank(-x, ties.method = "min"))
  # compute mean of each row (all ranks of each submission) 
  rank.mean <- rowMeans(predictions)
  # add rank mean column to submissions table and order table
  rank.mean.overall <- rank(rank.mean, ties.method = "min")
  predictions <- cbind(predictions, rank.mean, rank.mean.overall, seq(1:nrow(predictions))) # the last column is a sequence of numbers representing submission ID
  
  
  # print Submission ranking
  write.table(predictions[,1:ncol(predictions)-1], file='./results/general_statistics_rank_tab.txt', sep = "\t" , quote = F, col.names = NA )
 
}  
















