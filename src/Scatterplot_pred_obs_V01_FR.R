##################################################################
#    this script draw the experimental vs predicted scatterplot #
#################################################################


ExperimentalVsPredicted <- function(path.prediction, real.data){
  #get the real data
  realValue <- real.data[1:nrow(real.data), 2]
  # make plots
  ##################
  #### plot 1 : axis dimension is defined by min aand max value of all predictions
  ############
  png("./results/ExperimentalVsPredicted.png", width = 18, height = 26, units = 'in', res = 300)
  par(mfrow = c(ceiling(length(path.prediction)/4), 4)) # decide number of plots in each row of the picture and total number of lines
  # check min and max value of predicted to decide plot axis length
  min.vector=rep(1.0, length(path.prediction)) # create a vector for min of each prediction file
  max.vector=rep(1.0, length(path.prediction)) # create a vector for max of each prediction file
  for(i in 1:length(path.prediction)) { 
    #select prediction file i
    pat <- paste('_',i,'.txt', sep='') 
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction.table <- read.table(file=CurrentSubmission, sep="\t", header=TRUE)
    predicted.value <- prediction.table[1:nrow(prediction.table), 2]
    min.vector[i] <- min(as.numeric(predicted.value)) #changed TBD
    max.vector[i] <- max(as.numeric(predicted.value)) #changed TBD
  }
  min.max.predictions <- c(min(min.vector)*0.90, max(max.vector)*1.10) # vector for plot size
  
  #load the prediction
  for(i in 1:length(path.prediction)) { 
    #select prediction file i
    pat <- paste('_',i,'.txt', sep='') 
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction.table <- read.table(file=CurrentSubmission, sep="\t", header=TRUE)
    predicted.value <- prediction.table[1:nrow(prediction.table), 2]
    par(srt = 45) # point lables orientation
    plot(min.max.predictions, min.max.predictions, main=  paste("Submission", i), type= 'n', col = "#0000FFFF", xlab="Experimental values", ylab="Predicted values", cex.axis=1.3, cex.lab=1.5, cex.main=1.5, cex = 2)
    points(realValue, predicted.value, pch=20, cex = 2, col = "#0000FFFF")
    text(realValue, predicted.value, labels = real.data[1:nrow(real.data), 1], col = "#0000FFFF", pos = 4, cex=1.3) # comment this line to remove point lables
    segments(min.max.predictions[1], min.max.predictions[1], min.max.predictions[2], min.max.predictions[2], col= "#0000FFFF", lty= 2, lwd = 2)
  }

  dev.off()
  
  ##################
  #### plot 2: axis dimension based on min and max experimental value 
  ####        predicted value outside range of experimental values are rescaled as min_exp_value*0.92 and max_exp_value*1.05
  ################################## create plot with predictions outside range of experimental values in red ##########################
  
  
  png("./results/ExperimentalVsPredicted_out_of_scale.png", width = 18, height = 26, units = 'in', res = 300)
  par(mfrow = c(ceiling(length(path.prediction)/4), 4)) # decide number of plots in each row of the picture and total number of lines
  min.max.predictions <- c(min(realValue)*0.9, max(realValue)*1.10) # vector for plot size
  
  #load the prediction
  for(i in 1:length(path.prediction)) { 
    #select prediction file i
    pat <- paste('_',i,'.txt', sep='') 
    CurrentSubmission <- submission.files[grepl(pat, submission.files)]
    prediction.table <- read.table(file=CurrentSubmission, sep="\t", header=TRUE)
    predicted.value <- prediction.table[1:nrow(prediction.table), 2]
    color.list=rep('#0000FFFF', length(predicted.value)) # vector of point colors
    color.list[which(predicted.value < min(realValue))] = 'red' # plot points out of range of esperimental values in red 
    color.list[which(predicted.value > max(realValue))] = 'red'
    predicted.value[which(predicted.value < min(realValue))] = min(realValue)*0.92 # change value of predicted points outside experimental range in min or max of experimental range
    predicted.value[which(predicted.value > max(realValue))] = max(realValue)*1.05
    par(srt = 45) # point lables orientation
    plot(min.max.predictions, min.max.predictions, main=  paste("Submission", i), type= 'n', col = "#0000FFFF", xlab="Experimental values", ylab="Predicted values", cex.axis=1.3, cex.lab=1.5, cex.main=1.5, cex = 2)
    points(realValue, predicted.value, pch=20, cex = 2, col = color.list)
    text(realValue, predicted.value, labels = real.data[1:nrow(real.data), 1], col = color.list, pos = 4, cex=1.3) # comment this line to remove point lables
    segments(min.max.predictions[1], min.max.predictions[1], min.max.predictions[2], min.max.predictions[2], col= "#0000FFFF", lty= 2, lwd = 2)
  }
  
  dev.off()

}
