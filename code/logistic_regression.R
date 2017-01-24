#!/usr/bin/env Rscript
#logistic regression: package glmnet 
#Options handling: package optparse
suppressMessages(require(glmnet))
suppressMessages(require(optparse))

#Setting a seed for reproducibility
set.seed(48)

#Source the script with the function
source("def_func.R")

#The options for the script were set up in the def_fun script
#Here we are setting up option_list
opt = parse_args(OptionParser(usage = "\nExample: Rscript %prog 
  -i ../data/all_datasets.Rda -c ../data/classallsamples.Rda -o ../output_folder/", 
  option_list=option_list))

#Create the output directory defined by the user with option -o in the terminal
dir.create(paste(opt$output))

#If verbose option is set to TRUE then the output will be printed on the screen
#If verbose option is set to FALSE then the output will be in a log.txt file 
if (!opt$verbose) {sink(paste(opt$output, "log.txt"))}

#Notifications if the input file or output folder or the classes are missing
if(!is.na(opt$input) & !is.na(opt$output) & !is.na(opt$classes))  {
  cat(paste("", "Input file:" , opt$input, "\n", "Output folder:", opt$output, 
    "\n", "Class of the samples:", opt$classes, sep=" ", "\n"))
  cat("Number of repetitions", opt$repetition, "\n", sep=" ")
} else {
  cat("You didn't specify the input file, the output folder and/or the classes 
    of the samples, see --help")
  abort
}

#Load the dataset as defined by the user with the -i option
# In the example here: 12 dataframes: 4 normalizations * 3 filters
all_datasets <- get(load(paste(opt$input)))
classokf <- get(load(paste(opt$classes)))

#The next step will be to perform the logistic regression and the selection feature using lasso
#We will use the function 'logisticestimation' defined in the 'def_fun.R' script
#In our example this will be performed on the 12 datasets selected previously
#Each area under the curve (AUC) will be calculated 20 times by defaults 
#(or whatever number the user chooses with the option -r)
Testall <- logisticestimation(all_datasets, classokf, opt$repetition)

#Saving the AUC for each datasets
names(Testall) <- names(all_datasets)
AUC <- lapply(Testall, function(x){x[[2]]})

#Here we are generating a PDF with the boxplot of each AUC for each dataset
#If the user sets the option -g to FALSE: this step is skipped
if (opt$graph == TRUE){
  pdf(paste(opt$output,"AUC_graph.pdf"))
  boxplot(AUC, las = 2, cex.axis = 0.5, xlab = "Datasets",
   ylab = "Area Under the Curve" )
  dev.off()}

#Now extracting the features weight, to find the features of importance for both NSCLC & SCLC groups
#The functions here were defined in the def_fun.R script
features<-lapply(Testall, function(x){x[[1]]})
namok<-namefeatures(features)

#Find all the features across all the datasets
namokl <- list()
for (j in 1:length(namok)){
  tmpok <- c()
  for (i in 1:length(features)){
    tmp <- features[[i]][names(features[[i]]) == namok[j]]
    tmpok <- c(tmpok,tmp)
  }
  namokl[[j]] <- tmpok
}
namok <- gsub("X.", "", namok)
names(namokl) <- namok

#Remove the intercept
namokl_both <- namokl[names(namokl) != '(Intercept)']

#And save the features in the output file
save(namokl_both, file = paste(opt$output, "output_binomial_regression.Rda", sep=""))

#Finally generating boxplots ranking the features of importance if the option --graph is set to TRUE (default)
#In order to generate the plot, make the log + 1 transformation
namokl_nsclc_tr <- lapply(namokl_both, function(x){log10(abs(x)+1)})

# Save the plot sorted by the mean of the weight of each feature. 
if (opt$graph == TRUE){
  mean_namokl_nsclc_tr <- (lapply(namokl_nsclc_tr, function(x){mean(x)})) 
  pdf(paste(opt$output,"Log10_of_feature_weigth.pdf"))
  boxplot(namokl_nsclc_tr[order(unlist(mean_namokl_nsclc_tr), decreasing = TRUE)],
  las=2, cex.axis = 0.5, xlab = "Datasets", 
  ylab = "Log10 of features weigth involved in SCLC and NSCLC")
  dev.off()}
