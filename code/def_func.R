#This script defines functions sourced in the logistic_regression.R

#################################################################################
#Setting up the options for the script
#################################################################################
#This script uses six options
option_list = list(
  make_option(c("-i", "--input"), action="store", default=NA, type='character',
              help="Input file  -file.Rda "),
  #Input file. Need to be an .Rda file. In our example this file is a list of 12 dataframes
  #Each dataframe has 88 rows which correspond to the samples used as training dataset
  #The value in each dataframe depends on the normalization used. 
  #FOUR normalizations: 1/Original dataset='no_norm' 2/transformation log10 +1=sybset_ok_log
  # 3/ value / mean => xi/μi 4/ value - mean / SD => (xi-μi)/σ = 'red_sd'
  #THREE filtering methods: no filter = 'sybset_ok'; 
  #Spearman cut-off r2 at 0.99='no_cor_099'; Spearman cut-off r2 at 0.9='no_cor_09'
  #setting up the classe for each 
  make_option(c("-c", "--classes"), action="store", default=NA, type='character',
              help="Binary classes the samples belong to"),
  #This file needs to be an .Rda, integrer 1 or 0 depending on the classe the samples belongs to. 
  #Same length as the number of sample. 
  make_option(c("-o", "--output"), action="store", default=NA, type='character',
              help="Output folder"),
  #Where do you want the output to be
  make_option(c("-g", "--graph"), action="store", default=TRUE, type='logical',
              help="Logical: will the function produce a graph. [default %default]"),
  #How many repetitions of cross validation per feature do you want to calculate 
  #the area under the curve. Default: 20.
  make_option(c("-r", "--repetition"), action="store", default=20, type='integer',
              help="Integer: how many cross validation repetitions to performed 
              by datasets. [default %default]"),
  #If verbose is set to TRUE (default) then the output will be both printed on 
  #the screen AND in a log file 
  make_option(c("-v", "--verbose"), action="store", default=TRUE, type='logical',
              help="Make the program be verbose. [default %default]")
)

#################################################################################
#Main function using package glmnet to select the features using lasso
#################################################################################
#This function requires the glmnet package, already loaded in the logistic_regression.R script
logisticestimation <- function(listofdataset, classokl, numrep){
  AUC <- c()
  acc <- c()
  listtoreturn <- list()
  #We will iterate for each dataset in the input .Rda, which is a list dataframes
  for (i in 1:length(listofdataset)){
    outputlogitic <- list()
    dataset <- as.matrix(listofdataset[[i]])
    AUCok <- c()
    cat("running iteration ", i, " of ", length(listofdataset),"\n")
    
    #By default we will perform cross-validation 20 times for each boxplot.
    #This parameter can be changed with the option -r
    for (j in 1:numrep){
      cvfit <- cv.glmnet(dataset, classokl, family = "binomial", 
        type.measure = "auc", nfold=5)
      a <- predict(cvfit, dataset, s = "lambda.min", family = "binomial", 
        type = "class", type.measure = "auc" )
      lea <- length(classokl[a == classokl])/length(classokl)

      # How did the model perform for the alpha = 1 and the optimal lambda
      AUtmp <- cvfit$cvm[cvfit$lambda == cvfit$lambda.min]
      AUCok <- c(AUCok,AUtmp)
    }

    # Finally we select features of inportance, with a weight different than zero
    # Meaning that the feature matters for at least one of the two classes
    coefchosen <- coef(cvfit, s = "lambda.min")
    coefchosenok <- coefchosen[coefchosen[, 1] != 0, 1]
    names(coefchosenok) <- rownames(coefchosen)[coefchosen[, 1] != 0]
    nbfeatures <- (length(coefchosenok)-1)
    outputlogitic[[1]] <- sort(coefchosenok, decreasing = TRUE)
    outputlogitic[[2]] <- AUCok
    outputlogitic[[3]] <- lea
    outputlogitic[[4]] <- nbfeatures
    listtoreturn[[i]] <- outputlogitic
  }
  return(listtoreturn)
}

#################################################################################
#Function extracting the features weigth, to find the features of importance
#################################################################################
#Here we're suing the output from the previous function 'logisticestimation'
namefeatures <- function(dt){
  namokdup = c()
  for (i in 1:length(dt)){
    nam <- names(dt[[i]])
    namokdup <- c(namokdup,nam)
  }
  namoknodup <- unique(namokdup)
  return(namoknodup)
}