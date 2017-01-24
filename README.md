# Logistic Regression

This is an R function to run an example logistic regression using glmnet package. You will need to clone the repository:

    git clone https://github.com/mmdavid/R_lasso_example.git
    cd R_lasso_example/code

You can run the script as follows to see `--help`

    Rscript  logistic_regression.R --help

Then run a sample analysis:

    Rscript logistic_regression.R -i ../data/all_datasets.Rda -c ../data/classallsamples.Rda -o ../output_folder/

Notes: The data exemple are stored in the /data folder.

The script requires at least 3 arguments: 

-i input file, needs to be .Rda file with the data to analyze (see exemple above)

-c classes to which the samples belong, either 0 or 1. Needs to be .Rda file. 

-o path to output folder to store the plots generated as well as the file 'output_binomial_regression.Rda'

The other aguments are optionals, you can see them using the --help option.

This script will run an example of feature selection using lasso and cross-validation on the training dataset only.


The output gives three files at least: 

- AUC_graph.pdf: Boxplot of the performance of each classifier for each dataset for 20 repetitions. The number of repetition can be changed by the option -r. 

- Log10_of_feature_weigth.pdf: Boxplot ranking the absolute value of the mean of the weight attributed to each feature. 

- output_binomial_regression.Rda: an R object with name and weight of the features. 

An example of output is provided in the folder: example_results/
