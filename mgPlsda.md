## PLS-DA Class Prediction 

Version: 2.5 (MCR R2016A)
Creator: Rik de Wijn
Last Modification Date: March 22, 2016

##### Description:

-Partial Least Squared Discriminant Analysis
-Type: Matlab Operator Step

Performs partial least squares discriminant analysis. The method has been
slightly adapted compared to previous versions following some of the
recommendations in [...]. Multiple group analysis is supported, but
working with more than 2 groups may not be the main strength of the
method. 

The classifier implements a bagging methods for resampling the mode;.
In particular, for two group classification with unequal group sizes a Random Under Sampling scheme called Balance is implemented.
With balance multiple predictors with
equal group sizes are bagged by drawing a random sample from the larger
group NumberOfBags times.

The operator trains a PLS classifier based on the data in the calling
Bionavigator spreadsheet and known grouping. Training involves determining
the optimal number of PLS components by (inner) cross validation. The
classifier may be stored for later use with new data (see
predict.classification, not yet available in Tercen)

In addition the classifier performance is estimated by (outer) cross validation.
Here, the optimization of the number of PLS components is repeated for
each fold of the cross validation using the training set only (double
cross validation).

#####INPUT:

Array data from Bionavigator Spreadsheet with grouping defined using a single DataColor. 
Using more than a single value per cell results in an error, missing values are not allowed.
SpotID's have to specified in the BN spreadsheet

##### PARAMETERS

1. MaxComponents [10(dft)], the maximum number of PLS-components allowed. 
2. AutoScale [No(dft), Yes], autoscale spots.
3. NumberOfBags [24 (dft)]. Number of bags to use when bagging is applied
4. Bagging. [None, Balance, Bootstrap, Jackknife]
4. CrossValidation [LOOCV (dft), 10-fold, 20-fold, none]: cross validation type
5. Optimization [auto (dft), LOOCV, 10-fold, 20-fold, none]. Cross
 validation type for selecting the optimal number of pls components [1:MaxComponents]. With 'auto' the same type of CV as selected with 'CrossValidation'
 is used, unless CrossValidation = 'none' in which case LOOCV is applied.
 With Optimization = 'none', optimization is skipped and MaxComponents is used for building the classifier. 
6. Permutations [0(dft)], number of label permutations required
7. SaveClassifer [no(dft), yes], if yes the Operator prompts the user for
saving the obtained classifier.

##### OUTPUT (RETURNED TO BIONAVIGATOR/TERCEN):
Per sample: 
y<ClassName>, class affinity for each class predicted using the outer
cross validation, the predicted class is the one with largest affinity.
pamIndex (2 class prediction only): y predictions converted to the "PamIndex" format.
Per spot:
beta(1..N), were N is the number of groups. Relative weights of spots in the class
prediction rule. Beta(1..N-1) is usually sufficient. These are scaled to unit variance, Therefore do not use these
weights for predicting new samples, use the complimentary
predict.classification operator!

##### OUTPUT (SHOWRESULTS)

1. Plot of cross validated y predictions in "PamIndex" format (2-group
classification only)
2. Diagnostics plot showing the final peptide weights plus the weights
obtained from the succesive cross validation folds.
3. Diagnostics plot showing the number of pls components used in the final
model and in the succesive cross validation folds.
4. Cumulative distribution plot showing the distribution of error rate and <ClassName> 
predictive value obtained from label permutations (if any).
5. Tab delimited text file (.xls, best viewed using MS-Excel) with details on
classifier performance and cross validated predictions (not yet available in tercen)
