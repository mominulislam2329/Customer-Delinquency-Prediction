## Customer Delinquency Prediction

The aim of this project is to fit a logistic and Multivariate adaptive regression splines (MARS) model using real accounts data from a credit card company in Sioux Falls, South Dakota for predicting if a customer is delinquent or not and determine which model performs best.


The concept of binning will be applied in this project. Binning refers to dividing a list of continuous variables into groups (bins) to discover group patterns and impacts. For example, if you have data about a group of people, you might want to arrange their ages into a smaller number of age intervals. The MARS provide a convenient approach to capture the non-linear relationships in the data by assessing cutpoints (knots) similar to step functions. The procedure assesses each data point for each predictor as a knot and creates a linear regression model with the candidate features. Model comparison is done to compare the predictive power of the two models using the Receiver Operator Characteristics curves(ROC) and the Kolmogorov-Smirnov (KS) statistic.
