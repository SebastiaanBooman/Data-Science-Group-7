import pandas as pd
import numpy as np
import custom_metrics as cmetric
from sklearn import preprocessing
from sklearn import cross_validation
from sklearn import linear_model

# Read data files:
df_train = pd.read_csv(path + "/input/train.csv")
df_test  = pd.read_csv(path + "/input/test.csv")

#print df.shape
#(50999, 34)

#convert categorical features into integers
feature_cols_obj = [col for col in df_train.columns if df_train[col].dtypes == 'object']
le = preprocessing.LabelEncoder()
for col in feature_cols_obj:
    df_train[col] = le.fit_transform(df_train[col])
    df_test[col] = le.transform(df_test[col])

#Scale the data so that each feature has zero mean and unit std
feature_cols = [col for col in df_train.columns if col not in ['Hazard','Id']]
scaler = preprocessing.StandardScaler().fit(df_train[feature_cols])
df_train[feature_cols] = scaler.transform(df_train[feature_cols])                               
df_test[feature_cols] = scaler.transform(df_test[feature_cols]) 

#polynomial features/interactions
X_train = df_train[feature_cols]
X_test = df_test[feature_cols]
y = df_train['Hazard']
test_ids = df_test['Id']
poly = preprocessing.PolynomialFeatures(2)
X_train = poly.fit_transform(X_train)
X_test = poly.fit_transform(X_test)

#do grid search to find best value for alpha
#alphas = np.arange(-10,3,1)        
#clf = linear_model.RidgeCV(10**alphas)
alphas = np.arange(100,10000,10)        
clf = linear_model.RidgeCV(alphas)
clf.fit(X_train, y)
print clf.alpha_  
#clf.alpha=6060

cv = cross_validation.KFold(df_train.shape[0], n_folds=10)
mse = []
mse_train = []
fold_count = 0
for train, test in cv:
    print("Processing fold %s" % fold_count)
    train_fold = df_train.ix[train]
    test_fold = df_train.ix[test]

    # Get training examples
    X_train = train_fold[feature_cols]
    y = train_fold['Hazard']
    X_test = test_fold[feature_cols]
    #interactions
    poly = preprocessing.PolynomialFeatures(2)
    X_train = poly.fit_transform(X_train)
    X_test = poly.fit_transform(X_test)

    # Fit Ridge linear regression 
    cfr = linear_model.Ridge (alpha = 6060)
    cfr.fit(X_train, y)

    # Check error on test set
    pred = cfr.predict(X_test)

    mse.append(cmetric.normalized_gini(test_fold.Hazard, pred))

    # Check error on training set (Resubsitution error)
    mse_train.append(cmetric.normalized_gini(y, cfr.predict(X_train)))    

    # Done with the fold
    fold_count += 1

    #print model coeff

print cfr.coef_

print pd.DataFrame(mse).mean()
#0.311794
print pd.DataFrame(mse_train).mean()
#.344775