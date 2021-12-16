#!/usr/bin/env python
# -*- coding: utf-8 -*-

######### General #########
import numpy as np
import pandas as pd
import os, sys, re

######### Problem Type #########
######### Change!!!!!! #########
eval_type = 'auc' #{'logloss', 'auc', 'rmse'}

problem_type = 'classification' #{'classification','regression'}

classification_type = 'binary'# {'binary', 'multi-class'}



######### PATH #########
######### Change main folder name #########
FOLDER_NAME = 'Santander'
PATH = ''
INPUT_PATH = '/content/drive/MyDrive/data/input/' #path of original data and fold_index
OUTPUT_PATH = '/content/drive/MyDrive/data/output/temp/' #path of saving each stacking prediction
FEATURES_PATH = '/content/drive/MyDrive/data/output/features/' #path of dataset created in feat_verXX.py


# for creating fold index
ORIGINAL_TRAIN_FORMAT = 'features_train.csv'
# for saving the submitted format file
SUBMIT_FORMAT = 'sample_submission.csv'




######### BaseEstimator ##########
from sklearn.base import BaseEstimator
from sklearn.base import RegressorMixin, ClassifierMixin
from sklearn.base import TransformerMixin

######### Keras #########
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from tensorflow.keras.optimizers import SGD
from keras.utils import np_utils

######### XGBoost #########
import xgboost as xgb

######### Evaluation ##########
from sklearn.metrics import log_loss as ll
from sklearn.metrics import roc_auc_score as AUC
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import KFold, StratifiedKFold
from sklearn.metrics import mean_squared_error


######### CV index #########
cv_id_filename = '' #change if using fixed cv_index file
n_folds = 5


######### Utils #########

#loading data from feature list
def load_data(flist, drop_duplicates=False):

    flist_len = len(flist['train'])
    X_train = pd.DataFrame()
    test = pd.DataFrame()
    for i in range(flist_len):
        X_train = pd.concat([X_train,pd.read_csv(PATH+flist['train'][i])],axis=1)
        test = pd.concat([test,pd.read_csv(PATH+flist['test'][i])],axis=1)

    y_train = X_train['target']
    del X_train['target']
    #del test['t_id']
    #print X_train.columns
    #print test.columns
    assert( (False in X_train.columns == test.columns) == False)
    print ('train shape :{}'.format(X_train.shape))
    if drop_duplicates == True:
        #add for ver.12. Use later version than ver.12.
        #delete identical columns
        unique_col = X_train.T.drop_duplicates().T.columns
        X_train = X_train[unique_col]
        test = test[unique_col]
        assert( all(X_train.columns == test.columns))
        print ('train shape after drop_duplicates :{}'.format(X_train.shape))

    return X_train, y_train, test 

#saving prediction as submission format
def save_pred_as_submit_format(pred_path, output_file, col_name=('ID', "TARGET")):
    print ('writing prediction as submission format')
    print ('read prediction <{}>'.format(pred_path))
    pred = pd.read_csv(pred_path).values
    #(((test.mean(1) - test.mean(1).mean())/test.mean(1).std()/100. + 0.5).values + pred)/2.0
    submission = pd.read_csv(INPUT_PATH+SUBMIT_FORMAT)
    submission[col_name[1]] = pred
    submission.to_csv( output_file, columns = col_name, index = None )
    print ('done writing')
    return

#evalation function
def eval_pred( y_true, y_pred, eval_type=eval_type):
    if eval_type == 'logloss':#eval_typeはここに追加
        print ("logloss: ", ll( y_true, y_pred ))
        return ll( y_true, y_pred )             
    
    elif eval_type == 'auc':
        print ("AUC: ", AUC( y_true, y_pred ))
        return AUC( y_true, y_pred )             
    
    elif eval_type == 'rmse':
        print ("rmse: ", np.sqrt(mean_squared_error(y_true, y_pred)))
        return np.sqrt(mean_squared_error(y_true, y_pred))






######### BaseModel Class #########

class BaseModel(BaseEstimator):
    """
    Parameters of fit
    ----------
    FEATURE_LIST = {
                    'train':('flist_train.csv'),#targetはここに含まれる
                    'test':('flist_test.csv'),
                    }

    Note
    ----
    init: compiled model
    

    
    (Example)
    from base import BaseModel, XGBClassifier
    FEATURE_LIST = ["feat.group1.blp"]
    PARAMS = {
            'n_estimator':700,
            'sub_sample': 0.8,
            'seed': 71
        }
    class ModelV1(BaseModel):
         def build_model(self):
         return XGBClassifier(**self.params)


    if __name__ == "__main__":
        m = ModelV1(name="v1",
                    flist=FEATURE_LIST,
                    params=PARAMS,
                    kind='s')
        m.run()
   
    """
    def __init__(self, name="", flist={}, params={}, kind='s', fold_name=''):
        '''
        name: Model name
        flist: Feature list
        params: Parameters
        kind: Kind of run() 
        {'s': Stacking only. Svaing a oof prediction({}_all_fold.csv)
              and average of test prediction based on fold-train models({}_test.csv).
         't': Training all data and predict test({}_TestInAllTrainingData.csv).
         'st': Stacking and then training all data and predict test
               Using save final model with cross-validation
         'cv': Only cross validation without saving the prediction

        '''
        self.name = name
        self.flist = flist
        self.params = params
        self.kind = kind
        self.fold_name = fold_name
        assert(self.kind in ['s', 't', 'st', 'cv'])
        

    def build_model(self):
        return None


    def run(self):
        print ('running model: {}'.format(self.name))
        X, y, test = self.load_data()
        #print X.shape, test.shape
        #print X.dropna().shape, test.dropna().shape

        if self.kind == 't':
            clf = self.build_model()
            clf.fit(X, y)
            if problem_type == 'classification':
                y_submission = clf.predict_proba(test)#[:,1]#multi-class => 消す #コード変更
            elif problem_type == 'regression':
                y_submission = clf.predict(test)#[:,1]#multi-class => 消す #コード変更

            y_submission = pd.DataFrame(y_submission,columns=['{}_pred'.format(self.name)])
            y_submission.to_csv(OUTPUT_PATH+'{}_TestInAllTrainingData.csv'.format(self.name),index=False)
            save_pred_as_submit_format(OUTPUT_PATH+'{}_TestInAllTrainingData.csv'.format(self.name), OUTPUT_PATH+'OK_{}_TestInAllTrainingData.csv'.format(self.name))
            return 0 #saving only
        
        print ('using fold: {}'.format(self.fold_name))
        #skf = pd.read_pickle(INPUT_PATH+cv_id_filename)
        a = pd.read_csv('/content/drive/MyDrive/data/input/5fold_20times.csv')
        cv_index = {}
        set_name = self.fold_name
        #creating cv_index format
        for i in range(5):
            train_cv = a.loc[(a[set_name]!=i).values, set_name].index
            test_cv = a.loc[(a[set_name]==i).values, set_name].index
            cv_index[i] = {}
            cv_index[i]['train'] = train_cv.values
            cv_index[i]['test'] = test_cv.values

        skf = pd.DataFrame(cv_index).stack().T
        clf = self.build_model()
        print ("Creating train and test sets for stacking.")
        #print "\nLevel 0"

        ############# for binary #############
        if problem_type == 'regression' or classification_type == 'binary':
            dataset_blend_train = np.zeros(X.shape[0]) #array for prediction of train
            dataset_blend_test = np.zeros(test.shape[0]) #array for prediction of test
    
            #stacked_data_columns = X.columns.tolist()
            dataset_blend_test_j = np.zeros((test.shape[0], n_folds))
        
        ############# for multi-class #############
        elif classification_type == 'multi-class':
            #TODO
            pass


        evals = []
        for i in range(n_folds):# of n_folds
            train_fold = skf['train'][i]
            test_fold = skf['test'][i]
            print ("Fold", i)
            #print X
            #print train_fold
            X_train = X.loc[train_fold].dropna(how='all')
            y_train = y.loc[train_fold].dropna(how='all')
            X_test = X.loc[test_fold].dropna(how='all')
            y_test = y.loc[test_fold].dropna(how='all')
            
            #print X_train,y_train,X_test,y_test
            clf.fit(X_train, y_train)

            if problem_type == 'classification' and classification_type == 'binary':            
                #if using the mean of the prediction of each n_fold
                #print str(type(clf))
                if 'sklearn' in str(type(clf)):
                    y_submission = clf.predict_proba(X_test)[:,1]
                else:
                    y_submission = clf.predict_proba(X_test)

            elif problem_type == 'regression':      
                y_submission = clf.predict(X_test)

            #add .values for numpy.
            #print test_fold
            #print y_submission
            #print dataset_blend_train
            #dataset_blend_train[test_fold.values] = y_submission
            try:
                dataset_blend_train[test_fold] = y_submission
            except:
                dataset_blend_train[test_fold.values] = y_submission

            
            #using eval_pred function
            evals.append(eval_pred(y_test, y_submission, eval_type))

            ############ binary classification ############
            if problem_type == 'classification' and classification_type == 'binary':            
                #if using the mean of the prediction of each n_fold
                if 'sklearn' in str(type(clf)):
                    dataset_blend_test_j[:, i] = clf.predict_proba(test)[:,1]
                else:
                    dataset_blend_test_j[:, i] = clf.predict_proba(test)

            ############ multi-class classification ############
            elif problem_type == 'classification' and classification_type == 'multi-class':            
                #TODO
                #if using the mean of the prediction of each n_fold
                #dataset_blend_test_j += clf.predict_proba(test)
                #dataset_blend_test_j /= n_folds
                pass

            ############ regression ############
            elif problem_type == 'regression':      
                #if using the mean of the prediction of each n_fold
                dataset_blend_test_j[:, i] = clf.predict(test)


        dataset_blend_test = dataset_blend_test_j.mean(1)
        
        for i in range(n_folds):
            print( 'Fold{}: {}'.format(i+1, evals[i]))
        print ('{} Mean: '.format(eval_type), np.mean(evals), ' Std: ', np.std(evals))

        #if not cv only
        if self.kind != 'cv':
            print ('Saving results')
            dataset_blend_train = pd.DataFrame(dataset_blend_train,columns=['{}_stack'.format(self.name)])
            dataset_blend_train.to_csv(OUTPUT_PATH+'{}_all_fold.csv'.format(self.name),index=False)
            dataset_blend_test = pd.DataFrame(dataset_blend_test,columns=['{}_stack'.format(self.name)])
            dataset_blend_test.to_csv(OUTPUT_PATH+'{}_test.csv'.format(self.name),index=False)
            #save test prediction as submission format
            #save_pred_as_submit_format(OUTPUT_PATH+'{}_test.csv'.format(self.name), OUTPUT_PATH+'OK_{}_test.csv'.format(self.name))
        
        if self.kind == 'st':
            #train with all trainset after stacking(cross-validation)
            clf = self.build_model()
            clf.fit(X, y)
            if problem_type == 'classification':
                if 'sklearn' in str(type(clf)):
                    y_submission = clf.predict_proba(test)[:,1]#multi-class => 消す #コード変更
                else:
                    y_submission = clf.predict_proba(test)
            elif problem_type == 'regression':
                y_submission = clf.predict(test)#[:,1]#multi-class => 消す #コード変更
            
            y_submission = pd.DataFrame(y_submission,columns=['{}_pred'.format(self.name)])
            y_submission.to_csv(OUTPUT_PATH+'{}_TestInAllTrainingData.csv'.format(self.name),index=False)
            save_pred_as_submit_format(OUTPUT_PATH+'{}_TestInAllTrainingData.csv'.format(self.name), OUTPUT_PATH+'OK_{}_TestInAllTrainingData.csv'.format(self.name))

        return



    def load_data(self):

        return load_data(self.flist, drop_duplicates=True )
        





######### Wrapper Class of Classifiers #########
import logging
from keras.callbacks import Callback


class IntervalEvaluation(Callback):
    def __init__(self, validation_data=(), interval=10):
        super(Callback, self).__init__()

        self.interval = interval
        self.X_val, self.y_val = validation_data

    def on_epoch_end(self, epoch, logs={}):
        if epoch % self.interval == 0:
            predict_x=self.model.predict(self.X_val, verbose=0) 
            y_pred=np.argmax(predict_x,axis=1).reshape(-1, 1)
            score = AUC(self.y_val, y_pred)
            #logging.info("interval evaluation - epoch: {:d} - score: {:.6f}".format(epoch, score))
            print ("interval evaluation - epoch: {:d} - score: {:.6f}".format(epoch, score))


class KerasClassifier(BaseEstimator, ClassifierMixin):
    """
    (Example)
    from base import KerasClassifier
    class KerasModelV1(KerasClassifier):
        ###
        #Parameters for lerning
        #    batch_size=128,
        #    epochs=100,
        #    verbose=1, 
        #    callbacks=[],
        #    validation_split=0.,
        #    validation_data=None,
        #    shuffle=True,
        #    show_accuracy=False,
        #    class_weight=None,
        #    sample_weight=None,
        #    normalize=True,
        #    categorize_y=False
        ###
        
        def __init__(self,**params):
            model = Sequential()
            model.add(Dense(input_dim=X.shape[1], output_dim=100, init='uniform', activation='relu'))
            model.add(Dropout(0.3))
            model.add(Dense(input_dim=50,output_dim=2, init='uniform'))
            model.add(Activation('softmax'))
            model.compile(optimizer='adam', loss='binary_crossentropy',class_mode='binary')

            super(KerasModelV1, self).__init__(model,**params)
    
    KerasModelV1(batch_size=8, epochs=10, verbose=1, callbacks=[], validation_split=0., validation_data=None, shuffle=True, show_accuracy=True, class_weight=None, sample_weight=None, normalize=True, categorize_y=True)
    KerasModelV1.fit(X_train, y_train,validation_data=[X_test,y_test])
    KerasModelV1.predict_proba(X_test)[:,1]
    """

    def __init__(self,nn,batch_size=128, epochs=100, verbose=1, callbacks=[],
            validation_split=0., validation_data=None, shuffle=True,
             class_weight=None, sample_weight=None, normalize=True, categorize_y=False):
        self.nn = nn
        self.batch_size = batch_size
        self.epochs = epochs
        self.verbose = verbose
        self.callbacks = callbacks
        self.validation_split = validation_split
        self.validation_data = validation_data
        self.shuffle = shuffle
        self.class_weight = class_weight
        self.sample_weight = sample_weight
        self.normalize = normalize
        self.categorize_y = categorize_y
        #set initial weights
        self.init_weight = self.nn.get_weights()

    def fit(self, X, y, validation_data=None):
        X = X.values#Need for Keras
        y = y.values#Need for Keras
        if validation_data != None:
            self.validation_data = validation_data
            if self.normalize:
                self.validation_data[0] = (validation_data[0] - np.mean(validation_data[0],axis=0))/np.std(validation_data[0],axis=0)
            if self.categorize_y:
                self.validation_data[1] = np_utils.to_categorical(validation_data[1])

        if self.normalize:
            self.mean = np.mean(X,axis=0)
            self.std = np.std(X,axis=0) + 1 #CAUSION!!!
            X = (X - self.mean)/self.std
        if self.categorize_y:
            y = np_utils.to_categorical(y)
        
        #set initial weights
        self.nn.set_weights(self.init_weight)

        #set callbacks
        self.callbacks = [IntervalEvaluation(validation_data=(X, y), interval=5)]
        #print self.callbacks

        #print all(pd.DataFrame(np.isfinite(X)))
        #print X.shape
        return self.nn.fit(X, y, batch_size=self.batch_size, epochs=self.epochs, verbose=self.verbose, callbacks=self.callbacks, validation_split=self.validation_split, validation_data=self.validation_data, shuffle=self.shuffle, class_weight=self.class_weight, sample_weight=self.sample_weight)

    def predict_proba(self, X, batch_size=128, verbose=1):
        X = X.values#Need for Keras
        if self.normalize:
            X = (X - self.mean)/self.std
        
        if classification_type == 'binary':
            return self.nn.predict_proba(X, batch_size=batch_size, verbose=verbose)[:,1]#multi-class => 消す #コード変更
        elif classification_type == 'multi-class':
            return self.nn.predict_proba(X, batch_size=batch_size, verbose=verbose)


class XGBClassifier(BaseEstimator, ClassifierMixin):
    '''
    (Example)
    from base import XGBClassifier
    class XGBModelV1(XGBClassifier):
        def __init__(self,**params):
            super(XGBModelV1, self).__init__(**params)

    a = XGBModelV1(colsample_bytree=0.9, learning_rate=0.01,max_depth=5, min_child_weight=1,n_estimators=300, nthread=-1, objective='binary:logistic', seed=0,silent=True, subsample=0.8)
    a.fit(X_train, y_train, eval_metric='logloss',eval_set=[(X_train, y_train),(X_test, y_test)])
    
    '''
    def __init__(self, params={}, num_round=50 ):
        self.params = params
        self.num_round = num_round

        self.clf = xgb
        
    def fit(self, X, y=[], sample_weight=None, eval_set=None, eval_metric=None,
            early_stopping_rounds=None, verbose=True):
        
        dtrain = xgb.DMatrix(X, label=y,missing=-999)
        
        watchlist  = [(dtrain,'train')]
        
        self.clf = xgb.train(self.params, dtrain, self.num_round, watchlist)
        return self.clf

    def predict_proba(self, X, output_margin=False, ntree_limit=0):
        dtest = xgb.DMatrix(X,missing=-999)
        #return self.clf.predict(X, output_margin=output_margin, ntree_limit=ntree_limit)
        
        return self.clf.predict(dtest)




