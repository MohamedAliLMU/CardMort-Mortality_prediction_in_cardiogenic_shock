# ================================
# Standard Library
# ================================
import random
import time
import warnings
import pickle

# ================================
# Data Manipulation
# ================================
import numpy as np
import pandas as pd

# ================================
# Visualization
# ================================
import matplotlib.pyplot as plt
import seaborn as sns
import shap

# ================================
# Preprocessing & Feature Engineering
# ================================
from sklearn.base import BaseEstimator, TransformerMixin, ClassifierMixin
from sklearn.preprocessing import StandardScaler, RobustScaler, PowerTransformer
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline, make_pipeline
import category_encoders as ce
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer

# ================================
# Model Selection & Evaluation
# ================================
from sklearn.model_selection import (
    train_test_split, cross_val_score, GridSearchCV, RandomizedSearchCV,
    StratifiedKFold, KFold
)
from sklearn.metrics import (
    accuracy_score, precision_score, recall_score, f1_score, roc_auc_score,
    average_precision_score, precision_recall_curve, confusion_matrix,
    classification_report, roc_curve, auc,
    mean_absolute_error, mean_squared_error, explained_variance_score
)
from imblearn.metrics import classification_report_imbalanced

# ================================
# Machine Learning Models
# ================================
from sklearn.ensemble import (
    RandomForestClassifier, GradientBoostingClassifier, AdaBoostClassifier, 
    BaggingClassifier
)
from sklearn.linear_model import (
    LogisticRegression, SGDClassifier
)
from sklearn.naive_bayes import GaussianNB
from sklearn.neural_network import MLPClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
from sklearn.neighbors import KNeighborsClassifier, NearestCentroid
from sklearn.svm import SVC, NuSVC, LinearSVC
from sklearn.tree import DecisionTreeClassifier, ExtraTreeClassifier
from sklearn.gaussian_process import GaussianProcessClassifier
import xgboost as xgb

# ================================
# Imbalanced Data Handling
# ================================
from imblearn.pipeline import make_pipeline as imbalanced_make_pipeline
from imblearn.over_sampling import (
    SMOTE, ADASYN, KMeansSMOTE, SVMSMOTE, SMOTENC, BorderlineSMOTE
)
from imblearn.under_sampling import NearMiss, RandomUnderSampler

# ================================
# Optimization & Experiment Tracking
# ================================
import optuna
from optuna.integration import XGBoostPruningCallback
from dvclive.xgb import DVCLiveCallback

# ================================
# Additional Tools & Utilities
# ================================
from scipy.stats import uniform, randint, ttest_ind, chi2_contingency
from sklearn.datasets import make_classification
from sklearn.cluster import KMeans
import dice_ml
from sympy import plot

plt.rcParams['font.family'] = 'Arial'
# ================================
# Global Settings
# ================================
plt.rcParams['figure.figsize'] = [10, 8]
np.random.seed(42)
optuna.logging.set_verbosity(optuna.logging.CRITICAL)
warnings.filterwarnings('ignore')


def filter_df(df):

    # Sortiere nach ID und DayNumber aufsteigend
    df2 = df.sort_values(by=['ID', 'DayNumber'])

    # Definiere eine Funktion, um nach der Zeile mit HighestLactate >= 2 zu filtern
    def filter_after_lactate(group):
        # Finde den Index der ersten Zeile, in der HighestLactate >= 2 ist
        condition_met = group['HighestLactate'] >= 2
        
        if condition_met.any():
            # Finde den Index der ersten Zeile, die die Bedingung erfüllt
            first_valid_index = condition_met.idxmax()
            # Behalte nur die Zeilen nach dieser Zeile (inklusive dieser Zeile)
            return group.loc[first_valid_index:]
        else:
            # Wenn keine Zeile die Bedingung erfüllt, gibt es keine Zeilen zu behalten
            return pd.DataFrame()

    # Wende die Funktion auf jede Patientengruppe an
    df_filtered = df2.groupby('ID', group_keys=False).apply(filter_after_lactate)
    
    return df_filtered

def clean_data_small(dataset):
        
    # Cap the 'Heparin_weighted_mean' column values at 3000
    if 'Heparin_weighted_mean' in dataset.columns:
        dataset.loc[dataset['Heparin_weighted_mean'] > 3000, 'Heparin_weighted_mean'] = 3000
    
    # Cap the 'Heparin_full_dose' column values at 75,000    
    if 'Heparin_full_dose' in dataset.columns:
        dataset.loc[dataset['Heparin_full_dose'] > 75000, 'Heparin_full_dose'] = 75000
    
    # Cap the 'CKWeightedMean' column values at 25,000
    if 'CKWeightedMean' in dataset.columns:
        dataset.loc[dataset['CKWeightedMean'] > 25000, 'CKWeightedMean'] = 25000   
        
    # Cap the 'LDHMaxOnICUStay' column values at 20,000
    if 'LDHMaxOnICUStay' in dataset.columns:
        dataset.loc[dataset['LDHMaxOnICUStay'] > 20000, 'LDHMaxOnICUStay'] = 20000
        
    # Continue capping other variables with specific thresholds 
    if 'Pao2Artweightedmean' in dataset.columns:
        dataset.loc[dataset['Pao2Artweightedmean'] < 0, 'Pao2Artweightedmean'] = np.nan
        
    if 'TroponinIMax_inICUStay' in dataset.columns:
        dataset.loc[dataset['TroponinIMax_inICUStay'] > 100, 'TroponinIMax_inICUStay'] = 100
        
    if 'TroponinTMax_inICUStay' in dataset.columns:
        dataset.loc[dataset['TroponinTMax_inICUStay'] > 100, 'TroponinTMax_inICUStay'] = 100
    
    if 'TroponinTMin_inICUStay' in dataset.columns:
        dataset.loc[dataset['TroponinTMin_inICUStay'] > 100, 'TroponinTMin_inICUStay'] = 100
        
    if 'TroponinIMin_inICUStay' in dataset.columns:
        dataset.loc[dataset['TroponinIMin_inICUStay'] > 100, 'TroponinIMin_inICUStay'] = 100
    
    
    return dataset

def clean_data(df, encode=True, fillna=True, Troponin=True, log=True, scaling=True):
    """
    Cleans and preprocesses the input DataFrame by performing various data transformations, 
    including outlier handling, encoding categorical variables, filling missing values, 
    creating new features based on Troponin values, applying logarithmic transformation, 
    and scaling numerical features.

    Parameters:
    - df: DataFrame to be cleaned
    - encode: Boolean indicating whether to encode categorical variables
    - fillna: Boolean indicating whether to fill missing values with column means
    - Troponin: Boolean indicating whether to process Troponin-related features
    - log: Boolean indicating whether to apply logarithmic transformation
    - scaling: Boolean indicating whether to scale numerical features

    Returns:
    - Cleaned DataFrame
    """

    # Cap the values of certain features to specified maximum thresholds
    if 'Heparin_weighted_mean' in df.columns:
        df.loc[df['Heparin_weighted_mean'] > 3000, 'Heparin_weighted_mean'] = 3000
        
    if 'Heparin_full_dose' in df.columns:
        df.loc[df['Heparin_full_dose'] > 75000, 'Heparin_full_dose'] = 75000
        
    if 'CKWeightedMean' in df.columns:
        df.loc[df['CKWeightedMean'] > 25000, 'CKWeightedMean'] = 25000   
        
    if 'LDHMaxOnICUStay' in df.columns:
        df.loc[df['LDHMaxOnICUStay'] > 20000, 'LDHMaxOnICUStay'] = 20000
        
    if 'Pao2Artweightedmean' in df.columns:
        df.loc[df['Pao2Artweightedmean'] < -100, 'Pao2Artweightedmean'] = np.nan
        
    if 'TroponinIMax_inICUStay' in df.columns:
        df.loc[df['TroponinIMax_inICUStay'] > 100, 'TroponinIMax_inICUStay'] = 100
        
    if 'TroponinTMax_inICUStay' in df.columns:
        df.loc[df['TroponinTMax_inICUStay'] > 100, 'TroponinTMax_inICUStay'] = 100
    
    if 'TroponinTMin_inICUStay' in df.columns:
        df.loc[df['TroponinTMin_inICUStay'] > 100, 'TroponinTMin_inICUStay'] = 100
        
    if 'TroponinIMin_inICUStay' in df.columns:
        df.loc[df['TroponinIMin_inICUStay'] > 100, 'TroponinIMin_inICUStay'] = 100

    #------------------------------------------------------------------------------------------------
    
    # Encode categorical variables if the 'encode' flag is True
    if encode:
        # List of categorical columns to be encoded
        columns_to_code = [
            'GenderAtBirth',
            'OH_ProfessionalCPRTime_factors',
            'OH_TotalCPRTime_factors',
            'OH_EmergencyFibrinolysis_yes',
            'OH_ROSCAchieved',
            'IH_ProfessionalCPRTimeFromStayBegin_factors',
            'IH_TotalEpinephrineFromStayBegin_factors',
            'IH_EmergencyFibrinolysis',
            'FIO2weightedmean_factors',
            'Pinspweightedmean_factors',
            'Pexpweightedmean_factors',
            'TidalVolweightedmean_factors',
            'MinuteVolweightedmean_factors',
            'OH_TotalEpinephrine_factors',
            'Propofol_full_dose_binned', 
            'Epinephrine_full_dose_binned', 
            'Norepinephrine_full_dose_binned', 
            'Norepinephrine_weighted_mean_binned', 
            'Dobutamine_full_dose_binned', 
            'Vasopressin_full_dose_binned',
            'Catecholamines_binned', 
            'Midazolam_full_dose_binned', 
            'Ketamin_full_dose_binned', 
            'Sufentanyl_full_dose_binned', 
            'PipTaz_full_dose_binned', 
            'Meropenem_po_check_binned', 
            'Meropenem_iv_check_binned',
            'Amiodaron_full_dose_sum_binned', 
            'Digoxin_iv_check_binned', 
            'Heparin_weighted_mean_binned', 
            'Heparin_full_dose_binned', 
            'prednisolon_combi_po_check_binned', 
            'prednisolon_combi_iv_check_binned',
            'Epinephrine_duration_binned', 
            'Epinephrine_weighted_mean_binned', 
            'Dobutamine_duration_binned', 
            'Dobutamine_weighted_mean_binned', 
            'Vasopressin_duration_binned', 
            'Vasopressin_weighted_mean_binned',
            'Propofol_duration_binned', 
            'Propofol_weighted_mean_binned', 
            'Midazolam_duration_binned', 
            'Midazolam_weighted_mean_binned', 
            'Ketamin_duration_binned', 
            'Ketamin_weighted_mean_binned', 
            'Sufentanyl_duration_binned', 
            'Sufentanyl_weighted_mean_binned', 
            'PipTaz_duration_binned', 
            'PipTaz_weighted_mean_binned', 
            'Norepinephrine_duration_binned', 
            'Heparin_duration_binned',
            'Amiodaron_weighted_mean_sum_binned', 
            'vent_time_binned', 
            'RBCUnitsTaken_binned', 
            'GCS_Admission_binned', 
            'NumberOfPCIs_binned', 
            'Amiodaron_duration_binned',
            'CPConICUDischarge'
        ]
        
        # Filter to keep only columns that exist in the DataFrame
        vorhandene_Spalten_code = [column for column in columns_to_code if column in df.columns]
        
        # Apply one-hot encoding to categorical columns
        df = pd.get_dummies(df, columns=vorhandene_Spalten_code)
        
    # Convert boolean values to binary (True -> 1, False -> 0)
    df.replace(True, 1, inplace=True)
    df.replace(False, 0, inplace=True)
        
    # Fill missing values with the mean of the respective columns if the 'fillna' flag is True
    if fillna:
        df = df.fillna(df.mean())
            
    # Drop specific columns if they exist in the DataFrame
    if 'Amiodaron_full_dose' in df.columns:
        df = df.drop(['Amiodaron_full_dose', 'Amiodaron_weighted_mean', "Amiodaron_po_check"], axis=1).copy() 

    #---------------------------------------------------------------------------------------------
    
    # Process Troponin features if they exist and the 'Troponin' flag is True
    if all(col in df.columns for col in ['TroponinTMax_inICUStay', 'TroponinIMax_inICUStay', 'TroponinTMin_inICUStay', 'TroponinIMin_inICUStay']):
        if Troponin:
            # Define maximum values and thresholds for Troponin levels
            TroponinT_max = 100
            TroponinT_threshold = 0.013
            TroponinI_max = 100
            TroponinI_threshold = 0.05
            
            # Calculate deviations for Troponin T and I maximum values
            df["TroponinT_deviation_max"] = (df["TroponinTMax_inICUStay"] - TroponinT_threshold) / (TroponinT_max - TroponinT_threshold)
            df["TroponinI_deviation_max"] = (df["TroponinIMax_inICUStay"] - TroponinI_threshold) / (TroponinI_max - TroponinI_threshold)
            
            # Create a combined Troponin value based on deviations
            df['Troponin_value_max'] = np.where(df["TroponinI_deviation_max"].notnull(), df["TroponinI_deviation_max"], df["TroponinT_deviation_max"])
            
            # Calculate deviations for Troponin T and I minimum values
            df["TroponinT_deviation_min"] = (df["TroponinTMin_inICUStay"] - TroponinT_threshold) / (TroponinT_max - TroponinT_threshold)
            df["TroponinI_deviation_min"] = (df["TroponinIMin_inICUStay"] - TroponinI_threshold) / (TroponinI_max - TroponinI_threshold)
            
            # Create a combined Troponin value based on minimum deviations
            df['Troponin_value_min'] = np.where(df["TroponinI_deviation_min"].notnull(), df["TroponinI_deviation_min"], df["TroponinT_deviation_min"])
            
            # Define columns to drop after processing Troponin features
            colomns_to_drop = ['TroponinT_deviation_max', 'TroponinI_deviation_max', 'TroponinT_deviation_min', 'TroponinI_deviation_min', 
                               'TroponinIMax_inICUStay', 'TroponinTMax_inICUStay', 'TroponinIMin_inICUStay', 'TroponinTMin_inICUStay']
            df.drop(columns=colomns_to_drop, inplace=True)
    
    #---------------------------------------------------------------------------------------------
    
    # Apply logarithmic transformation to numerical columns if the 'log' flag is True
    if log: 
        columns_to_transform = ['Propofol_full_dose', 'Epinephrine_full_dose', 'Norepinephrine_full_dose', 'Norepinephrine_weighted_mean',
                                'Dobutamine_full_dose', 'Vasopressin_full_dose', 'Catecholamines', 'Midazolam_full_dose', 
                                'Ketamin_full_dose', 'Sufentanyl_full_dose','PipTaz_full_dose', 'Meropenem_po_check', 
                                'Meropenem_iv_check','Amiodaron_full_dose_sum', 'Digoxin_iv_check', 'Heparin_weighted_mean', 
                                'Heparin_full_dose', 'prednisolon_combi_po_check','prednisolon_combi_iv_check', 'PTTWeightedMean',
                                'INRWeightedMean', 'CreatinineWeightedMean', 'BilirubineWeightedMean','GPTWeightedMean', 
                                'GammaGTWeightedMean', 'LDHMaxOnICUStay', 'LeukocytesWeightedMean', 'DaysTillICUStay_end', 
                                'DaysTillHospitalStay_end', 'CRPWeightedMean', 'CKWeightedMean', "Epinephrine_duration",'Epinephrine_weighted_mean', 'Dobutamine_duration', 'Dobutamine_weighted_mean', 
                                "Vasopressin_duration", 'Vasopressin_weighted_mean', 'Propofol_duration', 'Propofol_weighted_mean', 
                                "Midazolam_duration",'Midazolam_weighted_mean', 'Ketamin_duration', 'Ketamin_weighted_mean', 
                                "Sufentanyl_duration",'Sufentanyl_weighted_mean', 'PipTaz_duration', 'PipTaz_weighted_mean', 
                                "Amiodaron_duration"]
        
        for column in columns_to_transform:
            if column in df.columns:
                df[column] = np.log(df[column] + 0.1)            
    #------------------------------------------------------------------------------------------------
    
    # Scale features if the 'scaling' flag is True
    if scaling:
        
        columns_to_scale = ['Height', 'Weight', 
                            "Epinephrine_duration",'Epinephrine_weighted_mean', 'Dobutamine_duration', 'Dobutamine_weighted_mean', "Vasopressin_duration", 'Vasopressin_weighted_mean', "GOTWeightedMean",
                            'Propofol_duration', 'Propofol_weighted_mean', "Midazolam_duration",'Midazolam_weighted_mean', 'Ketamin_duration', 'Ketamin_weighted_mean', 'LowestpHArt_0_12', 'LowestpHArt_12_24',
                            "Sufentanyl_duration",'Sufentanyl_weighted_mean', 'PipTaz_duration', 'PipTaz_weighted_mean', "Amiodaron_duration", 'WeightedMeanLactate', 'WeightedMeanLactateArt', 'BaseExcessFirstValue', 'BaseExcessDifference',
                            'DayNumber', 'AgeOnInclusion', 'Epinephrine_full_dose', 'Norepinephrine_duration', 'BMI', 'Norepinephrine_weighted_mean','Norepinephrine_full_dose', 'Dobutamine_full_dose', 
                            'Vasopressin_full_dose','Catecholamines', 'Propofol_full_dose', 'Midazolam_full_dose',
                            'Ketamin_full_dose', 'Sufentanyl_full_dose', 'PipTaz_full_dose','Meropenem_po_check', 'Meropenem_iv_check', 'Digoxin_iv_check','Heparin_duration', 'Heparin_weighted_mean', 'Heparin_full_dose',
                            'prednisolon_combi_iv_check', 'prednisolon_combi_po_check', 'HighestLactate_0_12', 'Amiodaron_weighted_mean_sum', 
                            'HighestLactate_12_24', 'HighestLactateArt_0_12', 'HighestLactateArt_12_24', 'LowestpH_0_12', 'LowestpH_12_24', 'PlateletsWeightedMean', 'PTTWeightedMean', 'INRWeightedMean', 
                            'CreatinineWeightedMean', 'GFRWeightedMean', 'BilirubineWeightedMean', 'GPTWeightedMean', 'GammaGTWeightedMean', 'CRPWeightedMean', 'ChlorideFirstValue', 'ChlorideDifference', 
                            'BicarbonateFirstValue', 'BicarbonateDifference', 'SodiumFirstValue', 'SodiumDifference', 'PotassiumFirstValue', 'PotassiumDifference', 'UreaWeightedMean', 'GlucoseFirstValue', 
                            'GlucoseDifference', 'CKWeightedMean', 'ProteinWeightedMean', 'LDHMaxOnICUStay', 'HighestTemperaturFirstValue', 'HighestTemperaturDifference', 
                            'RRSysWeightedMeanValue', 'RRDiaWeightedMeanValue', 'HeartRateWeightedMeanValue', 'sO2WeightedMeanValue', 'HemoglobinWeightedMeanValue', 'LeukocytesWeightedMean',
                            'vent_time', 'Pao2Artweightedmean', 'PaCO2Artweightedmean','LVEFAdmission', 'NumberOfPCIs', 'GCS_Admission','Troponin_value_max', 'Amiodaron_full_dose_sum', 'Troponin_value_min', 
                            'horoviz_WeightedMeanValue', 'RBCUnitsTaken']
        
        vorhandene_Spalten = [column for column in columns_to_scale if column in df.columns]
        
        scale_data = df[vorhandene_Spalten]
        no_scale_data = df.drop(columns=vorhandene_Spalten)        
        
        scaler = PowerTransformer(method='yeo-johnson')
        
        scaled_data = scaler.fit_transform(scale_data)
        
        scaled_data = pd.DataFrame(scaled_data, columns=scale_data.columns)
        
        df = pd.concat([no_scale_data, scaled_data], axis=1)
    
    # Return the cleaned DataFrame
    return df



def add_noise_to_data(df, label, noise_factor=0.1):
    # Create a copy of the input DataFrame to avoid modifying the original data.
    noisy_df = df.copy()
    
    # Iterate through each column in the DataFrame.
    for column in noisy_df.columns:
        # For the label column, add binary noise (0 or 1).
        if column == label:
            noise = np.random.choice([0, 1], size=noisy_df[column].shape)  # Generate random noise.
            noisy_df[column] = noise  # Replace the label column with noisy values.
        # For the 'Outliers' column, also add binary noise.
        elif column == 'Outliers':
            noise = np.random.choice([0, 1], size=noisy_df[column].shape)  # Generate random noise.
            noisy_df[column] = noise  # Replace the 'Outliers' column with noisy values.
        else:
            # For all other columns, add Gaussian noise.
            noise = np.random.normal(loc=0, scale=noise_factor, size=noisy_df[column].shape)  # Generate Gaussian noise.
            noisy_df[column] += noise  # Add the noise to the column.

    # Combine the original DataFrame and the noisy DataFrame, resetting the index.
    combined_df = pd.concat([df, noisy_df], axis=0).reset_index(drop=True)
    
    return combined_df  # Return the combined DataFrame with added noise.


def train_and_detect_outliers(df, label, columns_to_scan, scaler, noise_factor, SA_anzahl, outlier_anzahl, scale_pos_weight):
    # Create a copy of the label column to keep track of it separately.
    df_drop = df[[label]].copy()
    # Drop the label column from the original DataFrame for further processing.
    df = df.drop(columns=[label])

    # Scale the features using the provided scaler.
    df_scaled = scaler.transform(df)

    # Create a new DataFrame with scaled features.
    df_pca = pd.DataFrame(df_scaled, columns=df.columns.tolist())

    # Set the index of the dropped label DataFrame to match the scaled DataFrame.
    df_drop = df_drop.set_index(df_pca.index)
    # Concatenate the scaled DataFrame and the dropped label DataFrame.
    df_pca = pd.concat([df_pca, df_drop], axis=1)

    # Initialize a DataFrame to hold outlier flags.
    outliers = pd.DataFrame(index=df.index)
    
    # Iterate through each column specified for outlier detection.
    for column in columns_to_scan:
        # Add noise to the data for outlier detection.
        noisy_df = add_noise_to_data(df_pca, label, noise_factor=noise_factor)
        noisy_df = add_noise_to_data(noisy_df, label, noise_factor=noise_factor)

        # Separate features (X) and the current column to predict (y).
        X_train = noisy_df.drop(columns=[column])
        y_train = noisy_df[column]
        
        # Initialize and train the XGBoost regressor model.
        model = xgb.XGBRegressor(objective='reg:squarederror', n_jobs=-1, random_state=42, scale_pos_weight=scale_pos_weight)
        model.fit(X_train, y_train)
        
        # Make predictions on the original scaled DataFrame excluding the current column.
        y_pred = model.predict(df_pca.drop(columns=[column]))
        
        # Calculate the errors between the actual and predicted values.
        errors = df_pca[column] - y_pred
        # Identify outliers based on the specified threshold.
        outliers[column] = (errors.abs() > SA_anzahl * errors.std()).astype(int)
    
    # Count the total number of outliers detected for each row.
    outliers['outlier_count'] = outliers.sum(axis=1)
    # Determine if a row is an outlier based on the specified threshold.
    outliers['is_outlier'] = (outliers['outlier_count'] >= outlier_anzahl).astype(int)

    return outliers, df_pca  # Return the DataFrame of outliers and the PCA DataFrame.

def weighted_youden_threshold(y_true, pred_proba, weight_specificity=0.5):
    """
    Calculates the best threshold based on a weighted Youden's J-statistic.
    
    Args:
        y_true (np.array): Actual class labels (0 or 1).
        pred_proba (np.array): Predicted probabilities for the positive class.
        weight_specificity (float): Weight for specificity to penalize false positives.
        
    Returns:
        float: The threshold that maximizes the weighted Youden index.
    """
    
    # Generate values for thresholds
    thresholds = np.arange(0.0, 1.0, 0.01)  # Creates an array of thresholds from 0.0 to 1.0 in steps of 0.01
    best_threshold = 0.5  # Initialize the best threshold
    best_weighted_youden = -np.inf  # Set the initial best weighted Youden index to negative infinity
    
    # Iterate over all possible thresholds
    for threshold in thresholds:
        # Predictions based on the current threshold
        preds = (pred_proba >= threshold).astype(int)  # Convert probabilities to binary predictions (0 or 1)
        
        # Compute confusion matrix
        tn, fp, fn, tp = confusion_matrix(y_true, preds).ravel()  # Count true negatives, false positives, false negatives, and true positives
        
        # Calculate sensitivity (Recall) and specificity
        sensitivity = tp / (tp + fn) if (tp + fn) > 0 else 0  # Sensitivity is the proportion of correctly identified positive cases
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0  # Specificity is the proportion of correctly identified negative cases
        
        # Calculate the weighted Youden statistic
        weighted_youden = sensitivity + weight_specificity * specificity - 1  # Computes the weighted Youden index
        
        # Update the best threshold if the weighted Youden index is higher
        if weighted_youden > best_weighted_youden:
            best_weighted_youden = weighted_youden  # Update the best weighted Youden index
            best_threshold = threshold  # Update the best threshold
    
    return best_threshold  # Return the optimal threshold

def youden_threshold(y_true, pred_proba):
    """
    Calculates the best threshold based on Youden's J-statistic.
    
    Args:
        y_true (np.array): Actual class labels (0 or 1).
        pred_proba (np.array): Predicted probabilities for the positive class.
        
    Returns:
        float: The threshold that maximizes Youden's J statistic.
    """
    
    # Generate values for thresholds
    thresholds = np.arange(0.0, 1.0, 0.01)  # Creates an array of thresholds from 0.0 to 1.0 in steps of 0.01
    best_threshold = 0.5  # Initialize the best threshold
    best_youden = -np.inf  # Set the initial best Youden index to negative infinity
    
    # Iterate over all possible thresholds
    for threshold in thresholds:
        # Predictions based on the current threshold
        preds = (pred_proba >= threshold).astype(int)  # Convert probabilities to binary predictions (0 or 1)
        
        # Compute confusion matrix
        tn, fp, fn, tp = confusion_matrix(y_true, preds).ravel()  # Count true negatives, false positives, false negatives, and true positives
        
        # Calculate sensitivity (Recall) and specificity
        sensitivity = tp / (tp + fn) if (tp + fn) > 0 else 0  # Sensitivity is the proportion of correctly identified positive cases
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0  # Specificity is the proportion of correctly identified negative cases
        
        # Calculate Youden's J statistic
        youden = sensitivity + specificity - 1  # Computes Youden's index
        
        # Update the best threshold if Youden's index is higher
        if youden > best_youden:
            best_youden = youden  # Update the best Youden index
            best_threshold = threshold  # Update the best threshold
    
    return best_threshold  # Return the optimal threshold

def weighted_youden_threshold(y_true, pred_proba, beta=1.5):
    """
    Calculates the best threshold based on a weighted Youden's J-statistic.
    This version prioritizes specificity (reducing false positives).
    
    Args:
        y_true (np.array): Actual class labels (0 or 1).
        pred_proba (np.array): Predicted probabilities for the positive class.
        beta (float): Weight factor for specificity. Higher values favor fewer false positives.
        
    Returns:
        float: The threshold that maximizes the weighted Youden's J statistic.
    """
    
    thresholds = np.arange(0.0, 1.0, 0.01)  # Generate thresholds from 0.0 to 1.0 in steps of 0.01
    best_threshold = 0.5  # Default threshold
    best_youden = -np.inf  # Start with the lowest possible Youden index
    
    for threshold in thresholds:
        preds = (pred_proba >= threshold).astype(int)  # Convert probabilities to binary predictions
        
        # Compute confusion matrix
        tn, fp, fn, tp = confusion_matrix(y_true, preds).ravel()
        
        # Compute sensitivity (recall) and specificity
        sensitivity = tp / (tp + fn) if (tp + fn) > 0 else 0
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0
        
        # Weighted Youden’s J Statistic
        weighted_youden = (beta * specificity) + sensitivity - 1
        
        # Update best threshold if we find a better weighted Youden index
        if weighted_youden > best_youden:
            best_youden = weighted_youden
            best_threshold = threshold
    
    return best_threshold

def prepare_datasets(days_until_death_threshold, df_tr, df_ext):
    """
    Prepares training and external validation datasets for machine learning.

    Args:
        days_until_death_threshold (int): The number of days to determine if a patient will die (True if dies within this threshold).
        df_AG_Schock (DataFrame): DataFrame containing AG Schock data.
        df_mimic (DataFrame): DataFrame containing MIMIC data.
        df_hirid (DataFrame): DataFrame containing HIRID data.
        df_eicu (DataFrame): DataFrame containing EICU data.
        df_sicdb (DataFrame): DataFrame containing SICDB data.
        df_amsterdam (DataFrame): DataFrame containing Amsterdam data.

    Returns:
        df_train (DataFrame): The prepared training dataset.
        df_ext_val (DataFrame): The prepared external validation dataset.
    """

    # List of additional columns to keep
    additional_columns = [
        'AgeOnInclusion', 'RRSysWeightedMeanValue', 
        'RRDiaWeightedMeanValue', 'sO2WeightedMeanValue', 'PHWeightedMean',
        'LactateWeightedMean', 'gluWeightedMean', 'HCO3WeightedMean',
        'CreatinineWeightedMean', 'DayNumber', 'PO2WeightedMean',
        'PCO2WeightedMean', 'HBWeightedMean', 'ureaWeightedMean',
        'HRWeightedMean', 'TempWeightedMean', 'NaWeightedMean', 'KWeightedMean',
        'ClWeightedMean', 'Height', 'Weight', 'plateletsWeightedMean',
        'HighestLactate', 'LowestpH', 'leucoWeightedMean'
    ]
    
    # Concatenate the training datasets from multiple sources
    df_train = df_tr.copy()
    
    # Create a new column 'Label' based on the threshold
    df_train['Label'] = df_train['DaystillDeath'] <= days_until_death_threshold+1
    
    # Filter columns to include only relevant ones for training
    df_train = df_train[['Label'] + additional_columns]
    
    # Reset the index of the training dataset
    df_train = df_train.reset_index(drop=True)

    # Concatenate the external validation datasets from multiple sources
    df_ext_val = df_ext.copy()
    
    # Create a new column 'Label' based on the threshold for external validation set
    df_ext_val['Label'] = df_ext_val['DaystillDeath'] <= days_until_death_threshold+1
    
    # Filter columns to include only relevant ones for validation
    df_ext_val = df_ext_val[['Label'] + additional_columns]
    
    # Reset the index of the external validation dataset
    df_ext_val = df_ext_val.reset_index(drop=True)

    # Set a threshold for null values; rows with more than this threshold of missing values will be dropped
    threshold = 0.0001
    # Filter the training dataset to remove rows with null values exceeding the threshold
    df_train = df_train[df_train.isnull().mean(axis=1) < threshold]
    # Filter the external validation dataset similarly
    df_ext_val = df_ext_val[df_ext_val.isnull().mean(axis=1) < threshold]

    # Return the prepared training and external validation datasets
    return df_train, df_ext_val

def train_scaler_s_model(df_train, label, columns_to_scan, noise_factor, SA_number, scale_pos_wight_outlier_fun):
    """
    Trains a scaler and a machine learning model using the provided training DataFrame.
    
    Args:
        df_train (DataFrame): The training dataset.
        label (str): The target label/column name for classification tasks.
        columns_to_scan (list): List of columns to analyze for outliers.
        noise_factor (float): The amount of noise to add during outlier detection.
        SA_number (int): The threshold for determining significant anomalies.
        scale_pos_wight_outlier_fun (float): The scaling factor for positive weights in outlier detection.

    Returns:
        scaler (PowerTransformer): The fitted scaler object for transforming data.
        model (XGBClassifier): The trained XGBoost classifier model.
        df_train_clean (DataFrame): The cleaned training dataset without outliers.
        df_train_outliers (DataFrame): The DataFrame containing outlier information.
    """

    # Create a copy of the training dataset to avoid modifying the original
    df = df_train.copy()
    
    # Separate the target variable and the feature variables
    df_drop = df[[label]].copy()
    df = df.drop(columns=[label])

    # Initialize the PowerTransformer to scale features using the Yeo-Johnson method
    scaler = PowerTransformer(method='yeo-johnson')
    
    # Fit and transform the feature data
    df_scaled = scaler.fit_transform(df)

    # Copy the training dataset again for outlier detection
    df = df_train.copy()
    df = df.reset_index(drop=True)

    # Call the function to train the model and detect outliers
    outliers, df_pca = train_and_detect_outliers(
        df,
        label,
        columns_to_scan,
        scaler,
        outlier_anzahl=1,
        noise_factor=noise_factor,
        SA_anzahl=SA_number,
        scale_pos_weight=scale_pos_wight_outlier_fun,
    )

    # Create a clean dataset without detected outliers
    df_train_clean = df[outliers['is_outlier'] == 0]
    df_train_outliers = outliers

    # Add the outlier information to the PCA DataFrame
    df_pca['Outliers'] = outliers['is_outlier']

    # Prepare the features (X) and target (y) for model training
    X = df_pca.drop(label, axis=1)
    y = df_pca[label]
    
    # Apply SMOTE to handle class imbalance in the training data
    smote = BorderlineSMOTE(sampling_strategy='auto')
    X, y = smote.fit_resample(X, y)
    
    # Concatenate the resampled features and target into a single DataFrame
    df_pca = pd.concat([X, y], axis=1)

    # Drop the target column from the feature set for further processing
    df_pca = df_pca.drop(label, axis=1)

    # Define the target variable as the 'Outliers' column
    y = df_pca['Outliers']
    X = df_pca.drop('Outliers', axis=1)

    # Split the dataset into training and evaluation sets
    X_train, X_eval, y_train, y_eval = train_test_split(X, y, test_size=0.15, stratify=y)

    # Initialize and train the XGBoost classifier
    model = xgb.XGBClassifier(early_stopping_rounds=10, device="cuda", eval_metric="aucpr")
    
    # Fit the model on the training data and evaluate on the evaluation set
    model.fit(X_train, y_train, eval_set=[(X_eval, y_eval)], verbose=False)

    # Return the fitted scaler, trained model, cleaned training dataset, and outlier information
    return scaler, model, df_train_clean, df_train_outliers

def train_prediction_model(df_train, scaler, s_model, label, scale_pos_weight, n_trials, noise_factor_1=5, noise_factor_2=0.5, test_size=0.15, should_print=True):
    """
    Trains a prediction model using hyperparameter optimization and calculates the optimal threshold for classification.

    Args:
        df_train (DataFrame): The training dataset containing features and the target label.
        scaler (PowerTransformer): The fitted scaler used for transforming feature data.
        s_model (XGBClassifier): The previously trained outlier detection model.
        label (str): The name of the target variable for classification.
        scale_pos_weight (float): The weight for positive samples during training.
        n_trials (int): The number of trials for hyperparameter optimization.
        noise_factor_1 (float): The level of noise added to the training data during augmentation (default: 5).
        noise_factor_2 (float): The level of noise added to the training data during augmentation (default: 0.5).
        test_size (float): The proportion of the dataset to include in the test split (default: 0.15).

    Returns:
        final_model (XGBClassifier): The trained XGBoost classifier model.
        optimal_threshold (float): The optimal threshold for classifying predictions.
    """

    # Create a copy of the training dataset to avoid modifying the original
    df = df_train.copy()

    # Add noise to the training data to augment it for model training
    noisy_df = add_noise_to_data(df, label, noise_factor=noise_factor_1)
    noisy_df = add_noise_to_data(noisy_df, label, noise_factor=noise_factor_2)

    # Prepare features (X) and target (y) from the noisy data
    X = noisy_df.drop(label, axis=1)
    y = noisy_df[label]

    def objective(trial, X=X, y=y):
        """
        Objective function for Optuna's hyperparameter optimization.

        Args:
            trial (optuna.Trial): A single trial in the optimization process.
            X (DataFrame): Features for training.
            y (Series): Target labels.

        Returns:
            float: The ROC AUC score for the given hyperparameter configuration.
        """
        
        # Split the data into training and evaluation sets
        X_train, X_eval, y_train, y_eval = train_test_split(X, y, test_size=test_size, stratify=y)

        # Define hyperparameters to optimize
        param = {
            'objective': 'binary:logistic',
            'lambda': trial.suggest_float('lambda', 0.0001, 10.0),
            'alpha': trial.suggest_float('alpha', 0.0001, 10.0),
            'eta': trial.suggest_float('eta', 0.001, 0.1),  # Logarithmic distribution
            'learning_rate': trial.suggest_float('learning_rate', 0.008, 0.3),
            'n_estimators': trial.suggest_int('n_estimators', 50, 1000),
            'max_depth': trial.suggest_int('max_depth', 3, 20),
            'random_state': trial.suggest_categorical('random_state', [2020]),
            'min_child_weight': trial.suggest_int('min_child_weight', 1, 30),
            'gamma': trial.suggest_int('gamma', 0.009, 10),
            'scale_pos_weight': trial.suggest_int('scale_pos_weight', scale_pos_weight, scale_pos_weight),
            'max_delta_step': trial.suggest_int('max_delta_step', 1, 1)
        }
        
        # Initialize a pruning callback for early stopping based on the validation set's AUC score
        pruning_callback = XGBoostPruningCallback(trial, "validation_0-aucpr")
        
        # Create the XGBoost classifier with the current hyperparameters
        model = xgb.XGBClassifier(**param, early_stopping_rounds=10, device="cuda", eval_metric="aucpr", callbacks=[pruning_callback])
        
        # Fit the model on the training data
        model.fit(X_train, y_train, eval_set=[(X_eval, y_eval)], verbose=False)
        
        # Predict probabilities for the evaluation set
        pred_proba = model.predict_proba(X_eval)[:, 1]
        
        # Calculate the ROC AUC score as the evaluation metric
        score = roc_auc_score(y_eval, pred_proba)
        
        return score

    # Initialize Optuna's study for hyperparameter optimization
    direction = 'maximize'  # The goal is to maximize the score

    study = optuna.create_study(direction=direction)

    # Optimize the objective function over a specified number of trials
    study.optimize(objective, n_trials=n_trials, show_progress_bar=False)
    
    if should_print:
        print('Number of finished trials:', len(study.trials))
        print('Best trial:', study.best_trial.params)

    # Retrieve the best hyperparameters from the study
    Best_trial = study.best_trial.params

    # Combine features and target into a single DataFrame for resampling
    df = pd.concat([X, y], axis=1)

    # Shuffle the DataFrame to ensure randomness in training
    df = df.sample(frac=1)

    # Prepare final training set
    X1 = df.drop(label, axis=1)
    y1 = df[label]

    # Train the final model using the best hyperparameters
    final_model = xgb.XGBClassifier(**Best_trial, eval_metric="aucpr", device="cuda")
    final_model.fit(X1, y1, verbose=True)

    # Calculate optimal threshold based on the original training data
    df = df_train.copy()

    X = df.drop(columns=[label])
    df_scaled = scaler.transform(X)

    # Create a DataFrame for the scaled features
    df_scaled = pd.DataFrame(df_scaled, columns=X.columns.tolist(), index=X.index)

    # Predict outliers using the outlier detection model
    outliers = s_model.predict(df_scaled)
    df_scaled['outliers'] = outliers

    # Create DataFrames for clean data and outliers
    df_train_ausreisser = df[df_scaled['outliers'] == 0]
    df_train_ausreisser_echt = df[df_scaled['outliers'] == 1]

    # Use the cleaned training data for final evaluation
    df = df_train_ausreisser.copy()

    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities on the cleaned test set
    pred_proba = final_model.predict_proba(X_test)[:, 1]

    # Calculate the optimal threshold for binary classification
    optimal_threshold = youden_threshold(y_test, pred_proba)

    # Make predictions based on the optimal threshold
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate and print confusion matrix metrics
    tn, fp, fn, tp = confusion_matrix(y_test, preds).ravel()

    sensitivity = tp / (tp + fn)

    specificity = tn / (tn + fp)
    
    if should_print:
        # Print the optimal threshold
        print(f"Optimaler Schwellenwert: {optimal_threshold}")
        print(f"Sensitivität (True Positive Rate): {sensitivity:.4f}")
        print(f"Spezifität (True Negative Rate): {specificity:.4f}")

    return final_model, optimal_threshold

def eval_model(df_test, label, scaler, model, final_model, optimal_threshold, plots=False):
    """
    Evaluates the performance of the trained model on a test dataset and generates relevant metrics and plots.

    Args:
        df_test (DataFrame): The test dataset containing features and the target label.
        label (str): The name of the target variable for classification.
        scaler (PowerTransformer): The fitted scaler used for transforming feature data.
        model (XGBClassifier): The trained outlier detection model used for identifying outliers in the dataset.
        final_model (XGBClassifier): The final trained prediction model used for classification.
        optimal_threshold (float): The optimal threshold for classifying predictions.
        plots (bool): Flag to determine whether to plot evaluation results (default: False).

    Returns:
        None
    """

    # Create a copy of the test dataset to avoid modifying the original
    df = df_test.copy()

    # Reset the index of the DataFrame
    df = df.reset_index(drop=True)
    
    # Prepare the feature set for PCA and scaling
    df_pca = df.drop(columns=[label])
    df_scaled = scaler.transform(df_pca)

    # Create a DataFrame from the scaled features
    df_pca = pd.DataFrame(df_scaled, columns=df_pca.columns.tolist())

    # Predict outliers using the outlier detection model
    outliers = model.predict(df_pca)
    df_pca['outliers'] = outliers

    # Separate the clean data and outliers based on the predictions
    df_clean = df[df_pca['outliers'] == 0]
    df_outlier = df[df_pca['outliers'] == 1]
    
    print(df_clean.shape[0])
    print(df_outlier.shape[0])
    
    # Prepare the full test set for evaluation
    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities using the final trained model
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate various performance metrics for the whole dataset
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    roc_auc = roc_auc_score(y_test, pred_proba)
    aucpr_w = average_precision_score(y_test, pred_proba)
    
    # Precision-recall curve data
    precision_w, recall_w, thresholds = precision_recall_curve(y_test, pred_proba)

    # Confusion matrix for the whole dataset
    conf_matrix_w = confusion_matrix(y_test, preds)
    tn, fp, fn, tp = conf_matrix_w.ravel()
    FpFn = fp + fn

    # ROC curve data for the whole dataset
    fpr_w, tpr_w, thresholds = roc_curve(y_test, pred_proba)
    auc_score_w = auc(fpr_w, tpr_w)
    
    # Display results for the whole dataset
    print('#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#')
    print('Results on the whole dataset:')
    print('accuracy:', accuracy)
    print('precision:', precision)
    print('recall:', recall)
    print('f1:', f1)
    print('roc_auc:', roc_auc)
    print('aucpr:', aucpr_w)
    print('Fp+Fn:', FpFn)
    print('auc_score:', auc_score_w)
    
    # Evaluate the model on the cleaned dataset (without outliers)
    df = df_clean.copy()

    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities and classify based on the optimal threshold
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate metrics for the cleaned dataset
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    roc_auc = roc_auc_score(y_test, pred_proba)
    aucpr_g = average_precision_score(y_test, pred_proba)
    
    # Precision-recall curve data for the cleaned dataset
    precision_g, recall_g, thresholds = precision_recall_curve(y_test, pred_proba)

    # Confusion matrix for the cleaned dataset
    conf_matrix_g = confusion_matrix(y_test, preds)
    tn, fp, fn, tp = conf_matrix_g.ravel()
    FpFn = fp + fn

    # ROC curve data for the cleaned dataset
    fpr_g, tpr_g, thresholds = roc_curve(y_test, pred_proba)
    auc_score_g = auc(fpr_g, tpr_g)
    
    # Display results for the cleaned dataset
    print('#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#')
    print('Results on the good predictable cohort:')
    print('accuracy:', accuracy)
    print('precision:', precision)
    print('recall:', recall)
    print('f1:', f1)
    print('roc_auc:', roc_auc)
    print('aucpr:', aucpr_g)
    print('Fp+Fn:', FpFn)
    print('auc_score:', auc_score_g)
        
    # Evaluate the model on the outlier dataset
    df = df_outlier.copy()

    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities and classify based on the optimal threshold
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate metrics for the outlier dataset
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    roc_auc = roc_auc_score(y_test, pred_proba)
    aucpr_b = average_precision_score(y_test, pred_proba)
    
    # Precision-recall curve data for the outlier dataset
    precision_b, recall_b, thresholds = precision_recall_curve(y_test, pred_proba)
    
    # Confusion matrix for the outlier dataset
    conf_matrix_b = confusion_matrix(y_test, preds)
    tn, fp, fn, tp = conf_matrix_b.ravel()
    FpFn = fp + fn

    # ROC curve data for the outlier dataset
    fpr_b, tpr_b, thresholds = roc_curve(y_test, pred_proba)
    auc_score_b = auc(fpr_b, tpr_b)
    
    # Display results for the outlier dataset
    print('#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#')
    print('Results on the bad predictable cohort:')
    print('accuracy:', accuracy)
    print('precision:', precision)
    print('recall:', recall)
    print('f1:', f1)
    print('roc_auc:', roc_auc)
    print('aucpr:', aucpr_b)
    print('Fp+Fn:', FpFn)
    print('auc_score:', auc_score_b)
    
    # Plot results if the plots parameter is set to True
    if plots:
        # Create subplots for ROC and Precision-Recall curves
        fig, axs = plt.subplots(2, 2, figsize=(15, 9))  # 2x2 grid of subplots

        # Plot ROC curves in the first subplot
        axs[0, 0].plot(fpr_w, tpr_w, label='Whole Dataset (AUC = {:.2f})'.format(auc_score_w), lw=2, color='darkorange')
        axs[0, 0].plot(fpr_g, tpr_g, label='High confidence subgroup (AUC = {:.2f})'.format(auc_score_g), lw=2, color='green')
        axs[0, 0].plot(fpr_b, tpr_b, label='Low confidence subgroup (AUC = {:.2f})'.format(auc_score_b), lw=2, color='red')
        axs[0, 0].plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')  # Reference line
        axs[0, 0].set_xlabel('False Positive Rate')
        axs[0, 0].set_ylabel('True Positive Rate')
        axs[0, 0].set_title('ROC Curves')
        axs[0, 0].legend(loc="lower right")
        axs[0, 0].grid(True)

        # Plot Precision-Recall curves in the second subplot
        axs[0, 1].plot(recall_w, precision_w, label='Whole Dataset (AP = {:.2f})'.format(aucpr_w), lw=2, color='darkorange')
        axs[0, 1].plot(recall_g, precision_g, label='High confidence subgroup (AP = {:.2f})'.format(aucpr_g), lw=2, color='green')
        axs[0, 1].plot(recall_b, precision_b, label='Low confidence subgroup (AP = {:.2f})'.format(aucpr_b), lw=2, color='red')
        axs[0, 1].set_xlabel('Recall')
        axs[0, 1].set_ylabel('Precision')
        axs[0, 1].set_title('Precision-Recall Curves')
        axs[0, 1].legend(loc="lower left")
        axs[0, 1].grid(True)

        # Data for confusion matrices
        datasets = ['Whole dataset', 'High confidence subgroup', 'Low confidence subgroup']
        conf_matrices = [conf_matrix_w, conf_matrix_g, conf_matrix_b]

        # Extract counts for correct and incorrect classifications
        correct_alive = [conf_matrix[0, 0] for conf_matrix in conf_matrices]
        incorrect_alive = [conf_matrix[0, 1] for conf_matrix in conf_matrices]
        correct_dead = [conf_matrix[1, 1] for conf_matrix in conf_matrices]
        incorrect_dead = [conf_matrix[1, 0] for conf_matrix in conf_matrices]
        
        # Bar plot for correct and incorrect counts in the third subplot
        x = np.arange(len(datasets))  # The label locations
        width = 0.2  # The width of the bars

        bar1 = axs[1, 0].bar(x - width, correct_alive, width, label='Correct Alive', color='green')
        bar2 = axs[1, 0].bar(x, incorrect_alive, width, label='Incorrect Alive', color='lightgreen')
        bar3 = axs[1, 0].bar(x + width, correct_dead, width, label='Correct Dead', color='red')
        bar4 = axs[1, 0].bar(x + 2 * width, incorrect_dead, width, label='Incorrect Dead', color='lightcoral')

        axs[1, 0].set_xlabel('Datasets')
        axs[1, 0].set_ylabel('Count')
        axs[1, 0].set_title('Correct and Incorrect Classifications by Dataset and Class')
        axs[1, 0].set_xticks(x)
        axs[1, 0].set_xticklabels(datasets)
        axs[1, 0].legend()

        # Add labels to the bars for clarity
        def autolabel(bars, ax):
            """Attach a text label above each bar in *bars*, displaying its height."""
            for bar in bars:
                height = bar.get_height()
                ax.annotate('{}'.format(height),
                            xy=(bar.get_x() + bar.get_width() / 2, height),
                            xytext=(0, 3),  # 3 points vertical offset
                            textcoords="offset points",
                            ha='center', va='bottom')

        # Apply autolabel function to each set of bars
        autolabel(bar1, axs[1, 0])
        autolabel(bar2, axs[1, 0])
        autolabel(bar3, axs[1, 0])
        autolabel(bar4, axs[1, 0])

        # Retrieve feature importance from the final model
        booster = final_model.get_booster()
        importance = booster.get_score(importance_type='weight')
        
        # Sort and prepare feature importance data
        sorted_importance = sorted(importance.items(), key=lambda x: x[1], reverse=True)[:50]
        
        feature_names = [x[0] for x in sorted_importance]
        feature_importance = [x[1] for x in sorted_importance]
        
        feature_mapping = {
            "plateletsWeightedMean": "Platelets*",
            "AgeOnInclusion": "Age on Inclusion",
            "Catecholamines": "Catecholamines",
            "HighestLactate_12_24": "Highest Lactate from 12h to 24h",
            "LDHWeightedMean": "LDH*",
            "RRSysWeightedMeanValue": "Blood Pressure (Systolic)*",
            "vent_time": "Ventilation Time",
            "sO2WeightedMeanValue": "Oxygen Saturation*",
            "BilirubineWeightedMean": "Bilirubin*",
            "LowestpH_12_24": "Lowest pH from 12h to 24h",
            "TempWeightedMean": "Temperature*",
            "RRDiaWeightedMeanValue": "Blood Pressure (Diastolic)*",
            "horoviz_WeightedMeanValue": "Horovitz Index*",
            "HRWeightedMean": "Heart Rate*",
            "Troponin_value_max": "Max Troponin Value",
            "ClWeightedMean": "Chloride*",
            "BMI": "Body Mass Index",
            "PHWeightedMean": "pH*",
            "KWeightedMean": "Potassium*",
            "ureaWeightedMean": "Urea*",
            "PCO2WeightedMean": "PCO2*",
            "LactateWeightedMean": "Lactate*",
            "CreatinineWeightedMean": "Creatinine*",
            "LVEFAdmission": "LVEF on Admission",
            "NaWeightedMean": "Sodium*",
            "gluWeightedMean": "Glucose*",
            "leucoWeightedMean": "Leukocytes*",
            "RBCUnitsTaken": "RBC Units Taken",
            "PO2WeightedMean": "PO2*",
            "Troponin_value_min": "Min Troponin Value",
            "Sufentanyl_weighted_mean": "Sufentanyl*",
            "HighestLactate_0_12": "Highest Lactate from 0h to 12h",
            "BEWeightedMean1": "Base Excess*",
            "Endotrachealtube": "Endotracheal Tube",
            "NumberOfPCIs": "Number of PCIs",
            "OH_TotalEpinephrine_factors_1_4.9": "Total Epinephrine on OHCA",
            "Weight": "Weight",
            "Smoking_ex_smoker": "Smoking (Ex-Smoker)",
            "Sufentanyl_duration": "Sufentanyl Duration",
            "Heparin_weighted_mean": "Heparin*",
            "LowestpH_0_12": "Lowest pH from 0h to 12h",
            "HBWeightedMean": "Hemoglobin*",
            "LowestpH": "Lowest pH",
            "TidalVolweightedmean_factors_not_applicable": "Tidal Volume*",
            "Height": "Height",
            "Diabetes_binaer": "Diabetes",
            "HCO3WeightedMean": "HCO3*",
            "Pinspweightedmean_factors_20_24,9": "P inspiratory*",
            "DayNumber": "Day Number",
            "CAD": "Coronary Artery Disease",
            # Hier weitere Features ergänzen...
        }


        feature_names = [feature_mapping.get(name, name) for name in feature_names]
        
        # Feature importance plot in the fourth subplot
        axs[1, 1].barh(feature_names, feature_importance)
        axs[1, 1].set_xlabel('Feature Importance')
        axs[1, 1].set_ylabel('Feature')
        axs[1, 1].set_title('Top 25 Features Importance')
        axs[1, 1].invert_yaxis()  # Invert y-axis to show the most important feature at the top

        # Adjust layout to prevent overlap
        plt.tight_layout()
        plt.show()


#--------------------------------------------------


def prepare_datasets_simple(df_ext, df_AG_Schock, df_mimic, df_hirid, df_eicu, df_sicdb, df_amsterdam):
    """
    Prepares training and external validation datasets for machine learning.

    Args:
        df_AG_Schock (DataFrame): DataFrame containing AG Schock data.
        df_mimic (DataFrame): DataFrame containing MIMIC data.
        df_hirid (DataFrame): DataFrame containing HIRID data.
        df_eicu (DataFrame): DataFrame containing EICU data.
        df_sicdb (DataFrame): DataFrame containing SICDB data.
        df_amsterdam (DataFrame): DataFrame containing Amsterdam data.

    Returns:
        df_train (DataFrame): The prepared training dataset.
        df_ext_val (DataFrame): The prepared external validation dataset.
    """

    # List of additional columns to keep
    additional_columns = [
        'AgeOnInclusion', 'RRSysWeightedMeanValue',
        'RRDiaWeightedMeanValue', 'sO2WeightedMeanValue', 'PHWeightedMean',
        'LactateWeightedMean', 'gluWeightedMean', 'HCO3WeightedMean',
        'CreatinineWeightedMean', 'DayNumber', 'PO2WeightedMean',
        'PCO2WeightedMean', 'HBWeightedMean', 'ureaWeightedMean',
        'HRWeightedMean', 'TempWeightedMean', 'NaWeightedMean', 'KWeightedMean',
        'ClWeightedMean', 'Height', 'Weight', 'plateletsWeightedMean',
        'HighestLactate', 'LowestpH', 'leucoWeightedMean'
    ]
    
    # Concatenate the training datasets from multiple sources
    df_train = pd.concat([df_AG_Schock, df_mimic, df_hirid], axis=0)#

    df_train = df_train.reset_index(drop=True)

    patientid_list_train = df_train['ID'].tolist()
    
    # Filter columns to include only relevant ones for training
    df_train = df_train[['DaystillDeath'] + additional_columns]
    
    # Reset the index of the training dataset

    # Concatenate the external validation datasets from multiple sources
    #df_ext_val = pd.concat([df_eicu, df_sicdb, df_amsterdam], axis=0)
    df_ext_val = df_ext.copy()

    df_ext_val = df_ext_val.reset_index(drop=True)

    patientid_list_ext_val = df_ext_val['ID'].tolist()
    
    # Filter columns to include only relevant ones for validation
    df_ext_val = df_ext_val[['DaystillDeath'] + additional_columns]
    
    # Reset the index of the external validation dataset

    # Set a threshold for null values; rows with more than this threshold of missing values will be dropped
    threshold = 0.0001
    
    # Remove the 'DaystillDeath' column temporarily from df_train for NaN checking
    df_train_no_dtd = df_train.drop(columns=['DaystillDeath'])
    # Filter the training dataset to remove rows with null values exceeding the threshold, excluding 'DaystillDeath'
    df_train = df_train[df_train_no_dtd.isnull().mean(axis=1) < threshold]
    
    # Remove the 'DaystillDeath' column temporarily from df_ext_val for NaN checking
    df_ext_val_no_dtd = df_ext_val.drop(columns=['DaystillDeath'])
    # Filter the external validation dataset similarly
    df_ext_val = df_ext_val[df_ext_val_no_dtd.isnull().mean(axis=1) < threshold]

    # Return the prepared training and external validation datasets
    return df_train, df_ext_val, patientid_list_train, patientid_list_ext_val

def prepare_hourly_datasets(hours_until_death_threshold, df, additional_columns):
    """
    Prepares datasets for hourly mortality prediction.

    Args:
        hours_until_death_threshold (int): The number of hours to determine if a patient will die (True if dies within this threshold).
        df (DataFrame): DataFrame containing patient data with a 'DaystillDeath' column.
        additional_columns (list): List of additional columns to keep for training.

    Returns:
        df_prepared (DataFrame): The prepared dataset with the target label for hourly prediction.
    """
    # Convert days to hours and create 'Label'
    df['Label'] = df['DaystillDeath'] * 24 <= hours_until_death_threshold+24
    
    # Filter columns to include only relevant ones for training
    df_prepared = df[['Label'] + additional_columns]
    
    # Reset index and return the prepared dataset
    df_prepared = df_prepared.reset_index(drop=True)
    
    return df_prepared

def train_and_evaluate_for_hours(hours_list, df_ext, 
                    s_model_24h, p_model_24h, scaler_24h,
                    s_model_2d, p_model_2d, scaler_2d,
                    s_model_3d, p_model_3d, scaler_3d,
                    s_model_4d, p_model_4d, scaler_4d,
                    s_model_5d, p_model_5d, scaler_5d,
                    s_model_6d, p_model_6d, scaler_6d,
                    s_model_7d, p_model_7d, scaler_7d,
                    s_model_8d, p_model_8d, scaler_8d,
                    s_model_9d, p_model_9d, scaler_9d,
                    s_model_10d, p_model_10d, scaler_10d,
                    s_model_11d, p_model_11d, scaler_11d,
                    s_model_12d, p_model_12d, scaler_12d,
                    s_model_13d, p_model_13d, scaler_13d,
                    s_model_14d, p_model_14d, scaler_14d,
                    s_model_15d, p_model_15d, scaler_15d,
                    s_model_3w, p_model_3w, scaler_3w,
                    optimal_thresholds,
                    df_AG_Schock, df_mimic, df_hirid, df_eicu, df_sicdb, df_amsterdam, label_col='Label', optuna_trails=5):
    """
    Trains models and evaluates them for multiple hourly thresholds, with gradually decreasing
    scale_pos_wight_outlier_fun and scale_pos_weight. Also tracks and plots class imbalance over time.

    Args:
        hours_list (list): List of hours thresholds to predict mortality.
        label_col (str): The label column name.
        optuna_trails (int): Number of Optuna trials for model optimization.
    
    Returns:
        auc_values (dict): AUC values for whole, good, and bad cohorts.
        prauc_values (dict): PR AUC values for whole, good, and bad cohorts.
        imbalance_ratios (list): List of imbalance ratios (positives/negatives) for each hour.
    """
    auc_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    prauc_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    sen_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    spez_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    ppv_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    npv_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    fn_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    fp_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    tn_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    tp_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    pra_values = {'Whole dataset': [], 'High confidence subgroup': [], 'Low confidence subgroup': []}
    imbalance_ratios = []  # Store imbalance ratios for each hour
    subgroup_dict = {}

    df_train, df_ext_val, patientid_list_train, patientid_list_ext_val = prepare_datasets_simple(df_ext, df_AG_Schock=df_AG_Schock, df_mimic=df_mimic, df_hirid=df_hirid, df_eicu=df_eicu, df_sicdb=df_sicdb, df_amsterdam=df_amsterdam)
    #print(df_train.shape)

    additional_columns = [
        'AgeOnInclusion', 'RRSysWeightedMeanValue', 'RRDiaWeightedMeanValue', 'sO2WeightedMeanValue',
        'PHWeightedMean', 'LactateWeightedMean', 'gluWeightedMean', 'HCO3WeightedMean', 'CreatinineWeightedMean',
        'DayNumber', 'PO2WeightedMean', 'PCO2WeightedMean', 'HBWeightedMean', 'ureaWeightedMean', 'HRWeightedMean',
        'TempWeightedMean', 'NaWeightedMean', 'KWeightedMean', 'ClWeightedMean', 'Height', 'Weight', 'plateletsWeightedMean',
        'HighestLactate', 'LowestpH', 'leucoWeightedMean'
    ]
    


    # Start- und Zielwerte
    start_scale_pos_wight_outlier_fun = 15
    end_scale_pos_wight_outlier_fun = 1

    start_scale_pos_weight = 10
    end_scale_pos_weight = 1

    # Anzahl der Stunden
    n_hours = len(hours_list)

    # Berechnung der skalierten Werte mit einer Exponentialfunktion
    scale_pos_wight_outlier_fun_values = np.exp(
        np.linspace(np.log(start_scale_pos_wight_outlier_fun), np.log(end_scale_pos_wight_outlier_fun), n_hours)
    )
    scale_pos_weight_values = np.exp(
        np.linspace(np.log(start_scale_pos_weight), np.log(end_scale_pos_weight), n_hours)
    )
    
    final_models = {
        1: {'model': p_model_24h, 'outlier_model': s_model_24h, 'scaler': scaler_24h},
        2: {'model': p_model_2d, 'outlier_model': s_model_2d, 'scaler': scaler_2d},
        3: {'model': p_model_3d, 'outlier_model': s_model_3d, 'scaler': scaler_3d},
        4: {'model': p_model_4d, 'outlier_model': s_model_4d, 'scaler': scaler_4d},
        5: {'model': p_model_5d, 'outlier_model': s_model_5d, 'scaler': scaler_5d},
        6: {'model': p_model_6d, 'outlier_model': s_model_6d, 'scaler': scaler_6d},
        7: {'model': p_model_7d, 'outlier_model': s_model_7d, 'scaler': scaler_7d},
        8: {'model': p_model_8d, 'outlier_model': s_model_8d, 'scaler': scaler_8d},
        9: {'model': p_model_9d, 'outlier_model': s_model_9d, 'scaler': scaler_9d},
        10: {'model': p_model_10d, 'outlier_model': s_model_10d, 'scaler': scaler_10d},
        11: {'model': p_model_11d, 'outlier_model': s_model_11d, 'scaler': scaler_11d},
        12: {'model': p_model_12d, 'outlier_model': s_model_12d, 'scaler': scaler_12d},
        13: {'model': p_model_13d, 'outlier_model': s_model_13d, 'scaler': scaler_13d},
        14: {'model': p_model_14d, 'outlier_model': s_model_14d, 'scaler': scaler_14d},
        15: {'model': p_model_15d, 'outlier_model': s_model_15d, 'scaler': scaler_15d},
        21: {'model': p_model_3w, 'outlier_model': s_model_3w, 'scaler': scaler_3w}
    }

    for idx, hours in enumerate(hours_list):
        print(f"Training and evaluating for mortality prediction at {hours/24} days...")

        # Aktuelle Werte aus den vorab berechneten Listen entnehmen
        scale_pos_wight_outlier_fun = scale_pos_wight_outlier_fun_values[idx]
        scale_pos_weight = scale_pos_weight_values[idx]

        # Vorbereitung der Datensätze und Trainingsprozess
        df_train_hourly = prepare_hourly_datasets(hours, df_train, additional_columns)
        df_ext_val_hourly = prepare_hourly_datasets(hours, df_ext_val, additional_columns)

        # Berechnung des Klassenungleichgewichts
        pos_count = df_train_hourly[label_col].sum()
        neg_count = len(df_train_hourly) - pos_count
        #print(pos_count, neg_count)
        imbalance_ratio = pos_count / (pos_count + neg_count) if neg_count != 0 else 0
        imbalance_ratios.append(imbalance_ratio*100)

        # Training der Modelle
        
        '''
        scaler, s_model, df_train_clean, df_train_outliers = train_scaler_s_model(
            df_train_hourly, label_col, [label_col], noise_factor=4, SA_number=1, 
            scale_pos_wight_outlier_fun=scale_pos_wight_outlier_fun
        )

        p_model, optimal_threshold = train_prediction_model(
            df_train_hourly, scaler, s_model, label_col, scale_pos_weight=scale_pos_weight, n_trials=optuna_trails, should_print=False
        )
        
        '''
        
        #load the models

        model_data = final_models.get(hours / 24)
        
        # Extrahiere das Modell, den Outlier-Erkennungsmodell und den Scaler
        p_model = model_data['model']
        s_model = model_data['outlier_model']
        scaler = model_data['scaler']
        
        
        optimal_threshold = optimal_thresholds.get(hours/24, 0.5)
        
        
        

        # Evaluation und Speicherung der Ergebnisse
        auc, prauc, sen, spez, ppv, npv, low_confidence_indices = eval_model_over_time(df_ext_val_hourly, label_col, scaler, s_model, p_model, optimal_threshold, plots=False)
        fn, fp, tn, tp, pra = eval_model_con_mat(df_ext_val_hourly, label_col, scaler, s_model, p_model, optimal_threshold, plots=False)
        
        #print(prauc)
        auc_values['Whole dataset'].append(auc['whole'])
        auc_values['High confidence subgroup'].append(auc['good'])
        auc_values['Low confidence subgroup'].append(auc['bad'])
        prauc_values['Whole dataset'].append(prauc['whole'])
        prauc_values['High confidence subgroup'].append(prauc['good'])
        prauc_values['Low confidence subgroup'].append(prauc['bad'])
        
        sen_values['Whole dataset'].append(sen['whole'])
        sen_values['High confidence subgroup'].append(sen['good'])
        sen_values['Low confidence subgroup'].append(sen['bad'])
        spez_values['Whole dataset'].append(spez['whole'])
        spez_values['High confidence subgroup'].append(spez['good'])
        spez_values['Low confidence subgroup'].append(spez['bad'])
        ppv_values['Whole dataset'].append(ppv['whole'])
        ppv_values['High confidence subgroup'].append(ppv['good'])
        ppv_values['Low confidence subgroup'].append(ppv['bad'])
        npv_values['Whole dataset'].append(npv['whole'])
        npv_values['High confidence subgroup'].append(npv['good'])
        npv_values['Low confidence subgroup'].append(npv['bad'])
        
        fn_values['Whole dataset'].append(fn['whole'])
        fn_values['High confidence subgroup'].append(fn['good'])
        fn_values['Low confidence subgroup'].append(fn['bad'])
        fp_values['Whole dataset'].append(fp['whole'])
        fp_values['High confidence subgroup'].append(fp['good'])
        fp_values['Low confidence subgroup'].append(fp['bad'])
        tn_values['Whole dataset'].append(tn['whole'])
        tn_values['High confidence subgroup'].append(tn['good'])
        tn_values['Low confidence subgroup'].append(tn['bad'])
        tp_values['Whole dataset'].append(tp['whole'])
        tp_values['High confidence subgroup'].append(tp['good'])
        tp_values['Low confidence subgroup'].append(tp['bad'])
        pra_values['Whole dataset'].append(pra['whole'])
        pra_values['High confidence subgroup'].append(pra['good'])
        pra_values['Low confidence subgroup'].append(pra['bad'])
        
        subgroup_dict[hours] = low_confidence_indices
        
        
        
        

    # Plot ROC AUC, PR AUC, and class imbalance over the hourly timeframes
    plot_auc_over_time_and_imbalance(hours_list, auc_values, prauc_values, imbalance_ratios)
    plot_metrics_over_time(hours_list, sen_values, spez_values, ppv_values, npv_values)
    plot_balanced_error_trends(hours_list, fn_values, fp_values, tn_values, tp_values, pra_values)
    plot_patient_subgroup_evolution(subgroup_dict, hours_list)
    plot_separation_by_probabilities(df_ext_val, final_models, label_col, hours_list, additional_columns)
    plot_icu_patient_stay(subgroup_dict, hours_list, patientid_list_ext_val)
    return auc_values, prauc_values, imbalance_ratios



def plot_separation_by_probabilities(df_ext, final_models, label_col, time_windows, additional_columns):
    """
    Plots the separation of data based on predicted probabilities for different time windows
    (1, 5, 10, and 15 days) in subplots with different colors based on outlier status.

    Args:
        df_ext (DataFrame): External validation dataset.
        final_models (dict): Dictionary containing the prediction models (p_models) 
                             and separation models (s_models) for each time window.
        label_col (str): Name of the target label column.
        additional_columns (list): List of additional feature columns.

    Returns:
        None
    """
    selected_days = [1, 5, 10, 15]  # Die gewünschten Zeitfenster in Tagen
    fig, axes = plt.subplots(2, 2, figsize=(12, 10))  # 2x2 Grid für die Subplots
    axes = axes.flatten()  # Um einfacher auf die Achsen zuzugreifen
    
    for idx, day in enumerate(selected_days):
        if day not in final_models:
            print(f"Skipping {day}d: No model available.")
            continue  # Falls das Modell nicht existiert, weiter

        model_data = final_models[day]
        p_model = model_data['model']
        s_model = model_data['outlier_model']
        scaler = model_data['scaler']

        # Daten für das aktuelle Zeitfenster vorbereiten
        df_time = prepare_hourly_datasets(day, df_ext, additional_columns)

        if df_time.empty:
            print(f"Skipping {day}d: No data available after filtering.")
            continue

        # Kopie der Daten erstellen und Skalierung durchführen
        df = df_time.copy().reset_index(drop=True)
        df_pca = df.drop(columns=[label_col])
        df_scaled = scaler.transform(df_pca)
        df_pca = pd.DataFrame(df_scaled, columns=df_pca.columns.tolist())

        # Outlier-Vorhersage
        outliers = s_model.predict(df_pca)
        
        # Falls alle Datenpunkte Outlier sind oder keine Outlier vorhanden sind, weiter
        if np.all(outliers == 0) or np.all(outliers == 1):
            print(f"Skipping {day}d: No separation between outliers and non-outliers.")
            continue

        # Wahrscheinlichkeiten berechnen für beide Gruppen
        df_outliers = df_time[outliers == 1]  # Outlier = 0 (rote Farbe)
        df_non_outliers = df_time[outliers == 0]  # Nicht-Outlier = 1 (blaue Farbe)

        if not df_non_outliers.empty:
            pred_probs_non_outliers = p_model.predict_proba(df_non_outliers.drop(columns=[label_col]))[:, 1]
            axes[idx].hist(pred_probs_non_outliers, bins=30, alpha=0.5, color='green', label="High confidence subgroup")
        
        if not df_outliers.empty:
            pred_probs_outliers = p_model.predict_proba(df_outliers.drop(columns=[label_col]))[:, 1]
            axes[idx].hist(pred_probs_outliers, bins=30, alpha=0.5, color='red', label="Low confidence subgroup")
        

        # Achsentitel setzen
        axes[idx].set_title(f"Day {day}")
        axes[idx].set_xlabel("Predicted Probability")
        axes[idx].set_ylabel("Frequency")
        axes[idx].legend()
        axes[idx].grid(True)

    plt.tight_layout()
    plt.show()

def plot_patient_subgroup_evolution(subgroup_dict, hours_list):
    """
    Analyzes and visualizes the evolution of 'Low Confidence Subgroup' patient-days over time.

    Args:
        subgroup_dict (dict): Dictionary where keys are time thresholds (in hours, e.g., 24, 48, ...) 
                              and values are lists of patient IDs in the low confidence subgroup.
        hours_list (list): List of time thresholds (in hours).
    """
    days = [h // 24 for h in hours_list]  # Convert hours to days
    all_patient_days = set().union(*subgroup_dict.values())  # Get all unique patient-days

    # Tracking variables
    persistence_rate = []  # Fraction of patient-days that remained in the subgroup from the previous day
    cumulative_persistence = []  # Persistence rate considering all previous days
    day_1_persistence = []  # Persistence rate compared to Day 1

    previous_patient_days = set()
    cumulative_patients = set()

    # Get initial Day 1 patients
    day_1_patients = set(subgroup_dict.get(hours_list[0], [])) if hours_list else set()

    for hours in hours_list:
        current_patient_days = set(subgroup_dict.get(hours, []))
        
        # Update cumulative patients (to track how many have ever been in the subgroup)
        cumulative_patients.update(current_patient_days)

        # Calculate persistence metrics
        still_in_subgroup = len(previous_patient_days & current_patient_days)
        persistence_rate.append(still_in_subgroup / len(previous_patient_days) if previous_patient_days else 1)

        cumulative_persistence.append(len(cumulative_patients & current_patient_days) / len(cumulative_patients) if cumulative_patients else 1)

        # Persistence rate compared to Day 1
        day_1_persistence.append(len(day_1_patients & current_patient_days) / len(day_1_patients) if day_1_patients else 1)

        previous_patient_days = current_patient_days  # Update for next iteration
        
    # Plotting
    plt.figure(figsize=(12, 6))

    # Line plots for persistence rates
    plt.plot(days, persistence_rate, label="Daily Persistence Rate", marker="o", linestyle="-", color="blue") 
    plt.plot(days, cumulative_persistence, label="Cumulative Persistence Rate", marker="s", linestyle="--", color="purple")
    plt.plot(days, day_1_persistence, label="Persistence from Day 1", marker="D", linestyle="-.", color="orange")

    # Labels and legend
    plt.xlabel("Days Since Prediction Start", fontsize=14)
    plt.ylabel("Proportion of Patient-Days", fontsize=14)
    plt.title("Persistence of Low Confidence Subgroup Over Prediction Days", fontsize=16)
    plt.axhline(0, color="black", linewidth=0.8)  # Baseline
    plt.legend(fontsize=12)
    plt.grid(alpha=0.3)

    # Improve x-axis readability
    plt.xticks(days, labels=[f"{d}" for d in days], fontsize=12)
    plt.yticks(fontsize=12)

    plt.show()

def plot_icu_patient_stay(subgroup_dict, hours_list, patient_ID_list):
    """
    Analyzes and visualizes the ICU stay of patients in the low-confidence subgroup.

    Args:
        subgroup_dict (dict): Dictionary where keys are time thresholds (in hours, e.g., 24, 48, ...) 
                              and values are lists of patient IDs in the low-confidence subgroup.
        hours_list (list): List of time thresholds (in hours).
        patient_ID_list (list): List of patient IDs for each patient day in the sequence.
    """
    # Initialize data structures to track patient days in the subgroup
    #patient_first_day_in_subgroup = [] # To track the first ICU day of each patient
    patient_days_in_subgroup = {}  # To count how many days each patient stays in the subgroup
    total_days = {}

    # Loop through the patient_ID_list (each index represents a patient day)
    for day_index, patient_id in enumerate(patient_ID_list):
        # Check if the patient is in the subgroup for the current day (based on hours_list)
        day_index = day_index + 1

        if patient_id not in total_days:
            total_days[patient_id] = 0
        total_days[patient_id] += 1
            
        for hours in hours_list:
            if day_index in subgroup_dict.get(hours, []):
                # Track the first day of the ICU stay in the subgroup
                #if total_days[patient_id] == 1:
                #    patient_first_day_in_subgroup.append(patient_id)
                
                # Track how many days the patient is in the subgroup
                if patient_id not in patient_days_in_subgroup:
                    patient_days_in_subgroup[patient_id] = {}
                if total_days[patient_id] not in patient_days_in_subgroup[patient_id]:
                    patient_days_in_subgroup[patient_id][total_days[patient_id]] = []
                patient_days_in_subgroup[patient_id][total_days[patient_id]].append(hours/24)
                


    # Now categorize the patients
    patients_first_day_in_subgroup = {}
    #patients_some_days_in_subgroup = []
    patients_all_days_in_subgroup = {}
    patients_in_subgroup = []

    #total_days = len(patient_ID_list)
    
    
    unique_values = []
    for item in patient_ID_list:
        if item not in unique_values:
            unique_values.append(item)

        
        
    for patient_id in list(set(unique_values)):
        #print(unique_values)
        #first_day_in_subgroup = patient_first_day_in_subgroup[patient_id]
        #total_days_in_subgroup = patient_days_in_subgroup[patient_id]
        #print(len(patient_days_in_subgroup[patient_id].keys()))
        # Categorize patients

        if patient_id in patient_days_in_subgroup.keys():   

            patients_in_subgroup.append(patient_id)

            for patient, entries in patient_days_in_subgroup.items():
                common_values = set.intersection(*map(set, entries.values())) if entries else set()
                if common_values:
                    patients_all_days_in_subgroup[patient] = list(common_values)
                        
            #if len(patient_days_in_subgroup[patient_id].keys()) == total_days[patient_id]:
            #    patients_all_days_in_subgroup.append(patient_id)
                #print(patients_all_days_in_subgroup)
                
            #if patient_days_in_subgroup[patient_id] == total_days[patient_id]:
            #    patients_all_days_in_subgroup.append(patient_id)  # Stayed in subgroup all days
            if 1 in patient_days_in_subgroup[patient_id].keys():  # First ICU day was in the subgroup
                patients_first_day_in_subgroup[patient_id] = patient_days_in_subgroup[patient_id][1]

    patients_some_days_in_subgroup = {patient: sorted(set(val for sublist in entries.values() for val in sublist)) for patient, entries in patient_days_in_subgroup.items()}

    # Zähle die Anzahl der Patienten pro Kategorie für jeden Vorhersagetag
    first_day_counts = {day: 0 for day in hours_list}
    some_days_counts = {day: 0 for day in hours_list}
    all_days_counts = {day: 0 for day in hours_list}

    for patient, days in patients_first_day_in_subgroup.items():
        for day in days:
            first_day_counts[day * 24] += 1  # Umrechnung zurück in Stunden für Konsistenz

    for patient, days in patients_some_days_in_subgroup.items():
        for day in days:
            some_days_counts[day * 24] += 1

    for patient, days in patients_all_days_in_subgroup.items():
        for day in days:
            all_days_counts[day * 24] += 1

    # Daten für den Plot
    days = [h // 24 for h in hours_list]  # Umwandlung von Stunden in Tage
    first_day_values = [first_day_counts[h]/len(patients_in_subgroup) for h in hours_list]
    some_days_values = [some_days_counts[h]/len(patients_in_subgroup) for h in hours_list]
    all_days_values = [all_days_counts[h]/len(patients_in_subgroup) for h in hours_list]

    # Plot erstellen
    plt.figure(figsize=(12, 6))

    plt.plot(days, first_day_values, label="First Day in Subgroup", marker="o", linestyle="-", color="blue")
    plt.plot(days, some_days_values, label="Some Days in Subgroup", marker="s", linestyle="--", color="green")
    plt.plot(days, all_days_values, label="All Days in Subgroup", marker="D", linestyle=":", color="red")

    # Achsenbeschriftungen und Legende
    plt.xlabel("Prediction Days", fontsize=14)
    plt.ylabel("Number of Patients", fontsize=14)
    plt.title("ICU Stay in Low Confidence Subgroup Over Prediction Days", fontsize=16)
    plt.axhline(0, color="black", linewidth=0.8)  # Basislinie
    plt.legend(fontsize=12)
    plt.grid(alpha=0.3)

    # X-Achse besser lesbar machen
    plt.xticks(days, labels=[f"{d}" for d in days], fontsize=12)
    plt.yticks(fontsize=12)

    plt.show()

def plot_auc_over_time(hours_list, auc_values, prauc_values):
    """
    Plots ROC AUC and PR AUC across different hourly thresholds.

    Args:
        hours_list (list): List of hours thresholds.
        auc_values (dict): Dictionary containing AUC values for the whole dataset, good, and bad cohorts.
        prauc_values (dict): Dictionary containing PR AUC values for the whole dataset, good, and bad cohorts.
    
    Returns:
        None. It displays the plots.
    """
    plt.figure(figsize=(14, 7))

    # Plot ROC AUC
    plt.subplot(1, 2, 1)
    plt.plot(hours_list, auc_values['whole'], label="Whole Dataset AUC", color="orange", marker="o")
    plt.plot(hours_list, auc_values['good'], label="Good Cohort AUC", color="green", marker="o")
    plt.plot(hours_list, auc_values['bad'], label="Bad Cohort AUC", color="red", marker="o")
    plt.xlabel('Timeframe (Hours)')
    plt.ylabel('ROC AUC')
    plt.title('ROC AUC Across Hourly Predictions')
    plt.legend(loc="lower right")

    # Plot PR AUC
    plt.subplot(1, 2, 2)
    plt.plot(hours_list, prauc_values['whole'], label="Whole Dataset PR AUC", color="orange", marker="o")
    plt.plot(hours_list, prauc_values['good'], label="Good Cohort PR AUC", color="green", marker="o")
    plt.plot(hours_list, prauc_values['bad'], label="Bad Cohort PR AUC", color="red", marker="o")
    plt.xlabel('Timeframe (Hours)')
    plt.ylabel('PR AUC')
    plt.title('PR AUC Across Hourly Predictions')
    plt.legend(loc="lower right")

    plt.tight_layout()
    plt.show()


def calculate_metrics(y_test, preds, pred_proba):
    """ Helper function to calculate metrics and curve data. """
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    roc_auc = roc_auc_score(y_test, pred_proba)
    aucpr = average_precision_score(y_test, pred_proba)
    
    # Precision-recall curve data
    precision_curve, recall_curve, _ = precision_recall_curve(y_test, pred_proba)
    
    # Confusion matrix
    conf_matrix = confusion_matrix(y_test, preds)

    # ROC curve data
    fpr, tpr, _ = roc_curve(y_test, pred_proba)
    auc_score = auc(fpr, tpr)

    return auc_score, aucpr, conf_matrix, fpr, tpr, precision_curve, recall_curve

def evaluate_cohort(df_cohort, label, final_model, optimal_threshold, scaler):
    """ Helper function to evaluate the model on either the good or bad cohort. """
    if len(df_cohort) == 0:  # Handle empty cohorts
        return np.nan, np.nan, None, None, None, None, None

    X_test = df_cohort.drop(label, axis=1)
    y_test = df_cohort[label]
    
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate metrics
    return calculate_metrics(y_test, preds, pred_proba)

def plot_evaluation_results(fpr_w, tpr_w, auc_w, fpr_g, tpr_g, auc_g, fpr_b, tpr_b, auc_b, 
                            recall_w, precision_w, pr_auc_w, recall_g, precision_g, pr_auc_g, 
                            recall_b, precision_b, pr_auc_b, conf_matrix_w, conf_matrix_g, conf_matrix_b):
    """ Helper function to create ROC, PR, and confusion matrix plots. """
    fig, axs = plt.subplots(2, 2, figsize=(20, 16))

    # Plot ROC curves
    axs[0, 0].plot(fpr_w, tpr_w, label=f'Whole Dataset (AUC = {auc_w:.2f})', lw=2, color='orange')
    axs[0, 0].plot(fpr_g, tpr_g, label=f'Good Cohort (AUC = {auc_g:.2f})', lw=2, color='green')
    axs[0, 0].plot(fpr_b, tpr_b, label=f'Bad Cohort (AUC = {auc_b:.2f})', lw=2, color='red')
    axs[0, 0].plot([0, 1], [0, 1], linestyle='--', lw=2, color='navy')
    axs[0, 0].set_title('ROC Curve')
    axs[0, 0].legend(loc="lower right")

    # Plot Precision-Recall curves
    axs[0, 1].plot(recall_w, precision_w, label=f'Whole Dataset (PR AUC = {pr_auc_w:.2f})', lw=2, color='orange')
    axs[0, 1].plot(recall_g, precision_g, label=f'Good Cohort (PR AUC = {pr_auc_g:.2f})', lw=2, color='green')
    axs[0, 1].plot(recall_b, precision_b, label=f'Bad Cohort (PR AUC = {pr_auc_b:.2f})', lw=2, color='red')
    axs[0, 1].set_title('Precision-Recall Curve')
    axs[0, 1].legend(loc="lower left")

    # Confusion matrix plot skipped for clarity, but similar logic applies

    plt.tight_layout()
    plt.show()

def plot_auc_over_time_and_imbalance(hours_list, auc_values, prauc_values, imbalance_ratios):
    """
    Plots ROC AUC, PR AUC, and class imbalance (positive/negative ratio) across different time thresholds in days,
    with numerical values displayed at each point.

    Args:
        hours_list (list): List of hours thresholds.
        auc_values (dict): Dictionary containing AUC values for the whole dataset, good, and bad cohorts.
        prauc_values (dict): Dictionary containing PR AUC values for the whole dataset, good, and bad cohorts.
        imbalance_ratios (list): List of class imbalance ratios (positives/negatives) for each hour threshold.
    
    Returns:
        None. It displays the plots.
    """
    days_list = [h / 24 for h in hours_list]  # Umrechnung von Stunden in Tage

    plt.figure(figsize=(17, 9))

    # Plot ROC AUC
    plt.subplot(2, 2, 1)
    for key, color in zip(["Whole dataset", "High confidence subgroup", "Low confidence subgroup"], ["orange", "green", "red"]):
        plt.plot(days_list, auc_values[key], label=f"{key.capitalize()} ROC AUC", color=color, marker="o")
        for x, y in zip(days_list, auc_values[key]):
            plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('ROC AUC')
    plt.title('ROC AUC Across Daily Predictions')
    plt.legend(loc="upper right")

    # Plot PR AUC
    plt.subplot(2, 2, 2)
    for key, color in zip(["Whole dataset", "High confidence subgroup", "Low confidence subgroup"], ["orange", "green", "red"]):
        plt.plot(days_list, prauc_values[key], label=f"{key.capitalize()} PR AUC", color=color, marker="o")
        for x, y in zip(days_list, prauc_values[key]):
            plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('PR AUC')
    plt.title('PR AUC Across Daily Predictions')
    plt.legend(loc="lower right")

    # Plot Class Imbalance (Positives/Negatives)
    plt.subplot(2, 1, 2)
    plt.plot(days_list, imbalance_ratios, label="Class Imbalance (positives %)", color="blue", marker="o")
    for x, y in zip(days_list, imbalance_ratios):
        plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('positives %')
    plt.title('Class Imbalance Across Daily Predictions')
    plt.legend(loc="lower right")

    plt.tight_layout()
    plt.show()
    
def eval_model_over_time(df_test, label, scaler, model, final_model, optimal_threshold, plots=False):
    """
    Evaluates the performance of the trained model on a test dataset and generates relevant metrics and plots.

    Args:
        df_test (DataFrame): The test dataset containing features and the target label.
        label (str): The name of the target variable for classification.
        scaler (PowerTransformer): The fitted scaler used for transforming feature data.
        model (XGBClassifier): The trained outlier detection model used for identifying outliers in the dataset.
        final_model (XGBClassifier): The final trained prediction model used for classification.
        optimal_threshold (float): The optimal threshold for classifying predictions.
        plots (bool): Flag to determine whether to plot evaluation results (default: False).

    Returns:
        dict: A dictionary containing AUC, PR AUC, sensitivity, specificity, PPV, and NPV for the whole, good, and bad datasets.
    """
    
    # Helper function to calculate sensitivity, specificity, PPV, and NPV
    def calculate_additional_metrics(conf_matrix):
        tn, fp, fn, tp = conf_matrix.ravel()
        sensitivity = tp / (tp + fn) if (tp + fn) > 0 else 0
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0
        ppv = tp / (tp + fp) if (tp + fp) > 0 else 0
        npv = tn / (tn + fn) if (tn + fn) > 0 else 0
        return sensitivity, specificity, ppv, npv

    # Create a copy of the test dataset to avoid modifying the original
    df = df_test.copy()
    df = df.reset_index(drop=True)
    
    # Prepare the feature set for PCA and scaling
    df_pca = df.drop(columns=[label])
    df_scaled = scaler.transform(df_pca)

    # Create a DataFrame from the scaled features
    df_pca = pd.DataFrame(df_scaled, columns=df_pca.columns.tolist())

    # Predict outliers using the outlier detection model
    outliers = model.predict(df_pca)
    df_pca['outliers'] = outliers

    # Separate the clean data and outliers based on the predictions
    df_clean = df[df_pca['outliers'] == 0]
    df_outlier = df[df_pca['outliers'] == 1]
    
    # Prepare the full test set for evaluation
    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities using the final trained model
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate various performance metrics for the whole dataset
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    auc_w = roc_auc_score(y_test, pred_proba)
    pr_auc_w = average_precision_score(y_test, pred_proba)

    # Precision-recall curve data for the whole dataset
    precision_w, recall_w, thresholds = precision_recall_curve(y_test, pred_proba)

    # Confusion matrix for the whole dataset
    conf_matrix_w = confusion_matrix(y_test, preds)
    sensitivity_w, specificity_w, ppv_w, npv_w = calculate_additional_metrics(conf_matrix_w)

    # ROC curve data for the whole dataset
    fpr_w, tpr_w, thresholds = roc_curve(y_test, pred_proba)
    auc_score_w = auc(fpr_w, tpr_w)

    # Evaluate the model on the cleaned dataset (without outliers)
    df = df_clean.copy()
    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities and classify based on the optimal threshold
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate metrics for the cleaned dataset
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    auc_g = roc_auc_score(y_test, pred_proba)
    pr_auc_g = average_precision_score(y_test, pred_proba)

    # Precision-recall curve data for the cleaned dataset
    precision_g, recall_g, thresholds = precision_recall_curve(y_test, pred_proba)

    # Confusion matrix for the cleaned dataset
    conf_matrix_g = confusion_matrix(y_test, preds)
    sensitivity_g, specificity_g, ppv_g, npv_g = calculate_additional_metrics(conf_matrix_g)

    # ROC curve data for the cleaned dataset
    fpr_g, tpr_g, thresholds = roc_curve(y_test, pred_proba)
    auc_score_g = auc(fpr_g, tpr_g)

    # Evaluate the model on the outlier dataset
    df = df_outlier.copy()
    X_test = df.drop(label, axis=1)
    y_test = df[label]

    # Predict probabilities and classify based on the optimal threshold
    pred_proba = final_model.predict_proba(X_test)[:, 1]
    preds = (pred_proba >= optimal_threshold).astype(int)

    # Calculate metrics for the outlier dataset
    accuracy = accuracy_score(y_test, preds)
    precision = precision_score(y_test, preds)
    recall = recall_score(y_test, preds)
    f1 = f1_score(y_test, preds)
    auc_b = roc_auc_score(y_test, pred_proba)
    pr_auc_b = average_precision_score(y_test, pred_proba)

    # Precision-recall curve data for the outlier dataset
    precision_b, recall_b, thresholds = precision_recall_curve(y_test, pred_proba)

    # Confusion matrix for the outlier dataset
    conf_matrix_b = confusion_matrix(y_test, preds)
    sensitivity_b, specificity_b, ppv_b, npv_b = calculate_additional_metrics(conf_matrix_b)

    # ROC curve data for the outlier dataset
    fpr_b, tpr_b, thresholds = roc_curve(y_test, pred_proba)
    auc_score_b = auc(fpr_b, tpr_b)

    # Collect results in a dictionary
    auc_values = {'whole': auc_w, 'good': auc_g, 'bad': auc_b}
    prauc_values = {'whole': pr_auc_w, 'good': pr_auc_g, 'bad': pr_auc_b}
    sensitivity_values = {'whole': sensitivity_w, 'good': sensitivity_g, 'bad': sensitivity_b}
    specificity_values = {'whole': specificity_w, 'good': specificity_g, 'bad': specificity_b}
    ppv_values = {'whole': ppv_w, 'good': ppv_g, 'bad': ppv_b}
    npv_values = {'whole': npv_w, 'good': npv_g, 'bad': npv_b}
    
    low_confidence_indices = df_pca[df_pca['outliers'] == 1].index.tolist()  # Indices of outliers
    #subgroup_dict = {"low_confidence": low_confidence_indices}

    return auc_values, prauc_values, sensitivity_values, specificity_values, ppv_values, npv_values, low_confidence_indices

def plot_metrics_over_time(hours_list, sensitivity_values, specificity_values, ppv_values, npv_values):
    """
    Plots sensitivity, specificity, PPV, and NPV across different time thresholds in days,
    with numerical values displayed at each point.

    Args:
        hours_list (list): List of hours thresholds.
        sensitivity_values (dict): Dictionary containing sensitivity values for the whole dataset, good, and bad cohorts.
        specificity_values (dict): Dictionary containing specificity values for the whole dataset, good, and bad cohorts.
        ppv_values (dict): Dictionary containing PPV values for the whole dataset, good, and bad cohorts.
        npv_values (dict): Dictionary containing NPV values for the whole dataset, good, and bad cohorts.
    
    Returns:
        None. It displays the plots.
    """
    days_list = [h / 24 for h in hours_list]  # Convert hours to days

    # Ensure the metrics contain lists
    assert len(days_list) == len(sensitivity_values["Whole dataset"]), f"Mismatch in length: {len(days_list)} != {len(sensitivity_values['Whole dataset'])}"
    assert len(days_list) == len(specificity_values["Whole dataset"]), f"Mismatch in length: {len(days_list)} != {len(specificity_values['Whole dataset'])}"
    assert len(days_list) == len(ppv_values["Whole dataset"]), f"Mismatch in length: {len(days_list)} != {len(ppv_values['Whole dataset'])}"
    assert len(days_list) == len(npv_values["Whole dataset"]), f"Mismatch in length: {len(days_list)} != {len(npv_values['Whole dataset'])}"

    plt.figure(figsize=(17, 9))

    # Plot Sensitivity
    plt.subplot(2, 2, 1)
    for key, color in zip(["Whole dataset", "High confidence subgroup", "Low confidence subgroup"], ["orange", "green", "red"]):
        plt.plot(days_list, sensitivity_values[key], label=f"{key.capitalize()} Sensitivity", color=color, marker="o")
        for x, y in zip(days_list, sensitivity_values[key]):
            plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('Sensitivity')
    plt.title('Sensitivity Across Daily Predictions')
    plt.legend(loc="lower right")

    # Plot Specificity
    plt.subplot(2, 2, 2)
    for key, color in zip(["Whole dataset", "High confidence subgroup", "Low confidence subgroup"], ["orange", "green", "red"]):
        plt.plot(days_list, specificity_values[key], label=f"{key.capitalize()} Specificity", color=color, marker="o")
        for x, y in zip(days_list, specificity_values[key]):
            plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('Specificity')
    plt.title('Specificity Across Daily Predictions')
    plt.legend(loc="lower right")

    # Plot PPV
    plt.subplot(2, 2, 3)
    for key, color in zip(["Whole dataset", "High confidence subgroup", "Low confidence subgroup"], ["orange", "green", "red"]):
        plt.plot(days_list, ppv_values[key], label=f"{key.capitalize()} PPV", color=color, marker="o")
        for x, y in zip(days_list, ppv_values[key]):
            plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('PPV')
    plt.title('Positive Predictive Value Across Daily Predictions')
    plt.legend(loc="lower right")

    # Plot NPV
    plt.subplot(2, 2, 4)
    for key, color in zip(["Whole dataset", "High confidence subgroup", "Low confidence subgroup"], ["orange", "green", "red"]):
        plt.plot(days_list, npv_values[key], label=f"{key.capitalize()} NPV", color=color, marker="o")
        for x, y in zip(days_list, npv_values[key]):
            plt.text(x, y, f"{y:.2f}", fontsize=10, ha='right', va='bottom')
    plt.xlabel('Timeframe (Days)')
    plt.ylabel('NPV')
    plt.title('Negative Predictive Value Across Daily Predictions')
    plt.legend(loc="lower right")

    plt.tight_layout()
    plt.show()

def eval_model_con_mat(df_test, label, scaler, model, final_model, optimal_threshold, plots=False):
    """
    Evaluates the performance of the trained model on a test dataset and generates relevant metrics and plots.

    Args:
        df_test (DataFrame): The test dataset containing features and the target label.
        label (str): The name of the target variable for classification.
        scaler (PowerTransformer): The fitted scaler used for transforming feature data.
        model (XGBClassifier): The trained outlier detection model used for identifying outliers in the dataset.
        final_model (XGBClassifier): The final trained prediction model used for classification.
        optimal_threshold (float): The optimal threshold for classifying predictions.
        plots (bool): Flag to determine whether to plot evaluation results (default: False).

    Returns:
        dict: A dictionary containing AUC, PR AUC, sensitivity, specificity, PPV, NPV, FN, FP, TN, TP, and prevalence for the whole, good, and bad datasets.
    """

    def calculate_metrics(conf_matrix, y_true):
        tn, fp, fn, tp = conf_matrix.ravel()
        sensitivity = tp / (tp + fn) if (tp + fn) > 0 else 0
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0
        ppv = tp / (tp + fp) if (tp + fp) > 0 else 0
        npv = tn / (tn + fn) if (tn + fn) > 0 else 0
        prevalence = y_true.mean()
        return sensitivity, specificity, ppv, npv, fn, fp, tn, tp, prevalence

    df = df_test.copy().reset_index(drop=True)
    df_pca = df.drop(columns=[label])
    df_scaled = scaler.transform(df_pca)
    df_pca = pd.DataFrame(df_scaled, columns=df_pca.columns.tolist())

    outliers = model.predict(df_pca)
    df_pca['outliers'] = outliers

    df_clean = df[df_pca['outliers'] == 0]
    df_outlier = df[df_pca['outliers'] == 1]

    def evaluate_dataset(df_subset):
        if df_subset.empty:
            return None, None, None, None, None, None, None, None, None, None, None

        X_test = df_subset.drop(columns=[label])
        y_test = df_subset[label]

        pred_proba = final_model.predict_proba(X_test)[:, 1]
        preds = (pred_proba >= optimal_threshold).astype(int)

        auc_score = roc_auc_score(y_test, pred_proba)
        pr_auc_score = average_precision_score(y_test, pred_proba)
        conf_matrix = confusion_matrix(y_test, preds)
        sensitivity, specificity, ppv, npv, fn, fp, tn, tp, prevalence = calculate_metrics(conf_matrix, y_test)

        return auc_score, pr_auc_score, sensitivity, specificity, ppv, npv, fn, fp, tn, tp, prevalence

    auc_w, pr_auc_w, sensitivity_w, specificity_w, ppv_w, npv_w, fn_w, fp_w, tn_w, tp_w, prevalence_w = evaluate_dataset(df)
    auc_g, pr_auc_g, sensitivity_g, specificity_g, ppv_g, npv_g, fn_g, fp_g, tn_g, tp_g, prevalence_g = evaluate_dataset(df_clean)
    auc_b, pr_auc_b, sensitivity_b, specificity_b, ppv_b, npv_b, fn_b, fp_b, tn_b, tp_b, prevalence_b = evaluate_dataset(df_outlier)

    auc_values = {'whole': auc_w, 'good': auc_g, 'bad': auc_b}
    prauc_values = {'whole': pr_auc_w, 'good': pr_auc_g, 'bad': pr_auc_b}
    sensitivity_values = {'whole': sensitivity_w, 'good': sensitivity_g, 'bad': sensitivity_b}
    specificity_values = {'whole': specificity_w, 'good': specificity_g, 'bad': specificity_b}
    ppv_values = {'whole': ppv_w, 'good': ppv_g, 'bad': ppv_b}
    npv_values = {'whole': npv_w, 'good': npv_g, 'bad': npv_b}
    fn_values = {'whole': fn_w, 'good': fn_g, 'bad': fn_b}
    fp_values = {'whole': fp_w, 'good': fp_g, 'bad': fp_b}
    tn_values = {'whole': tn_w, 'good': tn_g, 'bad': tn_b}
    tp_values = {'whole': tp_w, 'good': tp_g, 'bad': tp_b}
    prevalence_values = {'whole': prevalence_w, 'good': prevalence_g, 'bad': prevalence_b}

    return fn_values, fp_values, tn_values, tp_values, prevalence_values

def plot_balanced_error_trends(hours_list, fn_values, fp_values, tn_values, tp_values, prevalence_values):
    """
    Visualizes the evolution of False Negatives (FN), False Positives (FP), 
    True Negatives (TN), and True Positives (TP) over time using stacked area plots.

    Args:
        hours_list (list): List of hour thresholds.
        fn_values (dict): Dictionary of FN values for each subgroup.
        fp_values (dict): Dictionary of FP values for each subgroup.
        tn_values (dict): Dictionary of TN values for each subgroup.
        tp_values (dict): Dictionary of TP values for each subgroup.
        prevalence_values (dict): Dictionary containing prevalence values.

    Returns:
        None. Displays the plots.
    """
    days_list = [h / 24 for h in hours_list]  # Convert hours to days

    # Ensure data consistency
    for key in fn_values.keys():
        assert len(days_list) == len(fn_values[key]) == len(fp_values[key]) == len(tn_values[key]) == len(tp_values[key]), \
            f"Mismatch in data length for {key}"

    # Normalize values to account for class imbalance
    def balance_values(values, prevalence_dict, invert=False):
        return {key: np.array(vals) / (1 - np.array(prevalence_dict[key]) if invert else np.array(prevalence_dict[key])) 
                for key, vals in values.items()}

    #fn_values = balance_values(fn_values, prevalence_values)
    #fp_values = balance_values(fp_values, prevalence_values, invert=True)
    #tn_values = balance_values(tn_values, prevalence_values, invert=True)
    #tp_values = balance_values(tp_values, prevalence_values)

    # Define color scheme
    colors = {"FN": "#E74C3C", "FP": "#F39C12", "TN": "#2ECC71", "TP": "#3498DB"}
    subgroups = ["Whole dataset", "High confidence subgroup", "Low confidence subgroup"]

    # Create a figure with subplots
    fig, axes = plt.subplots(1, 3, figsize=(18, 6), sharey=True)

    for ax, key in zip(axes, subgroups):
        ax.stackplot(days_list, fn_values[key], fp_values[key], tn_values[key], tp_values[key],
                     labels=["False Negatives", "False Positives", "True Negatives", "True Positives"],
                     colors=[colors["FN"], colors["FP"], colors["TN"], colors["TP"]], alpha=0.8)

        ax.set_title(f"{key}")
        ax.set_xlabel("Timeframe (Days)")
        ax.set_xticks(days_list)
        ax.grid(True, linestyle="--", alpha=0.5)
    # Add shared y-axis label
    axes[0].set_ylabel("Balanced Values")

    # Move suptitle slightly closer to the legend
    plt.suptitle("Confusion Matrix Trends Over Time", fontsize=14, y=1.07)

    # Move legend closer to subtitle, reducing empty space
    fig.legend(["False Negatives", "False Positives", "True Negatives", "True Positives"], loc="upper center", 
            bbox_to_anchor=(0.5, 1.03), ncol=4)
    # Adjust layout and show
    plt.tight_layout(rect=[0, 0, 1, 0.95])
    #plt.suptitle("Confusion Matrix Trends Over Time", fontsize=14)
    plt.show()

