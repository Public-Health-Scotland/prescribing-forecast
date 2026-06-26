## Machine Learning models for dissertation
## Liam Rooney
## Last Updated: 09/06/2026

# 1. Import packages
import pandas as pd
import numpy as np
import glob
import time

from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import f_regression
import xgboost as xgb
from lightgbm import LGBMRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from plotly.offline import init_notebook_mode, iplot
import plotly.graph_objs as go
from plotly import tools
#init_notebook_mode(connected=True)
from sklearn.metrics import mean_squared_error

# Set working directory
import os

os.chdir("/PHI_conf/PrescribingBCS/Topics/Budgets/Phasings/Development/prescribing-forecast")
print(f"Changed working directory to: {os.getcwd()}")

# 2. Read data in
# Read in data and filter for NHS A&A
df = pd.read_csv("shiny/forecasts/data/Historical Data.csv", thousands=',')

# Read vector of healthboards in
healthboards = ["NHS AYRSHIRE & ARRAN", "NHS BORDERS", "NHS DUMFRIES & GALLOWAY", "NHS FIFE","NHS FORTH VALLEY", "NHS GRAMPIAN", "NHS GREATER GLASGOW & CLYDE", "NHS HIGHLAND", "NHS LANARKSHIRE", "NHS LOTHIAN", "NHS TAYSIDE", "NHS WESTERN ISLES", "NHS ORKNEY", "NHS SHETLAND", "SCOTLAND"]

df = df[df['Presc Health Board Name'].isin(healthboards)]

#df = df[df['Presc Health Board Name'] == 'NHS AYRSHIRE & ARRAN']

# Check filter has worked
df.head()

# Drop board column
#del df['Disp Health Board Name']

#df.head()

# Check for missing values
df.isnull().sum()

# Check datatype of each column
df.info()

df['time'] = pd.to_datetime(df['Paid Date'])
df['year'] = df.time.dt.year
df['month'] = df.time.dt.month
df.drop('Paid Date', axis = 1, inplace = True)

# Move time column to front
col = df.pop('time')
df.insert(0, 'time', col)

df.head()

# # Sort the df using the paid date column and then drop it
# df = df.sort_values(by=['Presc Health Board Name', 'time'])

# df.head()

# #del df['time']
# df.head()

# items = df.drop(columns = ['Claim PD Paid GIC excl. BB'])
# cost = df.drop(columns = ['Claim PD Number of Paid Items'])

# # We will add lags for the previous month, previous end of quarter (3 months ago), and previous year (same month)
# items['lag1'] = df.groupby('Presc Health Board Name')['Claim PD Number of Paid Items'].shift(1)
# items['lag3'] = df.groupby('Presc Health Board Name')['Claim PD Number of Paid Items'].shift(3)
# items['lag12'] = df.groupby('Presc Health Board Name')['Claim PD Number of Paid Items'].shift(12)

# cost['lag1'] = df.groupby('Presc Health Board Name')['Claim PD Paid GIC excl. BB'].shift(1)
# cost['lag3'] = df.groupby('Presc Health Board Name')['Claim PD Paid GIC excl. BB'].shift(3)
# cost['lag12'] = df.groupby('Presc Health Board Name')['Claim PD Paid GIC excl. BB'].shift(12)

# items[:13]
# cost[:13]

# # Drop NAs to create feature scores
# items = items.dropna()
# cost = cost.dropna()

# x_items = items.drop(columns = ['Claim PD Number of Paid Items'])
# y_items = items.iloc[:, 0]

# # Use SelectKBest to create feature significance scores
# st=time.time()
# bestfeatures = SelectKBest(score_func=f_regression)
# fit = bestfeatures.fit(x_items,y_items)
# et=time.time()-st
# print(et)
# dfscores = pd.DataFrame(fit.scores_)
# dfcolumns = pd.DataFrame(x_items.columns)
# featureScores = pd.concat([dfcolumns,dfscores],axis=1)
# featureScores.columns = ['Featuress','Score']
# best_features=featureScores.nlargest(5,'Score')
# best_features

# import time
# import pandas as pd
# from sklearn.feature_selection import SelectKBest, f_regression

# # sort first to keep everything sensible for time-series derived features
# df = df.sort_values(['Presc Health Board Name', 'time']).copy()

# # define columns
# board_col = 'Presc Health Board Name'
# target_col = 'Claim PD Number of Paid Items'

# # columns you do NOT want used as predictors
# exclude_cols = [board_col, 'time', target_col]

# all_scores = []

# st = time.time()

# for board, board_df in df.groupby(board_col):
    
#     # create X and y for this board only
#     X = board_df.drop(columns=exclude_cols, errors='ignore').copy()
#     y = board_df[target_col].copy()
    
#     # remove rows with missing values
#     # (important if you've created lag variables, because first rows will often be NaN)
#     valid_idx = X.notna().all(axis=1) & y.notna()
#     X = X.loc[valid_idx]
#     y = y.loc[valid_idx]
    
#     # skip boards with too few rows
#     if len(X) < 2:
#         continue
    
#     # score all features
#     selector = SelectKBest(score_func=f_regression, k='all')
#     selector.fit(X, y)
    
#     # store results for this board
#     board_scores = pd.DataFrame({
#         'Board': board,
#         'Feature': X.columns,
#         'Score': selector.scores_
#     })
    
#     # optional ranking within each board
#     board_scores = board_scores.sort_values('Score', ascending=False)
#     board_scores['Rank'] = range(1, len(board_scores) + 1)
#     board_scores['Top5'] = board_scores['Rank'] <= 5
    
#     all_scores.append(board_scores)

# et = time.time() - st
# print(f"Elapsed time: {et:.2f} seconds")

# # combine all boards into one table
# feature_scores_all_boards = pd.concat(all_scores, ignore_index=True)

# # top 5 features per board only
# best_features_by_board = feature_scores_all_boards[feature_scores_all_boards['Top5']].copy()

# print(feature_scores_all_boards.head())
# print(best_features_by_board.head())





import time
import pandas as pd
from sklearn.feature_selection import SelectKBest, f_regression


def prepare_modelling_data(
    df,
    board_col='Presc Health Board Name',
    time_col='time',
    target_cols=None,
    lag_source_cols=None,
    lags=(1, 3, 6, 12),
    top_n=5
):
    """
    Prepare board-level modelling datasets for multiple targets.
    
    Steps:
    1. Sort by board and time
    2. Create lag features within each board
    3. Loop through each board and target
    4. Run SelectKBest feature scoring
    5. Create 70/20/10 train/test/validation split
    6. Return feature scores + split datasets
    
    Parameters
    ----------
    df : pandas.DataFrame
        Input dataframe
    board_col : str
        Column containing board names
    time_col : str
        Column containing time/date
    target_cols : list of str
        Target columns to model
    lag_source_cols : list of str
        Columns to create lag features from
    lags : iterable
        Lag periods to create
    top_n : int
        Number of top features to keep per board/target
        
    Returns
    -------
    feature_scores_all : DataFrame
        All feature scores for all boards and targets
    best_features : DataFrame
        Top N features for each board and target
    splits : dict
        Dictionary containing X/y train/test/validation splits
    """

    df = df.copy()

    if target_cols is None:
        target_cols = [
            'Claim PD Number of Paid Items',
            'Claim PD Paid GIC excl. BB'
        ]

    if lag_source_cols is None:
        lag_source_cols = target_cols

    # ---------------------------------------------------
    # 1. Sort correctly for time-series work
    # ---------------------------------------------------
    df = df.sort_values(by=[board_col, time_col]).reset_index(drop=True)

    # ---------------------------------------------------
    # 2. Create lag features within each board
    # ---------------------------------------------------
    for col in lag_source_cols:
        for lag in lags:
            lag_name = f"{col}_lag{lag}"
            df[lag_name] = df.groupby(board_col)[col].shift(lag)

    df = df[df['time'] > '2012-04-01']

    # ---------------------------------------------------
    # 3. Loop through board + target
    # ---------------------------------------------------
    all_scores = []
    splits = {}

    st = time.time()

    for board, board_df in df.groupby(board_col):

        board_df = board_df.sort_values(time_col).copy()

        for target_col in target_cols:

            # ---------------------------------------------------
            # Define feature columns
            # Drop board/time/targets from X to avoid leakage
            # ---------------------------------------------------
            drop_cols = [board_col, time_col] + target_cols
            X = board_df.drop(columns=drop_cols, errors='ignore').copy()
            y = board_df[target_col].copy()

            # Remove rows with missing values
            # (needed because lag features create NaNs at start)
            valid_idx = X.notna().all(axis=1) & y.notna()
            X = X.loc[valid_idx].copy()
            y = y.loc[valid_idx].copy()

            # Keep aligned board_df after NA removal
            board_valid = board_df.loc[valid_idx].copy()

            # Skip if too few rows
            if len(X) < 3:
                print(f"Skipping {board} | {target_col} (too few rows after lag removal)")
                continue

            # ---------------------------------------------------
            # 4. Feature scoring with SelectKBest
            # ---------------------------------------------------
            selector = SelectKBest(score_func=f_regression, k='all')
            selector.fit(X, y)

            score_df = pd.DataFrame({
                'Board': board,
                'Target': target_col,
                'Feature': X.columns,
                'Score': selector.scores_
            }).sort_values('Score', ascending=False)

            score_df['Rank'] = range(1, len(score_df) + 1)
            score_df['TopN'] = score_df['Rank'] <= top_n

            all_scores.append(score_df)

            # Top features only
            selected_features = score_df.head(top_n)['Feature'].tolist()

            # ---------------------------------------------------
            # 5. 70 / 20 / 10 split
            # ---------------------------------------------------
            # Use only target + selected features + time ordering data
            modelling_df = board_valid[[time_col, target_col] + selected_features].copy()
            modelling_df = modelling_df.sort_values(time_col).reset_index(drop=True)

            total_rows = len(modelling_df)
            training_rows = round(total_rows * 0.7)

            remaining_rows = total_rows - training_rows
            test_rows = round(remaining_rows * (2/3))
            vali_rows = remaining_rows - test_rows

            # Split in chronological order
            train = modelling_df.head(training_rows).copy()

            test_val = modelling_df.tail(test_rows + vali_rows).copy()
            test = test_val.head(test_rows).copy()
            pred = test_val.tail(vali_rows).copy()

            # Optional check
            if total_rows != len(train) + len(test) + len(pred):
                print(f"Row mismatch in split for {board} | {target_col}")

            # X/y objects
            y_train = train[target_col]
            X_train = train.drop(columns=[target_col, time_col], errors='ignore')

            y_test = test[target_col]
            X_test = test.drop(columns=[target_col, time_col], errors='ignore')

            y_pred = pred[target_col]
            X_pred = pred.drop(columns=[target_col, time_col], errors='ignore')

            # Store
            splits[(board, target_col)] = {
                'selected_features': selected_features,
                'train': train,
                'test': test,
                'pred': pred,
                'X_train': X_train,
                'y_train': y_train,
                'X_test': X_test,
                'y_test': y_test,
                'X_pred': X_pred,
                'y_pred': y_pred,
                'n_rows': total_rows,
                'n_train': len(train),
                'n_test': len(test),
                'n_pred': len(pred)
            }

    et = time.time() - st
    print(f"Pipeline completed in {et:.2f} seconds")

    # ---------------------------------------------------
    # 6. Combine outputs
    # ---------------------------------------------------
    feature_scores_all = pd.concat(all_scores, ignore_index=True)
    best_features = feature_scores_all[feature_scores_all['TopN']].copy()

    return feature_scores_all, best_features, splits


feature_scores_all, best_features, splits = prepare_modelling_data(df)

splits[('NHS AYRSHIRE & ARRAN', 'Claim PD Number of Paid Items')]


import xgboost as xgb
from sklearn.metrics import mean_absolute_error

results = []

for (board, target), data in splits.items():
    
    # Define model (you can move this outside if parameters are constant)
    xg_reg = xgb.XGBRegressor(
        objective='reg:squarederror',
        colsample_bytree=0.3,
        subsample=0.8,
        learning_rate=0.05,
        max_depth=5,
        alpha=1,
        n_estimators=800,
        random_state=42,
        n_jobs=-1
    )
    
    # Fit model
    xg_reg.fit(data['X_train'], data['y_train'])
    
    # Predict
    preds = xg_reg.predict(data['X_test'])
    
    # Evaluate
    mae = mean_absolute_error(data['y_test'], preds)
    
    # Store results
    results.append({
        'Board': board,
        'Target': target,
        'MAE': mae,
        'n_train': data['n_train'],
        'n_test': data['n_test']
    })

# Convert to dataframe
results_df = pd.DataFrame(results)

print(results_df)

# predictions = {}

# for (board, target), data in splits.items():
    
#     xg_reg = xgb.XGBRegressor(
#         objective='reg:squarederror',
#         colsample_bytree=0.3,
#         subsample=0.8,
#         learning_rate=0.05,
#         max_depth=5,
#         alpha=1,
#         n_estimators=800,
#         random_state=42,
#         n_jobs=-1
#     )

#     xg_reg.fit(data['X_train'], data['y_train'])
    
#     preds = xg_reg.predict(data['X_test'])

#     # include timestamps
#     predictions[(board, target)] = pd.DataFrame({
#     'time': data['test']['time'],
#     'Actual': data['y_test'],
#     'Predicted': preds
#     }).reset_index(drop=True)

# all_predictions = []

# for (board, target), data in splits.items():

#     xg_reg = xgb.XGBRegressor(
#         objective='reg:squarederror',
#         colsample_bytree=0.3,
#         subsample=0.8,
#         learning_rate=0.05,
#         max_depth=5,
#         alpha=1,
#         n_estimators=800,
#         random_state=42,
#         n_jobs=-1
#     )

#     xg_reg.fit(data['X_train'], data['y_train'])
    
#     preds = xg_reg.predict(data['X_test'])

#     temp = pd.DataFrame({
#         'time': data['test']['time'],
#         'Actual': data['y_test'],
#         'Predicted': preds,
#         'Board': board,
#         'Target': target
#     }).reset_index(drop=True)

#     all_predictions.append(temp)

# all_predictions_df = pd.concat(all_predictions, ignore_index=True)

# all_predictions_df[
#     (all_predictions_df['Board'] == 'NHS AYRSHIRE & ARRAN') &
#     (all_predictions_df['Target'] == 'Claim PD Number of Paid Items')
# ]

# filtered_df = all_predictions_df[
#     (all_predictions_df['Board'] == 'NHS AYRSHIRE & ARRAN') &
#     (all_predictions_df['Target'] == 'Claim PD Number of Paid Items')
# ]

import copy
import pandas as pd
import xgboost as xgb
from lightgbm import LGBMRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error

results = []
all_predictions = []

# Define models
models = {
    'XGBoost': xgb.XGBRegressor(
        objective='reg:squarederror',
        colsample_bytree=0.8,
        subsample=0.8,
        learning_rate=0.05,
        max_depth=6,
        alpha=0.5,
        n_estimators=800,
        random_state=42,
        n_jobs=-1
    ),
    'LightGBM': LGBMRegressor(
        n_estimators=500,
        learning_rate=0.05,
        max_depth=-1,
        num_leaves=31,
        min_data_in_leaf=20,
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=42,
        n_jobs=-1
    ),
    'RandomForest': RandomForestRegressor(
        n_estimators=300,
        max_depth=None,
        min_samples_leaf=5,
        random_state=42,
        n_jobs=-1
    )
}

def custom_mape(y_true, y_pred):
    y_true = np.array(y_true)
    y_pred = np.array(y_pred)
    
    mask = y_true != 0  # avoid division by zero
    
    error = (np.abs(y_true[mask] - y_pred[mask]) / y_true[mask]) * 100
    return np.mean(error)


for model_name, model in models.items():

    print(f"\nRunning model: {model_name}")

    for (board, target), data in splits.items():

        m = copy.deepcopy(model)

        # Fit
        m.fit(data['X_train'], data['y_train'])

        # Predict
        test_preds = m.predict(data['X_test'])
        pred_preds = m.predict(data['X_pred'])

        # Combine safely for evaluation
        combined_df = pd.concat([
            pd.DataFrame({'y_true': data['y_test'], 'y_pred': test_preds}),
            pd.DataFrame({'y_true': data['y_pred'], 'y_pred': pred_preds})
        ])

        mape = custom_mape(combined_df['y_true'], combined_df['y_pred'])

        results.append({
            'Model': model_name,
            'Board': board,
            'Target': target,
            'MAPE': mape,
            'n_obs': len(combined_df)
        })

        # Predictions for plotting
        test_df = pd.DataFrame({
            'time': data['test']['time'],
            'Actual': data['y_test'],
            'Predicted': test_preds
        })

        pred_df = pd.DataFrame({
            'time': data['pred']['time'],
            'Actual': data['y_pred'],
            'Predicted': pred_preds
        })

        temp = pd.concat([test_df, pred_df]).sort_values('time')

        temp['Board'] = board
        temp['Target'] = target
        temp['Model'] = model_name

        all_predictions.append(temp)

# Combine outputs
results_df = pd.DataFrame(results)
all_predictions_df = pd.concat(all_predictions, ignore_index=True)

print(results_df.head())
print(all_predictions_df.head())

print(models)

with pd.ExcelWriter("model_outputs_UPDATED.xlsx", engine='openpyxl') as writer:
    results_df.to_excel(writer, sheet_name="Model_Results", index=False)
    all_predictions_df.to_excel(writer, sheet_name="Predictions", index=False)
    feature_scores_all.to_excel(writer, sheet_name="Feature_Scores", index=False)
    best_features.to_excel(writer, sheet_name="Top_Features", index=False)


import seaborn as sns

sns.lineplot(
    data=filtered_df,
    x='time',
    y='Predicted',
    hue='Board'
)

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import pandas as pd
import seaborn as sns

# Expects a dataframe like all_predictions_df with columns:
# time, Actual, Predicted, Board, Target

def plot_dashboard_style(
    df,
    board=None,
    target=None,
    date_col='time',
    actual_col='Actual',
    pred_col='Predicted'
):
    data = df.copy()
    data[date_col] = pd.to_datetime(data[date_col])

    if board is not None:
        data = data[data['Board'] == board].copy()
    if target is not None:
        data = data[data['Target'] == target].copy()

    data = data.sort_values(date_col)

    # Error metrics
    data['Error'] = data[actual_col] - data[pred_col]
    data['AbsError'] = data['Error'].abs()
    data['APE'] = (data['AbsError'] / data[actual_col].replace(0, pd.NA)) * 100

    mae = data['AbsError'].mean()
    mape = data['APE'].dropna().mean()
    bias = data['Error'].mean()

    sns.set_theme(style='whitegrid')

    fig = plt.figure(figsize=(16, 9), constrained_layout=True)
    gs = fig.add_gridspec(3, 4)

    ax_main = fig.add_subplot(gs[0:2, 0:3])
    ax_err = fig.add_subplot(gs[2, 0:3], sharex=ax_main)
    ax_kpi1 = fig.add_subplot(gs[0, 3])
    ax_kpi2 = fig.add_subplot(gs[1, 3])
    ax_kpi3 = fig.add_subplot(gs[2, 3])

    # Main chart
    ax_main.plot(
        data[date_col],
        data[actual_col],
        label='Actual',
        linewidth=2.5,
        color='#1f77b4'
    )
    ax_main.plot(
        data[date_col],
        data[pred_col],
        label='Predicted',
        linewidth=2.5,
        color='#ff7f0e',
        linestyle='--'
    )

    ax_main.fill_between(
        data[date_col],
        data[actual_col],
        data[pred_col],
        color='#d9d9d9',
        alpha=0.3
    )

    title_board = board if board else 'All Boards'
    title_target = target if target else 'All Targets'

    ax_main.set_title(
        f'Actual vs Predicted | {title_board} | {title_target}',
        fontsize=16,
        weight='bold'
    )
    ax_main.set_ylabel('Value', fontsize=12)
    ax_main.legend(frameon=False, loc='upper left')
    ax_main.spines[['top', 'right']].set_visible(False)

    # Error chart
    colours = ['#2ca02c' if x >= 0 else '#d62728' for x in data['Error']]
    ax_err.bar(data[date_col], data['Error'], color=colours, width=20)
    ax_err.axhline(0, color='black', linewidth=1)
    ax_err.set_title('Prediction Error (Actual - Predicted)', fontsize=12, weight='bold')
    ax_err.set_ylabel('Error', fontsize=11)
    ax_err.set_xlabel('Time', fontsize=12)
    ax_err.spines[['top', 'right']].set_visible(False)

    # Tidy date formatting
    locator = mdates.AutoDateLocator()
    formatter = mdates.ConciseDateFormatter(locator)
    ax_err.xaxis.set_major_locator(locator)
    ax_err.xaxis.set_major_formatter(formatter)

    # KPI cards
    for ax in [ax_kpi1, ax_kpi2, ax_kpi3]:
        ax.set_xticks([])
        ax.set_yticks([])
        for spine in ax.spines.values():
            spine.set_visible(False)
        ax.set_facecolor('#f7f7f7')

    ax_kpi1.text(0.5, 0.62, 'MAE', ha='center', va='center', fontsize=13, weight='bold')
    ax_kpi1.text(0.5, 0.35, f'{mae:,.2f}', ha='center', va='center', fontsize=22, color='#1f77b4')

    ax_kpi2.text(0.5, 0.62, 'MAPE', ha='center', va='center', fontsize=13, weight='bold')
    ax_kpi2.text(0.5, 0.35, f'{mape:,.2f}%', ha='center', va='center', fontsize=22, color='#ff7f0e')

    ax_kpi3.text(0.5, 0.62, 'Mean Bias', ha='center', va='center', fontsize=13, weight='bold')
    ax_kpi3.text(
        0.5, 0.35, f'{bias:,.2f}',
        ha='center', va='center', fontsize=22,
        color='#2ca02c' if bias >= 0 else '#d62728'
    )

    return fig



fig = plot_dashboard_style(
    all_predictions_df,
    board='NHS AYRSHIRE & ARRAN',
    target='Claim PD Number of Paid Items'
)

plt.show()