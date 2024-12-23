options(digits.secs=3)
csv_encoding = 'utf-8'
base_name = ""

#spline with smoothing
use_spline = FALSE

#Resolution of input data
unit_of_time = "sec"
unit_of_record = (1/585936)*60*60*24

#Feature Smoothing
feature_smooth_window = 0

#Length of data sent at a time
one_input = 24*60*60
#585936

#Input data smoothing
smooth_window = 0
smooth_window_slide = 0
#Smoothing is done LOWESS.
use_lowess = TRUE

#Maximum data length for prediction
#max_data_len = 864000


#Training period
max_train_span = 11*30*24*60*60
#Maximum length of data to be sent and retained
max_retained_length = 3*30*24*60*60

###########################################################
#       The following are the values after moving average (after input data smoothing)
###########################################################
sampling_num <- 30

#Number of lookbacks for features (mean, variance, etc.) for each input variable
lookback=24*60*60
lookback_slide = 24*60*60

#Feature Smoothing
smooth_window2 = 12
smooth_window_slide2 = 1


#Scores just prior to use for predictive model training
train_num = -60
#The number of points immediately before the monotonicity calculation
monotonicity_num = 60


#Default Threshold
threshold = -1000

#plot Ymax
ymax = -10000
ymin =  10000

#Parameter set of threshold and Ymax for each feature
feature_param = NULL

#Predicted future length threshold
max_prediction_length = 30*2



#Time of anomaly occurrence measured from the predicted present
failure_time_init = max_prediction_length*unit_of_record
failure_time = failure_time_init

#Time unit at output
forecast_time_unit = "h"

#anomaly model
m_mahalanobis <- NULL

#Past to present in all data sent
#Data frames limited to the maximum data length used for forecasting
pre = NULL
pre_org = NULL
#Data frames limited to the maximum retention length
past = NULL

#Predictive Model Selection
use_auto_arima = F
use_arima = T
use_ets = F
use_plophet = F


#If data containing anomalies can be input = TRUE
abnormality_detected_data <- TRUE

#Features to be tracked
tracking_feature <- NULL
dynamic_threshold = TRUE
watch_name = "X793.."

RUL <- c()
pre = NULL
past = NULL
feature_param = NULL

index_number <- 0
time_Index <- 1

timeStamp <- ""
save.image("./predictive_maintenance.RData")
