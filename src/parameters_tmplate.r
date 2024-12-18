csv_encoding = 'utf-8'
base_name = ""

#平滑化でspline
use_spline = FALSE

#入力データの分解能
unit_of_time = "sec"
unit_of_record = (1/585936)*60*60*24

#特徴量平滑化
feature_smooth_window = 0

#一度に送られてくるデータ長
one_input = 24*60*60
#585936

#入力データ平滑化
smooth_window = 0
smooth_window_slide = 0
#平滑化をlowessで行う
use_lowess = TRUE

#予測に用いる最大データ長
#max_data_len = 864000


#訓練期間
max_train_span = 11*30*24*60*60
#送られてくるデータの最大保持長さ
max_retained_length = 3*30*24*60*60

###########################################################
#       以下は移動平均後の数値(入力データ平滑化後)
###########################################################
sampling_num <- 30

#入力各変数の特徴量（平均、分散、etc)を特徴量にする場合のlookback数
lookback=24*60*60
lookback_slide = 24*60*60

#特徴量平滑化
smooth_window2 = 12
smooth_window_slide2 = 1


#予測モデル訓練に使う直前の点数
train_num = -60
#monotonicity計算に使う直前の点数
monotonicity_num = 60


#デフォルトの閾値
threshold = -1000

#plot用Ymax
ymax = -10000
ymin =  10000

#各特徴量毎の閾値、Ymaxのパラメータセット
feature_param = NULL

#予測する未来の長さ閾値
max_prediction_length = 30*2



#予測された現在から測定した異常発生時間
failure_time_init = max_prediction_length*unit_of_record
failure_time = failure_time_init

#出力時の時間単位
forecast_time_unit = "h"

#異常度モデル
m_mahalanobis <- NULL

#送られてくるデータの全てで過去から現在まで
#予測に用いる最大データ長までに制限したデータフレーム
pre = NULL
pre_org = NULL
#最大保持長までに制限したデータフレーム
past = NULL

#予測モデル選択
use_auto_arima = F
use_arima = T
use_ets = F
use_plophet = F


#異常を含んだデータがinputできた場合=TRUE
abnormality_detected_data <- TRUE

#追跡していく特徴量
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
