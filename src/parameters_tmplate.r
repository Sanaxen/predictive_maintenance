csv_encoding = 'utf-8'
base_name = ""

#��������spline
use_spline = FALSE

#���̓f�[�^�̕���\
unit_of_time = "sec"
unit_of_record = (1/585936)*60*60*24

#�����ʕ�����
feature_smooth_window = 0

#��x�ɑ����Ă���f�[�^��
one_input = 24*60*60
#585936

#���̓f�[�^������
smooth_window = 0
smooth_window_slide = 0
#��������lowess�ōs��
use_lowess = TRUE

#�\���ɗp����ő�f�[�^��
#max_data_len = 864000


#�P������
max_train_span = 11*30*24*60*60
#�����Ă���f�[�^�̍ő�ێ�����
max_retained_length = 3*30*24*60*60

###########################################################
#       �ȉ��͈ړ����ό�̐��l(���̓f�[�^��������)
###########################################################
sampling_num <- 30

#���͊e�ϐ��̓����ʁi���ρA���U�Aetc)������ʂɂ���ꍇ��lookback��
lookback=24*60*60
lookback_slide = 24*60*60

#�����ʕ�����
smooth_window2 = 12
smooth_window_slide2 = 1


#�\�����f���P���Ɏg�����O�̓_��
train_num = -60
#monotonicity�v�Z�Ɏg�����O�̓_��
monotonicity_num = 60


#�f�t�H���g��臒l
threshold = -1000

#plot�pYmax
ymax = -10000
ymin =  10000

#�e�����ʖ���臒l�AYmax�̃p�����[�^�Z�b�g
feature_param = NULL

#�\�����関���̒���臒l
max_prediction_length = 30*2



#�\�����ꂽ���݂��瑪�肵���ُ픭������
failure_time_init = max_prediction_length*unit_of_record
failure_time = failure_time_init

#�o�͎��̎��ԒP��
forecast_time_unit = "h"

#�ُ�x���f��
m_mahalanobis <- NULL

#�����Ă���f�[�^�̑S�Ăŉߋ����猻�݂܂�
#�\���ɗp����ő�f�[�^���܂łɐ��������f�[�^�t���[��
pre = NULL
pre_org = NULL
#�ő�ێ����܂łɐ��������f�[�^�t���[��
past = NULL

#�\�����f���I��
use_auto_arima = F
use_arima = T
use_ets = F
use_plophet = F


#�ُ���܂񂾃f�[�^��input�ł����ꍇ=TRUE
abnormality_detected_data <- TRUE

#�ǐՂ��Ă���������
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
