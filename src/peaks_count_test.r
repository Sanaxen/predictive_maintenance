options(digits.secs=3)
# �f�[�^�̍쐬
set.seed(42)
x <- 1:100
y <- 2 * sin(0.1 * x) + rnorm(100, sd = 0.2)  # �W�O�U�O�����f�[�^

# �f�[�^�̃v���b�g
plot(x, y, main = "�W�O�U�O�����f�[�^", xlab = "x", ylab = "y")

# LOESS��p�����f�[�^�̕�����
span <- 0.2  # �X�p���p�����[�^�̐ݒ�
loess_model <- loess(y ~ x, span = span)
#loess_model <- loess(y ~ x)

# ���������ꂽ�f�[�^�̗\���l���擾
y_smooth <- predict(loess_model, x)

# ���������ꂽ�f�[�^�̃v���b�g
lines(x, y_smooth, col = "blue", lwd = 2)

# �s�[�N�i���[�J���ő�l�j�̌��o
find_peaks <- function(y) {
  peaks <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] > y[i - 1] && y[i] > y[i + 1]) {
      peaks <- c(peaks, i)
    }
  }
  return(peaks)
}

# �g���t�i���[�J���ŏ��l�j�̌��o
find_troughs <- function(y) {
  troughs <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] < y[i - 1] && y[i] < y[i + 1]) {
      troughs <- c(troughs, i)
    }
  }
  return(troughs)
}

# �s�[�N�ƃg���t�̌��o
peaks <- find_peaks(y_smooth)
troughs <- find_troughs(y_smooth)

peaks_count <- function(x, y)
{
	# LOESS��p�����f�[�^�̕�����
	span <- 0.2  # �X�p���p�����[�^�̐ݒ�
	loess_model <- loess(y ~ x, span = span)

	# ���������ꂽ�f�[�^�̗\���l���擾
	y_smooth <- predict(loess_model, x)
	
	peaks <- find_peaks(y_smooth)
	troughs <- find_troughs(y_smooth)

	num_peaks <- length(peaks)
	num_troughs <- length(troughs)

	return( c(num_peaks,num_troughs))
}


# �s�[�N�ƃg���t�̃v���b�g
points(x[peaks], y_smooth[peaks], col = "red", pch = 19)
points(x[troughs], y_smooth[troughs], col = "blue", pch = 19)

# �s�[�N�ƃg���t�̐���\��
num_peaks <- length(peaks)
num_troughs <- length(troughs)
print(paste("�s�[�N�̐�:", num_peaks))
print(paste("�g���t�̐�:", num_troughs))

z <- peaks_count(x, y)
peaks <- z[1]
troughs <- z[2]
print(paste("�s�[�N�̐�:", num_peaks))
print(paste("�g���t�̐�:", num_troughs))

