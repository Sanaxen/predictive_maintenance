# データの作成
set.seed(42)
x <- 1:100
y <- 2 * sin(0.1 * x) + rnorm(100, sd = 0.2)  # ジグザグしたデータ

# データのプロット
plot(x, y, main = "ジグザグしたデータ", xlab = "x", ylab = "y")

# LOESSを用いたデータの平滑化
span <- 0.2  # スパンパラメータの設定
loess_model <- loess(y ~ x, span = span)
#loess_model <- loess(y ~ x)

# 平滑化されたデータの予測値を取得
y_smooth <- predict(loess_model, x)

# 平滑化されたデータのプロット
lines(x, y_smooth, col = "blue", lwd = 2)

# ピーク（ローカル最大値）の検出
find_peaks <- function(y) {
  peaks <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] > y[i - 1] && y[i] > y[i + 1]) {
      peaks <- c(peaks, i)
    }
  }
  return(peaks)
}

# トラフ（ローカル最小値）の検出
find_troughs <- function(y) {
  troughs <- c()
  for (i in 2:(length(y) - 1)) {
    if (y[i] < y[i - 1] && y[i] < y[i + 1]) {
      troughs <- c(troughs, i)
    }
  }
  return(troughs)
}

# ピークとトラフの検出
peaks <- find_peaks(y_smooth)
troughs <- find_troughs(y_smooth)

peaks_count <- function(x, y)
{
	# LOESSを用いたデータの平滑化
	span <- 0.2  # スパンパラメータの設定
	loess_model <- loess(y ~ x, span = span)

	# 平滑化されたデータの予測値を取得
	y_smooth <- predict(loess_model, x)
	
	peaks <- find_peaks(y_smooth)
	troughs <- find_troughs(y_smooth)

	num_peaks <- length(peaks)
	num_troughs <- length(troughs)

	return( c(num_peaks,num_troughs))
}


# ピークとトラフのプロット
points(x[peaks], y_smooth[peaks], col = "red", pch = 19)
points(x[troughs], y_smooth[troughs], col = "blue", pch = 19)

# ピークとトラフの数を表示
num_peaks <- length(peaks)
num_troughs <- length(troughs)
print(paste("ピークの数:", num_peaks))
print(paste("トラフの数:", num_troughs))

z <- peaks_count(x, y)
peaks <- z[1]
troughs <- z[2]
print(paste("ピークの数:", num_peaks))
print(paste("トラフの数:", num_troughs))

