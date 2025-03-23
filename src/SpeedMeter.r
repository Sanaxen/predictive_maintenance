library(ggplot2)
library(cowplot)
library(grid)

SpeedMeter <- function(base_plot, current_speed = 70, title_text = "-SpeedoMeter-", x = 0.1, y = 0.1, width = 0.1, height = 0.1)
{
	min_speed <- 0
	max_speed <- 100
	r <- 1 

	needle_angle <- pi - (current_speed - min_speed) / (max_speed - min_speed) * pi

	n_segments <- 100
	theta <- seq(pi, 0, length.out = n_segments + 1)

	df_segments <- do.call(rbind, lapply(1:n_segments, function(i) {
	  data.frame(
	    x = c(0, r * cos(theta[i]), r * cos(theta[i+1])),
	    y = c(0, r * sin(theta[i]), r * sin(theta[i+1])),
	    group = i,
	    mid_angle = (theta[i] + theta[i+1]) / 2
	  )
	}))

	df_segments$norm <- (pi - df_segments$mid_angle) / pi

	#pal <- colorRampPalette(c("green", "yellow", "orange", "red"))
	pal <- colorRampPalette(c("#3399FF", "green",  "yellow", "orange", "red","#9900FF"))
	segment_colors <- pal(n_segments)

	group_colors <- sapply(1:n_segments, function(i) {
	  norm_val <- mean(df_segments$norm[df_segments$group == i])
	  col_index <- round(norm_val * (n_segments - 1)) + 1
	  segment_colors[col_index]
	})
	df_segments$fill_color <- group_colors[df_segments$group]
	
	#df_segments$fill_color_mod <- ifelse(df_segments$mid_angle < needle_angle,
	#                                       alpha(df_segments$fill_color, 0.1),
	#                                       df_segments$fill_color)
	df_segments$fill_color_mod <- ifelse(df_segments$mid_angle < max(0,needle_angle-0.6) |df_segments$mid_angle >  min(pi,needle_angle+0.6),
                                     "#F5F5F5", df_segments$fill_color)
	p <- ggplot() +
	  geom_polygon(data = df_segments, aes(x = x, y = y, group = group, fill = fill_color_mod), color = NA, alpha=1.0) +
	  scale_fill_identity() +
	  geom_path(data = data.frame(x = r * cos(theta), y = r * sin(theta)),
	            aes(x = x, y = y), color = "#888888") +
	  geom_segment(aes(x = 0, y = 0, 
	                   xend = r * cos(needle_angle), yend = r * sin(needle_angle)),
	               arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "#6633FF", alpha=0.5) +
	  geom_point(aes(x = 0, y = 0), size = 2) +
	  coord_fixed() +
	  theme_void() +
	  ggtitle(title_text)
	  

	  final_plot <- ggdraw(base_plot) +
	  draw_plot(p, x = x, y = y, width = width, height = height)
	  
	return(final_plot)
}

if ( F )
{
	base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
	  geom_point() +
	  ggtitle("Base Plot")

	p <- SpeedMeter(base_plot, x = 0.1, y = 0.1, width = 0.1, height = 0.1)

	print(p)
}

