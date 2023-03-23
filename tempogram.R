library(compmus)
library(ggplot2)
kendrick <- get_tidy_audio_analysis("5Gt9bxniM1SxN61yRzRhXL")
chart_kendrick <- kendrick |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
