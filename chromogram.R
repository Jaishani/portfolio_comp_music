remotes::install_github('jaburgoyne/compmus')
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(compmus)

 wecaadone<-
  get_tidy_audio_analysis("6rb3wFQ66EWR7DcPG0oEE1") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)
 WecaadoneChart <- wecaadone|>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c() 


 fetakuti<-
   get_tidy_audio_analysis("2oWex6rYoQ7Bl2D9zG8d3F") |>
   select(segments) |>
   unnest(segments) |>
   select(start, duration, pitches)
FetakuliChart <- fetakuti|>
   mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
   compmus_gather_chroma() |> 
   ggplot(
     aes(
       x = start + duration / 2,
       width = duration,
       y = pitch_class,
       fill = value
     )
   ) +
   geom_tile() +
   labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
   theme_minimal() +
   scale_fill_viridis_c() 
 