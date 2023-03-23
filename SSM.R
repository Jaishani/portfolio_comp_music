library(tidyverse)
library(spotifyr)
library(compmus)
library(ggplot2)

chartSSM_famous <-
  get_tidy_audio_analysis("19a3JfW8BQwqHWUMbcqSx8") |> 
  compmus_align(bars, segments) |>                    
  select(bars) |>                                     
  unnest(bars) |>                                 
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"         
      )
  )
chartSSM_famous |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()



chartSSM_famous |>
  compmus_self_similarity(pitches, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")



 chartSSM_famous<- ggplot(chartSSM_famous, aes(x = xstart + xduration / 2,width = xduration,y = ystart + yduration / 2,height = yduration,fill = d)) + 
  geom_tile() + 
  coord_fixed() + 
  scale_fill_viridis_c(guide = "none") + 
  theme_classic() + labs(x = "", y = "")
