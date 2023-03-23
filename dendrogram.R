install.packages("tidymodels")
install.packages("ggdendro")
install.packages("heatmaply")

library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
  
  halloween <-
    get_playlist_audio_features("bnfcollection", "1vsoLSK3ArkpaIHmUaF02C") |>
    add_audio_analysis() |>
    mutate(
      segments = map2(segments, key, compmus_c_transpose),
      pitches =
        map(segments,
            compmus_summarise, pitches,
            method = "mean", norm = "manhattan"
        ),
      timbre =
        map(
          segments,
          compmus_summarise, timbre,
          method = "mean"
        )
    ) |>
    mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
    mutate_at(vars(pitches, timbre), map, bind_rows) |>
    unnest(cols = c(pitches, timbre))
}  
halloween_dist <- dist(halloween_juice, method = "euclidean")
halloween_dist |> 
  hclust(method = "single") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()

heatmaply(
  halloween_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)
