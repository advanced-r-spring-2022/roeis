## code to prepare `notes` dataset goes here

## Note frequencies from here: https://pages.mtu.edu/~suits/notefreqs.html

## Example code from here: https://stackoverflow.com/questions/32857065/is-it-possible-to-code-music-in-r-and-play-it-back-mac-os-x

notes <- read.csv(file = "./notes.csv")
notes$wave <- vector(mode = "list", length = nrow(notes))
for (i in seq_len(nrow(notes))) {
  f <- notes$frequency[[i]]
  sr <- 8000
  bits <- 16
  secs <- 0.5
  amp <- 1
  t <- seq(0, secs, 1/sr)
  y <-  amp * sin(2 * pi * f * t)
  notes$wave[[i]] <- floor(2 ^ (bits - 2) * y)
}


usethis::use_data(notes, internal = TRUE, overwrite = TRUE)
