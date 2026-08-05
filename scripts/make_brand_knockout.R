# =============================================================================
# make_brand_knockout.R
#
# Derives www/brand/PARC_white_icon_knockout.png from www/brand/PARC_blue_icon.png.
#
# Why this script exists
#   The brand folder shipped four hand-drawn marks. Three are used as supplied.
#   The fourth need, a white glyph on transparency for dark grounds, had no
#   file: PARC_white_icon.png is a white TILE with a black glyph, drawn for
#   light grounds, and on the navy fact sheet banner it reads as a sticker
#   beside the wordmark's clean knockout. Rather than leave a generated asset
#   in the repo with no provenance, this script regenerates it from source on
#   demand, so the mark in www/brand is reproducible rather than a one-off.
#
#   If the ADA centres approve a designer-drawn knockout, delete this script
#   and drop their file in under the same name. This is a stopgap, not a
#   preferred pipeline.
#
# Why the blue mark is the source, not the white one
#   PARC_blue_icon.png is a navy tile with a WHITE glyph, so the glyph already
#   IS the knockout shape and its antialiased edge is a clean white-to-navy
#   ramp. Unmixing that ramp recovers a true alpha channel. PARC_white_icon.png
#   has a BLACK glyph on white, so it would have to be inverted, and inverting
#   a black-on-white antialias fringe leaves grey halos that show against navy.
#
# Usage
#   Rscript scripts/make_brand_knockout.R
# =============================================================================

if (!requireNamespace("png", quietly = TRUE)) {
  stop("Package 'png' is required. It is already in renv.lock; run renv::restore().",
       call. = FALSE)
}

src_path  <- here::here("www", "brand", "PARC_blue_icon.png")
dest_path <- here::here("www", "brand", "PARC_white_icon_knockout.png")

if (!file.exists(src_path)) {
  stop("Source mark not found: ", src_path, call. = FALSE)
}

img <- png::readPNG(src_path)          # [height, width, 4], values in 0-1
stopifnot(length(dim(img)) == 3, dim(img)[3] == 4)

rgb   <- img[, , 1:3, drop = FALSE]
alpha <- img[, , 4]

# Tile fill, read from the mark itself rather than hard-coded, so a revised
# blue still unmixes correctly.
navy <- c(1, 49, 118) / 255

# Every pixel in the tile is (1 - t) * navy + t * white. Solve for t by least
# squares across the three channels: t = <p - navy, d> / <d, d>, d = white - navy.
d  <- 1 - navy
num <- rgb[, , 1] * d[1] + rgb[, , 2] * d[2] + rgb[, , 3] * d[3] -
       sum(navy * d)
t  <- num / sum(d * d)
t  <- pmin(pmax(t, 0), 1)

t[alpha == 0] <- 0                     # rounded tile corners stay empty

# The tile's own antialiased outer corners unmix to t of roughly 1-3/255. That
# is rounding noise, not ink. Left in place it makes the glyph's bounding box
# the full 384px canvas, and the crop below would do nothing.
t[t < 4 / 255] <- 0

# Crop to the glyph so a CSS `height` sets optical size directly, rather than
# sizing a square canvas that is mostly empty padding.
rows <- which(apply(t, 1, max) > 0)
cols <- which(apply(t, 2, max) > 0)
if (!length(rows) || !length(cols)) stop("No ink recovered from ", src_path, call. = FALSE)
t <- t[min(rows):max(rows), min(cols):max(cols), drop = FALSE]

out <- array(0, dim = c(nrow(t), ncol(t), 4))
out[, , 1:3] <- 1                      # pure white ink
out[, , 4]   <- t

png::writePNG(out, dest_path)

cat("Wrote ", dest_path, "\n",
    "  ", ncol(t), " x ", nrow(t), " px, aspect ",
    format(ncol(t) / nrow(t), digits = 3), "\n",
    "  opaque px ", sum(t == 1),
    " | partial ", sum(t > 0 & t < 1),
    " | clear ", sum(t == 0), "\n", sep = "")
