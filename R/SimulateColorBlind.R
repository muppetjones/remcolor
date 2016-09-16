list <- structure(NA, class = "result")
"[<-.result" <- function(x, ..., value) {
    args <- as.list(match.call())
    args <- args[-c(1:2, length(args))]
    length(value) <- length(args)
    for (i in seq(along = args)) {
        a <- args[[i]]
        if (!missing(a)) {
            eval.parent(substitute(a <- v, list(a = a, v = value[[i]])))
        }
    }
    x
}

CVDMatrix <- list( # Color Vision Deficiency
    "Protanope" = c( # reds are greatly reduced (1% men)
        0.0, 2.02344, -2.52581,
        0.0, 1.0,      0.0,
        0.0, 0.0,      1.0
    ),
    "Deuteranope" = c( # greens are greatly reduced (1% men)
        1.0,      0.0, 0.0,
        0.494207, 0.0, 1.24827,
        0.0,      0.0, 1.0
    ),
    "Tritanope" = c( # blues are greatly reduced (0.003% population)
        1.0,       0.0,      0.0,
        0.0,       1.0,      0.0,
        -0.395913, 0.801109, 0.0
    )
)

#' Simulate colorblindness for a color
#'
#' Generate a hex color simulating colorblindness.
#' Algorithm from http://www.daltonize.org/
#' @param hex_str The hex color string, e.g., '#FF0000'
#' @param cvd A vector (3x3--9 values) specifying the color vision deficiency transform matrix
#' @keywords colors, palette, colorblind
#' @export
#' @examples
#' simulate_colorblind_green(
#'  c("#005000","#008600","#00BB00"),
#'  border = "light gray"
#' )
simulate_colorblind_hex <- function(hex_str, cvd) {
    # algorithm from http://www.daltonize.org/

    # init vars for lintr
    cvd_a <- cvd_b <- cvd_c <- 0
    cvd_d <- cvd_e <- cvd_f <- 0
    cvd_g <- cvd_h <- cvd_i <- 0
    r <- g <- b <- 0

    # break apart the vectors
    list[cvd_a, cvd_b, cvd_c, cvd_d, cvd_e, cvd_f, cvd_g, cvd_h, cvd_i] <- cvd
    list[r, g, b] <- col2rgb(hex_str)

    # RGB to LMS matrix conversion
    L <- (17.8824 * r) + (43.5161 * g) + (4.11935 * b)
    M <- (3.45565 * r) + (27.1554 * g) + (3.86714 * b)
    S <- (0.0299566 * r) + (0.184309 * g) + (1.46709 * b)

    # Simulate color blindness
    l <- (cvd_a * L) + (cvd_b * M) + (cvd_c * S)
    m <- (cvd_d * L) + (cvd_e * M) + (cvd_f * S)
    s <- (cvd_g * L) + (cvd_h * M) + (cvd_i * S)

    # LMS to RGB matrix conversion
    R <- (0.0809444479 * l) + (-0.130504409 * m) + (0.116721066 * s)
    G <- (-0.0102485335 * l) + (0.0540193266 * m) + (-0.113614708 * s)
    B <- (-0.000365296938 * l) + (-0.00412161469 * m) + (0.693511405 * s)

    # Isolate invisible colors to color vision deficiency
    # (calculate error matrix)
    R <- r - R
    G <- g - G
    B <- b - B

    # Shift colors towards visible spectrum (apply error modifications)
    RR <- (0.0 * R) + (0.0 * G) + (0.0 * B)
    GG <- (0.7 * R) + (1.0 * G) + (0.0 * B)
    BB <- (0.7 * R) + (0.0 * G) + (1.0 * B)

    # Add compensation to original values
    R <- RR + r
    G <- GG + g
    B <- BB + b

    # Clamp values
    if (R < 0)  R <- 0
    if (R > 255) R <- 255
    if (G < 0) G <- 0
    if (G > 255) G <- 255
    if (B < 0) B <- 0
    if (B > 255) B <- 255

    R <- as.integer(R)
    G <- as.integer(G)
    B <- as.integer(B)

    return(sprintf("#%02x%02x%02x", R, G, B))
}

#' Deuteranope (green-blind) version of palette (1% men)
#'
#' Generate a set of hex colors that simulate red color blindness
#' @param col The color vector
#' @keywords colors, palette, colorblind
#' @export
#' @examples
#' remcolor.simulate_colorblind_green(
#'  c("#005000","#008600","#00BB00"), border = "light gray")

simulate_colorblind_green <- function(col){
    return(sapply(col, function(x)
        simulate_colorblind_hex(x, CVDMatrix[["Deuteranope"]])
    ))
}

#' Protanope (red-blind) version of palette (1% men)
#'
#' Generate a set of hex colors that simulate green color blindness
#' @param col The color vector
#' @keywords colors, palette, colorblind
#' @export
#' @examples
#' remcolor.simulate_colorblind_red(
#'  c("#005000","#008600","#00BB00"), border = "light gray")

simulate_colorblind_red <- function(col){
    return(sapply(col, function(x)
        simulate_colorblind_hex(x, CVDMatrix[["Protanope"]])
    ))
}

#' Tritanope (blue-blind) version of palette (0.003% population)
#'
#' Generate a set of hex colors that simulate blue color blindness
#' @param col The color vector
#' @keywords colors, palette, colorblind
#' @export
#' @examples
#' remcolor.simulate_colorblind_blue(
#'  c("#005000","#008600","#00BB00"), border = "light gray")

simulate_colorblind_blue <- function(col){
    return(sapply(col, function(x)
        simulate_colorblind_hex(x, CVDMatrix[["Tritanope"]])
    ))
}
