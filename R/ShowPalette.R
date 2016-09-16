# library("plyr")
# library("ggplot2")


#' Show palette
#'
#' This function plots vector of colors side by side
#' @param col The color vector
#' @param border The color of the border separating each color. Defaults to NA.
#' @keywords colors palette
#' @export
#' @examples
#' show_palette(c("#005000", "#008600", "#00BB00"), border = "light gray")

show_palette <- function(col, border = NA, ...){
    n <- length(col)
    plot(0, 0,
        type = "n", xlim = c(0, 1), ylim = c(0, 1),
        axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n - 1) / n, 0, 1:n / n, 1, col = col, border = border)
}


#' Show palettes
#'
#' This function plots vector of colors side by side
#' @param col The color vector
#' @param border The color of the border separating each color. Defaults to NA.
#' @keywords colors palette
#' @export
#' @examples
#' show_palette(c("#005000", "#008600", "#00BB00"), border = "light gray")

show_palettes <- function(col_list, border = NA, show.rgb = T, ...){
    nr <- length(col_list)
    x_off <- -0.01
    plot(0, 0,
        type = "n", xlim = c(x_off, 1), ylim = c(0, 1),
        axes = FALSE, xlab = "", ylab = "", ...)
    yi <- rev(c(0:(nr - 1) / nr, 1))
    for (i in 1:length(col_list)) {
        col <- col_list[[i]]
        nc <- length(col)

        x1 <- 0:(nc - 1) / nc
        y1 <- yi[i + 1]
        x2 <- 1:nc / nc
        y2 <- yi[i]
        rect(x1, y1, x2, y2, col = col, border = border)

        if (show.rgb) {
            xd <- (x2 - x1) / 2
            xm <- x1 + xd
            yd <- (y2 - y1) / 2
            ym.1 <- y1 + (3 * yd / 2)
            ym.2 <- y1 + yd
            ym.3 <- y1 + (yd / 2)
            text(xm, ym.1, labels = lapply(col, function(x) col2rgb(x)[1, ]))
            text(xm, ym.2, labels = lapply(col, function(x) col2rgb(x)[2, ]))
            text(xm, ym.3, labels = lapply(col, function(x) col2rgb(x)[3, ]))
        }

    }

    length(yi) <- nr
    if (! is.null(names(col_list))) {
        text(x = x_off, y = yi,
            adj = c(1, 1.1), col = "black", labels = names(col_list))
    } else {
        text(x = x_off, y = yi,
            adj = c(1, 1.1), col = "black", labels = 1:length(col_list))
    }
}


#' Show palette with colorblind simulations
#'
#' This function plots vector of colors side by side
#' @param col The color vector
#' @param border The color of the border separating each color. Defaults to NA.
#' @keywords colors palette
#' @export
#' @examples
#' show_palette(c("#005000", "#008600", "#00BB00"), border = "light gray")

show_palette_colorblind <- function(col, border = NA, ...) {
    cb.red <- simulate_colorblind_red(col)
    cb.green <- simulate_colorblind_green(col)
    cb.blue <- simulate_colorblind_blue(col)
    show_palettes(
        list(O = col, R = cb.red, G = cb.green, B = cb.blue), border = border
    )
}

#' Show palette with colorblind simulations
#'
#' This function plots multiple vector of colors side by side
#' @param col_list A list of color vectors
#' @param border The color of the border separating each color. Defaults to NA.
#' @keywords colors palette
#' @export
#' @examples
#' a <- list(gradient.green(8), gradient.blue(8), gradient.red(8))
#' plot_palettes(a, border = "light gray")

plot_palettes <- function(col_list, border = NA, ...){

    n_col <- length(col_list)
    n_row <- length(col_list[[1]])
    col.vec <- c()
    for (i in 1:n_col) {
        col <- col_list[[i]]
        red <- sapply(col, function(x) col2rgb(x)[1, ])
        green <- sapply(col, function(x) col2rgb(x)[2, ])
        blue <- sapply(col, function(x) col2rgb(x)[3, ])
        col.vec <- c(col.vec, red, green, blue)
    }

    col.vec <- sapply(col.vec, as.numeric)
    # print(matrix(col.vec, ncol = n_col * 3, nrow = n_row))

    column.names <- rep(paste("color", seq(n_col), sep = ""))
    col.x <- rep(seq(n_row), n_col)
    if (! is.null(names(col_list))) {
        col.color_grp <- rep(names(col_list), each = n_row * 3)
    } else {
        col.color_grp <- rep(seq(n_col), each = n_row * 3)
    }
    col.channel <- rep(rep(c("R", "G", "B"), each = n_row), n_col)
    col.df <- data.frame(cbind(col.x, col.vec, col.color_grp, col.channel))
    col.df <- data.frame(
        x = col.x, intensity = col.vec,
        color = col.color_grp, channel = col.channel
    )
    colnames(col.df) <- c("x", "intensity", "color", "channel")
    col.df$channel <- factor(col.df$channel, c("R", "G", "B"))
    # col.df$intensity <- as.numeric(col.df$intensity)
    # col.df
    # print(col.df)

    g <- ggplot(col.df,
        aes(x = x, y = intensity,
            color = channel, group = interaction(channel, color)))
    g <- g + geom_line(aes(size = 4)) +
        scale_y_continuous(
            breaks = c(seq(0, 255, 32), 255), limits = c(0, 255)) +
        scale_x_continuous(breaks = c(seq(n_row)))
    g <- g + facet_wrap(~color, ncol = 1)
    g
}
