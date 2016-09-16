# Many of the color schemes used here are taken from Paul Tor"s
# technical note: https://personal.sron.nl/~pault/colourschemes.pdf.

library("pracma")

#' Create a gradient
#'
#' This function creates an RGB gradient based on a series of functions
#' @param R A single parameter function for calculating the red value.
#' @param G A single parameter function for calculating the green value.
#' @param B A single parameter function for calculating the blue value.
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' R <- function(x) 0.237 - 2.13 * x + 26.92 * x^2 - 65.5*x^3 + 63.5*x^4 - 22.36*x^5
#' G <- function(x) ((0.572 + 1.524 * x - 1.811 * x^2)/(1 - 0.291 * x + 0.1574 * x^2))^2
#' B <- function(x) 1/(1.579 - 4.03*x + 12.92*x^2 - 31.4*x^3 + 48.6*x^4 - 23.36*x^5)
#' create_smooth_gradient(R, G, B)

create_smooth_gradient <- function(R, G, B, incr=255) {
    step <- 1 / (incr - 1)
    rgb.palette <- rep(0, incr)
    for (i in 1:incr) {
        x <- (i - 1) * step
        r <- as.integer(R(x) * 255)
        g <- as.integer(G(x) * 255)
        b <- as.integer(B(x) * 255)

        if (r < 0)   r <- 0
        if (r > 255) r <- 255
        if (g < 0)   g <- 0
        if (g > 255) g <- 255
        if (b < 0)   b <- 0
        if (b > 255) b <- 255

        r <- as.hexmode(r)
        g <- as.hexmode(g)
        b <- as.hexmode(b)

        rgb.palette[i] <- sprintf("#%02x%02x%02x", r, g, b)
    }
    return (rgb.palette)
}

#' Create a mono red gradient
#'
#' This function creates a smooth, color blind-friendly, red-white gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.red(255)

gradient.red <- function(incr=255) {
    R <- function(x) 1 - 0.392 * (1 + erf((x - 0.869)/0.255))
    G <- function(x) 1.021 - 0.456 * (1 + erf((x - 0.527)/0.376))
    B <- function(x) 1 - 0.493 * (1 + erf((x - 0.272)/0.309))
    return(create_smooth_gradient(R, G, B, incr))
}

#' Create a mono blue gradient
#'
#' This function creates a smooth, color blind-friendly, blue-white gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.blue(R, G, B)

gradient.blue <- function(incr=255) {
    R <- function(x) 1.021 - 0.6 * (1 + erf((x - 0.6)/0.496))
    G <- function(x) 1.05 - 0.456 * (1 + erf((x - 0.45)/0.5))
    B <- function(x) 1.06 - 1.0 * (1.0 + erf((x - 1.38)/0.96))
    return(create_smooth_gradient(R, G, B, incr))
}

#' Create a mono purple gradient
#'
#' This function creates a smooth, color blind-friendly, purple-white gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.purple(R, G, B)

gradient.purple <- function(incr=255) {
    G <- function(x) 1.021 - 0.6 * (1 + erf((x - 0.6)/0.496))
    R <- function(x) 1.05 - 0.456 * (1 + erf((x - 0.45)/0.5))
    B <- function(x) 1.06 - 1.2 * (1.0 + erf((x - 1.38)/0.96))
    return(create_smooth_gradient(R, G, B, incr))
}

#' Create a mono green gradient
#'
#' This function creates a smooth, color blind-friendly, green-white gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.green(R, G, B)

gradient.green <- function(incr=255) {
    # a - b * (d + erf((x - e) / f))
    # a = y intercept (1=255)
    # b = x intercept (1.2=0)
    # e = location of inflection point
    # f = slope at inflection point (affects depth of color)

    R <- function(x) 1.021 - 0.6 * (1 + erf((x - 0.6)/0.496))
    G <- function(x) 1.06 - 1.2 * (1.0 + erf((x - 1.38)/0.96))
    B <- function(x) 1.05 - 0.456 * (1 + erf((x - 0.45)/0.5))
    return(create_smooth_gradient(R, G, B, incr))
}

gradient.green.save <- function(incr=255) {
    G <- function(x) 1 - 0.392 * (1 + erf((x - 0.869)/0.255))
    B <- function(x) 1.021 - 0.456 * (1 + erf((x - 0.527)/0.376))
    R <- function(x) 1 - 0.493 * (1 + erf((x - 0.272)/0.309))
    return(create_smooth_gradient(R, G, B, incr))
}


#' Create a black gradient
#'
#' This function creates a smooth, color blind-friendly, black-white gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.black(5)

gradient.black <- function(incr=255) {
    R <- function(x) 1 - 0.392 * (1 + erf((x - 0.869)/0.255))
    G <- function(x) 1.021 - 0.456 * (1 + erf((x - 0.527)/0.376))
    B <- function(x) 1 - 0.493 * (1 + erf((x - 0.272)/0.309))
    return(colorRampPalette(c("black", "white"))(incr))
}


#' Create a red-blue gradient
#'
#' This function creates a smooth, color blind-friendly, red-blue gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.red2Blue(255)

gradient.red2blue <- function(incr=255) {
    R <- function(x) 0.237 - 2.13 * x + 26.92 * x^2 - 65.5*x^3 + 63.5*x^4 - 22.36*x^5
    G <- function(x) ((0.572 + 1.524 * x - 1.811 * x^2)/(1 - 0.291 * x + 0.1574 * x^2))^2
    B <- function(x) 1/(1.579 - 4.03*x + 12.92*x^2 - 31.4*x^3 + 48.6*x^4 - 23.36*x^5)
    return(create_smooth_gradient(R, G, B, incr))
}

#' Create a rainbow gradient
#'
#' This function creates a smooth, color blind-friendly, rainbow gradient
#' @param incr The number of steps to take in the gradient. Defaults to 255.
#' @keywords colors palette
#' @export
#' @examples
#' gradient.smooth.red(R, G, B)

gradient.rainbow <- function(incr=255) {
    R <- function(x) ((0.472 - (0.567 * x) + (4.05 * x^2)) / (1 + (8.72 * x) - (19.17 * x^2) + (14.1 * x^3)))
    G <- function(x) (0.108932 - (1.22635 * x) + (27.284 * x^2) - (98.577 * x^3) + (163.3 * x^4) - (131.395 * x^5) + (40.634 * x^6) )
    B <- function(x) (1 / (1.97 + (3.54 * x) - (68.5 * x^2) + (243 * x^3) - (297 * x^4) + (125 * x^5) ))

    return(create_smooth_gradient(R, G, B, incr))
}


#' #' Create a green-purple gradient
#'
#' This function returns a color blind-friendly, green-purple gradient
#' @keywords colors palette
#' @export
#' @examples
#' gradient.green2purple()

gradient.green2purple <- function() {
    palette <- c("#005000","#008600","#00BB00","#00F100","#50FF50","#86FF86","#BBFFBB","#FFFFFF","#FFF1FF","#FFBBFF","#FF86FF","#FF50FF","#F100F1","#BB00BB","#860086","#500050")
    return(palette)
}

#' Create a brown-blue gradient
#'
#' This function returns a color blind-friendly, brown-blue gradient
#' @keywords colors palette
#' @export
#' @examples
#' gradient.brown2blue()

gradient.brown2blue <- function() {
    palette <- c("#331900","#662F00","#996035","#CC9B7A","#D8AF97","#F2DACD","#CCFDFF","#99F8FF","#65EFFF","#32E3FF","#00A9CC","#007A99")
    return(palette)
}


#' Create a gray-blue gradient
#'
#' This function returns a color blind-friendly, gray-blue gradient
#' @keywords colors palette
#' @export
#' @examples
#' gradient.gray2blue()

gradient.gray2blue <- function() {
    palette <- c("#2A2A2A","#4B4B4B","#656565","#8F8F8F","#B9B9B9","#E5E5E5","#CCFFFF","#99FFFF","#65EFFF","#32D2FF","#0098CC","#006999")
    return(palette)
}

#' Create a black-white gradient with a highlight
#'
#' This function returns a black to white gradient with a highlight
#' @param highlight The color to use as a highlight. Default is "red". Options are "green" or "blue".
#' @keywords colors palette
#' @export
#' @examples
#' gradient.focus(highlight="red")

gradient.focus <- function(highlight="red") {
    palette <- c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")
    if (highlight == "green")
    {
        palette[1] <- "#41AB5D"
    }
    else if (highlight == "blue")
    {
        palette[1] <- "#0033FF"
    }
    else
    {}
    return(palette)
}
