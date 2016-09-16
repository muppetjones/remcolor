library('pracma')

#' Create a rainbow palette
#'
#' This function creates a color blind-friendly, rainbow palette
#' @param n.colors The number of colors in the palette. Default=12. Options={6,8,10,12,14,15,18,21}
#' @keywords colors palette
#' @export
#' @examples
#' palette.rainbow(21)

palette.rainbow <- function(n.colors=12) {

    p <- switch(as.character(n.colors),
                '1' = c("#5577cc"),
                '2' = c("#5577cc", "#aa4477"),
                '3' = c("#5577cc", "#ddcc77", "#aa4477"),
                '4' = c("#404096", "#57a3ad", "#dea73a", "#d92120"),
                '5' = c("#404096", "#529db7", "#7db874", "#e39c37", "#d92120"),
                '6' = c("#404096", "#498cc2", "#63ad99", "#b3bc48", "#e68b33", "#d92120"),
                '7' = c("#781c81", "#3f60ae", "#539eb6", "#6db388", "#cab843", "#e78532", "#d92120"),
                '8' = c("#781c81", "#dd99bb", "#1f66aa", "#77aadd", "#117755", "#F6C141", "#e88c28", "#d92120"),
                '9' = c("#781c81", "#88ccee", "#44aa99", "#117733", "#999933", "#ddcc77", "#cc6677", "#882255", "#d92120"),
                '10' = c("#781c81", "#BF914D", "#A8BF4D", "#63BF4D", "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF", "#A84DBF", "#d92120"),
                '11' = c("#781c81", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#d92120"),
                '12' = c("#781c81", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#d92120"),
                '13' = c(palette.rainbow(12), gradient.smooth.black(4)[2]),
                '14' = c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C"),
                '15' = c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
                '16' = c(palette.rainbow(15), gradient.smooth.black(4)[2]),
                '17' = c(palette.rainbow(15), gradient.smooth.black(4)[2:3]),
                '18' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
                '19' = c(palette.rainbow(18), gradient.smooth.black(4)[2]),
                '20' = c(palette.rainbow(18), gradient.smooth.black(4)[2:3]),
                '21' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
#                 '22' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "#454545"),
#                 '23' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "#454545", "#8B8B8B"),
#                 '24' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "#454545", "#8B8B8B", "#D0D0D0"),
#                 '25' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "#454545", "#737373", "#A2A2A2", "#D0D0D0"),
#                 '26' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788", "#2E2E2E", "#5C5C5C", "#8B8B8B", "#B9B9B9","#E7E7E7" ),
    )
    if (is.null(p)) {
        g <- gradient.black(n.colors - 19)
#         print(g)
        e <- max(2, length(g) - 1)
#         print(g[2:e])
        p <- c(palette.rainbow(21), g[2:e])
    }
    return(p)
}


#' Color palette from gplots rich.colors
#'
#' This function creates a color blind-friendly palette
#' @param n.colors The number of colors in the palette. Default=12. Options={6,8,10,12}
#' @keywords colors palette
#' @export
#' @examples
#' palette.rich(10)

palette.rich <- function(n.colors=12) {
    # Generated with package "gplots" function rich.colors(12)
    switch(as.character(n.colors),
           '12' = c("#000040", "#000093", "#0020E9", "#0076FF", "#00B8C2", "#04E466", "#49FB25", "#E7FD09", "#FEEA02", "#FFC200", "#FF8500", "#FF3300"),
           '10' = c("#000041", "#0000A9", "#0049FF", "#00A4DE", "#03E070", "#5DFC21", "#F6F905", "#FFD701", "#FF9500", "#FF3300"),
           '8' = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300"),
           '6' = c("#000043", "#0033FF", "#01CCA4", "#BAFF12", "#FFCC00", "#FF3300")
    )

}

#' Color palette from gplots rich.colors
#'
#' This function creates a color blind-friendly, rainbow palette
#' @param n.colors The number of colors in the palette. Default=12. Options={6,8,10,12}
#' @keywords colors palette
#' @export
#' @examples
#' gradient.red2Blue()

palette.tim <- function(n.colors=12) {
    # Generated with package "fields" function tim.colors(12), which is said to emulate the default matlab colorset
    switch(as.character(n.colors),
           '12' = c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000"),
           '10' = c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#800000"),
           '8' = c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", "#CFFF30", "#FF9F00", "#FF1000", "#800000"),
           '7' = c("#00008F", "#0040FF", "#00AFFF", "#33FFCF", "#FF9F00", "#FF1000", "#800000"),
           '6' = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000")
    )
}


#' Color palette from gplots rich.colors
#'
#' This function creates a color blind-friendly, rainbow palette
#' @param n.colors The number of colors in the palette. Default=12. Options={6,8,10}
#' @keywords colors palette
#' @export
#' @examples
#' palette.dark()

palette.dark <- function(n.colors=12) {
    # Generated with sort(brewer.pal(8,"Dark2")) #Dark2, Set2
    switch(as.character(n.colors),
           '8' = c("#1B9E77", "#666666", "#66A61E", "#7570B3", "#A6761D", "#D95F02", "#E6AB02", "#E7298A"),
           '6' = c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", "#E6AB02", "#E7298A")
    )
}

#' Color palette from gplots rich.colors
#'
#' This function creates a color blind-friendly, rainbow palette
#' @param n.colors The number of colors in the palette. Default=12. Options={6,8,10}
#' @keywords colors palette
#' @export
#' @examples
#' palette.light()

palette.light <- function(n.colors=12) {
    # Generated with sort(brewer.pal(8,"Dark2")) #Dark2, Set2
    switch(as.character(n.colors),
           '8' = c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F"),
           '6' = c("#66C2A5", "#8DA0CB", "#A6D854", "#E78AC3", "#FC8D62", "#FFD92F")
    )
}

#' Another color-blind friendly qualitative scheme
#'
#' This function creates a color blind-friendly, rainbow palette
#' @keywords colors palette color-blind rainbow
#' @export
#' @examples
#' palette.qual(10)
palette.rainbow.alt <- function() {
    c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
}

#' Qualitative color palette by Paul Tol
#'
#' This function creates a color blind-friendly, rainbow palette
#' @param n.colors The number of colors in the palette. Default=12. Options={1-12}
#' @keywords colors palette
#' @export
#' @examples
#' palette.qual(10)

palette.qual <- function(n.colors=12) {
    # Qualitative color schemes by Paul Tol
    switch(as.character(n.colors),
           '1' = c("#4477AA"),
           '2' = c("#4477AA", "#CC6677"),
           '3' = c("#4477AA", "#DDCC77", "#CC6677"),
           '4' = c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
           '5' = c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
           '6' = c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
           '7' = c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
           '8' = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
           '9' = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
           '10' = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
           '11' = c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
           '12' = c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"),
           '13' = c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
           '14' = c("#223377", "#5588BB", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
           '15' = c("#223377", "#5588BB", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"),
           '16' = c(palette.qual(15), gradient.black(4)[2]),
           '17' = c(palette.qual(15), gradient.black(4)[2:3]),
           '18' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"),
           '19' = c(palette.qual(18), gradient.black(4)[2]),
           '20' = c(palette.qual(18), gradient.black(4)[2:3]),
           '21' = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
    )
}
