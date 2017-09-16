############ Main management of verbosity functions


.Verbosite <- 0
# un integer, ce peut etre 0, 1, 2 ou 3...  si c'est 0, le programe ne dit que
# ce qui est TRES important ( message d'erreur, warnings...)  si c'est 1, il
# raconte un peu plus sa vie : il affiche les noms des fonctions qui s'executes,
# ce qu'il fait...  si c'est 2, le mode debeugage est active et ca cause grave.


#' Set verbosity lvl
#' 
#' \code{setVerbosity} allow you to set the verbosity lvl of life insurance
#' classes and life table classes and everything that comes out of actulife
#' package.
#' @param x An integer, 0, 1 or 2.
#' @return Nothing. 0 to set normal verbosity, 1 to talk a little more and 2 for
#'   debug.
#' @examples
#' setVerbosity(0) # default value
#' setVerbosity(1) # much more talking
#' setVerbosity(2) # debug mode, talks a loot.
setVerbosity <- function(x) {
    if (x == 0 || x == 1 || x == 2) {
        .Verbosite <<- x
    } else {
        stop("La verbosite ne peut prendre come parametre que 0, 1 ou 2")
    }
}



getVerbosity <- function() {
    .Verbosite
}

# fonction causer :
verb <- function(x, v = 1) {
    if (v <= .Verbosite) {
        cat(x)
    }
}
