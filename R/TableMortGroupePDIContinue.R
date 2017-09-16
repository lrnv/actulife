#' @include Generics.R Verbosity.R TableMort.R TableMortDiscrete.R TableMortContinue.R TableMortGroupePDI.R
NULL



############################### (Fille) TableMortGroupePDIContinue Une table continue est definie par sa loi, et donc les pxt.
#' @name TableMortGroupePDIContinue
#' @export
setClass(Class = "TableMortGroupePDIContinue", representation = representation(), contains = c("TableMortGroupePDI", "TableMortContinue"), validity = function(object) {
    verb(" ~~~ TableMortGroupePDIContinue : verificateur ~~~\n", 2)
    for (i in 1:length(object@sous_tables)) {
        if (!methods::is(object@sous_tables[[i]], "TableMortContinue"))
            stop("Vous esayez de construire une table de groupe continue avec des object qui ne sont pas des tables continues.")
    }
    return(TRUE)
})

tableMortGroupePDIContinue <- function(Tables, nom) {
    verb(" ~~~ TableMortGroupePDIContinue : Constructeur ~~~\n", 2)
    # cdf = function(x){ # La CDF que nous avons ici est bien sur multi-dimentionelle if(!isOk(.Object,x)){stop(isOk(.Object,x))} # sinon, on fait le calcul : x <- as.matrix(x) prod =
    # rep(1,ncol(x)) for(j in 1:ncol(x)){for(i in 1:nrow(x)){ prod[j] = prod[j] * (1 - getCdf(.Object@sous_tables[[i]])(x[i,j])) }} return(1-prod) }
    methods::new(Class = "TableMortGroupePDIContinue", sous_tables = Tables, name = nom)
}  # constructeur
##### Methodes et surcharges de methodes.

setMethod("getCdf", "TableMortGroupePDIContinue", function(object) {
    return(cdf = function(x) {
        # La CDF que nous avons ici est bien sur multi-dimentionelle
        if (!isOk(object, x)) {
            stop(isOk(object, x))
        }
        # sinon, on fait le calcul :
        x <- as.matrix(x)
        prod <- rep(1, ncol(x))
        for (j in 1:ncol(x)) {
            for (i in 1:nrow(x)) {
                prod[j] <- prod[j] * (1 - getCdf(object@sous_tables[[i]])(x[i, j]))
            }
        }
        return(1 - prod)
    })
})  # getCdf
