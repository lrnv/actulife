#' @include Generics.R Verbosity.R TableMort.R TableMortDiscrete.R TableMortContinue.R TableMortGroupePDI.R
NULL



########################################## (Fille) TableMortGroupePDIDiscret Pour la table discret, on va la definir via les pxt toujours, juste a moins d'endroit.
#' @name TableMortGroupePDIDiscrete
#' @export
setClass(Class = "TableMortGroupePDIDiscrete", representation = representation(), contains = c("TableMortGroupePDI", "TableMortDiscrete"), validity = function(object) {
    verb(" ~~~ TableMortGroupePDIDiscrete : verificateur ~~~\n", 2)
    for (tbl in object@sous_tables) {
        if (!methods::is(tbl, "TableMort"))
            stop("Vous esayez de construire une table de groupe discret avec des object qui ne sont pas des tables.")
    }
    return(TRUE)
})


tableMortGroupePDIDiscrete <- function(Tables, nom) {
    verb(" ~~~ TableMortGroupePDIDiscrete : Constructeur ~~~\n", 2)
    # Pour la compatibilite, on colle une table discrete random, mais on ne l'utilisera pas..
    for (i in 1:length(Tables)) {
        if (!is.cont(Tables[[i]])) {
            table <- getTable(Tables[[i]])
            break
        }
    }
    methods::new(Class = "TableMortGroupePDIDiscrete", sous_tables = Tables, table = table, name = nom)
}  # constructeur

##### Methodes et surcharges de methodes.
setMethod("getTable", "TableMortGroupePDIDiscrete", function(object) {
    verb("La fonction getTable n'a pas le meme effet pour les groupes de dimention superieure a un.\n
       Elle retourne les sous_tables et non pas la table.", 1)
    return(object@sous_tables)
})  # getTable

setMethod("Lx", "TableMortGroupePDIDiscrete", definition = function(table, x, UDD = TRUE) {
    if (!isOk(table, x)) {
        stop(isOk(table, x))
    }
    # sinon, on fait le calcul :
    x <- as.matrix(x)
    # on fixe L0 a 100 000
    kikou <- rep(1e+05, ncol(x))
    for (j in 1:ncol(x)) {
        for (i in 1:nrow(x)) {
            kikou[j] <- kikou[j] * Lx(table@sous_tables[[i]], x[i, j], UDD) / Lx(table@sous_tables[[i]], 0)
        }
    }
    return(kikou)
})  # Lx

setMethod("getAgeMax", "TableMortGroupePDIDiscrete", function(object) {
    tbls <- getTable(object)
    age <- rep(+Inf, length(tbls))
    for (i in 1:length(tbls)) {
        if (!is.cont(tbls[[i]])) {
            age[i] <- min(age, getAgeMax(tbls[[i]]))
        }
    }
    verb("La fonction getAgeMax pour les groupes retourne un vecteur des getAgeMax pour les discretes et des +Inf pour les continues.", 1)
    return(age)
})  # getAgeMax
