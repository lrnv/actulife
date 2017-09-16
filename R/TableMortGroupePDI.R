#' @include Generics.R Verbosity.R TableMort.R TableMortDiscrete.R TableMortContinue.R
NULL


#################### (Fille / Mere virtuelle) TableMortGroupePDI Une table de groupePDI
#' @name TableMortGroupePDI
#' @export
setClass(Class = "TableMortGroupePDI", representation = representation(sous_tables = "list", "VIRTUAL"), prototype = prototype(sous_tables = list()), contains = "TableMort")

tableMortGroupePDI <- function(Tables, nom) {
    verb(" ~~~ TableMortGroupePDI : Constructeur ~~~\n", 1)
    continue <- TRUE
    for (i in 1:length(Tables)) {
        continue <- continue * is.cont(Tables[[i]])
    }
    if (!continue) {
        # alors il faut construire une table de groupePDI discrete
        verb("Vous construisez une table discrete \n", 2)
        return(tableMortGroupePDIDiscrete(Tables, nom))
    } else {
        verb("Vous construisez une table continue \n", 2)
        return(tableMortGroupePDIContinue(Tables, nom))
    }
}  # constructeur
##### Methodes et surcharges de methodes.

setMethod("length", "TableMortGroupePDI", function(x) {
    return(length(x@sous_tables))
})
### Petit fonction pour test si les ages pass?s en parametres sont bien les bons :

setMethod("isOk", "TableMortGroupePDI", function(table, x) {
    if (!((is.matrix(x) && nrow(x) == length(table)) || (is.vector(x) && length(x) == length(table)))) {
        # si l'argument passe est incorect, on rale.
        return("ERROR : L'argument des ages pour une table de groupe doit etre passe par matrice,
           avec les vecteurs d'age des assures en COLONE.\n
           Ainsi, le nombre de lignes doit etre egal au nombre d'assures.\n
           Hint : Essayez cbind()")
    }
    return(TRUE)
})

setMethod("show", "TableMortGroupePDI", function(object) {
    cat(" ~~~ Table de mortalite :", if (is.cont(object)) {
        " Continue"
    } else {
        "Discret"
    }, "Groupe (Independant) au premier deces ~~~\n")
    cat("     Les tables sous-jacentes sont les suivante: \n")
    kikou <- matrix(0)
    for (i in 1:length(object@sous_tables)) {
        kikou <- rbind(kikou, as.matrix(paste("  ", utils::capture.output(show(object@sous_tables[[i]])))))
    }
    kikou <- kikou[-1, ]
    df <- (data.frame(kikou))
    colnames(df) <- c("")
    cat("          ")
    print(format(df, justify = "left"), row.names = F)
})  # show

setMethod("ax", "TableMortGroupePDI", function(table, x, temp = 200, diff = 0, m = 1, due = TRUE, continue = FALSE) {
    verb("methode ax de groupe called", 2)
    if (!isOk(table, x)) {
        stop(isOk(table, x))
    }
    x <- as.matrix(x)
    plop <- x[1, ]
    for (i in 1:ncol(x)) {
        plop[i] <- methods::callNextMethod(table, x[, i], temp, diff, m, due, continue)
    }
    return(plop)
})

setMethod("Ax", "TableMortGroupePDI", function(table, x, temp = 200, diff = 0, m = 1, continue = FALSE) {
    verb("methode Ax de groupe called", 2)
    if (!isOk(table, x)) {
        stop(isOk(table, x))
    }
    x <- as.matrix(x)
    plop <- x[1, ]
    for (i in 1:ncol(x)) {
        plop[i] <- methods::callNextMethod(table, x[, i], temp, diff, m, continue)
    }
    return(plop)
})
# Shuette-Nesbit

setMethod("SkTables", "TableMortGroupePDI", definition = function(table, k) {
    if (k == 1) {
        return(table@sous_tables)
    }
    if (k != floor(k)) {
        stop("Le nombre de personne dans un groupe doit toujours etre entier, idiot !")
    }
    if (!(k <= length(table@sous_tables))) {
        stop("Skt : Vous essayer de combiner plus de tables qu'il n'en existe dans le groupe.")
    }
    Cmb <- utils::combn(table@sous_tables, k)
    kikou <- list()
    for (i in 1:length(Cmb[1, ])) {
        kikou[[i]] <- tableMortGroupePDI(Tables = Cmb[, i], nom = "Sk")
    }
    return(kikou)
})  #SkTables

setMethod("SN", "TableMortGroupePDI", definition = function(table, f, x, t = 1, paA) {
    verb("schuette nesbitt called", 2)
    if (!isOk(table, x)) {
        stop(isOk(table, x))
    }
    x <- as.matrix(x)
    plop <- x[1, ]
    functiontoapplySN <- switch(paA, p = function(table, x) pxt(table, x, t), a = function(table, x) ax(table, x), A = function(table, x) Ax(table, x))
    for (p in 1:ncol(x)) {
        if (is.null(functiontoapplySN)) {
            stop("Vous devez donner pour paA un character = 'p', 'a' ou 'A'")
        }
        for (i in 1:length(table@sous_tables)) {
            # Pour apliquer pxt, il faut que je recupere les ages, et la table qu'il me faut.
            tables <- SkTables(table, i)
            ages <- utils::combn(x[, p], i)
            deltf <- DELTA(i, f, 0)
            # donc le vecteur cmbs[,j] corepond a skTables[[j]]
            for (j in 1:length(tables)) {
                # retourner ici ma fonction avec : age = ages[,j] et table = tables[[j]]
                plop[p] <- plop[p] + functiontoapplySN(tables[[j]], unlist(ages[, j])) * deltf
            }
        }
    }
    return(plop)
})  #SN
