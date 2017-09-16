library(actulife)

tableGc <- tableMortGroupePDI(c(expolaw, expolaw), "kikou")
tableGd <- tableMortGroupePDI(c(expolaw, th002d), "kikou")
tableGc
tableGd
maxTbl <- tableMortGroupePDI(c(tableGc@sous_tables, tableGd@sous_tables), "kikou")
maxTbl

##### C'est partit pour shuette-nesbit. voila d'abord les sk des tables :
SkTables(tableGc, 1)
SkTables(tableGd, 2)
SkTables(tableGPDId, 3)

# Fonction f pour shuette-nesbit : par exemple l'indicatrice de x = 2
f = function(x) {
    return((x == 1) + (x == 0))
}


# ça devrais etre toujours positif ici non ??  Opérateur delta :
DELTA(k = 4, f = f, x = 0)


SN(table = tableGPDId, f = f, x = c(1, 2, 3, 4), t = 3, paA = "p")

SN(table = tableGPDId, f = f, x = c(1, 2, 3, 4), t = 3, paA = "a")

SN(table = tableGPDId, f = f, x = c(1, 2, 3, 4), t = 3, paA = "A")


# petit trick :
getAgeMax(tableMortGroupePDI(c(th002v, th002d), nom = "kikou"))
getAgeMax(tableMortGroupePDI(c(th002v, expolaw), nom = "kikou"))

# remaque : la fonction getTable n'a pas le même effet sur les tables et les groupes : getTable(th002d)
getTable(tableMortGroupePDI(c(th002v, th002d), nom = "kikou"))
getTable(expolaw)
getTable(th002d)

