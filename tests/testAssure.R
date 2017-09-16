# 2 assurés ont été construit :
library(actulife)
MrPat
MmePat

couple = c(MrPat, MmePat)
trouple = c(MrPat, c(MmePat, MrPat))
trouple

Ax(couple@Tbl, couple@Age)
Ax(MrPat@Tbl, MrPat@Age)

pxt(couple@Tbl, couple@Age, 30)
# Normalement le couple ça devrais donner le produit des proba individuelle non ?
pxt(couple@Tbl@sous_tables[[1]], couple@Age[1], 30) * pxt(couple@Tbl@sous_tables[[2]], couple@Age[2], 30) == pxt(couple@Tbl, couple@Age, 30)



