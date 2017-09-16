library(actulife)


### table d'un groupe au premier décès indépendants, tables continues

pxt(tableGPDIc, c(0, 0, 0, 0), 1)  # Bon, mon groupe vivra pas vieux, mais au moins ça marche ! 
pxt(tableGPDIc, c(1, 2, 3, 4), c(1, 2, 3))


# La fonction LX est vectorisé proprement pour les groupe discres, a coup de matrices.
Lx(tableGPDIc, c(1, 2, 3, 4))
Lx(tableGPDIc, matrix(1:12, nrow = 4))
Lx(tableGPDIc, cbind(c(1, 2, 3, 4), c(1, 3, 4, 5), c(1, 5, 7, 9)))  # la version cbind sera utilisée par défaut je pense. 
# Rappel : > Lx(th002d,c(1,2,3)) [1] 99511 99473 99446
getCdf(tableGPDIc)(cbind(c(18, 20, 30, 40), c(1, 2, 3, 4)))
getCdf(tableGPDIc)(c(1, 2, 3, 4))
# Rappel : > getCdf(expolaw)(c(1,2,3)) [1] 0.009950166 0.019801327 0.029554466 Ce qui n'est plus le cas ici

# Conclusion : Les fonction Lx et getCdf ne sont plus vectorisées sur leur parametres( l'age ) car l'age est maintenant un parametre multidimentionel.  Est-ce qu'on pourrais les
# vectorisées quand meme ? genre en passant plusieurs ages multidimentionels sous forme de matrices ? Maybe...
pxt(tableGPDIc, x = c(1, 2, 3, 4), t = c(1, 2, 3))
pxt(tableGPDIc, x = c(1, 2, 3, 4), t = c(1, 2, 3))
pxt(tableGPDIc, x = cbind(c(3, 4, 5, 6), c(1, 2, 3, 4)), t = c(1, 2, 3))
# On est donc bien sur que pxt est BIEN vecotrisé quoi qu'il arrive.

# Verifions pour Ext :
Ext(tableGPDIc, x = c(1, 2, 3, 4), t = c(1, 2, 3)) == pxt(tableGPDIc, x = c(1, 2, 3, 4), t = c(1, 2, 3)) * c(v, v^2, v^3)

# ax ?
ax(tableGPDIc, cbind(c(18, 18, 18, 18), c(0, 1, 90, 30)), temp = 10, diff = 2)
ax(tableGPDIc, cbind(c(0, 0, 0, 0), c(50, 50, 50, 50), c(0, 1, 90, 30), c(14, 20, 30, 12)))
ax(tableGPDIc, cbind(c(0.03, 90.1, 12.7, 18.9), c(37.76, 39, 43.2, 0)))
# en continue, comme j'ai que des tables exponentielles pour l'isntant lol.
ax(tableGPDIc, cbind(c(0, 0, 0, 0), c(50, 50, 50, 50), c(0, 1, 90, 30), c(14, 20, 30, 12)))


Ax(tableGPDIc, cbind(c(0, 0, 0, 0), c(50, 50, 50, 50), c(0, 1, 90, 30), c(14, 20, 30, 12)))
