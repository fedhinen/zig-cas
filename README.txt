# ZigCAS

Es un proyecto, espero que pequeño, para implementar un CAS en Zig, realmente es para aprender sobre el lenguaje, ahora tengo unas funcionalidades básicas, eval funciona siempre que pase todas las variables necesarias, tengo que hacerlo mas robusto, limpiarlo, empezar a organizar la información, y simplificar expresiones
> zig build run
info: arg: /home/fedhinen/Desktop/cas/zig-out/bin/cas
Result with variables: 30
Result with variables, derived: (1 + 0)

(5x)(20x)
Result derivative, ((((0 * x) + (5 * 1)) * (20 * x)) + ((5 * x) * ((0 * x) + (20 * 1))))

en principio esta expresión esta bien derivada, pero es dificil de leer, mucho parentesis, lo cual tiene sentido, al final lo que paso es que a cada nodo de la expresión se le aplico su regla de derivación, dando un arbol de expresiones mas grande, al que se le aplico un stringify por separado

lo siguiente que hare sera simplificar las expresiones
