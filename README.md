
# prolog-sliding-block-puzzle

15-puzzle logic generalized to N-by-N tiles using ProLog language.
I file devono seguire questa struttura rigida

- *azioni.pl*
    *trasforma(MossaDaFare, StatoPartenza, StatoArrivo)*
    *applicabile(MossaDaFare, StatoPartenza)*

- *dominio.pl*
    Esso contiene lo stato iniziale modificabile dall'utente e lo stato goal
    *metodi nostri da aggiungere su eventuale correttezza dell'input

- *ricerca.pl*
    *prova(Variabile)*: esso passa in input la variabile su cui andrà scritta la soluzione (nel nostro caso le slides)

    *contiene la logica del programma

Il progetto si concentra nel trovare una soluzione al problema del N-puzzle usando strategie di ricerca informata, in particolare usando l'algoritmo di Iterative Deepening A*, o comunemente IDA*.
