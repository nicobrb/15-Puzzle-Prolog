
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

1) CARICAMENTO DEL DOMINIO: compila il dominio mettendo una posizione che può essere valida come non valida, oltre al n (per ora n<= 4)  
l'input viene trascritto nella forma board([lista di elementi nella board nell'ordine + "v" come posizione blank], n), che verrà definita come ground truth della risoluzione. A seguire, digita il comando *dominio.pl* da riga di comando.

2) CARICAMENTO DELLE AZIONI PLAUSIBILI: Digita il comando *azioni.pl* da riga di comando.  
3) CARICAMENTO DELLA RICERCA: Digita il comando *ricerca.pl* da riga di comando.  
4) RISOLUZIONE: basterà digitare, a quel punto, da riga di comando, *prova(Variabile)* e attendere la fine della computazione.
