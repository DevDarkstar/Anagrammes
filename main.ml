(*
    rlwrap ocaml
    #use "main.ml";;

    ocamlc unix.cma main.ml -o main
*)

(* Permet d'importer l'ensemble des prénoms présents dans un fichier.
    param: filename -> le nom du fichier
    return: une liste contenant l'ensemble des prénoms présents dans le fichier.
    signature: val get_first_names : string -> string list = <fun>
*)
let get_first_names filename = 
    let file = open_in filename in
    let rec get_first_names_aux first_names =
        try
            (* On lit un prénom dans le fichier *)
            let first_name = input_line file in
            (* Et on l'ajoute en tête de la liste des prénoms *)
            get_first_names_aux (List.cons first_name first_names)
        (* Lorsqu'on a lu l'intégralité du fichier *)
        with End_of_file ->
            (* On ferme le fichier *)
            close_in file;
            (* et on inverse le contenu de la liste
            car nous avons ajouté chaque nouvel élement en tête de liste *)
            List.rev first_names
    in 
    get_first_names_aux [];;

(* Fonction permettant de compter le nombre d'occurrences de chaque lettre dans un mot.
    param: word -> mot à analyser
    return: un tableau de taille 26 (nombre de lettres de l'alphabet), associant à chaque lettre son nombre d'apparaition dans le mot.
    signature: val count_occurrences_letters : string -> int array = <fun> 
*)
let count_occurrences_letters word = 
    let word_length = String.length word in
    let tab_occurrences = Array.make 26 0 in
    let rec count_occurrences_letters i = 
        if i < word_length then(
            (* On transforme chaque caractère de la chaine en majuscule de sorte à avoir une uniformité de la casse *)
            let char = Char.uppercase_ascii word.[i] in
            match char with
            (* Dans le cas d'un caractère, on incrémente la valeur à l'indice correspondant à la position de la lettre dans l'alphabet - 1
               -> A = 0, B = 1, .., Z = 25 *)
            | 'A' .. 'Z' -> 
            tab_occurrences.(Char.code char - Char.code 'A') <- tab_occurrences.(Char.code char - Char.code 'A') + 1;
            count_occurrences_letters (i+1)
            (* On ignore tout autre type de caractère *)
            | _ -> count_occurrences_letters (i+1)
        )
        else tab_occurrences
    in
    count_occurrences_letters 0;;


(* Fonction permettant de compter le nombre le nombre d'occurrences de chaque lettre pour chaque mot contenus dans une liste.
    param: l -> liste de chaine de caractères contenant les mots à analyser.
    return: un tableau de tableau du nombre d'apparition de chaque lettre dans chaque mot.
    signature: val count_occurrences : string list -> (string, int array) Hashtbl.t = <fun>
*)
let count_occurences l =
    (* On récupère la taille de la liste l *)
    let list_size = List.length l in
    (* On crée un tableau de taille list_size *)
    let tab_occurrences = Hashtbl.create list_size in
    (* On remplit la hashtable en prenant un prénom comme clé et le tableau des occurrences des lettres de ce dernier comme valeur associée *)
    List.iter (function word -> (Hashtbl.add tab_occurrences word (count_occurrences_letters word))) l;
    (* On retourne le tableau contenant les tableaux du nombre d'apparition de chaque lettre dans chaque mot *)
    tab_occurrences;;

(* Permet de comparer deux mots en déterminant si le second mot passé en paramètre contient l'intégralité des lettres du premier.
    param: first_name -> tableau d'occurrences à tester.
    param: name -> tableau d'occurrences devant contenir l'ensemble des lettres de first_name.
    return true si name contient l'ensemble des lettres de first_name false sinon.
    signature: val compare_words : int array -> int array -> bool = <fun>
*)
let compare_words first_name name = 
    let first_name_length = Array.length first_name in
    let rec compare_words_aux i = 
        if i < first_name_length then(
            (* Si le nombre de lettre à l'indice i dans first_name est inférieur ou égal au nombre de lettres contenu dans name 
               on passe à l'itération suivante sinon on renvoie false. *)
            if first_name.(i) <= name.(i) then (
                (* On actualise le contenu de name en retirant dans name le nombre de lettres présentes dans first_name *)
                name.(i) <- name.(i) - first_name.(i);
                compare_words_aux (i+1)
            )
            else false
        )
        else true
    in
    compare_words_aux 0;;
    
    
    
let find_first_names h name = 
    let htle_size = Hashtbl.length h in
    let res = Hashtbl.create htle_size in
    Hashtbl.iter (fun key value -> 
        if compare_words value name then(
            Hashtbl.add res key name
        )) h;
    res;;
        

  

(*let t1 = Unix.gettimeofday ();;*)
let names = get_first_names "fichier.txt";;
let counts = count_occurences names;;
(*Hashtbl.iter (fun key value -> Printf.printf "%s -> %d\n" key value.(3)) counts;;*)
let nom = "RICHARDINE";;
let res = count_occurrences_letters nom;;
let resultat = find_first_names counts res;;
Hashtbl.iter (fun key value -> (
    Printf.printf "cle: %s -> [" key;
    Array.iter (fun i -> Printf.printf "%d," i) value
 )) resultat;;
(*let t2 = Unix.gettimeofday ();;
let execution_time = t2 -. t1 in
Printf.printf "Temps d'execution du programme: %.2f secondes\n" execution_time;;*)