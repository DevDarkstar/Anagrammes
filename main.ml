(*
    rlwrap ocaml
    #use "main.ml";;
*)

(* Permet d'importer l'ensemble des prénoms présents dans un fichier.
param: filename -> le nom du fichier
return: une liste contenant l'ensemble des prénoms présents dans le fichier.
signature: string -> string list = <fun>
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
signature: string -> int array = <fun> 
*)
let count_occurrences_letters word = 
    let word_length = String.length word in
    let tab_occurrences = Array.make 26 0 in
    let rec count_occurrences_letters i = 
        if i < word_length then(
            let char = Char.uppercase_ascii word.[i] in
            match char with
            | 'A' .. 'Z' -> 
            tab_occurrences.(Char.code char - Char.code 'A') <- tab_occurrences.(Char.code char - Char.code 'A') + 1;
            count_occurrences_letters (i+1)
            | _ -> count_occurrences_letters (i+1)
        )
        else tab_occurrences
    in
    count_occurrences_letters 0;;


(* Fonction permettant de compter le nombre le nombre d'occurrences de chaque lettre pour chaque mot contenus dans une liste.
param: l -> liste de chaine de caractères contenant les mots à analyser.
return: un tableau de tableau du nombre d'apparition de chaque lettre dans chaque mot.
signature: string list -> array array int = <fun>
*)
let count_occurences l =
    (* On récupère la taille de la liste l *)
    let list_size = List.length l in
    (* On crée un tableau de taille list_size *)
    let tab_occurrences = Array.make list_size (Array.make 26 0) in
    List.iteri (fun i word -> tab_occurrences.(i) <- count_occurrences_letters word) l;
    (* On retourne le tableau contenant les tableaux du nombre d'apparition de chaque lettre dans chaque mot *)
    tab_occurrences;;
    

  


let names = get_first_names "fichier.txt";;
let counts = count_occurences names;;
let nom = "UN MESSAGE";;
let res = count_occurrences_letters nom;;