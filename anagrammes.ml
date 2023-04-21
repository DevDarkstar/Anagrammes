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
            get_first_names_aux (first_name::first_names)
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
    let rec count_occurrences_letters_aux i = 
        if i < word_length then(
            (* On transforme chaque caractère de la chaine en majuscule de sorte à avoir une uniformité de la casse *)
            let c = Char.uppercase_ascii word.[i] in
            match c with
            (* Dans le cas d'un caractère, on incrémente la valeur à l'indice correspondant à la position de la lettre dans l'alphabet - 1
               -> A = 0, B = 1, .., Z = 25 *)
            | 'A' .. 'Z' -> 
            tab_occurrences.(Char.code c - Char.code 'A') <- tab_occurrences.(Char.code c - Char.code 'A') + 1;
            count_occurrences_letters_aux (i + 1)
            (* On ignore tout autre type de caractère *)
            | _ -> count_occurrences_letters_aux (i + 1)
        )
        else tab_occurrences
    in
    count_occurrences_letters_aux 0;;


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
                compare_words_aux (i + 1)
            )
            else false
        )
        else true
    in
    compare_words_aux 0;;

(*
    Permet de transformer un tableau d'occurrences de caractères d'un mot en chaine de caractères.
    param: tab -> tableau d'occurrences de caractères à transformer
    return: une chaine de caractères obtenue à partir du tableau d'occurrences

    signature: val occurrences_table_to_string : int array -> string = <fun>
*)
let occurrences_table_to_string tab =
    let length = Array.length tab in
    let rec occurrences_table_to_string_aux acc i = 
        if i < length then(
            (* A chaque indice du tableau tab, on récupère le nombre de lettres correspondantes *)
            let number = tab.(i) in
            let rec add_letters acc_aux j = 
                if j < number then
                    (* Pour un indice donné du tableau d'occurrences tab, on appelle récursivement la fonction
                       add_letters en accumulant les lettres en fonction de leur nombre.
                       Les indices du tableau d'occurences correspondant aux différentes lettres de l'alphabet, on y ajoute
                       65 afin d'avoir le caractère idoine dans la table ascii. *)
                    add_letters (acc_aux ^ (Char.escaped (Char.chr (i + 65)))) (j + 1)
                (* Lorsqu'on n'a plus de caractères à ajouter, on retourne l'accumulateur de la fonction occurrences_table_to_string_aux
                   contenant la concaténation de tous les accumulateurs des appels successifs de la fonction add_letters. *)
                else acc_aux
                in
                (* L'appel récursif de occurrences_table_to_string_aux va permettre de construire la chaine de caractères 
                   en utilisant un accumulateur qui va consister en la concaténation de l'ensemble des accumulateurs des appels
                   récursifs de la fonction add_letters sur les indices i du tableau d'occurrences tab. *)
                occurrences_table_to_string_aux (acc ^ (add_letters "" 0)) (i + 1)
        )
        else acc
        in
        occurrences_table_to_string_aux "" 0;;

(*
    Permet de compter le nombre de voyelles dans un mot.
    param: word -> mot à analyser
    return: le nombre de voyelles présentes dans le mot

    signature: val count_vowels : string -> int = <fun>
*)
let count_vowels word =
    let word_length = String.length word in
    let rec count_vowels_aux acc i =
        if i < word_length then(
            match Char.uppercase_ascii (String.get word i) with
            (* Si le caractère est une voyelle, alors on incrémente la valeur de l'accumulateur de la fonction *)
            | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y' -> count_vowels_aux (acc + 1) (i + 1)
            (* On ignore tous les autres types de caractères *)
            | _ -> count_vowels_aux acc (i + 1)
        )
        else acc
        in
        count_vowels_aux 0 0;;

(*
   Permet d'indiquer si un mot est viable ou non. Un mot est viable, c'est-à-dire éligible pour l'analyse par la chaine de Markov
   si le nombre de voyelles et le nombre de consonnes du mot sont équivalents à plus ou moins une différence près et si la taille 
   du mot est strictement supérieure à 1.
   param: word -> Le mot à analyser.
   return: true si le mot est viable false sinon.

   signature: val is_viable_word : string -> bool = <fun>
*)                
let is_viable_word word = 
    let word_length = String.length word in
    let number_vowels = count_vowels word in
    let number_consonants = word_length - number_vowels in
    if (number_vowels >= number_consonants && number_vowels <= number_consonants + 1) && word_length > 1 then true
    else false;;
   
(*
    Permet de déterminer les prénoms qui peuvent être des candidats idéaux pour former des anagrammes d'un nom donné
    par l'utilisateur. Un candidat idéal est un prénom dont toutes ses lettres sont contenues dans le nom de l'utilisateur
    et dont les lettres restantes (présentes dans le nom de l'utilisateur mais pas dans le prénom) peuvent former un nom de famille
    probable.
    param: h -> table de hashage contenant pour chaque duo clé/valeur un prénom de la base de données du programme
    comme clé et son tableau d'occurrences comme valeur.
    param: name -> nom entré par l'utilisateur à comparer avec l'ensemble de la banque de prénoms
    return: une table de hashage contenant pour chaque couple clé/valeur un prénom retenu pour l'étape suivante en tant que clé 
    et les lettres restantes du nom entré par l'utilisateur non utilisées par le prénom en tant que valeur. 
    
    signature: val find_first_names : (string, int array) Hashtbl.t -> string -> (string, string) Hashtbl.t = <fun>
*)   
let find_first_names h name = 
    let hashtable_size = Hashtbl.length h in
    let res = Hashtbl.create hashtable_size in
    (* On itère sur la liste des prénoms *)
    Hashtbl.iter (fun key value -> 
        let temp_name = Array.copy name in
        (* On regarde si peut former le prénom dont le tableau d'occurrences est contenu dans value avec les lettres contenues dans
           le tableau d'occurrences de name. *)
        if compare_words value temp_name then(
            (* Si oui, le prénom est un candidat potentiel pour le suite de l'algorithme.
               On récupère les lettres du nom name non utilisées par le prénom *)
            let remaining_letters = occurrences_table_to_string temp_name in
            (* et on regarde si les lettres restantes peuvent former un nom viable *)
            if is_viable_word remaining_letters then Hashtbl.add res key remaining_letters
        )
    ) h;
    res;;

(*
    Permet de créer une chaine de Markov contenant l'ensemble des occurrences de deux lettres successives
    param: value -> valeur d'initialisation de chaque clé de la chaine de Markov
    return: une chaine de Markov sous la forme d'une table de hashage où chaque clé (deux lettres successives) est associée à la valeur 'value'

    signature: val create_markov_chain : int -> (string, int) Hashtbl.t = <fun>
*)
let create_markov_chain value =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let alphabet_length = String.length alphabet in
    let markov_chain = Hashtbl.create (alphabet_length * alphabet_length) in
    let rec create_markov_chain_aux_one i = 
        if i < alphabet_length then(
            let rec create_markov_chain_aux_two j = 
                if j < alphabet_length then(
                let key = (Char.escaped alphabet.[i]) ^ (Char.escaped alphabet.[j]) in
                Hashtbl.add markov_chain key value;
                create_markov_chain_aux_two (j + 1)
                )
                in
                create_markov_chain_aux_two 0;
            create_markov_chain_aux_one (i + 1)
        )
        else markov_chain 
    in 
    create_markov_chain_aux_one 0;;

(*
    Permet de remplir une chaine de Markov à partir d'une liste de données.
    param: l -> liste des données utilisées pour remplir la chaine de Markov
    return: la chaine de Markov remplie à partir des données contenues dans la liste 'l'

    signature: val generate_markov_chain : string list -> (string, int) Hashtbl.t = <fun>
*)
        
let generate_markov_chain l = 
    let markov_chain = create_markov_chain 0 in
    let rec generate_markov_chain_aux l = 
        match l with
        | [] -> markov_chain
        | hd::tl -> (
            let word_length = String.length hd in
            let rec analyze_word i =
                if i < word_length - 1 then(
                    let key = (Char.escaped hd.[i]) ^ (Char.escaped hd.[i + 1]) in
                    let value = Hashtbl.find markov_chain key in
                    Hashtbl.replace markov_chain key (value + 1);
                    analyze_word (i + 1)
                )
                in
                analyze_word 0;
        generate_markov_chain_aux tl
        )
        in
        generate_markov_chain_aux l;;

(* 
    Permet de choisir la première lettre du patronyme de l'anagramme plausible du nom choisi par l'utilisateur.
    param: remaining_letters -> chaîne de caractères contenant l'ensemble des lettres parmi lesquelles choisir la première lettre du patronyme
    param: markov_chain -> chaîne de Markov permettant d'aider à choisir cette lettre
    return: l'indice de la position de la lettre choisie dans remaining_letters

    signature: val choose_first_letter string -> (string, int) Hashtbl.t -> int = <fun>
*)
let choose_first_letter remaining_letters markov_chain = 
    let letters_length = String.length remaining_letters in
    (* On fixe la seconde valeur à un nombre très élevé *)
    let rec choose_letters_aux i index mean_min =
        if i < letters_length then(
            (* On récupère la lettres dans les lettres restantes à l'indice i *)
            let letter = remaining_letters.[i] in
            (* On crée un tableau temporaire dans lequel on retire cette lettre *)
            let remaining_letters_aux = (String.sub remaining_letters 0 i) ^ (String.sub remaining_letters (i + 1) (letters_length - i - 1)) in
            (* On récupère la taille de la liste de lettres restantes *)
            let remaining_letters_aux_length = String.length remaining_letters_aux in
            (* 
                Permet de sommer l'ensemble des valeurs d'occurrences associées aux clés dans la chaîne de Markov où la lettre 'letter' suit l'ensemble des lettres contenues dans 'remaining_letters_aux'.
                param: acc -> accumulateur qui va contenir au fur et à mesure du parcours de 'remainig_letters_aux' la somme des valeurs d'occurrences
                param: j -> compteur allant de 0 à la taille de 'remaining_letters_aux' - 1
                return: la somme des valeurs d'occurrences, c'est-à-dire la valeur de acc
                
                signature: val sum_occurrences : int -> int -> int = <fun>
            *)
            let rec sum_occurrences acc j = 
                if j < remaining_letters_aux_length then(
                    let key = (Char.escaped remaining_letters_aux.[j]) ^ (Char.escaped letter) in
                    (* On récupère la valeur associée à la clé dans la chaîne de Markov *)
                    let number_occurrences = Hashtbl.find markov_chain key in
                    (* et on appelle récursivement la fonction en ajoutant la valeur récupérée à l'accumulateur *)
                    sum_occurrences (acc + number_occurrences) (j + 1)
                )
                else acc
                in
                (* On caste la somme retournée par la fonction 'sum_occurrences' en float afin de faire la moyenne *)
                let sum = float_of_int (sum_occurrences 0 0) in
                let mean = sum /. float_of_int remaining_letters_aux_length in
                (* Si la moyenne est plus petite que la plus petite moyenne actuellement contenue dans 'mean_min' 
                   alors, on garde en mémoire la valeur de l'indice de cette lettre dans 'remaining_letters' 
                   ainsi que la nouvelle valeur de la moyenne en nous passons à l'occurrence suivante. *)
                if mean < mean_min then choose_letters_aux (i + 1) i mean
                else choose_letters_aux (i + 1) index mean_min              
        )else index
        in 
        choose_letters_aux 0 0 10000.0;;

(* 
    Permet de choisir la dernière lettre du patronyme de l'anagramme plausible du nom choisi par l'utilisateur.
    param: remaining_letters -> chaîne de caractères contenant l'ensemble des lettres parmi lesquelles choisir la première lettre du patronyme
    param: markov_chain -> chaîne de Markov permettant d'aider à choisir cette lettre
    return: l'indice de la position de la lettre choisie dans remaining_letters

    signature: val choose_last_letter string -> (string, int) Hashtbl.t -> int = <fun>
*)
let choose_last_letter remaining_letters markov_chain = 
    let letters_length = String.length remaining_letters in
    (* On fixe la seconde valeur à un nombre très élevé *)
    let rec choose_letters_aux i index mean_max =
        if i < letters_length then(
            (* On récupère la lettres dans les lettres restantes à l'indice i *)
            let letter = remaining_letters.[i] in
            (* On crée un tableau temporaire dans lequel on retire cette lettre *)
            let remaining_letters_aux = (String.sub remaining_letters 0 i) ^ (String.sub remaining_letters (i + 1) (letters_length - i - 1)) in
            (* On récupère la taille de la liste de lettres restantes *)
            let remaining_letters_aux_length = String.length remaining_letters_aux in
            (* 
                Permet de sommer l'ensemble des valeurs d'occurrences associées aux clés dans la chaîne de Markov où la lettre 'letter' suit l'ensemble des lettres contenues dans 'remaining_letters_aux'.
                param: acc -> accumulateur qui va contenir au fur et à mesure du parcours de 'remainig_letters_aux' la somme des valeurs d'occurrences
                param: j -> compteur allant de 0 à la taille de 'remaining_letters_aux' - 1
                return: la somme des valeurs d'occurrences, c'est-à-dire la valeur de acc
                
                signature: val sum_occurrences : int -> int -> int = <fun>
            *)
            let rec sum_occurrences acc j = 
                if j < remaining_letters_aux_length then(
                    let key = (Char.escaped remaining_letters_aux.[j]) ^ (Char.escaped letter) in
                    (* On récupère la valeur associée à la clé dans la chaîne de Markov *)
                    let number_occurrences = Hashtbl.find markov_chain key in
                    (* et on appelle récursivement la fonction en ajoutant la valeur récupérée à l'accumulateur *)
                    sum_occurrences (acc + number_occurrences) (j + 1)
                )
                else acc
                in
                (* On caste la somme retournée par la fonction 'sum_occurrences' en float afin de faire la moyenne *)
                let sum = float_of_int (sum_occurrences 0 0) in
                let mean = sum /. float_of_int remaining_letters_aux_length in
                (* Si la moyenne est plus grande que la plus grande moyenne actuellement contenue dans 'mean_max' 
                   alors, on garde en mémoire la valeur de l'indice de cette lettre dans 'remaining_letters' 
                   ainsi que la nouvelle valeur de la moyenne en nous passons à l'occurrence suivante. *)
                if mean > mean_max then choose_letters_aux (i + 1) i mean
                else choose_letters_aux (i + 1) index mean_max             
        )else index
        in 
        choose_letters_aux 0 0 0.0;;

(* Permet de retourner une table de hashage contenant un couple clé valeur avec comme clé un prénom et comme valeur un nom de famille plausible obtenu par le réarrangement des lettres de ce dernier 
   grâce à une chaine de Markov.
   param: names -> table de hashage contenant pour chaque couple clé/valeur un prénom comme clé et une liste de lettres à réarranger grâce à cette fonction afin de former un nom de famille plausible
   param: markov_chain -> chaine de Markov permettant de générer des noms de famille plausibles pour les valeurs de la table de hashage 'names'
   return: une table de hashage contenant pour chaque couple clé/valeur un prénom comme clé et un nom de famille plausible comme valeur  
        
   signature: val generate_correct_names : (string, string) Hashtbl.t -> (string, int) Hashtbl.t -> (string, string) Hashtbl.t = <fun>
*)
let generate_correct_names names markov_chain = 
    let names_length = Hashtbl.length names in
    let names_res = Hashtbl.create names_length in
    Hashtbl.iter (fun key value -> (
        let word_length = String.length value in
        (* on sélectionne l'indice parmi les lettres du nom à former qui servira de première lettre du nom de famille *)
        let index_first_letter = choose_first_letter value markov_chain in
        (* On récupère le caractère associé à cet indice *)
        let first_letter = value.[index_first_letter] in
        (* On l'ajoute au début du nom que l'on souhaite former *)
        let name = "" ^ (Char.escaped first_letter) in
        (* On retire cette lettre de la chaine des lettres restantes *)
        let remaining_letters_one = (String.sub value 0 index_first_letter) ^ (String.sub value (index_first_letter + 1) (word_length - index_first_letter - 1)) in
        (* On récupère la taille des lettres restantes pour former le nom de famille *)
        let remaining_letters_one_length = String.length remaining_letters_one in
        (* on sélectionne l'indice parmi les lettres restantes qui servira de dernière lettre du nom de famille *)
        let index_last_letter = choose_last_letter remaining_letters_one markov_chain in
        (* On récupère le caractère associé à cet indice *)
        let last_letter = remaining_letters_one.[index_last_letter] in
        (* et on la retire de la liste des lettres restantes *)
        let remaining_letters_two = (String.sub remaining_letters_one 0 index_last_letter) ^ (String.sub remaining_letters_one (index_last_letter + 1) (remaining_letters_one_length - index_last_letter- 1)) in
        let rec create_name acc letters = 
            let letters_length = String.length letters in
            if letters_length > 1 then(
                (* On récupère la dernière lettre ajoutée à l'accumulateur *)
                let last_added_letter = acc.[String.length acc - 1] in
                (* 
                    fonction qui va permettre de déterminer quelle est la lettre contenue dans la liste de lettres restantes 'letters' qui a le plus de chance de suivre la lettre 'last_added_letter' dans 
                   le nom de famille.
                   param: letter_index -> indice de la lettre contenue dans 'letters' qui a le plus de chance de suivre la lettre 'last_added_letter'
                   param: i -> compteur sur la liste des lettres restantes
                   param: max -> la valeur du nombre d'occurrences où la lettre 'last_added_letter' est suivie directement par la lettre située à l'indice 'letter_index' dans la liste des lettres restantes
                   return: l'indice de la lettre qui a le plus de chances de suivre 'last_added_letter', c'est-à-dire la valeur de 'letter_index'

                   signature: val find_next_letter : int -> int -> int -> int = <fun>
                *)
                let rec find_next_letter letter_index i max = 
                    if i < letters_length then(
                        (* on recrée la clé en concaténant la dernière lettre qui a été ajoutée au nom avec une lettre contenue dans la liste des lettres restant à ajouter au nom *)
                        let key = (Char.escaped last_added_letter) ^ (Char.escaped letters.[i]) in
                        (* On récupère le nombre d'occurrences dans la chaine de Markov où la lettre 'last_added_letter' est suivie par la lettre à ajouter *)
                        let number_occurrences = Hashtbl.find markov_chain key in
                        (* Si le nombre d'occurrences dépasse celui déjà présent, alors la lettre à ajouter devient le candidat le plus plausible à être ajouté après la lettre 'last_added_letter' *)
                        if number_occurrences > max then(
                            find_next_letter i (i + 1) number_occurrences
                        )
                        (* sinon on garde le candidat déterminé précédemment et on passe à la lettre suivante *)
                        else find_next_letter letter_index (i + 1) max
                    )
                    else letter_index
                in
                (* On récupère l'indice de la lettre qui a la plus grande probabilité de suivre la dernière lettre qui a été ajoutée au nom *)
                let next_letter_index = find_next_letter 0 0 0 in
                (* et on ajoute la lettre correspondant à cet indice à la suite de l'accumulateur acc tout en la supprimant des lettres restant à ajouter au nom *)
                (* puis on effectue après cela une récursivité pour déterminer la suite du nom de famille *)
                create_name (acc ^ (Char.escaped letters.[next_letter_index])) ((String.sub letters 0 next_letter_index) ^ (String.sub letters (next_letter_index + 1) (letters_length - next_letter_index - 1)))
            )
            (* On ajoute à la table de hashage le nom corrigé associé au prénom contenu dans 'key' et qui correspond à la concaténation de l'accumulateur contenant les lettres du nom ajoutés au fur et à mesure
               des appels récursifs de la fonction 'create_name' avec la dernière lettre qui reste dans la liste des lettres ajouées pour la création du nom et avec la concaténation de la dernière lettre du 
               nom qui nous avions déterminé au début *)
            else Hashtbl.add names_res key (acc ^ letters ^ (Char.escaped last_letter))
        in
        create_name name remaining_letters_two 

    )) names;
    names_res;;

(* Bloc principal du programme *)
let () =

    (* On demande à l'utilisateur d'entrer le nom à partir duquel il souhaite déterminer les anagrammes *)
    print_string "Entrez un nom à partir duquel vous souhaitez déterminer les anagrammes: ";
    let name = read_line () in
    
    (* On lance le chronomètre sur le temps d'exécution du programme *)
    let t1 = Unix.gettimeofday () in

    (* On récupère le tableau du nombre d'apparition de chaque lettre dans ce nom *)
    let tab_occurrences_name = count_occurrences_letters name in
    
    (* On récupère l'intégralité des prénoms situés dans la banque de prénoms *)
    let first_names = get_first_names "banque_prenoms_complete.txt" in
    
    (* On récupère une table de hashage associant à chaque prénom de la banque de données un tableau du nombre d'apparition de chaque lettre dans ce prénom *)
    let tab_occurrences_first_names = count_occurences first_names in

    (* On récupère tous les prénoms candidats à former des anagrammes viables ainsi que le reste des lettres non utilisées par ces dits prénoms. *)
    (* retourne une table de hashage avec clé = "prénom", valeur = "reste des lettres du nom à partir duquel on détermine les anagrammes 
       non présentes dans le prénom "*)
    let correct_first_names = find_first_names tab_occurrences_first_names tab_occurrences_name in

    (* On récupère une chaine de Markov générée à partir de la banque de prénoms utilisée *)
    let markov_chain = generate_markov_chain first_names in

    (* On récupère enfin l'ensemble des anagrammes du nom choisi par l'utilisateur sous le forme de clé/valeur avec la clé
       un prénom et la valeur un nom de famille plausible obtenue en réarangeant le reste des lettres non présentes dans le prénom *)
    let correct_names = generate_correct_names correct_first_names markov_chain in
    
    (* Créatio nd'un fichier de sortie au format .dat qui contiendra les résultats du programme *)
    let filename = name ^ "_anagrammes.dat" in
    let output_file = open_out filename in

    (* On affiche dans la console la liste des anagrammes du nom trouvés par le programme et on exporte les résultats dans un fichier .dat *)
    Printf.printf "\n\nVoici une liste de noms plausibles qui sont anagrammes de '%s' (%d résultat(s)) :\n\n" name (Hashtbl.length correct_names);
    Printf.fprintf output_file "Voici une liste de noms plausibles qui sont anagrammes de '%s' (%d résultat(s)) :\n\n" name (Hashtbl.length correct_names);
    Hashtbl.iter (fun key value -> (
        Printf.printf "%s %s\n" key value;
        Printf.fprintf output_file "%s %s\n" key value
        )) correct_names;

    (* On arrête le chronomètre sur le temps d'exécution du programme *)
    let t2 = Unix.gettimeofday () in

    (* et on calcule son temps d'exécution total *)
    let execution_time = t2 -. t1 in
    
    (* Et on affiche le temps d'exécution du programme *)
    Printf.printf "\nTemps d'exécution du programme: %.3f secondes\n" execution_time;
    Printf.fprintf output_file "\nTemps d'exécution du programme: %.3f secondes\n" execution_time;
    (* et on ferme le fichier d'exportation des résultats *)
    close_out output_file;;