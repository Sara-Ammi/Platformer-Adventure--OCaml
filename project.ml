open Engine

let red_string s = "\027[31m" ^ s ^ "\027[0m" (* Texte rouge *)
let green_string s = "\027[32m" ^ s ^ "\027[0m" (* Texte vert *)
let yellow_string s = "\027[33m" ^ s ^ "\027[0m" (* Texte jaune *)
let bleu_string s = "\027[34m" ^ s ^ "\027[0m" (* Texte bleu *)
let pink_string s = "\027[35m" ^ s ^ "\027[0m" (* Texte rose *)
let cyran_string s =  "\027[36m" ^ s ^ "\027[0m" (* Texte cyran *)

(*la fonction build_block*)
let string_of_char c =
  String.make 1 c

let rec build_ligne c w =
  if w = 1 then (string_of_char c)
  else (string_of_char c)^build_ligne c (w-1)
let rec build_block c w h =
  if h = 1 then build_ligne c w
  else  (build_ligne c w)^"\n"^build_block c w (h-1)


let personnage =
    " ⚇ \n/[]\\\n /\\ " [@@ocamlformat "disable"]

let ob1 = build_block ')' 45 1
let ob2 = build_block ')' 51 1
let ob3 = build_block '{' 25 2
let ob4 = build_block ')' 10 1
let ob5 = build_block '|' 82 1
let ob6 = build_block ')' 25 1
let ob7 = build_block '{' 20 2
let ob8 = build_block ')' 20 1
let ob9 = build_block ')' 131 1
let ob10 = build_block '|' 15 1
let ob11 = build_block '{' 25 1
let mur1 = build_block '#' 133 1
let mur2 = build_block '#' 1 29



let list_mur =  [ ((5.0, 35.0), mur1) ; ((5.0, 7.0), mur2) ; ((137.0, 7.0), mur2) ;  ((5.0, 7.0), mur1) ]
let obstacle = [ ((5.0, 35.0), mur1) ; ((5.0, 7.0), mur2) ; ((137.0, 7.0), mur2) ;  ((5.0, 7.0), mur1) ; 
((70.0, 11.0), ob3);((91.0, 17.0), ob7);((32.0 , 29.0), ob11)] (*liste des obstacles*)

let obstacle2 = [ ((5.0, 35.0), mur1) ; ((5.0, 7.0), mur2) ; ((137.0, 7.0), mur2) ;  ((5.0, 7.0), mur1) ; 
((40.0, 22.0), ob11) ]

let air_up = build_block '_' 133 1 
let enemi =
  "    O\n  /(|)\\\n    ^" [@@ocamlformat "disable"]
let enemi_2 = "(¤-¤)"
let plante = " ^ \n^|^"
let plante_2 = "|\nX"
let fruit = "O-#"
let air = "|" 
let porte = "████\n████"

let winner = "            ,--.
,--.   ,--. `--' ,--,--,  ,--,--,   ,---.  ,--.--.
|  |.'.|  | ,--. |      \ |      \ | .-. : |  .--'
|   .'.   | |  | |  ||  | |  ||  | \   --. |  |
'--'   '--' `--' `--''--' `--''--'  `----' `--'"

let it_over =
  "████████  ███████  █████  █████  ███████      ██████   ██       ██  ███████  ███████\n\
   ██        ██   ██  ██   ██   ██  ██          ██    ██   ██     ██   ██       ██   ██\n\
   ██  ████  ███████  ██   ██   ██  ███████     ██    ██    ██   ██    ███████  ███████\n\
   ██    ██  ██   ██  ██        ██  ██          ██    ██     ██ ██     ██       ████\n\
   ████████  ██   ██  ██        ██  ███████      ██████        █       ███████  ██  ██" [@@ocamlformat "disable"]

(* Calcule la taille en nombre de caractères horizontaux et verticaux du message*)
let h_width, h_height = get_bounds personnage

   
type key =
   | LeftArrow
   | RightArrow
   | UpArrow
   | DownArrow
   | Char of char
   | Special of string

(* Fonction pour comparer une chaîne avec une clé *)
let compare_key_string key str =
  match key with
  | Char c -> String.length str = 1 && str.[0] = c
  | Special s -> s = str
  | _ -> false

let pos2 = ref (80.0, 17.0)  (* Position initiale de l'ennemi 1 *)
let posE = ref (20.0 , 22.0) (* Position initiale de l'ennemi 2 *)  
let pos3 = ref (123.0, 21.0)  (* Position initiale de l'ennemi 3 *)  
let posPro = ref (1.0, 7.0) (* Position du projectile *)
let last_direction = ref 0.5 (* la derniere direction de l'enemi 1 *)
let last_direction_2 = ref 0.5 (* la derniere direction de l'enemi 2 *)
let posAir = ref (137.0, 3.0) (* la position de l'air au niveau 1*)
let posAir2 = ref (137.0, 3.0) (* la position de l'air au niveau 2*)

let fruit_list = ref [((7.0 , 18.0) , yellow_string fruit );((55.5, 27.0) , yellow_string fruit );((75.0, 14.0), yellow_string fruit) ;
((110.0, 24.0), yellow_string fruit);((112.0, 32.0), yellow_string fruit)] (* liste des fruits a recolter *)

let fruit_list2 = ref [((23.0 , 15.0) , yellow_string fruit );((123.0, 19.0) , yellow_string fruit );((85.0, 17.0), yellow_string fruit) ;
((47.0, 24.0), yellow_string fruit)] (* liste des fruits a recolter *)

let plante_list = [((105.0, 22.0), green_string plante) ;((115.0, 22.0), green_string plante) ; 
((50.0, 10.0), green_string plante) ;(( 110.0 , 33.0), pink_string plante_2) ;(( 45.0 , 33.0), pink_string plante_2) ] (* liste des plantes nuisibles *)

let plante_list2 = [((55.0, 22.0), green_string plante) ;((90.0, 15.0), green_string plante) ; 
((25.0, 7.0), green_string plante) ] (* liste des plantes nuisibles *)

let enemi_list_v = [(pos2 , enemi)]
let enemi_list_h = [(posE , enemi_2)]

let enemi_list_2 = [( pos3 , enemi_2)]

let vies = ref 4 (* les vies *)
let list_vie = ref [ ((15.0, 0.5 ) , personnage) ;  ((20.0, 0.5)  , personnage) ;  ((25.0, 0.5), personnage) ;  ((30.0, 0.5), personnage)] (* liste de vies a afficher *)
let fruits_attrapes = ref 0 (* les nombre de fruits recolter *)
let niveau = ref 1 (* le niveau actuel *)

(* la fonction magnify *)
(*
let rec get_position s i j =
  if i < j then
    if String.get s i = '@' then i
    else get_position s (i+1) j
  else
     0

let rec my_list nx ny s i j l xc =
  if i = j then l
  else
    let c = String.get s i in
    if c <> ' ' && c <> '\n' && c <> '@' then
    my_list nx ny s (i+1) j ((( xc , 0.), (build_block c nx ny)) :: l ) ( xc -. (float_of_int nx))
  else
    my_list nx ny s (i+1) j l xc

let magnify nx ny s =
  let myX = float_of_int (nx * (get_position s 0 (String.length s))) in
  let myY = float_of_int (ny * (get_position s 0 (String.length s))) in
 (( myX , myY ) , my_list nx ny s 0 (String.length s) [] 24.)
 *)

 (* la position initiale du personnage *)
let init_state = ((7.0, 8.0) , (0.0 , 0.0))

(* verifie si le personnage est sur un obstacle traversable *)
let rec on_top pos l = match l with
| [] -> false 
| (obp , s ) :: f ->
  let ( px , py ) = pos in 
  let (obx , oby) = obp in 
  if abs_float (px -. obx) <= fst (get_bounds s) && px >= (obx -. 0.75 ) && py -. oby <= 1.5 &&  py -. oby >= 1.0 then true 
  else on_top pos f

(* verifie si le personnage est sur un obstacle fixe *)
let rec on_top_f pos l = match l with
  | [] -> false 
  | (obp , s ) :: f ->
    let ( px , py ) = pos in 
    let (obx , oby) = obp in 
    if abs_float (px -. obx) <= fst (get_bounds s) && px >= (obx -. 0.75 ) && py -. oby <= 2.5 &&  py -. oby >= 1.0 then true 
    else on_top_f pos f

(* supprime le fruit recolté *)
let rec catched_fruit pos l =
      match l with
      | [] -> []
      | ((fx, fy), s) :: f ->
          let (px, py) = pos in
          if abs_float (fx -. px) < 2.0 && abs_float (fy -. py) <= 1.0 && abs_float (fy -. py) < 1.5 then (
            fruits_attrapes := !fruits_attrapes + 1;
            catched_fruit pos f
          ) else
            ((fx, fy), s) :: catched_fruit pos f
 

(* supprime une vie si le personnage touche à l'ennemi 1 *)
  let rec game_over_v pos v l =  match l with
  | [] -> v
  | e :: f ->
  let (p , s) = e in 
  let (fx , fy) = !p in 
  let (px, py) = pos in
  if abs_float (fx -. px) <= 0.35 && abs_float (fy -. py) <= 2.0 then game_over_v pos (v - 1) f
  else game_over_v pos v f

(* supprime une vie si le personnage touche à l'ennemi 2 *)
  let rec game_over_h pos v l =  match l with
  | [] -> v
  | e :: f ->
  let (p , s) = e in 
  let (fx , fy) = !p in 
  let (px, py) = pos in
  if abs_float (fx -. px) <= 2.0 && abs_float (fy -. py) < 0.35 then game_over_h pos (v - 1) f
  else game_over_h pos v f

(* supprime une vie si le personnage touche à une plante *)
  let rec game_over_aux pos v l =  match l with
  | [] -> v
  | e :: f ->
  let (p , s) = e in 
  let (fx , fy) = p in 
  let (px, py) = pos in
  if abs_float (fx -. px) <= 0.5 && abs_float (fy -. py) <= 2.0 then game_over_aux pos (v - 1) f
  else game_over_aux pos v f

(* adapte la liste a afficher avec le nombre de vies restantes *)
let rec remaining v l = match l with 
  | [] -> []
  | e :: f -> if v = 0 then remaining v f
  else e :: remaining (v-1) f
 

let update (position, (xincr, yincr)) (key_mod : (Engine.key * bool * bool) option) =

if ( !niveau = 1) then (

  (*niveau 1*)

 (* le deplacement de l'ennemi 1 *)
  let ex = fst !pos2 in 
  let enemi_speed = 0.5 in

(* Détermine la direction en fonction de la position actuelle *)
let direction =
  if ex = 84.0  then
    -.enemi_speed  (* Si l'ennemi est à droite, déplacez-le vers la gauche *)
  else 
    if ex = 50.0 then
    enemi_speed   (* Si l'ennemi est à gauche, déplacez-le vers la droite *)
  else
    !last_direction
in

(* Mise à jour de la position de l'ennemi *)
let enemi_position = (!pos2 +.. (direction, 0.0)) in

(* Enregistrez la dernière direction pour une utilisation future *)
last_direction := direction ;
pos2 := enemi_position ;

(* le deplacement de l'ennemi 2 *)

let ex = snd !posE in 
let enemi_speed = 0.5 in

(* Détermine la direction en fonction de la position actuelle *)
let direction =
  if ex = 29.0  then
    -.enemi_speed  (* Si l'ennemi est à droite, déplacez-le vers la gauche *)
  else 
    if ex = 23.0 then
    enemi_speed   (* Si l'ennemi est à gauche, déplacez-le vers la droite *)
  else
    !last_direction_2
in

(* Mise à jour de la position de l'ennemi *)
let enemi_position = (!posE +.. (0.0, direction)) in

(* Enregistrez la dernière direction pour une utilisation future *)
last_direction_2 := direction ;

(* Mise à jour de la position de l'ennemi *)
posE := enemi_position ;

(* Mise à jour de l'air *)

let air_pos = (!posAir -.. (0.02 , 0.0)) in 
posAir := air_pos ;

(* Mise à jour du nombre de vies *)
let v = !vies in 
let n_list = ! list_vie in 
list_vie := remaining v n_list ; 
vies := game_over_v position v enemi_list_v ;
 
let v = !vies in 
let n_list = ! list_vie in 
list_vie := remaining v n_list ; 
vies := game_over_h position v enemi_list_h ;

let v = !vies in 
let n_list = !list_vie in 
list_vie := remaining v n_list; 
vies := game_over_aux position v plante_list;

(* liste des obstacles traversables *)
let ob = [((92.0, 12.0), ob1) ; ((20.0, 10.0), ob2); ((128.0 , 15.0),ob4); ((40.0, 16.0),ob5) ; ((6.5, 16.0),ob6)
; ((6.5, 19.0),ob8) ; ((6.5, 22.0),ob9) ; ((122.0,25.0),ob10) ; ((107.0, 28.0),ob6) ; ((32.0 , 25.0),ob2) ;
((32.0 , 29.0),ob11) ; ((57.5, 28.0) ,ob4) ] in 

(* liste des obstacles fixes *)
let obf = [((5.0, 7.0), mur1) ; ((70.0, 11.0), ob3) ; ((91.0, 17.0),ob7)] in
  let incr = match key_mod with 
  | None -> (0.0 , 0.0)
  | Some (a, b , c) -> match a with 
  | Char 'd' ->  (xincr +. 2.0,  0.0)
  | Char 'q' ->  (xincr -. 2.0, 0.0)
  | Char 'z' -> if on_top position ob || on_top_f position obf then (0.0 , yincr +. 110.0) else (0.0, 0.0)
  | _ -> (0.0, 0.0)
  in 
  let forces = if on_top position ob then 
  [] else [(0.0 , -170.0)] in 

  (* changement de niveau *)
  if (collision (position , personnage) ((37.0, 33.0), cyran_string porte)) && (catched_fruit position !fruit_list) = [] then  niveau := 2;

  (* mise a jour de la position du personnage *)
  let new_pos, new_vitesse, _ = update_physics (position , personnage ) incr forces obstacle in
  (new_pos, new_vitesse)

  ) else (

  (*niveau 2*)
 
 (* le deplacement de l'ennemi 1 *)
 let ex = fst !pos3 in 
 let enemi_speed = 0.5 in

(* Détermine la direction en fonction de la position actuelle *)

let direction =
 if ex = 133.0 then
   -.enemi_speed  (* Si l'ennemi est à droite, déplacez-le vers la gauche *)
 else 
   if ex = 121.0 then
   enemi_speed   (* Si l'ennemi est à gauche, déplacez-le vers la droite *)
 else
   !last_direction
in

(* Mise à jour de la position de l'ennemi *)
let enemi_position = (!pos3 +.. (direction, 0.0)) in

(* Enregistrez la dernière direction pour une utilisation future *)
last_direction := direction ;
pos3 := enemi_position ;

(* Mise à jour de l'air *)

let air_pos = (!posAir2 -.. (0.02 , 0.0)) in 
posAir2 := air_pos ;

(* Mise à jour du nombre de vies *)
let v = !vies in 
let n_list = ! list_vie in 
list_vie := remaining v n_list ; 
vies := game_over_v position v enemi_list_2 ;
 
let v = !vies in 
let n_list = !list_vie in 
list_vie := remaining v n_list; 
vies := game_over_aux position v plante_list2;

  (* liste des obstacles traversables *)
let ob = [((20.0, 13.0), ob4); ((45.0, 15.0), ob4); ((70.0, 15.0), ob4); ((110.0, 15.0), ob4); ((110.0, 19.0), ob4); ((50.0, 11.0), ob4);
((30.0, 11.0), ob4);((60.0, 13.0), ob4);((80.0, 15.0), ob4);((90.0, 15.0), ob4);((120.0, 17.0), ob4);((40.0, 9.0), ob4);((100.0, 13.0),ob4);
((122.0, 20.0),ob10);((122.0, 23.0),ob10)] in  

(* liste des obstacles fixes *)
let obf = [((5.0, 7.0), mur1) ; ((40.0, 22.0), ob11) ] in
  let incr = match key_mod with 
  | None -> (0.0 , 0.0)
  | Some (a, b , c) -> match a with 
  | Char 'd' ->  (xincr +. 2.0,  0.0)
  | Char 'q' ->  (xincr -. 2.0, 0.0)
  | Char 'z' -> if on_top position ob || on_top_f position obf then (0.0 , yincr +. 110.0) else (0.0, 0.0)
  | _ -> (0.0, 0.0)
  in 
  let forces = if on_top position ob then 
  [] else [(0.0 , -170.0)] in 

  (* changement de niveau *)
  if (collision (position , personnage) ((132.0, 24.0), cyran_string porte)) && (catched_fruit position !fruit_list2) = [] then  niveau := 3;

  (* mise a jour de la position du personnage *)
  let new_pos, new_vitesse, _ = update_physics (position , personnage ) incr forces obstacle2 in
  (new_pos, new_vitesse)


  )

  (* fonction d'affichage *)

  let affiche (pos, _) =

    let v = !vies in 
    let (x, y) = !posAir in
    let (x2 , y2 ) = !posAir2 in 

    (* l'affichage du niveau 1*)
    if (!niveau = 1 && v > 0 && x > 6.0) then (

      let n_list = !fruit_list in 
      fruit_list := catched_fruit pos n_list;  
 
      [
        (pos, personnage);
        ((37.0, 33.0), cyran_string porte);
        ((92.0, 12.0), red_string ob1);
        ((20.0, 10.0), red_string ob2);
        ((128.0, 15.0), red_string ob4);
        ((40.0, 16.0), green_string ob5);
        ((6.5, 16.0), red_string ob6);
        ((91.0, 17.0), red_string ob7);
        ((6.5, 19.0), red_string ob8);
        ((6.5, 22.0), red_string ob9);
        ((122.0, 25.0), green_string ob10);
        ((107.0, 28.0), red_string ob6) ;
        ((32.0, 25.0), red_string ob2) ;
        ((57.5, 28.0), red_string ob4) ;
        ((5.0, 3.0), "(") ;
        ((6.0, 5.0), bleu_string ("AIR :"));
        ((6.0, 4.0), air_up);
        ((6.0, 3.0), air_up);
        (!posAir, air);
        ((0.0, 0.0), "1");
        ((width -. 1.0, 0.0), "2");
        ((width -. 1.0, height -. 1.0), "3");
        ((0.0, height -. 1.0), "4");
        ((6.0, 1.5), bleu_string  ("Vies : "));
        ((1.0, height -. 2.0), "niveau : "^ string_of_int !niveau);
        ((30.0, height -. 2.0), "score : " ^ (string_of_int !fruits_attrapes));
      ] @ [(!pos2, yellow_string enemi) ; (!posE, bleu_string enemi_2)] @ plante_list @ !fruit_list @ !list_vie @ obstacle
    ) else if (!niveau = 2 && v > 0 && x2 > 6.0 ) then (
      let n_list = !fruit_list2 in 
      fruit_list2 := catched_fruit pos n_list; 
      (* passage au niveau 2 *)
    [
      (pos , personnage);
      ((20.0, 13.0), red_string ob4);
      ((45.0, 15.0), red_string ob4);
      ((70.0, 15.0), red_string ob4);
      ((110.0, 15.0), red_string ob4);
      ((110.0, 19.0), red_string ob4);
      ((50.0, 11.0), red_string ob4);
      ((30.0, 11.0), red_string ob4);
      ((60.0, 13.0), red_string ob4);
      ((80.0, 15.0), red_string ob4);
      ((90.0, 15.0), red_string ob4);
      ((120.0, 17.0), red_string ob4);
      ((40.0, 9.0), red_string ob4);
      ((100.0, 13.0), red_string ob4);
      ((122.0, 20.0), green_string ob10);
      ((122.0, 23.0),green_string ob10);
      ((132.0, 24.0),  cyran_string porte);
      ((5.0, 3.0), "(");
      ((6.0, 5.0), bleu_string ("AIR :"));
      ((6.0 ,4.0), air_up);
      ((6.0 , 3.0), air_up);
      (!posAir2 , air);
      ((0.0, 0.0), "1");
      ((width -. 1.0, 0.0), "2");
      ((width -. 1.0, height -. 1.0), "3");
      ((0.0, height -. 1.0), "4");
      ((1.0, height -. 2.0), "niveau : "^ string_of_int !niveau);
      ((6.0, 1.5), bleu_string  ("Vies : "));
      ((30.0, height -. 2.0), "score : " ^ (string_of_int !fruits_attrapes));
    ] @ obstacle2 @ !fruit_list2  @ !list_vie @ [!pos3 , pink_string enemi_2]  @ plante_list2
    ) else if (!niveau = 3 && v > 0 ) then (
      [
     ((45.0 , 20.0) , winner);
     ((0.0, 0.0), "1");
     ((width -. 1.0, 0.0), "2");
     ((width -. 1.0, height -. 1.0), "3");
     ((0.0, height -. 1.0), "4");
] @ list_mur
    )else (
      (* perdu *)
      [
        ((30.0 , 20.0) , it_over);
        ((0.0, 0.0), "1");
        ((width -. 1.0, 0.0), "2");
        ((width -. 1.0, height -. 1.0), "3");
        ((0.0, height -. 1.0), "4");
      ] @ list_mur
    )
 
(* Appelle de la boucle principale *)
let _ = loop init_state update affiche