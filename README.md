

# Introduction

Le but de ce TP est de se familiariser avec `LWT`. Le premier exercice peut se
faire dans `utop`. Le deuxième exercice se fait en complétant le fichier
`server.ml`.

Dans `utop`, pour la bibliothèque `lwt` est chargée par défaut, mais nous
utiliserons ici les extensions de programmations `UNIX` (`stdout`, `printf`,&#x2026;)
il faut donc charger ces extensions. Au démarrage de `utop` indiquer:

    #require "lwt.unix";; (* bien écrire le dièse *)
    open Base;;

Ne pas hésiter à consulter la documentation <https://ocsigen.org/lwt/3.2.1/api/Lwt>


# *Ring the alarm&#x2026;*

1.  Écrire une fonction qui prend un flottant `f` et renvoie un thread qui attend
    `f` secondes avant d&rsquo;afficher `"Debout f !"` (la lettre `f` doit être remplacée
    par sa valeur). Fonctions à utiliser :
    -   `Lwt_unix.sleep : float -> unit Lwt.t`
    -   `Lwt.bind : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t`
    -   `Lwt_io.write : Lwt_io.output_channel -> string -> unit Lwt.t` ou mieux `Lwt_io.fprintf` qui marche comme `fprintf` en `C`.
    -   `Lwt_io.stdout : Lwt_io.output_channel` la sortie standard
    -   `Float.to_string`
2.  Écrire un code qui lance en *parallèle* trois alarmes avec des temps différents. Fonctions à utiliser :
    -   `Lwt.join : unit Lwt.t list -> unit Lwt.t`
    -   si vous n&rsquo;utilisez pas utop, `Lwt_main.run : 'a Lwt.t -> 'a` mais dans ce cas&#x2026; vous êtes tout seul
3.  Écrire une fonction récursive qui crée un `join` de `f` alarmes (de temps 1 à `f`), et la tester.


# *When ya gonna ring it, when ya gonna ring it&#x2026;*

1.  Écrire une fonction qui crée un thread qui récupère un flottant donné par
    l&rsquo;utilisateur puis la tester (en affichant ce flottant). Fonctions et valeurs à
    utiliser :
    -   `Lwt_io.read_line : Lwt_io.input_channel -> string Lwt.t`
    -   `Lwt_io.stdin : Lwt_io.input_channel`
    -   `Lwt.return : 'a -> 'a Lwt.t`
    -   `Float.of_string`
2.  Écrire une fonction `interactive_alarm : unit -> unit Lwt.t` qui demande à l&rsquo;utilisateur un flottant `f` et lance `f`
    alarmes avec ce flottant (réutiliser les fonctions écrites précédemment)
3.  Écrire une fonction récursive `rec_alarm` qui demande un temps à
    l&rsquo;utilisateur et lance une alarme, et recommence quand l&rsquo;alarme est finie.
4.  Ce qui est dommage, c&rsquo;est que lorsque l&rsquo;utilisateur a demandé une alarme, il
    doit attendre qu&rsquo;elle termine avant de pouvoir en lancer une nouvelle. Écrire
    une fonction qui prend en paramètre le nombre de `rec_alarm` à lancer en
    *parallèle*.
5.  Ce qui est dommage,c&rsquo;est qu&rsquo;on est limités pour le nombre de `rec_alarm` en
    parallèle. Écrire une fonction `rec_alarm2` qui attend un flottant, et
    lorsqu&rsquo;elle le reçoit, lance une alarme et sans attendre demande un nouveau
    flottant.


# Question pour un netcat

Cet exercice est prévu pour un TP. L&rsquo;objectif est d&rsquo;approfondir avec les
fonctions de `Lwt_list` et la fonction `Lwt.choose`.

Le sujet est la réalisation d&rsquo;un jeu de questions/réponses entre un serveur et
des participants (deux pour faire simple).

La communication utilise des sockets, la partie serveur est faite en ocaml grâce
à la libraire `=Lwt_unix` ; la partie client est réalisée avec l&rsquo;utilitaire
`netcat`. Tout d&rsquo;abord définissons un type qui représentera les joueurs :

    type player = {input: Lwt_io.input_channel; output: Lwt_io.output_channel}

Un joueur est donc représenté par un canal de communication d&rsquo;entrée, et d&rsquo;un
canal de sortie (entrée et sortie du point de vue du serveur, attention). Voici
la fonction qui crée un thread qui renvoie une liste de `player` :

    open Base
    
    type player = {input: Lwt_io.input_channel; output: Lwt_io.output_channel}
    
    let (>>=) = Lwt.(>>=)
    let return = Lwt.return
    
    
    let getPlayers n : player list Lwt.t =
      let sockaddr = Lwt_unix.ADDR_INET (UnixLabels.inet_addr_loopback, 3003) in
      let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      Lwt_unix.set_close_on_exec sock ;
      Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true ;
      Lwt_unix.bind sock sockaddr >>= fun () ->
      Lwt_unix.listen sock 3003 ;
      let rec acceptPlayer n acc : player list Lwt.t =
        if n > 0 then
          let pt =
            Lwt_unix.accept sock >>= fun (cliFD, _sock) ->
            let inputChan = Lwt_io.of_fd ~mode:Lwt_io.input cliFD in
            let outputChan = Lwt_io.of_fd ~mode:Lwt_io.output cliFD in
            {input=inputChan; output= outputChan} |> return
          in
          pt >>= fun p ->
          acceptPlayer (n - 1) (p :: acc)
        else
          acc |> return
      in
      acceptPlayer n []
    
    
    
    let closePlayers listPlayers =
      Lwt_list.map_p
        (fun  player -> Lwt_io.close player.input)
        listPlayers
    
    let _ =
      Lwt_main.run
    
        (* création des player *)
        (
          (Lwt_io.fprintf Lwt_io.stderr "Attente des joueurs...\n") >>=
          fun () -> let threadListPlayers = getPlayers 2 in
         (* actions *)
          threadListPlayers >>=
          fun listPlayers -> (* fonction stupide à changer *) return listPlayers
    
    
         (* fermeture des player *)
         (* on reprend "threadListPlayers" pour être sur de tous les fermer *)
         >>= fun _ -> threadListPlayers >>= closePlayers)

On crée un serveur TCP/IP en `ocaml` comme en `C`, en créant une socket publique
et en écoutant cette socket. Remarquer qu&rsquo;on n&rsquo;accepte que deux joueurs.


## *Ready Player One*

Utiliser `git` pour télécharger le fichier du TP sur `https://classroom.github.com/a/PzY7PX0C`.
**Attention**, bien penser à créer une branche de travail pour rendre votre travail correctement.
Vous trouverez un dossier déjà préparé dans lequel on compile avec `dune` (comme pour le devoir maison).
Il y a un fichier `server.ml` dans lequel est écrit le code qu&rsquo;on vient de voir. Pour compiler: `dune build`
Si tout se passe bien, vous pouvez lancer l&rsquo;exécutable qui s’est créé dans le répertoire `_build` qui affiche le message *Attente des joueurs..*.
C&rsquo;est le moment de lancer les joueurs. Ouvrir deux nouveaux terminaux, et lancer dans chaque la commande `netcat localhost 3003`.
Selon la configuration, vous aurez peut-être besoin de changer l’adresse ou le port (du côté serveur aussi).
Si tout s’est bien passé, les trois programmes se ferment proprement et vous pouvez passer à l&rsquo;étape suivante.


## *4 à la suiiiiitttteee!!*

Écrire la fonction `send_msg : string -> player list -> player list Lwt.t` qui
envoie un message à chaque joueur. Bien Regarder le type de la fonction, elle
est destinée à être composée avec bind. Pour réaliser cette fonction, utilisez les fonctions suivantes :

-   `Lwt_list.map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t` qui applique un map à une liste de façon parallèle.
-   `Lwt_io.fprintf`
-   `Lwt_io.flush : Lwt_io.output_channel -> unit Lwt.t` à lancer après avoir
    utilisé `Lwt_io.fprintf` pour forcer l&rsquo;écriture (et vider le tampon).

Ensuite, ajouter cette fonction au bloc `Lwt_main.run` afin d&rsquo;envoyer la question suivante aux joueurs : *Ocaml est assez cool : vrai ou faux ?*. Tester


## *le monsieur te dit&#x2026;*

Écrire la fonction `get_answer : player list -> (player * string) list Lwt.t`
qui attend la réponse de chaque `player` et lui associe sa réponse `string` dans
un thread qui contient la liste. Pour cette question, utiliser la fonction
suivante `Lwt_io.read_line` qu&rsquo;on a déjà vue. Tester.


## *je ne crois pas qu&rsquo;il y ait de bonne ou mauvaise situation&#x2026;*

Écrire la fonction `filter_winners : string -> (player * string) list -> player list Lwt.t` qui prend la bonne réponse, et filtre les joueurs qui l&rsquo;ont donnée.
Vous utiliserez les fonctions:

-   `Lwt_list.filter_p`
-   `Lwt_list.map_p`

Ne pas hésiter à consulter la documentation (voir intro) si besoin. Vous pouvez
aussi utiliser la fonction `filter_map_p` Tester (la bonne réponse pour la
question est évidemment *vrai*). Ajouter une deuxième question *Javascript est
mieux qu&rsquo;Ocaml: vrai ou faux ?&ldquo;* (la bonne réponse pour la question est
évidemment *faux*)//


## *Highlander*

Le niveau des questions est très difficile mais il y a peut-être plusieurs
joueurs qui ont bien répondu. Mais il ne doit y avoir qu&rsquo;un gagnant.
On passe donc maintenant à la question finale, et seul le player le plus rapide à répondre sera le champion des netcat.
Avant tout, envoyer aux joueurs (seuls les gagnants des questions précédentes) la question : *Question de rapidité, avec quoi programment les vrais programmeurs : 1) nano, 2) emacs, 3) vi, 4) des papillons ?* (pour connaitre la réponse voir le [lien](https://xkcd.com/378/))


### *Fast and furious*

La première étape est de récupérer uniquement le premier joueur qui a répondu,
peu importe sa réponse. Utiliser pour cela la fonction `Lwt.choose` pour écrire
la fonction `filter_Fastest : player list -> player list Lwt.t`. Faites très
attention aux types de ces fonctions ! Note : vous ne pouvez pas réutiliser la
fonction `get_answer`. Pourquoi ?


### *Destination finale*

L&rsquo;étape précédente n&rsquo;était qu&rsquo;un échauffement; la vraie solution est un peu différente. Le problème de `filter_fastest` est qu&rsquo;elle ne vérifie pas la validité de la réponse.
Écrire la fonction `filter_faster_correct : string -> player list -> player list Lwt.t` qui renvoie la liste contenant le premier joueur ayant donné la bonne réponse (la liste peut être vide).
Utiliser pour cela la fonction `Lwt_nchoose_split` qui sépare les joueurs ayant répondu des autres.


## *bo-bo-bonus*

Pour améliorer le jeu:

-   Envoyer un message aux perdants.
-   Si la liste de joueurs toujours en lice ne contient qu&rsquo;un élément, on déclare
    ce joueur vainqueur tout de suite, sans poser les questions suivantes.

