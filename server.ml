open Base
(* open Lwt *)

type player = {input: Lwt_io.input_channel; output: Lwt_io.output_channel}

let (>>=) = Lwt.(>>=)
let return = Lwt.return


let getPlayers n : player list Lwt.t =
  let sockaddr = Lwt_unix.ADDR_INET (UnixLabels.inet_addr_loopback, 3006) in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec sock ;
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true ;
  Lwt_unix.bind sock sockaddr >>= fun () ->
  Lwt_unix.listen sock 3006 ;
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

let send_msg_win listPlayers =
     Lwt_list.map_p
     (fun player -> Lwt_io.fprintf player.output "congrats you are the winner ! \n")
     listPlayers 


(* send_msg ************************************************************************************************************* *)
let send_msg (msg : string) (lj : player list) :  player list Lwt.t =
             (Lwt_list.map_p (fun p -> (Lwt_io.fprintf p.output "%s" msg) >>= fun () -> Lwt_io.flush p.output ) lj) >>= fun _ -> return(lj) 
(* ********************************************************************************************************************** *)



(* get_answer ************************************************************************************************************* *)
let getAnswer (lj : player list) : (player * string) list Lwt.t  =
    (Lwt_list.map_p ( fun p-> (Lwt_io.read_line p.input) >>= fun s -> return (p,s) ) lj )
    
(* ************************************************************************************************************************ *)



(* filter_winners ********************************************************************************************************* *)
let filter_winners (answer : string) (lpa : (player * string) list) : (player list Lwt.t) =
    
    Lwt_list.filter_p  ( fun a -> return (String.equal (snd a) answer) ) lpa 
    >>= 
    Lwt_list.map_p ( fun a-> return(fst a) ) 

(* ************************************************************************************************************************ *)



(* filter_Fastest ********************************************************************************************************* *)
let filter_Fastest (pl : player list) : (player list Lwt.t) = 
   ( Lwt.choose
    ( List.map pl ( fun p-> (Lwt_io.read_line p.input) >>= fun s -> return (p) ) ) ) 
    >>=
    fun p -> return(p::[])
(* ************************************************************************************************************************ *)



(* filter_faster_correct ************************************************************************************************** *)
let filter_faster_correct (answer : string) (pl : player list) : (player list Lwt.t) =
    Lwt.nchoose_split  
       ( List.map pl ( fun p-> (Lwt_io.read_line p.input) >>= fun s -> return (p,s) ) )
    >>=
    fun (lg,ld) -> return(ld)
    >>=
    Lwt_list.filter_p (fun a -> a >>= fun z -> return(String.equal (snd z) answer ))
    >>=
    fun l -> List.map l (fun p-> p >>= fun e -> return(fst e))
    >>=
    Lwt.choose
    >>=
    fun p -> return(p::[])

   

(* ************************************************************************************************************************ *)



let _ =
  Lwt_main.run

    (* création des player *)
    (
      (Lwt_io.fprintf Lwt_io.stderr "Attente des joueurs...\n") >>=
      fun () -> let threadListPlayers = getPlayers 2 in
     (* actions *)
      threadListPlayers  >>=
      fun listPlayers -> threadListPlayers >>=  send_msg "Ocaml est assez cool : vrai ou faux ? \n" >>= ( fun _ -> return listPlayers)
      >>= getAnswer 
      >>= filter_winners "vrai"
      >>= send_msg "Javascript est mieux qu'Ocaml: vrai ou faux ? \n" 
      >>= getAnswer
      >>= filter_winners "faux" 
      >>= send_msg "Question de rapidité, avec quoi programment les vrais programmeurs : 1) nano, 2) emacs, 3) vi, 4) des papillons ?" 
      >>= filter_Fastest   (*'ou' filter_faster_correct "4"*) 
      (* sendMsg  "Bravo!" >>= *)


     (* fermeture des player *)
     (* on reprend "threadListPlayers" pour être sur de tous les fermer *)
     >>= fun _ -> threadListPlayers >>= closePlayers  )
