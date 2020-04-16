
type id = int

type action =
  | Balance           
  | Withdraw of int   
  | Deposit of int    
  | Next              
  | Finished          
;; 

type account_spec = {name : string; id : id; balance : int} ;;

let db = ref [] ;;

let initialize (init: account_spec list) : unit =
  db := List.map (fun x -> ref x) init
  in () ;;

let input_to_int (input : string) : int =
  let i = int_of_string input in
  if i < 0 then raise (Failure "negative input")
  else i ;;

let rec acquire_id : unit -> id =
  fun () -> 
  print_string "Enter customer id: ";
  let id = read_line () in
  try input_to_int id with
  | Failure _ -> (print_string "Invalid id! Try again.";
  print_newline ();
  acquire_id ()) ;;

let rec acquire_amount : unit -> int = 
  fun () -> 
  print_string "Enter amount: ";
  let id = read_line () in
  try input_to_int id with
  | Failure _ -> (print_string "Invalid amount! Try again.";
  print_newline ();
  acquire_amount ()) ;;

let rec acquire_act : unit -> action = 
  fun () -> 
  print_string "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
  match read_line () with 
  | "B" -> Balance 
  | "-" -> Withdraw (acquire_amount ())  
  | "+" -> Deposit (acquire_amount ())      
  | "=" -> Next
  | "X" -> Finished
  | _ -> print_string "Invalid action! Try again.";
         print_newline ();
         acquire_act () ;;

