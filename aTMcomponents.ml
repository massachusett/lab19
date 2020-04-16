type id = int

type action =
  | Balance           
  | Withdraw of int   
  | Deposit of int    
  | Next              
  | Finished          
;; 

type account_spec = {name : string; id : id; balance : int} ;;

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

let present_message (s : string) : unit =
  print_string s;
  print_newline () ;;

let print_bill (bill : int) : unit =
  let s = "[" ^ (string_of_int bill) ^ " @ " ^ (string_of_int bill) ^ "]" in
  print_string s ;;

let rec cash_aux (amount : int) (limit : int) (bills : int list) (count : int) : unit =
  match bills with
  | [] -> present_message (" and " ^ (string_of_int count) ^ " more")
  | hd :: tl ->
    let num_bills = (amount - amount mod hd) / hd in
    if limit = 0 then
      cash_aux (amount - num_bills * hd) limit tl (count + num_bills)
    else if num_bills <= limit then 
      (for i = 1 to num_bills do
        print_bill hd
      done;
      cash_aux (amount - num_bills * hd) (limit - num_bills) tl count)
    else 
      (for i = 1 to limit do
        print_bill hd
      done;
      cash_aux (amount - num_bills * hd) 0 tl (count + num_bills - limit))

let deliver_cash (a : int) : unit =
  cash_aux a 4 [100; 50; 20; 10; 5; 1] 0 ;;
