module B = PrintBox
open Mortgage__Types
(* open Core *)

let principal = 427000.0
let term = 25
let rows = Mortgage.calculate_rows
(* use property_value if you want to override the vaule used to calculate loan-to-value rate if nothing is provided initial principal is used, if interest_rate is provided, this won't do anything. *)
(* use interest_rate to use a static interest rate (this will invalidate any property_value input, as this is only used to calculate floating rates *)
             (* ~property_value: None *)
             ~property_value: (Some 600000.0)
             (* ~interest_rate: (Some 0.05) *)
             principal
             term
let summary = Mortgage.calc_summary principal rows;;

PrintBox_text.output ~style:true stdout @@ Mortgage.format_rows rows;;
print_endline "";;
Mortgage__Plot.graph rows

let () = print_endline (Summary.show summary)
