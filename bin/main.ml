module B = PrintBox
open Mortgage__Types

let principal = 400000.0
let interest_rate = 0.04
let term = 20
let rows = Mortgage.calculate_rows principal interest_rate term
let summary = Mortgage.calc_summary principal rows;;

PrintBox_text.output ~style:true stdout @@ Mortgage.format_rows rows;;
print_endline "";;
Mortgage__Plot.graph rows

let () = print_endline (Summary.show summary)
