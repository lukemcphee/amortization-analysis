open Types
module B = PrintBox

(* static sample rates, pulled from Bank of England data here: *)
(* https://www.bankofengland.co.uk/statistics/visual-summaries/quoted-household-interest-rates                                          *)
let calculate_interest_rate value outstanding_principle =
  let ltv = outstanding_principle /. value in
  if (ltv >= 0.95) then 0.0591
  else if (ltv >= 0.90) then 0.0545 
  else if (ltv >= 0.85) then 0.0503 
  else if (ltv >= 0.75) then 0.0473 
  else  0.0462

(* stolen from https://github.com/dbuenzli/gg/blob/8f761c278d0b2ee2adb94f9fbc033f1bfd76e536/src/gg.ml#L123-L126 *)
let round_dfrac d x =
  if x -. Float.round x = 0.
  then x
  else (
    (* x is an integer. *)
    let m = 10. ** float d in
    (* m moves 10^-d to 1. *)
    floor ((x *. m) +. 0.5) /. m)
;;

let format_rows (rows : row list) =
  let get_row_arr r =
    let year, month = r.year_month in
    [| B.int year
     ; B.int month
     ; B.float r.interest_rate
     ; B.float @@ round_dfrac 2 r.payment
     ; B.float @@ round_dfrac 2 r.interest
     ; B.float @@ round_dfrac 2 r.principle
     ; B.float @@ round_dfrac 2 r.loan_balance
     ; B.float @@ round_dfrac 2 r.end_period_balance
    |]
  in
  let rec get_rows rows converted_rows =
    match rows with
    | [] -> converted_rows
    | [ r ] -> get_row_arr r :: converted_rows
    | r :: xr -> get_rows xr (get_row_arr r :: converted_rows)
  in
  let output = get_rows rows [] in
  let output_with_headers =
    Array.of_list
    @@ ([| B.sprintf "Year"
         ; B.sprintf "Month"
         ; B.sprintf "Interest rate"
         ; B.sprintf "Payment"
         ; B.sprintf "Interest"
         ; B.sprintf "Principle"
         ; B.sprintf "Loan balance"
         ; B.sprintf "End period balance"
        |]
        :: output)
  in
  output_with_headers |> B.grid
;;

let calc_total_monthly_payment principal annual_interest number_of_payments =
  let monthly_interest = annual_interest /. 12.0 in
  let numerator =
    monthly_interest *. Float.pow (1.0 +. monthly_interest) number_of_payments
  in
  let denominator = Float.pow (1.0 +. monthly_interest) number_of_payments -. 1.0 in
  principal *. (numerator /. denominator)
;;

let calc_total_monthly_principal total_monthly_payment outstanding_balance interest_rate =
  let monthly_interest_rate = interest_rate /. 12. in
  total_monthly_payment -. (outstanding_balance *. monthly_interest_rate)
;;

let calculate_rows ?(property_value = None) ?(interest_rate = None) principal term_years =
  let _ = interest_rate in 
  let total_property_value = match property_value with
    | Some property_value -> property_value
    | None -> principal in
  let total_number_of_payments = float_of_int @@ (term_years * 12) in
  let calculate_month year month remaining_balance number_of_remaining_payments =
    let updated_interest_rate = match interest_rate with
      | Some rate -> rate
      | None -> calculate_interest_rate total_property_value remaining_balance in
    let total_monthly_payment = calc_total_monthly_payment remaining_balance updated_interest_rate number_of_remaining_payments
    in
    (* let _ = print_endline @@ string_of_float updated_interest_rate in *)
    let monthly_principal =
      calc_total_monthly_principal total_monthly_payment remaining_balance updated_interest_rate in 
    (* let _ = print_endline @@ string_of_float monthly_principal in *)
    { year_month = year, month
    ; interest_rate = updated_interest_rate
    ; loan_balance = remaining_balance
    ; payment = total_monthly_payment
    ; interest = total_monthly_payment -. monthly_principal
    ; principle = monthly_principal
    ; end_period_balance = remaining_balance -. monthly_principal
    }
  in
  let rec calc_all_months outstanding_balance year month schedule months_calculated =
    if total_number_of_payments <= months_calculated
    then schedule
    else (
      let number_of_remaining_payments = total_number_of_payments -. months_calculated in 
      let next_month_details = calculate_month year month outstanding_balance number_of_remaining_payments in
      let next_year = if month == 12 then year + 1 else year in
      let next_month = if month == 12 then 1 else month + 1 in
      let new_outstanding_balance = next_month_details.end_period_balance in
      let updated_schedule = next_month_details :: schedule in
      calc_all_months
        new_outstanding_balance
        next_year
        next_month
        updated_schedule
        (months_calculated +. 1.))
  in
  calc_all_months principal 0 1 [] 0.
;;

let calc_summary principal rows =
  let total_cost =
    let rec calc_total_cost rows sum_so_far =
      match rows with
      | [] -> sum_so_far
      | [ r ] -> calc_total_cost [] (sum_so_far +. r.payment)
      | r :: rs -> calc_total_cost rs (sum_so_far +. r.payment)
    in
    calc_total_cost rows 0.
  in
  { Summary.total_cost; principal }
;;
