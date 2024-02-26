open Owl
open Owl_plplot
open Types

let graph rows =
  let x_axis_months =
    Array.of_list
    @@ List.map
         (fun row ->
           let year, month = row.year_month in
           float_of_int @@ ((year * 12) + month))
         rows
  in
  (* let interest_rate_by_month = Array.of_list @@ List.map (fun row -> row.interest_rate) rows in *)
  (* let y_axis_interest_rate = *)
    (* Mat.of_array interest_rate_by_month (Array.length interest_rate_by_month) 1 *)
  (* in *)
  let x_axis = Mat.of_array x_axis_months (Array.length x_axis_months) 1 in

  let end_balance_by_month =
    Array.of_list @@ List.map (fun row -> row.end_period_balance) rows in
  let y_axis_end_balance =
    Mat.of_array end_balance_by_month (Array.length end_balance_by_month) 1 in

  let interest_by_month = Array.of_list @@ List.map (fun row -> row.interest) rows in
  let y_axis_interest =
    Mat.of_array interest_by_month (Array.length interest_by_month) 1 in

  let principle_by_month = Array.of_list @@ List.map (fun row -> row.principle) rows in
  let y_axis_principle =
    Mat.of_array principle_by_month (Array.length principle_by_month) 1 in
  
  let h = Plot.create ~m:3 ~n:2 "output.pdf" in
  Plot.subplot h 0 0;
  (* Plot.set_foreground_color h 0 50 255; *)
  (* Plot.set_background_color h 0 0 0; *)
  Plot.set_page_size h 1000 1000;
  Plot.(
    plot
      ~h
      ~spec:[ RGB (255, 0, 0); LineStyle 1; Marker "#[0x2299]"; MarkerSize 8. ]
      x_axis
      y_axis_end_balance);
  Plot.(legend_on h ~position:NorthEast [| "remaining-balance" |]);
  Plot.subplot h 1 0;
  Plot.(scatter ~h ~spec:[ RGB (255, 0, 0); LineStyle 1 ] x_axis y_axis_interest);
  Plot.(scatter ~h ~spec:[ RGB (255, 0, 0); LineStyle 2 ] x_axis y_axis_principle);
  Plot.(legend_on h ~position:East [| "interest"; "principle_payment" |]);

  (* Plot.subplot h 2 0; *)
  (* let _ = print_endline @@ string_of_int (Array.length interest_by_month) in *)
  (* Plot.(scatter ~h ~spec:[ RGB (255, 0, 0); LineStyle 1 ] x_axis y_axis_principle); *)
  (* Plot.(legend_on h ~position:East [| "interest rate" |]); *)

  Plot.output h
;;
