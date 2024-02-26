type row =
  { year_month : int * int
  ; interest_rate : float
  ; loan_balance : float
  ; payment : float
  ; interest : float
  ; principle : float
  ; end_period_balance : float
  }

module Summary = struct
  type t =
    { total_cost : float
    ; principal : float
    }
  [@@deriving show]
end
