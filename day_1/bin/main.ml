let () = print_endline "Day 1"

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* i.e. `L12 = -12` and  `r12 = + 12` *)
let convert_dial_action_to_int_string = function
  | 'L' -> '-'
  | 'R' -> '+'
  | c -> c

let rec decode_combos ?(acc = 100) ?(total = 0) = function
  | [] -> total
  | head :: body -> (
      let transformed = String.map convert_dial_action_to_int_string head in
      let value =
        try int_of_string transformed
        with Failure _ -> 0 (* Handle non-integer strings *)
      in
      let acc' = (value + 100 + acc) mod 100 in
      match acc' with
      | 0 -> decode_combos ~acc:acc' ~total:(total + 1) body
      | a -> decode_combos ~acc:a ~total body)

let rec decode_combos_2 ?(acc = 100) ?(total = 0) = function
  | [] -> total
  | head :: body -> (
      let transformed = String.map convert_dial_action_to_int_string head in
      let value =
        try int_of_string transformed
        with Failure _ -> 0 (* Handle non-integer strings *)
      in
      let n_zeros = abs value / 100 in
      let wrapped = abs (value + acc) mod 100 in
      let () =
        begin if wrapped = 0 then begin
          (* print_int modulated; *)
          (* print_endline " mod"; *)
          (* print_int acc; *)
          (* print_endline " acc"; *)
          (* print_int value; *)
          (* print_endline " value"; *)
          (* print_int n_zeros; *)
          (* print_int wrapped; *)
          (* print_endline " zeroes wrapped" *)
          Printf.printf "Zeroes: %d\tWrapped: %d\tAcc: %d\tValue: %d\n" n_zeros
            wrapped acc value
        end
        end
      in
      match wrapped with
      | 0 -> decode_combos_2 ~acc:wrapped ~total:(total + 1 + n_zeros) body
      | acc -> decode_combos_2 ~acc ~total:(total + n_zeros) body)

let () =
  let lines = read_lines "./data/input_a.txt" in
  let result_1 = decode_combos ~acc:50 lines in
  let result_2 = decode_combos_2 ~acc:50 lines in
  begin
    print_endline ("Result 1: " ^ string_of_int result_1) (* 964 *);
    print_endline ("Result 2: " ^ string_of_int result_2) (* 5872 *)
  end
