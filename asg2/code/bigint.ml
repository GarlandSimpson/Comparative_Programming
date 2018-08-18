(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])
    let pow2_list = [1]
	let zero_list = [0]
	let two_list  = [2]
	
	
    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

	let rec cmp list1 list2 = match (list1, list2) with
        | [], [] -> 1
	    | list1, [] -> 2
        | [], list2 -> 3
        | list1, list2 -> let check = cmp (cdr list1) (cdr list2) in
        if check != 1 then check 
            else if (car list1) > (car list2) then 2
            else if (car list2) > (car list1) then 3
            else 1			
						
						
    let rec zed' list' = match list' with
	    |[] -> []
		|[0] -> []
		| car::cdr -> let cdr' zed' cdr
		    in match car, cdr' with
			    | 0, [] -> []
				| car, cdr' -> car::cdr' in zed' list
	
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  zed (sum mod radix :: add' cdr1 cdr2 (sum / radix))

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let check_diff = (car1 - car2 - carry) in   
		        if (check_diff >= 0) then (
			        let diff = car1 - car2 - carry in zed (diff mod radix :: sub' cdr1 cdr2 0 ))
		        else 
				    let diff = (car1 + 10) - car2 - carry
					in zed (diff mod radix :: sub' cdr1 cdr2 (if check_diff < 0 then 1 else 0))
		
    let sub = (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, sub' value1 value2 0)
        else zero

    let double number = add' number number 0

    let rec remainder' (dividend, pow2, divisor') =
        if (cmp divisor' dividend = 2)
            then zero_list, dividend
        else let quotient, remainder =
            remainder' (dividend, double pow2, double divisor')
            in if (cmp remainder divisor' = 3)
                then quotient, remainder
            else add' quotient pow2 0, sub' remainder divisor' 0

    let remainder (dividend, divisor') = remainder' (dividend, pow2_list, divisor')

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, _ = remainder (value1, value2) in
        if neg1 = neg2 then (Bigint (Pos, quotient))
        else if (cmp value1 value2 = 3) then zero
        else if neg1 = Neg && neg2 = Pos then (Bigint (Neg, quotient))
         else (Bigint (Neg, quotient))


    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, remainder = remainder (value1, value2)
        in (Bigint (Pos, remainder))


    let rec mul' (multiplier, pow2, multiplied') =
        if (cmp pow2 multiplier = 2) then multiplier, zero_list
        else let remainder, product =
             mul' (multiplier, double pow2, double multiplied')
        in if (cmp remainder pow2 = 3) then remainder, product
        else sub' remainder pow2 0, add' product multiplied' 0


    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, product = mul' (value1, pow2_list, value2) in
        if neg1 = neg2 then (Bigint (Pos, product))
        else if neg1 = Neg && neg2 = Pos then (Bigint (Neg, product))
        else (Bigint (Neg, product))


    let even expt = match expt with
        | expt  -> let d, _ = remainder (expt, two_list) in
        if d = [0] then true
        else false


    let rec pow' base expt result = match expt with
    | [0]-> result
    | expt when even expt-> let _, prod = mul' 
    (base, pow2_list, base) in
    let d, _ = remainder' (expt, pow2_list, two_list) 
    in pow' prod d result
    | expt-> let s = sub' expt pow2_list 0 in
    let _, res = mul' (base, pow2_list, result) in
    pow' base s res


    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
    if rem (Bigint (Pos, value2)) (Bigint (Pos, [2])) = zero
        then let x = pow' value1 (add' value2 pow2_list 0 ) 
        pow2_list in (Bigint (Pos, x))
    else if neg1 = Pos && neg2 = Pos
        then let x = pow' value1 (add' value2 pow2_list 0 ) 
         pow2_list in (Bigint (Pos, x))
    else if neg1 = Pos && neg2 = Neg
        then zero
    else if neg1 = Neg && neg2 = Neg
        then zero
    else
        let x = pow' value1 (add' value2 pow2_list 0 ) 
        pow2_list in (Bigint (Neg, x))
	

end

