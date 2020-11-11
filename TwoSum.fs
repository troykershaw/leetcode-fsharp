module TwoSum

open Xunit
open FsUnit.Xunit

let twoSum (nums: int list) (target: int): Option<int * int> =
    let rec go (map: Map<int, int>) (l: (int * int) list): Option<int * int> =
        match l with
        | [] -> None
        | (i, n)::_ when Map.containsKey n map -> Some (Map.find n map, i)
        | (i, n)::t -> go (Map.add (target - n) i map) t

    nums
    |> List.mapi (fun i n -> (i, n))
    |> go Map.empty

[<Fact>]
let ``Two Sum`` () =
    twoSum [2; 7; 11; 15] 9 |> should equal (Some (0, 1))
    twoSum [3; 2; 4] 6 |> should equal (Some (1, 2))
    twoSum [3; 3] 6 |> should equal (Some (0, 1))
    twoSum [3; 2; 1] 7 |> should equal None
