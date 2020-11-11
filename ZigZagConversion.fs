module ZigZagConversion

open Xunit
open FsUnit.Xunit

type Direction = Up | Down

let zigZag (s: string) (numRows: int): string =
    let rec go direction row (agg: Map<int, char list>) l: string =
        match l with
        | [] ->
            agg
            |> Map.toArray
            |> Array.map (snd >> List.rev >> System.String.Concat)
            |> System.String.Concat
        | h::t ->
            let agg' =
                agg
                |> Map.change row (function
                    | Some r -> Some (h::r)
                    | None -> Some ([h]))

            let (direction', row') =
                match direction with
                | Down when row = numRows - 1 -> (Up, row - 1)
                | Up when row = 0 -> (Down, row + 1)
                | Down -> (Down, row + 1)
                | Up -> (Up, row - 1)
            go direction' row' agg' t

    s.ToCharArray()
    |> List.ofArray
    |> go Down 0 Map.empty

[<Fact>]
let ``ZigZag Conversion`` () =
    zigZag "PAYPALISHIRING" 3 |> should equal "PAHNAPLSIIGYIR"
    zigZag "PAYPALISHIRING" 4 |> should equal "PINALSIGYAHRPI"
    zigZag "A" 1 |> should equal "A"
