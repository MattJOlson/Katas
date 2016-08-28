module WordWrap

open System.Text.RegularExpressions

type WordWithSep = { Word : string; Sep : string; }
type BareWord = string

type Word =
    | WithSep of WordWithSep
    | Bare of BareWord

let length w = match w with
    | Bare s -> s.Length
    | WithSep {Word = t; Sep = s} -> t.Length + s.Length

let str w = match w with
    | Bare s -> s
    | WithSep {Word = t; Sep = s} -> t + s

let break w = match w with
    | Bare s -> "\n" + s
    | WithSep {Word = t; Sep = " "} -> "\n" + t
    | WithSep {Word = t; Sep = s} -> s + "\n" + t

let words2 t = Regex.Split(t, "( )") |> Seq.toList

let words t = 
    let rec wordFactory wip tokens = match tokens with 
        | [] -> wip
        | [""] -> wip
        | [t] -> wip @ [Bare (t :BareWord)]
        | s :: t :: ts -> wordFactory (wip @ [WithSep ({Word = t; Sep = s;})]) ts
    Regex.Split(t, "( )") |> Seq.toList |> (wordFactory [])

let wrap corpus len =
    let rec wrapRec (wip:string) (currlen:int) (rest:Word list) = match rest with
        | [] -> wip
        | [w] when currlen + (length w) <= len -> wip + (str w)
    wrapRec "" 0 (words corpus)

let wrap2 corpus len =
    let rec wrapRec (wip:string) (currsep:string) (currlen:int) (rest:string list) = match rest with
        | [] -> wip
        | [w] when currlen + w.Length + 1 <= len -> wip + currsep + w
        | [w] when wip = "" -> w
        | [w] when len < currlen + w.Length + 1 -> wip + "\n" + w
        | w :: s :: ws when currlen + w.Length + 1 < len -> wrapRec (wip + w) s (currlen + 1 + w.Length) ws
        | w :: s :: ws when currlen + w.Length + 1 = len -> wrapRec (wip + w) "\n" (currlen + 1 + w.Length) ws
        | w :: s :: ws when len < currlen + w.Length + 1 -> wrapRec wip "\n" 0 (w :: ws)
    wrapRec "" "" 0 (words2 corpus)