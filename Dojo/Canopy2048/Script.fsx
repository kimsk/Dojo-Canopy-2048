#I "./packages/Selenium.WebDriver.2.41.0/lib/net40/"
#I "./packages/Selenium.Support.2.41.0/lib/net40/"
#I "./packages/SizSelCsZzz.0.3.35.0/lib/"
#I "./packages/Newtonsoft.Json.5.0.1/lib/net40/"
#I "./packages/canopy.0.9.8/lib/"

#r "WebDriver.Support.dll"
#r "WebDriver.dll"
#r "canopy.dll"

#load "Core.fs"
open Canopy2048
#load "Game.fs"
#load "Interactions.fs"
open Canopy2048.Game
open Canopy2048.Interactions

open canopy
open runner
open System

let getCellValue row col (states:Map<Pos,int>) =
    let ret = states 
                |> Seq.filter (fun kv -> kv.Key.Row = row+1 && kv.Key.Col = col+1)
                |> Seq.map (fun kv -> kv.Value)
    
    if (ret |> Seq.length) > 0 then
        Some(ret |> Seq.head)
    else
        None


let scanStates states = Array2D.init 4 4 (fun row col -> getCellValue row col states)

// (maxNeighbor, lastValue) 
let rec maxNeighbor acc list =
    match list with
    | [] -> acc |> fst
    | hd::tl -> 
        match hd with
        | None -> maxNeighbor acc tl
        | Some(v) -> 
            if v > fst(acc) && v = snd(acc) then
                maxNeighbor (v, v) tl
            else
                maxNeighbor (fst(acc), v) tl

let scanRow row (arr:int option [,]) =
    arr.[row,*] 
    |> List.ofSeq 
    |> maxNeighbor (0,0) 

let scanCol col (arr:int option [,]) =
    arr.[*,col] 
    |> List.ofSeq 
    |> maxNeighbor (0,0)

let rnd = new Random();
let moves = [|up;left;down;right|]

let nextKey key =
    let arr = state() |> scanStates
    let maxRow = [0;1;2;3] |> List.map (fun n -> scanRow n arr) |> List.max
    let maxCol = [0;1;2;3] |> List.map (fun n -> scanCol n arr) |> List.max
    printfn "%d %d" maxRow maxCol

    if maxRow = 0 && maxCol = 0 then
        moves.[rnd.Next(0,4)]
    else
        if maxRow > maxCol then 
            if key = left then right
            else left
        else 
            if key = up then down
            else up

let keyToString = function
    | k when k = up -> "up"
    | k when k = left -> "left"
    | k when k = down -> "down"
    | k when k = right -> "right"
    | _ -> failwith "Impossible!"

let rec playing key =
    press key
    printfn "Press %s" (keyToString key)
    printfn "Score: %s" (element "div.score-container").Text
    match (won(),lost()) with
    | (false, false) -> playing (nextKey key)
    | (true, false) -> printfn "Game Won!"
    | (false, true) -> printfn "Game Over!"     
    | _ -> failwith "Impossible!"

let play2048() = 
    start firefox
    url "http://gabrielecirulli.github.io/2048/"
    playing up




play2048()
