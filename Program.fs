module Main
open System.IO
open System
open FSharp.Data
open System.Xml.Linq

type Collection = XmlProvider<"Resources/irg_collection.trec">


[<EntryPoint>]
let main argv =
   // let parsedFile = File.ReadAllLines("../../Resources/irg_collection.trec")
    //let parsedStr = Array.reduce (fun acc item -> acc + item) parsedFile 
    //let test = Collection.Parse "Resources/irg_collection.trec"   
    let collection=Collection.Load "../../Resources/irg_collection.trec"
    for doc in collection.Docs do
          printfn "%s" doc.Text

    0;
    //0 // return an integer exit code

