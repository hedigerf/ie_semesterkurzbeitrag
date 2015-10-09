namespace Semesterkurzbeitrag

module Main =

    open System.IO
    open System
    open FSharp.Data
    open System.Xml.Linq

    type Collection = XmlProvider<"Resources/irg_collection.trec">

    let tokenize (text:string) =
        let sepAry = [| ' '; ','|]
        Array.toList (text.Split sepAry)

    [<EntryPoint>]
    let main argv = 
        let collection =Collection.Load "../../Resources/irg_collection.trec"
        let documentItems= Array.toList(collection.Docs |> Array.map (fun doc ->
            (doc.RecordId,DocumentItem(doc.RecordId,doc.Text,tokenize(doc.Text)))))
        let documentsById = Map.ofList documentItems
        documentItems |> List.iter (fun doc -> printfn "%s" (snd doc).OriginalText)
        0;
        //0 // return an integer exit code

