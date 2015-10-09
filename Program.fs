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

    let rec addEntry entry (acc:Map<string,int>) =
        match acc.TryFind(entry) with
        | Some(count) -> acc.Remove(entry).Add(entry,count + 1)
        | None -> acc.Add(entry,1)

    let rec searchDocument (document:DocumentItem) (acc:Map<string,int>) =
        match document.TokenizedText with
        | head::tail -> addEntry head acc
        | [] -> acc

    let rec buildFrequencyMap (documentItems:List<DocumentItem>) (acc:Map<string,int>) =
        match documentItems with
        | head::tail -> searchDocument head acc
        | [] -> acc

    [<EntryPoint>]
    let main argv = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let collection =Collection.Load "../../Resources/irg_collection.trec"
        let documentItems = Array.toList(collection.Docs |> Array.map (fun doc ->
            DocumentItem(doc.RecordId,doc.Text,tokenize(doc.Text))))
        let frequencyMap = buildFrequencyMap documentItems Map.empty<string,int>
        //let documentItems= Array.toList(collection.Docs |> Array.map (fun doc ->
        //    (doc.RecordId,DocumentItem(doc.RecordId,doc.Text,tokenize(doc.Text)))))
        //let documentsById = Map.ofList documentItems
        //documentItems |> List.iter (fun doc -> printfn "%s" (snd doc).OriginalText)
        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds
        0;
        //0 // return an /integer exit code 

