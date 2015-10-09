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

    let rec addEntry word recordId (acc:Map<string,Occurency>) =
        match acc.TryFind(word) with
        | Some(occ) -> acc.Remove(word).Add(word,Occurency(recordId :: occ.RefecencedDocIds))
        | None -> acc.Add(word,Occurency([recordId]))

    let rec searchDocument (document:DocumentItem) (acc:Map<string,Occurency>) =
        match document.TokenizedText with
        | head::tail -> addEntry head document.RecordId acc
        | [] -> acc

    let rec buildFrequencyMap (documentItems:List<DocumentItem>) (acc:Map<string,Occurency>) =
        match documentItems with
        | head::tail -> searchDocument head acc
        | [] -> acc

    [<EntryPoint>]
    let main argv = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let collection =Collection.Load "../../Resources/irg_collection.trec"
        let documentItems = Array.toList(collection.Docs |> Array.map (fun doc ->
            DocumentItem(doc.RecordId,doc.Text,tokenize(doc.Text))))
        let frequencyMap = buildFrequencyMap documentItems Map.empty<string,Occurency>
        //let documentItems= Array.toList(collection.Docs |> Array.map (fun doc ->
        //    (doc.RecordId,DocumentItem(doc.RecordId,doc.Text,tokenize(doc.Text)))))
        //let documentsById = Map.ofList documentItems
        //documentItems |> List.iter (fun doc -> printfn "%s" (snd doc).OriginalText)
        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds
        Threading.Thread.Sleep(4000)
        0;
