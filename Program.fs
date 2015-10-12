namespace Semesterkurzbeitrag

module Main =

    open System.IO
    open System
    open FSharp.Data
    open System.Xml.Linq
    open Lucene.Net.Analysis
    open Lucene.Net.Analysis.Tokenattributes
    open Lucene.Net.Util
    open Lucene.Net.Analysis.Standard

    type Collection = XmlProvider<"Resources/irg_collection.trec">

    let stemm (text:string) =
        let lowerCaseTokenizer = new LowerCaseTokenizer(new StringReader(text))
        let standardFilter = new StandardFilter(lowerCaseTokenizer)
        let stopList = new Collections.Generic.HashSet<string>();
        let stopFilter = new StopFilter(false,standardFilter,StopAnalyzer.ENGLISH_STOP_WORDS_SET)
        let porterFilter = new PorterStemFilter(stopFilter)
        let termAttr = porterFilter.AddAttribute<ITermAttribute>()
        let terms =
            [
                while(porterFilter.IncrementToken())
                    do yield(porterFilter.GetAttribute<ITermAttribute>().ToString())
             ]
        terms


    let rec addEntry word recordId (acc:Map<string,Occurency>) =
        match acc.TryFind(word) with
        | Some(occ) -> acc.Remove(word).Add(word,occ.Add(recordId))
        | None -> acc.Add(word,Occurency([recordId]))

    let rec searchDocument (document:DocumentItem) (acc:Map<string,Occurency>) =
        match document.TokenizedText with
        | head::tail -> addEntry head document.RecordId acc
        | [] -> acc

    let rec buildFrequencyMap (documentItems:List<DocumentItem>) (acc:Map<string,Occurency>) =
        match documentItems with
        | head::tail -> searchDocument head acc
        | [] -> acc
    
    //not correctly implemented
    let rec intersect cur1 (postings1:List<int>) cur2 (postings2:List<int>) (acc:List<int>) =
    //implement with optipon types
        if (cur1 > -1 && cur2 > - 1) then
            if (cur1 = cur2) then
                intersect (if postings1.Length > 0 then postings1.Head else -1) postings1.Tail postings2.Head postings2.Tail (cur1::acc)
            elif cur1 < cur2 then intersect postings1.Head postings1.Tail cur2 postings2 acc
            else intersect cur1 postings1 postings2.Head postings2.Tail acc
        else acc
        
    [<EntryPoint>]
    let main argv = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let collection =Collection.Load "../../Resources/irg_collection.trec"
        let documentItems = Array.toList(collection.Docs |> Array.map (fun doc ->
            DocumentItem(doc.RecordId,doc.Text,stemm(doc.Text))))
        let frequencyMap = buildFrequencyMap documentItems Map.empty<string,Occurency>
        //let documentItems= Array.toList(collection.Docs |> Array.map (fun doc ->
        //    (doc.RecordId,DocumentItem(doc.RecordId,doc.Text,tokenize(doc.Text)))))
        //let documentsById = Map.ofList documentItems
        //documentItems |> List.iter (fun doc -> printfn "%s" (snd doc).OriginalText)
        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds
        Threading.Thread.Sleep(4000)
        0;
