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
    open Indexes

  

    module Seq =     
        let pmap f l =
            seq{ for a in l -> async { return f a } }
            |> Async.Parallel
            |> Async.RunSynchronously

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
                    do yield(
                        let iTermStr = porterFilter.GetAttribute<ITermAttribute>().ToString()
                        let sub = iTermStr.Substring(5)
                        sub
                    )
             ]
        terms

    let rec intersect cur1 postings1 cur2 postings2 acc =

        if(cur1 = cur2) then
            match postings1,postings2 with
            | head1::[], head2::tail2 ->  intersect head1 [] head2 tail2 (cur1::acc)
            | head1::tail1,head2::[] -> intersect head1 tail1 head2 [] (cur1::acc)
            | head1::tail1,head2::tail2 -> intersect head1 tail1 head2 tail2 (cur1::acc)
            | [], head2::tail2 -> intersect cur1 [] head2 tail2 (cur1::acc)
            | head1::tail1,[] -> intersect head1 tail1 cur2 [] (cur1::acc)
            | [],[] -> acc

        elif (cur1 < cur2) then
            match postings1,postings2 with
            | head1::[], _ ->  intersect head1 [] cur2 postings2 acc
            | head1::tail1,_ -> intersect head1 tail1 cur2 postings2 acc
            | [], head2::tail2 -> acc
            | [],[] -> acc

        else 
            match postings1,postings2 with
            | _, head2::[] -> intersect cur1 postings1 head2 [] acc
            | _, head2::tail2 ->  intersect cur1 postings1 head2 tail2 acc
            | head1::tail1,[] -> acc
            | [],[] -> acc
    
    let loadTrecEntries (pathToXmlFile:string) =
        let trecXmlEntries =Collection.Load pathToXmlFile
        Array.toList(trecXmlEntries.Docs |>
            (Array.Parallel.map (fun doc ->
            TrecEntry(doc.RecordId,doc.Text,stemm(doc.Text)))))
             
    let loadDocuments path =
        let filePathes = Directory.GetFiles(path, "*", SearchOption.AllDirectories)
        filePathes |> Array.map (fun filePath ->
             let fileName = Path.GetFileName(filePath)
             let text=System.IO.File.ReadAllText(filePath)
             let entry = TrecEntry((int fileName),text,stemm text)
             entry)
    
    //creates a result file in trecEntry format witht the given fileName
    let createResultTrecFile results fileName =
        let stream = new StreamWriter(fileName, false)
        stream.WriteLine("This line overwrites file contents!")
        results |> Array.iter (fun queryResult ->
            queryResult |> Array.iteri (fun rank (rsv,documentId,queryId,accuValue,dNormValue,qNormValue) ->
                stream.WriteLine("{0} {1} {2} {3} {4} {5}",queryId,"Q0",documentId,rank,rsv,"rethed")))


    [<EntryPoint>]
    let main argv = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let documentItems = loadTrecEntries "../../Resources/irg_collection.trec"
        //let documentItems = Array.toList (loadDocuments "../../Resources/Praktika5/documents")
        let documentCount = documentItems.Length;
        printfn "Loaded %i documents..." documentCount
        let indexPair = (Indexes.createIndexes documentItems)
        printfn "Inverted and Noninvertedindex created."
        let (idf:IdfMap) =  Indexes.calculateIdf indexPair.invertedIndex documentCount
        printfn "idf created."
        let (dNorm:DNormMap)  = Indexes.calculateDnorm indexPair.nonInvertedIndex idf documentCount
        printfn "dNorm created."
        let queries = loadTrecEntries "../../Resources/irg_queries.trec"
        //let queries = Array.toList (loadDocuments "../../Resources/Praktika5/queries")
        printfn "queries loaded."
        let queriesIndexPair = (Indexes.createIndexes queries)
        let queryProcessingList = Indexes.createQueryProcessingList queries queriesIndexPair.nonInvertedIndex documentCount idf indexPair.invertedIndex
        let results = Indexes.calculateRsv queryProcessingList documentItems dNorm
        printfn "rsv calculated."
        createResultTrecFile results "original.trec"
        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds
        Threading.Thread.Sleep(4000)
        0
