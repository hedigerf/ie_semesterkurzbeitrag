﻿namespace Semesterkurzbeitrag

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
                    do yield(porterFilter.GetAttribute<ITermAttribute>().ToString().Substring(5))
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

    let collectFrequency (query: TrecEntry) (freqMap:Map<string,InvertedIndexOccurence>) =
        let rec helper (lst:List<string>) (freqMap:Map<string,InvertedIndexOccurence>) (acc:List<InvertedIndexOccurence>) =
            match lst with
            | head::tail -> 
                match freqMap.TryFind(head) with
                | Some(occr) -> helper tail freqMap (occr::acc)
                | _ -> helper tail freqMap acc
            | _ -> acc
        let unsorted=helper query.TokenizedText freqMap []
        unsorted |> List.sortBy (fun occr -> occr.Frequency)

  //  let unitedIntersection (lst:List<InvertedIndexOccurence>) =
      //  let rec helper (lst:List<InvertedIndexOccurence>) (acc:List<int>) =
       ////     | head::tail -> 
        //    intersect acc.Head acc.Tail head tail [] 
       //     | [] -> acc
     //   helper lst []
      
    [<EntryPoint>]
    let main argv = 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let documentItems = loadTrecEntries "../../Resources/irg_collection.trec"
        let indexPair = (Indexes.createIndexes documentItems)
        let documentCount = documentItems.Length;
        let idf = Indexes.calculateIdf indexPair.invertedIndex documentCount
        let weight = Indexes.calculateTermWeight indexPair.invertedIndex idf
        let dNorm  = Indexes.calculateDnorm indexPair.nonInvertedIndex weight
        let queries = loadTrecEntries "../../Resources/irg_queries.trec"
        let queriesIndexPair = (Indexes.createIndexes queries)
        let qNorm = Indexes.calculateQnorm (Array.ofList queries) queriesIndexPair.nonInvertedIndex documentCount idf
        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds
        Threading.Thread.Sleep(4000)
        0;
