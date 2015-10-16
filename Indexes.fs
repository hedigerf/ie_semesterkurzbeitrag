namespace Semesterkurzbeitrag
module Indexes =

    type WorkingIndexPair = {wNonInvertedIndex: Map<int,list<string>>;
                      wInvertedIndex: Map<string,list<int>>}

    type IndexValue<'a> = {key:'a; frequency: int}

    type IndexPair = {nonInvertedIndex: Map<int,seq<IndexValue<string>>>;
                      invertedIndex: Map<string,seq<IndexValue<int>>>}

    type Idf = Map<string,double>

    let rec addIndex (index: Map<_,_>) key value =
        match index.TryFind(key) with
        | Some(occ) -> index.Remove(key).Add(key,(value::occ))
        | None -> index.Add(key,[value])
    
    let rec addEntry word recordId (workingIndexPair:WorkingIndexPair) =
        printfn "Add word %s in %i to index" word recordId
        {
            wNonInvertedIndex=(addIndex workingIndexPair.wNonInvertedIndex recordId word);
            wInvertedIndex=(addIndex workingIndexPair.wInvertedIndex word recordId;)
        }   
    
    let rec searchDocumentHelper tokenizedText recordId workingIndexPair=
        match tokenizedText with
        | head::tail -> searchDocumentHelper tail recordId (addEntry head recordId workingIndexPair)
        | [] ->  workingIndexPair
       
    let searchDocument (document:TrecEntry) workingIndexPair =
        searchDocumentHelper document.TokenizedText document.RecordId workingIndexPair
    
    let rec createIndexesHelper documentItems workingIndexPair =

            match documentItems with
            | head::tail -> createIndexesHelper tail (searchDocument head workingIndexPair)
            | [] -> workingIndexPair

    let finalizeIndex indexMap =
        indexMap |> Map.map (fun key value ->
            (Seq.ofList value) |> Seq.groupBy (fun elem ->
                elem) |> Seq.map (fun (key,values) -> 
                    {
                        key=key;
                        frequency=(Seq.length values);
                    }))

    //creates the inverted and non inverted index
    let createIndexes documentItems =
         let workingIndexPair=createIndexesHelper documentItems {wNonInvertedIndex=Map.empty; wInvertedIndex=Map.empty}
         {
            nonInvertedIndex=finalizeIndex workingIndexPair.wNonInvertedIndex;
            invertedIndex= finalizeIndex workingIndexPair.wInvertedIndex;
         }

    let calculateIdf invertedIndex documentCount =
        invertedIndex |> Map.map (fun indexKey indexValues ->
            (log (double(1+documentCount)/double(1+(Seq.length indexValues)))))
    
    let calculateTermWeight invertedIndex (idf:Idf) =
        let findIdfValue indexKey =
            match idf.TryFind(indexKey) with
            | Some(idfValue) -> idfValue
            | None -> 0.0

        invertedIndex |> Map.map (fun indexKey indexValues ->
            indexValues |> Seq.map (fun indexValue ->
                (indexValue.key,(double(indexValue.frequency) * (findIdfValue indexKey)))))
    //let calculateDnorm nonInvertedIndex 

   
  
