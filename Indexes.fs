namespace Semesterkurzbeitrag
module Indexes =

    type WorkingIndexPair = {wNonInvertedIndex: Map<int,list<string>>;
                      wInvertedIndex: Map<string,list<int>>}

    type IndexPair = {nonInvertedIndex: Map<int,seq<string*int>>;
                      invertedIndex: Map<string,seq<int*int>>}

    type IndexValue<'a> = {key:'a; frequency: int}

    let rec addIndex (index: Map<_,_>) key value =
        match index.TryFind(key) with
        | Some(occ) -> index.Remove(key).Add(key,(value::occ))
        | None -> index.Add(key,[value])
    
    let rec addEntry word recordId (workingIndexPair:WorkingIndexPair) =
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
                elem) |> Seq.map (fun (key,values) -> (key,Seq.length values)))

    let rec createIndexes documentItems =
         let workingIndexPair=createIndexesHelper documentItems {wNonInvertedIndex=Map.empty; wInvertedIndex=Map.empty}
         {
            nonInvertedIndex=finalizeIndex workingIndexPair.wNonInvertedIndex;
            invertedIndex= finalizeIndex workingIndexPair.wInvertedIndex;
         }

   
