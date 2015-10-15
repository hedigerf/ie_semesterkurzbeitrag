namespace Semesterkurzbeitrag
module Indexes =


    let createIndexes documentItems =

        let rec helper documentItems indexPair =

            let searchDocument (document:TrecEntry) indexPair =
         
                let rec helper tokenizedText indexPair=

                    let addEntry word recordId indexPair =

                        let addIndex (index: Map<_,InvertedIndexOccurence>) key value =
                            match index.TryFind(key) with
                            | Some(occ) -> index.Remove(key).Add(key,occ.Add(value))
                            | None -> index.Add(key,InvertedIndexOccurence([value]))

                        printfn "Adding Index for Document: %i and Term: %s" recordId word 
                        {
                            nonInvertedIndex=(addIndex indexPair.nonInvertedIndex recordId word);
                            invertedIndex=(addIndex indexPair.invertedIndex word recordId;)
                        }

                    match tokenizedText with
                    | head::tail -> helper tail (addEntry head document.RecordId indexPair)
                    | [] ->  indexPair

                printfn "Searching Document: %i" document.RecordId 
                helper document.TokenizedText indexPair

            match documentItems with
            | head::tail -> helper tail (searchDocument head indexPair)
            | [] -> indexPair
        
        helper documentItems {nonInvertedIndex=Map.empty; invertedIndex=Map.empty}
