namespace Semesterkurzbeitrag
open System.IO
module Indexes =

    type WorkingIndexPair = {wNonInvertedIndex: Map<int,list<string>>;
                      wInvertedIndex: Map<string,list<int>>}

    type IndexValue<'a> = {key:'a; frequency: int}

    type NonInvertedIndex = Map<int,seq<IndexValue<string>>>

    type InvertedIndex = Map<string,seq<IndexValue<int>>>

    type IndexPair = {nonInvertedIndex: NonInvertedIndex;
                      invertedIndex: InvertedIndex}

    type IdfMap = Map<string,double>

    type DNormMap = Map<int,double>

    type QNormMap = Map<int,double>

    type Accumulator = Map<int,double>

    ///term,Map<docId,weight>
    type TermWeights = Map<string,Map<int,double>>

    ///coded for performance reasons
    let concat value occ =
        value::occ
    ///coded for performance reasons
    let create value=
        [value]

    ///ads an index entry to an existing index
    let rec addIndex (index: Map<_,_>) key value applyToExisting createNew =
        match index.TryFind(key) with
        | Some(occ) -> index.Remove(key).Add(key,(applyToExisting value occ))
        | None -> index.Add(key,createNew value)

    ///adds an entry to both indexes
    let rec addEntry word recordId (workingIndexPair:WorkingIndexPair) =
        //printfn "Add word %s in %i to index" word recordId
        {
            wNonInvertedIndex=(addIndex workingIndexPair.wNonInvertedIndex recordId word concat create);
            wInvertedIndex=(addIndex workingIndexPair.wInvertedIndex word recordId concat create;)
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
    
    let calculateTermWeight invertedIndex (idf:IdfMap) =

        let findIdfValue indexKey =
            match idf.TryFind(indexKey) with
            | Some(idfValue) -> idfValue
            | None -> 0.0

        invertedIndex |> Map.map (fun indexKey indexValues ->
            Map.ofSeq (indexValues |> Seq.map (fun indexValue ->
                (indexValue.key,(double(indexValue.frequency) * (findIdfValue indexKey))))))

    let calculateDnorm (nonInvertedIndex:NonInvertedIndex) (termWeights:TermWeights) =

        let findTermWeight term docId =
            match termWeights.TryFind(term) with
            | Some(weightPerDocs) -> match weightPerDocs.TryFind(docId) with
                                     | Some(weight) -> weight
                                     | None -> 1.0
            | None -> 1.0

        nonInvertedIndex |> Map.map (fun indexKey indexValues ->
            sqrt (pown ((indexValues |> Seq.fold (fun acc indexValue ->
                 (findTermWeight indexValue.key indexKey)+acc ) 0.0)) 2))

    ///calculate Qnorm for one query and extend the Accumulator
    let calculateQnormQuery (query:TrecEntry) (queriesIndex:NonInvertedIndex) documentCount (idfMap:IdfMap) (inverseIndexDocs: InvertedIndex) (startAcc:Accumulator) =
        (query.TokenizedText |> List.fold (fun (qNormVal,(acc:Accumulator)) queryTerm ->
            let idfValue = match idfMap.TryFind queryTerm with
                           | Some(idfValue) -> idfValue 
                           | None -> log (double(1 + documentCount))
            let frequency = match queriesIndex.TryFind query.RecordId with
                            | Some(indexValue) -> (indexValue |> Seq.find (fun indexTerm -> queryTerm = indexTerm.key)).frequency
                            | None -> 0
            let b = idfValue * (double frequency)
            //pass intermediate accu as starting point of fold
            let newAccumulator : Accumulator = match inverseIndexDocs.TryFind queryTerm with
                                                | Some(indexValueSeq) ->
                                                    indexValueSeq |> Seq.fold (fun tempAcc indexValue ->
                                                        //printfn "Processing queryId: %i queryTerm: %s docId: %i" query.RecordId queryTerm indexValue.key
                                                        let a = idfValue * (double indexValue.frequency)
                                                        addIndex tempAcc indexValue.key (a*b) (fun x y -> x + y) (fun x -> x)) acc
                                                | None -> acc
            ((qNormVal + (pown b 2))),newAccumulator) (0.0,startAcc))

    /// calculateQnorm and retuns the QnormMap and an Accumulator 
    let calculateQnorm (queries:list<TrecEntry>) (queriesIndex:NonInvertedIndex) documentCount (idfMap:IdfMap) (inverseIndexDocs: InvertedIndex)=
        (queries |> List.fold (fun ((qNorm:QNormMap),(accumulator:Accumulator)) query ->
            let (qNormQuery,acc:Accumulator) = calculateQnormQuery query queriesIndex documentCount idfMap inverseIndexDocs accumulator 
            (qNorm.Add(query.RecordId,(sqrt qNormQuery)),acc))(Map.empty,Map.empty))



    let calculateRsv (queries:list<TrecEntry>) (documents:list<TrecEntry>) (accu:Accumulator) (dNorm:DNormMap) (qNorm:QNormMap) =
        queries |> List.map (fun query ->
            let searchResult=documents |> List.map (fun doc ->
                let rsv = match accu.TryFind doc.RecordId with
                          | None -> //printfn "No accu value found for DocumentId: %i" doc.RecordId
                                    (999.9,doc.RecordId,query.RecordId,0.0,0.0,0.0)
                          | Some(accuValue)-> match dNorm.TryFind doc.RecordId with
                                              | None -> //printfn "No dNorm value found for DocumentId: %i" doc.RecordId
                                                        (999.9,doc.RecordId,query.RecordId,accuValue,0.0,0.0)
                                              | Some(dNormValue) -> match qNorm.TryFind query.RecordId with
                                                                    | None -> //printfn "No dNorm value found for queryId: %i" query.RecordId
                                                                              (999.9,doc.RecordId,query.RecordId,accuValue,dNormValue,0.0)
                                                                    | Some(qNormValue) ->
                                                                        let rsv = accuValue/(dNormValue * qNormValue) 
                                                                        //printfn "QueryId: %i DocumentId: %i RSV: %f " query.RecordId doc.RecordId rsv
                                                                        (rsv,doc.RecordId,query.RecordId,accuValue,dNormValue,qNormValue)
                rsv                                                          
                )
            let sorted=searchResult |> List.sortBy (fun (rsv,documentId,queryId,accuValue,dNormValue,qNormValue) -> rsv) 
            let firstThousand = (Seq.ofList sorted) |> Seq.take 1000
            let sb = (new System.Text.StringBuilder()).Append("").Append(query.RecordId).Append("_query_rsv_calc.log")
            let stream = new StreamWriter(sb.ToString(), false)
            stream.WriteLine("This line overwrites file contents!")
            firstThousand |> Seq.iter (fun (rsv,documentId,queryId,accuValue,dNormValue,qNormValue) ->
                stream.WriteLine("QueryId: {0}, DocumentID: {1}, RSV: {2}, Accu: {3}, dNorm: {4}, qNorm {5}",queryId,documentId,rsv,accuValue,dNormValue,qNormValue)
                printfn "QueryId: %i DocumentId: %i RSV: %f " queryId documentId rsv)
            stream.Close
            firstThousand)


  

 
    