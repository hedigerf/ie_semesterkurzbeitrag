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

    let addIfNotExists (idfMap:IdfMap) idfKey idfValue =
        if idfMap.ContainsKey idfKey then
            idfMap
        else
            idfMap.Add(idfKey,idfValue)

   
    let calculateIdf(invertedIndex:InvertedIndex) documentCount = 
        let asArray = Map.toArray invertedIndex
        let result=asArray |> Array.Parallel.map (fun(word,seq) ->
            let documentFrequency = Seq.length seq
            let idfValue = log( ((double(1+documentCount)) / (double(1 + documentFrequency))))
            (word, (double idfValue))
        )
        Map.ofArray result
         
                
    
    ///calculate dNorm       
    let calculateDnorm (nonInvertedIndex:NonInvertedIndex) (idf:IdfMap) documentCount =
        
        ///calculate dNorm for one document
        let calculateDnormForDoc (idf:IdfMap) documentCount (docId,words)=
            let dNormDoc = words |> Seq.fold (fun (dNormDoc:double) word ->
                    let idfValue = match idf.TryFind word.key with //number of documents which contain the term
                                            | Some(idfValue) -> idfValue
                                            | None -> 0.0; 
                    let a = idfValue * (double word.frequency) 
                    let aHigh2 = pown a 2
                    let newDnormDoc = dNormDoc + aHigh2
                //let newIdfMap = addIfNotExists tempIdf word.key (double idfValue)
                    newDnormDoc)(1.0)
            (docId,sqrt dNormDoc)

        let asArray = (Map.toArray nonInvertedIndex)
        let result=asArray |> Array.Parallel.map (calculateDnormForDoc idf documentCount)
        Map.ofArray result
     
    /// calculateQnorm and retuns the QnormMap and an Accumulator 
    let createQueryProcessingList (queries:list<TrecEntry>) (queriesIndex:NonInvertedIndex) documentCount (idfMap:IdfMap) (inverseIndexDocs: InvertedIndex) =

        ///calculates qNorm for a term
        let calculateQnormTerm queryId (qNormVal,(acc:Accumulator)) queryTerm =
                let idfValue = match idfMap.TryFind queryTerm with
                               | Some(idfValue) -> idfValue 
                               | None -> log (double(1 + documentCount))
                let frequency = match queriesIndex.TryFind queryId with
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
                ((qNormVal + (pown b 2)),newAccumulator)

        ///calculate Qnorm for one query and generate the accumulator
        let calculateQnormQuery (queriesIndex:NonInvertedIndex) documentCount (idfMap:IdfMap) (inverseIndexDocs: InvertedIndex) (query:TrecEntry)  =
            let (qNormQuery,acc) = (query.TokenizedText |> List.fold (calculateQnormTerm query.RecordId) (0.0,Map.empty))
            query.RecordId,sqrt qNormQuery,acc

        let asArray = Array.ofList queries
        asArray |> Array.Parallel.map (calculateQnormQuery queriesIndex documentCount idfMap inverseIndexDocs)
    
    ///calculate the rsv for all queries and documents      
    let calculateRsv (queryProcessingList:array<int*double*Accumulator>) (documents:list<TrecEntry>) (dNorm:DNormMap) =

        let calculateRsvDoc queryId qNormValue (acc:Accumulator) (dNorm:DNormMap) (doc:TrecEntry) =
            match acc.TryFind doc.RecordId with
                          | None -> //printfn "No accu value found for DocumentId: %i" doc.RecordId
                                    (0.0,doc.RecordId,queryId,0.0,0.0,0.0)
                          | Some(accuValue)-> match dNorm.TryFind doc.RecordId with
                                              | None -> //printfn "No dNorm value found for DocumentId: %i" doc.RecordId
                                                        (0.0,doc.RecordId,queryId,accuValue,0.0,0.0)
                                              | Some(dNormValue) ->
                                                let rsv = accuValue/(dNormValue * qNormValue)
                                                (rsv,doc.RecordId,queryId,accuValue,dNormValue,qNormValue)
        ///sorts all results and takes the best thousand 
        let takeBest searchResult queryId qNormValue acc =
            let sorted=searchResult |> List.sortBy (fun (rsv,documentId,queryId,accuValue,dNormValue,qNormValue) -> -rsv) 
            (Seq.ofList sorted) |> Seq.take 1000

        ///calculates rsv for one query
        let calcRsvQuery (documents:list<TrecEntry>) (dNorm:DNormMap) ((queryId:int),qNormValue,acc)=
            let searchResult = documents |> List.map ((calculateRsvDoc queryId qNormValue acc dNorm))
            let firstThousand = takeBest searchResult queryId qNormValue acc
            let sb = (new System.Text.StringBuilder()).Append("").Append(queryId).Append("_query_rsv_calc.log")
            let stream = new StreamWriter(sb.ToString(), false)
            stream.WriteLine("This line overwrites file contents!")
            firstThousand |> Seq.iter (fun (rsv,documentId,queryId,accuValue,dNormValue,qNormValue) ->
               stream.WriteLine("QueryId: {0}, DocumentID: {1}, RSV: {2}, Accu: {3}, dNorm: {4}, qNorm {5}",queryId,documentId,rsv,accuValue,dNormValue,qNormValue))
            stream.Close()
            firstThousand  

        queryProcessingList |> Array.Parallel.map (calcRsvQuery documents dNorm) 
                       
