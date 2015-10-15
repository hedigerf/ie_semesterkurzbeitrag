namespace Semesterkurzbeitrag
open System
open System.IO
open System
open FSharp.Data
open System.Xml.Linq

[<AutoOpen>]
module TypeDomain =

    type Collection = XmlProvider<"Resources/irg_collection.trec">

    type Queries = XmlProvider<"Resources/irg_queries.trec">

    type InvertedIndexOccurence(referencedDocIds) =
        member this.RefecencedDocIds = Set.ofList (List.sort referencedDocIds) 
        member this.Frequency = List.length referencedDocIds
        
        member this.Add referencedDocId =
            new InvertedIndexOccurence(referencedDocId :: referencedDocIds)

    type IndexPair = {nonInvertedIndex: Map<int,InvertedIndexOccurence>;
                      invertedIndex: Map<string,InvertedIndexOccurence>}

    type TrecEntry(recordId,originalText,tokenizedText) =
        member this.RecordId: int = recordId
        member this.OriginalText: string = originalText
        member this.TokenizedText: List<string> = tokenizedText

    