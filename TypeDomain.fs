namespace Semesterkurzbeitrag
open System

[<AutoOpen>]
module TypeDomain =

    type TrecEntry(recordId,originalText,tokenizedText) =
        member this.RecordId: int = recordId
        member this.OriginalText: string = originalText
        member this.TokenizedText: List<string> = tokenizedText

    type InvertedIndexOccurence(referencedDocIds) =
        member this.RefecencedDocIds = Set.ofList (List.sort referencedDocIds) 
        member this.Frequency = List.length referencedDocIds
        
        member this.Add referencedDocId =
            new InvertedIndexOccurence(referencedDocId :: referencedDocIds)

    