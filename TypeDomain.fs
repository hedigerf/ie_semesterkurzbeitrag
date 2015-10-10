namespace Semesterkurzbeitrag
open System

[<AutoOpen>]
module TypeDomain =

    type DocumentItem(recordId,originalText,tokenizedText) =
        member this.RecordId: int = recordId
        member this.OriginalText: string = originalText
        member this.TokenizedText: List<string> = tokenizedText

    type Occurency(referencedDocIds) =
        member this.RefecencedDocIds: List<int> = List.sort referencedDocIds 
        member this.Frequency = List.length referencedDocIds
        
        member this.Add(referencedDocId) =
            new Occurency(referencedDocId :: referencedDocIds)