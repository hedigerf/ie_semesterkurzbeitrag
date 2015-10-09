namespace Semesterkurzbeitrag
open System

[<AutoOpen>]
module TypeDomain =

    type DocumentItem(recordId,tokenizedText) =
        member this.RecordId = recordId
        member this.TokenizedText = tokenizedText 
