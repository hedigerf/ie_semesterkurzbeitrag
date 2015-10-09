namespace Semesterkurzbeitrag
open System

[<AutoOpen>]
module TypeDomain =

    type DocumentItem(recordId,originalText,tokenizedText) =
        member this.RecordId: int = recordId
        member this.OriginalText: string = originalText
        member this.TokenizedText: string = tokenizedText 
