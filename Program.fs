namespace Semesterkurzbeitrag

module Main =

    open System.IO
    open System
    open FSharp.Data
    open System.Xml.Linq


    type Collection = XmlProvider<"Resources/irg_collection.trec">


    [<EntryPoint>]
    let main argv =
       // let parsedFile = File.ReadAllLines("../../Resources/irg_collection.trec")
        //let parsedStr = Array.reduce (fun acc item -> acc + item) parsedFile 
        //let test = Collection.Parse "Resources/irg_collection.trec"   
        let collection =Collection.Load "../../Resources/irg_collection.trec"
        //let docItem = new DocumentItem(12,"abc","abc")
        let abc = collection.Docs;
        //let documentItems = Array.map (fun doc -> new DocumentItem((1, doc.Text, "abc"))) collection.Docs 
        //documentItems.c

        for doc in collection.Docs do
            //printfn "%s" doc.GetType()
            printfn "%s" doc.Text
            printfn "%i" doc.RecordId 
        0;
        //0 // return an integer exit code

