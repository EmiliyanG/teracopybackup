

open System
open System.IO
open System.Xml
open System.Xml.Linq
open System.Text.RegularExpressions
open System.Diagnostics
open myApp
open myApp.ExtractClientsXML
open myApp.FileOperations


//process all files for client
let processClient teraCopyExe deleteFiles client =
    match client.name with
    //check if name attribute was specified
    | Some n ->
        printfn "%s started backing up files for client %s" (getTime()) n
        //check if there are no files against the specified client
        if Seq.isEmpty client.filesToBackup then printfn "%s no files found for client %s" (getTime()) n
        else client.filesToBackup |> Seq.iter (processFile teraCopyExe deleteFiles)
    
    | None ->  printfn "%s could not process client due to invalid format > No name attribute found" (getTime())

//process all clients
let processClients teraCopyExe deleteFiles clients =
    clients |> Seq.iter (processClient teraCopyExe deleteFiles)



// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



let pathRegex = "^(?:[\\w]\\:|\\\\)(\\\\[a-zA-Z_\\-\\s0-9\\.\\$]+)+\.(xml)$"
let teraCopyRegex = "^(?:[\\w]\\:|\\\\)(\\\\[a-zA-Z_\\-\\s0-9\\.\\$]+)+\.(exe)$"
let deleteFilesRegex = "^([0-9]+)|((all){1})$"
type CommandLineOptions = {teraCopy: string option; xmlPath: string option; deleteFiles: string option}

let defaultOptions = {teraCopy = None;xmlPath = None; deleteFiles = None}


let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some str
   else None



let rec parseCommandLine optionsSoFar args  = 
    match args with 
    // empty list means we're done.
    | [] -> optionsSoFar  

    // match teracopy flag
    | "--teraCopy"::xs -> 
        match xs with 
        | (ParseRegex teraCopyRegex x)::xss -> 
            let newOptionsSoFar = { optionsSoFar with teraCopy=Some x}
            parseCommandLine newOptionsSoFar xss  
        | _ ->
            eprintfn "--teraCopy needs a second argument" 
            parseCommandLine optionsSoFar xs  

    // match subdirectories flag
    | "--xmlPath"::xs -> 
        match xs with
        | (ParseRegex pathRegex x)::xss ->
            let newOptionsSoFar = { optionsSoFar with xmlPath=Some x}
            parseCommandLine newOptionsSoFar xss  
        | _ ->
            eprintfn "--xmlPath needs a second argument"
            parseCommandLine optionsSoFar xs  
    | "--deleteFiles"::xs ->
        match xs with
        |(ParseRegex deleteFilesRegex x)::xss ->
            let newOptionsSoFar = { optionsSoFar with deleteFiles=Some x}
            parseCommandLine newOptionsSoFar xss
        | _ -> 
            eprintfn "--deleteFiles needs a second argument"
            parseCommandLine optionsSoFar xs 

    // handle unrecognized option and keep looping
    | x::xs -> 
        eprintfn "Option '%s' is not recognized" x
        parseCommandLine optionsSoFar xs  

[<EntryPoint>]
let main argv = 
    let m =
        match Array.toList argv with
        | [] -> defaultOptions
        | [x] -> defaultOptions
        | xs -> xs |> parseCommandLine defaultOptions 

    match m.xmlPath, m.teraCopy with
    | Some x, Some y -> 
        let clients = extractedClients x 
        //printClients clients
        processClients y m.deleteFiles clients
        
    | _ , _ -> printfn "no appropriate params provided"


    
    0 // return an integer exit code 
