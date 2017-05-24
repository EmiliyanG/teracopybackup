open System
open System.IO
open System.Xml
open System.Xml.Linq
open System.Text.RegularExpressions
open System.Diagnostics

type xFile = {extension:string Option; source:string Option; destination:string Option}
type xClient = {name:string Option; filesToBackup: xFile seq}

let getTime() =  ( System.DateTime.Now.ToString("dd-MM-yyyy HH:mm:ss")) + " > " 

let xn s = XName.Get(s) 
let xnAttribute (f:XElement) attr = 
    match f.Attribute(xn attr) with
    | null -> None
    | x -> Some x.Value

let createXFile (f:XElement) = 
    {extension = xnAttribute f "extension"; 
    source = xnAttribute f "source";
    destination = xnAttribute f "destination"} 

let createXFileList files = 
    files |> Seq.map createXFile
    
let createClient (client:XElement) = 
    {name = (xnAttribute client "name");
    filesToBackup = (createXFileList (client.Elements(xn "file")))}
     
    
let extractedClients (path:string) =
    let doc = 
        try
            Some (XDocument.Load(path))
        with 
            | :? System.IO.FileNotFoundException -> None
    match doc with 
    | Some d ->
        d.Element(xn "clients").Elements(xn "client") 
        |> Seq.map createClient 
    | None -> printfn "unable to find the xml file" 
              Seq.empty



//let printClients clients =
//    if Seq.isEmpty clients then printfn "no clients found in the XML file"
//    else
//        clients
//               |> Seq.iter (fun x -> 
//                         printfn "client %s " x.name
//                         if Seq.isEmpty x.filesToBackup then printfn "no files found for client %s" x.name
//                         else
//                             x.filesToBackup
//                             |> Seq.iter (fun f -> 
//                                printfn "extension: %s \n source: %s \n destination: %s" (f.extension) (f.source) (f.destination))
//                        )

type myFile = {filePath:string; dateCreated:DateTime}

let getDateCreated filePath =
    let myFile = System.IO.FileInfo(filePath)
    myFile.LastWriteTime

let printFiles (f:myFile) = 
    printfn "%s ; %s" f.filePath (f.dateCreated.ToString("dd-mm-yy"))

//get most recently modified file for the specified extension in the given directory 
let getMostRecentFile dir ext =       
    
    let files = 
        try
            Some (System.IO.Directory.GetFiles(dir, "*."+ ext))
        with 
            | :? System.IO.FileNotFoundException -> None
            | :? System.IO.DirectoryNotFoundException -> None

    match files with 
    | Some x -> 
       let allFiles = 
           x
           |> Seq.map (fun x -> {filePath=x; dateCreated=getDateCreated x})
           |> Seq.sortByDescending(fun x-> x.dateCreated)
       if Seq.isEmpty allFiles then None
       else Some (Seq.head allFiles)

    | None -> printfn "Directory %s does not exist" dir
              None
    

let getFileNames files = 
    files
    |> Seq.map (fun x -> (Path.GetFileName(x)))

let moveFile teraCopyExe sourceFile destination= 

 
    match File.Exists teraCopyExe, File.Exists sourceFile  with 
    |true, true ->
        
        //skip file if already exists in the destination folder
        let command = "Copy \""+sourceFile+"\" \""+destination+"\" /SkipAll"
        
        printfn "%s started copying file \"%s\" to \"%s\"" (getTime()) sourceFile destination
        
        //command line > TeraCopy.exe Copy <sourceFile> <destination folder> /SkipAll
        let myProcess = System.Diagnostics.Process.Start(teraCopyExe,command)
        //wait for 2 hours
        myProcess.WaitForExit(7200000) |> ignore
        //if the process is still running terminate it
        match myProcess.HasExited with
        | true -> printfn "%s file copied successfully " (getTime()) 
        | false -> myProcess.Kill()
        

        if myProcess.ExitCode <> 0 then 
            printfn "%s process had to be terminated exitCode is %i " (getTime())  myProcess.ExitCode
            

    | true, false -> printfn "%s Tried copying file %s but the file does not exist" (getTime()) sourceFile
    |false, _ -> printfn "%s Tried copying file %s but the TeraCopy.exe path \"%s\" is invalid" (getTime()) sourceFile teraCopyExe

//process a single file for a client
let processFile teraCopyExe (f:xFile)  =
    match f.extension, f.source, f.destination with 
    | Some ext, Some src, Some dest -> 
        if not (Directory.Exists dest) 
            then 
                printfn "%s creating destination directory %s" (getTime()) dest
                Directory.CreateDirectory dest |> ignore
    
        match (getMostRecentFile src ext) with
        |Some x -> printfn "%s The most recently modified file is %s " (getTime()) (Path.GetFileName(x.filePath))
                   moveFile teraCopyExe x.filePath dest
        |None -> printfn "%s no files found for directory %s" (getTime()) src
    | _, _, _-> printfn "%s file could not be processed due to invalid format in the XML file" (getTime())

//process all files for client
let processClient teraCopyExe client =
    match client.name with
    //check if name attribute was specified
    | Some n ->
        printfn "%s started backing up files for client %s" (getTime()) n
        //check if there are no files against the specified client
        if Seq.isEmpty client.filesToBackup then printfn "%s no files found for client %s" (getTime()) n
        else client.filesToBackup |> Seq.iter (processFile teraCopyExe)
    
    | None ->  printfn "%s could not process client due to invalid format > No name attribute found" (getTime())

//process all clients
let processClients teraCopyExe clients =
    clients |> Seq.iter (processClient teraCopyExe)



// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.



let pathRegex = "^(?:[\\w]\\:|\\\\)(\\\\[a-zA-Z_\\-\\s0-9\\.\\$]+)+\.(xml)$"
let teraCopyRegex = "^(?:[\\w]\\:|\\\\)(\\\\[a-zA-Z_\\-\\s0-9\\.\\$]+)+\.(exe)$"

type CommandLineOptions = {teraCopy: string option; xmlPath: string option; }

let defaultOptions = {teraCopy = None;xmlPath = None;}

let testRegex rgx str= 
    let m = Regex(rgx).Match(str)
    if m.Success then true
    else false

let rec parseCommandLine optionsSoFar args  = 
    match args with 
    // empty list means we're done.
    | [] -> optionsSoFar  

    // match teracopy flag
    | "--teraCopy"::xs -> 
        match xs with 
        | x::xss when testRegex pathRegex x -> 
            let newOptionsSoFar = { optionsSoFar with teraCopy=Some x}
            parseCommandLine newOptionsSoFar xss  
        | _ ->
            eprintfn "--teraCopy needs a second argument"
            parseCommandLine optionsSoFar xs  

    // match subdirectories flag
    | "--xmlPath"::xs -> 
        match xs with
        | x::xss when testRegex teraCopyRegex x ->
            let newOptionsSoFar = { optionsSoFar with xmlPath=Some x}
            parseCommandLine newOptionsSoFar xss  
        | _ ->
            eprintfn "--xmlPath needs a second argument"
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
        processClients y clients
        
    | _ , _ -> printfn "no appropriate params provided"


    
    0 // return an integer exit code 
