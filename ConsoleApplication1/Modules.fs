namespace myApp

open System
open System.IO
open System.Xml
open System.Xml.Linq


type xFile = {extension:string Option; source:string Option; destination:string Option}
type xClient = {name:string Option; filesToBackup: xFile seq}
    
//contains all functions used to convert the clients.xml file to some useful data structures to work with
module ExtractClientsXML =
    
    let xn s = XName.Get(s)

    let xnAttribute (f:XElement) attr = 
        match f.Attribute(xn attr) with
        | null -> None
        | x -> Some x.Value

    let createXFile (f:XElement) = 
        {extension = xnAttribute f "extension"; 
        source = xnAttribute f "source";
        destination = xnAttribute f "destination"} 
        
    let createClient (client:XElement) = 
        {name = (xnAttribute client "name");
        filesToBackup = ( (client.Elements(xn "file")) |> Seq.map createXFile)}

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



module FileOperations = 
    type myFile = {filePath:string; dateCreated:DateTime}
    
    //function to return the current date time 
    let getTime() =  ( System.DateTime.Now.ToString("dd-MM-yyyy HH:mm:ss")) + " > " 
    
    //get IO.File last date modified
    let getDateModified filePath =
        let myFile = System.IO.FileInfo(filePath)
        myFile.LastWriteTime

    //get all files within given directory with the specified extension
    let getfilesForDirectory dir ext = 
            try
                Some ((System.IO.Directory.GetFiles(dir, "*."+ ext)) 
                     |> Seq.map (fun x -> {filePath=x; dateCreated=getDateModified x}))
            with 
                | :? System.IO.FileNotFoundException -> None
                | :? System.IO.DirectoryNotFoundException -> None
    
    let deleteFilesInDirectory files = 
        files |> fun x -> System.IO.File.Delete(x.filePath)
        
    
    //get most recently modified file for the specified extension in the given directory 
    let getMostRecentFile dir ext =       
        match getfilesForDirectory dir ext  with 
        | Some x -> 
           let allFiles = 
               x |> Seq.sortByDescending(fun x-> x.dateCreated)
           if Seq.isEmpty allFiles then None
           else Some (Seq.head allFiles)

        | None -> printfn "Directory %s does not exist" dir
                  None
    
    //copy file from source to destination Directory using TeraCopy
    let copyFile teraCopyExe sourceFile destination= 

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

    let (|Int|_|) str =
       match System.Int32.TryParse(str) with
       | (true,int) -> Some(int)
       | _ -> None


    let deletePreviousFiles deleteFiles dir ext =
        match deleteFiles with
        //delete only files older than x hours
        | Some (Int x) -> 
            printfn "%s deleting files older than %i hours for directory %s" (getTime()) x dir
            match getfilesForDirectory dir ext with
            | Some files -> 
                files 
                |> Seq.filter (fun y -> 
                    let diff = System.DateTime.Now - y.dateCreated;
                    diff.TotalHours > float x)
                |> Seq.iter (fun x -> System.IO.File.Delete(x.filePath))
            | None -> printfn "%s there were no files older than %i hours" (getTime()) x
        //delete all files
        | Some "all" -> 
            printfn "%s deleting all files for directory %s" (getTime()) dir
            match getfilesForDirectory dir ext with
            | Some files -> 
                files |> Seq.iter (fun x -> System.IO.File.Delete(x.filePath))
            | None -> printfn "%s no files with matching extension found" (getTime())
        //no command provided to delete any files
        | _ -> printfn "%s Will not delete any files as no delete argument was specified" (getTime())

    //process a single file for a client
    let processFile teraCopyExe deleteFiles (f:xFile) =
        //deletePreviousFiles deleteFiles

        match f.extension, f.source, f.destination with 
        | Some ext, Some src, Some dest -> 
            if not (Directory.Exists dest) 
                then 
                    printfn "%s creating destination directory %s" (getTime()) dest
                    Directory.CreateDirectory dest |> ignore
            
            deletePreviousFiles deleteFiles dest ext

            match (getMostRecentFile src ext) with
            |Some x -> printfn "%s The most recently modified file is %s " (getTime()) (Path.GetFileName(x.filePath))
                       copyFile teraCopyExe x.filePath dest
            |None -> printfn "%s no files found for directory %s" (getTime()) src
        | _, _, _-> printfn "%s file could not be processed due to invalid format in the XML file" (getTime())