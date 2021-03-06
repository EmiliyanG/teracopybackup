﻿

open System
open System.IO
open System.Diagnostics
open myApp
open myApp.ExtractClientsXML
open myApp.ParseCommandLineArgs
open myApp.Mail
open System.Collections.Generic

let mutable errorsList = new List<string>()

type myFile = {filePath:string; dateCreated:DateTime}
//function to return the current date time 
let getTime() =  ( System.DateTime.Now.ToString("dd-MM-yyyy HH:mm:ss")) + " > " 

let (^) l r = sprintf "%s%s" l r

//get IO.File last date modified
let getDateModified filePath =
    let myFile = System.IO.FileInfo(filePath)
    myFile.LastWriteTime

//get all files within given directory with the specified extension
let getfilesForDirectory dir ext clientName = 
        try
            Some ((System.IO.Directory.GetFiles(dir, "*."+ ext)) 
                    |> Seq.map (fun x -> {filePath=x; dateCreated=getDateModified x}))
        with 
            | :? System.UnauthorizedAccessException -> 
                printfn "%s Unable to access directory \"%s\" due to invalid permissions - Unauthorized access." (getTime()) dir
                errorsList.Add ( getTime()+" processing client: \""+clientName+"\"."+
                    "\nUnable to access directory \""+dir+"\" due to invalid permissions - Unauthorized access.")
                None 
            | :? System.IO.DirectoryNotFoundException ->
                printfn "%s Directory \"%s\" does not exist." (getTime()) dir
                errorsList.Add ( getTime()+" processing client: \""+clientName+"\"."+
                    "\nDirectory \""+dir+"\" does not exist.")
                None
            | :? System.IO.IOException -> 
                printfn "%s A network error has occurred while trying to access files in directory \"%s\"" (getTime()) dir
                errorsList.Add ( getTime()+" processing client: \""+clientName+"\"."+
                    "\nA network error has occurred while trying to access files in directory \""+dir+"\"")
                None
            | e -> 
                printfn "%s Unspecified error occured while trying to access files in directory \"%s\"" (getTime()) dir
                errorsList.Add ( getTime()+" processing client: \""+clientName+"\"."+
                    "\nUnspecified error occured while trying to access files in directory \""+dir+"\"")
                None
    
let deleteFilesInDirectory files = 
    files |> fun x -> System.IO.File.Delete(x.filePath)
        
    
//get most recently modified file for the specified extension in the given directory 
let getMostRecentFile dir ext clientName =       
    match getfilesForDirectory dir ext clientName  with 
    | Some x -> 
        let allFiles = 
            x |> Seq.sortByDescending(fun x-> x.dateCreated)
        if Seq.isEmpty allFiles then None
        else Some (Seq.head allFiles)

    | None -> None
    
//copy file from source to destination Directory using TeraCopy
let copyFile clientName teraCopyExe sourceFile destination= 

    match File.Exists teraCopyExe, File.Exists sourceFile  with 
    |true, true ->
        
        //skip file if already exists in the destination folder
        let command = "Copy \""+sourceFile+"\" \""+destination+"\" /SkipAll /Close"
        
        printfn "%s started copying file \"%s\" to \"%s\"" (getTime()) sourceFile destination
        
        //command line > TeraCopy.exe Copy <sourceFile> <destination folder> /SkipAll
        let myProcess = System.Diagnostics.Process.Start(teraCopyExe,command)
        //wait for 2 hours
        myProcess.WaitForExit(9000000) |> ignore
        //if the process is still running terminate it
        match myProcess.HasExited with
        | true -> printfn "%s file copied successfully " (getTime()) 
        | false -> myProcess.Kill()
                   errorsList.Add ( getTime()+" processing client: "+clientName+ 
                                    ". Copying process has failed for file \""+sourceFile+"\"."+
                                    "\nThe process had to be killed. Possible reasons can be:\n timeout; "+
                                    "\nanother process was trying to run teracopy in the same time." )
        

        if myProcess.ExitCode <> 0 then 
            printfn "%s process had to be terminated exitCode is %i " (getTime())  myProcess.ExitCode
            
    | true, false -> 
        printfn "%s Tried copying file \"%s\" but the file does not exist" (getTime()) sourceFile
        errorsList.Add ( getTime()+" processing client: \""+clientName+"\"."+
                                    "\nTried copying file \""+sourceFile+"\" but the file does no longer exists in the specified directory.")
    |false, _ -> 
        printfn "%s Tried copying file \"%s\" but the TeraCopy.exe path \"%s\" is invalid" (getTime()) sourceFile teraCopyExe
        errorsList.Add ( getTime()+" processing client: "+clientName+"."+
                                    "\nTried copying file \""+sourceFile+"\" but the TeraCopy.exe path \""+teraCopyExe+"\" is invalid.")

let (|Int|_|) str =
    match System.Int32.TryParse(str) with
    | (true,int) -> Some(int)
    | _ -> None


let deletePreviousFiles deleteFiles dir ext clientName =
    match deleteFiles with
    //delete only files older than x hours
    | Some (Int x) -> 
        printfn "%s deleting files older than %i hours for directory \"%s\"" (getTime()) x dir
        match getfilesForDirectory dir ext clientName with
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
        match getfilesForDirectory dir ext clientName with
        | Some files -> 
            files |> Seq.iter (fun x -> System.IO.File.Delete(x.filePath))
        | None -> printfn "%s no files with matching extension found" (getTime())
    //no command provided to delete any files
    | _ -> printfn "%s Will not delete any files as no delete argument was specified" (getTime())

//process a single file for a client
let processFile clientName teraCopyExe deleteFiles (f:xFile) =
    //deletePreviousFiles deleteFiles

    match f.extension, f.source, f.destination with 
    | Some ext, Some src, Some dest -> 
        if not (Directory.Exists dest) 
            then 
                printfn "%s creating destination directory \"%s\"" (getTime()) dest
                Directory.CreateDirectory dest |> ignore
            

        match (getMostRecentFile src ext clientName) with
        |Some x -> let fileName = Path.GetFileName(x.filePath)
                   printfn "%s The most recently modified file in directory \"%s\" is \"%s\" " (getTime()) src fileName
                   
                   
                   match File.Exists (dest ^ "\\" ^ fileName ) with 
                   | true -> printfn "%s Skipping file \"%s\". The file already exists in directory \"%s\"" (getTime()) (Path.GetFileName(x.filePath)) dest
                   | false ->
                       deletePreviousFiles deleteFiles dest ext clientName
                       try
                            copyFile clientName teraCopyExe x.filePath dest
                       with
                            | e -> 
                                let msg = getTime()+" processing client: "+clientName+ 
                                                ". Copying process has failed for file \""+x.filePath+"\"."+
                                                "\nThe process had to be killed. Here is the exception message:"+
                                                "\n" + e.ToString()

                                errorsList.Add ( msg )
                                printfn "%s" msg


        |None -> printfn "%s no files found for directory \"%s\"" (getTime()) src
    | _, _, _-> 
        printfn "%s file could not be processed due to invalid format in the XML file" (getTime())
        errorsList.Add ( getTime() ^ " processing client: \"" ^ clientName ^ "\"." ^
                                    "\nFile tag in the clients.xml file has invalid format and could not be processed.")
        
       

//process all files for client
let processClient teraCopyExe deleteFiles client =
    match client.name with
    //check if name attribute was specified
    | Some n ->
        printfn "%s started backing up files for client %s" (getTime()) n
        //check if there are no files against the specified client
        if Seq.isEmpty client.filesToBackup then 
            printfn "%s no files found for client %s" (getTime()) n
            errorsList.Add ( getTime()+" processing client: "+n+"."+
               "\nNo files to backup specified in the clients.xml file.")

        else client.filesToBackup |> Seq.iter (processFile n teraCopyExe deleteFiles)
    
    | None ->  
        printfn "%s could not process client due to invalid format > No name attribute found" (getTime())
        errorsList.Add ( getTime()+" a client in the clients.xml file was found with no name attribute. The client was skipped")

//process all clients
let processClients teraCopyExe deleteFiles clients =
    clients |> Seq.iter (processClient teraCopyExe deleteFiles)

let emailErrors (errorsList:List<String>) (mailSettings:xEmail) = 
    match errorsList with
    |x when x.Count = 0 -> printfn "no errors to email"
    |xs ->
        
        let errors =
            Seq.toList xs
            |> List.map (fun x ->"<tr><td>"+x+"</td></tr>" ) 
            |> String.concat "" 

        let msg = 
            "<p>Dear Sir\\Madam,</p>"+
            "<p>The backup process finished with errors:</p>"+
            "<table><tbody>"+
            errors +
            "</table></tbody>"+
            "<br /><p>Please fix these issues.</p>"
        ()
        sendEmail mailSettings msg |> ignore
    

[<EntryPoint>]
let main argv =
   //type CommandLineOptions = {teraCopy: string option; xmlPath: string option; deleteFiles: string option}
    let m =
        match Array.toList argv with
        | [] -> defaultOptions
        | [x] -> defaultOptions
        | xs -> xs |> parseCommandLine defaultOptions 
            

    match m.xmlPath, m.teraCopy with
    | Some x, Some y -> 
        let clients = extractedClients x 
        processClients y m.deleteFiles clients

        match (loadConfigs x) with
        | Some mailSettings -> 
           emailErrors errorsList mailSettings
        | None -> printfn "no email settings were provided. Unable to send email."

    | _ , _ -> printfn "no appropriate command line params provided"
    
    
    0 // return an integer exit code 
