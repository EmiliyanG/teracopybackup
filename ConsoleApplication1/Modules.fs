namespace myApp

open System



type xFile = {extension:string Option; source:string Option; destination:string Option}
type xClient = {name:string Option; filesToBackup: xFile seq}
    
//contains all functions used to convert the clients.xml file to some useful data structures to work with
module ExtractClientsXML =
    
    open System.Xml
    open System.Xml.Linq

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




module ParseCommandLineArgs = 
   
    open System.Text.RegularExpressions

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

module Mail =

    open System
    open System.Net.Mail
    open System.Net
    
    let sendEmailTest smtp msg= 
        printfn "sending email"
        let fromAddress = new MailAddress("emiliyang@illycorp.com", "From Name")
        let toAddress = new MailAddress("grigorov.emiliyan@gmail.com", "To Name")
        //let fromPassword = "password"
        let subject = "Backup process finished with errors"
        

        let smtp = new SmtpClient(smtp)
        smtp.Port <- 587
        smtp.EnableSsl <- true
        smtp.DeliveryMethod <- SmtpDeliveryMethod.Network
        smtp.UseDefaultCredentials <- false
        //smtp.Credentials <- new NetworkCredential(fromAddress.Address, fromPassword)
    
        let message = new MailMessage(fromAddress, toAddress)
        message.Subject <-subject
        message.Body<-msg
        message.IsBodyHtml <- true
        smtp.Send(message);