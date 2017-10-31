namespace myApp


type xFile = {extension:string Option; source:string Option; destination:string Option}
type xClient = {name:string Option; filesToBackup: xFile seq}
type xEmail = {smtpServer:string Option; smtpServerPort:int Option;fromEmail:string Option; fromEmailPassword:string Option; toEmail:string Option }


module Regex = 
    open System.Text.RegularExpressions

    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)
        if m.Success
        then Some str
        else None

//contains all functions used to convert the clients.xml file to some useful data structures to work with
module ExtractClientsXML =
    open Regex
    open System.Xml
    open System.Xml.Linq

    let xn s = XName.Get(s)

    let xnAttribute (f:XElement) attr = 
        match f.Attribute(xn attr) with
        | null -> None
        | x -> Some x.Value
    
    let extractEmailConfigsFromTag (email:XElement)=
        {smtpServer= xnAttribute email "smtpServer";
        smtpServerPort= (match (xnAttribute email "smtpServerPort") with
                            | Some p -> 
                                match p with
                                | ParseRegex "^[0-9]{2,4}$" p -> Some (p |> int)  //port can be int only
                                | _ -> None
                            | None -> None);
        fromEmail= xnAttribute email "fromEmail";
        fromEmailPassword= xnAttribute email "fromEmailPassword";
        toEmail= xnAttribute email "toEmail"
        }

    let createXFile (f:XElement) = 
        {extension = xnAttribute f "extension"; 
        source = xnAttribute f "source";
        destination = xnAttribute f "destination"} 
        
    let createClient (client:XElement) = 
        {name = (xnAttribute client "name");
        filesToBackup = ( (client.Elements(xn "file")) |> Seq.map createXFile)}

    let loadXDocument (path:string)=
        try
            let doc = Some (XDocument.Load(path))
            
            match doc with
            |Some d -> 
                match (d.Element(xn "root")) with // the root element is called root
                | null -> None
                | x -> Some x
            |None -> None
        with 
            | :? System.IO.FileNotFoundException -> None
    
    let extractedClients path =
        let doc = loadXDocument(path)
        match doc with 
        | Some d ->
            d.Element(xn "clients").Elements(xn "client") 
            |> Seq.map createClient 
        | None -> printfn "unable to find the xml file" 
                  Seq.empty

    let loadConfigs path=
        let doc = loadXDocument(path)
        match doc with
        |Some d -> 
            match d.Element(xn "config") with
            | null -> None //file found but no config tag exists
            | c -> 
                match c.Element(xn "email") with
                | null -> None //config tag found but no child email tag exists
                | email -> Some (extractEmailConfigsFromTag email)  
        |none -> None //file not found


module ParseCommandLineArgs = 
    open Regex

    let pathRegex = "^(?:[\\w]\\:|\\\\)(\\\\[a-zA-Z_\\-\\s0-9\\.\\$]+)+\.(xml)$"
    let teraCopyRegex = "^(?:[\\w]\\:|\\\\)(\\\\[a-zA-Z_\\-\\s0-9\\.\\$]+)+\.(exe)$"
    let deleteFilesRegex = "^([0-9]+)|((all){1})$"
    type CommandLineOptions = {teraCopy: string option; xmlPath: string option; deleteFiles: string option}

    let defaultOptions = {teraCopy = None;xmlPath = None; deleteFiles = None}
    
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

    let choose (v1, v2)=
        match v1 with
        | Some v -> v
        | None -> v2
    let sendEmail (mailSettings:xEmail) msg= 
        
        match mailSettings.smtpServer, mailSettings.toEmail with
        | Some smtp, Some e -> 
            let smtp = new SmtpClient(smtp)
            smtp.Port <- choose(mailSettings.smtpServerPort,25)
            smtp.DeliveryMethod <- SmtpDeliveryMethod.Network

            match mailSettings.fromEmail, mailSettings.fromEmailPassword with
            | Some fe, Some p -> 
                smtp.EnableSsl <- true 
                smtp.Credentials <- new NetworkCredential(fe, p)
                
            | _,_ -> smtp.UseDefaultCredentials <- true
            
            let message = new MailMessage(new MailAddress(
                                            choose(mailSettings.fromEmail,"backups@mail.com")//from
                                          ), 
                                          new MailAddress(e))//to 
            message.Subject <-"Backup process finished with errors"
            message.Body<-msg
            message.IsBodyHtml <- true
            smtp.Send(message);
            true
        | _,_-> false

        
    
    let testEmailSentSuccessfully toEmail=
        printfn "about to send test email to recepient: %s" toEmail
