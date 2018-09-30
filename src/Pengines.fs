(*
Copyright (c) 2016-2017 Virtustream Corporation

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

namespace Pengines

open Chiron.Builder
open Chiron.Formatting
open Chiron.Mapping
open Chiron.Parsing

module Prolog =

    type Operator = 
        | Infix of Term * string * Term
        | Prefix of string * Term
        | Postfix of Term * string

    and Term =
        | Atom of string
        | Number of decimal
        | Variable of Name:string
        | ListTerm of Term list
        | CompoundTerm of Functor:string * Terms:Term seq
        | DictTerm of Map<string, Term>
        | Operator of Operator
    
module Serialization =
    open Prolog

    let rec jsonToTerm =
        function
        | Chiron.Bool b -> b.ToString () |> Atom // bool not really supported in prolog, make it an atom
        | Chiron.Null _ -> invalidArg "json" "null not supported in prolog" // null not really supported in prolog
        | Chiron.Number n -> n |> Number
        | Chiron.String s -> s |> Atom
        | Chiron.Object o -> o |> Map.map (fun _ v -> (jsonToTerm (v))) |> DictTerm
        | Chiron.Array arr -> arr |> List.map jsonToTerm |> ListTerm

    let rec termToProlog =
        function
        | Atom s -> s
        | Number n -> n.ToString ()
        | Variable name -> name
        | ListTerm terms -> terms |> List.map termToProlog |> String.concat ", " |> sprintf "[%s]"
        | CompoundTerm (functor, terms) -> terms |> Seq.map termToProlog |> String.concat ", " |> sprintf "%s(%s)" functor
        | DictTerm dictionary ->
            dictionary
            |> Seq.map (fun kv -> sprintf "%s:%s" kv.Key (kv.Value |> termToProlog))
            |> String.concat ", "
            |> sprintf "{ %s }"
        | Operator (Infix (t1, o, t2)) -> sprintf "%s %s %s" (t1 |> termToProlog) o (t2 |> termToProlog)
        | Operator (Prefix (o, term)) -> sprintf " %s%s" o (term |> termToProlog)
        | Operator (Postfix (term, o)) -> sprintf "%s%s " (term |> termToProlog) o

module Operators =
    open Prolog
    let ( <&> ) (t1:Term) (t2:Term) = Operator (Infix (t1, ",", t2) )
    let ( <|> ) (t1:Term) (t2:Term) = Operator (Infix (t1, ";", t2) )
    let Fact name (terms:Term seq) = Operator (Postfix (CompoundTerm (name, terms), ".") )
    let Head name (terms:Term seq) = CompoundTerm (name, terms)
    let Rule (t1:Term) (t2:Term) =
        Operator (
            Postfix (
                Operator (
                    Infix (t1, ":-", t2)
                    ), "."
                )
            )

module Pengine =
    open Prolog

    type CreateRequest = {
        SrcText : string
        Format : string
        Ask : string option
        Chunk : int option
    }
    with 
        static member ToJson (cr:CreateRequest) =
            json {
                do! Json.write "src_text" cr.SrcText
                do! Json.write "format" cr.Format
                do! Json.writeUnlessDefault "ask" None cr.Ask
                do! Json.writeUnlessDefault "chunk" None cr.Chunk
            }

    type Answer = {
        Event : string
        Id : string
        More : bool
        Data : Term
        Error : string option
    }
    with
        static member FromJson (_:Answer) =
            json {
                let! event = Json.read "event"
                let! id = Json.read "id"
                let! more = Json.readOrDefault "more" false
                let! error = Json.tryRead "error"
                let! data = Json.readWith (Serialization.jsonToTerm >> Chiron.Functional.JsonResult.Value) "data"
                return {
                    Event = event
                    Id = id
                    More = more
                    Data = data
                    Error = error
                }
            }

    type CreateResponse = {
        Event : string
        Id : string
        Answer : Answer option
    }
        with
            static member FromJson (_:CreateResponse) =
                json {
                    let! event = Json.read "event"
                    let! id = Json.read "id"
                    let! answer = Json.tryRead "answer"
                    return {
                            Event = event
                            Id = id
                            Answer = answer
                        }
                }

    type DestroyResponse = {
        Event : string
        Id : string
    }
        with
            static member FromJson (_:DestroyResponse) =
                json {
                    let! event = Json.read "event"
                    let! id = Json.read "id"
                    return {
                        Event = event
                        Id = id
                    }
                }

    let createPengine (baseUrl:System.Uri) (http:System.Net.Http.HttpClient) (createRequest:CreateRequest) : Async<Result<CreateResponse, string>> =
        let uriBuilder = System.UriBuilder (baseUrl)
        uriBuilder.Path <- "pengine/create"
        async {
            use content = new System.Net.Http.StringContent ((createRequest |> Json.serialize |> Json.format), System.Text.Encoding.UTF8, "application/json")
            use! res = http.PostAsync (uriBuilder.Uri, content) |> Async.AwaitTask
            let! body = res.Content.ReadAsStringAsync () |> Async.AwaitTask
            let createResponse = body |> Json.parse |> Json.deserialize : CreateResponse
            return
                match createResponse.Answer with
                | Some answer when answer.Event = "destroy" ->
                    match answer.Data with
                    | DictTerm dictionary ->
                        match dictionary.TryFind ("event") with
                        // A bad query results in a failure event on create, nested in the "data" element.
                        | Some (Atom "failure") -> Error "Failed to create pengine."
                        // If this is the only result, then the answer is nested inside the "data" element.
                        | Some (Atom "success") when dictionary.ContainsKey "data" -> 
                            { createResponse with Answer = { answer with Data = dictionary.["data"] } |> Some } |> Ok
                        | _ -> createResponse |> Ok
                    | _ -> createResponse |> Ok
                | _ -> createResponse |> Ok
        }

    let nextResult (baseUrl:System.Uri) (http:System.Net.Http.HttpClient) (id:string) : Async<Answer> =
        let uriBuilder = System.UriBuilder (baseUrl)
        uriBuilder.Path <- "pengine/send"
        uriBuilder.Query <- sprintf "id=%s&event=next&format=json" id
        async {
            use! res = http.GetAsync uriBuilder.Uri |> Async.AwaitTask
            let! body = res.Content.ReadAsStringAsync () |> Async.AwaitTask
            let answer = body |> Json.parse |> Json.deserialize : Answer
            return
                match answer.Event with
                | "destroy" -> // When pengines destroys the engine on the last iteration, the "data" is nested in another "data".
                    match answer.Data with
                    | DictTerm dictionary when dictionary.ContainsKey "data" ->
                        { answer with Data = dictionary.["data"] }
                    | _ -> answer
                | _ -> answer
        }

    let destroyPengine (baseUrl:System.Uri) (http:System.Net.Http.HttpClient) (id:string) : Async<DestroyResponse> =
        let uriBuilder = System.UriBuilder (baseUrl)
        uriBuilder.Path <- "pengine/send"
        uriBuilder.Query <- sprintf "id=%s&event=destroy&format=json" id
        async {
            use! res = http.GetAsync uriBuilder.Uri |> Async.AwaitTask
            let! body = res.Content.ReadAsStringAsync () |> Async.AwaitTask
            return body |> Json.parse |> Json.deserialize
        }

    type Source = Term list
    type Ask = Term
    type Solutions = Term

    type SolverConfig = {
        HttpClient : System.Net.Http.HttpClient
        ChunkSize : int
    }

    let defaultSolverConfig = {
        HttpClient = new System.Net.Http.HttpClient ()
        ChunkSize = 100
    }

    let SolverWithConfig solverConfig (baseUri:System.Uri) (source:Source) (ask:Ask) =
        async {
            let! res =
                {
                    SrcText = source |> List.map Serialization.termToProlog |> String.concat System.Environment.NewLine
                    Format = "json"
                    Ask = ask |> Serialization.termToProlog |> Some
                    Chunk = solverConfig.ChunkSize |> Some // get up to 100 solutions at a time.
                }
                |> createPengine baseUri solverConfig.HttpClient
            let rec iterate (pengineId:string) (answer:Answer) (accumulatedAnswers:Term list) = 
                async {
                    if answer.More then
                        let! next = nextResult baseUri solverConfig.HttpClient pengineId
                        return! answer.Data :: accumulatedAnswers |> iterate pengineId next
                    else
                        return answer.Data :: accumulatedAnswers |> List.rev
                }
            match res with
            | Ok createResponse ->
                match createResponse.Answer with
                | Some answer ->
                    let! chunks = iterate createResponse.Id answer []
                    // For any ListTerm in the chunks, pull out the terms and add to accumulated list.
                    // Anything else, just add to the accumulated list.
                    return
                        seq {
                            for chunk in chunks do
                                match chunk with
                                | ListTerm terms -> yield! terms
                                | _ -> yield chunk
                        } |> List.ofSeq |> Ok
                | None -> return [] |> Ok
            | Error msg -> return Error msg
        }

    let Solver (baseUri:System.Uri) = SolverWithConfig defaultSolverConfig baseUri

    /// Matches an `Answer option` with a list of solutions.
    let (|SolutionsList|_|) (answerOpt:Answer option) =
        match answerOpt with
        | Some answer ->
            match answer.Data with
            | ListTerm solutions -> Some solutions
            | _ -> None
        | _ -> None
