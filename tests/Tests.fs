module FPenginesTests

open Xunit

open Pengines.Operators
open Pengines.Pengine
open Pengines.Prolog
open Pengines.Serialization

let createPengineReq src ask = { SrcText=src; Ask=ask |> Some; Format="json"; Chunk=None }

let baseUri = System.Uri ("http://pengines.swi-prolog.org")

[<Fact>]
let ``Create Pengine`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(X)."
            |> createPengine baseUri http
        match res with
        | Ok _ -> ()
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

[<Fact>]
let ``Create Pengine and iterate`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(X)."
            |> createPengine baseUri http
        let rec iterate pengineId maybeAnswer = 
            async {
                match maybeAnswer with
                | Some answer when answer.More ->
                    let! next = nextResult baseUri http pengineId
                    do! iterate pengineId (next |> Some)
                | _ -> ()
            }
        match res with
        | Ok createResponse ->
            do! iterate createResponse.Id createResponse.Answer
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

[<Fact>]
let ``Create Pengine and destroy`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(X)."
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            let! destroyRes = destroyPengine baseUri http createRes.Id
            Assert.Equal ({ Event="destroy"; Id=createRes.Id }, destroyRes)
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

[<Fact>]
let ``Create Pengine with Bad Query`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(x)."
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            failwithf "Should have returned error, but returned %A" createRes
        | Error msg -> ()
    } |> Async.RunSynchronously

[<Fact>]
let ``Create Pengine with single batch answer`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let! res = 
            {
                SrcText = "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)."
                Ask = "q(X)." |> Some
                Format = "json"
                Chunk = 10 |> Some
            }
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            match createRes.Answer with
            | Some answer ->
                match answer.Data with
                | ListTerm solutions ->
                    Assert.Equal (7, solutions.Length)
                | _ -> failwith "Answer.Data should be a list of solutions"
            | _ -> failwith "CreateResponse should contain an answer"
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

/// Results should come in multiple batches.
[<Fact>]
let ``Create Pengine with multiple batch answers`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let! res = 
            {
                SrcText = "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)."
                Ask = "q(X)." |> Some
                Format = "json"
                Chunk = 3 |> Some
            }
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            match createRes.Answer with
            | Some answer ->
                match answer.Data with
                | ListTerm solutions ->
                    Assert.Equal (3, solutions.Length)
                | _ -> failwith "Answer.Data should be a list of solutions"
            | _ -> failwith "CreateResponse should contain an answer"
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

[<Fact>]
let ``Atom to prolog`` () =
    let prolog = Atom "testing123" |> termToProlog
    Assert.Equal ("testing123", prolog)

[<Fact>]
let ``Number to prolog`` () =
    let prolog = Number 123M |> termToProlog
    Assert.Equal ("123", prolog)

[<Fact>]
let ``Float to prolog`` () =
    let prolog = Number 123.456M |> termToProlog
    Assert.Equal ("123.456", prolog)

[<Fact>]
let ``Variable to prolog`` () =
    let prolog = Variable "X" |> termToProlog
    Assert.Equal ("X", prolog)

[<Fact>]
let ``ListTerm to prolog`` () =
    let prolog = [Atom "testing123"; Number 123M; Atom "foobar"] |> ListTerm |> termToProlog
    Assert.Equal ("[testing123, 123, foobar]", prolog)

[<Fact>]
let ``CompoundTerm /1 to prolog`` () =
    let prolog = CompoundTerm ("a", [ Atom "testing123" ]) |> termToProlog
    Assert.Equal ("a(testing123)", prolog)

[<Fact>]
let ``CompoundTerm /2 to prolog`` () =
    let prolog = CompoundTerm ("a", [ Atom "testing123"; Number 123.456m]) |> termToProlog
    Assert.Equal ("a(testing123, 123.456)", prolog)

[<Fact>]
let ``DictTerm to prolog`` () =
    let prolog = [ "a", Atom ("testing123"); "b", Number (123M) ] |> Map.ofList |> DictTerm |> termToProlog
    Assert.Equal ("{ a:testing123, b:123 }", prolog)

[<Fact>]
let ``Fact to prolog`` () =
    let fact = Fact "a" [ Atom "b"; Atom "c" ] |> termToProlog
    Assert.Equal ("a(b, c). ", fact)

[<Fact>]
let ``Basic rule to prolog`` () =
    let rule =
        Operator (
            Postfix (
                Operator (
                    Infix (
                        CompoundTerm ("loves", [ Variable "Person1"; Variable "Person2" ]),
                        ":-",
                        CompoundTerm ("names", [ Atom "han"; Atom "leia" ])
                    )
                ),
                "."
            )
        ) |> termToProlog
    Assert.Equal ("loves(Person1, Person2) :- names(han, leia). ", rule)

[<Fact>]
let ``Rule to prolog with helpers`` () =
    let rule =
        Rule
            (Head "loves" [ Variable "Person1"; Variable "Person2" ])
            (CompoundTerm ("names", [ Atom "han"; Atom "leia" ]))
        |> termToProlog
    Assert.Equal ("loves(Person1, Person2) :- names(han, leia). ", rule)

[<Fact>]
let ``Rule to prolog with helper and operator`` () =
    let rule =
        Rule
            (Head "loves" [ Variable "Person1"; Variable "Person2" ])
                (CompoundTerm ("names", [ Atom "han"; Atom "leia" ]) <|> 
                 CompoundTerm ("names", [ Atom "frog"; Atom "toad" ]))
        |> termToProlog
    Assert.Equal ("loves(Person1, Person2) :- names(han, leia) ; names(frog, toad). ", rule)

[<Fact>]
let ``Create Pengine from typed Prolog with single batch answer`` () =
    async {
        use http = new System.Net.Http.HttpClient ()
        let src = 
            [
                Fact "p" [Atom "a"]
                Fact "p" [Atom "b"]
                Fact "p" [Atom "c"]
                Fact "p" [Atom "d"]
                Rule
                    (Head "q" [Variable "X"])
                    (CompoundTerm ("p", [Variable "X"]))
            ] |> List.map termToProlog |> String.concat System.Environment.NewLine

        let! res = 
            {
                SrcText = src
                Ask = Fact "q" [Variable "X"] |> termToProlog |> Some
                Format = "json"
                Chunk = 10 |> Some
            }
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            match createRes.Answer with
            | Some answer ->
                match answer.Data with
                | ListTerm solutions ->
                    Assert.Equal (4, solutions.Length)
                | _ -> failwith "Answer.Data should be a list of solutions"
            | _ -> failwith "CreateResponse should contain an answer"
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

[<Fact>]
let ``Typed Solve`` () =
    async {
        let src = 
            [
                Fact "p" [Atom "a"]
                Fact "p" [Atom "b"]
                Fact "p" [Atom "c"]
                Fact "p" [Atom "d"]
                Rule
                    (Head "q" [Variable "X"])
                    (CompoundTerm ("p", [Variable "X"]))
            ]
        let ask = Fact "q" [Variable "X"]
        let! result = Solver baseUri src ask
        match result with
        | Ok solutions ->
            let allSolutions =
                solutions |> List.map (fun term ->
                    match term with
                    | DictTerm dictionary when dictionary.ContainsKey ("X") ->
                        match dictionary.["X"] with
                        | Atom solution -> solution |> Some
                        | _ -> None
                    | _ -> None
                ) |> List.choose id |> String.concat ","
            Assert.Equal ("a,b,c,d", allSolutions)
        | Error msg -> failwith msg
    } |> Async.RunSynchronously

[<Fact>]
let ``hanoi`` () =
    let src =
        [
            Rule
                (Head "move" [ Number 1M; Variable "X"; Variable "Y"; Atom "_"; Variable "Instructions" ])
                (CompoundTerm ("format", [CompoundTerm ("atom", [Variable "Instruction"]); Atom "'move ~w to ~w'"; ListTerm [Variable "X";Variable "Y"]]) <&>
                 Operator (Infix (Variable "Instructions", "=", ListTerm [Variable "Instruction"])))
            Rule
                (Head "move" [ Variable "N"; Variable "X"; Variable "Y"; Variable "Z"; Variable "Instructions" ])
                (Operator (Infix (Variable "N", ">", Number 1M)) <&>
                 Operator (Infix (Variable "M", "is", Operator (Infix (Variable "N", "-", Number 1M)))) <&>
                 CompoundTerm ("move", [Variable "M"; Variable "X"; Variable "Z"; Variable "Y"; Variable "I1"]) <&>
                 CompoundTerm ("move", [Number 1m; Variable "X"; Variable "Y"; Atom "_"; Variable "I2"]) <&>
                 CompoundTerm ("move", [Variable "M"; Variable "Z"; Variable "Y"; Variable "X"; Variable "I3"]) <&>
                 CompoundTerm ("append", [Variable "I1"; Variable "I2"; Variable "Intermediate"]) <&>
                 CompoundTerm ("append", [Variable "Intermediate"; Variable "I3"; Variable "Instructions"]))
        ]
    let ask = Fact "move" [Number 3m; Atom "left"; Atom "right"; Atom "center"; Variable "Instructions"]
    Solver baseUri src ask |> Async.RunSynchronously
    |> function
    | Ok results when results.Length = 0 ->
        failwith "No results returned."
    | Ok results when results.Length > 0 ->
        let (firstResult::_) = results
        match firstResult with
        | DictTerm dictionary when dictionary.ContainsKey "Instructions" ->
            match dictionary.["Instructions"] with
            | ListTerm instructions ->
                let hanoiSolution =
                    instructions 
                    |> List.map (
                        function
                        | Atom instruction -> instruction |> Some
                        | _ -> None)
                    |> List.choose id
                let expected = [
                    "move left to right"
                    "move left to center"
                    "move right to center"
                    "move left to right"
                    "move center to left"
                    "move center to right"
                    "move left to right"
                ]
                Assert.StrictEqual (expected, hanoiSolution)
            | term -> failwithf "Received wrong result term: %A" term
        | term -> failwithf "First result was not a DictTerm: %A" term
    | Error msg -> failwith msg
