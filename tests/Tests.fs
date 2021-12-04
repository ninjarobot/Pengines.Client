module FPenginesTests

open Expecto

open Pengines.Operators
open Pengines.Pengine
open Pengines.Prolog
open Pengines.Serialization

let createPengineReq src ask = { SrcText=src; Ask=ask |> Some; Format="json"; Chunk=None }

let baseUri = System.Uri ("https://pengines.swi-prolog.org")
let http = new System.Net.Http.HttpClient ()

[<Tests>]
let tests = testList "Pengines tests" [
    testAsync "Create Pengine" {
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(X)."
            |> createPengine baseUri http
        match res with
        | Ok _ -> ()
        | Error msg -> failwith msg
    }

    testAsync "Create Pengine and iterate" {
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
    }

    testAsync "Create Pengine and destroy" {
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(X)."
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            let! destroyRes = destroyPengine baseUri http createRes.Id
            Expect.equal destroyRes { Event="destroy"; Id=createRes.Id } "Incorrect destroy response"
        | Error msg -> failwith msg
    }

    testAsync "Create Pengine with Bad Query" {
        let! res = 
            createPengineReq "q(X) :- p(X).\n\n\t\t    p(a). p(b). p(c). p(d). p(e). p(f). p(g)." "q(x)."
            |> createPengine baseUri http
        match res with
        | Ok createRes ->
            failwithf "Should have returned error, but returned %A" createRes
        | Error msg -> ()
    }

    testAsync "Create Pengine with single batch answer" {
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
                    Expect.hasLength solutions 7 "Incorrect number of possible solutions"
                | _ -> failwith "Answer.Data should be a list of solutions"
            | _ -> failwith "CreateResponse should contain an answer"
        | Error msg -> failwith msg
    }

    testAsync "Create Pengine with multiple batch answers" {
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
                    Expect.hasLength solutions 3 "Incorrect number of solutions"
                | _ -> failwith "Answer.Data should be a list of solutions"
            | _ -> failwith "CreateResponse should contain an answer"
        | Error msg -> failwith msg
    }

    test "Atom to prolog" {
        let prolog = Atom "testing123" |> termToProlog
        Expect.equal prolog "testing123" "Incorrect Atom"
    }

    test "Number to prolog" {
        let prolog = Number 123M |> termToProlog
        Expect.equal prolog "123" "Unexpected number term"
    }

    test "Float to prolog" {
        let prolog = Number 123.456M |> termToProlog
        Expect.equal prolog "123.456" "Incorrect float"
    }

    test "Variable to prolog" {
        let prolog = Variable "X" |> termToProlog
        Expect.equal prolog "X" "Incorrect variable"
    }

    test "ListTerm to prolog" {
        let prolog = [Atom "testing123"; Number 123M; Atom "foobar"] |> ListTerm |> termToProlog
        Expect.equal prolog "[testing123, 123, foobar]" "Incorrect list representation"
    }

    test "CompoundTerm /1 to prolog" {
        let prolog = CompoundTerm ("a", [ Atom "testing123" ]) |> termToProlog
        Expect.equal prolog "a(testing123)" "Incorrect compound term /1 representation"
    }

    test "CompoundTerm /2 to prolog" {
        let prolog = CompoundTerm ("a", [ Atom "testing123"; Number 123.456m]) |> termToProlog
        Expect.equal prolog "a(testing123, 123.456)" "Incorrect compound term /2 representation"
    }

    test "DictTerm to prolog" {
        let prolog = [ "a", Atom ("testing123"); "b", Number (123M) ] |> Map.ofList |> DictTerm |> termToProlog
        Expect.equal prolog "{ a:testing123, b:123 }" "Incorrect dict term"
    }

    test "Fact to prolog" {
        let fact = Fact "a" [ Atom "b"; Atom "c" ] |> termToProlog
        Expect.equal fact "a(b, c). " "Incorrect fact"
    }

    test "Basic rule to prolog" {
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
        Expect.equal rule "loves(Person1, Person2) :- names(han, leia). " "Incorrect rule"
    }

    test "Rule to prolog with helpers" {
        let rule =
            Rule
                (Head "loves" [ Variable "Person1"; Variable "Person2" ])
                (CompoundTerm ("names", [ Atom "han"; Atom "leia" ]))
            |> termToProlog
        Expect.equal rule "loves(Person1, Person2) :- names(han, leia). " "Incorrect rule with helpers"
    }

    test "Rule to prolog with helper and operator" {
        let rule =
            Rule
                (Head "loves" [ Variable "Person1"; Variable "Person2" ])
                    (CompoundTerm ("names", [ Atom "han"; Atom "leia" ]) <|> 
                    CompoundTerm ("names", [ Atom "frog"; Atom "toad" ]))
            |> termToProlog
        Expect.equal rule "loves(Person1, Person2) :- names(han, leia) ; names(frog, toad). " "Incorrect rule from generator"
    }

    testAsync "Create Pengine from typed Prolog with single batch answer" {
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
                    Expect.hasLength solutions 4 "Incorrect number of solutions"
                | _ -> failwith "Answer.Data should be a list of solutions"
            | _ -> failwith "CreateResponse should contain an answer"
        | Error msg -> failwith msg
    }

    testAsync "Typed Solve" {
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
            Expect.equal allSolutions "a,b,c,d" "Incorrect solutions"
        | Error msg -> failwith msg
    }

    testAsync "hanoi" {
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
        match! Solver baseUri src ask with
        | Ok results when results.Length = 0 ->
            failwith "No results returned."
        | Ok results ->
            let firstResult = results |> List.head
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
                    Expect.equal hanoiSolution expected "Hanoi steps did not match expected"
                | term -> failwithf "Received wrong result term: %A" term
            | term -> failwithf "First result was not a DictTerm: %A" term
        | Error msg -> failwith msg
    }
]
