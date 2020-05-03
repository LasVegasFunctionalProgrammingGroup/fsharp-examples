let expressionMatter a b c =
    let exps = [
        (a * b * c);
        (a + b + c);
        ((a + b) * c);
        (a * (b + c))]
    
    exps
    |> List.map(fun e -> e)
    |> List.max

let mutable passTotal = 0
let mutable failTotal = 0

let assert_Equal text actual expected =
    let pass = expected = actual
    let passFail = 
        if pass then 
            "... passed!" 
        else
            "... FAILED!"

    if pass then 
        passTotal <- passTotal + 1
    else
        failTotal <- failTotal + 1

    printfn "Testing %s ... %s" text passFail

// expressionMatter 1 2 3 |> printfn "%A"
printfn "Basic tests"
assert_Equal "expressionMatter 2 1 2" 6 (expressionMatter 2 1 2)
assert_Equal "expressionMatter 2 1 1" 4 (expressionMatter 2 1 1)
assert_Equal "expressionMatter 1 1 1" 3 (expressionMatter 1 1 1) 
assert_Equal "expressionMatter 1 2 3" 9 (expressionMatter 1 2 3) 
assert_Equal "expressionMatter 1 3 1" 5 (expressionMatter 1 3 1) 
assert_Equal "expressionMatter 2 2 2" 8 (expressionMatter 2 2 2) 
assert_Equal "expressionMatter 5 1 3" 20 (expressionMatter 5 1 3) 
assert_Equal "expressionMatter 3 5 7" 105 (expressionMatter 3 5 7) 
assert_Equal "expressionMatter 5 6 1" 35 (expressionMatter 5 6 1) 
assert_Equal "expressionMatter 1 6 1" 8 (expressionMatter 1 6 1) 
assert_Equal "expressionMatter 2 6 1" 14 (expressionMatter 2 6 1) 
assert_Equal "expressionMatter 6 7 1" 48 (expressionMatter 6 7 1) 
assert_Equal "expressionMatter 2 10 3" 60 (expressionMatter 2 10 3) 
assert_Equal "expressionMatter 1 8 3" 27 (expressionMatter 1 8 3) 
assert_Equal "expressionMatter 9 7 2" 126 (expressionMatter 9 7 2) 
assert_Equal "expressionMatter 1 1 10" 20 (expressionMatter 1 1 10) 
assert_Equal "expressionMatter 9 1 1" 18 (expressionMatter 9 1 1) 
assert_Equal "expressionMatter 10 5 6" 300 (expressionMatter 10 5 6) 
assert_Equal "expressionMatter 1 10 1" 12 (expressionMatter 1 10 1) 

printfn "Random tests" 

let r = new System.Random()
let sol (a: int) (b: int) (c: int): int =
    Seq.fold max 0 [
        a * (b + c);
        a * b * c;
        a + b * c;
        a + b + c;
        (a + b) * c;
        (a * b) + c
    ]

for _ in 1..100 do
    let a = r.Next(1, 11)
    let b = r.Next(1, 11)
    let c = r.Next(1, 11)
    let text = sprintf "expressionMatter %s %s %s" (string a) (string b) (string c)
    assert_Equal text (sol a b c) (expressionMatter a b c)

printfn "Total Tests: %d\r\nPassed: %d\r\nFailed: %d" (passTotal + failTotal) passTotal failTotal
