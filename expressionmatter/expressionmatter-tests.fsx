module Tests = begin
    open Fuchu
    let suite =
        testList "Expression Matter" [
            testCase "Basic tests" <|
                fun _ ->
                    Assert.Equal("expressionMatter 2 1 2", 6, expressionMatter 2 1 2)
                    Assert.Equal("expressionMatter 2 1 1", 4, expressionMatter 2 1 1)
                    Assert.Equal("expressionMatter 1 1 1", 3, expressionMatter 1 1 1)
                    Assert.Equal("expressionMatter 1 2 3", 9, expressionMatter 1 2 3)
                    Assert.Equal("expressionMatter 1 3 1", 5, expressionMatter 1 3 1)
                    Assert.Equal("expressionMatter 2 2 2", 8, expressionMatter 2 2 2)
                    Assert.Equal("expressionMatter 5 1 3", 20, expressionMatter 5 1 3)
                    Assert.Equal("expressionMatter 3 5 7", 105, expressionMatter 3 5 7)
                    Assert.Equal("expressionMatter 5 6 1", 35, expressionMatter 5 6 1)
                    Assert.Equal("expressionMatter 1 6 1", 8, expressionMatter 1 6 1)
                    Assert.Equal("expressionMatter 2 6 1", 14, expressionMatter 2 6 1)
                    Assert.Equal("expressionMatter 6 7 1", 48, expressionMatter 6 7 1)
                    Assert.Equal("expressionMatter 2 10 3", 60, expressionMatter 2 10 3)
                    Assert.Equal("expressionMatter 1 8 3", 27, expressionMatter 1 8 3)
                    Assert.Equal("expressionMatter 9 7 2", 126, expressionMatter 9 7 2)
                    Assert.Equal("expressionMatter 1 1 10", 20, expressionMatter 1 1 10)
                    Assert.Equal("expressionMatter 9 1 1", 18, expressionMatter 9 1 1)
                    Assert.Equal("expressionMatter 10 5 6", 300, expressionMatter 10 5 6)
                    Assert.Equal("expressionMatter 1 10 1", 12, expressionMatter 1 10 1)
            testCase "Random tests" <|
                fun _ ->
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
                        Assert.Equal("expressionMatter " + string(a) + " " + string(b) + " " + string(c), sol a b c, expressionMatter a b c)
        ]
end
