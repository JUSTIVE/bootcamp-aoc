open System.IO

module Domain = 
    type Seat= {
        row:int
        column:int
    }

    type ArbitraryBinaryMatcherPattern = {
        upperBound:string
        lowerBound:string
    }
    let applyMapper arbitraryBinaryMatcherPattern target =
        match target with
        | x when x = arbitraryBinaryMatcherPattern.upperBound -> 1
        | __ -> 0

    let parseBinaryString (arbitraryBinaryMatcherPattern:ArbitraryBinaryMatcherPattern) (target:string)=
        target
        |>Seq.rev
        |>Seq.map string
        |>Seq.map((applyMapper arbitraryBinaryMatcherPattern))
        |>Seq.mapi(fun x i->(2.0**(float i)|>int) * x )
        |>Seq.reduce(+)

    let readFileLine filePath :array<string> =
        filePath
        |>File.ReadAllLines

    let processSeatID seat =
        seat.row * 8 + seat.column

    let parseBoardingPass (boardingPass:string) :Seat=
        
        {
            row= ((boardingPass|>Seq.toArray).[0..7]|>string|> parseBinaryString ({upperBound="B"; lowerBound="F"}));
            column= ((boardingPass|>Seq.toArray).[7..10]|>string|>parseBinaryString ({upperBound="R"; lowerBound="L"}))
        }

    let takeFirst (x:array<'a>) = x.[0]
        
    let generateSeatID (fileContent:array<string>) :array<int> =
        fileContent
        |>Array.map parseBoardingPass
        |>Array.map processSeatID
        |>Array.sort

    let findMissingSeat (seatIDs:array<int>) :int=
        let bias = takeFirst seatIDs
        let firstMisaligned = 
            seatIDs
            |>Array.mapi(fun x i -> (x,i))
            |>Array.filter(fun (x,i) -> i+bias<>x)
            |>Array.map(fun (x,i)-> x)
            |>takeFirst
        firstMisaligned - 1

    let solutionCore (fileContent:array<string>) :int=
        fileContent
        |>generateSeatID
        |>findMissingSeat

    let solution filePath :int=
        filePath
        |>readFileLine
        |>solutionCore
        
        