open System 


let rec BubbleSort (numArray : int array) arraySize index (keepUp : bool) = 
    let mutable keepGoing = keepUp
    if index = (arraySize - 1) then 
        if keepGoing then
            BubbleSort numArray arraySize 0 false
        keepGoing <- false
    else if numArray.[index] > numArray.[index + 1] then 
        keepGoing <- true 
        let temp = numArray.[index] 
        numArray.[index] <- numArray.[index + 1] 
        numArray.[index + 1] <- temp 
    if index < (arraySize - 1) then
        BubbleSort numArray arraySize (index + 1) keepGoing 

 ///////////////////////////////////////////////////////////////////////////////////////////////////////
   
let rec ShellThirdRecursion (numArray : int array) jindex size gap =
    if jindex >= gap then
        if numArray.[jindex] < numArray.[jindex - gap] then
            let temp = numArray.[jindex]
            numArray.[jindex] <- numArray.[jindex - gap]
            numArray.[jindex - gap] <- temp;
        ShellThirdRecursion numArray (jindex - gap) size gap

let rec ShellSecondRecursion (numArray : int array) index size gap = 
    if index <> size then
        ShellThirdRecursion numArray index size gap
        ShellSecondRecursion numArray (index + 1) size gap

let rec ShellFirstRecursion (numArray : int array) gap size =
    if gap > 0 then
       ShellSecondRecursion numArray gap size gap
       ShellFirstRecursion numArray (gap/2) size

let ShellSort (numArray : int array) arraySize = 
    ShellFirstRecursion numArray (arraySize/2) arraySize

///////////////////////////////////////////////////////////////////////////////////////////////////////

let InsertionSort (numArray: int array) arraySize =
    for i in 1 .. (arraySize - 1) do
        for j in 0 .. (i - 1) do
            if numArray.[i] < numArray.[j] then
                let temp = numArray.[i]
                numArray.[i] <- numArray.[j]
                numArray.[j] <- temp

[<EntryPoint>] 
let main argv = 
    printfn "Sorting in F#!" 



    printfn "ShellSort" 
    let arr = [|3; 5; 1; 8; 6; 10; 2; 234; 1; 1; 6; 100; 213|]
    for i in 0 .. arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""

    ShellSort arr arr.Length
    for i in 0 .. arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""



    printfn "BubbleSort" 
    let arr = [|3; 5; 1; 8; 6; 10; 2; 234; 1; 1; 6; 100; 213|]
    for i in 0 .. arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""

    BubbleSort arr arr.Length 0 false
    for i in 0 .. arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""



    printfn "InsertionSort" 
    let arr = [|3; 5; 1; 8; 6; 10; 2; 234; 1; 1; 6; 100; 213|]
    for i in 0 .. arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""

    InsertionSort arr arr.Length
    for i in 0 .. arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""

    0