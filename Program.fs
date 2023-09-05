open System

let generateMaze (width:int) (height:int) : bool [,] =
    let map = Array2D.create width height true

    let inRange ((x, y): int * int) : bool =
        x >= 0 && x < width && y >= 0 && y < height

    let directions : (int * int) array = [|
        ( 0, -1); // North
        ( 1,  0); // East
        ( 0,  1); // South
        (-1,  0); // West
    |] 

    let setAt (x:int) (y:int) (value:'a) (grid: 'a [,]) : 'a[,] = 
        grid.[x, y] <- value
        grid

    let random = new Random()

    let rec generateMazeHelper ((x, y): int * int) (grid: bool [,]) : (bool [,])  =
        let valid (x:int) (y:int) : bool = inRange (x, y) && grid.[x, y]

        let neighbors = 
            Array.sortBy (fun _ -> random.Next()) directions // Go thru directions in a random order
            |> Array.map 
                (fun (xMod, yMod) -> 
                    (x + 2 * xMod, y + 2 * yMod), // neighbor
                    (x + xMod, y + yMod) // hallway
                )
            |> Array.map 
                (fun ((neighborX, neighborY), (hallwayX, hallwayY)) ->
                    if valid neighborX neighborY then // open path if its valid
                        grid 
                        |> setAt hallwayX hallwayY false // open hallway
                        |> setAt neighborX neighborY false // open neighbor
                        |> generateMazeHelper (neighborX, neighborY) // generate again starting from neighbor
                    else grid // otherwise we're done
                )
        
        Array.last neighbors // the last neighbor is the latest grid that was generated

    generateMazeHelper (2, 2) map

let getMazeCharacter (x:int) (y:int) (grid: bool [,]) : char =
    let height = grid.GetLength(1);
    let width = grid.GetLength(0);

    let inRange ((x, y): int * int) : bool =
        x >= 0 && x < width && y >= 0 && y < height

    let directions : (int * int) array = [|
        ( 0, -1); // North
        ( 1,  0); // East
        ( 0,  1); // South
        (-1,  0); // West
    |] 

    let neigbors = 
        Array.map (fun (xMod, yMod) -> (x + xMod, y + yMod)) directions // get all neighbors
        |> Array.map (fun (neighborX, neighborY) -> inRange (neighborX, neighborY) && grid.[neighborX, neighborY])  // check if neighbors exist and are walls

    let north = System.Convert.ToInt32(neigbors[0]);
    let east  = System.Convert.ToInt32(neigbors[1]);
    let south = System.Convert.ToInt32(neigbors[2]);
    let west  = System.Convert.ToInt32(neigbors[3]);

    let index: int = (north <<< 3) + (east <<< 2) + (south <<< 1) + west // lower 4 bytes are NESW

    let mazeParts = [|
        '╬'; // 0000
        '═'; // 0001
        '║'; // 0010
        '╗'; // 0011
        '═'; // 0100
        '═'; // 0101
        '╔'; // 0110
        '╦'; // 0111
        '║'; // 1000
        '╝'; // 1001
        '║'; // 1010
        '╣'; // 1011
        '╚'; // 1100
        '╩'; // 1101
        '╠'; // 1110
        '╬'; // 1111
    |]

    mazeParts[index]

let printMaze (maze: bool [,]) : string = 
    let rec printMazeHelder (maze: bool [,]) (y:int) : string =
        if y < maze.GetLength(1) then
            (Array.mapi (fun x vaue -> if maze.[x, y] then getMazeCharacter x y maze else ' ') maze.[*, y] // if maze is wall, get proper character otherwise its empty
            |> Array.fold (fun acc c -> acc + string c) "") // merge char array into string
            + "\n" + (printMazeHelder maze (y + 1)) // do the next row
        else ""

    printMazeHelder maze 0
        

let map = generateMaze 101 51

printfn "%s" (printMaze map)