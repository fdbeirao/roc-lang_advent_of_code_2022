app "app"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../example.txt" as example : Str,
        "../puzzle.txt" as puzzle : Str,
    ]
    provides [main] to pf

Model : {
    currentPath : Path,
    root : DiskObject,
    capacity : U32,
}

DiskObject : [
    Dir { path : Path, children : List DiskObject },
    File { path : Path, size : U32 },
]

Path : List Str

ChangeDirectoryTarget : [Root, Parent, Into Path]

ParseResult : [
    ChangeDirectory ChangeDirectoryTarget,
    ListDirectory,
    DirInfo Str,
    FileInfo { size : U32, name : Str },
]

init : Model
init = {
    currentPath: [],
    root: Dir { path: [], children: [] },
    capacity: 70_000_000,
}

apply : Model, ParseResult -> Model
apply = \model, action ->
    when action is
        ChangeDirectory target -> model |> changeDirectory target
        ListDirectory -> model
        DirInfo name -> model |> applyDirOrFileInfo (DirInfo name)
        FileInfo { size, name } -> model |> applyDirOrFileInfo (FileInfo { size, name })

changeDirectory : Model, ChangeDirectoryTarget -> Model
changeDirectory = \model, target ->
    newCurrentPath =
        when target is
            Root -> init.currentPath
            Parent -> model.currentPath |> List.dropLast
            Into path -> model.currentPath |> List.concat path

    { model & currentPath: newCurrentPath }

applyDirOrFileInfo : Model, [DirInfo Str, FileInfo { size : U32, name : Str }] -> Model
applyDirOrFileInfo = \model, objectDataToAdd ->
    diskObjectToAdd : Path -> DiskObject
    diskObjectToAdd = \parentPath ->
        when objectDataToAdd is
            DirInfo name -> Dir { path: parentPath |> List.append name, children: [] }
            FileInfo { size, name } -> File { path: parentPath |> List.append name, size }

    updateDirectory : DiskObject, List Str -> DiskObject
    updateDirectory = \diskObject, pathToFind ->
        pathToFindStr = pathToFind |> getPath

        when diskObject is
            Dir { path, children } ->
                dirPathStr = path |> getPath

                if dirPathStr == pathToFindStr then
                    # we have reached our destination
                    Dir { path, children: children |> List.append (diskObjectToAdd path) }
                else if pathToFindStr |> Str.startsWith dirPathStr then
                    # the path we are looking for is further down the line
                    Dir { path, children: children |> List.map (\child -> child |> updateDirectory pathToFind) }
                else
                    # no-op
                    diskObject

            # no-op
            _ -> diskObject

    updatedDisk = model.root |> updateDirectory model.currentPath

    { model & root: updatedDisk }

tryParseInputLine : Str -> Result ParseResult [UnableToParse Str]
tryParseInputLine = \inputLine ->
    error = Err (UnableToParse inputLine)

    when inputLine |> Str.split " " is
        ["$", "cd", "/"] -> Ok (ChangeDirectory Root)
        ["$", "cd", ".."] -> Ok (ChangeDirectory Parent)
        ["$", "cd", path] -> Ok (ChangeDirectory (Into (path |> Str.split "/")))
        ["$", "ls"] -> Ok ListDirectory
        ["dir", name] -> Ok (DirInfo name)
        [sizeStr, name] ->
            when sizeStr |> Str.toU32 is
                Ok size -> Ok (FileInfo { size, name })
                Err InvalidNumStr -> error

        _ -> error

getName : DiskObject -> Str
getName = \diskObject ->
    getLastPartOfPath = \path ->
        path |> List.last |> Result.withDefault "/"

    when diskObject is
        Dir { path } -> path |> getLastPartOfPath
        File { path } -> path |> getLastPartOfPath

getPath : Path -> Str
getPath = \path ->
    if path |> List.isEmpty then
        "/"
    else
        path |> List.prepend "" |> Str.joinWith "/"

getSize : DiskObject -> U32
getSize = \diskObject ->
    when diskObject is
        Dir { children } ->
            children |> List.map getSize |> List.sum

        File { size } -> size

tryGetDiskObject : DiskObject, Path -> Result DiskObject [PathNotFound]
tryGetDiskObject = \root, pathToFind ->
    pathToFindStr = pathToFind |> getPath
    notFound = Err PathNotFound

    when root is
        File { path } ->
            filePathStr = path |> getPath

            if filePathStr == pathToFindStr then
                Ok root
            else
                notFound

        Dir { path, children } ->
            dirPathStr = path |> getPath

            if dirPathStr == pathToFindStr then
                Ok root
            else if pathToFindStr |> Str.startsWith dirPathStr then
                when children |> List.keepOks (\child -> child |> tryGetDiskObject pathToFind) is
                    [single] -> Ok single
                    _ -> notFound
            else
                notFound

obtainDiskObjectsByCriteria : DiskObject, (DiskObject -> Bool) -> List DiskObject
obtainDiskObjectsByCriteria = \diskObject, criteriaFunc ->
    when diskObject is
        Dir { children } ->
            childrenThatMatchCriteria =
                children
                |> List.joinMap (\child -> child |> obtainDiskObjectsByCriteria criteriaFunc)

            if criteriaFunc diskObject then
                [diskObject] |> List.concat (childrenThatMatchCriteria)
            else
                childrenThatMatchCriteria

        File _ ->
            if criteriaFunc diskObject then
                [diskObject]
            else
                []

isDir : DiskObject -> Bool
isDir = \diskObject ->
    when diskObject is
        Dir _ -> Bool.true
        File _ -> Bool.false

trySolvePuzzle : Str -> Result U32 [UnableToParse Str, ListWasEmpty]
trySolvePuzzle = \rawInput ->
    model = init

    requiredForUpdate = 30_000_000u32

    criteria = \diskObject, requiredSpace ->
        (diskObject |> isDir)
        && diskObject
        |> getSize
        >= requiredSpace

    rawInput
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry tryParseInputLine
    |> Result.map (\actions -> actions |> List.walk model apply)
    |> Result.map
        (\state ->
            freeSpace = state.capacity - (state.root |> getSize)
            requiredSpace = requiredForUpdate - freeSpace
            state.root |> obtainDiskObjectsByCriteria (\diskObject -> diskObject |> criteria requiredSpace))
    |> Result.map (\diskObjects -> diskObjects |> List.map getSize)
    |> Result.map List.sortAsc
    |> Result.try List.first

main : Task {} []
main =
    prettyPrint = \label, rawInput ->
        when (rawInput |> trySolvePuzzle) is
            Ok result ->
                resultStr = result |> Num.toStr
                "\(label): result is \(resultStr)"

            Err (UnableToParse reason) ->
                "\(label): Unable to parse line: [\(reason)]"

            Err ListWasEmpty ->
                "\(label): Unable to find a solution"

    _ <- (prettyPrint "example" example) |> Stdout.line |> Task.await

    (prettyPrint "puzzle" puzzle) |> Stdout.line

# ############# Tests

exampleDisk =
    Dir {
        path: [],
        children: [
            Dir {
                path: ["a"],
                children: [
                    Dir {
                        path: ["a", "e"],
                        children: [
                            File { path: ["a", "e", "i"], size: 584 },
                        ],
                    },
                    File { path: ["a", "f"], size: 29116 },
                    File { path: ["a", "g"], size: 2557 },
                    File { path: ["a", "h.lst"], size: 62596 },
                ],
            },
            File { path: ["b.txt"], size: 14848514 },
            File { path: ["c.dat"], size: 8504156 },
            Dir {
                path: ["d"],
                children: [
                    File { path: ["d", "j"], size: 4060174 },
                    File { path: ["d", "d.log"], size: 8033020 },
                    File { path: ["d", "d.ext"], size: 5626152 },
                    File { path: ["d", "k"], size: 7214296 },
                ],
            },
        ],
    }

expect (Dir { path: [], children: [] } |> getName) == "/"
expect (Dir { path: ["a"], children: [] } |> getName) == "a"
expect (Dir { path: ["a", "b"], children: [] } |> getName) == "b"
expect (File { path: ["a"], size: 0 } |> getName) == "a"
expect (File { path: ["a", "b"], size: 0 } |> getName) == "b"
expect (File { path: ["a", "b.txt"], size: 0 } |> getName) == "b.txt"

expect (getSize exampleDisk) == 48381165

expect (tryParseInputLine "$ cd /") == Ok (ChangeDirectory Root)
expect (tryParseInputLine "$ cd ..") == Ok (ChangeDirectory Parent)
expect (tryParseInputLine "$ cd a") == Ok (ChangeDirectory (Into (["a"])))
expect (tryParseInputLine "$ ls") == Ok (ListDirectory)
expect (tryParseInputLine "dir a") == Ok (DirInfo "a")
expect (tryParseInputLine "14848514 b.txt") == Ok (FileInfo { size: 14848514, name: "b.txt" })

expect
    expected = "/"
    actually = getPath []
    actually == expected

expect
    expected = "/a"
    actually = getPath ["a"]
    actually == expected

expect
    expected = "/a/b"
    actually = getPath ["a", "b"]
    actually == expected

expect
    expected = init
    actually = init |> changeDirectory Root
    actually == expected

expect
    expected = ["a"]
    actually = init |> changeDirectory (Into ["a"]) |> .currentPath
    actually == expected

expect
    expected = { currentPath: [], capacity: 70000000, root: Dir { path: [], children: [Dir { path: ["a"], children: [] }] } }
    actually = init |> applyDirOrFileInfo (DirInfo "a")
    actually == expected

expect
    expected = { currentPath: ["a"], capacity: 70000000, root: Dir { path: [], children: [Dir { path: ["a"], children: [Dir { path: ["a", "b"], children: [] }] }] } }
    actually = init |> applyDirOrFileInfo (DirInfo "a") |> changeDirectory (Into ["a"]) |> applyDirOrFileInfo (DirInfo "b")
    actually == expected

expect
    expected = exampleDisk
    actually =
        init
        |> applyDirOrFileInfo (DirInfo "a")
        |> applyDirOrFileInfo (FileInfo { size: 14848514, name: "b.txt" })
        |> applyDirOrFileInfo (FileInfo { size: 8504156, name: "c.dat" })
        |> applyDirOrFileInfo (DirInfo "d")
        |> changeDirectory (Into ["a"])
        |> applyDirOrFileInfo (DirInfo "e")
        |> applyDirOrFileInfo (FileInfo { size: 29116, name: "f" })
        |> applyDirOrFileInfo (FileInfo { size: 2557, name: "g" })
        |> applyDirOrFileInfo (FileInfo { size: 62596, name: "h.lst" })
        |> changeDirectory (Into ["e"])
        |> applyDirOrFileInfo (FileInfo { size: 584, name: "i" })
        |> changeDirectory Parent
        |> changeDirectory Parent
        |> changeDirectory (Into ["d"])
        |> applyDirOrFileInfo (FileInfo { size: 4060174, name: "j" })
        |> applyDirOrFileInfo (FileInfo { size: 8033020, name: "d.log" })
        |> applyDirOrFileInfo (FileInfo { size: 5626152, name: "d.ext" })
        |> applyDirOrFileInfo (FileInfo { size: 7214296, name: "k" })
        |> .root
    actually == expected

expect
    expected = Ok 584
    actually = exampleDisk |> tryGetDiskObject ["a", "e"] |> Result.map getSize
    actually == expected

expect
    expected = Ok 94853
    actually = exampleDisk |> tryGetDiskObject ["a"] |> Result.map getSize
    actually == expected

expect
    expected = Ok 5626152
    actually = exampleDisk |> tryGetDiskObject ["d", "d.ext"] |> Result.map getSize
    actually == expected

expect
    expected = Err PathNotFound
    actually = exampleDisk |> tryGetDiskObject ["d", "non-existent"]
    actually == expected

expect
    criteria = \diskObject -> diskObject |> getSize <= 100000 && (diskObject |> isDir)

    expected = 95437
    actually =
        exampleDisk
        |> obtainDiskObjectsByCriteria criteria
        |> List.map getSize
        |> List.sum
    actually == expected

expect
    expected = Ok 24933642
    actually = trySolvePuzzle example
    actually == expected

expect
    expected = Ok 1544176
    actually = trySolvePuzzle puzzle
    actually == expected
