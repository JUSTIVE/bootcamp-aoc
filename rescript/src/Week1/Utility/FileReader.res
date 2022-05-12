let readAllFile = filePath => filePath->Node.Fs.readFileAsUtf8Sync
let readFileLine = filePath => filePath->readAllFile->Js.String2.split("\n")
