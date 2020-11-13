# lsx

list files and output csv or [ndjson](https://ndjson.org) format

# Usage

## Get executable

lsx can be downloaded from [release page](https://github.com/itn3000/lsx/releases).
or you can download and compile from source via `cargo install --git https://github.com/itn3000/lsx.git`

## command options

you can get help with `lsx --help`

## examples

* basic usage(list files and directories of tmp directory)
    * `lsx tmp`
* include and exclude(list files which has `*.cs` extension, exclude which name is `abc`(both files and directories))
    * `lsx -i "**/*.cs" -e "**/abc" tmp` 
* list only files
    * `lsx --leaf-only tmp`
* output to file `x.csv`
    * `lsx -o x.csv tmp`
* output file as ndjson
    * `lsx --output-format ndjson tmp`