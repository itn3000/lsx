# lsx

list files and output csv or [ndjson](https://ndjson.org) format

# Usage

## Get executable

lsx can be downloaded from [release page](https://github.com/itn3000/lsx/releases).
or you can download and compile from source via `cargo install --git https://github.com/itn3000/lsx.git`

## command options

you can get help with `lsx --help`.

## output fields

|name|description|
|----|-----------|
|path|path to file|
|length|file size(null if file_type is dir or link)|
|file_type|file type("file","dir","link")|
|last_modified|last updated time|
|link_target|target path if the path is symbolic link, or null|
|file_version|Windows file FileVersion by adding `--get-version` option if available|
|product_version|Windows file ProductVersion by adding `--get-version` option if available|

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
* get file version
    * `lsx --get-version`
