" Vim syntax file for G.
" Created by Daniel Bendix <daniel@bendix.io> on July 24, 2024.

" Add 'au BufNewFile,BufRead *.gg,*.glang set filetype=glang' to .vimrc

if exists("b:current_syntax")
  finish
end

syntax keyword glangKeyword fn const var unwrap bind nil case

syntax keyword glangModifier static private public compact unpadded

syntax keyword glangStatement return throw break continue

syntax keyword glangData struct enum protocol union

syntax keyword glangConditional if else guard for while in

syntax keyword glangType void bool i8 i16 i32 i64 isize u8 u16 u32 u64 usize f32 f64

syntax keyword glangOperator not and or

syntax keyword glangFunction main

syntax keyword glangBoolean true false

" Make these based on actual names of intrinsics.
syntax match glangIntrinsic "\#[a-zA-Z_][a-zA-Z0-9_]*"

syntax match glangFloat "\<\d\+\.\d\+\([eE][-+]\=\d\+\)\=\>"
syntax match glangBinary "\<0b[01]\+\>"
syntax match glangOctal "\<0o[0-7]\+\>"
syntax match glangDecimal "\<\d\+\>"
syntax match glangHexadecimal "\<0x[0-9a-fA-F]\+\>"

syntax match glangString "\"\([ -!#-&(-[\]-~]\|\\[\x0-\x7f]\)*\""

syntax match glangComment "//.*$"

syntax region glangBlockComment start="/\*" end="\*/" contains=glangBlockComment

highlight link glangKeyword Keyword
highlight link glangModifier StorageClass
highlight link glangStatement Statement
highlight link glangData Structure
highlight link glangConditional conditional
highlight link glangOperator Operator
highlight link glangType Type
highlight link glangFunction Function

highlight link glangIntrinsic PreProc

highlight link glangBoolean Constant

highlight link glangFloat Number
highlight link glangBinary Number
highlight link glangOctal Number
highlight link glangDecimal Number
highlight link glangHexadecimal Number

highlight link glangString String

highlight link glangComment Comment
highlight link glangBlockComment Comment
