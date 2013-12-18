set iskeyword=-,<,>,+,*,/,%,@,48-57,_,192-255,:,#

syntax region DocBlock matchgroup=DocBound start=+{doc:\|{doc\s+ end=+}+
      \ contains=String,DocSubblock,DocBold,DocItalic,DocOperator
syntax region DocSubblock matchgroup=DocBound start=+{+ end=+}+ contained transparent
      \ contains=DocSubblock,String,DocBold,DocItalic,DocOperator
syntax region DocBold matchgroup=DocBound start="{b\s\+" end="}" contained 
      \ contains=String,DocSubblock,DocBold,DocBoldItalic,DocOperator
syntax region DocItalic matchgroup=DocBound start="{i\s\+" end="}" contained
      \ contains=String,DocSubblock,DocItalic,DocItalicBold,DocOperator
syntax region DocBoldItalic matchgroup=DocBound start="{i\s\+" end="}" contained
      \ contains=String,DocSubblock,DocItalicBold,DocBoldItalic,DocOperator
syntax region DocItalicBold matchgroup=DocBound start="{b\s\+" end="}" contained
      \ contains=String,DocSubblock,DocItalicBold,DocBoldItalic,DocOperator

"syntax region DocOperatorArea matchgroup=DocOperator start="{\(ul\|ol\)\s\+"lc=1 end="}" contained transparent
syntax match DocOperator /{\(ul\|ol\)\s\+/lc=1
syntax match DocOperator /{\s*}/

syntax match MarkupChar /[()]/
syntax match Number /\d\+\(.\d\+\([Ee][+-]\?\d\+\)\?\)\?/
syntax match Identifier /\K\k*/
syntax keyword Keyword var def fun if ret mk-map mk-array true false null when on-exn while
      \ for-array case
syntax region String start=+"+ end=+"+ skip=+\(^\s*\|\\\)"+

syntax match KeywordAttr /{\(private\|public\|vararg\|export\)\s*}/ contains=SpecialChars
syntax match SpecialChars /[{}]/ contained containedin=DocOperatorA

syntax match ImportSpec /{\(use\|from\|export\|after\):\?\s* [^}]*}/

highlight MarkupChar guifg=#999999 ctermfg=5
highlight Identifier guifg=#FFFFFF ctermfg=16
highlight DocBound guifg=#555555 ctermfg=5
highlight DocBlock guifg=#80A0FF ctermfg=2
"highlight link DocSubblock DocBlock
highlight DocBold guifg=#80A0FF ctermfg=5 term=bold cterm=bold gui=bold
highlight DocItalic guifg=#80A0FF ctermfg=5 term=italic cterm=italic gui=italic
highlight DocBoldItalic guifg=#80A0FF ctermfg=5 term=bold,italic cterm=bold,italic gui=bold,italic
highlight link DocItalicBold DocBoldItalic
highlight DocOperator guifg=#FF80FF ctermfg=5
highlight ImportSpec guifg=#FF80FF ctermfg=4

highlight link KeywordAttr Keyword
highlight link SpecialChars MarkupChar
