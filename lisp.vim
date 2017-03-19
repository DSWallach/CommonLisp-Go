let SessionLoad = 1
if &cp | set nocp | endif
map [3~ x
let s:cpo_save=&cpo
set cpo&vim
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <M-F8> :call NextColor(0)
nnoremap <S-F8> :call NextColor(-1)
nnoremap <F8> :call NextColor(1)
map! [3~ <Del>
map! [6;3~ <PageDown>
map! [5;3~ <PageUp>
map! [3;3~ <Del>
map! [2;3~ <Insert>
map! [1;3F <End>
map! [1;3H <Home>
map! [1;3B <M-Down>
map! [1;3A <M-Up>
map! [1;3C <M-Right>
map! [1;3D <M-Left>
map! [6;5~ <PageDown>
map! [5;5~ <PageUp>
map! [3;5~ <Del>
map! [2;5~ <Insert>
map! [1;5F <End>
map! [1;5H <Home>
map! [1;5B <C-Down>
map! [1;5A <C-Up>
map! [1;5C <C-Right>
map! [1;5D <C-Left>
map! [6;2~ <PageDown>
map! [5;2~ <PageUp>
map! [3;2~ <Del>
map! [2;2~ <Insert>
map! [1;2F <End>
map! [1;2H <Home>
map! [1;2B <S-Down>
map! [1;2A <S-Up>
map! [1;2C <S-Right>
map! [1;2D <S-Left>
let &cpo=s:cpo_save
unlet s:cpo_save
set background=dark
set backspace=indent,eol,start
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set foldlevelstart=1
set helplang=en
set ignorecase
set iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,&
set mouse=a
set ruler
set runtimepath=~/.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/seabird,~/.vim/bundle/vim-colorschemes,~/.vim/bundle/vim-flake8,~/.vim/bundle/vim-javascript,~/.vim/bundle/vim-jsx,/usr/share/vim/site,/usr/share/vim/current,/usr/share/vim/site/after,~/.vim/bundle/vim-jsx/after,~/.vim/bundle/vim-javascript/after,~/.vim/after,~/.vim/bundle/Vundle.vim/after,~/.vim/bundle/seabird/after,~/.vim/bundle/vim-colorschemes/after
set shiftwidth=4
set showmatch
set smartcase
set tabstop=4
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Documents/GitHub/CommonLisp-Go
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 Go-Game.lisp
badd +0 alpha-beta-go.lisp
argglobal
silent! argdel *
argadd Go-Game.lisp
set stal=2
edit Go-Game.lisp
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=;%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=1
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,&,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
1,48fold
51,78fold
80,113fold
115,123fold
125,146fold
148,168fold
170,221fold
226,238fold
243,283fold
285,297fold
240,298fold
300,303fold
305,311fold
305,311fold
313,319fold
321,327fold
329,335fold
337,344fold
223,347fold
349,356fold
358,367fold
369,391fold
50,391fold
394,405fold
407,418fold
420,428fold
430,439fold
441,447fold
449,476fold
478,516fold
545,570fold
518,580fold
594,625fold
582,645fold
647,664fold
666,692fold
694,704fold
719,727fold
729,745fold
747,782fold
787,798fold
803,813fold
816,827fold
830,842fold
706,845fold
847,863fold
393,863fold
866,878fold
880,880fold
880,944fold
946,1003fold
1005,1071fold
1073,1077fold
1095,1102fold
1104,1111fold
1079,1149fold
865,1149fold
1153,1157fold
1158,1158fold
1159,1168fold
1170,1211fold
1213,1240fold
1242,1281fold
1283,1314fold
1316,1334fold
1336,1387fold
1389,1412fold
1414,1436fold
1438,1460fold
1462,1466fold
1468,1485fold
1487,1502fold
1151,1502fold
1151,1502fold
1
normal! zc
50
normal! zo
223
normal! zo
240
normal! zo
240
normal! zc
305
normal! zo
223
normal! zc
50
normal! zc
393
normal! zo
518
normal! zo
545
normal! zo
518
normal! zc
582
normal! zo
582
normal! zc
706
normal! zo
706
normal! zc
393
normal! zc
865
normal! zo
880
normal! zo
880
normal! zc
1079
normal! zo
1095
normal! zo
1104
normal! zo
1079
normal! zc
865
normal! zc
1151
normal! zo
1151
normal! zo
1151
normal! zc
let s:l = 1 - ((0 * winheight(0) + 15) / 30)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
tabedit alpha-beta-go.lisp
set splitbelow splitright
set nosplitbelow
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=;%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
setlocal nocursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=1
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,&,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal lisp
setlocal lispwords=
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=0
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal undolevels=-123456
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
1,17fold
19,104fold
106,188fold
190,248fold
250,266fold
1
normal! zc
19
normal! zc
106
normal! zc
190
normal! zc
250
normal! zc
let s:l = 199 - ((180 * winheight(0) + 15) / 30)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
199
normal! 0
tabnext 1
set stal=1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
