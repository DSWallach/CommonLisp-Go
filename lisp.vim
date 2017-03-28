let SessionLoad = 1
if &cp | set nocp | endif
map OE i
map O5C w
map O5D b
map O5B ^F
map O5A ^B
map O2C w
map O2D b
map O2B ^F
map O2A ^B
map [3;5~ x
map [2;5~ i
map [3;2~ x
map [2;2~ i
map O5F $
map O5H 0
map O2F $
map O2H 0
map OF $
map OH 0
map [E i
map [D h
map [C l
map [B j
map [A k
map [4~ $
map [1~ 0
map [F $
map [H 0
map On .
map Op 0
map Os 3
map Or 2
map Oq 1
map Ov 6
map Ou 5
map Ot 4
map Oy 9
map Ox 8
map Ow 7
map OM 
map Ol ,
map Ok +
map Om -
map Oj *
map Oo :
map [3~ x
let s:cpo_save=&cpo
set cpo&vim
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <M-F8> :call NextColor(0)
nnoremap <S-F8> :call NextColor(-1)
nnoremap <F8> :call NextColor(1)
map! OE <Insert>
map! O5D <S-Left>
map! O5C <S-Right>
map! O5B <PageDown>
map! O5A <PageUp>
map! O2D <S-Left>
map! O2C <S-Right>
map! O2B <PageDown>
map! O2A <PageUp>
map! O5F <End>
map! O5H <Home>
map! O2F <End>
map! O2H <Home>
map! OF <End>
map! OH <Home>
map! [E <Insert>
map! [D <Left>
map! [C <Right>
map! [B <Down>
map! [A <Up>
map! [4~ <End>
map! [1~ <Home>
map! [F <End>
map! [H <Home>
map! On .
map! Op 0
map! Os 3
map! Or 2
map! Oq 1
map! Ov 6
map! Ou 5
map! Ot 4
map! Oy 9
map! Ox 8
map! Ow 7
map! OM 
map! Ol ,
map! Ok +
map! Om -
map! Oj *
map! Oo :
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
set mouse=a
set ruler
set runtimepath=~/.vim,~/.vim/bundle/Vundle.vim,~/.vim/bundle/seabird,~/.vim/bundle/vim-colorschemes,~/.vim/bundle/vim-flake8,~/.vim/bundle/vim-javascript,~/.vim/bundle/vim-jsx,/usr/share/vim/site,/usr/share/vim/current,/usr/share/vim/site/after,~/.vim/bundle/vim-jsx/after,~/.vim/bundle/vim-javascript/after,~/.vim/after,~/.vim/bundle/Vundle.vim/after,~/.vim/bundle/seabird/after,~/.vim/bundle/vim-colorschemes/after
set shiftwidth=4
set showmatch
set tabstop=4
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Documents/GitHub/CommonLisp-Go
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 Go-Game.lisp
argglobal
silent! argdel *
argadd Go-Game.lisp
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
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
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
17,47fold
49,62fold
64,84fold
86,104fold
1,104fold
107,135fold
137,186fold
188,196fold
198,220fold
222,227fold
229,242fold
244,344fold
346,425fold
427,434fold
436,445fold
447,469fold
471,508fold
510,548fold
106,548fold
551,565fold
567,578fold
580,590fold
592,602fold
604,610fold
612,638fold
640,676fold
678,739fold
741,770fold
772,792fold
794,830fold
832,885fold
887,915fold
550,915fold
918,1098fold
1100,1213fold
1215,1336fold
1338,1423fold
1425,1429fold
1431,1569fold
917,1569fold
1572,1588fold
1591,1632fold
1634,1667fold
1669,1689fold
1691,1718fold
1590,1718fold
1721,1739fold
1741,1792fold
1794,1815fold
1817,1840fold
1842,1865fold
1867,1895fold
1897,1928fold
1930,1958fold
1720,1958fold
1961,2000fold
2002,2027fold
2029,2060fold
2062,2091fold
2093,2127fold
2129,2151fold
1960,2151fold
2154,2169fold
2171,2196fold
2198,2221fold
2223,2227fold
2153,2227fold
2229,2257fold
2259,2274fold
2276,2283fold
1571,2283fold
1
normal! zo
1
normal! zc
106
normal! zo
106
normal! zc
550
normal! zo
550
normal! zc
917
normal! zo
917
normal! zc
1571
normal! zo
1590
normal! zo
1590
normal! zc
1720
normal! zo
1720
normal! zc
1960
normal! zo
1960
normal! zc
2153
normal! zo
2153
normal! zc
1571
normal! zc
let s:l = 1214 - ((1213 * winheight(0) + 26) / 52)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1214
normal! 0
tabnext 1
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
