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
badd +1 Go-Game.lisp
badd +1 alpha-beta-go.lisp
argglobal
silent! argdel *
argadd Go-Game.lisp
set stal=2
edit alpha-beta-go.lisp
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
let s:l = 1 - ((0 * winheight(0) + 29) / 59)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
tabedit Go-Game.lisp
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
1,55fold
58,85fold
87,120fold
122,130fold
132,154fold
156,176fold
178,235fold
240,254fold
256,299fold
301,314fold
316,319fold
321,331fold
333,343fold
345,355fold
237,363fold
365,372fold
374,383fold
385,407fold
57,407fold
410,423fold
425,436fold
438,446fold
448,458fold
460,466fold
468,495fold
497,535fold
537,599fold
613,644fold
601,664fold
666,689fold
691,720fold
722,764fold
766,776fold
791,799fold
801,817fold
819,855fold
857,872fold
877,903fold
904,918fold
778,918fold
920,936fold
409,936fold
939,951fold
962,997fold
953,1042fold
1044,1128fold
1130,1187fold
1189,1193fold
1195,1266fold
938,1266fold
1269,1273fold
1275,1284fold
1286,1327fold
1329,1347fold
1349,1376fold
1378,1396fold
1398,1449fold
1451,1474fold
1476,1498fold
1500,1539fold
1541,1566fold
1568,1599fold
1601,1630fold
1632,1666fold
1668,1690fold
1692,1696fold
1698,1719fold
1721,1736fold
1268,1736fold
1
normal! zc
57
normal! zo
237
normal! zo
345
normal! zo
237
normal! zc
57
normal! zc
409
normal! zo
601
normal! zo
601
normal! zc
778
normal! zo
778
normal! zc
409
normal! zc
938
normal! zo
953
normal! zo
953
normal! zc
938
normal! zc
1268
normal! zo
1268
normal! zc
let s:l = 1268 - ((1267 * winheight(0) + 29) / 58)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1268
normal! 01|
tabnext 2
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
