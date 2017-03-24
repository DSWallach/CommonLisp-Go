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
let s:l = 1 - ((0 * winheight(0) + 25) / 51)
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
setlocal foldlevel=2
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
58,86fold
88,121fold
123,131fold
133,155fold
157,177fold
179,236fold
241,255fold
257,300fold
302,315fold
317,320fold
322,332fold
334,344fold
346,356fold
238,364fold
366,373fold
375,384fold
386,408fold
410,448fold
57,448fold
451,464fold
466,477fold
479,487fold
489,499fold
501,507fold
509,536fold
538,576fold
578,640fold
654,685fold
642,705fold
707,732fold
734,763fold
765,809fold
811,821fold
823,839fold
450,839fold
865,873fold
875,891fold
893,929fold
935,948fold
952,964fold
965,978fold
842,997fold
1012,1054fold
999,1125fold
1171,1206fold
1212,1217fold
1225,1249fold
1127,1275fold
1332,1346fold
1331,1382fold
1384,1389fold
1277,1390fold
1392,1396fold
1398,1469fold
841,1469fold
1472,1476fold
1478,1487fold
1489,1530fold
1532,1550fold
1552,1579fold
1581,1599fold
1601,1652fold
1654,1677fold
1679,1701fold
1703,1742fold
1744,1769fold
1771,1802fold
1804,1833fold
1835,1869fold
1871,1889fold
1891,1913fold
1915,1930fold
1932,1936fold
1938,1961fold
1963,1978fold
1471,1978fold
1
normal! zc
57
normal! zo
58
normal! zc
88
normal! zc
123
normal! zc
133
normal! zc
157
normal! zc
179
normal! zc
238
normal! zo
238
normal! zc
366
normal! zc
375
normal! zc
386
normal! zc
410
normal! zc
57
normal! zc
450
normal! zo
451
normal! zc
466
normal! zc
479
normal! zc
489
normal! zc
501
normal! zc
509
normal! zc
538
normal! zc
578
normal! zc
642
normal! zo
642
normal! zc
707
normal! zc
734
normal! zc
765
normal! zc
811
normal! zc
823
normal! zc
450
normal! zc
841
normal! zo
842
normal! zo
842
normal! zc
999
normal! zo
999
normal! zc
1127
normal! zo
1171
normal! zo
1212
normal! zo
1127
normal! zc
1277
normal! zo
1331
normal! zo
1332
normal! zo
1384
normal! zo
1277
normal! zc
1392
normal! zc
1398
normal! zc
841
normal! zc
1471
normal! zo
1472
normal! zc
1478
normal! zc
1489
normal! zc
1532
normal! zc
1552
normal! zc
1581
normal! zc
1601
normal! zc
1654
normal! zc
1679
normal! zc
1703
normal! zc
1744
normal! zc
1771
normal! zc
1804
normal! zc
1835
normal! zc
1871
normal! zc
1891
normal! zc
1915
normal! zc
1932
normal! zc
1938
normal! zc
1963
normal! zc
1471
normal! zc
let s:l = 1 - ((0 * winheight(0) + 25) / 51)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
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
