let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <S-Tab> <Plug>snipMateBack
inoremap <silent> <Plug>ragtagXmlV ="&#".getchar().";"
inoremap <silent> <SNR>190_yrrecord =YRRecord3()
inoremap <Plug>TComment_9 :call tcomment#SetOption("count", 9)
inoremap <Plug>TComment_8 :call tcomment#SetOption("count", 8)
inoremap <Plug>TComment_7 :call tcomment#SetOption("count", 7)
inoremap <Plug>TComment_6 :call tcomment#SetOption("count", 6)
inoremap <Plug>TComment_5 :call tcomment#SetOption("count", 5)
inoremap <Plug>TComment_4 :call tcomment#SetOption("count", 4)
inoremap <Plug>TComment_3 :call tcomment#SetOption("count", 3)
inoremap <Plug>TComment_2 :call tcomment#SetOption("count", 2)
inoremap <Plug>TComment_1 :call tcomment#SetOption("count", 1)
inoremap <Plug>TComment_s :TCommentAs =&ft_
inoremap <Plug>TComment_n :TCommentAs =&ft 
inoremap <Plug>TComment_a :TCommentAs 
inoremap <Plug>TComment_b :TCommentBlock mode=#
inoremap <Plug>TComment_i v:TCommentInline mode=#
inoremap <Plug>TComment_r :TCommentRight
inoremap <Plug>TComment_  :TComment 
inoremap <Plug>TComment_p :norm! m`vip:TComment``
inoremap <Plug>TComment_ :TComment
inoremap <silent> <expr> <Plug>delimitMateS-BS delimitMate#WithinEmptyPair() ? "\<Del>" : "\<S-BS>"
inoremap <silent> <Plug>delimitMateBS =delimitMate#BS()
inoremap <silent> <Plug>snipMateShow =snipMate#ShowAvailableSnips()
inoremap <silent> <Plug>snipMateBack =snipMate#BackwardsSnippet()
inoremap <silent> <Plug>snipMateTrigger =snipMate#TriggerSnippet(1)
inoremap <silent> <Plug>snipMateNextOrTrigger =snipMate#TriggerSnippet()
inoremap <C-Space> 
nnoremap <silent>  :TmuxNavigateLeft
xmap 	 <Plug>snipMateVisual
smap 	 <Plug>snipMateNextOrTrigger
nnoremap <silent> <NL> :TmuxNavigateDown
nnoremap <silent>  :TmuxNavigateUp
nnoremap <silent>  :TmuxNavigateRight
nnoremap <silent>  :YRReplace '1', 'p'
nnoremap <silent>  :YRReplace '-1', 'P'
nnoremap <silent>  :RelatedSpecVOpen
nnoremap gf :tabe<cfile>
nnoremap f :sp +e<cfile>
nnoremap <silent>  :cn
nnoremap <silent>  :cp
map [3~ x
nnoremap <silent>  :call OpenNerdTree()
vmap 9 <Plug>TComment_9
nmap 9 <Plug>TComment_9
omap 9 <Plug>TComment_9
vmap 8 <Plug>TComment_8
nmap 8 <Plug>TComment_8
omap 8 <Plug>TComment_8
vmap 7 <Plug>TComment_7
nmap 7 <Plug>TComment_7
omap 7 <Plug>TComment_7
vmap 6 <Plug>TComment_6
nmap 6 <Plug>TComment_6
omap 6 <Plug>TComment_6
vmap 5 <Plug>TComment_5
nmap 5 <Plug>TComment_5
omap 5 <Plug>TComment_5
vmap 4 <Plug>TComment_4
nmap 4 <Plug>TComment_4
omap 4 <Plug>TComment_4
vmap 3 <Plug>TComment_3
nmap 3 <Plug>TComment_3
omap 3 <Plug>TComment_3
vmap 2 <Plug>TComment_2
nmap 2 <Plug>TComment_2
omap 2 <Plug>TComment_2
vmap 1 <Plug>TComment_1
nmap 1 <Plug>TComment_1
omap 1 <Plug>TComment_1
map ca <Plug>TComment_ca
map cc <Plug>TComment_cc
map s <Plug>TComment_s
map n <Plug>TComment_n
map a <Plug>TComment_a
map b <Plug>TComment_b
map i <Plug>TComment_i
map r <Plug>TComment_r
map   <Plug>TComment_ 
map p <Plug>TComment_p
vmap  <Plug>TComment_
nmap  <Plug>TComment_
omap  <Plug>TComment_
nmap   <Plug>SneakForward
nmap # <Plug>(indexed-search-#)
nnoremap ' `
nmap * <Plug>(indexed-search-*)
map ,rwp <Plug>RestoreWinPosn
map ,swp <Plug>SaveWinPosn
xmap ,Nr <Plug>NrrwrgnBangDo
nmap ,nr <Plug>NrrwrgnDo
xmap ,nr <Plug>NrrwrgnDo
xnoremap <silent> ,mc :call multiple_cursors#new("v")
nnoremap <silent> ,mc :call multiple_cursors#new("n")
map ,_s <Plug>TComment_,_s
map ,_n <Plug>TComment_,_n
map ,_a <Plug>TComment_,_a
map ,_b <Plug>TComment_,_b
map ,_r <Plug>TComment_,_r
xmap ,_i <Plug>TComment_,_i
map ,_  <Plug>TComment_,_ 
map ,_p <Plug>TComment_,_p
xmap ,__ <Plug>TComment_,__
nmap ,__ <Plug>TComment_,__
smap ,__ <Plug>TComment_,__
omap ,__ <Plug>TComment_,__
map ,, <Plug>(easymotion-prefix)
nmap ,* :execute 'noautocmd vimgrep /\V' . substitute(escape(expand("<cword>"), '\'), '\n', '\\n', 'g') . '/ **'
nmap <silent> ,ig <Plug>IndentGuidesToggle
map <silent> ,mm :ShowMarksPlaceMark
map <silent> ,ma :ShowMarksClearAll
map <silent> ,mh :ShowMarksClearMark
map <silent> ,mo :ShowMarksOn
map <silent> ,mt :ShowMarksToggle
map <silent> ,hu :call HtmlUnEscape()
map <silent> ,he :call HtmlEscape()
nnoremap <silent> , :RelatedSpecOpen
vnoremap ,rem :RExtractMethod
vnoremap ,rriv :RRenameInstanceVariable
vnoremap ,rrlv :RRenameLocalVariable
vnoremap ,relv :RExtractLocalVariable
vnoremap ,rec :RExtractConstant
nnoremap ,riv :RIntroduceVariable
nnoremap ,rcpc :RConvertPostConditional
nnoremap ,rel :RExtractLet
nnoremap ,rit :RInlineTemp
nnoremap ,rapn :RAddParameterNB
nnoremap ,rap :RAddParameter
nnoremap ,yr :YRShow
nmap ,w :StripTrailingWhitespaces
map <silent> ,hp :!open -a Safari %
map ,hi :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")
noremap ,hl :set hlsearch! hlsearch?
nmap <silent> ,vr :so %
nmap <silent> ,vc yy:p
nnoremap <silent> ,cn :let @* = expand("%:t")
nnoremap <silent> ,cr :let @* = expand("%")
nnoremap <silent> ,cf :let @* = expand("%:~")
map <silent> ,gz o
nnoremap <silent> ,x :bn
nnoremap <silent> ,z :bp
nmap <silent> ,qo :copen
nmap <silent> ,qc :cclose
nnoremap ,. '.
map ,` ysiw`
vmap ,{ c{"}
vmap ,} c{ " }
nmap ,{ ysiw{
omap ,{ ysiw{
nmap ,} ysiw}
omap ,} ysiw}
vmap ,] c["]
vmap ,[ c[ " ]
nmap ,[ ysiw[
omap ,[ ysiw[
nmap ,] ysiw]
omap ,] ysiw]
vmap ,) c(")
vmap ,( c( " )
nmap ,) ysiw)
omap ,) ysiw)
nmap ,( ysiw(
omap ,( ysiw(
vmap ,' c'"'
nmap ,' ysiw'
omap ,' ysiw'
vmap ," c"""
nmap ," ysiw"
omap ," ysiw"
vmap ,# c#{"}
nmap ,# ysiw#
omap ,# ysiw#
nnoremap ,ow "_diwhp
nnoremap ,yw yiww
vmap ,gt :!tidy -q -i --show-errors 0
nnoremap <silent> ,F :let word=expand("<cword>"):vsp:wincmd w:exec("tag ". word)
nnoremap <silent> ,f 
nnoremap ,gcf :exec "Ag " . expand("%:t:r")
nnoremap ,gg :Ag ""<Left>
nnoremap <silent> ,gd :Ag 'def <cword>'
nnoremap ,K viwf!:execute "Ag " . GetVisual()
vnoremap ,k :execute "Ag " . GetVisual()
nnoremap <silent> ,k :Ag <cword>
nnoremap <silent> ,rxit :call ChangePendingRspecToXit()
nnoremap ,cc :Econtroller
nnoremap ,vv :Eview
nnoremap <silent> ,qa/ :execute "Ag! '" . substitute(substitute(substitute(@/, "\\\\<", "\\\\b", ""), "\\\\>", "\\\\b", ""), "\\\\v", "", "") . "'"
nnoremap <silent> ,q/ :execute 'vimgrep /'.@/.'/g %':copen
nnoremap <silent> ,orb :normal varar%
nnoremap ,ocf :OpenChangedFiles
nnoremap ,sl :RunItermSpringSpecLine
nnoremap ,ss :RunItermSpringSpec
nnoremap ,rl :RunItermSpecLine
nnoremap ,rs :RunItermSpec
nmap ,u :GundoToggle
nnoremap <silent> ,gf :vertical botright wincmd F
nnoremap <silent> ,dp :diffput
nnoremap <silent> ,dg :diffget
nmap ,<S-Esc> ,,b
nmap , ,,w
map ,jT :CtrlP test
map ,jF :CtrlP factories
map ,jV :CtrlP vendor
map ,jC :CtrlP config
map ,jd :CtrlP db
map ,jf :CtrlP fast_spec
map ,js :CtrlP spec
map ,jp :CtrlP public
map ,jl :CtrlP lib
map ,jh :CtrlP app/helpers
map ,jj :CtrlP app/assets/javascripts
map ,jv :CtrlP app/views
map ,jc :CtrlP app/controllers
map ,jm :CtrlP app/models
map ,ja :CtrlP app/assets
nnoremap <silent> ,b :CtrlPBuffer
map ,t <Plug>(ctrlp)
nmap ,af :AgFile ""<Left>
nmap ,ag :Ag ""<Left>
nmap / <Plug>(indexed-search-/)
nmap <silent> // :nohlsearch
nnoremap 0 ^
xmap ; <Plug>Sneak_;
nmap ; <Plug>Sneak_;
nmap <D-0> g^
nmap <D-^> g^
nmap <D-$> g$
nmap <D-k> gk
nmap <D-j> gj
vmap <D-0> g^
vmap <D-^> g^
vmap <D-$> g$
vmap <D-k> gk
vmap <D-j> gj
nnoremap <silent> <D-M> :CtrlPBufTag
nnoremap <silent> <D-P> :ClearCtrlPCache
nmap ? <Plug>(indexed-search-?)
xnoremap <silent> <expr> A targets#e('A')
onoremap <silent> AL$ :call targets#o('$LA', v:count1)
onoremap <silent> AN$ :call targets#o('$NA', v:count1)
onoremap <silent> Al$ :call targets#o('$lA', v:count1)
onoremap <silent> An$ :call targets#o('$nA', v:count1)
onoremap <silent> A$ :call targets#o('$cA', v:count1)
onoremap <silent> AL& :call targets#o('&LA', v:count1)
onoremap <silent> AN& :call targets#o('&NA', v:count1)
onoremap <silent> Al& :call targets#o('&lA', v:count1)
onoremap <silent> An& :call targets#o('&nA', v:count1)
onoremap <silent> A& :call targets#o('&cA', v:count1)
onoremap <silent> AL| :call targets#o('|LA', v:count1)
onoremap <silent> AN| :call targets#o('|NA', v:count1)
onoremap <silent> Al| :call targets#o('|lA', v:count1)
onoremap <silent> An| :call targets#o('|nA', v:count1)
onoremap <silent> A| :call targets#o('|cA', v:count1)
onoremap <silent> AL\ :call targets#o('\LA', v:count1)
onoremap <silent> AN\ :call targets#o('\NA', v:count1)
onoremap <silent> Al\ :call targets#o('\lA', v:count1)
onoremap <silent> An\ :call targets#o('\nA', v:count1)
onoremap <silent> A\ :call targets#o('\cA', v:count1)
onoremap <silent> AL/ :call targets#o('/LA', v:count1)
onoremap <silent> AN/ :call targets#o('/NA', v:count1)
onoremap <silent> Al/ :call targets#o('/lA', v:count1)
onoremap <silent> An/ :call targets#o('/nA', v:count1)
onoremap <silent> A/ :call targets#o('/cA', v:count1)
onoremap <silent> AL# :call targets#o('#LA', v:count1)
onoremap <silent> AN# :call targets#o('#NA', v:count1)
onoremap <silent> Al# :call targets#o('#lA', v:count1)
onoremap <silent> An# :call targets#o('#nA', v:count1)
onoremap <silent> A# :call targets#o('#cA', v:count1)
onoremap <silent> AL* :call targets#o('*LA', v:count1)
onoremap <silent> AN* :call targets#o('*NA', v:count1)
onoremap <silent> Al* :call targets#o('*lA', v:count1)
onoremap <silent> An* :call targets#o('*nA', v:count1)
onoremap <silent> A* :call targets#o('*cA', v:count1)
onoremap <silent> AL_ :call targets#o('_LA', v:count1)
onoremap <silent> AN_ :call targets#o('_NA', v:count1)
onoremap <silent> Al_ :call targets#o('_lA', v:count1)
onoremap <silent> An_ :call targets#o('_nA', v:count1)
onoremap <silent> A_ :call targets#o('_cA', v:count1)
onoremap <silent> AL~ :call targets#o('~LA', v:count1)
onoremap <silent> AN~ :call targets#o('~NA', v:count1)
onoremap <silent> Al~ :call targets#o('~lA', v:count1)
onoremap <silent> An~ :call targets#o('~nA', v:count1)
onoremap <silent> A~ :call targets#o('~cA', v:count1)
onoremap <silent> AL= :call targets#o('=LA', v:count1)
onoremap <silent> AN= :call targets#o('=NA', v:count1)
onoremap <silent> Al= :call targets#o('=lA', v:count1)
onoremap <silent> An= :call targets#o('=nA', v:count1)
onoremap <silent> A= :call targets#o('=cA', v:count1)
onoremap <silent> AL- :call targets#o('-LA', v:count1)
onoremap <silent> AN- :call targets#o('-NA', v:count1)
onoremap <silent> Al- :call targets#o('-lA', v:count1)
onoremap <silent> An- :call targets#o('-nA', v:count1)
onoremap <silent> A- :call targets#o('-cA', v:count1)
onoremap <silent> AL+ :call targets#o('+LA', v:count1)
onoremap <silent> AN+ :call targets#o('+NA', v:count1)
onoremap <silent> Al+ :call targets#o('+lA', v:count1)
onoremap <silent> An+ :call targets#o('+nA', v:count1)
onoremap <silent> A+ :call targets#o('+cA', v:count1)
onoremap <silent> AL: :call targets#o(':LA', v:count1)
onoremap <silent> AN: :call targets#o(':NA', v:count1)
onoremap <silent> Al: :call targets#o(':lA', v:count1)
onoremap <silent> An: :call targets#o(':nA', v:count1)
onoremap <silent> A: :call targets#o(':cA', v:count1)
onoremap <silent> AL; :call targets#o(';LA', v:count1)
onoremap <silent> AN; :call targets#o(';NA', v:count1)
onoremap <silent> Al; :call targets#o(';lA', v:count1)
onoremap <silent> An; :call targets#o(';nA', v:count1)
onoremap <silent> A; :call targets#o(';cA', v:count1)
onoremap <silent> AL. :call targets#o('.LA', v:count1)
onoremap <silent> AN. :call targets#o('.NA', v:count1)
onoremap <silent> Al. :call targets#o('.lA', v:count1)
onoremap <silent> An. :call targets#o('.nA', v:count1)
onoremap <silent> A. :call targets#o('.cA', v:count1)
onoremap <silent> AL, :call targets#o(',LA', v:count1)
onoremap <silent> AN, :call targets#o(',NA', v:count1)
onoremap <silent> Al, :call targets#o(',lA', v:count1)
onoremap <silent> An, :call targets#o(',nA', v:count1)
onoremap <silent> A, :call targets#o(',cA', v:count1)
onoremap <silent> Al` :call targets#o('`lA', v:count1)
onoremap <silent> An` :call targets#o('`nA', v:count1)
onoremap <silent> A` :call targets#o('`cA', v:count1)
onoremap <silent> Al' :call targets#o('''lA', v:count1)
onoremap <silent> An' :call targets#o('''nA', v:count1)
onoremap <silent> A' :call targets#o('''cA', v:count1)
onoremap <silent> Al" :call targets#o('"lA', v:count1)
onoremap <silent> An" :call targets#o('"nA', v:count1)
onoremap <silent> A" :call targets#o('"cA', v:count1)
onoremap <silent> Al> :call targets#o('>lA', v:count1)
onoremap <silent> An> :call targets#o('>nA', v:count1)
onoremap <silent> A> :call targets#o('>cA', v:count1)
onoremap <silent> Al< :call targets#o('<lA', v:count1)
onoremap <silent> An< :call targets#o('<nA', v:count1)
onoremap <silent> A< :call targets#o('<cA', v:count1)
onoremap <silent> Al] :call targets#o(']lA', v:count1)
onoremap <silent> An] :call targets#o(']nA', v:count1)
onoremap <silent> A] :call targets#o(']cA', v:count1)
onoremap <silent> Al[ :call targets#o('[lA', v:count1)
onoremap <silent> An[ :call targets#o('[nA', v:count1)
onoremap <silent> A[ :call targets#o('[cA', v:count1)
onoremap <silent> AlB :call targets#o('BlA', v:count1)
onoremap <silent> AnB :call targets#o('BnA', v:count1)
onoremap <silent> AB :call targets#o('BcA', v:count1)
onoremap <silent> Al} :call targets#o('}lA', v:count1)
onoremap <silent> An} :call targets#o('}nA', v:count1)
onoremap <silent> A} :call targets#o('}cA', v:count1)
onoremap <silent> Al{ :call targets#o('{lA', v:count1)
onoremap <silent> An{ :call targets#o('{nA', v:count1)
onoremap <silent> A{ :call targets#o('{cA', v:count1)
onoremap <silent> Alb :call targets#o('blA', v:count1)
onoremap <silent> Anb :call targets#o('bnA', v:count1)
onoremap <silent> Ab :call targets#o('bcA', v:count1)
onoremap <silent> Al) :call targets#o(')lA', v:count1)
onoremap <silent> An) :call targets#o(')nA', v:count1)
onoremap <silent> A) :call targets#o(')cA', v:count1)
onoremap <silent> Al( :call targets#o('(lA', v:count1)
onoremap <silent> An( :call targets#o('(nA', v:count1)
onoremap <silent> A( :call targets#o('(cA', v:count1)
onoremap <silent> Ala :call targets#o('alA', v:count1)
onoremap <silent> Ana :call targets#o('anA', v:count1)
onoremap <silent> Aa :call targets#o('acA', v:count1)
onoremap <silent> Alt :call targets#o('tlA', v:count1)
onoremap <silent> Ant :call targets#o('tnA', v:count1)
onoremap <silent> At :call targets#o('tcA', v:count1)
imap ¯ :TCommenti
nmap B <Plug>CamelCaseMotion_b
xmap B <Plug>CamelCaseMotion_b
nnoremap C-y :YRShow
imap <silent> Ä -
imap <silent> Ë -
imap <silent> ä _
imap <silent> ë _
nmap E <Plug>CamelCaseMotion_e
xmap E <Plug>CamelCaseMotion_e
xnoremap <silent> <expr> I targets#e('I')
onoremap <silent> IL$ :call targets#o('$LI', v:count1)
onoremap <silent> IN$ :call targets#o('$NI', v:count1)
onoremap <silent> Il$ :call targets#o('$lI', v:count1)
onoremap <silent> In$ :call targets#o('$nI', v:count1)
onoremap <silent> I$ :call targets#o('$cI', v:count1)
onoremap <silent> IL& :call targets#o('&LI', v:count1)
onoremap <silent> IN& :call targets#o('&NI', v:count1)
onoremap <silent> Il& :call targets#o('&lI', v:count1)
onoremap <silent> In& :call targets#o('&nI', v:count1)
onoremap <silent> I& :call targets#o('&cI', v:count1)
onoremap <silent> IL| :call targets#o('|LI', v:count1)
onoremap <silent> IN| :call targets#o('|NI', v:count1)
onoremap <silent> Il| :call targets#o('|lI', v:count1)
onoremap <silent> In| :call targets#o('|nI', v:count1)
onoremap <silent> I| :call targets#o('|cI', v:count1)
onoremap <silent> IL\ :call targets#o('\LI', v:count1)
onoremap <silent> IN\ :call targets#o('\NI', v:count1)
onoremap <silent> Il\ :call targets#o('\lI', v:count1)
onoremap <silent> In\ :call targets#o('\nI', v:count1)
onoremap <silent> I\ :call targets#o('\cI', v:count1)
onoremap <silent> IL/ :call targets#o('/LI', v:count1)
onoremap <silent> IN/ :call targets#o('/NI', v:count1)
onoremap <silent> Il/ :call targets#o('/lI', v:count1)
onoremap <silent> In/ :call targets#o('/nI', v:count1)
onoremap <silent> I/ :call targets#o('/cI', v:count1)
onoremap <silent> IL# :call targets#o('#LI', v:count1)
onoremap <silent> IN# :call targets#o('#NI', v:count1)
onoremap <silent> Il# :call targets#o('#lI', v:count1)
onoremap <silent> In# :call targets#o('#nI', v:count1)
onoremap <silent> I# :call targets#o('#cI', v:count1)
onoremap <silent> IL* :call targets#o('*LI', v:count1)
onoremap <silent> IN* :call targets#o('*NI', v:count1)
onoremap <silent> Il* :call targets#o('*lI', v:count1)
onoremap <silent> In* :call targets#o('*nI', v:count1)
onoremap <silent> I* :call targets#o('*cI', v:count1)
onoremap <silent> IL_ :call targets#o('_LI', v:count1)
onoremap <silent> IN_ :call targets#o('_NI', v:count1)
onoremap <silent> Il_ :call targets#o('_lI', v:count1)
onoremap <silent> In_ :call targets#o('_nI', v:count1)
onoremap <silent> I_ :call targets#o('_cI', v:count1)
onoremap <silent> IL~ :call targets#o('~LI', v:count1)
onoremap <silent> IN~ :call targets#o('~NI', v:count1)
onoremap <silent> Il~ :call targets#o('~lI', v:count1)
onoremap <silent> In~ :call targets#o('~nI', v:count1)
onoremap <silent> I~ :call targets#o('~cI', v:count1)
onoremap <silent> IL= :call targets#o('=LI', v:count1)
onoremap <silent> IN= :call targets#o('=NI', v:count1)
onoremap <silent> Il= :call targets#o('=lI', v:count1)
onoremap <silent> In= :call targets#o('=nI', v:count1)
onoremap <silent> I= :call targets#o('=cI', v:count1)
onoremap <silent> IL- :call targets#o('-LI', v:count1)
onoremap <silent> IN- :call targets#o('-NI', v:count1)
onoremap <silent> Il- :call targets#o('-lI', v:count1)
onoremap <silent> In- :call targets#o('-nI', v:count1)
onoremap <silent> I- :call targets#o('-cI', v:count1)
onoremap <silent> IL+ :call targets#o('+LI', v:count1)
onoremap <silent> IN+ :call targets#o('+NI', v:count1)
onoremap <silent> Il+ :call targets#o('+lI', v:count1)
onoremap <silent> In+ :call targets#o('+nI', v:count1)
onoremap <silent> I+ :call targets#o('+cI', v:count1)
onoremap <silent> IL: :call targets#o(':LI', v:count1)
onoremap <silent> IN: :call targets#o(':NI', v:count1)
onoremap <silent> Il: :call targets#o(':lI', v:count1)
onoremap <silent> In: :call targets#o(':nI', v:count1)
onoremap <silent> I: :call targets#o(':cI', v:count1)
onoremap <silent> IL; :call targets#o(';LI', v:count1)
onoremap <silent> IN; :call targets#o(';NI', v:count1)
onoremap <silent> Il; :call targets#o(';lI', v:count1)
onoremap <silent> In; :call targets#o(';nI', v:count1)
onoremap <silent> I; :call targets#o(';cI', v:count1)
onoremap <silent> IL. :call targets#o('.LI', v:count1)
onoremap <silent> IN. :call targets#o('.NI', v:count1)
onoremap <silent> Il. :call targets#o('.lI', v:count1)
onoremap <silent> In. :call targets#o('.nI', v:count1)
onoremap <silent> I. :call targets#o('.cI', v:count1)
onoremap <silent> IL, :call targets#o(',LI', v:count1)
onoremap <silent> IN, :call targets#o(',NI', v:count1)
onoremap <silent> Il, :call targets#o(',lI', v:count1)
onoremap <silent> In, :call targets#o(',nI', v:count1)
onoremap <silent> I, :call targets#o(',cI', v:count1)
onoremap <silent> Il` :call targets#o('`lI', v:count1)
onoremap <silent> In` :call targets#o('`nI', v:count1)
onoremap <silent> I` :call targets#o('`cI', v:count1)
onoremap <silent> Il' :call targets#o('''lI', v:count1)
onoremap <silent> In' :call targets#o('''nI', v:count1)
onoremap <silent> I' :call targets#o('''cI', v:count1)
onoremap <silent> Il" :call targets#o('"lI', v:count1)
onoremap <silent> In" :call targets#o('"nI', v:count1)
onoremap <silent> I" :call targets#o('"cI', v:count1)
onoremap <silent> Il> :call targets#o('>lI', v:count1)
onoremap <silent> In> :call targets#o('>nI', v:count1)
onoremap <silent> I> :call targets#o('>cI', v:count1)
onoremap <silent> Il< :call targets#o('<lI', v:count1)
onoremap <silent> In< :call targets#o('<nI', v:count1)
onoremap <silent> I< :call targets#o('<cI', v:count1)
onoremap <silent> Il] :call targets#o(']lI', v:count1)
onoremap <silent> In] :call targets#o(']nI', v:count1)
onoremap <silent> I] :call targets#o(']cI', v:count1)
onoremap <silent> Il[ :call targets#o('[lI', v:count1)
onoremap <silent> In[ :call targets#o('[nI', v:count1)
onoremap <silent> I[ :call targets#o('[cI', v:count1)
onoremap <silent> IlB :call targets#o('BlI', v:count1)
onoremap <silent> InB :call targets#o('BnI', v:count1)
onoremap <silent> IB :call targets#o('BcI', v:count1)
onoremap <silent> Il} :call targets#o('}lI', v:count1)
onoremap <silent> In} :call targets#o('}nI', v:count1)
onoremap <silent> I} :call targets#o('}cI', v:count1)
onoremap <silent> Il{ :call targets#o('{lI', v:count1)
onoremap <silent> In{ :call targets#o('{nI', v:count1)
onoremap <silent> I{ :call targets#o('{cI', v:count1)
onoremap <silent> Ilb :call targets#o('blI', v:count1)
onoremap <silent> Inb :call targets#o('bnI', v:count1)
onoremap <silent> Ib :call targets#o('bcI', v:count1)
onoremap <silent> Il) :call targets#o(')lI', v:count1)
onoremap <silent> In) :call targets#o(')nI', v:count1)
onoremap <silent> I) :call targets#o(')cI', v:count1)
onoremap <silent> Il( :call targets#o('(lI', v:count1)
onoremap <silent> In( :call targets#o('(nI', v:count1)
onoremap <silent> I( :call targets#o('(cI', v:count1)
onoremap <silent> Ila :call targets#o('alI', v:count1)
onoremap <silent> Ina :call targets#o('anI', v:count1)
onoremap <silent> Ia :call targets#o('acI', v:count1)
onoremap <silent> Ilt :call targets#o('tlI', v:count1)
onoremap <silent> Int :call targets#o('tnI', v:count1)
onoremap <silent> It :call targets#o('tcI', v:count1)
nmap N <Plug>(indexed-search-N)zv
xnoremap <silent> P :YRPaste 'P', 'v'
nnoremap <silent> P :YRPaste 'P'
nnoremap <silent> Q :call CloseWindowOrKillBuffer()
xmap S <Plug>VSurround
nmap S <Plug>Sneak_S
nmap W <Plug>CamelCaseMotion_w
xmap W <Plug>CamelCaseMotion_w
nnoremap Y :YRYankCount 'y$'
xmap Z <Plug>Sneak_S
omap Z <Plug>Sneak_S
nmap [xx <Plug>unimpaired_line_xml_encode
xmap [x <Plug>unimpaired_xml_encode
nmap [x <Plug>unimpaired_xml_encode
nmap [uu <Plug>unimpaired_line_url_encode
xmap [u <Plug>unimpaired_url_encode
nmap [u <Plug>unimpaired_url_encode
nmap [yy <Plug>unimpaired_line_string_encode
xmap [y <Plug>unimpaired_string_encode
nmap [y <Plug>unimpaired_string_encode
nmap [p <Plug>unimpairedPutAbove
nnoremap [ov :set virtualedit+=all
nnoremap [ox :set cursorline cursorcolumn
nnoremap [od :diffthis
nnoremap [ob :set background=light
xmap [e <Plug>unimpairedMoveSelectionUp
nmap [e <Plug>unimpairedMoveUp
nmap [  <Plug>unimpairedBlankUp
omap [n <Plug>unimpairedContextPrevious
nmap [n <Plug>unimpairedContextPrevious
nmap [o <Plug>unimpairedOPrevious
nmap [f <Plug>unimpairedDirectoryPrevious
nmap <silent> [T <Plug>unimpairedTFirst
nmap <silent> [t <Plug>unimpairedTPrevious
nmap <silent> [ <Plug>unimpairedQPFile
nmap <silent> [Q <Plug>unimpairedQFirst
nmap <silent> [q <Plug>unimpairedQPrevious
nmap <silent> [ <Plug>unimpairedLPFile
nmap <silent> [L <Plug>unimpairedLFirst
nmap <silent> [l <Plug>unimpairedLPrevious
nmap <silent> [B <Plug>unimpairedBFirst
nmap <silent> [b <Plug>unimpairedBPrevious
nmap <silent> [A <Plug>unimpairedAFirst
nmap <silent> [a <Plug>unimpairedAPrevious
vmap [% [%m'gv``
nnoremap <silent> \sp ^ispecify { $a }
nnoremap <silent> \bf ^ibefore { $a }
nmap ]xx <Plug>unimpaired_line_xml_decode
xmap ]x <Plug>unimpaired_xml_decode
nmap ]x <Plug>unimpaired_xml_decode
nmap ]uu <Plug>unimpaired_line_url_decode
xmap ]u <Plug>unimpaired_url_decode
nmap ]u <Plug>unimpaired_url_decode
nmap ]yy <Plug>unimpaired_line_string_decode
xmap ]y <Plug>unimpaired_string_decode
nmap ]y <Plug>unimpaired_string_decode
nmap ]p <Plug>unimpairedPutBelow
nnoremap ]ov :set virtualedit-=all
nnoremap ]ox :set nocursorline nocursorcolumn
nnoremap ]od :diffoff
nnoremap ]ob :set background=dark
xmap ]e <Plug>unimpairedMoveSelectionDown
nmap ]e <Plug>unimpairedMoveDown
nmap ]  <Plug>unimpairedBlankDown
omap ]n <Plug>unimpairedContextNext
nmap ]n <Plug>unimpairedContextNext
nmap ]o <Plug>unimpairedONext
nmap ]f <Plug>unimpairedDirectoryNext
nmap <silent> ]T <Plug>unimpairedTLast
nmap <silent> ]t <Plug>unimpairedTNext
nmap <silent> ] <Plug>unimpairedQNFile
nmap <silent> ]Q <Plug>unimpairedQLast
nmap <silent> ]q <Plug>unimpairedQNext
nmap <silent> ] <Plug>unimpairedLNFile
nmap <silent> ]L <Plug>unimpairedLLast
nmap <silent> ]l <Plug>unimpairedLNext
nmap <silent> ]B <Plug>unimpairedBLast
nmap <silent> ]b <Plug>unimpairedBNext
nmap <silent> ]A <Plug>unimpairedALast
nmap <silent> ]a <Plug>unimpairedANext
vmap ]% ]%m'gv``
nnoremap ^ 0
nnoremap ` '
vmap a% [%v]%
xnoremap <silent> <expr> a targets#e('a')
onoremap <silent> aL$ :call targets#o('$La', v:count1)
onoremap <silent> aN$ :call targets#o('$Na', v:count1)
onoremap <silent> al$ :call targets#o('$la', v:count1)
onoremap <silent> an$ :call targets#o('$na', v:count1)
onoremap <silent> a$ :call targets#o('$ca', v:count1)
onoremap <silent> aL& :call targets#o('&La', v:count1)
onoremap <silent> aN& :call targets#o('&Na', v:count1)
onoremap <silent> al& :call targets#o('&la', v:count1)
onoremap <silent> an& :call targets#o('&na', v:count1)
onoremap <silent> a& :call targets#o('&ca', v:count1)
onoremap <silent> aL| :call targets#o('|La', v:count1)
onoremap <silent> aN| :call targets#o('|Na', v:count1)
onoremap <silent> al| :call targets#o('|la', v:count1)
onoremap <silent> an| :call targets#o('|na', v:count1)
onoremap <silent> a| :call targets#o('|ca', v:count1)
onoremap <silent> aL\ :call targets#o('\La', v:count1)
onoremap <silent> aN\ :call targets#o('\Na', v:count1)
onoremap <silent> al\ :call targets#o('\la', v:count1)
onoremap <silent> an\ :call targets#o('\na', v:count1)
onoremap <silent> a\ :call targets#o('\ca', v:count1)
onoremap <silent> aL/ :call targets#o('/La', v:count1)
onoremap <silent> aN/ :call targets#o('/Na', v:count1)
onoremap <silent> al/ :call targets#o('/la', v:count1)
onoremap <silent> an/ :call targets#o('/na', v:count1)
onoremap <silent> a/ :call targets#o('/ca', v:count1)
onoremap <silent> aL# :call targets#o('#La', v:count1)
onoremap <silent> aN# :call targets#o('#Na', v:count1)
onoremap <silent> al# :call targets#o('#la', v:count1)
onoremap <silent> an# :call targets#o('#na', v:count1)
onoremap <silent> a# :call targets#o('#ca', v:count1)
onoremap <silent> aL* :call targets#o('*La', v:count1)
onoremap <silent> aN* :call targets#o('*Na', v:count1)
onoremap <silent> al* :call targets#o('*la', v:count1)
onoremap <silent> an* :call targets#o('*na', v:count1)
onoremap <silent> a* :call targets#o('*ca', v:count1)
onoremap <silent> aL_ :call targets#o('_La', v:count1)
onoremap <silent> aN_ :call targets#o('_Na', v:count1)
onoremap <silent> al_ :call targets#o('_la', v:count1)
onoremap <silent> an_ :call targets#o('_na', v:count1)
onoremap <silent> aL~ :call targets#o('~La', v:count1)
onoremap <silent> aN~ :call targets#o('~Na', v:count1)
onoremap <silent> al~ :call targets#o('~la', v:count1)
onoremap <silent> an~ :call targets#o('~na', v:count1)
onoremap <silent> a~ :call targets#o('~ca', v:count1)
onoremap <silent> aL= :call targets#o('=La', v:count1)
onoremap <silent> aN= :call targets#o('=Na', v:count1)
onoremap <silent> al= :call targets#o('=la', v:count1)
onoremap <silent> an= :call targets#o('=na', v:count1)
onoremap <silent> a= :call targets#o('=ca', v:count1)
onoremap <silent> aL- :call targets#o('-La', v:count1)
onoremap <silent> aN- :call targets#o('-Na', v:count1)
onoremap <silent> al- :call targets#o('-la', v:count1)
onoremap <silent> an- :call targets#o('-na', v:count1)
onoremap <silent> a- :call targets#o('-ca', v:count1)
onoremap <silent> aL+ :call targets#o('+La', v:count1)
onoremap <silent> aN+ :call targets#o('+Na', v:count1)
onoremap <silent> al+ :call targets#o('+la', v:count1)
onoremap <silent> an+ :call targets#o('+na', v:count1)
onoremap <silent> a+ :call targets#o('+ca', v:count1)
onoremap <silent> aL: :call targets#o(':La', v:count1)
onoremap <silent> aN: :call targets#o(':Na', v:count1)
onoremap <silent> al: :call targets#o(':la', v:count1)
onoremap <silent> an: :call targets#o(':na', v:count1)
onoremap <silent> aL; :call targets#o(';La', v:count1)
onoremap <silent> aN; :call targets#o(';Na', v:count1)
onoremap <silent> al; :call targets#o(';la', v:count1)
onoremap <silent> an; :call targets#o(';na', v:count1)
onoremap <silent> a; :call targets#o(';ca', v:count1)
onoremap <silent> aL. :call targets#o('.La', v:count1)
onoremap <silent> aN. :call targets#o('.Na', v:count1)
onoremap <silent> al. :call targets#o('.la', v:count1)
onoremap <silent> an. :call targets#o('.na', v:count1)
onoremap <silent> a. :call targets#o('.ca', v:count1)
onoremap <silent> aL, :call targets#o(',La', v:count1)
onoremap <silent> aN, :call targets#o(',Na', v:count1)
onoremap <silent> al, :call targets#o(',la', v:count1)
onoremap <silent> an, :call targets#o(',na', v:count1)
onoremap <silent> a, :call targets#o(',ca', v:count1)
onoremap <silent> al` :call targets#o('`la', v:count1)
onoremap <silent> an` :call targets#o('`na', v:count1)
onoremap <silent> al' :call targets#o('''la', v:count1)
onoremap <silent> an' :call targets#o('''na', v:count1)
onoremap <silent> al" :call targets#o('"la', v:count1)
onoremap <silent> an" :call targets#o('"na', v:count1)
onoremap <silent> al> :call targets#o('>la', v:count1)
onoremap <silent> an> :call targets#o('>na', v:count1)
onoremap <silent> al< :call targets#o('<la', v:count1)
onoremap <silent> an< :call targets#o('<na', v:count1)
onoremap <silent> al] :call targets#o(']la', v:count1)
onoremap <silent> an] :call targets#o(']na', v:count1)
onoremap <silent> al[ :call targets#o('[la', v:count1)
onoremap <silent> an[ :call targets#o('[na', v:count1)
onoremap <silent> alB :call targets#o('Bla', v:count1)
onoremap <silent> anB :call targets#o('Bna', v:count1)
onoremap <silent> al} :call targets#o('}la', v:count1)
onoremap <silent> an} :call targets#o('}na', v:count1)
onoremap <silent> al{ :call targets#o('{la', v:count1)
onoremap <silent> an{ :call targets#o('{na', v:count1)
onoremap <silent> alb :call targets#o('bla', v:count1)
onoremap <silent> anb :call targets#o('bna', v:count1)
onoremap <silent> al) :call targets#o(')la', v:count1)
onoremap <silent> an) :call targets#o(')na', v:count1)
onoremap <silent> al( :call targets#o('(la', v:count1)
onoremap <silent> an( :call targets#o('(na', v:count1)
onoremap <silent> ala :call targets#o('ala', v:count1)
onoremap <silent> ana :call targets#o('ana', v:count1)
onoremap <silent> aa :call targets#o('aca', v:count1)
onoremap <silent> alt :call targets#o('tla', v:count1)
onoremap <silent> ant :call targets#o('tna', v:count1)
omap ar <Plug>(textobj-rubyblock-a)
xmap ar <Plug>(textobj-rubyblock-a)
omap a_ <Plug>(textobj-underscore-a)
xmap a_ <Plug>(textobj-underscore-a)
omap aF <Plug>(textobj-function-A)
xmap aF <Plug>(textobj-function-A)
omap af <Plug>(textobj-function-a)
xmap af <Plug>(textobj-function-a)
omap ae <Plug>(textobj-entire-a)
xmap ae <Plug>(textobj-entire-a)
omap adz <Plug>(textobj-datetime-tz)
xmap adz <Plug>(textobj-datetime-tz)
omap ada <Plug>(textobj-datetime-auto)
xmap ada <Plug>(textobj-datetime-auto)
omap adt <Plug>(textobj-datetime-time)
xmap adt <Plug>(textobj-datetime-time)
omap add <Plug>(textobj-datetime-date)
xmap add <Plug>(textobj-datetime-date)
omap adf <Plug>(textobj-datetime-full)
xmap adf <Plug>(textobj-datetime-full)
onoremap <silent> aC :call TextObjWordBasedColumn("aW")
onoremap <silent> ac :call TextObjWordBasedColumn("aw")
xnoremap <silent> aC :call TextObjWordBasedColumn("aW")
xnoremap <silent> ac :call TextObjWordBasedColumn("aw")
omap a: <Plug>(textobj-rubysymbol-a)
xmap a: <Plug>(textobj-rubysymbol-a)
vnoremap <silent> ai :call IndentTextObject(0)gv
onoremap <silent> ai :call IndentTextObject(0)
nnoremap cov :set =(&virtualedit =~# "all") ? 'virtualedit-=all' : 'virtualedit+=all'
nnoremap cox :set =&cursorline && &cursorcolumn ? 'nocursorline nocursorcolumn' : 'cursorline cursorcolumn'
nnoremap cod :=&diff ? 'diffoff' : 'diffthis'
nnoremap cob :set background==&background == 'dark' ? 'light' : 'dark'
nmap cS <Plug>CSurround
nmap cs <Plug>Csurround
nmap cr <Plug>Coerce
nmap ds <Plug>Dsurround
xnoremap <silent> d :YRDeleteRange 'v'
nmap gx <Plug>NetrwBrowseX
xmap gS <Plug>VgSurround
nnoremap <silent> gp :YRPaste 'gp'
nnoremap <silent> gP :YRPaste 'gP'
vnoremap gK :call investigate#Investigate('v')
nnoremap gK :call investigate#Investigate('n')
xmap g> <Plug>TComment_Comment
nmap <silent> g>b <Plug>TComment_Commentb
nmap <silent> g>c <Plug>TComment_Commentc
nmap <silent> g> <Plug>TComment_Comment
xmap g< <Plug>TComment_Uncomment
nmap <silent> g<b <Plug>TComment_Uncommentb
nmap <silent> g<c <Plug>TComment_Uncommentc
nmap <silent> g< <Plug>TComment_Uncomment
xmap gc <Plug>TComment_gc
nmap <silent> gcb <Plug>TComment_gcb
nmap <silent> gcc <Plug>TComment_gcc
nmap <silent> gc9c <Plug>TComment_gc9c
nmap <silent> gc9 <Plug>TComment_gc9
nmap <silent> gc8c <Plug>TComment_gc8c
nmap <silent> gc8 <Plug>TComment_gc8
nmap <silent> gc7c <Plug>TComment_gc7c
nmap <silent> gc7 <Plug>TComment_gc7
nmap <silent> gc6c <Plug>TComment_gc6c
nmap <silent> gc6 <Plug>TComment_gc6
nmap <silent> gc5c <Plug>TComment_gc5c
nmap <silent> gc5 <Plug>TComment_gc5
nmap <silent> gc4c <Plug>TComment_gc4c
nmap <silent> gc4 <Plug>TComment_gc4
nmap <silent> gc3c <Plug>TComment_gc3c
nmap <silent> gc3 <Plug>TComment_gc3
nmap <silent> gc2c <Plug>TComment_gc2c
nmap <silent> gc2 <Plug>TComment_gc2
nmap <silent> gc1c <Plug>TComment_gc1c
nmap <silent> gc1 <Plug>TComment_gc1
nmap <silent> gc <Plug>TComment_gc
nmap <silent> gcp p
xmap i,e <Plug>CamelCaseMotion_ie
xmap i,b <Plug>CamelCaseMotion_ib
xmap i,w <Plug>CamelCaseMotion_iw
omap i,e <Plug>CamelCaseMotion_ie
omap i,b <Plug>CamelCaseMotion_ib
omap i,w <Plug>CamelCaseMotion_iw
xnoremap <silent> <expr> i targets#e('i')
onoremap <silent> iL$ :call targets#o('$Li', v:count1)
onoremap <silent> iN$ :call targets#o('$Ni', v:count1)
onoremap <silent> il$ :call targets#o('$li', v:count1)
onoremap <silent> in$ :call targets#o('$ni', v:count1)
onoremap <silent> i$ :call targets#o('$ci', v:count1)
onoremap <silent> iL& :call targets#o('&Li', v:count1)
onoremap <silent> iN& :call targets#o('&Ni', v:count1)
onoremap <silent> il& :call targets#o('&li', v:count1)
onoremap <silent> in& :call targets#o('&ni', v:count1)
onoremap <silent> i& :call targets#o('&ci', v:count1)
onoremap <silent> iL| :call targets#o('|Li', v:count1)
onoremap <silent> iN| :call targets#o('|Ni', v:count1)
onoremap <silent> il| :call targets#o('|li', v:count1)
onoremap <silent> in| :call targets#o('|ni', v:count1)
onoremap <silent> i| :call targets#o('|ci', v:count1)
onoremap <silent> iL\ :call targets#o('\Li', v:count1)
onoremap <silent> iN\ :call targets#o('\Ni', v:count1)
onoremap <silent> il\ :call targets#o('\li', v:count1)
onoremap <silent> in\ :call targets#o('\ni', v:count1)
onoremap <silent> i\ :call targets#o('\ci', v:count1)
onoremap <silent> iL/ :call targets#o('/Li', v:count1)
onoremap <silent> iN/ :call targets#o('/Ni', v:count1)
onoremap <silent> il/ :call targets#o('/li', v:count1)
onoremap <silent> in/ :call targets#o('/ni', v:count1)
onoremap <silent> i/ :call targets#o('/ci', v:count1)
onoremap <silent> iL# :call targets#o('#Li', v:count1)
onoremap <silent> iN# :call targets#o('#Ni', v:count1)
onoremap <silent> il# :call targets#o('#li', v:count1)
onoremap <silent> in# :call targets#o('#ni', v:count1)
onoremap <silent> i# :call targets#o('#ci', v:count1)
onoremap <silent> iL* :call targets#o('*Li', v:count1)
onoremap <silent> iN* :call targets#o('*Ni', v:count1)
onoremap <silent> il* :call targets#o('*li', v:count1)
onoremap <silent> in* :call targets#o('*ni', v:count1)
onoremap <silent> i* :call targets#o('*ci', v:count1)
onoremap <silent> iL_ :call targets#o('_Li', v:count1)
onoremap <silent> iN_ :call targets#o('_Ni', v:count1)
onoremap <silent> il_ :call targets#o('_li', v:count1)
onoremap <silent> in_ :call targets#o('_ni', v:count1)
onoremap <silent> iL~ :call targets#o('~Li', v:count1)
onoremap <silent> iN~ :call targets#o('~Ni', v:count1)
onoremap <silent> il~ :call targets#o('~li', v:count1)
onoremap <silent> in~ :call targets#o('~ni', v:count1)
onoremap <silent> i~ :call targets#o('~ci', v:count1)
onoremap <silent> iL= :call targets#o('=Li', v:count1)
onoremap <silent> iN= :call targets#o('=Ni', v:count1)
onoremap <silent> il= :call targets#o('=li', v:count1)
onoremap <silent> in= :call targets#o('=ni', v:count1)
onoremap <silent> i= :call targets#o('=ci', v:count1)
onoremap <silent> iL- :call targets#o('-Li', v:count1)
onoremap <silent> iN- :call targets#o('-Ni', v:count1)
onoremap <silent> il- :call targets#o('-li', v:count1)
onoremap <silent> in- :call targets#o('-ni', v:count1)
onoremap <silent> i- :call targets#o('-ci', v:count1)
onoremap <silent> iL+ :call targets#o('+Li', v:count1)
onoremap <silent> iN+ :call targets#o('+Ni', v:count1)
onoremap <silent> il+ :call targets#o('+li', v:count1)
onoremap <silent> in+ :call targets#o('+ni', v:count1)
onoremap <silent> i+ :call targets#o('+ci', v:count1)
onoremap <silent> iL: :call targets#o(':Li', v:count1)
onoremap <silent> iN: :call targets#o(':Ni', v:count1)
onoremap <silent> il: :call targets#o(':li', v:count1)
onoremap <silent> in: :call targets#o(':ni', v:count1)
onoremap <silent> iL; :call targets#o(';Li', v:count1)
onoremap <silent> iN; :call targets#o(';Ni', v:count1)
onoremap <silent> il; :call targets#o(';li', v:count1)
onoremap <silent> in; :call targets#o(';ni', v:count1)
onoremap <silent> i; :call targets#o(';ci', v:count1)
onoremap <silent> iL. :call targets#o('.Li', v:count1)
onoremap <silent> iN. :call targets#o('.Ni', v:count1)
onoremap <silent> il. :call targets#o('.li', v:count1)
onoremap <silent> in. :call targets#o('.ni', v:count1)
onoremap <silent> i. :call targets#o('.ci', v:count1)
onoremap <silent> iL, :call targets#o(',Li', v:count1)
onoremap <silent> iN, :call targets#o(',Ni', v:count1)
onoremap <silent> il, :call targets#o(',li', v:count1)
onoremap <silent> in, :call targets#o(',ni', v:count1)
onoremap <silent> i, :call targets#o(',ci', v:count1)
onoremap <silent> il` :call targets#o('`li', v:count1)
onoremap <silent> in` :call targets#o('`ni', v:count1)
onoremap <silent> il' :call targets#o('''li', v:count1)
onoremap <silent> in' :call targets#o('''ni', v:count1)
onoremap <silent> il" :call targets#o('"li', v:count1)
onoremap <silent> in" :call targets#o('"ni', v:count1)
onoremap <silent> il> :call targets#o('>li', v:count1)
onoremap <silent> in> :call targets#o('>ni', v:count1)
onoremap <silent> il< :call targets#o('<li', v:count1)
onoremap <silent> in< :call targets#o('<ni', v:count1)
onoremap <silent> il] :call targets#o(']li', v:count1)
onoremap <silent> in] :call targets#o(']ni', v:count1)
onoremap <silent> il[ :call targets#o('[li', v:count1)
onoremap <silent> in[ :call targets#o('[ni', v:count1)
onoremap <silent> ilB :call targets#o('Bli', v:count1)
onoremap <silent> inB :call targets#o('Bni', v:count1)
onoremap <silent> il} :call targets#o('}li', v:count1)
onoremap <silent> in} :call targets#o('}ni', v:count1)
onoremap <silent> il{ :call targets#o('{li', v:count1)
onoremap <silent> in{ :call targets#o('{ni', v:count1)
onoremap <silent> ilb :call targets#o('bli', v:count1)
onoremap <silent> inb :call targets#o('bni', v:count1)
onoremap <silent> il) :call targets#o(')li', v:count1)
onoremap <silent> in) :call targets#o(')ni', v:count1)
onoremap <silent> il( :call targets#o('(li', v:count1)
onoremap <silent> in( :call targets#o('(ni', v:count1)
onoremap <silent> ila :call targets#o('ali', v:count1)
onoremap <silent> ina :call targets#o('ani', v:count1)
onoremap <silent> ia :call targets#o('aci', v:count1)
onoremap <silent> ilt :call targets#o('tli', v:count1)
onoremap <silent> int :call targets#o('tni', v:count1)
omap ir <Plug>(textobj-rubyblock-i)
xmap ir <Plug>(textobj-rubyblock-i)
omap i_ <Plug>(textobj-underscore-i)
xmap i_ <Plug>(textobj-underscore-i)
omap if <Plug>(textobj-function-i)
xmap if <Plug>(textobj-function-i)
omap iF <Plug>(textobj-function-I)
xmap iF <Plug>(textobj-function-I)
omap ie <Plug>(textobj-entire-i)
xmap ie <Plug>(textobj-entire-i)
omap idz <Plug>(textobj-datetime-tz)
xmap idz <Plug>(textobj-datetime-tz)
omap ida <Plug>(textobj-datetime-auto)
xmap ida <Plug>(textobj-datetime-auto)
omap idt <Plug>(textobj-datetime-time)
xmap idt <Plug>(textobj-datetime-time)
omap idd <Plug>(textobj-datetime-date)
xmap idd <Plug>(textobj-datetime-date)
omap idf <Plug>(textobj-datetime-full)
xmap idf <Plug>(textobj-datetime-full)
onoremap <silent> iC :call TextObjWordBasedColumn("iW")
omap ic <Plug>TComment_ic
xnoremap <silent> iC :call TextObjWordBasedColumn("iW")
vmap ic <Plug>TComment_ic
omap i: <Plug>(textobj-rubysymbol-i)
xmap i: <Plug>(textobj-rubysymbol-i)
vnoremap <silent> ii :call IndentTextObject(1)gv
onoremap <silent> ii :call IndentTextObject(1)
nmap n <Plug>(indexed-search-n)zv
xnoremap <silent> p :YRPaste 'p', 'v'
nnoremap <silent> p :YRPaste 'p'
xmap s <Plug>Sneak_s
nmap sk :SplitjoinJoin
nmap sj :SplitjoinSplit
nnoremap <silent> ss s
nnoremap <silent> vv v
xnoremap <silent> x :YRDeleteRange 'v'
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
xnoremap <silent> y :YRYankRange 'v'
omap z <Plug>Sneak_s
nnoremap zs :RunItermZeusSpec
nnoremap zl :RunItermZeusSpecLine
smap <S-Tab> <Plug>snipMateBack
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nmap <silent> <Plug>RestoreWinPosn :call RestoreWinPosn()
nmap <silent> <Plug>SaveWinPosn :call SaveWinPosn()
nmap <silent> <Plug>unimpairedOPrevious <Plug>unimpairedDirectoryPrevious:echohl WarningMSG|echo "[o is deprecated. Use [f"|echohl NONE
nmap <silent> <Plug>unimpairedONext <Plug>unimpairedDirectoryNext:echohl WarningMSG|echo "]o is deprecated. Use ]f"|echohl NONE
nnoremap <silent> <Plug>unimpairedTLast :exe "".(v:count ? v:count : "")."tlast"
nnoremap <silent> <Plug>unimpairedTFirst :exe "".(v:count ? v:count : "")."tfirst"
nnoremap <silent> <Plug>unimpairedTNext :exe "".(v:count ? v:count : "")."tnext"
nnoremap <silent> <Plug>unimpairedTPrevious :exe "".(v:count ? v:count : "")."tprevious"
nnoremap <silent> <Plug>unimpairedQNFile :exe "".(v:count ? v:count : "")."cnfile"zv
nnoremap <silent> <Plug>unimpairedQPFile :exe "".(v:count ? v:count : "")."cpfile"zv
nnoremap <silent> <Plug>unimpairedQLast :exe "".(v:count ? v:count : "")."clast"zv
nnoremap <silent> <Plug>unimpairedQFirst :exe "".(v:count ? v:count : "")."cfirst"zv
nnoremap <silent> <Plug>unimpairedQNext :exe "".(v:count ? v:count : "")."cnext"zv
nnoremap <silent> <Plug>unimpairedQPrevious :exe "".(v:count ? v:count : "")."cprevious"zv
nnoremap <silent> <Plug>unimpairedLNFile :exe "".(v:count ? v:count : "")."lnfile"zv
nnoremap <silent> <Plug>unimpairedLPFile :exe "".(v:count ? v:count : "")."lpfile"zv
nnoremap <silent> <Plug>unimpairedLLast :exe "".(v:count ? v:count : "")."llast"zv
nnoremap <silent> <Plug>unimpairedLFirst :exe "".(v:count ? v:count : "")."lfirst"zv
nnoremap <silent> <Plug>unimpairedLNext :exe "".(v:count ? v:count : "")."lnext"zv
nnoremap <silent> <Plug>unimpairedLPrevious :exe "".(v:count ? v:count : "")."lprevious"zv
nnoremap <silent> <Plug>unimpairedBLast :exe "".(v:count ? v:count : "")."blast"
nnoremap <silent> <Plug>unimpairedBFirst :exe "".(v:count ? v:count : "")."bfirst"
nnoremap <silent> <Plug>unimpairedBNext :exe "".(v:count ? v:count : "")."bnext"
nnoremap <silent> <Plug>unimpairedBPrevious :exe "".(v:count ? v:count : "")."bprevious"
nnoremap <silent> <Plug>unimpairedALast :exe "".(v:count ? v:count : "")."last"
nnoremap <silent> <Plug>unimpairedAFirst :exe "".(v:count ? v:count : "")."first"
nnoremap <silent> <Plug>unimpairedANext :exe "".(v:count ? v:count : "")."next"
nnoremap <silent> <Plug>unimpairedAPrevious :exe "".(v:count ? v:count : "")."previous"
nnoremap <silent> <Plug>SurroundRepeat .
nnoremap <silent> <SNR>190_yrrecord :call YRRecord3()
xnoremap <SNR>186_VisualNrrwBang :call nrrwrgn#NrrwRgn(visualmode(),'!')
xnoremap <SNR>186_VisualNrrwRgn :call nrrwrgn#NrrwRgn(visualmode(),'')
vnoremap <silent> <Plug>CamelCaseMotion_ie :call camelcasemotion#InnerMotion('e',v:count1)
vnoremap <silent> <Plug>CamelCaseMotion_ib :call camelcasemotion#InnerMotion('b',v:count1)
vnoremap <silent> <Plug>CamelCaseMotion_iw :call camelcasemotion#InnerMotion('w',v:count1)
onoremap <silent> <Plug>CamelCaseMotion_ie :call camelcasemotion#InnerMotion('e',v:count1)
onoremap <silent> <Plug>CamelCaseMotion_ib :call camelcasemotion#InnerMotion('b',v:count1)
onoremap <silent> <Plug>CamelCaseMotion_iw :call camelcasemotion#InnerMotion('w',v:count1)
vnoremap <silent> <Plug>CamelCaseMotion_e :call camelcasemotion#Motion('e',v:count1,'v')
vnoremap <silent> <Plug>CamelCaseMotion_b :call camelcasemotion#Motion('b',v:count1,'v')
vnoremap <silent> <Plug>CamelCaseMotion_w :call camelcasemotion#Motion('w',v:count1,'v')
onoremap <silent> <Plug>CamelCaseMotion_e :call camelcasemotion#Motion('e',v:count1,'o')
onoremap <silent> <Plug>CamelCaseMotion_b :call camelcasemotion#Motion('b',v:count1,'o')
onoremap <silent> <Plug>CamelCaseMotion_w :call camelcasemotion#Motion('w',v:count1,'o')
nnoremap <silent> <Plug>CamelCaseMotion_e :call camelcasemotion#Motion('e',v:count1,'n')
nnoremap <silent> <Plug>CamelCaseMotion_b :call camelcasemotion#Motion('b',v:count1,'n')
nnoremap <silent> <Plug>CamelCaseMotion_w :call camelcasemotion#Motion('w',v:count1,'n')
nnoremap <silent> <Plug>TComment_gc9c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc9cg@
nnoremap <silent> <Plug>TComment_gc8c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc8cg@
nnoremap <silent> <Plug>TComment_gc7c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc7cg@
nnoremap <silent> <Plug>TComment_gc6c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc6cg@
nnoremap <silent> <Plug>TComment_gc5c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc5cg@
nnoremap <silent> <Plug>TComment_gc4c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc4cg@
nnoremap <silent> <Plug>TComment_gc3c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc3cg@
nnoremap <silent> <Plug>TComment_gc2c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc2cg@
nnoremap <silent> <Plug>TComment_gc1c :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gc1cg@
vnoremap <Plug>TComment_9 :call tcomment#SetOption("count", 9)
nnoremap <Plug>TComment_9 :call tcomment#SetOption("count", 9)
onoremap <Plug>TComment_9 :call tcomment#SetOption("count", 9)
vnoremap <Plug>TComment_8 :call tcomment#SetOption("count", 8)
nnoremap <Plug>TComment_8 :call tcomment#SetOption("count", 8)
onoremap <Plug>TComment_8 :call tcomment#SetOption("count", 8)
vnoremap <Plug>TComment_7 :call tcomment#SetOption("count", 7)
nnoremap <Plug>TComment_7 :call tcomment#SetOption("count", 7)
onoremap <Plug>TComment_7 :call tcomment#SetOption("count", 7)
vnoremap <Plug>TComment_6 :call tcomment#SetOption("count", 6)
nnoremap <Plug>TComment_6 :call tcomment#SetOption("count", 6)
onoremap <Plug>TComment_6 :call tcomment#SetOption("count", 6)
vnoremap <Plug>TComment_5 :call tcomment#SetOption("count", 5)
nnoremap <Plug>TComment_5 :call tcomment#SetOption("count", 5)
onoremap <Plug>TComment_5 :call tcomment#SetOption("count", 5)
vnoremap <Plug>TComment_4 :call tcomment#SetOption("count", 4)
nnoremap <Plug>TComment_4 :call tcomment#SetOption("count", 4)
onoremap <Plug>TComment_4 :call tcomment#SetOption("count", 4)
vnoremap <Plug>TComment_3 :call tcomment#SetOption("count", 3)
nnoremap <Plug>TComment_3 :call tcomment#SetOption("count", 3)
onoremap <Plug>TComment_3 :call tcomment#SetOption("count", 3)
vnoremap <Plug>TComment_2 :call tcomment#SetOption("count", 2)
nnoremap <Plug>TComment_2 :call tcomment#SetOption("count", 2)
onoremap <Plug>TComment_2 :call tcomment#SetOption("count", 2)
vnoremap <Plug>TComment_1 :call tcomment#SetOption("count", 1)
nnoremap <Plug>TComment_1 :call tcomment#SetOption("count", 1)
onoremap <Plug>TComment_1 :call tcomment#SetOption("count", 1)
nnoremap <silent> <Plug>TComment_gC :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gCg@
nnoremap <silent> <Plug>TComment_gc :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gcg@
xnoremap <Plug>TComment_gc :TCommentMaybeInline
nnoremap <silent> <Plug>TComment_gcb :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gcbg@
nnoremap <silent> <Plug>TComment_gcc :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_gccg@$
noremap <Plug>TComment_ic :call tcomment#TextObjectInlineComment()
xnoremap <silent> <Plug>TComment_Comment :if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | '<,'>TCommentMaybeInline!
nnoremap <silent> <Plug>TComment_Commentb :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Commentbg@
nnoremap <silent> <Plug>TComment_Commentc :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Commentcg@$
nnoremap <silent> <Plug>TComment_Commentl :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Commentlg@$
nnoremap <silent> <Plug>TComment_Comment :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Commentg@
xnoremap <silent> <Plug>TComment_Uncomment :if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | call tcomment#SetOption("mode_extra", "U") | '<,'>TCommentMaybeInline
nnoremap <silent> <Plug>TComment_Uncommentb :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Uncommentbg@
nnoremap <silent> <Plug>TComment_Uncommentc :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Uncommentcg@$
nnoremap <silent> <Plug>TComment_Uncomment :call tcomment#ResetOption() | if v:count > 0 | call tcomment#SetOption("count", v:count) | endif | let w:tcommentPos = getpos(".") |set opfunc=TCommentOpFunc_Uncommentg@
noremap <Plug>TComment_,_s :TCommentAs =&ft_
noremap <Plug>TComment_,_n :TCommentAs =&ft 
noremap <Plug>TComment_,_a :TCommentAs 
noremap <Plug>TComment_,_b :TCommentBlock
noremap <Plug>TComment_,_r :TCommentRight
xnoremap <Plug>TComment_,_i :TCommentInline
noremap <Plug>TComment_,_  :TComment 
noremap <Plug>TComment_,_p vip:TComment
xnoremap <Plug>TComment_,__ :TCommentMaybeInline
nnoremap <Plug>TComment_,__ :TComment
snoremap <Plug>TComment_,__ :TComment
onoremap <Plug>TComment_,__ :TComment
noremap <Plug>TComment_ca :call tcomment#SetOption("as", input("Comment as: ", &filetype, "customlist,tcomment#Complete"))
noremap <Plug>TComment_cc :call tcomment#SetOption("count", v:count1)
noremap <Plug>TComment_s :TCommentAs =&ft_
noremap <Plug>TComment_n :TCommentAs =&ft 
noremap <Plug>TComment_a :TCommentAs 
noremap <Plug>TComment_b :TCommentBlock
noremap <Plug>TComment_i v:TCommentInline mode=I#
noremap <Plug>TComment_r :TCommentRight
noremap <Plug>TComment_  :TComment 
noremap <Plug>TComment_p m`vip:TComment``
vnoremap <Plug>TComment_ :TCommentMaybeInline
nnoremap <Plug>TComment_ :TComment
onoremap <Plug>TComment_ :TComment
nnoremap <silent> <Plug>(ctrlp) :CtrlP
map <silent> <Plug>(easymotion-prefix)N <Plug>(easymotion-N)
map <silent> <Plug>(easymotion-prefix)n <Plug>(easymotion-n)
map <silent> <Plug>(easymotion-prefix)k <Plug>(easymotion-k)
map <silent> <Plug>(easymotion-prefix)j <Plug>(easymotion-j)
map <silent> <Plug>(easymotion-prefix)gE <Plug>(easymotion-gE)
map <silent> <Plug>(easymotion-prefix)ge <Plug>(easymotion-ge)
map <silent> <Plug>(easymotion-prefix)E <Plug>(easymotion-E)
map <silent> <Plug>(easymotion-prefix)e <Plug>(easymotion-e)
map <silent> <Plug>(easymotion-prefix)B <Plug>(easymotion-B)
map <silent> <Plug>(easymotion-prefix)b <Plug>(easymotion-b)
map <silent> <Plug>(easymotion-prefix)W <Plug>(easymotion-W)
map <silent> <Plug>(easymotion-prefix)w <Plug>(easymotion-w)
map <silent> <Plug>(easymotion-prefix)T <Plug>(easymotion-T)
map <silent> <Plug>(easymotion-prefix)t <Plug>(easymotion-t)
map <silent> <Plug>(easymotion-prefix)s <Plug>(easymotion-s)
map <silent> <Plug>(easymotion-prefix)F <Plug>(easymotion-F)
map <silent> <Plug>(easymotion-prefix)f <Plug>(easymotion-f)
xnoremap <silent> <Plug>(easymotion-activate) :call EasyMotion#activate(1)
nnoremap <silent> <Plug>(easymotion-activate) :call EasyMotion#activate(0)
snoremap <silent> <Plug>(easymotion-activate) :call EasyMotion#activate(0)
onoremap <silent> <Plug>(easymotion-activate) :call EasyMotion#activate(0)
noremap <silent> <Plug>(easymotion-dotrepeat) :call EasyMotion#DotRepeat()
xnoremap <silent> <Plug>(easymotion-repeat) :call EasyMotion#Repeat(1)
nnoremap <silent> <Plug>(easymotion-repeat) :call EasyMotion#Repeat(0)
snoremap <silent> <Plug>(easymotion-repeat) :call EasyMotion#Repeat(0)
onoremap <silent> <Plug>(easymotion-repeat) :call EasyMotion#Repeat(0)
xnoremap <silent> <Plug>(easymotion-prev) :call EasyMotion#NextPrevious(1,1)
nnoremap <silent> <Plug>(easymotion-prev) :call EasyMotion#NextPrevious(0,1)
snoremap <silent> <Plug>(easymotion-prev) :call EasyMotion#NextPrevious(0,1)
onoremap <silent> <Plug>(easymotion-prev) :call EasyMotion#NextPrevious(0,1)
xnoremap <silent> <Plug>(easymotion-next) :call EasyMotion#NextPrevious(1,0)
nnoremap <silent> <Plug>(easymotion-next) :call EasyMotion#NextPrevious(0,0)
snoremap <silent> <Plug>(easymotion-next) :call EasyMotion#NextPrevious(0,0)
onoremap <silent> <Plug>(easymotion-next) :call EasyMotion#NextPrevious(0,0)
xnoremap <silent> <Plug>(easymotion-wl) :call EasyMotion#WBL(1,0)
nnoremap <silent> <Plug>(easymotion-wl) :call EasyMotion#WBL(0,0)
snoremap <silent> <Plug>(easymotion-wl) :call EasyMotion#WBL(0,0)
onoremap <silent> <Plug>(easymotion-wl) :call EasyMotion#WBL(0,0)
xnoremap <silent> <Plug>(easymotion-lineforward) :call EasyMotion#LineAnywhere(1,0)
nnoremap <silent> <Plug>(easymotion-lineforward) :call EasyMotion#LineAnywhere(0,0)
snoremap <silent> <Plug>(easymotion-lineforward) :call EasyMotion#LineAnywhere(0,0)
onoremap <silent> <Plug>(easymotion-lineforward) :call EasyMotion#LineAnywhere(0,0)
xnoremap <silent> <Plug>(easymotion-lineanywhere) :call EasyMotion#LineAnywhere(1,2)
nnoremap <silent> <Plug>(easymotion-lineanywhere) :call EasyMotion#LineAnywhere(0,2)
snoremap <silent> <Plug>(easymotion-lineanywhere) :call EasyMotion#LineAnywhere(0,2)
onoremap <silent> <Plug>(easymotion-lineanywhere) :call EasyMotion#LineAnywhere(0,2)
xnoremap <silent> <Plug>(easymotion-bd-wl) :call EasyMotion#WBL(1,2)
nnoremap <silent> <Plug>(easymotion-bd-wl) :call EasyMotion#WBL(0,2)
snoremap <silent> <Plug>(easymotion-bd-wl) :call EasyMotion#WBL(0,2)
onoremap <silent> <Plug>(easymotion-bd-wl) :call EasyMotion#WBL(0,2)
xnoremap <silent> <Plug>(easymotion-linebackward) :call EasyMotion#LineAnywhere(1,1)
nnoremap <silent> <Plug>(easymotion-linebackward) :call EasyMotion#LineAnywhere(0,1)
snoremap <silent> <Plug>(easymotion-linebackward) :call EasyMotion#LineAnywhere(0,1)
onoremap <silent> <Plug>(easymotion-linebackward) :call EasyMotion#LineAnywhere(0,1)
xnoremap <silent> <Plug>(easymotion-bl) :call EasyMotion#WBL(1,1)
nnoremap <silent> <Plug>(easymotion-bl) :call EasyMotion#WBL(0,1)
snoremap <silent> <Plug>(easymotion-bl) :call EasyMotion#WBL(0,1)
onoremap <silent> <Plug>(easymotion-bl) :call EasyMotion#WBL(0,1)
xnoremap <silent> <Plug>(easymotion-el) :call EasyMotion#EL(1,0)
nnoremap <silent> <Plug>(easymotion-el) :call EasyMotion#EL(0,0)
snoremap <silent> <Plug>(easymotion-el) :call EasyMotion#EL(0,0)
onoremap <silent> <Plug>(easymotion-el) :call EasyMotion#EL(0,0)
xnoremap <silent> <Plug>(easymotion-gel) :call EasyMotion#EL(1,1)
nnoremap <silent> <Plug>(easymotion-gel) :call EasyMotion#EL(0,1)
snoremap <silent> <Plug>(easymotion-gel) :call EasyMotion#EL(0,1)
onoremap <silent> <Plug>(easymotion-gel) :call EasyMotion#EL(0,1)
xnoremap <silent> <Plug>(easymotion-bd-el) :call EasyMotion#EL(1,2)
nnoremap <silent> <Plug>(easymotion-bd-el) :call EasyMotion#EL(0,2)
snoremap <silent> <Plug>(easymotion-bd-el) :call EasyMotion#EL(0,2)
onoremap <silent> <Plug>(easymotion-bd-el) :call EasyMotion#EL(0,2)
xnoremap <silent> <Plug>(easymotion-jumptoanywhere) :call EasyMotion#JumpToAnywhere(1,2)
nnoremap <silent> <Plug>(easymotion-jumptoanywhere) :call EasyMotion#JumpToAnywhere(0,2)
snoremap <silent> <Plug>(easymotion-jumptoanywhere) :call EasyMotion#JumpToAnywhere(0,2)
onoremap <silent> <Plug>(easymotion-jumptoanywhere) :call EasyMotion#JumpToAnywhere(0,2)
xnoremap <silent> <Plug>(easymotion-vim-n) :call EasyMotion#Search(1,0,1)
nnoremap <silent> <Plug>(easymotion-vim-n) :call EasyMotion#Search(0,0,1)
snoremap <silent> <Plug>(easymotion-vim-n) :call EasyMotion#Search(0,0,1)
onoremap <silent> <Plug>(easymotion-vim-n) :call EasyMotion#Search(0,0,1)
xnoremap <silent> <Plug>(easymotion-n) :call EasyMotion#Search(1,0,0)
nnoremap <silent> <Plug>(easymotion-n) :call EasyMotion#Search(0,0,0)
snoremap <silent> <Plug>(easymotion-n) :call EasyMotion#Search(0,0,0)
onoremap <silent> <Plug>(easymotion-n) :call EasyMotion#Search(0,0,0)
xnoremap <silent> <Plug>(easymotion-bd-n) :call EasyMotion#Search(1,2,0)
nnoremap <silent> <Plug>(easymotion-bd-n) :call EasyMotion#Search(0,2,0)
snoremap <silent> <Plug>(easymotion-bd-n) :call EasyMotion#Search(0,2,0)
onoremap <silent> <Plug>(easymotion-bd-n) :call EasyMotion#Search(0,2,0)
xnoremap <silent> <Plug>(easymotion-vim-N) :call EasyMotion#Search(1,1,1)
nnoremap <silent> <Plug>(easymotion-vim-N) :call EasyMotion#Search(0,1,1)
snoremap <silent> <Plug>(easymotion-vim-N) :call EasyMotion#Search(0,1,1)
onoremap <silent> <Plug>(easymotion-vim-N) :call EasyMotion#Search(0,1,1)
xnoremap <silent> <Plug>(easymotion-N) :call EasyMotion#Search(1,1,0)
nnoremap <silent> <Plug>(easymotion-N) :call EasyMotion#Search(0,1,0)
snoremap <silent> <Plug>(easymotion-N) :call EasyMotion#Search(0,1,0)
onoremap <silent> <Plug>(easymotion-N) :call EasyMotion#Search(0,1,0)
xnoremap <silent> <Plug>(easymotion-eol-j) :call EasyMotion#Eol(1,0)
nnoremap <silent> <Plug>(easymotion-eol-j) :call EasyMotion#Eol(0,0)
snoremap <silent> <Plug>(easymotion-eol-j) :call EasyMotion#Eol(0,0)
onoremap <silent> <Plug>(easymotion-eol-j) :call EasyMotion#Eol(0,0)
xnoremap <silent> <Plug>(easymotion-sol-k) :call EasyMotion#Sol(1,1)
nnoremap <silent> <Plug>(easymotion-sol-k) :call EasyMotion#Sol(0,1)
snoremap <silent> <Plug>(easymotion-sol-k) :call EasyMotion#Sol(0,1)
onoremap <silent> <Plug>(easymotion-sol-k) :call EasyMotion#Sol(0,1)
xnoremap <silent> <Plug>(easymotion-sol-j) :call EasyMotion#Sol(1,0)
nnoremap <silent> <Plug>(easymotion-sol-j) :call EasyMotion#Sol(0,0)
snoremap <silent> <Plug>(easymotion-sol-j) :call EasyMotion#Sol(0,0)
onoremap <silent> <Plug>(easymotion-sol-j) :call EasyMotion#Sol(0,0)
xnoremap <silent> <Plug>(easymotion-k) :call EasyMotion#JK(1,1)
nnoremap <silent> <Plug>(easymotion-k) :call EasyMotion#JK(0,1)
snoremap <silent> <Plug>(easymotion-k) :call EasyMotion#JK(0,1)
onoremap <silent> <Plug>(easymotion-k) :call EasyMotion#JK(0,1)
xnoremap <silent> <Plug>(easymotion-j) :call EasyMotion#JK(1,0)
nnoremap <silent> <Plug>(easymotion-j) :call EasyMotion#JK(0,0)
snoremap <silent> <Plug>(easymotion-j) :call EasyMotion#JK(0,0)
onoremap <silent> <Plug>(easymotion-j) :call EasyMotion#JK(0,0)
xnoremap <silent> <Plug>(easymotion-bd-jk) :call EasyMotion#JK(1,2)
nnoremap <silent> <Plug>(easymotion-bd-jk) :call EasyMotion#JK(0,2)
snoremap <silent> <Plug>(easymotion-bd-jk) :call EasyMotion#JK(0,2)
onoremap <silent> <Plug>(easymotion-bd-jk) :call EasyMotion#JK(0,2)
xnoremap <silent> <Plug>(easymotion-eol-bd-jk) :call EasyMotion#Eol(1,2)
nnoremap <silent> <Plug>(easymotion-eol-bd-jk) :call EasyMotion#Eol(0,2)
snoremap <silent> <Plug>(easymotion-eol-bd-jk) :call EasyMotion#Eol(0,2)
onoremap <silent> <Plug>(easymotion-eol-bd-jk) :call EasyMotion#Eol(0,2)
xnoremap <silent> <Plug>(easymotion-sol-bd-jk) :call EasyMotion#Sol(1,2)
nnoremap <silent> <Plug>(easymotion-sol-bd-jk) :call EasyMotion#Sol(0,2)
snoremap <silent> <Plug>(easymotion-sol-bd-jk) :call EasyMotion#Sol(0,2)
onoremap <silent> <Plug>(easymotion-sol-bd-jk) :call EasyMotion#Sol(0,2)
xnoremap <silent> <Plug>(easymotion-eol-k) :call EasyMotion#Eol(1,1)
nnoremap <silent> <Plug>(easymotion-eol-k) :call EasyMotion#Eol(0,1)
snoremap <silent> <Plug>(easymotion-eol-k) :call EasyMotion#Eol(0,1)
onoremap <silent> <Plug>(easymotion-eol-k) :call EasyMotion#Eol(0,1)
xnoremap <silent> <Plug>(easymotion-iskeyword-ge) :call EasyMotion#EK(1,1)
nnoremap <silent> <Plug>(easymotion-iskeyword-ge) :call EasyMotion#EK(0,1)
snoremap <silent> <Plug>(easymotion-iskeyword-ge) :call EasyMotion#EK(0,1)
onoremap <silent> <Plug>(easymotion-iskeyword-ge) :call EasyMotion#EK(0,1)
xnoremap <silent> <Plug>(easymotion-w) :call EasyMotion#WB(1,0)
nnoremap <silent> <Plug>(easymotion-w) :call EasyMotion#WB(0,0)
snoremap <silent> <Plug>(easymotion-w) :call EasyMotion#WB(0,0)
onoremap <silent> <Plug>(easymotion-w) :call EasyMotion#WB(0,0)
xnoremap <silent> <Plug>(easymotion-bd-W) :call EasyMotion#WBW(1,2)
nnoremap <silent> <Plug>(easymotion-bd-W) :call EasyMotion#WBW(0,2)
snoremap <silent> <Plug>(easymotion-bd-W) :call EasyMotion#WBW(0,2)
onoremap <silent> <Plug>(easymotion-bd-W) :call EasyMotion#WBW(0,2)
xnoremap <silent> <Plug>(easymotion-iskeyword-w) :call EasyMotion#WBK(1,0)
nnoremap <silent> <Plug>(easymotion-iskeyword-w) :call EasyMotion#WBK(0,0)
snoremap <silent> <Plug>(easymotion-iskeyword-w) :call EasyMotion#WBK(0,0)
onoremap <silent> <Plug>(easymotion-iskeyword-w) :call EasyMotion#WBK(0,0)
xnoremap <silent> <Plug>(easymotion-gE) :call EasyMotion#EW(1,1)
nnoremap <silent> <Plug>(easymotion-gE) :call EasyMotion#EW(0,1)
snoremap <silent> <Plug>(easymotion-gE) :call EasyMotion#EW(0,1)
onoremap <silent> <Plug>(easymotion-gE) :call EasyMotion#EW(0,1)
xnoremap <silent> <Plug>(easymotion-e) :call EasyMotion#E(1,0)
nnoremap <silent> <Plug>(easymotion-e) :call EasyMotion#E(0,0)
snoremap <silent> <Plug>(easymotion-e) :call EasyMotion#E(0,0)
onoremap <silent> <Plug>(easymotion-e) :call EasyMotion#E(0,0)
xnoremap <silent> <Plug>(easymotion-bd-E) :call EasyMotion#EW(1,2)
nnoremap <silent> <Plug>(easymotion-bd-E) :call EasyMotion#EW(0,2)
snoremap <silent> <Plug>(easymotion-bd-E) :call EasyMotion#EW(0,2)
onoremap <silent> <Plug>(easymotion-bd-E) :call EasyMotion#EW(0,2)
xnoremap <silent> <Plug>(easymotion-iskeyword-e) :call EasyMotion#EK(1,0)
nnoremap <silent> <Plug>(easymotion-iskeyword-e) :call EasyMotion#EK(0,0)
snoremap <silent> <Plug>(easymotion-iskeyword-e) :call EasyMotion#EK(0,0)
onoremap <silent> <Plug>(easymotion-iskeyword-e) :call EasyMotion#EK(0,0)
xnoremap <silent> <Plug>(easymotion-b) :call EasyMotion#WB(1,1)
nnoremap <silent> <Plug>(easymotion-b) :call EasyMotion#WB(0,1)
snoremap <silent> <Plug>(easymotion-b) :call EasyMotion#WB(0,1)
onoremap <silent> <Plug>(easymotion-b) :call EasyMotion#WB(0,1)
xnoremap <silent> <Plug>(easymotion-iskeyword-b) :call EasyMotion#WBK(1,1)
nnoremap <silent> <Plug>(easymotion-iskeyword-b) :call EasyMotion#WBK(0,1)
snoremap <silent> <Plug>(easymotion-iskeyword-b) :call EasyMotion#WBK(0,1)
onoremap <silent> <Plug>(easymotion-iskeyword-b) :call EasyMotion#WBK(0,1)
xnoremap <silent> <Plug>(easymotion-iskeyword-bd-w) :call EasyMotion#WBK(1,2)
nnoremap <silent> <Plug>(easymotion-iskeyword-bd-w) :call EasyMotion#WBK(0,2)
snoremap <silent> <Plug>(easymotion-iskeyword-bd-w) :call EasyMotion#WBK(0,2)
onoremap <silent> <Plug>(easymotion-iskeyword-bd-w) :call EasyMotion#WBK(0,2)
xnoremap <silent> <Plug>(easymotion-W) :call EasyMotion#WBW(1,0)
nnoremap <silent> <Plug>(easymotion-W) :call EasyMotion#WBW(0,0)
snoremap <silent> <Plug>(easymotion-W) :call EasyMotion#WBW(0,0)
onoremap <silent> <Plug>(easymotion-W) :call EasyMotion#WBW(0,0)
xnoremap <silent> <Plug>(easymotion-bd-w) :call EasyMotion#WB(1,2)
nnoremap <silent> <Plug>(easymotion-bd-w) :call EasyMotion#WB(0,2)
snoremap <silent> <Plug>(easymotion-bd-w) :call EasyMotion#WB(0,2)
onoremap <silent> <Plug>(easymotion-bd-w) :call EasyMotion#WB(0,2)
xnoremap <silent> <Plug>(easymotion-iskeyword-bd-e) :call EasyMotion#EK(1,2)
nnoremap <silent> <Plug>(easymotion-iskeyword-bd-e) :call EasyMotion#EK(0,2)
snoremap <silent> <Plug>(easymotion-iskeyword-bd-e) :call EasyMotion#EK(0,2)
onoremap <silent> <Plug>(easymotion-iskeyword-bd-e) :call EasyMotion#EK(0,2)
xnoremap <silent> <Plug>(easymotion-ge) :call EasyMotion#E(1,1)
nnoremap <silent> <Plug>(easymotion-ge) :call EasyMotion#E(0,1)
snoremap <silent> <Plug>(easymotion-ge) :call EasyMotion#E(0,1)
onoremap <silent> <Plug>(easymotion-ge) :call EasyMotion#E(0,1)
xnoremap <silent> <Plug>(easymotion-E) :call EasyMotion#EW(1,0)
nnoremap <silent> <Plug>(easymotion-E) :call EasyMotion#EW(0,0)
snoremap <silent> <Plug>(easymotion-E) :call EasyMotion#EW(0,0)
onoremap <silent> <Plug>(easymotion-E) :call EasyMotion#EW(0,0)
xnoremap <silent> <Plug>(easymotion-bd-e) :call EasyMotion#E(1,2)
nnoremap <silent> <Plug>(easymotion-bd-e) :call EasyMotion#E(0,2)
snoremap <silent> <Plug>(easymotion-bd-e) :call EasyMotion#E(0,2)
onoremap <silent> <Plug>(easymotion-bd-e) :call EasyMotion#E(0,2)
xnoremap <silent> <Plug>(easymotion-B) :call EasyMotion#WBW(1,1)
nnoremap <silent> <Plug>(easymotion-B) :call EasyMotion#WBW(0,1)
snoremap <silent> <Plug>(easymotion-B) :call EasyMotion#WBW(0,1)
onoremap <silent> <Plug>(easymotion-B) :call EasyMotion#WBW(0,1)
nnoremap <silent> <Plug>(easymotion-overwin-w) :call EasyMotion#overwin#w()
nnoremap <silent> <Plug>(easymotion-overwin-line) :call EasyMotion#overwin#line()
nnoremap <silent> <Plug>(easymotion-overwin-f2) :call EasyMotion#OverwinF(2)
nnoremap <silent> <Plug>(easymotion-overwin-f) :call EasyMotion#OverwinF(1)
xnoremap <silent> <Plug>(easymotion-Tln) :call EasyMotion#TL(-1,1,1)
nnoremap <silent> <Plug>(easymotion-Tln) :call EasyMotion#TL(-1,0,1)
snoremap <silent> <Plug>(easymotion-Tln) :call EasyMotion#TL(-1,0,1)
onoremap <silent> <Plug>(easymotion-Tln) :call EasyMotion#TL(-1,0,1)
xnoremap <silent> <Plug>(easymotion-t2) :call EasyMotion#T(2,1,0)
nnoremap <silent> <Plug>(easymotion-t2) :call EasyMotion#T(2,0,0)
snoremap <silent> <Plug>(easymotion-t2) :call EasyMotion#T(2,0,0)
onoremap <silent> <Plug>(easymotion-t2) :call EasyMotion#T(2,0,0)
xnoremap <silent> <Plug>(easymotion-t) :call EasyMotion#T(1,1,0)
nnoremap <silent> <Plug>(easymotion-t) :call EasyMotion#T(1,0,0)
snoremap <silent> <Plug>(easymotion-t) :call EasyMotion#T(1,0,0)
onoremap <silent> <Plug>(easymotion-t) :call EasyMotion#T(1,0,0)
xnoremap <silent> <Plug>(easymotion-s) :call EasyMotion#S(1,1,2)
nnoremap <silent> <Plug>(easymotion-s) :call EasyMotion#S(1,0,2)
snoremap <silent> <Plug>(easymotion-s) :call EasyMotion#S(1,0,2)
onoremap <silent> <Plug>(easymotion-s) :call EasyMotion#S(1,0,2)
xnoremap <silent> <Plug>(easymotion-tn) :call EasyMotion#T(-1,1,0)
nnoremap <silent> <Plug>(easymotion-tn) :call EasyMotion#T(-1,0,0)
snoremap <silent> <Plug>(easymotion-tn) :call EasyMotion#T(-1,0,0)
onoremap <silent> <Plug>(easymotion-tn) :call EasyMotion#T(-1,0,0)
xnoremap <silent> <Plug>(easymotion-bd-t2) :call EasyMotion#T(2,1,2)
nnoremap <silent> <Plug>(easymotion-bd-t2) :call EasyMotion#T(2,0,2)
snoremap <silent> <Plug>(easymotion-bd-t2) :call EasyMotion#T(2,0,2)
onoremap <silent> <Plug>(easymotion-bd-t2) :call EasyMotion#T(2,0,2)
xnoremap <silent> <Plug>(easymotion-tl) :call EasyMotion#TL(1,1,0)
nnoremap <silent> <Plug>(easymotion-tl) :call EasyMotion#TL(1,0,0)
snoremap <silent> <Plug>(easymotion-tl) :call EasyMotion#TL(1,0,0)
onoremap <silent> <Plug>(easymotion-tl) :call EasyMotion#TL(1,0,0)
xnoremap <silent> <Plug>(easymotion-bd-tn) :call EasyMotion#T(-1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-tn) :call EasyMotion#T(-1,0,2)
snoremap <silent> <Plug>(easymotion-bd-tn) :call EasyMotion#T(-1,0,2)
onoremap <silent> <Plug>(easymotion-bd-tn) :call EasyMotion#T(-1,0,2)
xnoremap <silent> <Plug>(easymotion-fn) :call EasyMotion#S(-1,1,0)
nnoremap <silent> <Plug>(easymotion-fn) :call EasyMotion#S(-1,0,0)
snoremap <silent> <Plug>(easymotion-fn) :call EasyMotion#S(-1,0,0)
onoremap <silent> <Plug>(easymotion-fn) :call EasyMotion#S(-1,0,0)
xnoremap <silent> <Plug>(easymotion-bd-tl) :call EasyMotion#TL(1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-tl) :call EasyMotion#TL(1,0,2)
snoremap <silent> <Plug>(easymotion-bd-tl) :call EasyMotion#TL(1,0,2)
onoremap <silent> <Plug>(easymotion-bd-tl) :call EasyMotion#TL(1,0,2)
xnoremap <silent> <Plug>(easymotion-fl) :call EasyMotion#SL(1,1,0)
nnoremap <silent> <Plug>(easymotion-fl) :call EasyMotion#SL(1,0,0)
snoremap <silent> <Plug>(easymotion-fl) :call EasyMotion#SL(1,0,0)
onoremap <silent> <Plug>(easymotion-fl) :call EasyMotion#SL(1,0,0)
xnoremap <silent> <Plug>(easymotion-bd-tl2) :call EasyMotion#TL(2,1,2)
nnoremap <silent> <Plug>(easymotion-bd-tl2) :call EasyMotion#TL(2,0,2)
snoremap <silent> <Plug>(easymotion-bd-tl2) :call EasyMotion#TL(2,0,2)
onoremap <silent> <Plug>(easymotion-bd-tl2) :call EasyMotion#TL(2,0,2)
xnoremap <silent> <Plug>(easymotion-bd-fn) :call EasyMotion#S(-1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-fn) :call EasyMotion#S(-1,0,2)
snoremap <silent> <Plug>(easymotion-bd-fn) :call EasyMotion#S(-1,0,2)
onoremap <silent> <Plug>(easymotion-bd-fn) :call EasyMotion#S(-1,0,2)
xnoremap <silent> <Plug>(easymotion-f) :call EasyMotion#S(1,1,0)
nnoremap <silent> <Plug>(easymotion-f) :call EasyMotion#S(1,0,0)
snoremap <silent> <Plug>(easymotion-f) :call EasyMotion#S(1,0,0)
onoremap <silent> <Plug>(easymotion-f) :call EasyMotion#S(1,0,0)
xnoremap <silent> <Plug>(easymotion-bd-fl) :call EasyMotion#SL(1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-fl) :call EasyMotion#SL(1,0,2)
snoremap <silent> <Plug>(easymotion-bd-fl) :call EasyMotion#SL(1,0,2)
onoremap <silent> <Plug>(easymotion-bd-fl) :call EasyMotion#SL(1,0,2)
xnoremap <silent> <Plug>(easymotion-Fl2) :call EasyMotion#SL(2,1,1)
nnoremap <silent> <Plug>(easymotion-Fl2) :call EasyMotion#SL(2,0,1)
snoremap <silent> <Plug>(easymotion-Fl2) :call EasyMotion#SL(2,0,1)
onoremap <silent> <Plug>(easymotion-Fl2) :call EasyMotion#SL(2,0,1)
xnoremap <silent> <Plug>(easymotion-tl2) :call EasyMotion#TL(2,1,0)
nnoremap <silent> <Plug>(easymotion-tl2) :call EasyMotion#TL(2,0,0)
snoremap <silent> <Plug>(easymotion-tl2) :call EasyMotion#TL(2,0,0)
onoremap <silent> <Plug>(easymotion-tl2) :call EasyMotion#TL(2,0,0)
xnoremap <silent> <Plug>(easymotion-f2) :call EasyMotion#S(2,1,0)
nnoremap <silent> <Plug>(easymotion-f2) :call EasyMotion#S(2,0,0)
snoremap <silent> <Plug>(easymotion-f2) :call EasyMotion#S(2,0,0)
onoremap <silent> <Plug>(easymotion-f2) :call EasyMotion#S(2,0,0)
xnoremap <silent> <Plug>(easymotion-Fln) :call EasyMotion#SL(-1,1,1)
nnoremap <silent> <Plug>(easymotion-Fln) :call EasyMotion#SL(-1,0,1)
snoremap <silent> <Plug>(easymotion-Fln) :call EasyMotion#SL(-1,0,1)
onoremap <silent> <Plug>(easymotion-Fln) :call EasyMotion#SL(-1,0,1)
xnoremap <silent> <Plug>(easymotion-sln) :call EasyMotion#SL(-1,1,2)
nnoremap <silent> <Plug>(easymotion-sln) :call EasyMotion#SL(-1,0,2)
snoremap <silent> <Plug>(easymotion-sln) :call EasyMotion#SL(-1,0,2)
onoremap <silent> <Plug>(easymotion-sln) :call EasyMotion#SL(-1,0,2)
xnoremap <silent> <Plug>(easymotion-tln) :call EasyMotion#TL(-1,1,0)
nnoremap <silent> <Plug>(easymotion-tln) :call EasyMotion#TL(-1,0,0)
snoremap <silent> <Plug>(easymotion-tln) :call EasyMotion#TL(-1,0,0)
onoremap <silent> <Plug>(easymotion-tln) :call EasyMotion#TL(-1,0,0)
xnoremap <silent> <Plug>(easymotion-fl2) :call EasyMotion#SL(2,1,0)
nnoremap <silent> <Plug>(easymotion-fl2) :call EasyMotion#SL(2,0,0)
snoremap <silent> <Plug>(easymotion-fl2) :call EasyMotion#SL(2,0,0)
onoremap <silent> <Plug>(easymotion-fl2) :call EasyMotion#SL(2,0,0)
xnoremap <silent> <Plug>(easymotion-bd-fl2) :call EasyMotion#SL(2,1,2)
nnoremap <silent> <Plug>(easymotion-bd-fl2) :call EasyMotion#SL(2,0,2)
snoremap <silent> <Plug>(easymotion-bd-fl2) :call EasyMotion#SL(2,0,2)
onoremap <silent> <Plug>(easymotion-bd-fl2) :call EasyMotion#SL(2,0,2)
xnoremap <silent> <Plug>(easymotion-T2) :call EasyMotion#T(2,1,1)
nnoremap <silent> <Plug>(easymotion-T2) :call EasyMotion#T(2,0,1)
snoremap <silent> <Plug>(easymotion-T2) :call EasyMotion#T(2,0,1)
onoremap <silent> <Plug>(easymotion-T2) :call EasyMotion#T(2,0,1)
xnoremap <silent> <Plug>(easymotion-bd-tln) :call EasyMotion#TL(-1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-tln) :call EasyMotion#TL(-1,0,2)
snoremap <silent> <Plug>(easymotion-bd-tln) :call EasyMotion#TL(-1,0,2)
onoremap <silent> <Plug>(easymotion-bd-tln) :call EasyMotion#TL(-1,0,2)
xnoremap <silent> <Plug>(easymotion-T) :call EasyMotion#T(1,1,1)
nnoremap <silent> <Plug>(easymotion-T) :call EasyMotion#T(1,0,1)
snoremap <silent> <Plug>(easymotion-T) :call EasyMotion#T(1,0,1)
onoremap <silent> <Plug>(easymotion-T) :call EasyMotion#T(1,0,1)
xnoremap <silent> <Plug>(easymotion-bd-t) :call EasyMotion#T(1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-t) :call EasyMotion#T(1,0,2)
snoremap <silent> <Plug>(easymotion-bd-t) :call EasyMotion#T(1,0,2)
onoremap <silent> <Plug>(easymotion-bd-t) :call EasyMotion#T(1,0,2)
xnoremap <silent> <Plug>(easymotion-Tn) :call EasyMotion#T(-1,1,1)
nnoremap <silent> <Plug>(easymotion-Tn) :call EasyMotion#T(-1,0,1)
snoremap <silent> <Plug>(easymotion-Tn) :call EasyMotion#T(-1,0,1)
onoremap <silent> <Plug>(easymotion-Tn) :call EasyMotion#T(-1,0,1)
xnoremap <silent> <Plug>(easymotion-s2) :call EasyMotion#S(2,1,2)
nnoremap <silent> <Plug>(easymotion-s2) :call EasyMotion#S(2,0,2)
snoremap <silent> <Plug>(easymotion-s2) :call EasyMotion#S(2,0,2)
onoremap <silent> <Plug>(easymotion-s2) :call EasyMotion#S(2,0,2)
xnoremap <silent> <Plug>(easymotion-Tl) :call EasyMotion#TL(1,1,1)
nnoremap <silent> <Plug>(easymotion-Tl) :call EasyMotion#TL(1,0,1)
snoremap <silent> <Plug>(easymotion-Tl) :call EasyMotion#TL(1,0,1)
onoremap <silent> <Plug>(easymotion-Tl) :call EasyMotion#TL(1,0,1)
xnoremap <silent> <Plug>(easymotion-sn) :call EasyMotion#S(-1,1,2)
nnoremap <silent> <Plug>(easymotion-sn) :call EasyMotion#S(-1,0,2)
snoremap <silent> <Plug>(easymotion-sn) :call EasyMotion#S(-1,0,2)
onoremap <silent> <Plug>(easymotion-sn) :call EasyMotion#S(-1,0,2)
xnoremap <silent> <Plug>(easymotion-Fn) :call EasyMotion#S(-1,1,1)
nnoremap <silent> <Plug>(easymotion-Fn) :call EasyMotion#S(-1,0,1)
snoremap <silent> <Plug>(easymotion-Fn) :call EasyMotion#S(-1,0,1)
onoremap <silent> <Plug>(easymotion-Fn) :call EasyMotion#S(-1,0,1)
xnoremap <silent> <Plug>(easymotion-sl) :call EasyMotion#SL(1,1,2)
nnoremap <silent> <Plug>(easymotion-sl) :call EasyMotion#SL(1,0,2)
snoremap <silent> <Plug>(easymotion-sl) :call EasyMotion#SL(1,0,2)
onoremap <silent> <Plug>(easymotion-sl) :call EasyMotion#SL(1,0,2)
xnoremap <silent> <Plug>(easymotion-Fl) :call EasyMotion#SL(1,1,1)
nnoremap <silent> <Plug>(easymotion-Fl) :call EasyMotion#SL(1,0,1)
snoremap <silent> <Plug>(easymotion-Fl) :call EasyMotion#SL(1,0,1)
onoremap <silent> <Plug>(easymotion-Fl) :call EasyMotion#SL(1,0,1)
xnoremap <silent> <Plug>(easymotion-sl2) :call EasyMotion#SL(2,1,2)
nnoremap <silent> <Plug>(easymotion-sl2) :call EasyMotion#SL(2,0,2)
snoremap <silent> <Plug>(easymotion-sl2) :call EasyMotion#SL(2,0,2)
onoremap <silent> <Plug>(easymotion-sl2) :call EasyMotion#SL(2,0,2)
xnoremap <silent> <Plug>(easymotion-bd-fln) :call EasyMotion#SL(-1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-fln) :call EasyMotion#SL(-1,0,2)
snoremap <silent> <Plug>(easymotion-bd-fln) :call EasyMotion#SL(-1,0,2)
onoremap <silent> <Plug>(easymotion-bd-fln) :call EasyMotion#SL(-1,0,2)
xnoremap <silent> <Plug>(easymotion-F) :call EasyMotion#S(1,1,1)
nnoremap <silent> <Plug>(easymotion-F) :call EasyMotion#S(1,0,1)
snoremap <silent> <Plug>(easymotion-F) :call EasyMotion#S(1,0,1)
onoremap <silent> <Plug>(easymotion-F) :call EasyMotion#S(1,0,1)
xnoremap <silent> <Plug>(easymotion-bd-f) :call EasyMotion#S(1,1,2)
nnoremap <silent> <Plug>(easymotion-bd-f) :call EasyMotion#S(1,0,2)
snoremap <silent> <Plug>(easymotion-bd-f) :call EasyMotion#S(1,0,2)
onoremap <silent> <Plug>(easymotion-bd-f) :call EasyMotion#S(1,0,2)
xnoremap <silent> <Plug>(easymotion-F2) :call EasyMotion#S(2,1,1)
nnoremap <silent> <Plug>(easymotion-F2) :call EasyMotion#S(2,0,1)
snoremap <silent> <Plug>(easymotion-F2) :call EasyMotion#S(2,0,1)
onoremap <silent> <Plug>(easymotion-F2) :call EasyMotion#S(2,0,1)
xnoremap <silent> <Plug>(easymotion-bd-f2) :call EasyMotion#S(2,1,2)
nnoremap <silent> <Plug>(easymotion-bd-f2) :call EasyMotion#S(2,0,2)
snoremap <silent> <Plug>(easymotion-bd-f2) :call EasyMotion#S(2,0,2)
onoremap <silent> <Plug>(easymotion-bd-f2) :call EasyMotion#S(2,0,2)
xnoremap <silent> <Plug>(easymotion-Tl2) :call EasyMotion#TL(2,1,1)
nnoremap <silent> <Plug>(easymotion-Tl2) :call EasyMotion#TL(2,0,1)
snoremap <silent> <Plug>(easymotion-Tl2) :call EasyMotion#TL(2,0,1)
onoremap <silent> <Plug>(easymotion-Tl2) :call EasyMotion#TL(2,0,1)
xnoremap <silent> <Plug>(easymotion-fln) :call EasyMotion#SL(-1,1,0)
nnoremap <silent> <Plug>(easymotion-fln) :call EasyMotion#SL(-1,0,0)
snoremap <silent> <Plug>(easymotion-fln) :call EasyMotion#SL(-1,0,0)
onoremap <silent> <Plug>(easymotion-fln) :call EasyMotion#SL(-1,0,0)
noremap <silent> <Plug>(indexed-search-N) N:ShowSearchIndex
noremap <silent> <Plug>(indexed-search-n) n:ShowSearchIndex
noremap <silent> <Plug>(indexed-search-#) #:ShowSearchIndex
noremap <silent> <Plug>(indexed-search-*) *:ShowSearchIndex
noremap <Plug>(indexed-search-?) :ShowSearchIndex?
noremap <Plug>(indexed-search-/) :ShowSearchIndex/
omap <Plug>SneakPrevious <Plug>Sneak_,
omap <Plug>SneakNext <Plug>Sneak_;
xmap <Plug>SneakPrevious <Plug>Sneak_,
xmap <Plug>SneakNext <Plug>Sneak_;
nmap <Plug>SneakPrevious <Plug>Sneak_,
nmap <Plug>SneakNext <Plug>Sneak_;
omap <Plug>(SneakStreakBackward) <Plug>SneakLabel_S
omap <Plug>(SneakStreak) <Plug>SneakLabel_s
xmap <Plug>(SneakStreakBackward) <Plug>SneakLabel_S
xmap <Plug>(SneakStreak) <Plug>SneakLabel_s
nmap <Plug>(SneakStreakBackward) <Plug>SneakLabel_S
nmap <Plug>(SneakStreak) <Plug>SneakLabel_s
xmap <Plug>VSneakPrevious <Plug>Sneak_,
xmap <Plug>VSneakNext <Plug>Sneak_;
xmap <Plug>VSneakBackward <Plug>Sneak_S
xmap <Plug>VSneakForward <Plug>Sneak_s
nmap <Plug>SneakBackward <Plug>Sneak_S
nmap <Plug>SneakForward <Plug>Sneak_s
onoremap <silent> <Plug>SneakLabel_S :call sneak#wrap(v:operator, 2, 1, 2, 2)
onoremap <silent> <Plug>SneakLabel_s :call sneak#wrap(v:operator, 2, 0, 2, 2)
xnoremap <silent> <Plug>SneakLabel_S :call sneak#wrap(visualmode(), 2, 1, 2, 2)
xnoremap <silent> <Plug>SneakLabel_s :call sneak#wrap(visualmode(), 2, 0, 2, 2)
nnoremap <silent> <Plug>SneakLabel_S :call sneak#wrap('', 2, 1, 2, 2)
nnoremap <silent> <Plug>SneakLabel_s :call sneak#wrap('', 2, 0, 2, 2)
onoremap <silent> <Plug>Sneak_T :call sneak#wrap(v:operator, 1, 1, 0, 0)
onoremap <silent> <Plug>Sneak_t :call sneak#wrap(v:operator, 1, 0, 0, 0)
xnoremap <silent> <Plug>Sneak_T :call sneak#wrap(visualmode(), 1, 1, 0, 0)
xnoremap <silent> <Plug>Sneak_t :call sneak#wrap(visualmode(), 1, 0, 0, 0)
nnoremap <silent> <Plug>Sneak_T :call sneak#wrap('', 1, 1, 0, 0)
nnoremap <silent> <Plug>Sneak_t :call sneak#wrap('', 1, 0, 0, 0)
onoremap <silent> <Plug>Sneak_F :call sneak#wrap(v:operator, 1, 1, 1, 0)
onoremap <silent> <Plug>Sneak_f :call sneak#wrap(v:operator, 1, 0, 1, 0)
xnoremap <silent> <Plug>Sneak_F :call sneak#wrap(visualmode(), 1, 1, 1, 0)
xnoremap <silent> <Plug>Sneak_f :call sneak#wrap(visualmode(), 1, 0, 1, 0)
nnoremap <silent> <Plug>Sneak_F :call sneak#wrap('', 1, 1, 1, 0)
nnoremap <silent> <Plug>Sneak_f :call sneak#wrap('', 1, 0, 1, 0)
onoremap <silent> <Plug>Sneak_, :call sneak#rpt(v:operator, 1)
onoremap <silent> <Plug>Sneak_; :call sneak#rpt(v:operator, 0)
xnoremap <silent> <Plug>Sneak_, :call sneak#rpt(visualmode(), 1)
xnoremap <silent> <Plug>Sneak_; :call sneak#rpt(visualmode(), 0)
nnoremap <silent> <Plug>Sneak_, :call sneak#rpt('', 1)
nnoremap <silent> <Plug>Sneak_; :call sneak#rpt('', 0)
onoremap <silent> <Plug>SneakRepeat :call sneak#wrap(v:operator, sneak#util#getc(), sneak#util#getc(), sneak#util#getc(), sneak#util#getc())
onoremap <silent> <Plug>Sneak_S :call sneak#wrap(v:operator, 2, 1, 2, 1)
onoremap <silent> <Plug>Sneak_s :call sneak#wrap(v:operator, 2, 0, 2, 1)
xnoremap <silent> <Plug>Sneak_S :call sneak#wrap(visualmode(), 2, 1, 2, 1)
xnoremap <silent> <Plug>Sneak_s :call sneak#wrap(visualmode(), 2, 0, 2, 1)
nnoremap <silent> <Plug>Sneak_S :call sneak#wrap('', 2, 1, 2, 1)
nnoremap <silent> <Plug>Sneak_s :call sneak#wrap('', 2, 0, 2, 1)
onoremap <silent> <Plug>(textobj-rubyblock-i) :call g:__textobj_rubyblock.do_by_function("select-i","-","o")
vnoremap <silent> <Plug>(textobj-rubyblock-i) :call g:__textobj_rubyblock.do_by_function("select-i","-","v")
onoremap <silent> <Plug>(textobj-rubyblock-a) :call g:__textobj_rubyblock.do_by_function("select-a","-","o")
vnoremap <silent> <Plug>(textobj-rubyblock-a) :call g:__textobj_rubyblock.do_by_function("select-a","-","v")
onoremap <silent> <Plug>(textobj-underscore-i) :call g:__textobj_underscore.do_by_function("select-i","-","o")
vnoremap <silent> <Plug>(textobj-underscore-i) :call g:__textobj_underscore.do_by_function("select-i","-","v")
onoremap <silent> <Plug>(textobj-underscore-a) :call g:__textobj_underscore.do_by_function("select-a","-","o")
vnoremap <silent> <Plug>(textobj-underscore-a) :call g:__textobj_underscore.do_by_function("select-a","-","v")
onoremap <silent> <Plug>(textobj-function-i) :call g:__textobj_function.do_by_function("select","i","o")
vnoremap <silent> <Plug>(textobj-function-i) :call g:__textobj_function.do_by_function("select","i","v")
onoremap <silent> <Plug>(textobj-function-A) :call g:__textobj_function.do_by_function("select","A","o")
vnoremap <silent> <Plug>(textobj-function-A) :call g:__textobj_function.do_by_function("select","A","v")
onoremap <silent> <Plug>(textobj-function-I) :call g:__textobj_function.do_by_function("select","I","o")
vnoremap <silent> <Plug>(textobj-function-I) :call g:__textobj_function.do_by_function("select","I","v")
onoremap <silent> <Plug>(textobj-function-a) :call g:__textobj_function.do_by_function("select","a","o")
vnoremap <silent> <Plug>(textobj-function-a) :call g:__textobj_function.do_by_function("select","a","v")
onoremap <silent> <Plug>(textobj-entire-i) :call g:__textobj_entire.do_by_function("select-i","-","o")
vnoremap <silent> <Plug>(textobj-entire-i) :call g:__textobj_entire.do_by_function("select-i","-","v")
onoremap <silent> <Plug>(textobj-entire-a) :call g:__textobj_entire.do_by_function("select-a","-","o")
vnoremap <silent> <Plug>(textobj-entire-a) :call g:__textobj_entire.do_by_function("select-a","-","v")
onoremap <silent> <Plug>(textobj-datetime-tz) :call g:__textobj_datetime.do_by_pattern("select","tz","o")
vnoremap <silent> <Plug>(textobj-datetime-tz) :call g:__textobj_datetime.do_by_pattern("select","tz","v")
onoremap <silent> <Plug>(textobj-datetime-auto) :call g:__textobj_datetime.do_by_pattern("select","auto","o")
vnoremap <silent> <Plug>(textobj-datetime-auto) :call g:__textobj_datetime.do_by_pattern("select","auto","v")
onoremap <silent> <Plug>(textobj-datetime-time) :call g:__textobj_datetime.do_by_pattern("select","time","o")
vnoremap <silent> <Plug>(textobj-datetime-time) :call g:__textobj_datetime.do_by_pattern("select","time","v")
onoremap <silent> <Plug>(textobj-datetime-date) :call g:__textobj_datetime.do_by_pattern("select","date","o")
vnoremap <silent> <Plug>(textobj-datetime-date) :call g:__textobj_datetime.do_by_pattern("select","date","v")
onoremap <silent> <Plug>(textobj-datetime-full) :call g:__textobj_datetime.do_by_pattern("select","full","o")
vnoremap <silent> <Plug>(textobj-datetime-full) :call g:__textobj_datetime.do_by_pattern("select","full","v")
onoremap <silent> <Plug>(textobj-rubysymbol-i) :call g:__textobj_rubysymbol.do_by_function("select-i","-","o")
vnoremap <silent> <Plug>(textobj-rubysymbol-i) :call g:__textobj_rubysymbol.do_by_function("select-i","-","v")
onoremap <silent> <Plug>(textobj-rubysymbol-a) :call g:__textobj_rubysymbol.do_by_function("select-a","-","o")
vnoremap <silent> <Plug>(textobj-rubysymbol-a) :call g:__textobj_rubysymbol.do_by_function("select-a","-","v")
xnoremap <Plug>ColorFgBg :ColorSwapFgBg
nnoremap <Plug>ColorFgBg :ColorSwapFgBg
xnoremap <Plug>ColorContrast :ColorContrast
nnoremap <Plug>ColorContrast :ColorContrast
xnoremap <Plug>Colorizer :ColorHighlight
nnoremap <Plug>Colorizer :ColorToggle
nnoremap <SNR>119_: :=v:count ? v:count : ''
snoremap <silent> <Plug>snipMateBack a=snipMate#BackwardsSnippet()
snoremap <silent> <Plug>snipMateNextOrTrigger a=snipMate#TriggerSnippet()
nnoremap <C-Right> >
nnoremap <C-Left> <
vmap <C-Down> ]egv
vmap <C-Up> [egv
nnoremap <C-Down> -
nnoremap <C-Up> +
imap  wa
imap S <Plug>ISurround
imap s <Plug>Isurround
imap 	 <Plug>snipMateNextOrTrigger
imap <silent> <NL> <%  %>2hi
imap <silent>  <%=   %>3hi
imap   => 
imap  <Plug>DiscretionaryEnd
imap 	 <Plug>snipMateShow
imap  <Plug>Isurround
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
imap 9 <Plug>TComment_9
imap 8 <Plug>TComment_8
imap 7 <Plug>TComment_7
imap 6 <Plug>TComment_6
imap 5 <Plug>TComment_5
imap 4 <Plug>TComment_4
imap 3 <Plug>TComment_3
imap 2 <Plug>TComment_2
imap 1 <Plug>TComment_1
imap s <Plug>TComment_s
imap n <Plug>TComment_n
imap a <Plug>TComment_a
imap b <Plug>TComment_b
imap i <Plug>TComment_i
imap r <Plug>TComment_r
imap   <Plug>TComment_ 
imap p <Plug>TComment_p
imap  <Plug>TComment_
map ¥ :so %
map <silent> ¹ :tabn 9
map <silent> ¸ :tabn 8
map <silent> · :tabn 7
map <silent> ¶ :tabn 6
map <silent> µ :tabn 5
map <silent> ´ :tabn 4
map <silent> ³ :tabn 3
map <silent> ² :tabn 2
map <silent> ± :tabn 1
map ¯ :TComment
nnoremap © f)ci)
nnoremap ¨ f(ci(
nnoremap ¢ f"ci"
nnoremap § f'ci'
vmap Á :Tabularize /
nmap Á :Tabularize /
nnoremap <silent> ë {
nnoremap <silent> ê }
nmap Î :NERDTreeToggle
nnoremap Ý f]ci]
nnoremap Û f[ci[
cmap w!! w !sudo tee % >/dev/null
cabbr gitv =(getcmdtype()==':' && getcmdpos()==1 ? 'Gitv' : 'gitv')
abbr rbf before { }<Left><Left>
abbr cl! console.log( )<Left><Left>
abbr pry! require 'pry'; binding.pry
abbr rld Rails.logger.debug
abbr rlb Rails.logger.banner
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autoread
set background=dark
set backspace=indent,eol,start
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set grepprg=git\ grep
set guicursor=a:blinkon0
set helplang=en
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set listchars=tab:\ \ ,trail:·
set ruler
set runtimepath=~/.vim,~/.vim/bundle/vundle,~/.vim/bundle/vim-ruby-refactoring,~/.vim/bundle/vim-rails,~/.vim/bundle/vim-rake,~/.vim/bundle/vim-rvm,~/.vim/bundle/vim-ruby,~/.vim/bundle/rspec.vim,~/.vim/bundle/vim-iterm-rspec,~/.vim/bundle/vim-spec-finder,~/.vim/bundle/vim-change-hash-syntax,~/.vim/bundle/vim-bundler,~/.vim/bundle/vim-polyglot,~/.vim/bundle/vim-snipmate,~/.vim/bundle/vim-snippets,~/.vim/bundle/vim-flavored-markdown,~/.vim/bundle/syntastic,~/.vim/bundle/vim-markdown-preview,~/.vim/bundle/vim-html-escape,~/.vim/bundle/vim-jsx,~/.vim/bundle/vim-graphql,~/.vim/bundle/gitv,~/.vim/bundle/gist-vim,~/.vim/bundle/vim-fugitive,~/.vim/bundle/vim-git,~/.vim/bundle/color_highlight,~/.vim/bundle/vim-colors-solarized,~/.vim/bundle/lightline.vim,~/.vim/bundle/tmux.vim,~/.vim/bundle/gruvbox,~/.vim/bundle/showmarks,~/.vim/bundle/base16-vim,~/.vim/bundle/csapprox,~/.vim/bundle/vim-indentobject,~/.vim/bundle/vim-textobj-rubysymbol,~/.vim/bundle/textobj-word-column.vim,~/.vim/bundle/vim-textobj-datetime,~/.vim/bundle/vim-textobj-entire,~/.vim/bundle/vim-textobj-function,~/.vim/bundle/vim-textobj-user,~/.vim/bundle/vim-textobj-underscore,~/.vim/bundle/vim-indent-guides,~/.vim/bundle/vim-textobj-rubyblock,~/.vim/bundle/vim-textobj-function-javascript,~/.vim/bundle/targets.vim,~/.vim/bundle/vim-sneak,~/.vim/bundle/ag.vim,~/.vim/bundle/vim-indexed-search,~/.vim/bundle/vim-visual-star-search,~/.vim/bundle/greplace.vim,~/.vim/bundle/vim-easymotion,~/.vim/bundle/vim-nerdtree-tabs,~/.vim/bundle/nerdtree,~/.vim/bundle/ctrlp.vim,~/.vim/bundle/ctrlp-cmatcher,~/.vim/bundle/fzf,~/.vim/bundle/vim-misc,~/.vim/bundle/vim-session,~/.vim/bundle/splitjoin.vim,~/.vim/bundle/delimitMate,~/.vim/bundle/neocomplete,~/.vim/bundle/change-inside-surroundings.vim,~/.vim/bundle/tabular,~/.vim/bundle/tcomment_vim,~/.vim/bundle/camelcasemotion,~/.vim/bundle/matchit.zip,~/.vim/bundle/vim-multiple-cursors,~/.vim/bundle/investigate.vim,~/.vim/bundle/NrrwRgn,~/.vim/bundle/vim-tmux-navigator,~/.vim/bundle/vim-addon-mw-utils,~/.vim/bundle/file-line,~/.vim/bundle/webapi-vim,~/.vim/bundle/gundo.vim,~/.vim/bundle/YankRing.vim,~/.vim/bundle/tlib_vim,~/.vim/bundle/vim-abolish,~/.vim/bundle/vim-endwise,~/.vim/bundle/vim-ragtag,~/.vim/bundle/vim-repeat,~/.vim/bundle/vim-surround,~/.vim/bundle/vim-unimpaired,~/.vim/bundle/AnsiEsc.vim,~/.vim/bundle/AutoTag,~/.vim/bundle/lastpos.vim,~/.vim/bundle/sudo.vim,~/.vim/bundle/ctrlr.vim,~/.vim/bundle/editorconfig-vim,/usr/share/vim/site,/usr/share/vim/current,/usr/share/vim/site/after,~/.vim/after,~/.vim/bundle/vundle/,~/.vim/vundles/,~/.vim/bundle/vundle/after,~/.vim/bundle/vim-ruby-refactoring/after,~/.vim/bundle/vim-rails/after,~/.vim/bundle/vim-rake/after,~/.vim/bundle/vim-rvm/after,~/.vim/bundle/vim-ruby/after,~/.vim/bundle/rspec.vim/after,~/.vim/bundle/vim-iterm-rspec/after,~/.vim/bundle/vim-spec-finder/after,~/.vim/bundle/vim-change-hash-syntax/after,~/.vim/bundle/vim-bundler/after,~/.vim/bundle/vim-polyglot/after,~/.vim/bundle/vim-snipmate/after,~/.vim/bundle/vim-snippets/after,~/.vim/bundle/vim-flavored-markdown/after,~/.vim/bundle/syntastic/after,~/.vim/bundle/vim-markdown-preview/after,~/.vim/bundle/vim-html-escape/after,~/.vim/bundle/vim-jsx/after,~/.vim/bundle/vim-graphql/after,~/.vim/bundle/gitv/after,~/.vim/bundle/gist-vim/after,~/.vim/bundle/vim-fugitive/after,~/.vim/bundle/vim-git/after,~/.vim/bundle/color_highlight/after,~/.vim/bundle/vim-colors-solarized/after,~/.vim/bundle/lightline.vim/after,~/.vim/bundle/tmux.vim/after,~/.vim/bundle/gruvbox/after,~/.vim/bundle/showmarks/after,~/.vim/bundle/base16-vim/after,~/.vim/bundle/csapprox/after,~/.vim/bundle/vim-indentobject/after,~/.vim/bundle/vim-textobj-rubysymbol/after,~/.vim/bundle/textobj-word-column.vim/after,~/.vim/bundle/vim-textobj-datetime/after,~/.vim/bundle/vim-textobj-entire/after,~/.vim/bundle/vim-textobj-function/after,~/.vim/bundle/vim-textobj-user/after,~/.vim/bundle/vim-textobj-underscore/after,~/.vim/bundle/vim-indent-guides/after,~/.vim/bundle/vim-textobj-rubyblock/after,~/.vim/bundle/vim-textobj-function-javascript/after,~/.vim/bundle/targets.vim/after,~/.vim/b
set scrolloff=8
set shell=bash\ -i
set shiftwidth=2
set showcmd
set showmatch
set sidescroll=1
set sidescrolloff=15
set smartcase
set smartindent
set smarttab
set softtabstop=2
set noswapfile
set tabline=%!lightline#tabline()
set tabstop=2
set visualbell
set wildignore=*.o,*.obj,*~,*vim/backups*,*sass-cache*,*DS_Store*,vendor/rails/**,vendor/cache/**,*.gem,log/**,tmp/**,*.png,*.jpg,*.gif
set wildmenu
set wildmode=list:longest
set nowritebackup
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Documents/GitHub/CommonLisp-Go
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 Go-Game.lisp
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
let s:cpo_save=&cpo
set cpo&vim
imap <buffer> <S-BS> <Plug>delimitMateS-BS
imap <buffer> <BS> <Plug>delimitMateBS
imap <buffer> <silent> g <Plug>delimitMateJumpMany
imap <buffer>  <Plug>delimitMateBS
imap <buffer> " <Plug>delimitMate"
imap <buffer> ' <Plug>delimitMate'
imap <buffer> ( <Plug>delimitMate(
imap <buffer> ) <Plug>delimitMate)
imap <buffer> [ <Plug>delimitMate[
imap <buffer> ] <Plug>delimitMate]
imap <buffer> ` <Plug>delimitMate`
imap <buffer> { <Plug>delimitMate{
imap <buffer> } <Plug>delimitMate}
let &cpo=s:cpo_save
unlet s:cpo_save
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
set nofoldenable
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=1
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=manual
setlocal foldminlines=1
set foldnestmax=3
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
setlocal iskeyword=@,48-57,_,192-255,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
set linebreak
setlocal nolinebreak
setlocal lisp
setlocal lispwords=
set list
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
setlocal statusline=%{lightline#link()}%#LightlineLeft_active_0#%(\ %{lightline#mode()}\ %)%{(&paste)?\"⮁\":\"\"}%(\ %{&paste?\"PASTE\":\"\"}\ %)%#LightlineLeft_active_0_1#���%#LightlineLeft_active_1#%(\ %{MyFugitive()}\ %)%{MyFugitive()!=#\"\"&&(MyReadonly()!=#\"\"||MyFilename()!=#\"\"||(&modified||!&modifiable))?\"⮁\":\"\"}%(\ %{MyReadonly()}\ %)%{MyReadonly()!=#\"\"&&(MyFilename()!=#\"\"||(&modified||!&modifiable))?\"⮁\":\"\"}%(\ %{MyFilename()}\ %)%{MyFilename()!=#\"\"&&((&modified||!&modifiable))?\"⮁\":\"\"}%(\ %M\ %)%#LightlineLeft_active_1_2#���%#LightlineMiddle_active#%=%#LightlineRight_active_2_3#⮂%#LightlineRight_active_2#%(\ %{&ff}\ %)%{1||1?\"⮃\":\"\"}%(\ %{&fenc!=#\"\"?&fenc:&enc}\ %)%{1?\"⮃\":\"\"}%(\ %{&ft!=#\"\"?&ft:\"no\ ft\"}\ %)%#LightlineRight_active_1_2#⮂%#LightlineRight_active_1#%(\ %3p%%\ %)%#LightlineRight_active_0_1#⮂%#LightlineRight_active_0#%(\ %3l:%-2v\ %)
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
set nowrap
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
17,47fold
49,62fold
64,84fold
1,105fold
108,136fold
138,187fold
189,197fold
199,221fold
223,228fold
230,243fold
245,346fold
348,427fold
429,436fold
438,447fold
449,471fold
473,510fold
512,550fold
107,550fold
553,567fold
569,580fold
582,592fold
594,604fold
606,612fold
614,640fold
642,678fold
680,745fold
747,776fold
778,798fold
800,836fold
838,891fold
893,921fold
552,921fold
924,1104fold
1106,1219fold
1221,1342fold
1344,1428fold
1430,1434fold
1448,1478fold
1510,1539fold
1480,1540fold
1436,1570fold
923,1570fold
1573,1589fold
1592,1633fold
1635,1668fold
1670,1690fold
1692,1719fold
1591,1719fold
1722,1740fold
1742,1793fold
1795,1816fold
1818,1841fold
1843,1866fold
1868,1896fold
1898,1929fold
1931,1959fold
1721,1959fold
1962,2001fold
2003,2028fold
2030,2061fold
2063,2092fold
2094,2128fold
2130,2152fold
1961,2152fold
2155,2170fold
2172,2198fold
2200,2223fold
2225,2229fold
2154,2229fold
2231,2259fold
2261,2276fold
2278,2285fold
1572,2285fold
1
normal! zo
49
normal! zo
1
normal! zc
107
normal! zo
107
normal! zc
552
normal! zo
552
normal! zc
923
normal! zo
1436
normal! zo
1448
normal! zo
1480
normal! zo
1480
normal! zc
1436
normal! zc
923
normal! zc
1572
normal! zo
1591
normal! zo
1591
normal! zc
1721
normal! zo
1721
normal! zc
1961
normal! zo
1961
normal! zc
2154
normal! zo
2172
normal! zo
1572
normal! zc
let s:l = 1 - ((0 * winheight(0) + 25) / 51)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
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
