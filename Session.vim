let SessionLoad = 1
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/mydoc/research/dusk2dawn/initialAnalysis
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +101 explore/optics_evaluate.R
badd +108 explore/make.R
badd +34 explore/optics_explore.R
badd +326 lib/optics_find_staypoint.R
badd +4 /tmp/nvim2AHjgM/182
badd +74 explore/map_explore.v2.R
badd +28 lib/find_staypoint.R
badd +60 lib/evaluate_staypoint_estimates_helper.R
badd +98 lib/gps_functions.R
argglobal
%argdel
edit explore/optics_explore.R
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
set nosplitbelow
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 152 + 126) / 253)
exe 'vert 2resize ' . ((&columns * 100 + 126) / 253)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 17 - ((16 * winheight(0) + 30) / 61)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
17
normal! 06|
wincmd w
argglobal
if bufexists("term://.//1536:R\ ") | buffer term://.//1536:R\  | else | edit term://.//1536:R\  | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1448 - ((57 * winheight(0) + 30) / 61)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1448
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 152 + 126) / 253)
exe 'vert 2resize ' . ((&columns * 100 + 126) / 253)
tabnext 1
if exists('s:wipebuf') && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 winminheight=1 winminwidth=1 shortmess=aFc
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
