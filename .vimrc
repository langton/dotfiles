set nocompatible
filetype plugin indent on
"set smartindent
set autoindent
set expandtab
set softtabstop=4
set tabstop=4
set shiftwidth=4
"set textwidth=80
set showmatch
set guioptions-=T
set guioptions-=m
set showmode
set showcmd
set vb t_vb=
set ruler
set nohls
set incsearch
set display=lastline
if &t_Co > 1
	syntax enable
endif
set gcr=a:blinkwait0,a:block-cursor
set listchars=tab:>~,trail:~
set printoptions=paper:letter,formfeed:y

if has("python")
python  << endpython
try:
    import sys
    import vim
    import os
    for p in sys.path:
        if os.path.isdir(p):
            vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
except Exception:
    pass
def EvaluateCurrentRange():
    eval(compile('\n'.join(vim.current.range),'','exec'),globals())
endpython
endif

map <silent><C-Left> <C-T>
set tags+=$HOME/.vim/tags/python.ctags
map <C-h> :py EvaluateCurrentRange() 
autocmd BufRead *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
map <silent><A-Right> :tabnext<CR>
autocmd BufRead *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m
map <silent><A-Left> :tabprevious<CR> 
inoremap <Nul> <C-x><C-o>
if version >= 700
  autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
  autocmd InsertLeave * if pumvisible() == 0|pclose|endif 
  set cot+=menuone
  colorscheme macvim
endif
set nobackup
set directory=$HOME/.vimswap
set foldmethod=indent
set foldnestmax=10
set nofoldenable
function! GnuIndent()
  setlocal cinoptions=>4,n-2,{2,^-2,:2,=2,g0,h2,p5,t0,+2,(0,u0,w1,m1
  setlocal shiftwidth=2
  setlocal tabstop=8
endfunction
au FileType c,cpp call GnuIndent()
