"" My personal customization of vim
"" TODO : maybe exchange ultisnips and maralla completor TO TEST

set nocompatible " Basic of config for vim

" OPTIONS {{{

let my_lsp = true " enables lsp in Vim

" }}}

" PLUGINS ---------------------------------------------------------------------- {{{

filetype plugin indent on " For plugins to load correctly
filetype off " Helps force plugins to load correctly when it is turned back on below
call plug#begin('~/.vim/plugged') " for user only

Plug 'tpope/vim-sensible' " sane defaults

" Color themes
Plug 'gruvbox-community/gruvbox/' " nice and soft colorscheme
Plug 'tomasr/molokai' " colorscheme compatible with many terminals
Plug 'catppuccin/vim'

" essential plugins
" see for example https://github.com/autozimu/LanguageClient-neovim/issues/35#issuecomment-288731665
"Plug 'maralla/completor.vim' " auto-complete

" snippets allow to easily 'fill' common patterns
"Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" to close automatically parenthesis-likes
"Plug 'Raimondi/delimitMate'

" file explorer
"Plug 'preservim/nerdtree'
"Plug 'Nopik/vim-nerdtree-direnter'

" markdown in vim
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown'

" to comment/uncomment on multiple file types
Plug 'preservim/nerdcommenter'

" Vim status line
Plug 'powerline/powerline'

" plugin for python (syntax, folding, gotodef, ...)
"Plug 'python-mode/python-mode', { 'for': 'python', 'branch': 'develop' }

" Plugins pour Rmd
Plug 'vim-pandoc/vim-pandoc-syntax'

" Enhanced C and C++ syntax highlighting
Plug 'bfrg/vim-c-cpp-modern'

" Plugin pour Latex
Plug 'lervag/vimtex'

" GLSL
Plug 'tikhomirov/vim-glsl'

call plug#end()
filetype on " c.f. comment above on 'filetype off'
"}}}

" KEYS MAPPINGS -------------------------------------------------- {{{
" Move on visual lines
nnoremap j gj
nnoremap k gk

" Remap esc in insert & visual mode & command mode
inoremap kj <esc>
vnoremap kj <esc>
cnoremap kj <C-C>


" configure maralla/completor to use tab
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

" ultisnips default bindings compete with completor's tab
" so we need to remap them
let g:UltiSnipsExpandTrigger="<c-t>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Searching
nnoremap / /\v
vnoremap / y/\V<C-R>=escape(@",'/\')<CR><CR>
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

" Remap help key.
inoremap <F1> <ESC>:set invfullscreen<CR>a
nnoremap <F1> :set invfullscreen<CR>
vnoremap <F1> :set invfullscreen<CR>

" opens NERDTree
nnoremap <F2> :NERDTreeToggle<CR>
inoremap <F2> :NERDTreeToggle<CR>
nnoremap <leader>t :NERDTreeToggle<CR>

" LEADER KEY CONFIGURATION {{{

" Pick a leader key
let mapleader = "<space>"

" Save file
map <leader>w :w<CR>
" Formatting
map <leader>q gqip
" clear search
map <leader>/ :noh<CR>
" Toggle to visualize tabs and EOL
set listchars=tab:▸\ ,eol:¬
map <leader>l :set list!<CR>
" substitute (cf function in COMMANDS section)
vnoremap <leader>s :<BS><BS><BS><BS><BS>Substitute v 
nnoremap <leader>s :Substitute n 

" }}}

"}}}

" COMMAND FUNCTIONS -------------------------------------------------- {{{

" Substitute
command -nargs=+ Substitute :call Substitute(<f-args>)
function Substitute(...)
    if a:1 == 'v'
        execute printf('%%substitute/\%%V%s\%%V/%s/g', a:2, a:3)
    elseif a:1 == 'n'
        execute printf('%%substitute/%s/%s/g', a:2, a:3)
    endif
endfunction

" }}}

" STATUS LINE -------------------------------------------------- {{{

"set ruler " Show file stats
"set laststatus=2
" Last line
"set showmode
"set showcmd

"function! GitBranch()
  "return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
"endfunction

"function! StatuslineGit()
  "let l:branchname = GitBranch()
  "return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
"endfunction

"set statusline=
"set statusline+=%#PmenuSel#
"set statusline+=%{StatuslineGit()}
"set statusline+=%#LineNr#
"set statusline+=\ %f
"set statusline+=%m\
"set statusline+=%=
"set statusline+=%#CursorColumn#
""set statusline+=\ %y
""set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
""set statusline+=\[%{&fileformat}\]
"set statusline+=\ %p%%
"set statusline+=\ %l:%c
"set statusline+=\

"}}}

" COLOR THEME -------------------------------------------------- {{{

syntax on " Turn on syntax highlighting

set termguicolors
colorscheme catppuccin_mocha
set background=dark " Change terminal background

"}}}

set foldmethod=marker

" FILE CONFIGURATION -------------------------------------------------- {{{

" C, C++ FILES {{{

autocmd FileType c setlocal foldmethod=syntax
autocmd FileType c setlocal foldnestmax=1
autocmd FileType h setlocal foldmethod=marker

" Map Ctrl+b to check for Makefile and compile accordingly for C and C++ files
autocmd FileType c,cpp,h nnoremap <buffer> <C-b> :w <CR> :call CompileRunFile()<CR>

function! CompileRunFile()
    " Check if Makefile exists in the current directory
    if filereadable("Makefile") || filereadable("makefile")
        :make
        :make run
    else
        " Determine the compiler and file extension
        let l:filename = expand('%')
        let l:output = substitute(l:filename, '\.\(c\|cpp\|h\)$', '', '')

        if &filetype == 'c' || &filetype == 'h'
            " Compile current C file with gcc
            execute "!clear && gcc -o " . l:output . " " . l:filename . " -Wall -Wextra -fsanitize=address,undefined -lm"
        elseif &filetype == 'cpp'
            " Compile current C++ file with g++
            execute "!g++ -o " . l:output . " " . l:filename . " -Wall -Wextra -fsanitize=address,undefined -lm && ./" . l:output
        else
            echo "Unsupported file type"
        endif
    endif
endfunction


" }}}

" PYTHON FILES {{{

" Interpretation for Python files
autocmd FileType python nnoremap <C-b> :w<CR>:!python3 %<CR>
autocmd FileType python setlocal foldmethod=indent
autocmd FileType python setlocal foldnestmax=1
"set nofoldenable

" }}}

" RMARKDOWN FILES -------------------------------------------------- {{{
" Compilation Rmd -> html
autocmd Filetype rmd nnoremap <C-b> :w <CR> :!Rscript -e "rmarkdown::render('%')" <CR>
" }}}

" GLSL FILES -------------------------------------------------- {{{

autocmd FileType glsl setlocal foldmethod=syntax
autocmd Filetype glsl nnoremap <F5> :w <CR> :!make compile<CR>
autocmd Filetype glsl nnoremap <F6> :w <CR> :!make run FRAG_SHADER_PATH=%<CR>
autocmd Filetype glsl nnoremap <C-b> :w <CR> :!clear && make run FRAG_SHADER_PATH=%<CR>

" }}}

" }}}

set number " Show line numbers

set nomagic

" replace tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" highlight trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+\%#\@<!$/

set modelines=0 " Security
set encoding=utf-8

" Cursor motion
set scrolloff=3
set backspace=indent,eol,start
set matchpairs+=<:>         " use % to jump between pairs
runtime! macros/matchit.vim " enable matchit that extend the matching pairs to some like if/else
set mouse=a                 " enable to move the mouse on clicks

" NERDTree configuration {{{

" Start NERDTree and put the cursor back in the other window.
" and start NERDTree when Vim starts with a directory argument.
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in') |
            "\ execute 'NERDTree' argv()[0] | wincmd p | enew | execute 'cd '.argv()[0] | wincmd p |
            "\ else | execute 'NERDTree' argv()[0] | wincmd p | endif
"" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if &buftype != 'quickfix' && getcmdwintype() == '' | silent NERDTreeMirror | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | call feedkeys(":quit\<CR>:\<BS>") | endif

let NERDTreeShowHidden=1
"let NERDTreeMapOpenInTab='<ENTER>'

" }}}

" Allow hidden buffers
set hidden

" Rendering
set ttyfast
